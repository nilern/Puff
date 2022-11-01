use crate::bytecode::{Bytecode, Builder};
use crate::compiler::Compiler;
use crate::compiler::cfg::{self, PosInstr};
use crate::oref::Gc;
use crate::handle::{Root, root};

pub fn emit(cmp: &mut Compiler, f: &cfg::Fn) -> Gc<Bytecode> {
    fn emit_instr(cmp: &mut Compiler, builder: &mut Builder, instr: &cfg::PosInstr, rpo_next: Option<cfg::Label>) {
        use cfg::Instr::*;

        let PosInstr {pos, instr} = instr;
        match instr {
            &Define(ref name) => builder.define(name.clone(), pos.clone()),
            &GlobalSet(ref name) => builder.global_set(name.clone(), pos.clone()),
            &Global(ref name) => builder.global(name.clone(), pos.clone()),

            &Const(ref c) => builder.r#const(c.clone(), pos.clone()),
            &Local(reg) => builder.local(reg, pos.clone()),
            &Clover(i) => builder.clover(i, pos.clone()),

            &Pop => builder.pop(pos.clone()),
            &Prune(ref prunes) => builder.prune(prunes, pos.clone()),

            &Box => builder.r#box(pos.clone()),
            &UninitializedBox => builder.uninitialized_box(pos.clone()),
            &BoxSet => builder.box_set(pos.clone()),
            &CheckedBoxSet => builder.checked_box_set(pos.clone()),
            &BoxGet => builder.box_get(pos.clone()),
            &CheckedBoxGet => builder.checked_box_get(pos.clone()),
            &CheckUse => builder.check_use(pos.clone()),

            &If(_, alt) => builder.brf(alt, pos.clone()),
            &Goto(dest) => if dest != rpo_next.unwrap() { builder.br(dest, pos.clone()) },

            &Fn(ref code, len) => {
                let code = emit(cmp, code);
                builder.r#fn(root!(&mut cmp.mt, code), len, pos.clone());
            },

            &Call(cargc, ref prunes) => builder.call(cargc, prunes, pos.clone()),
            &CheckOneReturnValue => builder.check_one_return_value(pos.clone()),
            &IgnoreReturnValues => builder.ignore_return_values(pos.clone()),
            &TailCallWithValues => builder.tailcall_with_values(pos.clone()),
            &TailCall(cargc) => builder.tailcall(cargc, pos.clone()),
            &Ret => builder.ret(pos.clone())
        }
    }

    fn emit_block(cmp: &mut Compiler, builder: &mut Builder, f: &cfg::Fn, label: cfg::Label,
        rpo_next: Option<cfg::Label>
    ) {
        builder.label(label);

        for instr in f.block(label).iter() {
            emit_instr(cmp, builder, instr, rpo_next);
        }
    }

    let po = f.post_order();

    let mut builder = Builder::new(f.min_arity, f.varargs, f.max_regs, f.clover_names.clone());

    let mut rpo = po.iter().rev().peekable();
    while let Some(&label) = rpo.next() {
        emit_block(cmp, &mut builder, f, label, rpo.peek().map(|&&label| label));
    }

    builder.build(cmp.mt)
}
