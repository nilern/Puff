use crate::bytecode::{Bytecode, Builder};
use crate::compiler::Compiler;
use crate::compiler::cfg;
use crate::oref::Gc;

pub fn emit(cmp: &mut Compiler, f: &cfg::Fn) -> Gc<Bytecode> {
    fn emit_instr(cmp: &mut Compiler, builder: &mut Builder, instr: &cfg::Instr, rpo_next: Option<cfg::Label>) {
        use cfg::Instr::*;

        match instr {
            &Define(ref name) => builder.define(name.clone()),
            &GlobalSet(ref name) => builder.global_set(name.clone()),
            &Global(ref name) => builder.global(name.clone()),

            &Const(ref c) => builder.r#const(c.clone()),
            &Local(reg) => builder.local(reg),
            &Clover(i) => builder.clover(i),

            &PopNNT(n) => builder.popnnt(n),
            &Prune(ref prunes) => builder.prune(prunes),

            &Box => builder.r#box(),
            &UninitializedBox => builder.uninitialized_box(),
            &BoxSet => builder.box_set(),
            &CheckedBoxSet => builder.checked_box_set(),
            &BoxGet => builder.box_get(),
            &CheckedBoxGet => builder.checked_box_get(),
            &CheckUse => builder.check_use(),

            &If(_, alt) => builder.brf(alt),
            &Goto(dest) => if dest != rpo_next.unwrap() { builder.br(dest) },

            &Fn(ref code, len) => {
                let code = emit(cmp, code);
                builder.r#fn(cmp.mt.root_t(code), len);
            },

            &Call(cargc, ref prunes) => builder.call(cargc, prunes),
            &TailCall(cargc) => builder.tailcall(cargc),
            &Ret => builder.ret()
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
