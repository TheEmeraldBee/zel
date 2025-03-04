use std::collections::HashMap;

use cranelift::{
    module::Module,
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::*,
};

use crate::{
    Spanned,
    parse::{BinaryOp, Expr, SetOp},
};

pub struct Compiler {
    builder_context: FunctionBuilderContext,

    ctx: codegen::Context,

    module: ObjectModule,
}

impl Compiler {
    pub fn new(name: &str) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic").unwrap();
        let isa_builder = isa::lookup_by_name("aarch64-apple-darwin").unwrap();
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder = ObjectBuilder::new(
            isa,
            name.to_owned(),
            cranelift::module::default_libcall_names(),
        )
        .unwrap();

        let module = ObjectModule::new(builder);

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
        }
    }

    pub fn compile<'src>(&mut self, expr: Spanned<Expr<'src>>) -> Result<(), String> {
        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::F64));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut trans = Translator {
            builder,
            variables: HashMap::new(),
            module: &mut self.module,
        };

        let ret = trans.translate_expr(expr);
        trans.builder.ins().return_(&[ret]);

        trans.builder.finalize();

        println!("{:?}", self.ctx.func);

        let id = self
            .module
            .declare_function(
                "main",
                cranelift::module::Linkage::Export,
                &self.ctx.func.signature,
            )
            .unwrap();

        self.module.define_function(id, &mut self.ctx).unwrap();

        self.module.clear_context(&mut self.ctx);

        Ok(())
    }

    pub fn finish(self) -> ObjectProduct {
        self.module.finish()
    }
}

struct Translator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut ObjectModule,
}

impl<'a> Translator<'a> {
    pub fn translate_expr<'src>(&mut self, expr: Spanned<Expr<'src>>) -> Value {
        match expr.0 {
            Expr::Error => unreachable!("Error expr type should be handled"),
            Expr::Value(a) => match a {
                crate::value::Value::Null => self.builder.ins().f64const(0.0),
                crate::value::Value::Bool(b) => self.builder.ins().f64const(match b {
                    true => 1.0,
                    false => 0.0,
                }),
                crate::value::Value::Num(n) => self.builder.ins().f64const(n),
                _ => todo!(),
            },
            Expr::Local(t) => {
                let variable = self.variables.get(t).unwrap();
                self.builder.use_var(*variable)
            }
            Expr::Var(ident, body) | Expr::Const(ident, body) => {
                let value = self.translate_expr(*body);
                let variable = self.variables.get(ident).unwrap();
                self.builder.def_var(*variable, value);
                value
            }
            Expr::Set(ident, op, body) => {
                let value = self.translate_expr(*body);
                let variable = self.variables.get(ident).unwrap();
                match op {
                    SetOp::Set => {
                        self.builder.def_var(*variable, value);
                        value
                    }
                    SetOp::Add => {
                        let var = self.builder.use_var(*variable);
                        self.builder.ins().fadd(value, var)
                    }
                    SetOp::Sub => {
                        let var = self.builder.use_var(*variable);
                        self.builder.ins().fsub(value, var)
                    }
                    SetOp::Mul => {
                        let var = self.builder.use_var(*variable);
                        self.builder.ins().fmul(value, var)
                    }
                    SetOp::Div => {
                        let var = self.builder.use_var(*variable);
                        self.builder.ins().fdiv(value, var)
                    }
                }
            }
            Expr::Then(a, b) => {
                self.translate_expr(*a);
                self.translate_expr(*b)
            }
            Expr::Block(a) => self.translate_expr(*a),
            Expr::Binary(lhs, op, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                match op {
                    BinaryOp::Add => self.builder.ins().fadd(lhs, rhs),
                    BinaryOp::Sub => self.builder.ins().fsub(lhs, rhs),
                    BinaryOp::Mul => self.builder.ins().fmul(lhs, rhs),
                    BinaryOp::Div => self.builder.ins().fdiv(lhs, rhs),

                    BinaryOp::Eq => self.builder.ins().fcmp(FloatCC::Equal, lhs, rhs),
                    BinaryOp::NotEq => self.builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs),

                    BinaryOp::Greater => self.builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs),
                    BinaryOp::GreaterEq => {
                        self.builder
                            .ins()
                            .fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs)
                    }

                    BinaryOp::Less => self.builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                    BinaryOp::LessEq => self.builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs),

                    BinaryOp::Or => self.builder.ins().bor(lhs, rhs),
                    BinaryOp::And => self.builder.ins().band(lhs, rhs),
                }
            }
            Expr::If(cond, then, else_) => {
                let condition_value = self.translate_expr(*cond);

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                self.builder.append_block_param(merge_block, types::F64);

                self.builder
                    .ins()
                    .brif(condition_value, then_block, &[], else_block, &[]);

                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_return = self.translate_expr(*then);

                self.builder.ins().jump(merge_block, &[then_return]);

                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_return = self.translate_expr(*else_);

                self.builder.ins().jump(merge_block, &[else_return]);

                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);

                let phi = self.builder.block_params(merge_block)[0];

                phi
            }
            Expr::While(cond, body) => {
                let header_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                self.builder.switch_to_block(header_block);

                let condition_value = self.translate_expr(*cond);
                self.builder
                    .ins()
                    .brif(condition_value, body_block, &[], exit_block, &[]);

                self.builder.switch_to_block(body_block);
                self.builder.seal_block(body_block);

                self.translate_expr(*body);

                self.builder.ins().jump(header_block, &[]);

                self.builder.switch_to_block(exit_block);

                self.builder.seal_block(header_block);
                self.builder.seal_block(exit_block);

                self.builder.ins().f64const(0.0)
            }
            Expr::Call(func, args) => self.translate_call(*func, args.0),
            Expr::Func(args, body) => self.translate_func(args, (*body.0, body.1)),
            _ => todo!(),
        }
    }

    fn translate_func<'src>(
        &mut self,
        args: Vec<(&'src str, crate::types::Type)>,
        body: (Spanned<Expr>, crate::types::Type),
    ) -> Value {
        let mut sig = self.module.make_signature();
        for _arg in args {
            sig.params.push(AbiParam::new(types::F64));
        }

        sig.returns.push(AbiParam::new(types::F64));

        self.builder.ins().f64const(0.0)
    }

    fn translate_call<'src>(
        &mut self,
        body: Spanned<Expr<'src>>,
        args: Vec<Spanned<Expr<'src>>>,
    ) -> Value {
        let mut sig = self.module.make_signature();

        for _arg in &args {
            sig.params.push(AbiParam::new(types::F64));
        }

        sig.returns.push(AbiParam::new(types::F64));

        let callee = self
            .module
            .declare_anonymous_function(&sig)
            .expect("problem declaring function");
        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let mut arg_values = Vec::new();

        for arg in &args {
            arg_values.push(self.translate_expr(arg.clone()));
        }
        let call = self.builder.ins().call(local_callee, &arg_values);

        self.builder.inst_results(call)[0]
    }
}
