use std::collections::HashMap;

use inkwell::{
    AddressSpace, IntPredicate,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{ArrayValue, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ast::{expr::Expr, literal::Literal, ops::BinaryOp},
    comptime::Comptime,
    func::FunctionScope,
    scope::{Scope, Solver, Variable, VariableValue},
    types::{self, Type},
    value::Value,
};

#[derive(Clone)]
/// Represents a variable that is registered within llvm
struct CompilerVariable<'ctx> {
    pointer: PointerValue<'ctx>,
    zel_type: Type,
    llvm_type: BasicTypeEnum<'ctx>,
}

/// A scope to represent all registered variables in the compiler
type CompilerScope<'ctx> = Scope<CompilerVariable<'ctx>>;

/// The core construct for compiling zel ast.
pub struct Compiler<'ctx, 'a> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,

    // Represent comptime execution systems
    pub comptime: &'a mut Comptime,
    pub comptime_scope: &'a mut Scope<Value>,
    pub comptime_funcs: &'a mut FunctionScope,

    pub mono_cache: HashMap<(String, Vec<Type>), String>,
    pub struct_type_cache: HashMap<types::Struct, StructType<'ctx>>,

    // The currently compiled function
    pub current_fn: Option<FunctionValue<'ctx>>,
}

impl<'ctx, 'a> Compiler<'ctx, 'a> {
    /// Creates a new compiler with the given state
    /// name is the name of the module.
    pub fn new(
        context: &'ctx Context,
        name: &str,
        comptime: &'a mut Comptime,
        comptime_scope: &'a mut Scope<Value>,
        comptime_funcs: &'a mut FunctionScope,
    ) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(name);

        Self {
            context,
            builder,
            module,
            comptime,
            comptime_scope,
            comptime_funcs,
            mono_cache: HashMap::new(),
            struct_type_cache: HashMap::new(),
            current_fn: None,
        }
    }

    /// Converts a zel type into an llvm type
    fn to_llvm_type(&mut self, type_: &Type) -> BasicTypeEnum<'ctx> {
        match type_ {
            Type::Integer(bits) => self
                .context
                .custom_width_int_type(*bits as u32)
                .as_basic_type_enum(),
            Type::Bool => self.context.bool_type().as_basic_type_enum(),
            Type::Array(size, element_type) => self
                .to_llvm_type(element_type)
                .array_type(*size as u32)
                .as_basic_type_enum(),
            Type::Struct(s) => {
                if let Some(t) = self.struct_type_cache.get(s) {
                    return (*t).as_basic_type_enum();
                }
                let field_types = s
                    .fields
                    .iter()
                    .map(|(_, t)| self.to_llvm_type(t))
                    .collect::<Vec<_>>();
                let struct_type = self.context.struct_type(&field_types, false);
                self.struct_type_cache.insert(s.clone(), struct_type);
                struct_type.as_basic_type_enum()
            }
            Type::Pointer(_) => self
                .context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            _ => unimplemented!("Type {:?} is not yet compilable", type_),
        }
    }

    /// Compiles an expression type into a pointer value
    fn compile_lvalue_to_ptr(
        &mut self,
        expr: &Expr,
        scope: &mut CompilerScope<'ctx>,
    ) -> anyhow::Result<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
        match expr {
            Expr::Local(name) => {
                let var = scope
                    .get_solved(name)
                    .map_err(|e| anyhow::anyhow!(e.to_string()))?;
                Ok((var.pointer, var.llvm_type))
            }
            Expr::Access { val, field } => {
                let (base_ptr, base_type) = self.compile_lvalue_to_ptr(val, scope)?;
                let zel_base_type = self.get_expr_type(val, scope, None)?;

                let Type::Struct(struct_def) = zel_base_type else {
                    anyhow::bail!("Cannot access field on non-struct type");
                };

                let field_index = struct_def
                    .fields
                    .iter()
                    .position(|(name, _)| name == field)
                    .ok_or_else(|| anyhow::anyhow!("No such field '{}'", field))?;

                let field_type = self.to_llvm_type(&struct_def.fields[field_index].1);

                let ptr = self
                    .builder
                    .build_struct_gep(base_type, base_ptr, field_index as u32, field)
                    .map_err(|e| anyhow::anyhow!(e.to_string()))?;

                Ok((ptr, field_type))
            }
            Expr::Index { value, index } => {
                let base_zel_type = self.get_expr_type(value, scope, None)?;
                let index_val = self
                    .compile_expr(index, scope, Some(&Type::Integer(64)))?
                    .into_int_value();

                match &base_zel_type {
                    Type::Array(_, element_type) => {
                        let (base_ptr, base_llvm_type) =
                            self.compile_lvalue_to_ptr(value, scope)?;
                        let zero = self.context.i64_type().const_int(0, false);
                        let ptr = unsafe {
                            self.builder
                                .build_gep(
                                    base_llvm_type,
                                    base_ptr,
                                    &[zero, index_val],
                                    "arrayidxptr",
                                )
                                .map_err(|e| anyhow::anyhow!(e.to_string()))?
                        };
                        Ok((ptr, self.to_llvm_type(&element_type)))
                    }
                    Type::Pointer(element_type) => {
                        let ptr_val = self
                            .compile_expr(value, scope, Some(&base_zel_type))?
                            .into_pointer_value();
                        let llvm_element_type = self.to_llvm_type(&element_type);
                        let ptr = unsafe {
                            self.builder
                                .build_gep(llvm_element_type, ptr_val, &[index_val], "ptr_idx_ptr")
                                .map_err(|e| anyhow::anyhow!(e.to_string()))?
                        };
                        Ok((ptr, llvm_element_type))
                    }
                    _ => {
                        anyhow::bail!("Cannot index non-array/pointer type '{}'", base_zel_type);
                    }
                }
            }
            Expr::Deref(ptr_expr) => {
                let ptr_type = self.get_expr_type(ptr_expr, scope, None)?;
                let inner_type = if let Type::Pointer(t) = ptr_type {
                    t
                } else {
                    anyhow::bail!("Cannot dereference non-pointer type");
                };
                let ptr_val = self
                    .compile_expr(ptr_expr, scope, None)?
                    .into_pointer_value();
                Ok((ptr_val, self.to_llvm_type(&inner_type)))
            }
            _ => anyhow::bail!("Expression is not a valid l-value: {:?}", expr),
        }
    }

    /// Get's the zel-type from the expression passed.
    fn get_expr_type(
        &mut self,
        expr: &Expr,
        scope: &mut CompilerScope<'ctx>,
        type_hint: Option<&Type>,
    ) -> anyhow::Result<Type> {
        match expr {
            Expr::Literal(Literal::Num(_)) => Ok(type_hint.cloned().unwrap_or(Type::Integer(64))),
            Expr::Literal(Literal::Bool(_)) => Ok(Type::Bool),
            Expr::Literal(Literal::String(_)) => Ok(Type::Pointer(Box::new(Type::Integer(8)))),
            Expr::Local(name) => {
                if let Ok(var) = scope.get_solved(name) {
                    return Ok(var.zel_type.clone());
                }

                let comptime_var = self.comptime_scope.get_raw(name)?;
                let solved_val = match comptime_var.value {
                    VariableValue::Initialized(v) => v,
                    VariableValue::Unsolved(e) => {
                        self.comptime
                            .solve(self.comptime_scope, self.comptime_funcs, &e)?
                    }
                };
                Ok(solved_val.type_of())
            }
            Expr::Call { func, args } => {
                let Expr::Local(fn_name) = &**func else {
                    anyhow::bail!("Dynamic function calls not supported yet");
                };
                let raw_var = self.comptime_scope.get_raw(fn_name)?;

                let (original_fn_args, body_expr, return_type_expr) = match raw_var.value {
                    VariableValue::Unsolved(Expr::Extern {
                        args, return_type, ..
                    }) => (args.clone(), return_type.clone(), return_type.clone()),
                    VariableValue::Initialized(Value::Function(id)) => {
                        let (a, b, r) = self.comptime_funcs.get_ast(id).unwrap();
                        (a.clone(), Box::new(b.clone()), Box::new(r.clone()))
                    }
                    _ => anyhow::bail!("Cannot call non-function '{}'", fn_name),
                };

                self.comptime_scope.push_scope();

                if original_fn_args.len() != args.len() {
                    anyhow::bail!("Incorrect number of arguments to function")
                }

                for ((param_name, param_type_expr), call_arg_expr) in
                    original_fn_args.iter().zip(args.iter())
                {
                    let param_type_val = self.comptime.solve(
                        self.comptime_scope,
                        self.comptime_funcs,
                        param_type_expr,
                    );

                    if let Ok(Value::Type(Type::Type)) = param_type_val {
                        let actual_type_val = self.comptime.solve(
                            self.comptime_scope,
                            self.comptime_funcs,
                            call_arg_expr,
                        )?;

                        self.comptime_scope.register(
                            param_name,
                            Variable {
                                mutable: false,
                                value: VariableValue::Initialized(actual_type_val),
                            },
                        );
                        continue;
                    }

                    if let Expr::Local(type_param_name) = param_type_expr {
                        let is_known_type = self
                            .comptime
                            .solve(self.comptime_scope, self.comptime_funcs, param_type_expr)
                            .is_ok();
                        if !is_known_type {
                            let actual_arg_type = self.get_expr_type(call_arg_expr, scope, None)?;

                            self.comptime_scope.register(
                                type_param_name,
                                Variable {
                                    mutable: false,
                                    value: VariableValue::Initialized(Value::Type(actual_arg_type)),
                                },
                            );
                        }
                    }
                }

                let annotation_val = self.comptime.solve(
                    self.comptime_scope,
                    self.comptime_funcs,
                    &return_type_expr,
                )?;

                let final_expr_to_solve = if let Value::Type(Type::Type) = annotation_val {
                    body_expr
                } else {
                    return_type_expr
                };

                let return_val = self.comptime.solve(
                    self.comptime_scope,
                    self.comptime_funcs,
                    &final_expr_to_solve,
                )?;

                self.comptime_scope.pop_scope();

                let Value::Type(t) = return_val else {
                    anyhow::bail!(
                        "Function return value must be a type, but got {:?}",
                        return_val
                    );
                };
                Ok(t)
            }
            Expr::Access { val, field } => {
                let base_type = self.get_expr_type(val, scope, None)?;

                let Type::Struct(struct_def) = base_type else {
                    anyhow::bail!("Attempted to access field '{}' on a non-struct type", field);
                };

                let field_type = struct_def
                    .fields
                    .iter()
                    .find(|(name, _)| name == field)
                    .map(|(_, ty)| ty.clone())
                    .ok_or_else(|| {
                        anyhow::anyhow!("Struct does not have a field named '{}'", field)
                    })?;

                Ok(field_type)
            }
            Expr::InitStruct { struct_, .. } => {
                let struct_type_val =
                    self.comptime
                        .solve(self.comptime_scope, self.comptime_funcs, struct_)?;

                let Value::Type(t) = struct_type_val else {
                    anyhow::bail!(
                        "Expected a struct type for initialization, but got {:?}",
                        struct_type_val
                    );
                };

                Ok(t)
            }
            Expr::ArrayLiteral { values } => {
                let elem_type = if let Some(Type::Array(_, et)) = type_hint {
                    et.as_ref().clone()
                } else {
                    self.get_expr_type(&values[0], scope, None)?
                };
                Ok(Type::Array(values.len(), Box::new(elem_type)))
            }
            Expr::ArrayFill { value, size } => {
                let elem_type = if let Some(Type::Array(_, et)) = type_hint {
                    et.as_ref().clone()
                } else {
                    self.get_expr_type(value, scope, None)?
                };
                let size_val =
                    self.comptime
                        .solve(self.comptime_scope, self.comptime_funcs, size)?;
                let Value::Literal(Literal::Num(n)) = size_val else {
                    anyhow::bail!("Array fill size must be a comptime integer");
                };
                Ok(Type::Array(n as usize, Box::new(elem_type)))
            }
            Expr::Binary { lhs, op, .. } => {
                if matches!(
                    op,
                    BinaryOp::Eq
                        | BinaryOp::Ne
                        | BinaryOp::Gt
                        | BinaryOp::Gte
                        | BinaryOp::Lt
                        | BinaryOp::Lte
                ) {
                    return Ok(Type::Bool);
                }
                self.get_expr_type(lhs, scope, type_hint)
            }
            Expr::If { body, else_, .. } => {
                let then_type = self.get_expr_type(body, scope, type_hint)?;
                let Some(else_expr) = else_ else {
                    return Ok(Type::Null);
                };
                let else_type = self.get_expr_type(else_expr, scope, Some(&then_type))?;
                if then_type != else_type {
                    anyhow::bail!(
                        "If branches have mismatched types: then is '{}', else is '{}'",
                        then_type,
                        else_type
                    );
                }
                Ok(then_type)
            }
            Expr::Block { body } => self.get_expr_type(body, scope, type_hint),
            Expr::Then { next, .. } => self.get_expr_type(next, scope, type_hint),
            Expr::For { .. } | Expr::While { .. } => Ok(Type::Null),
            Expr::Index { value, .. } => {
                let value_type = self.get_expr_type(value, scope, None)?;
                match value_type {
                    Type::Array(_, elem_type) => Ok(*elem_type),
                    Type::Pointer(elem_type) => Ok(*elem_type),
                    _ => anyhow::bail!("Cannot index type '{}'", value_type),
                }
            }
            Expr::AddressOf(expr) => {
                let inner_type = self.get_expr_type(expr, scope, None)?;
                Ok(Type::Pointer(Box::new(inner_type)))
            }
            Expr::Deref(expr) => {
                let inner_type = self.get_expr_type(expr, scope, None)?;
                if let Type::Pointer(pointed_to_type) = inner_type {
                    Ok(*pointed_to_type)
                } else {
                    anyhow::bail!("Cannot dereference non-pointer type '{}'", inner_type)
                }
            }
            _ => unreachable!(),
        }
    }

    /// Convert generic function into a solved function
    pub fn monomorphize_and_compile_fn(
        &mut self,
        fn_name: &str,
        generic_args: &[(String, Value)],
    ) -> anyhow::Result<String> {
        let type_args: Vec<_> = generic_args.iter().map(|(_, v)| v.type_of()).collect();
        let cache_key = (fn_name.to_string(), type_args.clone());

        if let Some(mangled_name) = self.mono_cache.get(&cache_key) {
            return Ok(mangled_name.clone());
        }

        let raw_var = self.comptime_scope.get_raw(fn_name)?;

        // Check for external functions, as these aren't checked by our systems
        if let VariableValue::Unsolved(Expr::Extern {
            name,
            args,
            return_type,
        }) = raw_var.value
        {
            self.declare_fn_signature(&name, &args, &return_type, true)?;
            self.mono_cache.insert(cache_key, name.clone());
            return Ok(name);
        }

        let fn_var = self.comptime_scope.get_solved(fn_name)?;
        let Value::Function(fn_id) = fn_var else {
            anyhow::bail!("'{}' is not a function", fn_name);
        };

        let Some((original_args, body, return_type)) = self
            .comptime_funcs
            .get_ast(fn_id)
            .map(|(a, b, c)| (a.clone(), b.clone(), c.clone()))
        else {
            anyhow::bail!("Could not retrieve AST for function '{}'", fn_name);
        };

        self.comptime_scope.push_scope();
        for (name, value) in generic_args {
            self.comptime_scope.register(
                name,
                Variable {
                    mutable: false,
                    value: VariableValue::Initialized(value.clone()),
                },
            );
        }

        // Combine generic arguments in order to make a unique function.
        let mangled_name = format!(
            "{}{}",
            fn_name,
            type_args
                .iter()
                .map(|t| t.to_string().replace(['[', ']'], ""))
                .collect::<Vec<_>>()
                .join("")
        );

        // Insert the function into the cache, to tell other functions that we know it's type.
        self.mono_cache
            .insert(cache_key.clone(), mangled_name.clone());

        // Declare and compile the actual function
        self.declare_fn_signature(&mangled_name, &original_args, &return_type, false)?;
        self.compile_fn_body(&mangled_name, &original_args, &body, &return_type)?;

        self.comptime_scope.pop_scope();

        Ok(mangled_name)
    }

    /// Tell llvm that the function type exists
    pub fn declare_fn_signature(
        &mut self,
        name: &str,
        args: &[(String, Expr)],
        return_type: &Expr,
        is_extern: bool,
    ) -> anyhow::Result<()> {
        let ret_type_val =
            self.comptime
                .solve(self.comptime_scope, self.comptime_funcs, return_type)?;
        let Value::Type(ret_type) = ret_type_val else {
            anyhow::bail!("Function return type must be a type");
        };

        let mut arg_types = vec![];
        for (_, arg_type_expr) in args.iter() {
            let arg_type_val =
                self.comptime
                    .solve(self.comptime_scope, self.comptime_funcs, arg_type_expr)?;
            if let Value::Type(Type::Type) = arg_type_val {
                continue;
            }
            let Value::Type(t) = arg_type_val else {
                anyhow::bail!("Function argument must be a type");
            };
            if is_extern {
                if let Type::Array(_, _) = t {
                    arg_types.push(
                        self.context
                            .ptr_type(AddressSpace::default())
                            .as_basic_type_enum(),
                    );
                    continue;
                }
            }
            arg_types.push(self.to_llvm_type(&t));
        }

        let arg_metadata_types = arg_types
            .iter()
            .map(|x| (*x).into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let fn_type = if ret_type == Type::Null {
            if name == "main" {
                self.context.i64_type().fn_type(&arg_metadata_types, false)
            } else {
                self.context.void_type().fn_type(&arg_metadata_types, false)
            }
        } else {
            self.to_llvm_type(&ret_type)
                .fn_type(&arg_metadata_types, false)
        };

        let linkage = if is_extern {
            Some(Linkage::External)
        } else {
            None
        };

        self.module.add_function(name, fn_type, linkage);
        Ok(())
    }

    /// Compile the function's body into a block
    pub fn compile_fn_body(
        &mut self,
        name: &str,
        args: &[(String, Expr)],
        body: &Expr,
        return_type_expr: &Expr,
    ) -> anyhow::Result<()> {
        let function = self
            .module
            .get_function(name)
            .expect("Function should have been declared in pass 1");

        let original_fn = self.current_fn;
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        self.current_fn = Some(function);

        let mut fn_scope = CompilerScope::default();

        let mut param_idx = 0;
        for (arg_name, arg_expr) in args.iter() {
            let arg_type_val =
                self.comptime
                    .solve(self.comptime_scope, self.comptime_funcs, arg_expr)?;

            let Value::Type(zel_type) = arg_type_val else {
                anyhow::bail!("Function parameter must be a type");
            };

            if zel_type == Type::Type {
                continue;
            }

            let arg = function.get_nth_param(param_idx as u32).unwrap();
            param_idx += 1;

            let llvm_type = arg.get_type();
            arg.set_name(arg_name);

            let alloca = self.builder.build_alloca(llvm_type, arg_name).unwrap();
            self.builder.build_store(alloca, arg).unwrap();

            let var = CompilerVariable {
                pointer: alloca,
                zel_type,
                llvm_type,
            };

            fn_scope.register(
                arg_name,
                Variable {
                    mutable: false,
                    value: VariableValue::Initialized(var),
                },
            );
        }

        let body_val = self.compile_expr(body, &mut fn_scope, None)?;

        let ret_type_val =
            self.comptime
                .solve(self.comptime_scope, self.comptime_funcs, return_type_expr)?;
        let Value::Type(ret_type) = ret_type_val else {
            anyhow::bail!("Function return type must be a type");
        };

        if ret_type == Type::Null {
            if name == "main" {
                self.builder
                    .build_return(Some(&self.context.i64_type().const_int(0, false)))
                    .unwrap();
            } else {
                self.builder.build_return(None).unwrap();
            }
        } else {
            self.builder.build_return(Some(&body_val)).unwrap();
        }

        if !function.verify(true) {
            anyhow::bail!("Invalid function generated: {}", name);
        }

        self.current_fn = original_fn;

        Ok(())
    }

    /// Compiles any given expression into llvm code.
    fn compile_expr(
        &mut self,
        expr: &Expr,
        scope: &mut CompilerScope<'ctx>,
        type_hint: Option<&Type>,
    ) -> anyhow::Result<BasicValueEnum<'ctx>> {
        match expr {
            // These should never be compiled
            Expr::Extern { .. } => unreachable!(),

            // Types are only knowable in the comptime expr
            // So they aren't compilable
            Expr::Type(_) => anyhow::bail!("Types are comptime only values"),

            // Compiles a function call
            Expr::Call { func, args } => {
                // TODO: Support dynamic functions
                let Expr::Local(fn_name) = &**func else {
                    anyhow::bail!("Dynamic function calls not supported yet");
                };

                let raw_var = self.comptime_scope.get_raw(fn_name)?;
                let (original_fn_args, is_extern) = match raw_var.value {
                    VariableValue::Unsolved(Expr::Extern { args, .. }) => (args.clone(), true),
                    VariableValue::Initialized(Value::Function(id)) => {
                        (self.comptime_funcs.get_ast(id).unwrap().0.clone(), false)
                    }
                    _ => anyhow::bail!("Cannot call non-function '{}'", fn_name),
                };

                let return_type = self.get_expr_type(expr, scope, None)?;
                if return_type == Type::Type {
                    return Ok(self.context.i64_type().const_zero().as_basic_value_enum());
                }

                if args.len() != original_fn_args.len() {
                    anyhow::bail!(
                        "Incorrect number of arguments for function '{}'. Expected {}, got {}",
                        fn_name,
                        original_fn_args.len(),
                        args.len()
                    );
                }

                let mut param_types = Vec::new();
                let mut generic_args = Vec::new();

                self.comptime_scope.push_scope();

                // Register comptime types and their generics if needed.
                for ((param_name, param_type_expr), call_arg_expr) in
                    original_fn_args.iter().zip(args.iter())
                {
                    let param_type_val = self.comptime.solve(
                        self.comptime_scope,
                        self.comptime_funcs,
                        param_type_expr,
                    )?;

                    // We found a type, so we need to fill that in as if it were a generic (cuz it is).
                    if let Value::Type(Type::Type) = param_type_val {
                        let generic_arg_val = self.comptime.solve(
                            self.comptime_scope,
                            self.comptime_funcs,
                            call_arg_expr,
                        )?;
                        self.comptime_scope.register(
                            param_name,
                            Variable {
                                mutable: false,
                                value: VariableValue::Initialized(generic_arg_val.clone()),
                            },
                        );
                        generic_args.push((param_name.clone(), generic_arg_val));
                    } else if let Value::Type(t) = param_type_val {
                        param_types.push(t);
                    }
                }
                self.comptime_scope.pop_scope();

                let mut runtime_arg_idx = 0;
                for (i, arg_expr) in args.iter().enumerate() {
                    let arg_zel_type = self.get_expr_type(arg_expr, scope, None)?;
                    if arg_zel_type == Type::Type {
                        continue;
                    }

                    if runtime_arg_idx >= param_types.len() {
                        runtime_arg_idx += 1;
                        continue;
                    }

                    let expected_type = &param_types[runtime_arg_idx];
                    let actual_type = self.get_expr_type(arg_expr, scope, Some(expected_type))?;

                    // Simply type-check the code.
                    if &actual_type != expected_type {
                        anyhow::bail!(
                            "Type mismatch for argument {}: expected {}, found {}",
                            i,
                            expected_type,
                            actual_type
                        );
                    }
                    runtime_arg_idx += 1;
                }

                let current_block = self.builder.get_insert_block().unwrap();
                let mangled_name = self.monomorphize_and_compile_fn(fn_name, &generic_args)?;
                self.builder.position_at_end(current_block);

                let function = self.module.get_function(&mangled_name).unwrap();

                let mut compiled_args = vec![];
                let mut runtime_arg_idx = 0;
                for arg_expr in args.iter() {
                    let arg_type = self.get_expr_type(arg_expr, scope, None)?;
                    if arg_type == Type::Type {
                        continue;
                    }

                    let type_hint = if runtime_arg_idx < param_types.len() {
                        Some(&param_types[runtime_arg_idx])
                    } else {
                        None
                    };

                    if is_extern {
                        if let Type::Array(_, _) = arg_type {
                            let (ptr, _) = self.compile_lvalue_to_ptr(arg_expr, scope)?;
                            compiled_args.push(ptr.into());
                            runtime_arg_idx += 1;
                            continue;
                        }
                    }

                    compiled_args.push(self.compile_expr(arg_expr, scope, type_hint)?.into());
                    runtime_arg_idx += 1;
                }

                let call = self
                    .builder
                    .build_call(function, &compiled_args, "calltmp")
                    .unwrap();

                Ok(call
                    .try_as_basic_value()
                    .left()
                    .unwrap_or_else(|| self.context.i64_type().const_zero().as_basic_value_enum()))
            }

            // A constant number, so simply compile it.
            Expr::Literal(Literal::Num(n)) => {
                let ty = type_hint.cloned().unwrap_or(Type::Integer(64));
                if let Type::Integer(bits) = ty {
                    Ok(self
                        .context
                        .custom_width_int_type(bits as u32)
                        .const_int(*n as u64, true)
                        .as_basic_value_enum())
                } else {
                    anyhow::bail!("Numeric literal used in non-integer context")
                }
            }

            // Simply compile booleans as well
            Expr::Literal(Literal::Bool(b)) => Ok(self
                .context
                .bool_type()
                .const_int(*b as u64, false)
                .as_basic_value_enum()),

            // Strings are a *i8
            Expr::Literal(Literal::String(s)) => Ok(self
                .builder
                .build_global_string_ptr(s, ".str")
                .unwrap()
                .as_basic_value_enum()),

            // Pretty straight forward, just compile and check types, and compile from there.
            Expr::Binary { lhs, op, rhs } => {
                let lhs_type = self.get_expr_type(lhs, scope, None)?;
                let rhs_type = self.get_expr_type(rhs, scope, Some(&lhs_type))?;

                if lhs_type != rhs_type {
                    anyhow::bail!(
                        "Type mismatch in binary operation '{}'. Left is '{}', right is '{}'",
                        op,
                        lhs_type,
                        rhs_type
                    );
                }

                let lhs_val = self.compile_expr(lhs, scope, Some(&lhs_type))?;
                let rhs_val = self.compile_expr(rhs, scope, Some(&rhs_type))?;

                let res = match lhs_type {
                    Type::Integer(_) => {
                        let lhs_int = lhs_val.into_int_value();
                        let rhs_int = rhs_val.into_int_value();
                        match op {
                            BinaryOp::Add => self
                                .builder
                                .build_int_add(lhs_int, rhs_int, "addtmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            BinaryOp::Sub => self
                                .builder
                                .build_int_sub(lhs_int, rhs_int, "subtmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            BinaryOp::Mul => self
                                .builder
                                .build_int_mul(lhs_int, rhs_int, "multmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            BinaryOp::Div => self
                                .builder
                                .build_int_signed_div(lhs_int, rhs_int, "divtmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            op @ (BinaryOp::Eq
                            | BinaryOp::Ne
                            | BinaryOp::Gt
                            | BinaryOp::Gte
                            | BinaryOp::Lt
                            | BinaryOp::Lte) => {
                                let inkwell_op = match op {
                                    BinaryOp::Eq => IntPredicate::EQ,
                                    BinaryOp::Ne => IntPredicate::NE,
                                    BinaryOp::Gt => IntPredicate::SGT,
                                    BinaryOp::Gte => IntPredicate::SGE,
                                    BinaryOp::Lt => IntPredicate::SLT,
                                    BinaryOp::Lte => IntPredicate::SLE,
                                    _ => unreachable!(),
                                };
                                self.builder
                                    .build_int_compare(inkwell_op, lhs_int, rhs_int, "cmptmp")
                                    .unwrap()
                                    .as_basic_value_enum()
                            }
                        }
                    }
                    Type::Bool => {
                        let lhs_bool = lhs_val.into_int_value();
                        let rhs_bool = rhs_val.into_int_value();
                        match op {
                            BinaryOp::Eq => self
                                .builder
                                .build_int_compare(IntPredicate::EQ, lhs_bool, rhs_bool, "cmptmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            BinaryOp::Ne => self
                                .builder
                                .build_int_compare(IntPredicate::NE, lhs_bool, rhs_bool, "cmptmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            _ => anyhow::bail!("Operator '{}' is not supported for booleans", op),
                        }
                    }
                    _ => {
                        anyhow::bail!(
                            "Binary operations are not supported for type '{}'",
                            lhs_type
                        )
                    }
                };
                Ok(res)
            }

            // Let expressions save values to a pointer
            Expr::Let {
                mutable,
                name,
                type_annotation,
                body,
            } => {
                let hint = if let Some(type_expr) = type_annotation {
                    let val =
                        self.comptime
                            .solve(self.comptime_scope, self.comptime_funcs, type_expr)?;
                    let Value::Type(t) = val else {
                        anyhow::bail!("Annotation must be a type");
                    };
                    Some(t)
                } else {
                    None
                };

                let zel_type = self.get_expr_type(body, scope, hint.as_ref())?;

                if let Some(expected_type) = &hint {
                    if &zel_type != expected_type {
                        anyhow::bail!(
                            "Type mismatch in let binding: expected {}, found {}",
                            expected_type,
                            zel_type
                        );
                    }
                }

                let body_val = self.compile_expr(body, scope, Some(&zel_type))?;
                let llvm_type = body_val.get_type();
                let alloca = self.builder.build_alloca(llvm_type, name).unwrap();
                self.builder.build_store(alloca, body_val).unwrap();

                let var = CompilerVariable {
                    pointer: alloca,
                    zel_type,
                    llvm_type,
                };
                scope.register(
                    name,
                    Variable {
                        mutable: *mutable,
                        value: VariableValue::Initialized(var),
                    },
                );

                Ok(self.context.i64_type().const_zero().as_basic_value_enum())
            }

            // Must get the pointer type (lvalue) then save the value into place.
            Expr::Set { target, body } => {
                let (target_ptr, _target_llvm_type) = self.compile_lvalue_to_ptr(target, scope)?;

                let target_type = self.get_expr_type(target, scope, None)?;
                let body_type = self.get_expr_type(body, scope, Some(&target_type))?;

                if target_type != body_type {
                    anyhow::bail!(
                        "Type mismatch in assignment. Cannot assign type '{}' to '{}'",
                        body_type,
                        target_type
                    );
                }

                let body_val = self.compile_expr(body, scope, Some(&target_type))?;
                self.builder.build_store(target_ptr, body_val).unwrap();
                Ok(self.context.i64_type().const_zero().as_basic_value_enum())
            }

            // Tread all of these as accesses, simply load the value at the pointer
            Expr::Local(_) | Expr::Access { .. } | Expr::Index { .. } | Expr::Deref(_) => {
                let (ptr, llvm_type) = self.compile_lvalue_to_ptr(expr, scope)?;
                Ok(self.builder.build_load(llvm_type, ptr, "loadtmp").unwrap())
            }

            // Given a struct type, initialize the value.
            Expr::InitStruct { struct_, fields } => {
                let struct_type_val =
                    self.comptime
                        .solve(self.comptime_scope, self.comptime_funcs, struct_)?;
                let Value::Type(struct_type) = struct_type_val else {
                    anyhow::bail!("Expected a struct type, but got {:?}", struct_type_val);
                };

                let Type::Struct(struct_def) = &struct_type else {
                    anyhow::bail!("Cannot initialize non-struct type '{}'", struct_type);
                };

                if fields.len() != struct_def.fields.len() {
                    anyhow::bail!(
                        "Incorrect number of fields for struct '{}'. Expected {}, got {}",
                        struct_type,
                        struct_def.fields.len(),
                        fields.len()
                    );
                }

                let llvm_struct_type = self.to_llvm_type(&struct_type);
                let alloca = self
                    .builder
                    .build_alloca(llvm_struct_type, "struct_init")
                    .unwrap();

                for (field_name, field_expr) in fields {
                    let (field_index, (_, expected_type)) = struct_def
                        .fields
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| name == field_name)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Struct '{}' has no field named '{}'",
                                struct_type,
                                field_name
                            )
                        })?;

                    let actual_type = self.get_expr_type(field_expr, scope, Some(expected_type))?;
                    if actual_type != *expected_type {
                        anyhow::bail!(
                            "Type mismatch for field '{}'. Expected '{}', but got '{}'",
                            field_name,
                            expected_type,
                            actual_type
                        );
                    }

                    let field_ptr = self
                        .builder
                        .build_struct_gep(llvm_struct_type, alloca, field_index as u32, field_name)
                        .unwrap();
                    let field_val = self.compile_expr(field_expr, scope, Some(expected_type))?;
                    self.builder.build_store(field_ptr, field_val).unwrap();
                }

                Ok(self
                    .builder
                    .build_load(llvm_struct_type, alloca, "loadstruct")
                    .unwrap())
            }

            // An array of items, size is known.
            Expr::ArrayLiteral { values } => {
                if values.is_empty() {
                    anyhow::bail!("Cannot compile empty array literal without a type annotation");
                }

                let elem_type = if let Some(Type::Array(_, et)) = type_hint {
                    et.as_ref().clone()
                } else {
                    self.get_expr_type(&values[0], scope, None)?
                };

                let mut compiled_values = Vec::with_capacity(values.len());
                for (i, v) in values.iter().enumerate() {
                    let current_elem_type = self.get_expr_type(v, scope, Some(&elem_type))?;
                    if current_elem_type != elem_type {
                        anyhow::bail!(
                            "Mismatched types in array literal. Element 0 has type '{}', but element {} has type '{}'",
                            elem_type,
                            i,
                            current_elem_type
                        );
                    }
                    compiled_values.push(self.compile_expr(v, scope, Some(&elem_type))?);
                }

                let llvm_elem_type = self.to_llvm_type(&elem_type);
                Ok(unsafe {
                    ArrayValue::new_const_array(&llvm_elem_type, &compiled_values)
                        .as_basic_value_enum()
                })
            }

            // Repeat the given item x number of times.
            Expr::ArrayFill { value, size } => {
                let elem_type = if let Some(Type::Array(_, et)) = type_hint {
                    et.as_ref().clone()
                } else {
                    self.get_expr_type(value, scope, None)?
                };

                let size_val =
                    self.comptime
                        .solve(self.comptime_scope, self.comptime_funcs, size)?;
                let Value::Literal(Literal::Num(n)) = size_val else {
                    anyhow::bail!("Array fill size must be a comptime integer");
                };

                let compiled_value = self.compile_expr(value, scope, Some(&elem_type))?;
                let values = vec![compiled_value; n as usize];

                let llvm_elem_type = self.to_llvm_type(&elem_type);

                Ok(unsafe {
                    ArrayValue::new_const_array(&llvm_elem_type, &values).as_basic_value_enum()
                })
            }

            Expr::ArrayInit { .. } => {
                unimplemented!("ArrayInit is not yet fully supported for compilation")
            }

            // If statments are just condition (bool) then, and if an else exists, the else and a return type
            Expr::If { cond, body, else_ } => {
                let cond_type = self.get_expr_type(cond, scope, Some(&Type::Bool))?;
                if cond_type != Type::Bool {
                    anyhow::bail!("If condition must be a boolean, but got '{}'", cond_type);
                }
                let cond_val = self.compile_expr(cond, scope, None)?.into_int_value();

                let parent_fn = self.current_fn.unwrap();
                let then_bb = self.context.append_basic_block(parent_fn, "then");
                let else_bb = self.context.append_basic_block(parent_fn, "else");
                let merge_bb = self.context.append_basic_block(parent_fn, "merge");

                self.builder
                    .build_conditional_branch(cond_val, then_bb, else_bb)
                    .unwrap();

                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expr(body, scope, type_hint)?;
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                let else_val = if let Some(else_expr) = else_ {
                    let then_type = self.get_expr_type(body, scope, type_hint)?;
                    let else_type = self.get_expr_type(else_expr, scope, Some(&then_type))?;
                    if then_type != else_type {
                        anyhow::bail!(
                            "If branches have mismatched types: then is '{}', else is '{}'",
                            then_type,
                            else_type
                        );
                    }
                    self.compile_expr(else_expr, scope, Some(&then_type))?
                } else {
                    then_val.get_type().const_zero()
                };
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let else_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(merge_bb);
                let phi = self
                    .builder
                    .build_phi(then_val.get_type(), "iftmp")
                    .unwrap();
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value())
            }

            // Whiles are also just a condition, and a jump statement
            Expr::While { cond, body } => {
                let parent_fn = self.current_fn.unwrap();
                let cond_bb = self.context.append_basic_block(parent_fn, "while.cond");
                let loop_bb = self.context.append_basic_block(parent_fn, "while.body");
                let after_bb = self.context.append_basic_block(parent_fn, "while.end");

                self.builder.build_unconditional_branch(cond_bb).unwrap();
                self.builder.position_at_end(cond_bb);

                let cond_type = self.get_expr_type(cond, scope, Some(&Type::Bool))?;
                if cond_type != Type::Bool {
                    anyhow::bail!(
                        "While loop condition must be a boolean, but got '{}'",
                        cond_type
                    );
                }
                let cond_val = self
                    .compile_expr(cond, scope, Some(&Type::Bool))?
                    .into_int_value();
                self.builder
                    .build_conditional_branch(cond_val, loop_bb, after_bb)
                    .unwrap();

                self.builder.position_at_end(loop_bb);
                self.compile_expr(body, scope, None)?;
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(after_bb);
                Ok(self.context.i64_type().const_zero().as_basic_value_enum())
            }

            // For loops are a first, condition, inc, and body. Pretty simple still
            Expr::For {
                first,
                cond,
                each,
                body,
            } => {
                let parent_fn = self.current_fn.unwrap();
                scope.push_scope();

                self.compile_expr(first, scope, None)?;

                let cond_bb = self.context.append_basic_block(parent_fn, "for.cond");
                let loop_bb = self.context.append_basic_block(parent_fn, "for.body");
                let inc_bb = self.context.append_basic_block(parent_fn, "for.inc");
                let after_bb = self.context.append_basic_block(parent_fn, "for.end");

                self.builder.build_unconditional_branch(cond_bb).unwrap();
                self.builder.position_at_end(cond_bb);

                let cond_type = self.get_expr_type(cond, scope, Some(&Type::Bool))?;
                if cond_type != Type::Bool {
                    anyhow::bail!(
                        "For loop condition must be a boolean, but got '{}'",
                        cond_type
                    );
                }
                let cond_val = self
                    .compile_expr(cond, scope, Some(&Type::Bool))?
                    .into_int_value();
                self.builder
                    .build_conditional_branch(cond_val, loop_bb, after_bb)
                    .unwrap();

                self.builder.position_at_end(loop_bb);
                self.compile_expr(body, scope, None)?;
                self.builder.build_unconditional_branch(inc_bb).unwrap();

                self.builder.position_at_end(inc_bb);
                self.compile_expr(each, scope, None)?;
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(after_bb);
                scope.pop_scope();
                Ok(self.context.i64_type().const_zero().as_basic_value_enum())
            }

            // Simply Compile the first, then the second :)
            Expr::Then { first, next } => {
                self.compile_expr(first, scope, None)?;
                self.compile_expr(next, scope, type_hint)
            }

            // Blocks are an encapsulated scope, so pretty simple still
            Expr::Block { body } => {
                scope.push_scope();
                let result = self.compile_expr(body, scope, type_hint);
                scope.pop_scope();
                result
            }

            // Null expressions are temporarily just 0s
            Expr::Null => Ok(self.context.i64_type().const_zero().as_basic_value_enum()),

            // Functions are also temporarily just 0s
            Expr::Func { .. } | Expr::Struct { .. } => {
                Ok(self.context.i64_type().const_zero().as_basic_value_enum())
            }

            Expr::AddressOf(expr) => {
                let (ptr, _) = self.compile_lvalue_to_ptr(expr, scope)?;
                Ok(ptr.as_basic_value_enum())
            }
        }
    }
}
