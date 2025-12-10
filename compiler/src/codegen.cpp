#include "codegen.hpp"
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/Support/raw_ostream.h>
#include <sstream>

namespace shotgun {

// Lightweight best-effort type inference for codegen when no explicit type is available.
static ResolvedTypePtr infer_expr_type(const ExprPtr& expr) {
    if (!expr) return ResolvedType::make_void();

    return std::visit([](const auto& e) -> ResolvedTypePtr {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, Expr::IntLit>) {
            return ResolvedType::make_int();
        } else if constexpr (std::is_same_v<T, Expr::FloatLit>) {
            return ResolvedType::make_f64();
        } else if constexpr (std::is_same_v<T, Expr::StringLit>) {
            return ResolvedType::make_str();
        } else if constexpr (std::is_same_v<T, Expr::CharLit>) {
            return ResolvedType::make_char();
        } else if constexpr (std::is_same_v<T, Expr::BoolLit>) {
            return ResolvedType::make_bool();
        } else if constexpr (std::is_same_v<T, Expr::NoneLit>) {
            return ResolvedType::make_optional(ResolvedType::make_void());
        } else if constexpr (std::is_same_v<T, Expr::StructLit>) {
            // Variant construction uses Type.Case; map back to variant name
            auto pos = e.type_name.find('.');
            if (pos != std::string::npos) {
                return ResolvedType::make_variant(e.type_name.substr(0, pos));
            }
            return ResolvedType::make_struct(e.type_name);
        } else if constexpr (std::is_same_v<T, Expr::Ident>) {
            // Unknown without symbol info; leave void
            return ResolvedType::make_void();
        } else if constexpr (std::is_same_v<T, Expr::ArrayLit>) {
            if (e.elements.empty()) {
                return ResolvedType::make_array(ResolvedType::make_void());
            }
            auto elem_type = infer_expr_type(e.elements[0]);
            return ResolvedType::make_array(elem_type);
        } else {
            return ResolvedType::make_void();
        }
    }, expr->kind);
}

CodeGen::CodeGen(const std::string& module_name) {
    context_ = std::make_unique<llvm::LLVMContext>();
    module_ = std::make_unique<llvm::Module>(module_name, *context_);
    builder_ = std::make_unique<llvm::IRBuilder<>>(*context_);
}

bool CodeGen::generate(Program& prog, SymbolTable& symbols) {
    symbols_ = &symbols;

    // Generate runtime function declarations
    gen_runtime_decls();

    // First pass: declare all functions/structs (for forward references)
    for (const auto& decl : prog.decls) {
        if (auto* fn = std::get_if<FnDecl>(&decl)) {
            // Pre-declare function
            std::vector<llvm::Type*> param_types;
            for (const auto& p : fn->params) {
                if (p.type) {
                    param_types.push_back(get_llvm_type(p.type));
                }
            }

            llvm::Type* ret_type = fn->return_type
                ? get_llvm_type(fn->return_type)
                : llvm::Type::getVoidTy(*context_);

            llvm::FunctionType* fn_type = llvm::FunctionType::get(ret_type, param_types, false);
            llvm::Function::Create(fn_type, llvm::Function::ExternalLinkage,
                                   fn->name, module_.get());
        } else if (auto* s = std::get_if<StructDecl>(&decl)) {
            gen_struct_decl(*s);
        } else if (auto* v = std::get_if<VariantDecl>(&decl)) {
            gen_variant_decl(*v);
        }
    }

    // Second pass: generate all declarations
    for (const auto& decl : prog.decls) {
        gen_decl(decl);
    }

    // Verify the module
    std::string err_str;
    llvm::raw_string_ostream err_stream(err_str);
    if (llvm::verifyModule(*module_, &err_stream)) {
        error("Module verification failed: " + err_str);
        return false;
    }

    return !has_errors();
}

std::string CodeGen::get_ir() const {
    std::string str;
    llvm::raw_string_ostream os(str);
    module_->print(os, nullptr);
    return str;
}

// Type mapping

llvm::Type* CodeGen::get_llvm_type(ResolvedTypePtr type) {
    if (!type) return llvm::Type::getVoidTy(*context_);

    switch (type->kind) {
        case ResolvedType::Kind::Void:
            return llvm::Type::getVoidTy(*context_);
        case ResolvedType::Kind::Int:
            return llvm::Type::getInt64Ty(*context_);
        case ResolvedType::Kind::Bool:
            return llvm::Type::getInt1Ty(*context_);
        case ResolvedType::Kind::Str:
            return llvm::PointerType::get(*context_, 0); // opaque pointer
        case ResolvedType::Kind::Char:
            return llvm::Type::getInt8Ty(*context_);
        case ResolvedType::Kind::F32:
            return llvm::Type::getFloatTy(*context_);
        case ResolvedType::Kind::F64:
            return llvm::Type::getDoubleTy(*context_);
        case ResolvedType::Kind::U32:
            return llvm::Type::getInt32Ty(*context_);
        case ResolvedType::Kind::U64:
            return llvm::Type::getInt64Ty(*context_);

        case ResolvedType::Kind::Array: {
            // Arrays are represented as {i64 len, ptr}
            return llvm::StructType::get(*context_, {
                llvm::Type::getInt64Ty(*context_),  // len
                llvm::PointerType::get(*context_, 0)  // data ptr (opaque)
            });
        }

        case ResolvedType::Kind::Optional: {
            // Optional is {i1 has_value, T value}
            llvm::Type* inner = get_llvm_type(type->element_type);
            return llvm::StructType::get(*context_, {
                llvm::Type::getInt1Ty(*context_),  // has_value
                inner                               // value
            });
        }

        case ResolvedType::Kind::Channel:
            // Channels are opaque pointers (runtime handles them)
            return llvm::PointerType::get(*context_, 0);

        case ResolvedType::Kind::Function: {
            // Function pointers are opaque pointers in LLVM 21
            return llvm::PointerType::get(*context_, 0);
        }

        case ResolvedType::Kind::Struct:
            return get_or_create_struct_type(type->name);

        case ResolvedType::Kind::Variant:
            return get_or_create_struct_type(type->name);

        case ResolvedType::Kind::Error:
            return get_or_create_struct_type(type->name);

        case ResolvedType::Kind::TypeParam:
            // Type params should be resolved by now; default to i64
            error("Unresolved type parameter: " + type->param_name);
            return llvm::Type::getInt64Ty(*context_);
    }

    return llvm::Type::getVoidTy(*context_);
}

llvm::Type* CodeGen::get_llvm_type(const TypePtr& ast_type) {
    if (!ast_type) return llvm::Type::getVoidTy(*context_);

    if (auto* named = std::get_if<Type::Named>(&ast_type->kind)) {
        // Primitive types
        if (named->name == "int") return llvm::Type::getInt64Ty(*context_);
        if (named->name == "bool") return llvm::Type::getInt1Ty(*context_);
        if (named->name == "str") return llvm::PointerType::get(*context_, 0);
        if (named->name == "char") return llvm::Type::getInt8Ty(*context_);
        if (named->name == "f32") return llvm::Type::getFloatTy(*context_);
        if (named->name == "f64") return llvm::Type::getDoubleTy(*context_);
        if (named->name == "u32") return llvm::Type::getInt32Ty(*context_);
        if (named->name == "u64") return llvm::Type::getInt64Ty(*context_);

        // User-defined types
        return get_or_create_struct_type(named->name);
    }

    if (auto* arr = std::get_if<Type::Array>(&ast_type->kind)) {
        return llvm::StructType::get(*context_, {
            llvm::Type::getInt64Ty(*context_),
            llvm::PointerType::get(*context_, 0)
        });
    }

    if (auto* opt = std::get_if<Type::Optional>(&ast_type->kind)) {
        llvm::Type* inner = get_llvm_type(opt->inner);
        return llvm::StructType::get(*context_, {
            llvm::Type::getInt1Ty(*context_), inner
        });
    }

    if (auto* fn = std::get_if<Type::Function>(&ast_type->kind)) {
        // Function pointers are opaque pointers in LLVM 21
        return llvm::PointerType::get(*context_, 0);
    }

    if (auto* chan = std::get_if<Type::Channel>(&ast_type->kind)) {
        return llvm::PointerType::get(*context_, 0);
    }

    return llvm::Type::getVoidTy(*context_);
}

// Helper to convert AST type to resolved type (simplified version for codegen)
ResolvedTypePtr CodeGen::ast_type_to_resolved(const TypePtr& ast_type) {
    if (!ast_type) return ResolvedType::make_void();

    if (auto* named = std::get_if<Type::Named>(&ast_type->kind)) {
        if (named->name == "int") return ResolvedType::make_int();
        if (named->name == "bool") return ResolvedType::make_bool();
        if (named->name == "str") return ResolvedType::make_str();
        if (named->name == "char") return ResolvedType::make_char();
        if (named->name == "f32") return ResolvedType::make_f32();
        if (named->name == "f64") return ResolvedType::make_f64();
        if (named->name == "u32") return ResolvedType::make_u32();
        if (named->name == "u64") return ResolvedType::make_u64();
        return ResolvedType::make_struct(named->name);
    }

    if (auto* arr = std::get_if<Type::Array>(&ast_type->kind)) {
        return ResolvedType::make_array(ast_type_to_resolved(arr->element));
    }

    if (auto* opt = std::get_if<Type::Optional>(&ast_type->kind)) {
        return ResolvedType::make_optional(ast_type_to_resolved(opt->inner));
    }

    return ResolvedType::make_void();
}

llvm::StructType* CodeGen::get_or_create_struct_type(const std::string& name) {
    auto it = struct_types_.find(name);
    if (it != struct_types_.end()) {
        return it->second;
    }

    // Create opaque struct first (for recursive types)
    auto* st = llvm::StructType::create(*context_, name);
    struct_types_[name] = st;

    // Look up the struct definition
    if (auto def = symbols_->lookup_struct(name)) {
        std::vector<llvm::Type*> field_types;
        for (const auto& f : def->fields) {
            field_types.push_back(get_llvm_type(f.type));
        }
        st->setBody(field_types);
    } else if (auto def = symbols_->lookup_variant(name)) {
        // Variants: {i32 tag, union of cases}
        // For simplicity, use largest case + tag
        size_t max_size = 0;
        for (const auto& c : def->cases) {
            size_t case_size = 0;
            for (const auto& f : c.fields) {
                // Approximate size
                case_size += 8; // assume 8 bytes per field
            }
            max_size = std::max(max_size, case_size);
        }
        // Tag + padding for largest case
        std::vector<llvm::Type*> types;
        types.push_back(llvm::Type::getInt32Ty(*context_)); // tag
        if (max_size > 0) {
            types.push_back(llvm::ArrayType::get(
                llvm::Type::getInt8Ty(*context_), max_size));
        }
        st->setBody(types);
    } else if (auto def = symbols_->lookup_error(name)) {
        std::vector<llvm::Type*> field_types;
        for (const auto& f : def->fields) {
            field_types.push_back(get_llvm_type(f.type));
        }
        st->setBody(field_types);
    }

    return st;
}

// Runtime declarations

void CodeGen::gen_runtime_decls() {
    // printf for print function
    auto* printf_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_),
        {llvm::PointerType::get(*context_, 0)},
        true  // vararg
    );
    llvm::Function::Create(printf_type, llvm::Function::ExternalLinkage,
                           "printf", module_.get());

    // malloc
    auto* malloc_type = llvm::FunctionType::get(
        llvm::PointerType::get(*context_, 0),
        {llvm::Type::getInt64Ty(*context_)},
        false
    );
    llvm::Function::Create(malloc_type, llvm::Function::ExternalLinkage,
                           "malloc", module_.get());

    // free
    auto* free_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*context_),
        {llvm::PointerType::get(*context_, 0)},
        false
    );
    llvm::Function::Create(free_type, llvm::Function::ExternalLinkage,
                           "free", module_.get());

    // snprintf for string interpolation
    auto* snprintf_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_),
        {llvm::PointerType::get(*context_, 0),  // buf
         llvm::Type::getInt64Ty(*context_),     // size
         llvm::PointerType::get(*context_, 0)}, // format
        true  // vararg
    );
    llvm::Function::Create(snprintf_type, llvm::Function::ExternalLinkage,
                           "snprintf", module_.get());

    // exit for assertion failures
    auto* exit_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*context_),
        {llvm::Type::getInt32Ty(*context_)},
        false
    );
    llvm::Function::Create(exit_type, llvm::Function::ExternalLinkage,
                           "exit", module_.get());

    // fprintf for assertion error output
    auto* fprintf_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_),
        {llvm::PointerType::get(*context_, 0),  // FILE* stream
         llvm::PointerType::get(*context_, 0)}, // format
        true  // vararg
    );
    llvm::Function::Create(fprintf_type, llvm::Function::ExternalLinkage,
                           "fprintf", module_.get());

    // Declare stderr as external global variable
    new llvm::GlobalVariable(
        *module_,
        llvm::PointerType::get(*context_, 0),
        false,
        llvm::GlobalValue::ExternalLinkage,
        nullptr,
        "stderr"
    );

    // strcmp for string comparison
    auto* strcmp_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_),
        {llvm::PointerType::get(*context_, 0),
         llvm::PointerType::get(*context_, 0)},
        false
    );
    llvm::Function::Create(strcmp_type, llvm::Function::ExternalLinkage,
                           "strcmp", module_.get());
}

// Declaration generation

void CodeGen::gen_decl(const Decl& decl) {
    std::visit([this](auto&& d) {
        using T = std::decay_t<decltype(d)>;
        if constexpr (std::is_same_v<T, FnDecl>) {
            gen_fn_decl(d);
        } else if constexpr (std::is_same_v<T, StructDecl>) {
            // Already handled in first pass
        } else if constexpr (std::is_same_v<T, VariantDecl>) {
            // Already handled in first pass
        } else if constexpr (std::is_same_v<T, ImplDecl>) {
            gen_impl_decl(d);
        } else if constexpr (std::is_same_v<T, TraitDecl>) {
            // Traits don't generate code directly
        } else if constexpr (std::is_same_v<T, ErrorDecl>) {
            // Error types are just structs
        } else if constexpr (std::is_same_v<T, UseDecl>) {
            // Imports don't generate code
        }
    }, decl);
}

void CodeGen::gen_struct_decl(const StructDecl& s) {
    // Just ensure the type is created
    get_or_create_struct_type(s.name);
}

void CodeGen::gen_variant_decl(const VariantDecl& v) {
    // Just ensure the type is created
    get_or_create_struct_type(v.name);
}

void CodeGen::gen_fn_decl(const FnDecl& fn) {
    // Get the function (already declared)
    llvm::Function* func = module_->getFunction(fn.name);
    if (!func) {
        error("Function not found: " + fn.name);
        return;
    }

    // Create entry block
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context_, "entry", func);
    builder_->SetInsertPoint(entry);

    current_function_ = func;
    named_values_.clear();

    // Create allocas for parameters
    size_t idx = 0;
    for (auto& arg : func->args()) {
        const auto& param = fn.params[idx];
        arg.setName(param.name);

        llvm::AllocaInst* alloca = create_entry_alloca(func, param.name, arg.getType());
        builder_->CreateStore(&arg, alloca);
        named_values_[param.name] = alloca;
        idx++;
    }

    // Generate body
    gen_block(fn.body);

    // Add implicit return if needed
    if (!builder_->GetInsertBlock()->getTerminator()) {
        if (fn.return_type) {
            // Return default value
            llvm::Type* ret_type = get_llvm_type(fn.return_type);
            builder_->CreateRet(llvm::Constant::getNullValue(ret_type));
        } else {
            builder_->CreateRetVoid();
        }
    }

    current_function_ = nullptr;
}

void CodeGen::gen_impl_decl(const ImplDecl& impl) {
    for (const auto& method : impl.methods) {
        // Mangle name as TypeName_methodName
        std::string mangled = impl.type_name + "_" + method.name;

        // Build parameter types including self (opaque pointer)
        std::vector<llvm::Type*> param_types;
        llvm::Type* self_type = llvm::PointerType::get(*context_, 0);
        param_types.push_back(self_type);

        for (const auto& p : method.params) {
            if (p.name != "self" && p.type) {
                param_types.push_back(get_llvm_type(p.type));
            }
        }

        llvm::Type* ret_type = method.return_type
            ? get_llvm_type(method.return_type)
            : llvm::Type::getVoidTy(*context_);

        llvm::FunctionType* fn_type = llvm::FunctionType::get(ret_type, param_types, false);
        llvm::Function* func = llvm::Function::Create(
            fn_type, llvm::Function::ExternalLinkage, mangled, module_.get());

        // Create entry
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context_, "entry", func);
        builder_->SetInsertPoint(entry);

        current_function_ = func;
        named_values_.clear();

        // Name and store parameters
        auto arg_it = func->arg_begin();
        arg_it->setName("self");
        // Keep self as a pointer directly to the struct
        llvm::AllocaInst* self_alloca = create_entry_alloca(func, "self", self_type);
        builder_->CreateStore(&*arg_it, self_alloca);
        named_values_["self"] = self_alloca;
        var_types_["self"] = ResolvedType::make_struct(impl.type_name, {});
        ++arg_it;

        for (const auto& p : method.params) {
            if (p.name != "self" && p.type) {
                arg_it->setName(p.name);
                llvm::AllocaInst* alloca = create_entry_alloca(func, p.name, arg_it->getType());
                builder_->CreateStore(&*arg_it, alloca);
                named_values_[p.name] = alloca;
                ++arg_it;
            }
        }

        gen_block(method.body);

        if (!builder_->GetInsertBlock()->getTerminator()) {
            if (method.return_type) {
                builder_->CreateRet(llvm::Constant::getNullValue(ret_type));
            } else {
                builder_->CreateRetVoid();
            }
        }

        current_function_ = nullptr;
    }
}

// Statement generation

void CodeGen::gen_stmt(const StmtPtr& stmt) {
    if (!stmt) return;

    std::visit([this](auto&& s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, Stmt::VarDecl>) {
            gen_var_decl(s);
        } else if constexpr (std::is_same_v<T, Stmt::Assign>) {
            gen_assign(s);
        } else if constexpr (std::is_same_v<T, Stmt::ExprStmt>) {
            gen_expr_stmt(s);
        } else if constexpr (std::is_same_v<T, Stmt::Return>) {
            gen_return(s);
        } else if constexpr (std::is_same_v<T, Stmt::If>) {
            gen_if(s);
        } else if constexpr (std::is_same_v<T, Stmt::While>) {
            gen_while(s);
        } else if constexpr (std::is_same_v<T, Stmt::For>) {
            gen_for(s);
        } else if constexpr (std::is_same_v<T, Stmt::Break>) {
            if (break_target_) {
                builder_->CreateBr(break_target_);
            }
        } else if constexpr (std::is_same_v<T, Stmt::Continue>) {
            if (continue_target_) {
                builder_->CreateBr(continue_target_);
            }
        } else if constexpr (std::is_same_v<T, Stmt::Go>) {
            gen_go(s);
        } else if constexpr (std::is_same_v<T, Stmt::ChanSend>) {
            // TODO: channel send
        }
    }, stmt->kind);
}

void CodeGen::gen_block(const std::vector<StmtPtr>& block) {
    for (const auto& stmt : block) {
        gen_stmt(stmt);
        // Stop if we hit a terminator
        if (builder_->GetInsertBlock()->getTerminator()) {
            break;
        }
    }
}

void CodeGen::gen_var_decl(const Stmt::VarDecl& var) {
    llvm::Value* init_val = var.init ? gen_expr(var.init) : nullptr;
    if (!init_val) return;

    // Wrap optionals when the declaration type is an optional
    llvm::Type* stored_type = init_val->getType();
    if (var.type) {
        if (auto* opt = std::get_if<Type::Optional>(&var.type->kind)) {
            llvm::Type* opt_ty = get_llvm_type(var.type);
            if (opt_ty) {
                stored_type = opt_ty;
                if (init_val->getType() != opt_ty) {
                    llvm::Type* inner_ty = get_llvm_type(opt->inner);
                    bool is_none = std::holds_alternative<Expr::NoneLit>(var.init->kind);
                    llvm::Value* value_part = is_none
                        ? llvm::Constant::getNullValue(inner_ty)
                        : init_val;
                    llvm::Value* opt_val = llvm::UndefValue::get(opt_ty);
                    llvm::Value* has_flag = llvm::ConstantInt::get(
                        llvm::Type::getInt1Ty(*context_), is_none ? 0 : 1);
                    opt_val = builder_->CreateInsertValue(opt_val, has_flag, 0);
                    opt_val = builder_->CreateInsertValue(opt_val, value_part, 1);
                    init_val = opt_val;
                }
            }
        }
    }

    llvm::AllocaInst* alloca = create_entry_alloca(
        current_function_, var.name, stored_type);
    builder_->CreateStore(init_val, alloca);
    named_values_[var.name] = alloca;

    // Store type for later use (e.g., array element type lookup, field assignment)
    if (var.type) {
        // Convert AST type to resolved type for array element access
        if (auto* arr = std::get_if<Type::Array>(&var.type->kind)) {
            var_types_[var.name] = ResolvedType::make_array(
                ast_type_to_resolved(arr->element));
        } else if (auto* named = std::get_if<Type::Named>(&var.type->kind)) {
            // Track struct/variant types for field assignment and is-expr
            if (symbols_->lookup_struct(named->name)) {
                var_types_[var.name] = ResolvedType::make_struct(named->name);
            } else if (symbols_->lookup_variant(named->name)) {
                var_types_[var.name] = ResolvedType::make_variant(named->name);
            }
        }
    } else if (var.init) {
        var_types_[var.name] = infer_expr_type(var.init);
    }
}

void CodeGen::gen_assign(const Stmt::Assign& assign) {
    llvm::Value* val = gen_expr(assign.value);
    if (!val) return;

    // Handle compound assignment
    if (assign.op != TokenKind::Assign) {
        llvm::Value* old_val = gen_expr(assign.target);
        if (!old_val) return;

        switch (assign.op) {
            case TokenKind::PlusEq:
                if (val->getType()->isIntegerTy()) {
                    val = builder_->CreateAdd(old_val, val, "addtmp");
                } else {
                    val = builder_->CreateFAdd(old_val, val, "faddtmp");
                }
                break;
            case TokenKind::MinusEq:
                if (val->getType()->isIntegerTy()) {
                    val = builder_->CreateSub(old_val, val, "subtmp");
                } else {
                    val = builder_->CreateFSub(old_val, val, "fsubtmp");
                }
                break;
            case TokenKind::StarEq:
                if (val->getType()->isIntegerTy()) {
                    val = builder_->CreateMul(old_val, val, "multmp");
                } else {
                    val = builder_->CreateFMul(old_val, val, "fmultmp");
                }
                break;
            case TokenKind::SlashEq:
                if (val->getType()->isIntegerTy()) {
                    val = builder_->CreateSDiv(old_val, val, "divtmp");
                } else {
                    val = builder_->CreateFDiv(old_val, val, "fdivtmp");
                }
                break;
            default:
                break;
        }
    }

    // Get the target address
    if (auto* ident = std::get_if<Expr::Ident>(&assign.target->kind)) {
        auto it = named_values_.find(ident->name);
        if (it != named_values_.end()) {
            builder_->CreateStore(val, it->second);
        }
    } else if (auto* field = std::get_if<Expr::Field>(&assign.target->kind)) {
        // Field assignment - need to get the variable alloca, not its value
        if (auto* ident = std::get_if<Expr::Ident>(&field->object->kind)) {
            auto alloca_it = named_values_.find(ident->name);
            auto type_it = var_types_.find(ident->name);
            if (alloca_it != named_values_.end() && type_it != var_types_.end() &&
                type_it->second->kind == ResolvedType::Kind::Struct) {
                // Base pointer to struct storage
                llvm::Value* struct_ptr = alloca_it->second;
                if (auto* ai = llvm::dyn_cast<llvm::AllocaInst>(struct_ptr)) {
                    llvm::Type* alloc_ty = ai->getAllocatedType();
                    if (alloc_ty->isPointerTy()) {
                        struct_ptr = builder_->CreateLoad(alloc_ty, struct_ptr, ident->name + "_ptr");
                    }
                }

                // Get struct type and find field index
                std::string struct_name = type_it->second->name;
                llvm::StructType* struct_ty = get_or_create_struct_type(struct_name);
                unsigned field_idx = 0;

                if (auto def = symbols_->lookup_struct(struct_name)) {
                    for (size_t i = 0; i < def->fields.size(); ++i) {
                        if (def->fields[i].name == field->name) {
                            field_idx = i;
                            break;
                        }
                    }
                }

                // GEP to field, store value
                llvm::Value* field_ptr = builder_->CreateStructGEP(
                    struct_ty, struct_ptr, field_idx, field->name + "_ptr");
                builder_->CreateStore(val, field_ptr);
            }
        }
    } else if (auto* idx = std::get_if<Expr::Index>(&assign.target->kind)) {
        // Index assignment for arrays
        llvm::Value* index = gen_expr(idx->index);
        if (!index) return;

        llvm::Value* data_ptr = nullptr;
        llvm::Type* elem_ty = val->getType();

        if (auto* ident = std::get_if<Expr::Ident>(&idx->object->kind)) {
            auto alloca_it = named_values_.find(ident->name);
            auto type_it = var_types_.find(ident->name);
            if (alloca_it != named_values_.end() && type_it != var_types_.end() &&
                type_it->second->kind == ResolvedType::Kind::Array) {
                llvm::Value* arr_val = builder_->CreateLoad(alloca_it->second->getAllocatedType(),
                                                            alloca_it->second, ident->name + "_arr");
                data_ptr = builder_->CreateExtractValue(arr_val, 1, "data_ptr");
                elem_ty = get_llvm_type(type_it->second->element_type);
            }
        }

        if (!data_ptr) {
            // Fallback: evaluate object and try to extract data pointer
            llvm::Value* obj = gen_expr(idx->object);
            if (!obj || !obj->getType()->isStructTy()) return;
            data_ptr = builder_->CreateExtractValue(obj, 1, "data_ptr");
        }

        llvm::Value* elem_ptr = builder_->CreateGEP(elem_ty, data_ptr, index, "elemptr");
        builder_->CreateStore(val, elem_ptr);
    }
}

void CodeGen::gen_if(const Stmt::If& if_stmt) {
    llvm::Value* cond = gen_expr(if_stmt.condition);
    if (!cond) return;

    // Convert to i1 if needed
    if (!cond->getType()->isIntegerTy(1)) {
        cond = builder_->CreateICmpNE(cond,
            llvm::Constant::getNullValue(cond->getType()), "ifcond");
    }

    llvm::Function* func = current_function_;
    llvm::BasicBlock* then_bb = llvm::BasicBlock::Create(*context_, "then", func);
    llvm::BasicBlock* else_bb = llvm::BasicBlock::Create(*context_, "else");
    llvm::BasicBlock* merge_bb = llvm::BasicBlock::Create(*context_, "ifcont");

    builder_->CreateCondBr(cond, then_bb, else_bb);

    // Then block
    builder_->SetInsertPoint(then_bb);
    gen_block(if_stmt.then_block);
    if (!builder_->GetInsertBlock()->getTerminator()) {
        builder_->CreateBr(merge_bb);
    }

    // Else block
    func->insert(func->end(), else_bb);
    builder_->SetInsertPoint(else_bb);
    if (!if_stmt.else_block.empty()) {
        gen_block(if_stmt.else_block);
    }
    if (!builder_->GetInsertBlock()->getTerminator()) {
        builder_->CreateBr(merge_bb);
    }

    // Merge block
    func->insert(func->end(), merge_bb);
    builder_->SetInsertPoint(merge_bb);
}

void CodeGen::gen_while(const Stmt::While& while_stmt) {
    llvm::Function* func = current_function_;
    llvm::BasicBlock* cond_bb = llvm::BasicBlock::Create(*context_, "whilecond", func);
    llvm::BasicBlock* body_bb = llvm::BasicBlock::Create(*context_, "whilebody");
    llvm::BasicBlock* after_bb = llvm::BasicBlock::Create(*context_, "whileend");

    // Save break/continue targets
    llvm::BasicBlock* old_break = break_target_;
    llvm::BasicBlock* old_continue = continue_target_;
    break_target_ = after_bb;
    continue_target_ = cond_bb;

    builder_->CreateBr(cond_bb);
    builder_->SetInsertPoint(cond_bb);

    llvm::Value* cond = gen_expr(while_stmt.condition);
    if (cond && !cond->getType()->isIntegerTy(1)) {
        cond = builder_->CreateICmpNE(cond,
            llvm::Constant::getNullValue(cond->getType()), "whilecond");
    }

    builder_->CreateCondBr(cond, body_bb, after_bb);

    func->insert(func->end(), body_bb);
    builder_->SetInsertPoint(body_bb);
    gen_block(while_stmt.body);
    if (!builder_->GetInsertBlock()->getTerminator()) {
        builder_->CreateBr(cond_bb);
    }

    func->insert(func->end(), after_bb);
    builder_->SetInsertPoint(after_bb);

    break_target_ = old_break;
    continue_target_ = old_continue;
}

void CodeGen::gen_for(const Stmt::For& for_stmt) {
    llvm::Value* iterable = gen_expr(for_stmt.iterable);
    if (!iterable) return;

    llvm::Function* func = current_function_;

    // Determine element type using stored var info or best-effort inference
    ResolvedTypePtr iter_type = infer_expr_type(for_stmt.iterable);
    if (auto* ident = std::get_if<Expr::Ident>(&for_stmt.iterable->kind)) {
        auto it = var_types_.find(ident->name);
        if (it != var_types_.end()) {
            iter_type = it->second;
        }
    }

    llvm::Type* elem_llvm_ty = llvm::Type::getInt64Ty(*context_); // default fallback
    if (iter_type && iter_type->kind == ResolvedType::Kind::Array && iter_type->element_type) {
        elem_llvm_ty = get_llvm_type(iter_type->element_type);
        var_types_[for_stmt.var] = iter_type->element_type;
    }

    // For arrays: iterate from 0 to len
    llvm::AllocaInst* idx_alloca = create_entry_alloca(func, "__idx",
        llvm::Type::getInt64Ty(*context_));
    builder_->CreateStore(llvm::ConstantInt::get(
        llvm::Type::getInt64Ty(*context_), 0), idx_alloca);

    llvm::BasicBlock* cond_bb = llvm::BasicBlock::Create(*context_, "forcond", func);
    llvm::BasicBlock* body_bb = llvm::BasicBlock::Create(*context_, "forbody");
    llvm::BasicBlock* incr_bb = llvm::BasicBlock::Create(*context_, "forincr");
    llvm::BasicBlock* after_bb = llvm::BasicBlock::Create(*context_, "forend");

    llvm::BasicBlock* old_break = break_target_;
    llvm::BasicBlock* old_continue = continue_target_;
    break_target_ = after_bb;
    continue_target_ = incr_bb;

    builder_->CreateBr(cond_bb);
    builder_->SetInsertPoint(cond_bb);

    llvm::Value* idx = builder_->CreateLoad(
        llvm::Type::getInt64Ty(*context_), idx_alloca, "idx");

    // Get array length (first field of array struct)
    llvm::Value* len = nullptr;
    if (iterable->getType()->isStructTy()) {
        len = builder_->CreateExtractValue(iterable, 0, "len");
    } else {
        len = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 0);
    }
    llvm::Value* cond = builder_->CreateICmpSLT(idx, len, "forcond");
    builder_->CreateCondBr(cond, body_bb, after_bb);

    func->insert(func->end(), body_bb);
    builder_->SetInsertPoint(body_bb);

    // Load current element and bind loop variable
    llvm::Value* data_ptr = nullptr;
    if (iterable->getType()->isStructTy()) {
        data_ptr = builder_->CreateExtractValue(iterable, 1, "data");
    } else {
        data_ptr = llvm::ConstantPointerNull::get(llvm::PointerType::get(*context_, 0));
    }
    llvm::Value* elem_ptr = builder_->CreateGEP(elem_llvm_ty, data_ptr, idx, "elemptr");
    llvm::Value* elem = builder_->CreateLoad(elem_llvm_ty, elem_ptr, for_stmt.var);

    llvm::AllocaInst* var_alloca = create_entry_alloca(func, for_stmt.var, elem_llvm_ty);
    builder_->CreateStore(elem, var_alloca);
    named_values_[for_stmt.var] = var_alloca;

    gen_block(for_stmt.body);
    if (!builder_->GetInsertBlock()->getTerminator()) {
        builder_->CreateBr(incr_bb);
    }

    func->insert(func->end(), incr_bb);
    builder_->SetInsertPoint(incr_bb);

    llvm::Value* next_idx = builder_->CreateAdd(idx,
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 1), "nextidx");
    builder_->CreateStore(next_idx, idx_alloca);
    builder_->CreateBr(cond_bb);

    func->insert(func->end(), after_bb);
    builder_->SetInsertPoint(after_bb);

    break_target_ = old_break;
    continue_target_ = old_continue;
}

void CodeGen::gen_return(const Stmt::Return& ret) {
    if (ret.value) {
        llvm::Value* val = gen_expr(ret.value);
        if (val) {
            builder_->CreateRet(val);
        }
    } else {
        builder_->CreateRetVoid();
    }
}

void CodeGen::gen_expr_stmt(const Stmt::ExprStmt& expr) {
    gen_expr(expr.expr);
}

void CodeGen::gen_go(const Stmt::Go& go) {
    // TODO: goroutine support - for now just call directly
    gen_expr(go.call);
}

// Expression generation

llvm::Value* CodeGen::gen_expr(const ExprPtr& expr) {
    if (!expr) return nullptr;

    return std::visit([this](auto&& e) -> llvm::Value* {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, Expr::IntLit>) {
            return gen_int_lit(e);
        } else if constexpr (std::is_same_v<T, Expr::FloatLit>) {
            return gen_float_lit(e);
        } else if constexpr (std::is_same_v<T, Expr::StringLit>) {
            return gen_string_lit(e);
        } else if constexpr (std::is_same_v<T, Expr::CharLit>) {
            return gen_char_lit(e);
        } else if constexpr (std::is_same_v<T, Expr::BoolLit>) {
            return gen_bool_lit(e);
        } else if constexpr (std::is_same_v<T, Expr::NoneLit>) {
            return gen_none_lit(e);
        } else if constexpr (std::is_same_v<T, Expr::Ident>) {
            return gen_ident(e);
        } else if constexpr (std::is_same_v<T, Expr::Binary>) {
            return gen_binary(e);
        } else if constexpr (std::is_same_v<T, Expr::Unary>) {
            return gen_unary(e);
        } else if constexpr (std::is_same_v<T, Expr::Call>) {
            return gen_call(e);
        } else if constexpr (std::is_same_v<T, Expr::Index>) {
            return gen_index(e);
        } else if constexpr (std::is_same_v<T, Expr::Field>) {
            return gen_field(e);
        } else if constexpr (std::is_same_v<T, Expr::StructLit>) {
            return gen_struct_lit(e);
        } else if constexpr (std::is_same_v<T, Expr::ArrayLit>) {
            return gen_array_lit(e);
        } else if constexpr (std::is_same_v<T, Expr::Lambda>) {
            return gen_lambda(e);
        } else if constexpr (std::is_same_v<T, Expr::Match>) {
            return gen_match(e);
        } else if constexpr (std::is_same_v<T, Expr::Or>) {
            return gen_or_expr(e);
        } else if constexpr (std::is_same_v<T, Expr::Cast>) {
            return gen_cast(e);
        } else if constexpr (std::is_same_v<T, Expr::Is>) {
            return gen_is_expr(e);
        } else if constexpr (std::is_same_v<T, Expr::Block>) {
            return gen_block_expr(e);
        } else if constexpr (std::is_same_v<T, Expr::ChanRecv>) {
            // TODO: channel receive
            return nullptr;
        }
        return nullptr;
    }, expr->kind);
}

llvm::Value* CodeGen::gen_int_lit(const Expr::IntLit& lit) {
    return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), lit.value);
}

llvm::Value* CodeGen::gen_float_lit(const Expr::FloatLit& lit) {
    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(*context_), lit.value);
}

llvm::Value* CodeGen::gen_string_lit(const Expr::StringLit& lit) {
    // Simple string - no interpolation
    if (lit.parts.empty()) {
        return builder_->CreateGlobalString(lit.value, "str");
    }

    // F-string with interpolation
    // Build format string and collect values
    std::string format;
    std::vector<llvm::Value*> values;
    size_t expr_idx = 0;

    for (const auto& part : lit.parts) {
        if (!part.is_expr) {
            // Literal part - escape % for printf
            for (char c : part.literal) {
                if (c == '%') format += "%%";
                else format += c;
            }
        } else {
            // Expression part - generate value and determine format
            if (expr_idx < lit.exprs.size()) {
                llvm::Value* val = gen_expr(lit.exprs[expr_idx]);
                if (val) {
                    if (val->getType()->isIntegerTy(64)) {
                        format += "%ld";
                        values.push_back(val);
                    } else if (val->getType()->isIntegerTy(1)) {
                        // bool - convert to string
                        format += "%s";
                        llvm::Value* true_str = builder_->CreateGlobalString("true");
                        llvm::Value* false_str = builder_->CreateGlobalString("false");
                        llvm::Value* str = builder_->CreateSelect(val, true_str, false_str);
                        values.push_back(str);
                    } else if (val->getType()->isDoubleTy()) {
                        format += "%f";
                        values.push_back(val);
                    } else if (val->getType()->isPointerTy()) {
                        format += "%s";
                        values.push_back(val);
                    } else if (val->getType()->isIntegerTy(8)) {
                        format += "%c";
                        values.push_back(val);
                    } else {
                        format += "%d";
                        values.push_back(val);
                    }
                }
                expr_idx++;
            }
        }
    }

    // Get snprintf and malloc
    llvm::Function* snprintf_fn = module_->getFunction("snprintf");
    llvm::Function* malloc_fn = module_->getFunction("malloc");
    if (!snprintf_fn || !malloc_fn) {
        return builder_->CreateGlobalString("<f-string error>", "str");
    }

    llvm::Value* fmt = builder_->CreateGlobalString(format, "fmt");

    // First call: snprintf(NULL, 0, fmt, ...) to get required size
    std::vector<llvm::Value*> size_args;
    size_args.push_back(llvm::ConstantPointerNull::get(llvm::PointerType::get(*context_, 0)));
    size_args.push_back(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 0));
    size_args.push_back(fmt);
    for (auto* v : values) size_args.push_back(v);

    llvm::Value* len = builder_->CreateCall(snprintf_fn, size_args, "len");

    // Add 1 for null terminator
    llvm::Value* buf_size = builder_->CreateAdd(
        builder_->CreateSExt(len, llvm::Type::getInt64Ty(*context_)),
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 1),
        "buf_size"
    );

    // Allocate buffer
    llvm::Value* buf = builder_->CreateCall(malloc_fn, {buf_size}, "buf");

    // Second call: snprintf(buf, buf_size, fmt, ...) to fill buffer
    std::vector<llvm::Value*> fill_args;
    fill_args.push_back(buf);
    fill_args.push_back(buf_size);
    fill_args.push_back(fmt);
    for (auto* v : values) fill_args.push_back(v);

    builder_->CreateCall(snprintf_fn, fill_args);

    return buf;
}

llvm::Value* CodeGen::gen_char_lit(const Expr::CharLit& lit) {
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_), lit.value);
}

llvm::Value* CodeGen::gen_bool_lit(const Expr::BoolLit& lit) {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), lit.value ? 1 : 0);
}

llvm::Value* CodeGen::gen_none_lit(const Expr::NoneLit& lit) {
    // Return null pointer
    return llvm::ConstantPointerNull::get(llvm::PointerType::get(*context_, 0));
}

llvm::Value* CodeGen::gen_ident(const Expr::Ident& ident) {
    // Check local variables
    auto it = named_values_.find(ident.name);
    if (it != named_values_.end()) {
        return builder_->CreateLoad(it->second->getAllocatedType(),
                                    it->second, ident.name);
    }

    // Check for function
    llvm::Function* fn = module_->getFunction(ident.name);
    if (fn) {
        return fn;
    }

    error("Unknown variable: " + ident.name);
    return nullptr;
}

llvm::Value* CodeGen::gen_binary(const Expr::Binary& bin) {
    llvm::Value* left = gen_expr(bin.left);
    llvm::Value* right = gen_expr(bin.right);
    if (!left || !right) return nullptr;

    bool is_float = left->getType()->isFloatingPointTy();
    bool is_signed = true; // assume signed integers

    switch (bin.op) {
        case TokenKind::Plus:
            return is_float
                ? builder_->CreateFAdd(left, right, "faddtmp")
                : builder_->CreateAdd(left, right, "addtmp");
        case TokenKind::Minus:
            return is_float
                ? builder_->CreateFSub(left, right, "fsubtmp")
                : builder_->CreateSub(left, right, "subtmp");
        case TokenKind::Star:
            return is_float
                ? builder_->CreateFMul(left, right, "fmultmp")
                : builder_->CreateMul(left, right, "multmp");
        case TokenKind::Slash:
            return is_float
                ? builder_->CreateFDiv(left, right, "fdivtmp")
                : builder_->CreateSDiv(left, right, "divtmp");
        case TokenKind::Percent:
            return builder_->CreateSRem(left, right, "modtmp");

        case TokenKind::Eq:
            return is_float
                ? builder_->CreateFCmpOEQ(left, right, "eqtmp")
                : builder_->CreateICmpEQ(left, right, "eqtmp");
        case TokenKind::NotEq:
            return is_float
                ? builder_->CreateFCmpONE(left, right, "netmp")
                : builder_->CreateICmpNE(left, right, "netmp");
        case TokenKind::Lt:
            return is_float
                ? builder_->CreateFCmpOLT(left, right, "lttmp")
                : builder_->CreateICmpSLT(left, right, "lttmp");
        case TokenKind::LtEq:
            return is_float
                ? builder_->CreateFCmpOLE(left, right, "letmp")
                : builder_->CreateICmpSLE(left, right, "letmp");
        case TokenKind::Gt:
            return is_float
                ? builder_->CreateFCmpOGT(left, right, "gttmp")
                : builder_->CreateICmpSGT(left, right, "gttmp");
        case TokenKind::GtEq:
            return is_float
                ? builder_->CreateFCmpOGE(left, right, "getmp")
                : builder_->CreateICmpSGE(left, right, "getmp");

        case TokenKind::AndAnd:
        case TokenKind::And:
            return builder_->CreateAnd(left, right, "andtmp");
        case TokenKind::OrOr:
            return builder_->CreateOr(left, right, "ortmp");

        case TokenKind::In: {
            // `in` operator - check if left is in right (array)
            // right is an array struct {i64 len, ptr}
            llvm::Value* arr_len = builder_->CreateExtractValue(right, 0, "arrlen");
            llvm::Value* arr_data = builder_->CreateExtractValue(right, 1, "arrdata");

            llvm::Function* func = builder_->GetInsertBlock()->getParent();
            llvm::BasicBlock* loop_bb = llvm::BasicBlock::Create(*context_, "in.loop", func);
            llvm::BasicBlock* body_bb = llvm::BasicBlock::Create(*context_, "in.body", func);
            llvm::BasicBlock* found_bb = llvm::BasicBlock::Create(*context_, "in.found", func);
            llvm::BasicBlock* inc_bb = llvm::BasicBlock::Create(*context_, "in.inc", func);
            llvm::BasicBlock* done_bb = llvm::BasicBlock::Create(*context_, "in.done", func);

            // Allocate result and index
            llvm::AllocaInst* result = builder_->CreateAlloca(
                llvm::Type::getInt1Ty(*context_), nullptr, "in.result");
            llvm::AllocaInst* idx = builder_->CreateAlloca(
                llvm::Type::getInt64Ty(*context_), nullptr, "in.idx");
            builder_->CreateStore(llvm::ConstantInt::getFalse(*context_), result);
            builder_->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 0), idx);

            builder_->CreateBr(loop_bb);

            // Loop header - check condition
            builder_->SetInsertPoint(loop_bb);
            llvm::Value* i = builder_->CreateLoad(llvm::Type::getInt64Ty(*context_), idx, "i");
            llvm::Value* cond = builder_->CreateICmpSLT(i, arr_len, "loopcond");
            builder_->CreateCondBr(cond, body_bb, done_bb);

            // Loop body - check if element matches
            builder_->SetInsertPoint(body_bb);
            llvm::Value* elem_ptr = builder_->CreateGEP(left->getType(), arr_data, i, "elemptr");
            llvm::Value* elem = builder_->CreateLoad(left->getType(), elem_ptr, "elem");

            llvm::Value* eq;
            if (left->getType()->isFloatingPointTy()) {
                eq = builder_->CreateFCmpOEQ(left, elem, "eq");
            } else {
                eq = builder_->CreateICmpEQ(left, elem, "eq");
            }
            builder_->CreateCondBr(eq, found_bb, inc_bb);

            // Increment index and continue loop
            builder_->SetInsertPoint(inc_bb);
            llvm::Value* next_i = builder_->CreateAdd(i,
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 1), "nexti");
            builder_->CreateStore(next_i, idx);
            builder_->CreateBr(loop_bb);

            // Found - set result true and exit
            builder_->SetInsertPoint(found_bb);
            builder_->CreateStore(llvm::ConstantInt::getTrue(*context_), result);
            builder_->CreateBr(done_bb);

            // Done - return result
            builder_->SetInsertPoint(done_bb);
            return builder_->CreateLoad(llvm::Type::getInt1Ty(*context_), result, "in.val");
        }

        default:
            error("Unknown binary operator");
            return nullptr;
    }
}

llvm::Value* CodeGen::gen_unary(const Expr::Unary& un) {
    llvm::Value* operand = gen_expr(un.operand);
    if (!operand) return nullptr;

    switch (un.op) {
        case TokenKind::Minus:
            if (operand->getType()->isFloatingPointTy()) {
                return builder_->CreateFNeg(operand, "fnegtmp");
            }
            return builder_->CreateNeg(operand, "negtmp");
        case TokenKind::Bang:
        case TokenKind::Not:
            return builder_->CreateNot(operand, "nottmp");
        default:
            error("Unknown unary operator");
            return nullptr;
    }
}

llvm::Value* CodeGen::gen_call(const Expr::Call& call) {
    // Check for built-in print
    if (auto* ident = std::get_if<Expr::Ident>(&call.callee->kind)) {
        if (ident->name == "print") {
            llvm::Function* printf_fn = module_->getFunction("printf");
            if (!printf_fn) return nullptr;

            if (call.args.empty()) {
                // print newline
                llvm::Value* fmt = builder_->CreateGlobalString("\n");
                return builder_->CreateCall(printf_fn, {fmt});
            }

            llvm::Value* arg = gen_expr(call.args[0]);
            if (!arg) return nullptr;

            // Choose format string based on type
            std::string fmt_str;
            std::vector<llvm::Value*> printf_args;

            if (arg->getType()->isIntegerTy(64)) {
                fmt_str = "%ld\n";
                printf_args.push_back(builder_->CreateGlobalString(fmt_str));
                printf_args.push_back(arg);
            } else if (arg->getType()->isIntegerTy(1)) {
                // bool - print true/false
                fmt_str = "%s\n";
                llvm::Value* true_str = builder_->CreateGlobalString("true");
                llvm::Value* false_str = builder_->CreateGlobalString("false");
                llvm::Value* str = builder_->CreateSelect(arg, true_str, false_str);
                printf_args.push_back(builder_->CreateGlobalString(fmt_str));
                printf_args.push_back(str);
            } else if (arg->getType()->isDoubleTy()) {
                fmt_str = "%f\n";
                printf_args.push_back(builder_->CreateGlobalString(fmt_str));
                printf_args.push_back(arg);
            } else if (arg->getType()->isPointerTy()) {
                fmt_str = "%s\n";
                printf_args.push_back(builder_->CreateGlobalString(fmt_str));
                printf_args.push_back(arg);
            } else {
                fmt_str = "%d\n";
                printf_args.push_back(builder_->CreateGlobalString(fmt_str));
                printf_args.push_back(arg);
            }

            return builder_->CreateCall(printf_fn, printf_args);
        }

        // Assertion built-ins
        if (ident->name == "assert") {
            return gen_assert(call);
        }
        if (ident->name == "assert_eq") {
            return gen_assert_eq(call);
        }
        if (ident->name == "assert_ne") {
            return gen_assert_ne(call);
        }
        if (ident->name == "assert_true") {
            return gen_assert_true(call);
        }
        if (ident->name == "assert_false") {
            return gen_assert_false(call);
        }
    }

    // Check for method call (field.name())
    if (auto* field_expr = std::get_if<Expr::Field>(&call.callee->kind)) {
        // For method calls, we need the pointer to the object (not the loaded value)
        llvm::Value* obj_ptr = nullptr;
        std::string type_name;

        // If object is an identifier, get its alloca directly
        if (auto* ident = std::get_if<Expr::Ident>(&field_expr->object->kind)) {
            auto it = named_values_.find(ident->name);
            if (it != named_values_.end()) {
                obj_ptr = it->second;
                if (auto* alloca_type = llvm::dyn_cast<llvm::StructType>(it->second->getAllocatedType())) {
                    type_name = alloca_type->getName().str();
                }

                // Built-in array method: len()
                auto vt = var_types_.find(ident->name);
                if (vt != var_types_.end() &&
                    vt->second->kind == ResolvedType::Kind::Array &&
                    field_expr->name == "len" && call.args.empty()) {
                    llvm::Type* arr_ty = it->second->getAllocatedType();
                    llvm::Value* arr_val = builder_->CreateLoad(arr_ty, obj_ptr, ident->name + "_arr");
                    return builder_->CreateExtractValue(arr_val, 0, "len");
                }
            }
        }

        if (obj_ptr && !type_name.empty()) {
            std::string method_name = type_name + "_" + field_expr->name;
            llvm::Function* fn = module_->getFunction(method_name);
            if (fn) {
                std::vector<llvm::Value*> args;
                args.push_back(obj_ptr); // self pointer
                for (const auto& arg : call.args) {
                    args.push_back(gen_expr(arg));
                }
                return builder_->CreateCall(fn, args);
            }
        }

        // Fallback: evaluate the object (for field access, not method call)
        llvm::Value* obj = gen_expr(field_expr->object);
        if (!obj) return nullptr;

        // Try to find method with mangled name
        std::string method_name = field_expr->name;
        if (auto* struct_ty = llvm::dyn_cast<llvm::StructType>(obj->getType())) {
            type_name = struct_ty->getName().str();
            method_name = type_name + "_" + field_expr->name;
        }

        llvm::Function* fn = module_->getFunction(method_name);
        if (fn) {
            std::vector<llvm::Value*> args;
            args.push_back(obj); // self
            for (const auto& arg : call.args) {
                args.push_back(gen_expr(arg));
            }
            return builder_->CreateCall(fn, args);
        }
    }

    // Regular function call
    llvm::Value* callee = gen_expr(call.callee);
    if (!callee) return nullptr;

    std::vector<llvm::Value*> args;
    for (const auto& arg : call.args) {
        llvm::Value* val = gen_expr(arg);
        if (!val) return nullptr;
        args.push_back(val);
    }

    if (auto* fn = llvm::dyn_cast<llvm::Function>(callee)) {
        return builder_->CreateCall(fn, args);
    }

    // Function pointer call - build function type from args
    // With opaque pointers, we need to construct the function type
    std::vector<llvm::Type*> arg_types;
    for (auto* arg : args) {
        arg_types.push_back(arg->getType());
    }
    // Assume void return for now (proper handling would use type info)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*context_), arg_types, false);
    return builder_->CreateCall(fn_type, callee, args);
}

llvm::Value* CodeGen::gen_index(const Expr::Index& idx) {
    llvm::Value* obj = gen_expr(idx.object);
    llvm::Value* index = gen_expr(idx.index);
    if (!obj || !index) return nullptr;

    // Array indexing: obj is {i64 len, ptr}
    // Get data pointer (second element)
    llvm::Value* data_ptr = builder_->CreateExtractValue(obj, 1, "data");

    // Determine element type from the indexed object
    llvm::Type* elem_type = llvm::Type::getInt64Ty(*context_);  // default

    // If object is an identifier, look up its type from our var_types_ map
    if (auto* ident = std::get_if<Expr::Ident>(&idx.object->kind)) {
        auto it = var_types_.find(ident->name);
        if (it != var_types_.end() && it->second->kind == ResolvedType::Kind::Array) {
            if (it->second->element_type) {
                elem_type = get_llvm_type(it->second->element_type);
            }
        }
    }

    // GEP to index
    llvm::Value* elem_ptr = builder_->CreateGEP(elem_type, data_ptr, index, "elemptr");
    return builder_->CreateLoad(elem_type, elem_ptr, "elem");
}

llvm::Value* CodeGen::gen_field(const Expr::Field& field) {
    // Check if this is a field access on a known variable (like self)
    if (auto* ident = std::get_if<Expr::Ident>(&field.object->kind)) {
        auto type_it = var_types_.find(ident->name);
        if (type_it != var_types_.end() && type_it->second->kind == ResolvedType::Kind::Struct) {
            auto it = named_values_.find(ident->name);
            if (it != named_values_.end()) {
                llvm::Value* ptr = it->second;
                // If the alloca holds a pointer (e.g., method self), load it
                if (auto* ai = llvm::dyn_cast<llvm::AllocaInst>(ptr)) {
                    llvm::Type* alloc_ty = ai->getAllocatedType();
                    if (alloc_ty->isPointerTy()) {
                        ptr = builder_->CreateLoad(alloc_ty, ptr, ident->name + "_ptr");
                    }
                }
                // Look up struct definition to find field index
                std::string struct_name = type_it->second->name;
                llvm::StructType* struct_ty = get_or_create_struct_type(struct_name);
                unsigned field_idx = 0;

                if (auto def = symbols_->lookup_struct(struct_name)) {
                    for (size_t i = 0; i < def->fields.size(); ++i) {
                        if (def->fields[i].name == field.name) {
                            field_idx = i;
                            break;
                        }
                    }
                }

                // GEP to get field pointer
                llvm::Value* field_ptr = builder_->CreateStructGEP(
                    struct_ty, ptr, field_idx, field.name);

                // Load and return the field value
                llvm::Type* field_type = struct_ty->getElementType(field_idx);
                return builder_->CreateLoad(field_type, field_ptr, field.name);
            }
        }
    }

    llvm::Value* obj = gen_expr(field.object);
    if (!obj) return nullptr;

    // Struct value - use extractvalue
    if (auto* struct_ty = llvm::dyn_cast<llvm::StructType>(obj->getType())) {
        // Get struct name and look up field index
        std::string struct_name = struct_ty->getName().str();
        unsigned field_idx = 0;

        if (auto def = symbols_->lookup_struct(struct_name)) {
            for (size_t i = 0; i < def->fields.size(); ++i) {
                if (def->fields[i].name == field.name) {
                    field_idx = i;
                    break;
                }
            }
        }

        return builder_->CreateExtractValue(obj, field_idx, field.name);
    }

    // For pointers, we'd need type info to do GEP
    // For now, just return the object
    return obj;
}

llvm::Value* CodeGen::gen_struct_lit(const Expr::StructLit& lit) {
    // Variant construction: Type.Case { ... }
    auto dot = lit.type_name.find('.');
    if (dot != std::string::npos) {
        std::string variant_name = lit.type_name.substr(0, dot);
        std::string case_name = lit.type_name.substr(dot + 1);

        auto def = symbols_->lookup_variant(variant_name);
        if (!def) {
            error("Unknown variant type: " + variant_name);
            return nullptr;
        }

        // Determine tag index and collect field info
        int tag_index = -1;
        std::vector<FieldInfo> case_fields;
        for (size_t i = 0; i < def->cases.size(); ++i) {
            if (def->cases[i].name == case_name) {
                tag_index = static_cast<int>(i);
                case_fields = def->cases[i].fields;
                break;
            }
        }
        if (tag_index < 0) {
            error("Unknown variant case: " + case_name);
            return nullptr;
        }

        llvm::StructType* variant_ty = get_or_create_struct_type(variant_name);
        if (!variant_ty) {
            error("Failed to create variant type for " + variant_name);
            return nullptr;
        }

        // Allocate variant value
        llvm::AllocaInst* alloca = builder_->CreateAlloca(variant_ty, nullptr, "variant");

        // Store tag
        llvm::Value* tag_ptr = builder_->CreateStructGEP(variant_ty, alloca, 0, "tagptr");
        builder_->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), tag_index), tag_ptr);

        // Payload pointer (if present)
        if (variant_ty->getNumElements() > 1 && !case_fields.empty()) {
            llvm::Value* payload_ptr = builder_->CreateStructGEP(variant_ty, alloca, 1, "payload");
            llvm::Type* payload_ty = variant_ty->getElementType(1);
            llvm::Value* base_i8 = builder_->CreateBitCast(
                payload_ptr, llvm::PointerType::get(llvm::Type::getInt8Ty(*context_), 0));

            uint64_t offset = 0;
            for (size_t i = 0; i < case_fields.size() && i < lit.fields.size(); ++i) {
                llvm::Value* val = gen_expr(lit.fields[i].second);
                llvm::Type* field_ty = get_llvm_type(case_fields[i].type);
                uint64_t field_size = module_->getDataLayout().getTypeAllocSize(field_ty);

                llvm::Value* dest = builder_->CreateConstGEP1_64(llvm::Type::getInt8Ty(*context_), base_i8, offset);
                llvm::Value* dest_typed = builder_->CreateBitCast(dest, field_ty->getPointerTo());
                builder_->CreateStore(val, dest_typed);
                offset += field_size;
            }
        }

        return builder_->CreateLoad(variant_ty, alloca, "variantval");
    }

    llvm::StructType* struct_type = get_or_create_struct_type(lit.type_name);
    if (!struct_type) {
        error("Unknown struct type: " + lit.type_name);
        return nullptr;
    }

    // Allocate struct on stack
    llvm::AllocaInst* alloca = builder_->CreateAlloca(struct_type, nullptr, "struct");

    // Initialize fields
    size_t idx = 0;
    for (const auto& [name, value] : lit.fields) {
        llvm::Value* val = gen_expr(value);
        if (val) {
            llvm::Value* field_ptr = builder_->CreateStructGEP(
                struct_type, alloca, idx, name);
            builder_->CreateStore(val, field_ptr);
        }
        idx++;
    }

    return builder_->CreateLoad(struct_type, alloca, "structval");
}

llvm::Value* CodeGen::gen_array_lit(const Expr::ArrayLit& lit) {
    // Array struct type: {i64 len, ptr}
    auto* arr_type = llvm::StructType::get(*context_, {
        llvm::Type::getInt64Ty(*context_),
        llvm::PointerType::get(*context_, 0)
    });

    if (lit.elements.empty()) {
        // Empty array - return {0, null}
        return llvm::ConstantStruct::get(arr_type, {
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 0),
            llvm::ConstantPointerNull::get(llvm::PointerType::get(*context_, 0))
        });
    }

    // Generate all elements
    std::vector<llvm::Value*> values;
    for (const auto& elem : lit.elements) {
        values.push_back(gen_expr(elem));
    }

    if (values.empty() || !values[0]) return nullptr;

    llvm::Type* elem_type = values[0]->getType();
    size_t count = values.size();

    // Allocate array data using element size
    llvm::Function* malloc_fn = module_->getFunction("malloc");
    uint64_t elem_size_bytes = module_->getDataLayout().getTypeAllocSize(elem_type);
    llvm::Value* elem_size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), elem_size_bytes);
    llvm::Value* count_val = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), count);
    llvm::Value* total_size = builder_->CreateMul(count_val, elem_size, "arrsize");
    llvm::Value* data_ptr = builder_->CreateCall(malloc_fn, {total_size}, "dataptr");

    // Store elements
    for (size_t i = 0; i < values.size(); i++) {
        llvm::Value* elem_ptr = builder_->CreateGEP(elem_type, data_ptr,
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), i), "elemptr");
        builder_->CreateStore(values[i], elem_ptr);
    }

    // Build array struct {len, data}
    llvm::Value* arr = llvm::UndefValue::get(arr_type);
    arr = builder_->CreateInsertValue(arr,
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), count), 0);
    arr = builder_->CreateInsertValue(arr, data_ptr, 1);

    return arr;
}

llvm::Value* CodeGen::gen_match(const Expr::Match& match) {
    llvm::Value* subject = gen_expr(match.subject);
    if (!subject) return nullptr;

    llvm::Function* func = current_function_;
    llvm::BasicBlock* merge_bb = llvm::BasicBlock::Create(*context_, "matchend");

    // For now, simple implementation using if-else chain
    llvm::Value* result = nullptr;
    llvm::BasicBlock* current_bb = builder_->GetInsertBlock();

    for (size_t i = 0; i < match.arms.size(); i++) {
        const auto& [pattern, arm_expr] = match.arms[i];
        bool is_last = (i == match.arms.size() - 1);

        // Check pattern
        llvm::Value* matches = nullptr;
        if (auto* lit = std::get_if<Pattern::Literal>(&pattern->kind)) {
            // Compare with literal
            llvm::Value* lit_val = nullptr;
            if (lit->value.kind == TokenKind::Int) {
                lit_val = llvm::ConstantInt::get(
                    llvm::Type::getInt64Ty(*context_),
                    std::get<int64_t>(lit->value.value));
            } else if (lit->value.kind == TokenKind::True) {
                lit_val = llvm::ConstantInt::getTrue(*context_);
            } else if (lit->value.kind == TokenKind::False) {
                lit_val = llvm::ConstantInt::getFalse(*context_);
            }
            if (lit_val) {
                matches = builder_->CreateICmpEQ(subject, lit_val, "match");
            }
        } else if (std::holds_alternative<Pattern::Wildcard>(pattern->kind)) {
            matches = llvm::ConstantInt::getTrue(*context_);
        } else if (auto* ident = std::get_if<Pattern::Ident>(&pattern->kind)) {
            // Bind to variable
            llvm::AllocaInst* alloca = create_entry_alloca(func, ident->name, subject->getType());
            builder_->CreateStore(subject, alloca);
            named_values_[ident->name] = alloca;
            matches = llvm::ConstantInt::getTrue(*context_);
        }

        if (!matches) {
            matches = llvm::ConstantInt::getTrue(*context_);
        }

        llvm::BasicBlock* arm_bb = llvm::BasicBlock::Create(*context_, "matcharm", func);
        llvm::BasicBlock* next_bb = is_last ? merge_bb :
            llvm::BasicBlock::Create(*context_, "matchnext");

        builder_->CreateCondBr(matches, arm_bb, next_bb);

        builder_->SetInsertPoint(arm_bb);
        llvm::Value* arm_val = gen_expr(arm_expr);
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_bb);
        }

        if (!is_last) {
            func->insert(func->end(), next_bb);
            builder_->SetInsertPoint(next_bb);
        }

        // Record result for phi
        if (arm_val && !result) {
            result = arm_val;
        }
    }

    func->insert(func->end(), merge_bb);
    builder_->SetInsertPoint(merge_bb);

    return result;
}

llvm::Value* CodeGen::gen_lambda(const Expr::Lambda& lambda) {
    // Create anonymous function
    static int lambda_counter = 0;
    std::string name = "__lambda_" + std::to_string(lambda_counter++);

    std::vector<llvm::Type*> param_types;
    for (const auto& [pname, ptype] : lambda.params) {
        param_types.push_back(get_llvm_type(ptype));
    }

    llvm::Type* ret_type = lambda.return_type
        ? get_llvm_type(lambda.return_type)
        : llvm::Type::getVoidTy(*context_);

    llvm::FunctionType* fn_type = llvm::FunctionType::get(ret_type, param_types, false);
    llvm::Function* fn = llvm::Function::Create(
        fn_type, llvm::Function::InternalLinkage, name, module_.get());

    // Save current state
    llvm::BasicBlock* saved_block = builder_->GetInsertBlock();
    llvm::Function* saved_function = current_function_;
    auto saved_values = named_values_;

    // Generate lambda body
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context_, "entry", fn);
    builder_->SetInsertPoint(entry);
    current_function_ = fn;
    named_values_.clear();

    size_t idx = 0;
    for (auto& arg : fn->args()) {
        const auto& [pname, ptype] = lambda.params[idx];
        arg.setName(pname);
        llvm::AllocaInst* alloca = create_entry_alloca(fn, pname, arg.getType());
        builder_->CreateStore(&arg, alloca);
        named_values_[pname] = alloca;
        idx++;
    }

    gen_block(lambda.body);

    if (!builder_->GetInsertBlock()->getTerminator()) {
        if (lambda.return_type) {
            builder_->CreateRet(llvm::Constant::getNullValue(ret_type));
        } else {
            builder_->CreateRetVoid();
        }
    }

    // Restore state
    builder_->SetInsertPoint(saved_block);
    current_function_ = saved_function;
    named_values_ = saved_values;

    return fn;
}

llvm::Value* CodeGen::gen_or_expr(const Expr::Or& or_expr) {
    llvm::Value* opt = gen_expr(or_expr.left);
    if (!opt) return nullptr;

    // Optional layout: { i1 has_value, T value }
    if (!opt->getType()->isStructTy() ||
        opt->getType()->getStructNumElements() < 2) {
        return opt;  // best effort fallback
    }

    llvm::StructType* opt_ty = llvm::cast<llvm::StructType>(opt->getType());
    llvm::Type* value_ty = opt_ty->getElementType(1);

    llvm::Value* has_val = builder_->CreateExtractValue(opt, 0, "has_val");
    llvm::Value* val = builder_->CreateExtractValue(opt, 1, "opt_value");

    llvm::Function* func = current_function_;
    llvm::BasicBlock* then_bb = llvm::BasicBlock::Create(*context_, "or.has", func);
    llvm::BasicBlock* else_bb = llvm::BasicBlock::Create(*context_, "or.none");
    llvm::BasicBlock* merge_bb = llvm::BasicBlock::Create(*context_, "or.merge");

    builder_->CreateCondBr(has_val, then_bb, else_bb);

    // has-value branch
    builder_->SetInsertPoint(then_bb);
    builder_->CreateBr(merge_bb);

    // none branch
    func->insert(func->end(), else_bb);
    builder_->SetInsertPoint(else_bb);

    llvm::Value* fallback_val = nullptr;
    if (or_expr.is_return && !or_expr.right) {
        // Early return if none
        if (func->getReturnType()->isVoidTy()) {
            builder_->CreateRetVoid();
        } else {
            builder_->CreateRet(llvm::UndefValue::get(func->getReturnType()));
        }
    } else {
        fallback_val = gen_expr(or_expr.right);
        if (!fallback_val) return nullptr;
        builder_->CreateBr(merge_bb);
    }

    // Merge
    func->insert(func->end(), merge_bb);
    builder_->SetInsertPoint(merge_bb);

    if (or_expr.is_return && !or_expr.right) {
        return val;
    }

    llvm::PHINode* phi = builder_->CreatePHI(value_ty, 2, "or.result");
    phi->addIncoming(val, then_bb);
    if (fallback_val) {
        phi->addIncoming(fallback_val, else_bb);
    }

    return phi;
}

llvm::Value* CodeGen::gen_is_expr(const Expr::Is& is_expr) {
    llvm::Value* val = gen_expr(is_expr.value);
    if (!val) return nullptr;

    // Determine variant type name
    std::string variant_name;
    if (auto* ident = std::get_if<Expr::Ident>(&is_expr.value->kind)) {
        auto it = var_types_.find(ident->name);
        if (it != var_types_.end() &&
            it->second->kind == ResolvedType::Kind::Variant) {
            variant_name = it->second->name;
        }
    }
    if (variant_name.empty()) {
        auto inferred = infer_expr_type(is_expr.value);
        if (inferred && inferred->kind == ResolvedType::Kind::Variant) {
            variant_name = inferred->name;
        }
    }

    if (variant_name.empty() || !val->getType()->isStructTy()) {
        return llvm::ConstantInt::getFalse(*context_);
    }

    auto def = symbols_->lookup_variant(variant_name);
    if (!def) {
        return llvm::ConstantInt::getFalse(*context_);
    }

    int tag_index = -1;
    for (size_t i = 0; i < def->cases.size(); ++i) {
        if (def->cases[i].name == is_expr.variant_name) {
            tag_index = static_cast<int>(i);
            break;
        }
    }
    if (tag_index < 0) {
        return llvm::ConstantInt::getFalse(*context_);
    }

    llvm::Value* tag = builder_->CreateExtractValue(val, 0, "tag");
    return builder_->CreateICmpEQ(tag,
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), tag_index), "istmp");
}

llvm::Value* CodeGen::gen_cast(const Expr::Cast& cast) {
    llvm::Value* val = gen_expr(cast.value);
    if (!val) return nullptr;

    llvm::Type* target = get_llvm_type(cast.target_type);

    // Handle various casts
    if (val->getType() == target) {
        return val;
    }

    if (val->getType()->isIntegerTy() && target->isIntegerTy()) {
        unsigned src_bits = val->getType()->getIntegerBitWidth();
        unsigned dst_bits = target->getIntegerBitWidth();
        if (src_bits < dst_bits) {
            return builder_->CreateSExt(val, target, "sext");
        } else {
            return builder_->CreateTrunc(val, target, "trunc");
        }
    }

    if (val->getType()->isIntegerTy() && target->isFloatingPointTy()) {
        return builder_->CreateSIToFP(val, target, "sitofp");
    }

    if (val->getType()->isFloatingPointTy() && target->isIntegerTy()) {
        return builder_->CreateFPToSI(val, target, "fptosi");
    }

    if (val->getType()->isPointerTy() && target->isPointerTy()) {
        return builder_->CreateBitCast(val, target, "bitcast");
    }

    return val;
}

llvm::Value* CodeGen::gen_block_expr(const Expr::Block& block) {
    for (const auto& stmt : block.stmts) {
        gen_stmt(stmt);
    }

    if (block.result) {
        return gen_expr(block.result);
    }

    return nullptr;
}

// Helpers

llvm::AllocaInst* CodeGen::create_entry_alloca(llvm::Function* fn,
                                               const std::string& name,
                                               llvm::Type* type) {
    llvm::IRBuilder<> tmp_builder(&fn->getEntryBlock(), fn->getEntryBlock().begin());
    return tmp_builder.CreateAlloca(type, nullptr, name);
}

void CodeGen::error(const std::string& msg) {
    errors_.push_back(msg);
}

void CodeGen::error_at(const SourceLoc& loc, const std::string& msg) {
    std::stringstream ss;
    ss << loc.file << ":" << loc.line << ":" << loc.column << ": " << msg;
    errors_.push_back(ss.str());
}

// Assertion implementations

llvm::Value* CodeGen::gen_assert(const Expr::Call& call) {
    // assert(condition, message)
    llvm::Value* cond = gen_expr(call.args[0]);
    llvm::Value* msg = gen_expr(call.args[1]);
    if (!cond || !msg) return nullptr;

    llvm::Function* func = current_function_;
    llvm::BasicBlock* fail_bb = llvm::BasicBlock::Create(*context_, "assert_fail", func);
    llvm::BasicBlock* pass_bb = llvm::BasicBlock::Create(*context_, "assert_pass");

    builder_->CreateCondBr(cond, pass_bb, fail_bb);

    // Fail block - print error and exit(1)
    builder_->SetInsertPoint(fail_bb);

    llvm::Function* fprintf_fn = module_->getFunction("fprintf");
    llvm::GlobalVariable* stderr_var = module_->getGlobalVariable("stderr");
    llvm::Value* stderr_val = builder_->CreateLoad(
        llvm::PointerType::get(*context_, 0), stderr_var, "stderr_load");

    // Format: "Assertion failed at file:line: message\n"
    std::string loc_str = call.callee->loc.file + ":" + std::to_string(call.callee->loc.line);
    llvm::Value* fmt = builder_->CreateGlobalString("Assertion failed at " + loc_str + ": %s\n");
    builder_->CreateCall(fprintf_fn, {stderr_val, fmt, msg});

    llvm::Function* exit_fn = module_->getFunction("exit");
    builder_->CreateCall(exit_fn, {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1)
    });
    builder_->CreateUnreachable();

    // Pass block - continue execution
    func->insert(func->end(), pass_bb);
    builder_->SetInsertPoint(pass_bb);

    return nullptr;
}

llvm::Value* CodeGen::gen_assert_eq(const Expr::Call& call) {
    // assert_eq(a, b)
    llvm::Value* left = gen_expr(call.args[0]);
    llvm::Value* right = gen_expr(call.args[1]);
    if (!left || !right) return nullptr;

    // Generate comparison based on type
    llvm::Value* cmp;
    if (left->getType()->isIntegerTy()) {
        cmp = builder_->CreateICmpEQ(left, right, "assert_eq_cmp");
    } else if (left->getType()->isFloatingPointTy()) {
        cmp = builder_->CreateFCmpOEQ(left, right, "assert_eq_cmp");
    } else if (left->getType()->isPointerTy()) {
        // String comparison - use strcmp
        llvm::Function* strcmp_fn = module_->getFunction("strcmp");
        llvm::Value* result = builder_->CreateCall(strcmp_fn, {left, right}, "strcmp_result");
        cmp = builder_->CreateICmpEQ(result,
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0),
            "assert_eq_cmp");
    } else {
        cmp = builder_->CreateICmpEQ(left, right, "assert_eq_cmp");
    }

    llvm::Function* func = current_function_;
    llvm::BasicBlock* fail_bb = llvm::BasicBlock::Create(*context_, "assert_fail", func);
    llvm::BasicBlock* pass_bb = llvm::BasicBlock::Create(*context_, "assert_pass");

    builder_->CreateCondBr(cmp, pass_bb, fail_bb);

    // Fail block
    builder_->SetInsertPoint(fail_bb);

    llvm::Function* fprintf_fn = module_->getFunction("fprintf");
    llvm::GlobalVariable* stderr_var = module_->getGlobalVariable("stderr");
    llvm::Value* stderr_val = builder_->CreateLoad(
        llvm::PointerType::get(*context_, 0), stderr_var, "stderr_load");

    std::string loc_str = call.callee->loc.file + ":" + std::to_string(call.callee->loc.line);
    llvm::Value* fmt = builder_->CreateGlobalString(
        "Assertion failed at " + loc_str + ": values not equal\n");
    builder_->CreateCall(fprintf_fn, {stderr_val, fmt});

    llvm::Function* exit_fn = module_->getFunction("exit");
    builder_->CreateCall(exit_fn, {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1)
    });
    builder_->CreateUnreachable();

    // Pass block
    func->insert(func->end(), pass_bb);
    builder_->SetInsertPoint(pass_bb);

    return nullptr;
}

llvm::Value* CodeGen::gen_assert_ne(const Expr::Call& call) {
    // assert_ne(a, b)
    llvm::Value* left = gen_expr(call.args[0]);
    llvm::Value* right = gen_expr(call.args[1]);
    if (!left || !right) return nullptr;

    llvm::Value* cmp;
    if (left->getType()->isIntegerTy()) {
        cmp = builder_->CreateICmpNE(left, right, "assert_ne_cmp");
    } else if (left->getType()->isFloatingPointTy()) {
        cmp = builder_->CreateFCmpONE(left, right, "assert_ne_cmp");
    } else if (left->getType()->isPointerTy()) {
        cmp = builder_->CreateICmpNE(
            builder_->CreatePtrToInt(left, llvm::Type::getInt64Ty(*context_)),
            builder_->CreatePtrToInt(right, llvm::Type::getInt64Ty(*context_)),
            "assert_ne_cmp");
    } else {
        cmp = builder_->CreateICmpNE(left, right, "assert_ne_cmp");
    }

    llvm::Function* func = current_function_;
    llvm::BasicBlock* fail_bb = llvm::BasicBlock::Create(*context_, "assert_fail", func);
    llvm::BasicBlock* pass_bb = llvm::BasicBlock::Create(*context_, "assert_pass");

    builder_->CreateCondBr(cmp, pass_bb, fail_bb);

    // Fail block
    builder_->SetInsertPoint(fail_bb);

    llvm::Function* fprintf_fn = module_->getFunction("fprintf");
    llvm::GlobalVariable* stderr_var = module_->getGlobalVariable("stderr");
    llvm::Value* stderr_val = builder_->CreateLoad(
        llvm::PointerType::get(*context_, 0), stderr_var, "stderr_load");

    std::string loc_str = call.callee->loc.file + ":" + std::to_string(call.callee->loc.line);
    llvm::Value* fmt = builder_->CreateGlobalString(
        "Assertion failed at " + loc_str + ": values should not be equal\n");
    builder_->CreateCall(fprintf_fn, {stderr_val, fmt});

    llvm::Function* exit_fn = module_->getFunction("exit");
    builder_->CreateCall(exit_fn, {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1)
    });
    builder_->CreateUnreachable();

    // Pass block
    func->insert(func->end(), pass_bb);
    builder_->SetInsertPoint(pass_bb);

    return nullptr;
}

llvm::Value* CodeGen::gen_assert_true(const Expr::Call& call) {
    // assert_true(value)
    llvm::Value* val = gen_expr(call.args[0]);
    if (!val) return nullptr;

    llvm::Function* func = current_function_;
    llvm::BasicBlock* fail_bb = llvm::BasicBlock::Create(*context_, "assert_fail", func);
    llvm::BasicBlock* pass_bb = llvm::BasicBlock::Create(*context_, "assert_pass");

    builder_->CreateCondBr(val, pass_bb, fail_bb);

    // Fail block
    builder_->SetInsertPoint(fail_bb);

    llvm::Function* fprintf_fn = module_->getFunction("fprintf");
    llvm::GlobalVariable* stderr_var = module_->getGlobalVariable("stderr");
    llvm::Value* stderr_val = builder_->CreateLoad(
        llvm::PointerType::get(*context_, 0), stderr_var, "stderr_load");

    std::string loc_str = call.callee->loc.file + ":" + std::to_string(call.callee->loc.line);
    llvm::Value* fmt = builder_->CreateGlobalString(
        "Assertion failed at " + loc_str + ": expected true, got false\n");
    builder_->CreateCall(fprintf_fn, {stderr_val, fmt});

    llvm::Function* exit_fn = module_->getFunction("exit");
    builder_->CreateCall(exit_fn, {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1)
    });
    builder_->CreateUnreachable();

    // Pass block
    func->insert(func->end(), pass_bb);
    builder_->SetInsertPoint(pass_bb);

    return nullptr;
}

llvm::Value* CodeGen::gen_assert_false(const Expr::Call& call) {
    // assert_false(value)
    llvm::Value* val = gen_expr(call.args[0]);
    if (!val) return nullptr;

    llvm::Function* func = current_function_;
    llvm::BasicBlock* fail_bb = llvm::BasicBlock::Create(*context_, "assert_fail", func);
    llvm::BasicBlock* pass_bb = llvm::BasicBlock::Create(*context_, "assert_pass");

    // Note: inverted - fail if true, pass if false
    builder_->CreateCondBr(val, fail_bb, pass_bb);

    // Fail block
    builder_->SetInsertPoint(fail_bb);

    llvm::Function* fprintf_fn = module_->getFunction("fprintf");
    llvm::GlobalVariable* stderr_var = module_->getGlobalVariable("stderr");
    llvm::Value* stderr_val = builder_->CreateLoad(
        llvm::PointerType::get(*context_, 0), stderr_var, "stderr_load");

    std::string loc_str = call.callee->loc.file + ":" + std::to_string(call.callee->loc.line);
    llvm::Value* fmt = builder_->CreateGlobalString(
        "Assertion failed at " + loc_str + ": expected false, got true\n");
    builder_->CreateCall(fprintf_fn, {stderr_val, fmt});

    llvm::Function* exit_fn = module_->getFunction("exit");
    builder_->CreateCall(exit_fn, {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1)
    });
    builder_->CreateUnreachable();

    // Pass block
    func->insert(func->end(), pass_bb);
    builder_->SetInsertPoint(pass_bb);

    return nullptr;
}

} // namespace shotgun
