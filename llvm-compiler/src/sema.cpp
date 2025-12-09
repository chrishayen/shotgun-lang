#include "sema.hpp"
#include <sstream>

namespace shotgun {

Sema::Sema() {
    // Register built-in functions
    // Note: print is handled specially in check_call to accept any type

    // read_file(path) -> str
    symbols_.declare_function("read_file", FunctionSig{
        "read_file", {}, {{"path", ResolvedType::make_str()}}, ResolvedType::make_str(), false
    });

    // write_file(path, content) -> void
    symbols_.declare_function("write_file", FunctionSig{
        "write_file", {},
        {{"path", ResolvedType::make_str()}, {"content", ResolvedType::make_str()}},
        ResolvedType::make_void(), false
    });
}

bool Sema::analyze(Program& prog) {
    // Pass 1: Register declarations
    register_decls(prog);

    if (has_errors()) return false;

    // Pass 2: Type check
    check_decls(prog);

    return !has_errors();
}

// === Pass 1: Register declarations ===

void Sema::register_decls(Program& prog) {
    for (const auto& decl : prog.decls) {
        register_decl(decl);
    }
}

void Sema::register_decl(const Decl& decl) {
    std::visit([this](const auto& d) {
        using T = std::decay_t<decltype(d)>;

        if constexpr (std::is_same_v<T, FnDecl>) {
            FunctionSig sig;
            sig.name = d.name;
            sig.type_params = d.type_params;
            for (const auto& p : d.params) {
                sig.params.push_back({p.name, resolve_type(p.type)});
            }
            sig.return_type = d.return_type ? resolve_type(d.return_type) : ResolvedType::make_void();
            symbols_.declare_function(d.name, sig);
        }
        else if constexpr (std::is_same_v<T, StructDecl>) {
            StructDef def;
            def.name = d.name;
            def.type_params = d.type_params;
            for (const auto& f : d.fields) {
                FieldInfo fi;
                fi.name = f.name;
                fi.type = resolve_type(f.type);
                fi.is_optional = f.optional;
                def.fields.push_back(fi);
            }
            symbols_.declare_struct(d.name, def);
        }
        else if constexpr (std::is_same_v<T, VariantDecl>) {
            VariantDef def;
            def.name = d.name;
            def.type_params = d.type_params;
            for (const auto& c : d.cases) {
                VariantCaseInfo ci;
                ci.name = c.name;
                for (const auto& f : c.fields) {
                    FieldInfo fi;
                    fi.name = f.name;
                    fi.type = resolve_type(f.type);
                    ci.fields.push_back(fi);
                }
                def.cases.push_back(ci);
            }
            symbols_.declare_variant(d.name, def);
        }
        else if constexpr (std::is_same_v<T, TraitDecl>) {
            TraitDef def;
            def.name = d.name;
            for (const auto& m : d.methods) {
                FunctionSig sig;
                sig.name = m.name;
                for (const auto& p : m.params) {
                    sig.params.push_back({p.name, resolve_type(p.type)});
                }
                sig.return_type = m.return_type ? resolve_type(m.return_type) : ResolvedType::make_void();
                sig.has_self = !m.params.empty() && m.params[0].name == "self";
                def.methods.push_back(sig);
            }
            symbols_.declare_trait(d.name, def);
        }
        else if constexpr (std::is_same_v<T, ErrorDecl>) {
            ErrorDef def;
            def.name = d.name;
            for (const auto& f : d.fields) {
                FieldInfo fi;
                fi.name = f.name;
                fi.type = resolve_type(f.type);
                def.fields.push_back(fi);
            }
            symbols_.declare_error(d.name, def);
        }
        else if constexpr (std::is_same_v<T, ImplDecl>) {
            // Register methods
            for (const auto& m : d.methods) {
                FunctionSig sig;
                sig.name = m.name;
                sig.type_params = d.type_params;
                for (const auto& p : m.params) {
                    sig.params.push_back({p.name, resolve_type(p.type)});
                }
                sig.return_type = m.return_type ? resolve_type(m.return_type) : ResolvedType::make_void();
                sig.has_self = !m.params.empty() && m.params[0].name == "self";

                if (d.trait_name.empty()) {
                    symbols_.declare_method(d.type_name, m.name, sig);
                } else {
                    symbols_.declare_impl(d.type_name, d.trait_name);
                    symbols_.declare_method(d.type_name, m.name, sig);
                }
            }
        }
        else if constexpr (std::is_same_v<T, UseDecl>) {
            // Module imports handled separately
        }
    }, decl);
}

// === Pass 2: Type checking ===

void Sema::check_decls(Program& prog) {
    for (const auto& decl : prog.decls) {
        check_decl(decl);
    }
}

void Sema::check_decl(const Decl& decl) {
    std::visit([this](const auto& d) {
        using T = std::decay_t<decltype(d)>;

        if constexpr (std::is_same_v<T, FnDecl>) {
            check_fn_decl(d);
        }
        else if constexpr (std::is_same_v<T, StructDecl>) {
            check_struct_decl(d);
        }
        else if constexpr (std::is_same_v<T, VariantDecl>) {
            check_variant_decl(d);
        }
        else if constexpr (std::is_same_v<T, ImplDecl>) {
            check_impl_decl(d);
        }
        else if constexpr (std::is_same_v<T, TraitDecl>) {
            check_trait_decl(d);
        }
        else if constexpr (std::is_same_v<T, ErrorDecl>) {
            check_error_decl(d);
        }
    }, decl);
}

void Sema::check_fn_decl(const FnDecl& fn) {
    current_function_ = fn.name;
    current_return_type_ = fn.return_type ? resolve_type(fn.return_type) : ResolvedType::make_void();

    symbols_.push_scope();

    // Add type parameters to scope
    for (const auto& tp : fn.type_params) {
        symbols_.declare_var(tp, ResolvedType::make_type_param(tp));
    }

    // Add parameters to scope
    for (const auto& p : fn.params) {
        auto ptype = resolve_type(p.type);
        symbols_.declare_var(p.name, ptype);
    }

    // Check body
    check_block(fn.body);

    symbols_.pop_scope();
    current_function_.clear();
}

void Sema::check_struct_decl(const StructDecl& s) {
    // Verify field types exist
    for (const auto& f : s.fields) {
        resolve_type(f.type);
    }
}

void Sema::check_variant_decl(const VariantDecl& v) {
    // Verify case field types exist
    for (const auto& c : v.cases) {
        for (const auto& f : c.fields) {
            resolve_type(f.type);
        }
    }
}

void Sema::check_impl_decl(const ImplDecl& impl) {
    // Check that the type exists
    bool found = symbols_.lookup_struct(impl.type_name).has_value() ||
                 symbols_.lookup_variant(impl.type_name).has_value();

    if (!found) {
        error("Unknown type '" + impl.type_name + "' in impl");
        return;
    }

    // Check each method
    for (const auto& m : impl.methods) {
        symbols_.push_scope();

        // Add type parameters
        for (const auto& tp : impl.type_params) {
            symbols_.declare_var(tp, ResolvedType::make_type_param(tp));
        }

        // Add self if present
        if (!m.params.empty() && m.params[0].name == "self") {
            std::vector<ResolvedTypePtr> type_args;
            for (const auto& tp : impl.type_params) {
                type_args.push_back(ResolvedType::make_type_param(tp));
            }
            auto self_type = ResolvedType::make_struct(impl.type_name, type_args);
            symbols_.declare_var("self", self_type);
        }

        // Add other parameters
        for (size_t i = (m.params.empty() || m.params[0].name != "self") ? 0 : 1; i < m.params.size(); i++) {
            symbols_.declare_var(m.params[i].name, resolve_type(m.params[i].type));
        }

        current_return_type_ = m.return_type ? resolve_type(m.return_type) : ResolvedType::make_void();
        check_block(m.body);

        symbols_.pop_scope();
    }
}

void Sema::check_trait_decl(const TraitDecl& t) {
    // Traits are just declarations, nothing to check
}

void Sema::check_error_decl(const ErrorDecl& e) {
    // Verify field types
    for (const auto& f : e.fields) {
        resolve_type(f.type);
    }
}

// === Statement checking ===

void Sema::check_stmt(const StmtPtr& stmt) {
    std::visit([this](const auto& s) {
        using T = std::decay_t<decltype(s)>;

        if constexpr (std::is_same_v<T, Stmt::VarDecl>) {
            check_var_decl(s);
        }
        else if constexpr (std::is_same_v<T, Stmt::Assign>) {
            check_assign(s);
        }
        else if constexpr (std::is_same_v<T, Stmt::If>) {
            check_if(s);
        }
        else if constexpr (std::is_same_v<T, Stmt::While>) {
            check_while(s);
        }
        else if constexpr (std::is_same_v<T, Stmt::For>) {
            check_for(s);
        }
        else if constexpr (std::is_same_v<T, Stmt::Return>) {
            check_return(s);
        }
        else if constexpr (std::is_same_v<T, Stmt::Go>) {
            check_go(s);
        }
        else if constexpr (std::is_same_v<T, Stmt::Break>) {
            if (!in_loop_) {
                error("'break' outside of loop");
            }
        }
        else if constexpr (std::is_same_v<T, Stmt::Continue>) {
            if (!in_loop_) {
                error("'continue' outside of loop");
            }
        }
        else if constexpr (std::is_same_v<T, Stmt::ExprStmt>) {
            check_expr(s.expr);
        }
        else if constexpr (std::is_same_v<T, Stmt::ChanSend>) {
            check_expr(s.channel);
            check_expr(s.value);
        }
    }, stmt->kind);
}

void Sema::check_var_decl(const Stmt::VarDecl& var) {
    ResolvedTypePtr var_type;

    if (var.type) {
        var_type = resolve_type(var.type);
    }

    if (var.init) {
        auto init_type = check_expr(var.init);

        if (var_type) {
            if (!is_assignable(var_type, init_type)) {
                error("Cannot assign " + init_type->to_string() + " to " + var_type->to_string());
            }
        } else {
            var_type = init_type;
        }
    }

    if (!var_type) {
        error("Cannot infer type for '" + var.name + "'");
        var_type = ResolvedType::make_void();
    }

    if (!symbols_.declare_var(var.name, var_type, var.is_const)) {
        error("Variable '" + var.name + "' already declared in this scope");
    }
}

void Sema::check_assign(const Stmt::Assign& assign) {
    auto target_type = check_expr(assign.target);
    auto value_type = check_expr(assign.value);

    // Check if target is an identifier and is const
    if (auto* ident = std::get_if<Expr::Ident>(&assign.target->kind)) {
        auto var = symbols_.lookup_var(ident->name);
        if (var && var->is_const) {
            error("Cannot assign to constant '" + ident->name + "'");
        }
    }

    // For compound assignments, check operand types
    if (assign.op != TokenKind::Assign) {
        // +=, -=, *=, /= require numeric types
        bool is_numeric = target_type->kind == ResolvedType::Kind::Int ||
                         target_type->kind == ResolvedType::Kind::F32 ||
                         target_type->kind == ResolvedType::Kind::F64 ||
                         target_type->kind == ResolvedType::Kind::U32 ||
                         target_type->kind == ResolvedType::Kind::U64;

        // += also works on strings
        if (assign.op == TokenKind::PlusEq && target_type->kind == ResolvedType::Kind::Str) {
            is_numeric = true;
        }

        if (!is_numeric) {
            error("Compound assignment requires numeric type");
        }
    }

    if (!is_assignable(target_type, value_type)) {
        error("Cannot assign " + value_type->to_string() + " to " + target_type->to_string());
    }
}

void Sema::check_if(const Stmt::If& if_stmt) {
    auto cond_type = check_expr(if_stmt.condition);

    // Condition must be bool or truthy (optional)
    if (cond_type->kind != ResolvedType::Kind::Bool &&
        cond_type->kind != ResolvedType::Kind::Optional) {
        error("Condition must be bool, got " + cond_type->to_string());
    }

    symbols_.push_scope();
    check_block(if_stmt.then_block);
    symbols_.pop_scope();

    if (!if_stmt.else_block.empty()) {
        symbols_.push_scope();
        check_block(if_stmt.else_block);
        symbols_.pop_scope();
    }
}

void Sema::check_while(const Stmt::While& while_stmt) {
    auto cond_type = check_expr(while_stmt.condition);

    if (cond_type->kind != ResolvedType::Kind::Bool) {
        error("While condition must be bool, got " + cond_type->to_string());
    }

    bool was_in_loop = in_loop_;
    in_loop_ = true;

    symbols_.push_scope();
    check_block(while_stmt.body);
    symbols_.pop_scope();

    in_loop_ = was_in_loop;
}

void Sema::check_for(const Stmt::For& for_stmt) {
    auto iter_type = check_expr(for_stmt.iterable);

    symbols_.push_scope();

    // Determine element type based on iterable
    ResolvedTypePtr elem_type;

    if (iter_type->kind == ResolvedType::Kind::Array) {
        elem_type = iter_type->element_type;
    }
    else if (iter_type->kind == ResolvedType::Kind::Str) {
        elem_type = ResolvedType::make_char();
    }
    else if (iter_type->kind == ResolvedType::Kind::Struct && iter_type->name == "Map") {
        // Map iteration gives key, value
        if (for_stmt.var2.empty()) {
            elem_type = iter_type->type_args[0]; // key type
        } else {
            symbols_.declare_var(for_stmt.var, iter_type->type_args[0]);
            symbols_.declare_var(for_stmt.var2, iter_type->type_args[1]);
            elem_type = nullptr; // Already handled
        }
    }
    else {
        error("Cannot iterate over " + iter_type->to_string());
        elem_type = ResolvedType::make_void();
    }

    if (elem_type) {
        symbols_.declare_var(for_stmt.var, elem_type);
    }

    bool was_in_loop = in_loop_;
    in_loop_ = true;

    check_block(for_stmt.body);

    symbols_.pop_scope();
    in_loop_ = was_in_loop;
}

void Sema::check_return(const Stmt::Return& ret) {
    if (ret.value) {
        auto val_type = check_expr(ret.value);
        if (!is_assignable(current_return_type_, val_type)) {
            error("Return type mismatch: expected " + current_return_type_->to_string() +
                  ", got " + val_type->to_string());
        }
    } else if (current_return_type_->kind != ResolvedType::Kind::Void) {
        error("Missing return value in function returning " + current_return_type_->to_string());
    }
}

void Sema::check_go(const Stmt::Go& go) {
    check_expr(go.call);
}

void Sema::check_block(const std::vector<StmtPtr>& block) {
    for (const auto& stmt : block) {
        check_stmt(stmt);
    }
}

// === Expression checking ===

ResolvedTypePtr Sema::check_expr(const ExprPtr& expr) {
    return std::visit([this](const auto& e) -> ResolvedTypePtr {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, Expr::IntLit>) {
            return check_int_lit(e);
        }
        else if constexpr (std::is_same_v<T, Expr::FloatLit>) {
            return check_float_lit(e);
        }
        else if constexpr (std::is_same_v<T, Expr::StringLit>) {
            return check_string_lit(e);
        }
        else if constexpr (std::is_same_v<T, Expr::CharLit>) {
            return check_char_lit(e);
        }
        else if constexpr (std::is_same_v<T, Expr::BoolLit>) {
            return check_bool_lit(e);
        }
        else if constexpr (std::is_same_v<T, Expr::NoneLit>) {
            return check_none_lit(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Ident>) {
            return check_ident(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Binary>) {
            return check_binary(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Unary>) {
            return check_unary(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Call>) {
            return check_call(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Index>) {
            return check_index(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Field>) {
            return check_field(e);
        }
        else if constexpr (std::is_same_v<T, Expr::StructLit>) {
            return check_struct_lit(e);
        }
        else if constexpr (std::is_same_v<T, Expr::ArrayLit>) {
            return check_array_lit(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Match>) {
            return check_match(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Lambda>) {
            return check_lambda(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Or>) {
            return check_or_expr(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Is>) {
            return check_is_expr(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Cast>) {
            return check_cast(e);
        }
        else if constexpr (std::is_same_v<T, Expr::Block>) {
            return check_block(e);
        }
        else if constexpr (std::is_same_v<T, Expr::ChanRecv>) {
            return check_chan_recv(e);
        }
        else {
            return ResolvedType::make_void();
        }
    }, expr->kind);
}

ResolvedTypePtr Sema::check_int_lit(const Expr::IntLit& lit) {
    return ResolvedType::make_int();
}

ResolvedTypePtr Sema::check_float_lit(const Expr::FloatLit& lit) {
    return ResolvedType::make_f64();
}

ResolvedTypePtr Sema::check_string_lit(const Expr::StringLit& lit) {
    // Type-check any interpolated expressions in f-strings
    for (const auto& expr : lit.exprs) {
        if (expr) {
            check_expr(expr);
            // Any type can be interpolated - will be converted to string at runtime
        }
    }
    return ResolvedType::make_str();
}

ResolvedTypePtr Sema::check_char_lit(const Expr::CharLit& lit) {
    return ResolvedType::make_char();
}

ResolvedTypePtr Sema::check_bool_lit(const Expr::BoolLit& lit) {
    return ResolvedType::make_bool();
}

ResolvedTypePtr Sema::check_none_lit(const Expr::NoneLit& lit) {
    // NoneLit needs context to determine type - for now return optional void
    error("'none' requires type context");
    return ResolvedType::make_optional(ResolvedType::make_void());
}

ResolvedTypePtr Sema::check_ident(const Expr::Ident& ident) {
    auto var = symbols_.lookup_var(ident.name);
    if (var) {
        return var->type;
    }

    auto fn = symbols_.lookup_function(ident.name);
    if (fn) {
        std::vector<ResolvedTypePtr> param_types;
        for (const auto& p : fn->params) {
            param_types.push_back(p.second);
        }
        return ResolvedType::make_function(param_types, fn->return_type);
    }

    error("Undefined variable '" + ident.name + "'");
    return ResolvedType::make_void();
}

ResolvedTypePtr Sema::check_binary(const Expr::Binary& bin) {
    auto left = check_expr(bin.left);
    auto right = check_expr(bin.right);

    switch (bin.op) {
        case TokenKind::Plus:
            // + works on numbers and strings
            if (left->kind == ResolvedType::Kind::Str && right->kind == ResolvedType::Kind::Str) {
                return ResolvedType::make_str();
            }
            [[fallthrough]];
        case TokenKind::Minus:
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:
            if (!types_equal(left, right)) {
                error("Binary operator requires matching types");
            }
            return left;

        case TokenKind::Eq:
        case TokenKind::NotEq:
            return ResolvedType::make_bool();

        case TokenKind::Lt:
        case TokenKind::Gt:
        case TokenKind::LtEq:
        case TokenKind::GtEq:
            return ResolvedType::make_bool();

        case TokenKind::AndAnd:
        case TokenKind::OrOr:
            if (left->kind != ResolvedType::Kind::Bool || right->kind != ResolvedType::Kind::Bool) {
                error("Logical operators require bool operands");
            }
            return ResolvedType::make_bool();

        case TokenKind::In:
            // `in` checks membership: x in array
            if (right->kind == ResolvedType::Kind::Array) {
                // Check that left matches array element type
                if (!types_equal(left, right->element_type)) {
                    error("'in' operator: element type mismatch");
                }
            } else {
                error("'in' operator requires array on right side");
            }
            return ResolvedType::make_bool();

        default:
            error("Unknown binary operator");
            return ResolvedType::make_void();
    }
}

ResolvedTypePtr Sema::check_unary(const Expr::Unary& un) {
    auto operand = check_expr(un.operand);

    switch (un.op) {
        case TokenKind::Minus:
            if (operand->kind != ResolvedType::Kind::Int &&
                operand->kind != ResolvedType::Kind::F32 &&
                operand->kind != ResolvedType::Kind::F64) {
                error("Unary minus requires numeric type");
            }
            return operand;

        case TokenKind::Not:
            if (operand->kind != ResolvedType::Kind::Bool) {
                error("Unary not requires bool type");
            }
            return ResolvedType::make_bool();

        default:
            error("Unknown unary operator");
            return ResolvedType::make_void();
    }
}

ResolvedTypePtr Sema::check_call(const Expr::Call& call) {
    // Check for built-in functions FIRST before evaluating callee
    if (auto* ident = std::get_if<Expr::Ident>(&call.callee->kind)) {
        // print accepts any type
        if (ident->name == "print") {
            if (call.args.size() != 1) {
                error("print expects 1 argument, got " + std::to_string(call.args.size()));
            }
            if (!call.args.empty()) {
                check_expr(call.args[0]);  // Just type-check, accept any type
            }
            return ResolvedType::make_void();
        }
    }

    // Check for method call BEFORE evaluating callee as expression
    // This prevents field access errors for method names
    if (auto* field = std::get_if<Expr::Field>(&call.callee->kind)) {
        auto obj_type = check_expr(field->object);

        // Check built-in methods first
        auto builtin = symbols_.lookup_builtin_method(obj_type, field->name);
        if (builtin) {
            // Check argument count
            if (call.args.size() != builtin->params.size()) {
                error("Method '" + field->name + "' expects " +
                      std::to_string(builtin->params.size()) + " arguments, got " +
                      std::to_string(call.args.size()));
            }
            // Check argument types
            for (size_t i = 0; i < call.args.size() && i < builtin->params.size(); i++) {
                auto arg_type = check_expr(call.args[i]);
                if (!is_assignable(builtin->params[i].second, arg_type)) {
                    error("Argument type mismatch in call to '" + field->name + "'");
                }
            }
            return builtin->return_type;
        }

        // User-defined method
        std::string type_name;
        if (obj_type->kind == ResolvedType::Kind::Struct ||
            obj_type->kind == ResolvedType::Kind::Variant) {
            type_name = obj_type->name;
        }

        auto method = symbols_.lookup_method(type_name, field->name);
        if (method) {
            // Skip self parameter in argument check
            size_t param_start = method->has_self ? 1 : 0;
            size_t expected = method->params.size() - param_start;

            if (call.args.size() != expected) {
                error("Method '" + field->name + "' expects " +
                      std::to_string(expected) + " arguments, got " +
                      std::to_string(call.args.size()));
            }

            for (size_t i = 0; i < call.args.size() && i + param_start < method->params.size(); i++) {
                auto arg_type = check_expr(call.args[i]);
                auto param_type = method->params[i + param_start].second;
                if (!is_assignable(param_type, arg_type)) {
                    error("Argument type mismatch in call to '" + field->name + "'");
                }
            }

            return method->return_type;
        }

        error("Unknown method '" + field->name + "' on type " + obj_type->to_string());
        return ResolvedType::make_void();
    }

    // For non-method calls, evaluate the callee
    auto callee_type = check_expr(call.callee);

    // Regular function call
    if (callee_type->kind != ResolvedType::Kind::Function) {
        // Could be a direct function name
        if (auto* ident = std::get_if<Expr::Ident>(&call.callee->kind)) {
            auto fn = symbols_.lookup_function(ident->name);
            if (fn) {
                if (call.args.size() != fn->params.size()) {
                    error("Function '" + ident->name + "' expects " +
                          std::to_string(fn->params.size()) + " arguments, got " +
                          std::to_string(call.args.size()));
                }

                for (size_t i = 0; i < call.args.size() && i < fn->params.size(); i++) {
                    auto arg_type = check_expr(call.args[i]);
                    if (!is_assignable(fn->params[i].second, arg_type)) {
                        error("Argument type mismatch in call to '" + ident->name + "'");
                    }
                }

                return fn->return_type;
            }
        }

        error("Cannot call non-function type");
        return ResolvedType::make_void();
    }

    // Function type call
    if (call.args.size() != callee_type->param_types.size()) {
        error("Function expects " + std::to_string(callee_type->param_types.size()) +
              " arguments, got " + std::to_string(call.args.size()));
    }

    for (size_t i = 0; i < call.args.size() && i < callee_type->param_types.size(); i++) {
        auto arg_type = check_expr(call.args[i]);
        if (!is_assignable(callee_type->param_types[i], arg_type)) {
            error("Argument type mismatch");
        }
    }

    return callee_type->return_type;
}

ResolvedTypePtr Sema::check_index(const Expr::Index& idx) {
    auto obj_type = check_expr(idx.object);
    auto index_type = check_expr(idx.index);

    if (obj_type->kind == ResolvedType::Kind::Array) {
        if (index_type->kind != ResolvedType::Kind::Int) {
            error("Array index must be int");
        }
        return obj_type->element_type;
    }

    if (obj_type->kind == ResolvedType::Kind::Str) {
        if (index_type->kind != ResolvedType::Kind::Int) {
            error("String index must be int");
        }
        return ResolvedType::make_char();
    }

    // Map indexing
    if (obj_type->kind == ResolvedType::Kind::Struct && obj_type->name == "Map") {
        if (!is_assignable(obj_type->type_args[0], index_type)) {
            error("Map key type mismatch");
        }
        return obj_type->type_args[1];
    }

    error("Cannot index type " + obj_type->to_string());
    return ResolvedType::make_void();
}

ResolvedTypePtr Sema::check_field(const Expr::Field& field) {
    auto obj_type = check_expr(field.object);

    if (obj_type->kind == ResolvedType::Kind::Struct) {
        auto def = symbols_.lookup_struct(obj_type->name);
        if (def) {
            for (const auto& f : def->fields) {
                if (f.name == field.name) {
                    // Substitute type parameters if generic
                    if (!obj_type->type_args.empty() && !def->type_params.empty()) {
                        std::unordered_map<std::string, ResolvedTypePtr> subst;
                        for (size_t i = 0; i < def->type_params.size() && i < obj_type->type_args.size(); i++) {
                            subst[def->type_params[i]] = obj_type->type_args[i];
                        }
                        return f.type->substitute(subst);
                    }
                    return f.type;
                }
            }
        }
        error("Unknown field '" + field.name + "' on struct " + obj_type->name);
    }

    error("Cannot access field on type " + obj_type->to_string());
    return ResolvedType::make_void();
}

ResolvedTypePtr Sema::check_struct_lit(const Expr::StructLit& lit) {
    // Check if it's a variant construction
    if (lit.type_name.find('.') != std::string::npos) {
        // Variant.Case { ... }
        size_t dot = lit.type_name.find('.');
        std::string variant_name = lit.type_name.substr(0, dot);
        std::string case_name = lit.type_name.substr(dot + 1);

        auto def = symbols_.lookup_variant(variant_name);
        if (!def) {
            error("Unknown variant '" + variant_name + "'");
            return ResolvedType::make_void();
        }

        // Find the case
        for (const auto& c : def->cases) {
            if (c.name == case_name) {
                // Check fields
                for (const auto& [fname, fexpr] : lit.fields) {
                    bool found = false;
                    for (const auto& cf : c.fields) {
                        if (cf.name == fname) {
                            auto val_type = check_expr(fexpr);
                            if (!is_assignable(cf.type, val_type)) {
                                error("Field '" + fname + "' type mismatch");
                            }
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        error("Unknown field '" + fname + "' in variant case " + case_name);
                    }
                }

                std::vector<ResolvedTypePtr> type_args;
                for (const auto& tp : def->type_params) {
                    type_args.push_back(ResolvedType::make_type_param(tp));
                }
                return ResolvedType::make_variant(variant_name, type_args);
            }
        }

        error("Unknown case '" + case_name + "' in variant " + variant_name);
        return ResolvedType::make_void();
    }

    // Regular struct
    auto def = symbols_.lookup_struct(lit.type_name);
    if (!def) {
        error("Unknown struct '" + lit.type_name + "'");
        return ResolvedType::make_void();
    }

    // Build type parameter inference map from field values
    std::unordered_map<std::string, ResolvedTypePtr> inferred;

    // First pass: infer type parameters from field values
    for (const auto& [fname, fexpr] : lit.fields) {
        for (const auto& f : def->fields) {
            if (f.name == fname) {
                auto val_type = check_expr(fexpr);
                // If field type is a type parameter, infer it
                if (f.type->kind == ResolvedType::Kind::TypeParam) {
                    inferred[f.type->param_name] = val_type;
                }
                break;
            }
        }
    }

    // Second pass: check field types with substitution
    for (const auto& [fname, fexpr] : lit.fields) {
        bool found = false;
        for (const auto& f : def->fields) {
            if (f.name == fname) {
                auto val_type = check_expr(fexpr);
                auto expected_type = f.type->substitute(inferred);
                if (!is_assignable(expected_type, val_type)) {
                    error("Field '" + fname + "' type mismatch");
                }
                found = true;
                break;
            }
        }
        if (!found) {
            error("Unknown field '" + fname + "' in struct " + lit.type_name);
        }
    }

    // Build type args from inferred or remaining params
    std::vector<ResolvedTypePtr> type_args;
    for (const auto& tp : def->type_params) {
        auto it = inferred.find(tp);
        if (it != inferred.end()) {
            type_args.push_back(it->second);
        } else {
            type_args.push_back(ResolvedType::make_type_param(tp));
        }
    }
    return ResolvedType::make_struct(lit.type_name, type_args);
}

ResolvedTypePtr Sema::check_array_lit(const Expr::ArrayLit& lit) {
    if (lit.elements.empty()) {
        error("Cannot infer type of empty array literal");
        return ResolvedType::make_array(ResolvedType::make_void());
    }

    auto elem_type = check_expr(lit.elements[0]);
    for (size_t i = 1; i < lit.elements.size(); i++) {
        auto t = check_expr(lit.elements[i]);
        if (!types_equal(elem_type, t)) {
            error("Array elements must have consistent types");
        }
    }

    return ResolvedType::make_array(elem_type);
}

ResolvedTypePtr Sema::check_match(const Expr::Match& match) {
    auto scrutinee_type = check_expr(match.subject);

    ResolvedTypePtr result_type = nullptr;

    for (const auto& [pattern, body] : match.arms) {
        symbols_.push_scope();

        // Check pattern and bind variables
        check_pattern(pattern, scrutinee_type);

        // Check arm body
        auto arm_type = check_expr(body);

        if (!result_type) {
            result_type = arm_type;
        } else if (!types_equal(result_type, arm_type)) {
            error("Match arms must have consistent types");
        }

        symbols_.pop_scope();
    }

    return result_type ? result_type : ResolvedType::make_void();
}

ResolvedTypePtr Sema::check_lambda(const Expr::Lambda& lambda) {
    symbols_.push_scope();

    std::vector<ResolvedTypePtr> param_types;
    for (const auto& [name, type] : lambda.params) {
        auto ptype = resolve_type(type);
        param_types.push_back(ptype);
        symbols_.declare_var(name, ptype);
    }

    // Check body - last expression determines return type
    ResolvedTypePtr return_type = ResolvedType::make_void();
    for (size_t i = 0; i < lambda.body.size(); i++) {
        if (i == lambda.body.size() - 1) {
            // Last statement - could be expression or return
            if (auto* expr_stmt = std::get_if<Stmt::ExprStmt>(&lambda.body[i]->kind)) {
                return_type = check_expr(expr_stmt->expr);
            } else if (auto* ret = std::get_if<Stmt::Return>(&lambda.body[i]->kind)) {
                if (ret->value) {
                    return_type = check_expr(ret->value);
                }
            } else {
                check_stmt(lambda.body[i]);
            }
        } else {
            check_stmt(lambda.body[i]);
        }
    }

    symbols_.pop_scope();

    return ResolvedType::make_function(param_types, return_type);
}

ResolvedTypePtr Sema::check_or_expr(const Expr::Or& or_expr) {
    auto left = check_expr(or_expr.left);

    if (left->kind != ResolvedType::Kind::Optional) {
        error("'or' requires optional type on left");
        return left;
    }

    // or return - type is the unwrapped optional
    if (or_expr.is_return) {
        return left->element_type;
    }

    // or default - right must be compatible with unwrapped type
    if (or_expr.right) {
        auto right = check_expr(or_expr.right);
        if (!is_assignable(left->element_type, right)) {
            error("'or' fallback type mismatch");
        }
    }

    return left->element_type;
}

ResolvedTypePtr Sema::check_is_expr(const Expr::Is& is_expr) {
    check_expr(is_expr.value);
    // 'is' always returns bool
    return ResolvedType::make_bool();
}

ResolvedTypePtr Sema::check_cast(const Expr::Cast& cast) {
    check_expr(cast.value);
    return resolve_type(cast.target_type);
}

ResolvedTypePtr Sema::check_block(const Expr::Block& block) {
    symbols_.push_scope();

    for (const auto& stmt : block.stmts) {
        check_stmt(stmt);
    }

    ResolvedTypePtr result = ResolvedType::make_void();
    if (block.result) {
        result = check_expr(block.result);
    }

    symbols_.pop_scope();
    return result;
}

ResolvedTypePtr Sema::check_chan_recv(const Expr::ChanRecv& recv) {
    auto chan_type = check_expr(recv.channel);

    if (chan_type->kind != ResolvedType::Kind::Channel) {
        error("Cannot receive from non-channel type");
        return ResolvedType::make_void();
    }

    return chan_type->element_type;
}

// === Type resolution ===

ResolvedTypePtr Sema::resolve_type(const TypePtr& type) {
    if (!type) return ResolvedType::make_void();

    return std::visit([this](const auto& t) -> ResolvedTypePtr {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, Type::Named>) {
            // Built-in types
            if (t.name == "int") return ResolvedType::make_int();
            if (t.name == "bool") return ResolvedType::make_bool();
            if (t.name == "str") return ResolvedType::make_str();
            if (t.name == "char") return ResolvedType::make_char();
            if (t.name == "f32") return ResolvedType::make_f32();
            if (t.name == "f64") return ResolvedType::make_f64();
            if (t.name == "u32") return ResolvedType::make_u32();
            if (t.name == "u64") return ResolvedType::make_u64();
            if (t.name == "void") return ResolvedType::make_void();

            // Type parameter?
            auto var = symbols_.lookup_var(t.name);
            if (var && var->type->kind == ResolvedType::Kind::TypeParam) {
                return ResolvedType::make_type_param(t.name);
            }

            // User-defined struct
            if (symbols_.lookup_struct(t.name)) {
                std::vector<ResolvedTypePtr> type_args;
                for (const auto& arg : t.type_args) {
                    type_args.push_back(resolve_type(arg));
                }
                return ResolvedType::make_struct(t.name, type_args);
            }

            // User-defined variant
            if (symbols_.lookup_variant(t.name)) {
                std::vector<ResolvedTypePtr> type_args;
                for (const auto& arg : t.type_args) {
                    type_args.push_back(resolve_type(arg));
                }
                return ResolvedType::make_variant(t.name, type_args);
            }

            // Error type
            if (symbols_.lookup_error(t.name)) {
                return ResolvedType::make_error(t.name);
            }

            // Could be a type parameter not yet in scope
            return ResolvedType::make_type_param(t.name);
        }
        else if constexpr (std::is_same_v<T, Type::Array>) {
            return ResolvedType::make_array(resolve_type(t.element));
        }
        else if constexpr (std::is_same_v<T, Type::Optional>) {
            return ResolvedType::make_optional(resolve_type(t.inner));
        }
        else if constexpr (std::is_same_v<T, Type::Channel>) {
            return ResolvedType::make_channel(resolve_type(t.element));
        }
        else if constexpr (std::is_same_v<T, Type::Function>) {
            std::vector<ResolvedTypePtr> params;
            for (const auto& p : t.params) {
                params.push_back(resolve_type(p));
            }
            return ResolvedType::make_function(params, resolve_type(t.return_type));
        }
        else if constexpr (std::is_same_v<T, Type::Result>) {
            // Result types - use the value type
            return resolve_type(t.value);
        }
        else {
            return ResolvedType::make_void();
        }
    }, type->kind);
}

// === Pattern checking ===

ResolvedTypePtr Sema::check_pattern(const PatternPtr& pattern, ResolvedTypePtr expected) {
    return std::visit([this, expected](const auto& p) -> ResolvedTypePtr {
        using T = std::decay_t<decltype(p)>;

        if constexpr (std::is_same_v<T, Pattern::Wildcard>) {
            return expected;
        }
        else if constexpr (std::is_same_v<T, Pattern::Ident>) {
            symbols_.declare_var(p.name, expected);
            return expected;
        }
        else if constexpr (std::is_same_v<T, Pattern::Literal>) {
            // Check literal type matches expected
            return expected;
        }
        else if constexpr (std::is_same_v<T, Pattern::Tuple>) {
            // Not fully supported yet
            return expected;
        }
        else if constexpr (std::is_same_v<T, Pattern::Variant>) {
            // Match variant case
            return expected;
        }
        else {
            return expected;
        }
    }, pattern->kind);
}

// === Type compatibility ===

bool Sema::is_assignable(ResolvedTypePtr target, ResolvedTypePtr source) {
    if (types_equal(target, source)) return true;

    // T is assignable to T?
    if (target->kind == ResolvedType::Kind::Optional) {
        if (types_equal(target->element_type, source)) return true;
    }

    return false;
}

bool Sema::types_equal(ResolvedTypePtr a, ResolvedTypePtr b) {
    if (!a || !b) return false;
    return a->equals(*b);
}

// === Error reporting ===

void Sema::error(const std::string& msg) {
    std::ostringstream oss;
    if (!current_function_.empty()) {
        oss << "in function '" << current_function_ << "': ";
    }
    oss << msg;
    errors_.push_back(oss.str());
}

void Sema::error_at(const SourceLoc& loc, const std::string& msg) {
    std::ostringstream oss;
    oss << loc.line << ":" << loc.column << ": " << msg;
    errors_.push_back(oss.str());
}

} // namespace shotgun
