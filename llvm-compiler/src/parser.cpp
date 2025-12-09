#include "parser.hpp"
#include <sstream>

namespace shotgun {

Parser::Parser(Lexer& lexer) : lexer_(lexer) {
    tokens_ = lexer_.tokenize();
}

Token Parser::peek() const {
    if (current_ >= tokens_.size()) {
        return tokens_.back(); // EOF
    }
    return tokens_[current_];
}

Token Parser::peek_next() const {
    if (current_ + 1 >= tokens_.size()) {
        return tokens_.back();
    }
    return tokens_[current_ + 1];
}

Token Parser::advance() {
    if (!at_end()) {
        current_++;
    }
    return tokens_[current_ - 1];
}

bool Parser::at_end() const {
    return peek().kind == TokenKind::Eof;
}

bool Parser::check(TokenKind kind) const {
    return peek().kind == kind;
}

bool Parser::match(TokenKind kind) {
    if (check(kind)) {
        advance();
        return true;
    }
    return false;
}

Token Parser::consume(TokenKind kind, const std::string& msg) {
    if (check(kind)) {
        return advance();
    }
    error(msg + ", got " + token_kind_name(peek().kind));
    return peek();
}

void Parser::skip_newlines() {
    while (check(TokenKind::Newline)) {
        advance();
    }
}

void Parser::expect_newline_or_eof() {
    if (!check(TokenKind::Newline) && !check(TokenKind::Eof) && !check(TokenKind::RBrace)) {
        error("Expected newline or end of statement");
    }
    skip_newlines();
}

void Parser::error(const std::string& msg) {
    error_at(peek().loc, msg);
}

void Parser::error_at(const SourceLoc& loc, const std::string& msg) {
    std::stringstream ss;
    ss << loc.file << ":" << loc.line << ":" << loc.column << ": " << msg;
    errors_.push_back(ss.str());
}

void Parser::synchronize() {
    advance();
    while (!at_end()) {
        if (tokens_[current_ - 1].kind == TokenKind::Newline) return;

        switch (peek().kind) {
            case TokenKind::Fn:
            case TokenKind::Struct:
            case TokenKind::Variant:
            case TokenKind::Trait:
            case TokenKind::Impl:
            case TokenKind::Error:
            case TokenKind::Uses:
            case TokenKind::If:
            case TokenKind::For:
            case TokenKind::While:
            case TokenKind::Return:
                return;
            default:
                advance();
        }
    }
}

Program Parser::parse() {
    Program prog;
    skip_newlines();

    while (!at_end()) {
        try {
            prog.decls.push_back(parse_decl());
        } catch (...) {
            synchronize();
        }
        skip_newlines();
    }

    return prog;
}

Decl Parser::parse_decl() {
    skip_newlines();

    if (match(TokenKind::Fn)) {
        return parse_fn_decl();
    }

    if (match(TokenKind::Uses)) {
        return parse_use_decl();
    }

    // Name :: struct/variant/trait/impl/error OR Name :: method(...)
    if (check(TokenKind::Ident)) {
        Token name = advance();
        std::vector<std::string> type_params;

        // Check for generic params
        if (match(TokenKind::Lt)) {
            type_params = parse_type_params();
            consume(TokenKind::Gt, "Expected '>' after type parameters");
        }

        consume(TokenKind::ColonColon, "Expected '::' after type name");

        if (match(TokenKind::Struct)) {
            return parse_struct_decl(name.lexeme, type_params);
        }
        if (match(TokenKind::Variant)) {
            return parse_variant_decl(name.lexeme, type_params);
        }
        if (match(TokenKind::Trait)) {
            return parse_trait_decl(name.lexeme);
        }
        if (match(TokenKind::Impl)) {
            return parse_impl_decl(name.lexeme, type_params);
        }
        if (match(TokenKind::Error)) {
            return parse_error_decl(name.lexeme);
        }

        // Otherwise it's a method: Person :: greet(self) { ... }
        return parse_method(name.lexeme, type_params);
    }

    error("Expected declaration");
    throw std::runtime_error("parse error");
}

FnDecl Parser::parse_fn_decl() {
    FnDecl fn;
    fn.loc = peek().loc;

    Token name = consume(TokenKind::Ident, "Expected function name");
    fn.name = name.lexeme;

    // Optional type params
    if (match(TokenKind::Lt)) {
        fn.type_params = parse_type_params();
        consume(TokenKind::Gt, "Expected '>' after type parameters");
    }

    // Parameters (optional parens - fn main { } is valid)
    if (match(TokenKind::LParen)) {
        if (!check(TokenKind::RParen)) {
            fn.params = parse_params();
        }
        consume(TokenKind::RParen, "Expected ')' after parameters");
    }

    // Return type (optional)
    if (!check(TokenKind::LBrace) && !check(TokenKind::Newline)) {
        fn.return_type = parse_type();
    }

    // Body
    consume(TokenKind::LBrace, "Expected '{' before function body");
    fn.body = parse_block();
    consume(TokenKind::RBrace, "Expected '}' after function body");

    return fn;
}

ImplDecl Parser::parse_method(const std::string& type_name, std::vector<std::string> type_params) {
    FnDecl method;
    method.loc = peek().loc;

    Token name = consume(TokenKind::Ident, "Expected method name");
    method.name = name.lexeme;
    method.type_params = type_params;

    consume(TokenKind::LParen, "Expected '(' after method name");
    method.params = parse_params();
    consume(TokenKind::RParen, "Expected ')' after parameters");

    // Return type (optional)
    if (!check(TokenKind::LBrace) && !check(TokenKind::Newline)) {
        method.return_type = parse_type();
    }

    consume(TokenKind::LBrace, "Expected '{' before method body");
    method.body = parse_block();
    consume(TokenKind::RBrace, "Expected '}' after method body");

    // Wrap in ImplDecl
    ImplDecl impl;
    impl.loc = method.loc;
    impl.type_name = type_name;
    impl.type_params = type_params;
    impl.methods.push_back(std::move(method));

    return impl;
}

StructDecl Parser::parse_struct_decl(const std::string& name, std::vector<std::string> type_params) {
    StructDecl decl;
    decl.loc = peek().loc;
    decl.name = name;
    decl.type_params = type_params;

    consume(TokenKind::LBrace, "Expected '{' after 'struct'");
    decl.fields = parse_fields();
    consume(TokenKind::RBrace, "Expected '}' after struct fields");

    return decl;
}

VariantDecl Parser::parse_variant_decl(const std::string& name, std::vector<std::string> type_params) {
    VariantDecl decl;
    decl.loc = peek().loc;
    decl.name = name;
    decl.type_params = type_params;

    consume(TokenKind::LBrace, "Expected '{' after 'variant'");
    skip_newlines();

    while (!check(TokenKind::RBrace) && !at_end()) {
        VariantDecl::Case case_;
        Token case_name = consume(TokenKind::Ident, "Expected variant case name");
        case_.name = case_name.lexeme;

        if (match(TokenKind::LBrace)) {
            case_.fields = parse_fields();
            consume(TokenKind::RBrace, "Expected '}' after variant case fields");
        }

        decl.cases.push_back(std::move(case_));
        skip_newlines();

        if (match(TokenKind::Comma)) {
            skip_newlines();
        }
    }

    consume(TokenKind::RBrace, "Expected '}' after variant cases");
    return decl;
}

TraitDecl Parser::parse_trait_decl(const std::string& name) {
    TraitDecl decl;
    decl.loc = peek().loc;
    decl.name = name;

    consume(TokenKind::LBrace, "Expected '{' after 'trait'");
    skip_newlines();

    while (!check(TokenKind::RBrace) && !at_end()) {
        TraitDecl::Method method;
        Token method_name = consume(TokenKind::Ident, "Expected method name");
        method.name = method_name.lexeme;

        consume(TokenKind::LParen, "Expected '(' after method name");
        method.params = parse_params();
        consume(TokenKind::RParen, "Expected ')' after parameters");

        if (!check(TokenKind::Newline) && !check(TokenKind::RBrace)) {
            method.return_type = parse_type();
        }

        decl.methods.push_back(std::move(method));
        skip_newlines();
    }

    consume(TokenKind::RBrace, "Expected '}' after trait methods");
    return decl;
}

ImplDecl Parser::parse_impl_decl(const std::string& type_name, std::vector<std::string> type_params) {
    ImplDecl decl;
    decl.loc = peek().loc;
    decl.type_name = type_name;
    decl.type_params = type_params;

    Token trait_name = consume(TokenKind::Ident, "Expected trait name");
    decl.trait_name = trait_name.lexeme;

    consume(TokenKind::LBrace, "Expected '{' after trait name");
    skip_newlines();

    while (!check(TokenKind::RBrace) && !at_end()) {
        FnDecl method;
        method.loc = peek().loc;

        Token method_name = consume(TokenKind::Ident, "Expected method name");
        method.name = method_name.lexeme;

        consume(TokenKind::LParen, "Expected '(' after method name");
        method.params = parse_params();
        consume(TokenKind::RParen, "Expected ')' after parameters");

        if (!check(TokenKind::LBrace)) {
            method.return_type = parse_type();
        }

        consume(TokenKind::LBrace, "Expected '{' before method body");
        method.body = parse_block();
        consume(TokenKind::RBrace, "Expected '}' after method body");

        decl.methods.push_back(std::move(method));
        skip_newlines();
    }

    consume(TokenKind::RBrace, "Expected '}' after impl block");
    return decl;
}

ErrorDecl Parser::parse_error_decl(const std::string& name) {
    ErrorDecl decl;
    decl.loc = peek().loc;
    decl.name = name;

    consume(TokenKind::LBrace, "Expected '{' after 'error'");
    decl.fields = parse_fields();
    consume(TokenKind::RBrace, "Expected '}' after error fields");

    return decl;
}

UseDecl Parser::parse_use_decl() {
    UseDecl decl;
    decl.loc = peek().loc;

    // uses: module1, module2
    // or uses:\n  - module1\n  - module2
    consume(TokenKind::Colon, "Expected ':' after 'uses'");

    if (match(TokenKind::Newline)) {
        // Multi-line form
        skip_newlines();
        while (match(TokenKind::Minus)) {
            Token mod = consume(TokenKind::Ident, "Expected module name");
            std::string module = mod.lexeme;
            while (match(TokenKind::Dot)) {
                Token part = consume(TokenKind::Ident, "Expected module part");
                module += "." + part.lexeme;
            }
            decl.modules.push_back(module);
            skip_newlines();
        }
    } else {
        // Single-line form
        do {
            Token mod = consume(TokenKind::Ident, "Expected module name");
            std::string module = mod.lexeme;
            while (match(TokenKind::Dot)) {
                Token part = consume(TokenKind::Ident, "Expected module part");
                module += "." + part.lexeme;
            }
            decl.modules.push_back(module);
        } while (match(TokenKind::Comma));
    }

    return decl;
}

// Type parsing
TypePtr Parser::parse_type() {
    auto type = parse_base_type();

    // Check for optional (?)
    while (match(TokenKind::Question)) {
        auto opt = std::make_unique<Type>();
        opt->loc = type->loc;
        opt->kind = Type::Optional{std::move(type)};
        type = std::move(opt);
    }

    // Check for result type (or Error)
    if (match(TokenKind::Or)) {
        Token err = consume(TokenKind::Ident, "Expected error type name");
        auto result = std::make_unique<Type>();
        result->loc = type->loc;
        result->kind = Type::Result{std::move(type), err.lexeme};
        type = std::move(result);
    }

    return type;
}

TypePtr Parser::parse_base_type() {
    auto type = std::make_unique<Type>();
    type->loc = peek().loc;

    // Array type: int[]
    // Named type: int, str, Person, Box<int>
    // Function type: fn(int, int) int
    // Channel type: chan int

    if (match(TokenKind::Fn)) {
        // Function type
        Type::Function fn;
        consume(TokenKind::LParen, "Expected '(' in function type");
        if (!check(TokenKind::RParen)) {
            do {
                fn.params.push_back(parse_type());
            } while (match(TokenKind::Comma));
        }
        consume(TokenKind::RParen, "Expected ')' in function type");
        if (!check(TokenKind::Newline) && !check(TokenKind::RParen) && !check(TokenKind::Comma) &&
            !check(TokenKind::LBrace) && !check(TokenKind::Question)) {
            fn.return_type = parse_type();
        }
        type->kind = std::move(fn);
        return type;
    }

    if (match(TokenKind::Chan)) {
        Type::Channel chan;
        chan.element = parse_type();
        type->kind = std::move(chan);
        return type;
    }

    // Named type (including primitives)
    std::string name;
    if (match(TokenKind::TInt)) name = "int";
    else if (match(TokenKind::TStr)) name = "str";
    else if (match(TokenKind::TBool)) name = "bool";
    else if (match(TokenKind::TChar)) name = "char";
    else if (match(TokenKind::TF32)) name = "f32";
    else if (match(TokenKind::TF64)) name = "f64";
    else if (match(TokenKind::TU32)) name = "u32";
    else if (match(TokenKind::TU64)) name = "u64";
    else if (match(TokenKind::Map)) name = "Map";
    else if (check(TokenKind::Ident)) {
        name = advance().lexeme;
    } else {
        error("Expected type");
        return type;
    }

    Type::Named named;
    named.name = name;

    // Generic arguments
    if (match(TokenKind::Lt)) {
        named.type_args = parse_type_args();
        consume(TokenKind::Gt, "Expected '>' after type arguments");
    }

    type->kind = std::move(named);

    // Check for array suffix
    if (match(TokenKind::LBracket)) {
        consume(TokenKind::RBracket, "Expected ']' for array type");
        auto arr = std::make_unique<Type>();
        arr->loc = type->loc;
        arr->kind = Type::Array{std::move(type)};
        return arr;
    }

    return type;
}

std::vector<std::string> Parser::parse_type_params() {
    std::vector<std::string> params;
    do {
        Token param = consume(TokenKind::Ident, "Expected type parameter");
        params.push_back(param.lexeme);
    } while (match(TokenKind::Comma));
    return params;
}

std::vector<TypePtr> Parser::parse_type_args() {
    std::vector<TypePtr> args;
    do {
        args.push_back(parse_type());
    } while (match(TokenKind::Comma));
    return args;
}

std::vector<Field> Parser::parse_fields() {
    std::vector<Field> fields;
    skip_newlines();

    while (!check(TokenKind::RBrace) && !at_end()) {
        Field f;
        Token name = consume(TokenKind::Ident, "Expected field name");
        f.name = name.lexeme;
        f.type = parse_type();
        f.optional = false;

        // Check if type ends with ?
        if (auto* named = std::get_if<Type::Named>(&f.type->kind)) {
            // handled by parse_type
        }
        if (std::holds_alternative<Type::Optional>(f.type->kind)) {
            f.optional = true;
        }

        fields.push_back(std::move(f));
        skip_newlines();

        if (match(TokenKind::Comma)) {
            skip_newlines();
        }
    }

    return fields;
}

std::vector<Param> Parser::parse_params() {
    std::vector<Param> params;

    if (check(TokenKind::RParen)) return params;

    do {
        Param p;
        // Check for 'self' parameter
        if (match(TokenKind::Self)) {
            p.name = "self";
            p.type = nullptr;
        } else {
            // type name format
            p.type = parse_type();
            Token name = consume(TokenKind::Ident, "Expected parameter name");
            p.name = name.lexeme;
        }
        params.push_back(std::move(p));
    } while (match(TokenKind::Comma));

    return params;
}

std::vector<StmtPtr> Parser::parse_block() {
    std::vector<StmtPtr> stmts;
    skip_newlines();

    while (!check(TokenKind::RBrace) && !at_end()) {
        stmts.push_back(parse_stmt());
        skip_newlines();
    }

    return stmts;
}

StmtPtr Parser::parse_stmt() {
    skip_newlines();
    auto stmt = std::make_unique<Stmt>();
    stmt->loc = peek().loc;

    if (match(TokenKind::Return)) {
        return parse_return_stmt();
    }

    if (match(TokenKind::If)) {
        return parse_if_stmt();
    }

    if (match(TokenKind::While)) {
        return parse_while_stmt();
    }

    if (match(TokenKind::For)) {
        return parse_for_stmt();
    }

    if (match(TokenKind::Break)) {
        stmt->kind = Stmt::Break{};
        return stmt;
    }

    if (match(TokenKind::Continue)) {
        stmt->kind = Stmt::Continue{};
        return stmt;
    }

    if (match(TokenKind::Go)) {
        return parse_go_stmt();
    }

    if (match(TokenKind::Const)) {
        return parse_var_decl();
    }

    // Variable declaration or expression statement
    // Check for: type name = expr  OR  name := expr  OR  expr

    // Type-annotated variable: int x = 5  or  Box<int> x = ...
    // Check for: primitive Ident, or UserType Ident, or UserType<...> Ident
    bool is_type_decl = false;
    if (check(TokenKind::TInt) || check(TokenKind::TStr) || check(TokenKind::TBool) ||
        check(TokenKind::TChar) || check(TokenKind::TF32) || check(TokenKind::TF64) ||
        check(TokenKind::TU32) || check(TokenKind::TU64) || check(TokenKind::Map) ||
        check(TokenKind::Chan) || check(TokenKind::Fn)) {
        is_type_decl = true;
    } else if (check(TokenKind::Ident)) {
        // Check if this looks like a type declaration
        // Pattern: TypeName varname = ...  or  TypeName<...> varname = ...
        if (peek_next().kind == TokenKind::Ident) {
            is_type_decl = true;  // TypeName varname
        } else if (peek_next().kind == TokenKind::Lt) {
            // Could be TypeName<...> varname - need to skip past <...> to check
            size_t saved = current_;
            advance(); // skip TypeName
            advance(); // skip <
            int depth = 1;
            while (!at_end() && depth > 0) {
                if (check(TokenKind::Lt)) depth++;
                else if (check(TokenKind::Gt)) depth--;
                advance();
            }
            // Now check if next is Ident (the variable name)
            if (check(TokenKind::Ident)) {
                is_type_decl = true;
            }
            current_ = saved; // restore
        }
    }

    if (is_type_decl) {
        // This looks like a type followed by a name
        auto type = parse_type();
        Token name = consume(TokenKind::Ident, "Expected variable name");
        consume(TokenKind::Assign, "Expected '=' in variable declaration");
        auto init = parse_expr();

        stmt->kind = Stmt::VarDecl{name.lexeme, std::move(type), std::move(init), false};
        return stmt;
    }

    // Could be: name := expr (inference) or expression
    auto expr = parse_expr();

    // Check for := (inference)
    if (match(TokenKind::ColonEq)) {
        if (auto* ident = std::get_if<Expr::Ident>(&expr->kind)) {
            auto init = parse_expr();
            stmt->kind = Stmt::VarDecl{ident->name, nullptr, std::move(init), false};
            return stmt;
        }
        error("Expected identifier before ':='");
    }

    // Check for assignment operators
    if (check(TokenKind::Assign) || check(TokenKind::PlusEq) || check(TokenKind::MinusEq) ||
        check(TokenKind::StarEq) || check(TokenKind::SlashEq)) {
        TokenKind op = advance().kind;
        auto value = parse_expr();
        stmt->kind = Stmt::Assign{std::move(expr), op, std::move(value)};
        return stmt;
    }

    // Check for channel send: ch.send(value)
    // This is handled as method call in expression

    // Expression statement
    stmt->kind = Stmt::ExprStmt{std::move(expr)};
    return stmt;
}

StmtPtr Parser::parse_var_decl() {
    auto stmt = std::make_unique<Stmt>();
    stmt->loc = peek().loc;

    // const name = expr (type always inferred for const)
    Token name = consume(TokenKind::Ident, "Expected constant name");
    consume(TokenKind::Assign, "Expected '=' in constant declaration");
    auto init = parse_expr();

    stmt->kind = Stmt::VarDecl{name.lexeme, nullptr, std::move(init), true};
    return stmt;
}

StmtPtr Parser::parse_return_stmt() {
    auto stmt = std::make_unique<Stmt>();
    stmt->loc = peek().loc;

    ExprPtr value = nullptr;
    if (!check(TokenKind::Newline) && !check(TokenKind::RBrace) && !at_end()) {
        value = parse_expr();
    }

    stmt->kind = Stmt::Return{std::move(value)};
    return stmt;
}

StmtPtr Parser::parse_if_stmt() {
    auto stmt = std::make_unique<Stmt>();
    stmt->loc = peek().loc;

    auto condition = parse_expr();
    consume(TokenKind::LBrace, "Expected '{' after if condition");
    auto then_block = parse_block();
    consume(TokenKind::RBrace, "Expected '}' after if body");

    std::vector<StmtPtr> else_block;
    skip_newlines();
    if (match(TokenKind::Else)) {
        if (match(TokenKind::If)) {
            else_block.push_back(parse_if_stmt());
        } else {
            consume(TokenKind::LBrace, "Expected '{' after else");
            else_block = parse_block();
            consume(TokenKind::RBrace, "Expected '}' after else body");
        }
    }

    stmt->kind = Stmt::If{std::move(condition), std::move(then_block), std::move(else_block)};
    return stmt;
}

StmtPtr Parser::parse_while_stmt() {
    auto stmt = std::make_unique<Stmt>();
    stmt->loc = peek().loc;

    auto condition = parse_expr();
    consume(TokenKind::LBrace, "Expected '{' after while condition");
    auto body = parse_block();
    consume(TokenKind::RBrace, "Expected '}' after while body");

    stmt->kind = Stmt::While{std::move(condition), std::move(body)};
    return stmt;
}

StmtPtr Parser::parse_for_stmt() {
    auto stmt = std::make_unique<Stmt>();
    stmt->loc = peek().loc;

    Token var1 = consume(TokenKind::Ident, "Expected loop variable");
    std::string var2;

    if (match(TokenKind::Comma)) {
        Token v2 = consume(TokenKind::Ident, "Expected second loop variable");
        var2 = v2.lexeme;
    }

    consume(TokenKind::In, "Expected 'in' in for loop");
    auto iterable = parse_expr();
    consume(TokenKind::LBrace, "Expected '{' after for");
    auto body = parse_block();
    consume(TokenKind::RBrace, "Expected '}' after for body");

    stmt->kind = Stmt::For{var1.lexeme, var2, std::move(iterable), std::move(body)};
    return stmt;
}

StmtPtr Parser::parse_go_stmt() {
    auto stmt = std::make_unique<Stmt>();
    stmt->loc = peek().loc;

    auto call = parse_expr();
    stmt->kind = Stmt::Go{std::move(call)};
    return stmt;
}

// Expression parsing with precedence
ExprPtr Parser::parse_expr() {
    return parse_or_expr();
}

ExprPtr Parser::parse_or_expr() {
    auto left = parse_and_expr();

    while (match(TokenKind::OrOr) || match(TokenKind::Or)) {
        TokenKind op = tokens_[current_ - 1].kind;

        if (op == TokenKind::Or) {
            // Handle 'or' for optionals/errors
            auto expr = std::make_unique<Expr>();
            expr->loc = left->loc;

            bool is_return = match(TokenKind::Return);
            bool is_error = false;
            std::string error_type;
            std::vector<std::pair<std::string, ExprPtr>> error_fields;
            ExprPtr right = nullptr;

            if (match(TokenKind::Error)) {
                is_error = true;
                Token err = consume(TokenKind::Ident, "Expected error type");
                error_type = err.lexeme;
                if (match(TokenKind::LBrace)) {
                    while (!check(TokenKind::RBrace) && !at_end()) {
                        Token fname = consume(TokenKind::Ident, "Expected field name");
                        consume(TokenKind::Colon, "Expected ':'");
                        auto fval = parse_expr();
                        error_fields.push_back({fname.lexeme, std::move(fval)});
                        if (!match(TokenKind::Comma)) break;
                    }
                    consume(TokenKind::RBrace, "Expected '}'");
                }
            } else if (!is_return && !check(TokenKind::Newline) && !check(TokenKind::RBrace)) {
                // Check for wait(ms)
                if (match(TokenKind::Wait)) {
                    consume(TokenKind::LParen, "Expected '(' after 'wait'");
                    right = parse_expr();
                    consume(TokenKind::RParen, "Expected ')' after wait timeout");
                } else {
                    right = parse_and_expr();
                }
            }

            expr->kind = Expr::Or{std::move(left), std::move(right), is_return, is_error, error_type, std::move(error_fields)};
            left = std::move(expr);
        } else {
            auto right = parse_and_expr();
            auto expr = std::make_unique<Expr>();
            expr->loc = left->loc;
            expr->kind = Expr::Binary{std::move(left), op, std::move(right)};
            left = std::move(expr);
        }
    }

    return left;
}

ExprPtr Parser::parse_and_expr() {
    auto left = parse_equality_expr();

    while (match(TokenKind::AndAnd) || match(TokenKind::And)) {
        TokenKind op = tokens_[current_ - 1].kind;
        auto right = parse_equality_expr();
        auto expr = std::make_unique<Expr>();
        expr->loc = left->loc;
        expr->kind = Expr::Binary{std::move(left), op, std::move(right)};
        left = std::move(expr);
    }

    return left;
}

ExprPtr Parser::parse_equality_expr() {
    auto left = parse_comparison_expr();

    while (match(TokenKind::Eq) || match(TokenKind::NotEq)) {
        TokenKind op = tokens_[current_ - 1].kind;
        auto right = parse_comparison_expr();
        auto expr = std::make_unique<Expr>();
        expr->loc = left->loc;
        expr->kind = Expr::Binary{std::move(left), op, std::move(right)};
        left = std::move(expr);
    }

    return left;
}

ExprPtr Parser::parse_comparison_expr() {
    auto left = parse_term_expr();

    while (match(TokenKind::Lt) || match(TokenKind::Gt) ||
           match(TokenKind::LtEq) || match(TokenKind::GtEq) ||
           match(TokenKind::In) || match(TokenKind::Is)) {
        TokenKind op = tokens_[current_ - 1].kind;

        if (op == TokenKind::Is) {
            Token variant = consume(TokenKind::Ident, "Expected variant name after 'is'");
            auto expr = std::make_unique<Expr>();
            expr->loc = left->loc;
            expr->kind = Expr::Is{std::move(left), variant.lexeme};
            left = std::move(expr);
        } else {
            auto right = parse_term_expr();
            auto expr = std::make_unique<Expr>();
            expr->loc = left->loc;
            expr->kind = Expr::Binary{std::move(left), op, std::move(right)};
            left = std::move(expr);
        }
    }

    return left;
}

ExprPtr Parser::parse_term_expr() {
    auto left = parse_factor_expr();

    while (match(TokenKind::Plus) || match(TokenKind::Minus)) {
        TokenKind op = tokens_[current_ - 1].kind;
        auto right = parse_factor_expr();
        auto expr = std::make_unique<Expr>();
        expr->loc = left->loc;
        expr->kind = Expr::Binary{std::move(left), op, std::move(right)};
        left = std::move(expr);
    }

    return left;
}

ExprPtr Parser::parse_factor_expr() {
    auto left = parse_unary_expr();

    while (match(TokenKind::Star) || match(TokenKind::Slash) || match(TokenKind::Percent)) {
        TokenKind op = tokens_[current_ - 1].kind;
        auto right = parse_unary_expr();
        auto expr = std::make_unique<Expr>();
        expr->loc = left->loc;
        expr->kind = Expr::Binary{std::move(left), op, std::move(right)};
        left = std::move(expr);
    }

    return left;
}

ExprPtr Parser::parse_unary_expr() {
    if (match(TokenKind::Bang) || match(TokenKind::Minus) || match(TokenKind::Not)) {
        TokenKind op = tokens_[current_ - 1].kind;
        auto operand = parse_unary_expr();
        auto expr = std::make_unique<Expr>();
        expr->loc = operand->loc;
        expr->kind = Expr::Unary{op, std::move(operand)};
        return expr;
    }

    return parse_postfix_expr();
}

ExprPtr Parser::parse_postfix_expr() {
    auto expr = parse_primary_expr();

    while (true) {
        if (match(TokenKind::LParen)) {
            // Function call
            std::vector<ExprPtr> args;
            if (!check(TokenKind::RParen)) {
                do {
                    args.push_back(parse_expr());
                } while (match(TokenKind::Comma));
            }
            consume(TokenKind::RParen, "Expected ')' after arguments");

            auto call = std::make_unique<Expr>();
            call->loc = expr->loc;
            call->kind = Expr::Call{std::move(expr), std::move(args), {}};
            expr = std::move(call);
        } else if (match(TokenKind::LBracket)) {
            // Index
            auto index = parse_expr();
            consume(TokenKind::RBracket, "Expected ']' after index");

            auto idx = std::make_unique<Expr>();
            idx->loc = expr->loc;
            idx->kind = Expr::Index{std::move(expr), std::move(index)};
            expr = std::move(idx);
        } else if (match(TokenKind::Dot)) {
            // Field access or method call
            Token name = consume(TokenKind::Ident, "Expected field or method name");

            auto field = std::make_unique<Expr>();
            field->loc = expr->loc;
            field->kind = Expr::Field{std::move(expr), name.lexeme};
            expr = std::move(field);
        } else if (match(TokenKind::ColonColon)) {
            // Type-qualified access: Option.Some or func::<int>()
            if (match(TokenKind::Lt)) {
                auto type_args = parse_type_args();
                consume(TokenKind::Gt, "Expected '>' after type arguments");
                consume(TokenKind::LParen, "Expected '(' after type arguments");

                std::vector<ExprPtr> args;
                if (!check(TokenKind::RParen)) {
                    do {
                        args.push_back(parse_expr());
                    } while (match(TokenKind::Comma));
                }
                consume(TokenKind::RParen, "Expected ')' after arguments");

                auto call = std::make_unique<Expr>();
                call->loc = expr->loc;
                call->kind = Expr::Call{std::move(expr), std::move(args), std::move(type_args)};
                expr = std::move(call);
            }
        } else if (match(TokenKind::As)) {
            // Type cast
            auto target = parse_type();
            auto cast = std::make_unique<Expr>();
            cast->loc = expr->loc;
            cast->kind = Expr::Cast{std::move(expr), std::move(target)};
            expr = std::move(cast);
        } else {
            break;
        }
    }

    return expr;
}

ExprPtr Parser::parse_primary_expr() {
    auto expr = std::make_unique<Expr>();
    expr->loc = peek().loc;

    // Literals
    if (match(TokenKind::Int)) {
        expr->kind = Expr::IntLit{std::get<int64_t>(tokens_[current_ - 1].value)};
        return expr;
    }

    if (match(TokenKind::Float)) {
        expr->kind = Expr::FloatLit{std::get<double>(tokens_[current_ - 1].value)};
        return expr;
    }

    if (match(TokenKind::String)) {
        expr->kind = Expr::StringLit{std::get<std::string>(tokens_[current_ - 1].value)};
        return expr;
    }

    if (match(TokenKind::FString)) {
        Token tok = tokens_[current_ - 1];
        std::string content = std::get<std::string>(tok.value);
        return parse_fstring(content, tok.loc);
    }

    if (match(TokenKind::Char)) {
        expr->kind = Expr::CharLit{std::get<char>(tokens_[current_ - 1].value)};
        return expr;
    }

    if (match(TokenKind::True)) {
        expr->kind = Expr::BoolLit{true};
        return expr;
    }

    if (match(TokenKind::False)) {
        expr->kind = Expr::BoolLit{false};
        return expr;
    }

    if (match(TokenKind::None)) {
        expr->kind = Expr::NoneLit{};
        return expr;
    }

    // Match expression
    if (match(TokenKind::Match)) {
        return parse_match_expr();
    }

    // Lambda/anonymous function
    if (check(TokenKind::Fn) && peek_next().kind == TokenKind::LParen) {
        advance(); // consume 'fn'
        return parse_lambda_expr();
    }

    // Channel creation: chan()
    if (match(TokenKind::Chan)) {
        consume(TokenKind::LParen, "Expected '(' after 'chan'");
        consume(TokenKind::RParen, "Expected ')' for channel creation");
        // Return as a call to chan()
        auto chan_ident = std::make_unique<Expr>();
        chan_ident->loc = expr->loc;
        chan_ident->kind = Expr::Ident{"chan"};
        expr->kind = Expr::Call{std::move(chan_ident), {}, {}};
        return expr;
    }

    // Array literal
    if (match(TokenKind::LBracket)) {
        std::vector<ExprPtr> elements;
        if (!check(TokenKind::RBracket)) {
            do {
                elements.push_back(parse_expr());
            } while (match(TokenKind::Comma));
        }
        consume(TokenKind::RBracket, "Expected ']' after array elements");
        expr->kind = Expr::ArrayLit{std::move(elements)};
        return expr;
    }

    // Parenthesized expression or tuple
    if (match(TokenKind::LParen)) {
        auto inner = parse_expr();
        consume(TokenKind::RParen, "Expected ')' after expression");
        return inner;
    }

    // Block expression
    if (match(TokenKind::LBrace)) {
        auto stmts = parse_block();
        consume(TokenKind::RBrace, "Expected '}' after block");
        expr->kind = Expr::Block{std::move(stmts), nullptr};
        return expr;
    }

    // Identifier, possibly followed by struct literal or variant construction
    if (match(TokenKind::Ident)) {
        std::string name = tokens_[current_ - 1].lexeme;

        // Only parse generic type args if followed by { or . (struct/variant literal)
        // Otherwise < is the comparison operator, not generic args
        std::vector<TypePtr> type_args;
        if (check(TokenKind::Lt)) {
            // Lookahead to see if this looks like generic args (Type<T> { or Type<T>.)
            // vs comparison (x < 5)
            size_t saved = current_;
            advance(); // consume <

            // Try to parse what follows - if it's a valid type context, keep it
            // Otherwise restore and let comparison handle <
            bool looks_like_generic = false;

            // Simple heuristic: if next token is a type name (uppercase ident or primitive)
            // followed by > and then { or ., it's probably generic
            if (check(TokenKind::TInt) || check(TokenKind::TStr) || check(TokenKind::TBool) ||
                check(TokenKind::TChar) || check(TokenKind::TF32) || check(TokenKind::TF64) ||
                check(TokenKind::TU32) || check(TokenKind::TU64) ||
                (check(TokenKind::Ident) && !tokens_[current_].lexeme.empty() &&
                 std::isupper(tokens_[current_].lexeme[0]))) {
                looks_like_generic = true;
            }

            current_ = saved; // restore

            if (looks_like_generic) {
                advance(); // re-consume <
                type_args = parse_type_args();
                consume(TokenKind::Gt, "Expected '>' after type arguments");
            }
        }

        // Check for struct literal: Name { field: value }
        // Only if followed by "ident :" pattern, otherwise it's a block
        if (check(TokenKind::LBrace)) {
            // Lookahead to distinguish struct literal from block
            size_t saved = current_;
            advance(); // consume {
            skip_newlines();
            bool is_struct_lit = check(TokenKind::RBrace) || // empty struct
                (check(TokenKind::Ident) && peek_next().kind == TokenKind::Colon);
            current_ = saved; // restore

            if (is_struct_lit) {
                advance(); // consume {
                std::vector<std::pair<std::string, ExprPtr>> fields;
                skip_newlines();
                if (!check(TokenKind::RBrace)) {
                    do {
                        skip_newlines();
                        Token fname = consume(TokenKind::Ident, "Expected field name");
                        consume(TokenKind::Colon, "Expected ':' after field name");
                        auto fval = parse_expr();
                        fields.push_back({fname.lexeme, std::move(fval)});
                        skip_newlines();
                    } while (match(TokenKind::Comma));
                    skip_newlines();
                }
                consume(TokenKind::RBrace, "Expected '}' after struct fields");
                expr->kind = Expr::StructLit{name, std::move(type_args), std::move(fields)};
                return expr;
            }
        }

        // Check for variant construction: Option.Some { value: 42 }
        // Only if followed by { - otherwise it's field access handled in postfix
        if (check(TokenKind::Dot) && peek_next().kind == TokenKind::Ident) {
            // Look ahead to see if this is variant construction (Type.Case {) or field access (obj.field)
            size_t saved = current_;
            advance(); // consume .
            Token maybe_case = advance(); // consume ident
            bool is_variant = check(TokenKind::LBrace);
            current_ = saved; // restore

            if (is_variant) {
                advance(); // consume .
                Token variant_name = advance();
                std::vector<std::pair<std::string, ExprPtr>> fields;

                consume(TokenKind::LBrace, "Expected '{' for variant construction");
                skip_newlines();
                if (!check(TokenKind::RBrace)) {
                    do {
                        skip_newlines();
                        Token fname = consume(TokenKind::Ident, "Expected field name");
                        consume(TokenKind::Colon, "Expected ':' after field name");
                        auto fval = parse_expr();
                        fields.push_back({fname.lexeme, std::move(fval)});
                        skip_newlines();
                    } while (match(TokenKind::Comma));
                    skip_newlines();
                }
                consume(TokenKind::RBrace, "Expected '}' after variant fields");

                expr->kind = Expr::StructLit{name + "." + variant_name.lexeme, std::move(type_args), std::move(fields)};
                return expr;
            }
        }

        // Plain identifier
        expr->kind = Expr::Ident{name};
        return expr;
    }

    // 'self' keyword used as expression in methods
    if (match(TokenKind::Self)) {
        expr->kind = Expr::Ident{"self"};
        return expr;
    }

    // Map literal: Map<K,V>{}
    if (match(TokenKind::Map)) {
        std::vector<TypePtr> type_args;
        if (match(TokenKind::Lt)) {
            type_args = parse_type_args();
            consume(TokenKind::Gt, "Expected '>' after type arguments");
        }
        consume(TokenKind::LBrace, "Expected '{' for map literal");
        consume(TokenKind::RBrace, "Expected '}' for map literal");
        expr->kind = Expr::StructLit{"Map", std::move(type_args), {}};
        return expr;
    }

    error("Expected expression");
    expr->kind = Expr::NoneLit{};
    return expr;
}

ExprPtr Parser::parse_match_expr() {
    auto expr = std::make_unique<Expr>();
    expr->loc = peek().loc;

    auto subject = parse_expr();

    std::string using_type;
    if (match(TokenKind::Using)) {
        Token type_name = consume(TokenKind::Ident, "Expected type name after 'using'");
        using_type = type_name.lexeme;
    }

    consume(TokenKind::LBrace, "Expected '{' after match subject");
    skip_newlines();

    std::vector<std::pair<PatternPtr, ExprPtr>> arms;
    while (!check(TokenKind::RBrace) && !at_end()) {
        auto pattern = parse_pattern();
        consume(TokenKind::Arrow, "Expected '->' after pattern");

        ExprPtr result;
        if (match(TokenKind::LBrace)) {
            auto stmts = parse_block();
            ExprPtr block_result = nullptr;
            // Last statement might be an expression
            if (!stmts.empty()) {
                if (auto* expr_stmt = std::get_if<Stmt::ExprStmt>(&stmts.back()->kind)) {
                    block_result = std::move(expr_stmt->expr);
                    stmts.pop_back();
                }
            }
            auto block = std::make_unique<Expr>();
            block->loc = pattern->loc;
            block->kind = Expr::Block{std::move(stmts), std::move(block_result)};
            result = std::move(block);
            consume(TokenKind::RBrace, "Expected '}' after match arm block");
        } else {
            result = parse_expr();
        }

        arms.push_back({std::move(pattern), std::move(result)});
        skip_newlines();

        if (match(TokenKind::Comma)) {
            skip_newlines();
        }
    }

    consume(TokenKind::RBrace, "Expected '}' after match arms");

    expr->kind = Expr::Match{std::move(subject), using_type, std::move(arms)};
    return expr;
}

ExprPtr Parser::parse_lambda_expr() {
    auto expr = std::make_unique<Expr>();
    expr->loc = peek().loc;

    consume(TokenKind::LParen, "Expected '(' after 'fn'");
    std::vector<std::pair<std::string, TypePtr>> params;

    if (!check(TokenKind::RParen)) {
        do {
            auto type = parse_type();
            Token name = consume(TokenKind::Ident, "Expected parameter name");
            params.push_back({name.lexeme, std::move(type)});
        } while (match(TokenKind::Comma));
    }
    consume(TokenKind::RParen, "Expected ')' after lambda parameters");

    TypePtr return_type = nullptr;
    if (!check(TokenKind::LBrace)) {
        return_type = parse_type();
    }

    consume(TokenKind::LBrace, "Expected '{' before lambda body");
    auto body = parse_block();
    consume(TokenKind::RBrace, "Expected '}' after lambda body");

    expr->kind = Expr::Lambda{std::move(params), std::move(return_type), std::move(body)};
    return expr;
}

PatternPtr Parser::parse_pattern() {
    auto pattern = std::make_unique<Pattern>();
    pattern->loc = peek().loc;

    // Wildcard
    if (check(TokenKind::Ident) && peek().lexeme == "_") {
        advance();
        pattern->kind = Pattern::Wildcard{};
        return pattern;
    }

    // Literal patterns
    if (match(TokenKind::Int) || match(TokenKind::Float) || match(TokenKind::String) ||
        match(TokenKind::Char) || match(TokenKind::True) || match(TokenKind::False)) {
        pattern->kind = Pattern::Literal{tokens_[current_ - 1]};
        return pattern;
    }

    // Tuple pattern: (a, b)
    if (match(TokenKind::LParen)) {
        std::vector<PatternPtr> elements;
        if (!check(TokenKind::RParen)) {
            do {
                elements.push_back(parse_pattern());
            } while (match(TokenKind::Comma));
        }
        consume(TokenKind::RParen, "Expected ')' after tuple pattern");
        pattern->kind = Pattern::Tuple{std::move(elements)};
        return pattern;
    }

    // Identifier or variant pattern
    if (match(TokenKind::Ident)) {
        std::string name = tokens_[current_ - 1].lexeme;

        // Check for variant pattern with fields: Some { value: v }
        if (match(TokenKind::LBrace)) {
            std::vector<std::pair<std::string, std::string>> bindings;
            skip_newlines();
            if (!check(TokenKind::RBrace)) {
                do {
                    skip_newlines();
                    Token fname = consume(TokenKind::Ident, "Expected field name");
                    consume(TokenKind::Colon, "Expected ':' after field name");
                    Token binding = consume(TokenKind::Ident, "Expected binding variable");
                    bindings.push_back({fname.lexeme, binding.lexeme});
                    skip_newlines();
                } while (match(TokenKind::Comma));
                skip_newlines();
            }
            consume(TokenKind::RBrace, "Expected '}' after variant pattern");
            pattern->kind = Pattern::Variant{"", name, std::move(bindings)};
            return pattern;
        }

        // Check for qualified variant: Type.Case or Type.Case { ... }
        if (match(TokenKind::Dot)) {
            Token case_name = consume(TokenKind::Ident, "Expected variant case name");
            std::vector<std::pair<std::string, std::string>> bindings;

            if (match(TokenKind::LBrace)) {
                skip_newlines();
                if (!check(TokenKind::RBrace)) {
                    do {
                        skip_newlines();
                        Token fname = consume(TokenKind::Ident, "Expected field name");
                        consume(TokenKind::Colon, "Expected ':' after field name");
                        Token binding = consume(TokenKind::Ident, "Expected binding variable");
                        bindings.push_back({fname.lexeme, binding.lexeme});
                        skip_newlines();
                    } while (match(TokenKind::Comma));
                    skip_newlines();
                }
                consume(TokenKind::RBrace, "Expected '}' after variant pattern");
            }

            pattern->kind = Pattern::Variant{name, case_name.lexeme, std::move(bindings)};
            return pattern;
        }

        // Plain identifier pattern (binding)
        pattern->kind = Pattern::Ident{name};
        return pattern;
    }

    error("Expected pattern");
    pattern->kind = Pattern::Wildcard{};
    return pattern;
}

ExprPtr Parser::parse_fstring(const std::string& content, const SourceLoc& loc) {
    auto expr = std::make_unique<Expr>();
    expr->loc = loc;

    Expr::StringLit str_lit;
    std::vector<StringPart> parts;
    std::vector<ExprPtr> exprs;

    std::string current_literal;
    size_t i = 0;

    while (i < content.size()) {
        if (content[i] == '\\' && i + 1 < content.size()) {
            // Escaped character - already processed by lexer
            // But check for escaped braces
            if (content[i + 1] == '{' || content[i + 1] == '}') {
                current_literal += content[i + 1];
                i += 2;
            } else {
                // Other escapes already converted by lexer
                current_literal += content[i];
                i++;
            }
        } else if (content[i] == '{') {
            // Start of interpolation - find matching }
            // First, save current literal if any
            if (!current_literal.empty()) {
                parts.push_back({false, current_literal});
                current_literal.clear();
            }

            // Find the matching closing brace
            int brace_depth = 1;
            size_t start = i + 1;
            i++;
            while (i < content.size() && brace_depth > 0) {
                if (content[i] == '{') brace_depth++;
                else if (content[i] == '}') brace_depth--;
                i++;
            }

            // Extract expression string
            std::string expr_str = content.substr(start, i - start - 1);

            // Parse the expression using a sub-lexer/parser
            Lexer sub_lexer(expr_str, loc.file);
            Parser sub_parser(sub_lexer);
            ExprPtr sub_expr = sub_parser.parse_expr();

            if (sub_parser.has_errors()) {
                for (const auto& err : sub_parser.errors()) {
                    errors_.push_back(err);
                }
            }

            parts.push_back({true, ""});
            exprs.push_back(std::move(sub_expr));
        } else {
            current_literal += content[i];
            i++;
        }
    }

    // Add any remaining literal
    if (!current_literal.empty()) {
        parts.push_back({false, current_literal});
    }

    str_lit.parts = std::move(parts);
    str_lit.exprs = std::move(exprs);
    expr->kind = std::move(str_lit);
    return expr;
}

} // namespace shotgun
