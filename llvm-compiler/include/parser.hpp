#pragma once

#include "lexer.hpp"
#include "ast.hpp"
#include <vector>
#include <string>

namespace shotgun {

class Parser {
public:
    Parser(Lexer& lexer);

    Program parse();

    bool has_errors() const { return !errors_.empty(); }
    const std::vector<std::string>& errors() const { return errors_; }

private:
    // Token handling
    Token peek() const;
    Token peek_next() const;
    Token advance();
    bool at_end() const;
    bool check(TokenKind kind) const;
    bool match(TokenKind kind);
    Token consume(TokenKind kind, const std::string& msg);
    void skip_newlines();
    void expect_newline_or_eof();

    // Error handling
    void error(const std::string& msg);
    void error_at(const SourceLoc& loc, const std::string& msg);
    void synchronize();

    // Declarations
    Decl parse_decl();
    FnDecl parse_fn_decl();
    StructDecl parse_struct_decl(const std::string& name, std::vector<std::string> type_params);
    VariantDecl parse_variant_decl(const std::string& name, std::vector<std::string> type_params);
    TraitDecl parse_trait_decl(const std::string& name);
    ImplDecl parse_impl_decl(const std::string& type_name, std::vector<std::string> type_params);
    ErrorDecl parse_error_decl(const std::string& name);
    UseDecl parse_use_decl();
    ImplDecl parse_method(const std::string& type_name, std::vector<std::string> type_params);

    // Type parsing
    TypePtr parse_type();
    TypePtr parse_base_type();
    std::vector<std::string> parse_type_params();
    std::vector<TypePtr> parse_type_args();

    // Statement parsing
    StmtPtr parse_stmt();
    StmtPtr parse_var_decl();
    StmtPtr parse_if_stmt();
    StmtPtr parse_while_stmt();
    StmtPtr parse_for_stmt();
    StmtPtr parse_return_stmt();
    StmtPtr parse_go_stmt();
    std::vector<StmtPtr> parse_block();

    // Expression parsing (precedence climbing)
    ExprPtr parse_expr();
    ExprPtr parse_or_expr();
    ExprPtr parse_and_expr();
    ExprPtr parse_equality_expr();
    ExprPtr parse_comparison_expr();
    ExprPtr parse_term_expr();
    ExprPtr parse_factor_expr();
    ExprPtr parse_unary_expr();
    ExprPtr parse_postfix_expr();
    ExprPtr parse_primary_expr();
    ExprPtr parse_match_expr();
    ExprPtr parse_lambda_expr();

    // Pattern parsing
    PatternPtr parse_pattern();

    // F-string parsing
    ExprPtr parse_fstring(const std::string& content, const SourceLoc& loc);

    // Helpers
    std::vector<Field> parse_fields();
    std::vector<Param> parse_params();

    Lexer& lexer_;
    std::vector<Token> tokens_;
    size_t current_ = 0;
    std::vector<std::string> errors_;
};

} // namespace shotgun
