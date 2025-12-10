/**
 * @file parser.hpp
 * @brief Recursive descent parser for the Shotgun language.
 *
 * The Parser consumes tokens from the Lexer and builds an Abstract Syntax Tree.
 * It uses recursive descent with precedence climbing for expressions.
 */

#pragma once

#include "lexer.hpp"
#include "ast.hpp"
#include <vector>
#include <string>

namespace shotgun {

/**
 * @brief Recursive descent parser for Shotgun source code.
 *
 * Consumes tokens and produces an AST. The parser handles:
 * - Top-level declarations (functions, structs, variants, traits, impls, errors)
 * - Statements (variables, control flow, assignments)
 * - Expressions (with operator precedence via precedence climbing)
 * - Patterns (for match expressions)
 *
 * Usage:
 * @code
 * Lexer lexer(source, "file.bs");
 * Parser parser(lexer);
 * Program ast = parser.parse();
 * if (parser.has_errors()) { ... }
 * @endcode
 */
class Parser {
public:
    /**
     * @brief Constructs a parser from a lexer.
     * @param lexer The lexer to read tokens from.
     */
    Parser(Lexer& lexer);

    /**
     * @brief Parses the entire program and returns the AST.
     * @return The parsed Program AST.
     */
    Program parse();

    /// @brief Returns true if parsing encountered errors.
    bool has_errors() const { return !errors_.empty(); }

    /// @brief Returns the list of error messages.
    const std::vector<std::string>& errors() const { return errors_; }

private:
    /// @name Token Handling
    /// @{
    Token peek() const;       ///< Returns current token without consuming.
    Token peek_next() const;  ///< Returns next token without consuming.
    Token advance();          ///< Consumes and returns current token.
    bool at_end() const;      ///< Returns true if at end of tokens.
    bool check(TokenKind kind) const;  ///< Returns true if current token is of given kind.
    bool match(TokenKind kind);        ///< Consumes token if it matches kind.
    Token consume(TokenKind kind, const std::string& msg);  ///< Consumes expected token or reports error.
    void skip_newlines();              ///< Skips newline tokens.
    void expect_newline_or_eof();      ///< Expects newline or end of file.
    /// @}

    /// @name Error Handling
    /// @{
    void error(const std::string& msg);  ///< Reports error at current position.
    void error_at(const SourceLoc& loc, const std::string& msg);  ///< Reports error at location.
    void synchronize();  ///< Recovers from error by skipping to next statement.
    /// @}

    /// @name Declaration Parsing
    /// @{
    Decl parse_decl();
    FnDecl parse_fn_decl();
    StructDecl parse_struct_decl(const std::string& name, std::vector<std::string> type_params);
    VariantDecl parse_variant_decl(const std::string& name, std::vector<std::string> type_params);
    TraitDecl parse_trait_decl(const std::string& name);
    ImplDecl parse_impl_decl(const std::string& type_name, std::vector<std::string> type_params);
    ErrorDecl parse_error_decl(const std::string& name);
    UseDecl parse_use_decl();
    ImplDecl parse_method(const std::string& type_name, std::vector<std::string> type_params);
    /// @}

    /// @name Type Parsing
    /// @{
    TypePtr parse_type();       ///< Parses a type annotation.
    TypePtr parse_base_type();  ///< Parses base type (without optional/result).
    std::vector<std::string> parse_type_params();  ///< Parses generic type parameters.
    std::vector<TypePtr> parse_type_args();        ///< Parses generic type arguments.
    /// @}

    /// @name Statement Parsing
    /// @{
    StmtPtr parse_stmt();
    StmtPtr parse_var_decl();
    StmtPtr parse_if_stmt();
    StmtPtr parse_while_stmt();
    StmtPtr parse_for_stmt();
    StmtPtr parse_return_stmt();
    StmtPtr parse_go_stmt();
    std::vector<StmtPtr> parse_block();  ///< Parses a block of statements.
    /// @}

    /// @name Expression Parsing (Precedence Climbing)
    /// @{
    ExprPtr parse_expr();             ///< Entry point for expression parsing.
    ExprPtr parse_or_expr();          ///< Parses || and 'or' expressions.
    ExprPtr parse_and_expr();         ///< Parses && and 'and' expressions.
    ExprPtr parse_equality_expr();    ///< Parses == and != expressions.
    ExprPtr parse_comparison_expr();  ///< Parses <, >, <=, >=, 'in', 'is' expressions.
    ExprPtr parse_term_expr();        ///< Parses + and - expressions.
    ExprPtr parse_factor_expr();      ///< Parses *, /, % expressions.
    ExprPtr parse_unary_expr();       ///< Parses !, -, 'not' expressions.
    ExprPtr parse_postfix_expr();     ///< Parses calls, indexing, field access.
    ExprPtr parse_primary_expr();     ///< Parses literals, identifiers, parenthesized.
    ExprPtr parse_match_expr();       ///< Parses match expression.
    ExprPtr parse_lambda_expr();      ///< Parses anonymous function.
    /// @}

    /// @name Pattern Parsing
    /// @{
    PatternPtr parse_pattern();  ///< Parses a match pattern.
    /// @}

    /// @name F-String Parsing
    /// @{
    ExprPtr parse_fstring(const std::string& content, const SourceLoc& loc);
    /// @}

    /// @name Helper Methods
    /// @{
    std::vector<Field> parse_fields();  ///< Parses struct/variant fields.
    std::vector<Param> parse_params();  ///< Parses function parameters.
    /// @}

    Lexer& lexer_;                     ///< Source lexer
    std::vector<Token> tokens_;        ///< All tokens from lexer
    size_t current_ = 0;               ///< Current token index
    std::vector<std::string> errors_;  ///< Collected error messages
};

} // namespace shotgun
