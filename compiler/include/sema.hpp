/**
 * @file sema.hpp
 * @brief Semantic analyzer for the Shotgun language.
 *
 * The Sema class performs type checking and semantic analysis on the AST.
 * It runs in two passes:
 * 1. Register all type and function declarations
 * 2. Type check all code
 */

#pragma once

#include "ast.hpp"
#include "types.hpp"
#include "symbol_table.hpp"
#include <vector>
#include <string>

namespace shotgun {

/**
 * @brief Semantic analyzer that type checks Shotgun programs.
 *
 * Performs:
 * - Type inference and checking
 * - Variable scope validation
 * - Function signature verification
 * - Pattern exhaustiveness (basic)
 * - Type compatibility checking
 *
 * Usage:
 * @code
 * Sema sema;
 * if (!sema.analyze(program)) {
 *     for (const auto& err : sema.errors()) {
 *         std::cerr << err << "\n";
 *     }
 * }
 * // After analysis, symbol table is available for codegen
 * CodeGen codegen;
 * codegen.generate(program, sema.symbols());
 * @endcode
 */
class Sema {
public:
    /// @brief Constructs a semantic analyzer with empty symbol table.
    Sema();

    /**
     * @brief Analyzes a program for type correctness.
     * @param prog The program AST to analyze.
     * @return True if analysis succeeded with no errors.
     */
    bool analyze(Program& prog);

    /// @brief Returns true if analysis encountered errors.
    bool has_errors() const { return !errors_.empty(); }

    /// @brief Returns the list of error messages.
    const std::vector<std::string>& errors() const { return errors_; }

    /// @brief Returns the symbol table (for use by codegen).
    SymbolTable& symbols() { return symbols_; }

private:
    /// @name Pass 1: Declaration Registration
    /// Registers all types and functions before type checking.
    /// @{
    void register_decls(Program& prog);
    void register_decl(const Decl& decl);
    /// @}

    /// @name Pass 2: Type Checking
    /// Type checks all declarations and their bodies.
    /// @{
    void check_decls(Program& prog);
    void check_decl(const Decl& decl);
    /// @}

    /// @name Declaration Checking
    /// @{
    void check_fn_decl(const FnDecl& fn);
    void check_struct_decl(const StructDecl& s);
    void check_variant_decl(const VariantDecl& v);
    void check_impl_decl(const ImplDecl& impl);
    void check_trait_decl(const TraitDecl& t);
    void check_error_decl(const ErrorDecl& e);
    /// @}

    /// @name Statement Checking
    /// @{
    void check_stmt(const StmtPtr& stmt);
    void check_var_decl(const Stmt::VarDecl& var);
    void check_assign(const Stmt::Assign& assign);
    void check_if(const Stmt::If& if_stmt);
    void check_while(const Stmt::While& while_stmt);
    void check_for(const Stmt::For& for_stmt);
    void check_return(const Stmt::Return& ret);
    void check_go(const Stmt::Go& go);
    void check_block(const std::vector<StmtPtr>& block);
    /// @}

    /// @name Expression Checking
    /// Each method returns the inferred type of the expression.
    /// @{
    ResolvedTypePtr check_expr(const ExprPtr& expr);
    ResolvedTypePtr check_int_lit(const Expr::IntLit& lit);
    ResolvedTypePtr check_float_lit(const Expr::FloatLit& lit);
    ResolvedTypePtr check_string_lit(const Expr::StringLit& lit);
    ResolvedTypePtr check_char_lit(const Expr::CharLit& lit);
    ResolvedTypePtr check_bool_lit(const Expr::BoolLit& lit);
    ResolvedTypePtr check_none_lit(const Expr::NoneLit& lit);
    ResolvedTypePtr check_ident(const Expr::Ident& ident);
    ResolvedTypePtr check_binary(const Expr::Binary& bin);
    ResolvedTypePtr check_unary(const Expr::Unary& un);
    ResolvedTypePtr check_call(const Expr::Call& call);
    ResolvedTypePtr check_index(const Expr::Index& idx);
    ResolvedTypePtr check_field(const Expr::Field& field);
    ResolvedTypePtr check_struct_lit(const Expr::StructLit& lit);
    ResolvedTypePtr check_array_lit(const Expr::ArrayLit& lit);
    ResolvedTypePtr check_match(const Expr::Match& match);
    ResolvedTypePtr check_lambda(const Expr::Lambda& lambda);
    ResolvedTypePtr check_or_expr(const Expr::Or& or_expr);
    ResolvedTypePtr check_is_expr(const Expr::Is& is_expr);
    ResolvedTypePtr check_cast(const Expr::Cast& cast);
    ResolvedTypePtr check_block(const Expr::Block& block);
    ResolvedTypePtr check_chan_recv(const Expr::ChanRecv& recv);
    /// @}

    /// @name Type Resolution
    /// @{
    /**
     * @brief Converts an AST type to a resolved type.
     * @param type The AST type node.
     * @return The corresponding resolved type.
     */
    ResolvedTypePtr resolve_type(const TypePtr& type);
    /// @}

    /// @name Pattern Checking
    /// @{
    /**
     * @brief Type checks a pattern against an expected type.
     * @param pattern The pattern to check.
     * @param expected The type being matched against.
     * @return The type bound by the pattern.
     */
    ResolvedTypePtr check_pattern(const PatternPtr& pattern, ResolvedTypePtr expected);
    /// @}

    /// @name Type Compatibility
    /// @{
    /**
     * @brief Checks if source type can be assigned to target type.
     * @param target The type being assigned to.
     * @param source The type being assigned from.
     * @return True if assignment is valid.
     */
    bool is_assignable(ResolvedTypePtr target, ResolvedTypePtr source);

    /**
     * @brief Checks if two types are structurally equal.
     * @param a First type.
     * @param b Second type.
     * @return True if types are equal.
     */
    bool types_equal(ResolvedTypePtr a, ResolvedTypePtr b);
    /// @}

    /// @name Error Reporting
    /// @{
    void error(const std::string& msg);
    void error_at(const SourceLoc& loc, const std::string& msg);
    /// @}

    /// @name Context Tracking
    /// @{
    ResolvedTypePtr current_return_type_;  ///< Expected return type of current function
    std::string current_function_;         ///< Name of current function being checked
    bool in_loop_ = false;                 ///< True if inside a loop (for break/continue)
    /// @}

    SymbolTable symbols_;              ///< Symbol table
    std::vector<std::string> errors_;  ///< Collected error messages
};

} // namespace shotgun
