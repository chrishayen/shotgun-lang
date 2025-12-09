#pragma once

#include "ast.hpp"
#include "types.hpp"
#include "symbol_table.hpp"
#include <vector>
#include <string>

namespace shotgun {

class Sema {
public:
    Sema();

    // Main entry point - analyze a program
    bool analyze(Program& prog);

    // Error access
    bool has_errors() const { return !errors_.empty(); }
    const std::vector<std::string>& errors() const { return errors_; }

    // Symbol table access (for codegen)
    SymbolTable& symbols() { return symbols_; }

private:
    // Pass 1: Register all type and function declarations
    void register_decls(Program& prog);
    void register_decl(const Decl& decl);

    // Pass 2: Type check everything
    void check_decls(Program& prog);
    void check_decl(const Decl& decl);

    // Declaration checking
    void check_fn_decl(const FnDecl& fn);
    void check_struct_decl(const StructDecl& s);
    void check_variant_decl(const VariantDecl& v);
    void check_impl_decl(const ImplDecl& impl);
    void check_trait_decl(const TraitDecl& t);
    void check_error_decl(const ErrorDecl& e);

    // Statement checking
    void check_stmt(const StmtPtr& stmt);
    void check_var_decl(const Stmt::VarDecl& var);
    void check_assign(const Stmt::Assign& assign);
    void check_if(const Stmt::If& if_stmt);
    void check_while(const Stmt::While& while_stmt);
    void check_for(const Stmt::For& for_stmt);
    void check_return(const Stmt::Return& ret);
    void check_go(const Stmt::Go& go);
    void check_block(const std::vector<StmtPtr>& block);

    // Expression checking - returns the type of the expression
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

    // Type resolution - convert AST type to resolved type
    ResolvedTypePtr resolve_type(const TypePtr& type);

    // Pattern checking
    ResolvedTypePtr check_pattern(const PatternPtr& pattern, ResolvedTypePtr expected);

    // Type compatibility
    bool is_assignable(ResolvedTypePtr target, ResolvedTypePtr source);
    bool types_equal(ResolvedTypePtr a, ResolvedTypePtr b);

    // Error reporting
    void error(const std::string& msg);
    void error_at(const SourceLoc& loc, const std::string& msg);

    // Current context
    ResolvedTypePtr current_return_type_;
    std::string current_function_;
    bool in_loop_ = false;

    SymbolTable symbols_;
    std::vector<std::string> errors_;
};

} // namespace shotgun
