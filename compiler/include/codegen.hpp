#pragma once

#include "ast.hpp"
#include "types.hpp"
#include "symbol_table.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Verifier.h>

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace shotgun {

class CodeGen {
public:
    CodeGen(const std::string& module_name);

    // Main entry point - generate code for a program
    bool generate(Program& prog, SymbolTable& symbols);

    // Get the generated module
    llvm::Module* module() { return module_.get(); }

    // Print IR to string (for debugging/testing)
    std::string get_ir() const;

    // Error access
    bool has_errors() const { return !errors_.empty(); }
    const std::vector<std::string>& errors() const { return errors_; }

private:
    // LLVM context and module
    std::unique_ptr<llvm::LLVMContext> context_;
    std::unique_ptr<llvm::Module> module_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;

    // Symbol table reference
    SymbolTable* symbols_ = nullptr;

    // Type mapping
    llvm::Type* get_llvm_type(ResolvedTypePtr type);
    llvm::Type* get_llvm_type(const TypePtr& ast_type);

    // Struct type cache (for recursive types)
    std::unordered_map<std::string, llvm::StructType*> struct_types_;

    // Named values (variables) in current scope
    std::unordered_map<std::string, llvm::AllocaInst*> named_values_;

    // Current function being compiled
    llvm::Function* current_function_ = nullptr;

    // Break/continue targets
    llvm::BasicBlock* break_target_ = nullptr;
    llvm::BasicBlock* continue_target_ = nullptr;

    // Declaration generation
    void gen_decl(const Decl& decl);
    void gen_fn_decl(const FnDecl& fn);
    void gen_struct_decl(const StructDecl& s);
    void gen_variant_decl(const VariantDecl& v);
    void gen_impl_decl(const ImplDecl& impl);

    // Statement generation
    void gen_stmt(const StmtPtr& stmt);
    void gen_var_decl(const Stmt::VarDecl& var);
    void gen_assign(const Stmt::Assign& assign);
    void gen_if(const Stmt::If& if_stmt);
    void gen_while(const Stmt::While& while_stmt);
    void gen_for(const Stmt::For& for_stmt);
    void gen_return(const Stmt::Return& ret);
    void gen_expr_stmt(const Stmt::ExprStmt& expr);
    void gen_go(const Stmt::Go& go);
    void gen_block(const std::vector<StmtPtr>& block);

    // Expression generation
    llvm::Value* gen_expr(const ExprPtr& expr);
    llvm::Value* gen_int_lit(const Expr::IntLit& lit);
    llvm::Value* gen_float_lit(const Expr::FloatLit& lit);
    llvm::Value* gen_string_lit(const Expr::StringLit& lit);
    llvm::Value* gen_char_lit(const Expr::CharLit& lit);
    llvm::Value* gen_bool_lit(const Expr::BoolLit& lit);
    llvm::Value* gen_none_lit(const Expr::NoneLit& lit);
    llvm::Value* gen_ident(const Expr::Ident& ident);
    llvm::Value* gen_binary(const Expr::Binary& bin);
    llvm::Value* gen_unary(const Expr::Unary& un);
    llvm::Value* gen_call(const Expr::Call& call);
    llvm::Value* gen_index(const Expr::Index& idx);
    llvm::Value* gen_field(const Expr::Field& field);
    llvm::Value* gen_struct_lit(const Expr::StructLit& lit);
    llvm::Value* gen_array_lit(const Expr::ArrayLit& lit);
    llvm::Value* gen_match(const Expr::Match& match);
    llvm::Value* gen_lambda(const Expr::Lambda& lambda);
    llvm::Value* gen_or_expr(const Expr::Or& or_expr);
    llvm::Value* gen_is_expr(const Expr::Is& is_expr);
    llvm::Value* gen_cast(const Expr::Cast& cast);
    llvm::Value* gen_block_expr(const Expr::Block& block);

    // Assertion generation
    llvm::Value* gen_assert(const Expr::Call& call);
    llvm::Value* gen_assert_eq(const Expr::Call& call);
    llvm::Value* gen_assert_ne(const Expr::Call& call);
    llvm::Value* gen_assert_true(const Expr::Call& call);
    llvm::Value* gen_assert_false(const Expr::Call& call);

    // Helper: create alloca in entry block
    llvm::AllocaInst* create_entry_alloca(llvm::Function* fn,
                                          const std::string& name,
                                          llvm::Type* type);

    // Helper: get or create struct type
    llvm::StructType* get_or_create_struct_type(const std::string& name);

    // Generate runtime functions (print, etc.)
    void gen_runtime_decls();

    // Error reporting
    void error(const std::string& msg);
    void error_at(const SourceLoc& loc, const std::string& msg);

    std::vector<std::string> errors_;
};

} // namespace shotgun
