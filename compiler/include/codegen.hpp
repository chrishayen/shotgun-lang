/**
 * @file codegen.hpp
 * @brief LLVM IR code generator for the Shotgun language.
 *
 * The CodeGen class transforms a type-checked AST into LLVM IR,
 * which can then be compiled to native machine code or run via JIT.
 */

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

/**
 * @brief LLVM IR code generator for Shotgun programs.
 *
 * Transforms a type-checked AST into LLVM IR. The generated module
 * can be compiled to object code or executed via JIT.
 *
 * Usage:
 * @code
 * CodeGen codegen("mymodule");
 * if (!codegen.generate(program, sema.symbols())) {
 *     for (const auto& err : codegen.errors()) {
 *         std::cerr << err << "\n";
 *     }
 * }
 * // Get the LLVM module for further processing
 * llvm::Module* mod = codegen.module();
 * @endcode
 */
class CodeGen {
public:
    /**
     * @brief Constructs a code generator with the given module name.
     * @param module_name Name for the generated LLVM module.
     */
    CodeGen(const std::string& module_name);

    /**
     * @brief Generates LLVM IR for a program.
     * @param prog The type-checked program AST.
     * @param symbols The symbol table from semantic analysis.
     * @return True if code generation succeeded.
     */
    bool generate(Program& prog, SymbolTable& symbols);

    /// @brief Returns the generated LLVM module.
    llvm::Module* module() { return module_.get(); }

    /**
     * @brief Returns the generated IR as a string.
     * @return LLVM IR in textual form (for debugging/testing).
     */
    std::string get_ir() const;

    /// @brief Returns true if code generation encountered errors.
    bool has_errors() const { return !errors_.empty(); }

    /// @brief Returns the list of error messages.
    const std::vector<std::string>& errors() const { return errors_; }

private:
    /// @name LLVM Infrastructure
    /// @{
    std::unique_ptr<llvm::LLVMContext> context_;  ///< LLVM context (owns types)
    std::unique_ptr<llvm::Module> module_;        ///< Generated LLVM module
    std::unique_ptr<llvm::IRBuilder<>> builder_;  ///< IR builder for instructions
    /// @}

    SymbolTable* symbols_ = nullptr;  ///< Symbol table from sema

    /// @name Type Mapping
    /// @{
    llvm::Type* get_llvm_type(ResolvedTypePtr type);  ///< Maps resolved type to LLVM type
    llvm::Type* get_llvm_type(const TypePtr& ast_type);  ///< Maps AST type to LLVM type
    ResolvedTypePtr ast_type_to_resolved(const TypePtr& ast_type);  ///< Converts AST type to resolved
    /// @}

    /// @name Caches and State
    /// @{
    std::unordered_map<std::string, llvm::StructType*> struct_types_;  ///< Struct type cache
    std::unordered_map<std::string, llvm::AllocaInst*> named_values_;  ///< Variable allocas
    std::unordered_map<std::string, ResolvedTypePtr> var_types_;       ///< Variable types
    llvm::Function* current_function_ = nullptr;   ///< Current function being compiled
    llvm::BasicBlock* break_target_ = nullptr;     ///< Break target for loops
    llvm::BasicBlock* continue_target_ = nullptr;  ///< Continue target for loops
    /// @}

    /// @name Declaration Generation
    /// @{
    void gen_decl(const Decl& decl);
    void gen_fn_decl(const FnDecl& fn);
    void gen_struct_decl(const StructDecl& s);
    void gen_variant_decl(const VariantDecl& v);
    void gen_impl_decl(const ImplDecl& impl);
    /// @}

    /// @name Statement Generation
    /// @{
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
    /// @}

    /// @name Expression Generation
    /// Each method returns the LLVM Value representing the expression result.
    /// @{
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
    /// @}

    /// @name Assertion Generation
    /// Generates code for test assertion functions.
    /// @{
    llvm::Value* gen_assert(const Expr::Call& call);
    llvm::Value* gen_assert_eq(const Expr::Call& call);
    llvm::Value* gen_assert_ne(const Expr::Call& call);
    llvm::Value* gen_assert_true(const Expr::Call& call);
    llvm::Value* gen_assert_false(const Expr::Call& call);
    /// @}

    /// @name Helper Methods
    /// @{
    /**
     * @brief Creates an alloca instruction in the function's entry block.
     * @param fn The function to add the alloca to.
     * @param name Variable name for the alloca.
     * @param type LLVM type to allocate.
     * @return The created alloca instruction.
     */
    llvm::AllocaInst* create_entry_alloca(llvm::Function* fn,
                                          const std::string& name,
                                          llvm::Type* type);

    /**
     * @brief Gets or creates an LLVM struct type for a named struct.
     * @param name The struct name.
     * @return The LLVM struct type.
     */
    llvm::StructType* get_or_create_struct_type(const std::string& name);

    /// @brief Generates declarations for runtime functions (print, etc.).
    void gen_runtime_decls();
    /// @}

    /// @name Error Reporting
    /// @{
    void error(const std::string& msg);
    void error_at(const SourceLoc& loc, const std::string& msg);
    /// @}

    std::vector<std::string> errors_;  ///< Collected error messages
};

} // namespace shotgun
