/**
 * @file symbol_table.hpp
 * @brief Symbol table for semantic analysis.
 *
 * The SymbolTable tracks all declared symbols including variables,
 * functions, types, methods, and trait implementations. It supports
 * nested scopes for proper variable shadowing.
 */

#pragma once

#include "types.hpp"
#include <string>
#include <vector>
#include <unordered_map>
#include <optional>

namespace shotgun {

/// @brief Variable entry in the symbol table
struct VarEntry {
    std::string name;         ///< Variable name
    ResolvedTypePtr type;     ///< Variable type
    bool is_const;            ///< True if declared with const
    bool is_initialized;      ///< True if initialized
};

/// @brief A single lexical scope
struct Scope {
    std::unordered_map<std::string, VarEntry> variables;  ///< Variables in this scope
    Scope* parent = nullptr;  ///< Parent scope (nullptr for global)
};

/**
 * @brief Symbol table for tracking declarations during semantic analysis.
 *
 * Manages:
 * - Variable scopes (with proper nesting and shadowing)
 * - Function declarations
 * - Type declarations (struct, variant, error, trait)
 * - Method declarations
 * - Trait implementations
 * - Built-in method signatures
 *
 * Usage:
 * @code
 * SymbolTable symbols;
 * symbols.push_scope();
 * symbols.declare_var("x", ResolvedType::make_int());
 * auto entry = symbols.lookup_var("x");
 * symbols.pop_scope();
 * @endcode
 */
class SymbolTable {
public:
    /// @brief Constructs a symbol table with a global scope.
    SymbolTable();

    /// @name Scope Management
    /// @{
    void push_scope();  ///< Enters a new nested scope.
    void pop_scope();   ///< Exits the current scope.
    /// @}

    /// @name Variable Operations
    /// @{
    /**
     * @brief Declares a variable in the current scope.
     * @param name Variable name.
     * @param type Variable type.
     * @param is_const True if constant.
     * @return False if already declared in current scope.
     */
    bool declare_var(const std::string& name, ResolvedTypePtr type, bool is_const = false);

    /**
     * @brief Looks up a variable by name.
     * @param name Variable name to find.
     * @return The VarEntry if found, nullopt otherwise.
     */
    std::optional<VarEntry> lookup_var(const std::string& name) const;

    /**
     * @brief Checks if name is declared in current scope (not parent scopes).
     * @param name Variable name to check.
     * @return True if declared in current scope.
     */
    bool is_declared_in_current_scope(const std::string& name) const;
    /// @}

    /// @name Function Operations
    /// @{
    void declare_function(const std::string& name, const FunctionSig& sig);
    std::optional<FunctionSig> lookup_function(const std::string& name) const;
    /// @}

    /// @name Type Operations
    /// @{
    void declare_struct(const std::string& name, const StructDef& def);
    void declare_variant(const std::string& name, const VariantDef& def);
    void declare_error(const std::string& name, const ErrorDef& def);
    void declare_trait(const std::string& name, const TraitDef& def);

    std::optional<StructDef> lookup_struct(const std::string& name) const;
    std::optional<VariantDef> lookup_variant(const std::string& name) const;
    std::optional<ErrorDef> lookup_error(const std::string& name) const;
    std::optional<TraitDef> lookup_trait(const std::string& name) const;
    /// @}

    /// @name Method Operations
    /// Methods are keyed by "TypeName.methodName"
    /// @{
    void declare_method(const std::string& type_name, const std::string& method_name, const FunctionSig& sig);
    std::optional<FunctionSig> lookup_method(const std::string& type_name, const std::string& method_name) const;
    /// @}

    /// @name Trait Implementation Operations
    /// Implementations are keyed by "TypeName:TraitName"
    /// @{
    void declare_impl(const std::string& type_name, const std::string& trait_name);
    bool has_impl(const std::string& type_name, const std::string& trait_name) const;
    /// @}

    /// @name Built-in Methods
    /// @{
    /**
     * @brief Looks up a built-in method for a type.
     *
     * Handles methods like:
     * - Array: len(), push()
     * - String: len(), at(), contains(), split(), etc.
     * - Map: len(), has(), get(), delete()
     * - Channel: send(), recv()
     *
     * @param type The type to look up methods for.
     * @param method The method name.
     * @return The method signature if found, nullopt otherwise.
     */
    std::optional<FunctionSig> lookup_builtin_method(ResolvedTypePtr type, const std::string& method) const;
    /// @}

private:
    std::vector<Scope> scopes_;                               ///< Scope stack
    std::unordered_map<std::string, FunctionSig> functions_;  ///< Function declarations
    std::unordered_map<std::string, StructDef> structs_;      ///< Struct definitions
    std::unordered_map<std::string, VariantDef> variants_;    ///< Variant definitions
    std::unordered_map<std::string, ErrorDef> errors_;        ///< Error definitions
    std::unordered_map<std::string, TraitDef> traits_;        ///< Trait definitions
    std::unordered_map<std::string, FunctionSig> methods_;    ///< Method signatures
    std::unordered_map<std::string, bool> impls_;             ///< Trait implementations
};

} // namespace shotgun
