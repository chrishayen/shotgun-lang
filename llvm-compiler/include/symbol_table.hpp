#pragma once

#include "types.hpp"
#include <string>
#include <vector>
#include <unordered_map>
#include <optional>

namespace shotgun {

// Variable entry in symbol table
struct VarEntry {
    std::string name;
    ResolvedTypePtr type;
    bool is_const;
    bool is_initialized;
};

// A single scope
struct Scope {
    std::unordered_map<std::string, VarEntry> variables;
    Scope* parent = nullptr;
};

class SymbolTable {
public:
    SymbolTable();

    // Scope management
    void push_scope();
    void pop_scope();

    // Variables
    bool declare_var(const std::string& name, ResolvedTypePtr type, bool is_const = false);
    std::optional<VarEntry> lookup_var(const std::string& name) const;
    bool is_declared_in_current_scope(const std::string& name) const;

    // Functions
    void declare_function(const std::string& name, const FunctionSig& sig);
    std::optional<FunctionSig> lookup_function(const std::string& name) const;

    // Types
    void declare_struct(const std::string& name, const StructDef& def);
    void declare_variant(const std::string& name, const VariantDef& def);
    void declare_error(const std::string& name, const ErrorDef& def);
    void declare_trait(const std::string& name, const TraitDef& def);

    std::optional<StructDef> lookup_struct(const std::string& name) const;
    std::optional<VariantDef> lookup_variant(const std::string& name) const;
    std::optional<ErrorDef> lookup_error(const std::string& name) const;
    std::optional<TraitDef> lookup_trait(const std::string& name) const;

    // Methods - keyed by "TypeName.methodName"
    void declare_method(const std::string& type_name, const std::string& method_name, const FunctionSig& sig);
    std::optional<FunctionSig> lookup_method(const std::string& type_name, const std::string& method_name) const;

    // Trait implementations - keyed by "TypeName:TraitName"
    void declare_impl(const std::string& type_name, const std::string& trait_name);
    bool has_impl(const std::string& type_name, const std::string& trait_name) const;

    // Built-in methods (like .len(), .push(), etc.)
    std::optional<FunctionSig> lookup_builtin_method(ResolvedTypePtr type, const std::string& method) const;

private:
    std::vector<Scope> scopes_;
    std::unordered_map<std::string, FunctionSig> functions_;
    std::unordered_map<std::string, StructDef> structs_;
    std::unordered_map<std::string, VariantDef> variants_;
    std::unordered_map<std::string, ErrorDef> errors_;
    std::unordered_map<std::string, TraitDef> traits_;
    std::unordered_map<std::string, FunctionSig> methods_;
    std::unordered_map<std::string, bool> impls_;
};

} // namespace shotgun
