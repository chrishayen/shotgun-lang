#pragma once

#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <unordered_map>

namespace shotgun {

struct ResolvedType;
using ResolvedTypePtr = std::shared_ptr<ResolvedType>;

// Resolved types - after type checking, all types are concrete
struct ResolvedType {
    enum class Kind {
        Void,
        Int,
        Bool,
        Str,
        Char,
        F32,
        F64,
        U32,
        U64,
        Array,
        Map,        // Map<K, V> - key and value in type_args
        Optional,
        Function,
        Channel,
        Struct,
        Variant,
        Error,
        TypeParam,  // Unresolved generic parameter
    };

    Kind kind;

    // For Array, Optional, Channel
    ResolvedTypePtr element_type;

    // For Function
    std::vector<ResolvedTypePtr> param_types;
    ResolvedTypePtr return_type;

    // For Struct, Variant, Error - the name
    std::string name;

    // For generic instantiations
    std::vector<ResolvedTypePtr> type_args;

    // For TypeParam
    std::string param_name;

    // Constructors
    static ResolvedTypePtr make_void();
    static ResolvedTypePtr make_int();
    static ResolvedTypePtr make_bool();
    static ResolvedTypePtr make_str();
    static ResolvedTypePtr make_char();
    static ResolvedTypePtr make_f32();
    static ResolvedTypePtr make_f64();
    static ResolvedTypePtr make_u32();
    static ResolvedTypePtr make_u64();
    static ResolvedTypePtr make_array(ResolvedTypePtr elem);
    static ResolvedTypePtr make_map(ResolvedTypePtr key, ResolvedTypePtr value);
    static ResolvedTypePtr make_optional(ResolvedTypePtr elem);
    static ResolvedTypePtr make_channel(ResolvedTypePtr elem);
    static ResolvedTypePtr make_function(std::vector<ResolvedTypePtr> params, ResolvedTypePtr ret);
    static ResolvedTypePtr make_struct(const std::string& name, std::vector<ResolvedTypePtr> type_args = {});
    static ResolvedTypePtr make_variant(const std::string& name, std::vector<ResolvedTypePtr> type_args = {});
    static ResolvedTypePtr make_error(const std::string& name);
    static ResolvedTypePtr make_type_param(const std::string& name);

    // Type equality
    bool equals(const ResolvedType& other) const;

    // String representation for error messages
    std::string to_string() const;

    // Check if type contains unresolved type parameters
    bool has_type_params() const;

    // Substitute type parameters with concrete types
    ResolvedTypePtr substitute(const std::unordered_map<std::string, ResolvedTypePtr>& subst) const;
};

// Field info for structs/variants
struct FieldInfo {
    std::string name;
    ResolvedTypePtr type;
    bool is_optional = false;
};

// Variant case info
struct VariantCaseInfo {
    std::string name;
    std::vector<FieldInfo> fields;
};

// Function signature
struct FunctionSig {
    std::string name;
    std::vector<std::string> type_params;
    std::vector<std::pair<std::string, ResolvedTypePtr>> params;
    ResolvedTypePtr return_type;
    bool has_self = false;
};

// Struct definition
struct StructDef {
    std::string name;
    std::vector<std::string> type_params;
    std::vector<FieldInfo> fields;
};

// Variant definition
struct VariantDef {
    std::string name;
    std::vector<std::string> type_params;
    std::vector<VariantCaseInfo> cases;
};

// Trait definition
struct TraitDef {
    std::string name;
    std::vector<FunctionSig> methods;
};

// Error definition
struct ErrorDef {
    std::string name;
    std::vector<FieldInfo> fields;
};

} // namespace shotgun
