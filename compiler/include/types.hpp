/**
 * @file types.hpp
 * @brief Resolved type system for semantic analysis.
 *
 * This file defines ResolvedType which represents types after semantic
 * analysis, along with supporting structures for structs, variants,
 * traits, functions, and errors.
 */

#pragma once

#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <unordered_map>

namespace shotgun {

struct ResolvedType;
using ResolvedTypePtr = std::shared_ptr<ResolvedType>;  ///< Shared pointer to resolved type

/**
 * @brief Represents a fully resolved type after semantic analysis.
 *
 * Unlike AST Type nodes which are syntactic, ResolvedType represents
 * the actual types determined during type checking. This includes
 * primitive types, compound types (array, optional, function), and
 * user-defined types (struct, variant, error).
 */
struct ResolvedType {
    /**
     * @brief Enumeration of all resolved type kinds.
     */
    enum class Kind {
        Void,       ///< No value (unit type)
        Int,        ///< 64-bit signed integer
        Bool,       ///< Boolean
        Str,        ///< String
        Char,       ///< Character
        F32,        ///< 32-bit float
        F64,        ///< 64-bit float
        U32,        ///< 32-bit unsigned integer
        U64,        ///< 64-bit unsigned integer
        Array,      ///< Array type (element_type is the element type)
        Map,        ///< Map type (type_args[0]=key, type_args[1]=value)
        Optional,   ///< Optional type (element_type is the inner type)
        Function,   ///< Function type (param_types, return_type)
        Channel,    ///< Channel type (element_type is the element type)
        Struct,     ///< User-defined struct
        Variant,    ///< User-defined sum type
        Error,      ///< User-defined error type
        TypeParam,  ///< Unresolved generic type parameter
    };

    Kind kind;  ///< The kind of type

    /// @name Type Data Fields
    /// @{
    ResolvedTypePtr element_type;              ///< Element type (Array, Optional, Channel)
    std::vector<ResolvedTypePtr> param_types;  ///< Parameter types (Function)
    ResolvedTypePtr return_type;               ///< Return type (Function)
    std::string name;                          ///< Type name (Struct, Variant, Error)
    std::vector<ResolvedTypePtr> type_args;    ///< Generic type arguments
    std::string param_name;                    ///< Parameter name (TypeParam)
    /// @}

    /// @name Factory Methods
    /// Static factory methods for creating types.
    /// @{
    static ResolvedTypePtr make_void();   ///< Creates void type
    static ResolvedTypePtr make_int();    ///< Creates int type
    static ResolvedTypePtr make_bool();   ///< Creates bool type
    static ResolvedTypePtr make_str();    ///< Creates str type
    static ResolvedTypePtr make_char();   ///< Creates char type
    static ResolvedTypePtr make_f32();    ///< Creates f32 type
    static ResolvedTypePtr make_f64();    ///< Creates f64 type
    static ResolvedTypePtr make_u32();    ///< Creates u32 type
    static ResolvedTypePtr make_u64();    ///< Creates u64 type
    static ResolvedTypePtr make_array(ResolvedTypePtr elem);      ///< Creates array type
    static ResolvedTypePtr make_map(ResolvedTypePtr key, ResolvedTypePtr value);  ///< Creates map type
    static ResolvedTypePtr make_optional(ResolvedTypePtr elem);   ///< Creates optional type
    static ResolvedTypePtr make_channel(ResolvedTypePtr elem);    ///< Creates channel type
    static ResolvedTypePtr make_function(std::vector<ResolvedTypePtr> params, ResolvedTypePtr ret);  ///< Creates function type
    static ResolvedTypePtr make_struct(const std::string& name, std::vector<ResolvedTypePtr> type_args = {});  ///< Creates struct type
    static ResolvedTypePtr make_variant(const std::string& name, std::vector<ResolvedTypePtr> type_args = {});  ///< Creates variant type
    static ResolvedTypePtr make_error(const std::string& name);   ///< Creates error type
    static ResolvedTypePtr make_type_param(const std::string& name);  ///< Creates type parameter
    /// @}

    /// @name Type Operations
    /// @{
    /**
     * @brief Checks structural equality with another type.
     * @param other The type to compare with.
     * @return True if types are structurally equal.
     */
    bool equals(const ResolvedType& other) const;

    /**
     * @brief Returns a human-readable string representation.
     * @return String like "int", "str[]", "Map<str, int>", etc.
     */
    std::string to_string() const;

    /**
     * @brief Checks if type contains unresolved type parameters.
     * @return True if any TypeParam is present in the type tree.
     */
    bool has_type_params() const;

    /**
     * @brief Substitutes type parameters with concrete types.
     * @param subst Map from parameter names to concrete types.
     * @return New type with parameters substituted.
     */
    ResolvedTypePtr substitute(const std::unordered_map<std::string, ResolvedTypePtr>& subst) const;
    /// @}
};

/// @brief Field information for structs and variants
struct FieldInfo {
    std::string name;           ///< Field name
    ResolvedTypePtr type;       ///< Field type
    bool is_optional = false;   ///< True if field is optional (T?)
};

/// @brief Variant case information
struct VariantCaseInfo {
    std::string name;               ///< Case name
    std::vector<FieldInfo> fields;  ///< Case fields
};

/// @brief Function signature for type checking
struct FunctionSig {
    std::string name;                                            ///< Function name
    std::vector<std::string> type_params;                        ///< Generic type parameters
    std::vector<std::pair<std::string, ResolvedTypePtr>> params;  ///< Parameters (name, type)
    ResolvedTypePtr return_type;                                 ///< Return type
    bool has_self = false;                                       ///< True if first param is self
};

/// @brief Struct type definition
struct StructDef {
    std::string name;                        ///< Struct name
    std::vector<std::string> type_params;    ///< Generic type parameters
    std::vector<FieldInfo> fields;           ///< Struct fields
};

/// @brief Variant (sum type) definition
struct VariantDef {
    std::string name;                        ///< Variant name
    std::vector<std::string> type_params;    ///< Generic type parameters
    std::vector<VariantCaseInfo> cases;      ///< Variant cases
};

/// @brief Trait definition (interface)
struct TraitDef {
    std::string name;                    ///< Trait name
    std::vector<FunctionSig> methods;    ///< Required methods
};

/// @brief Error type definition
struct ErrorDef {
    std::string name;               ///< Error type name
    std::vector<FieldInfo> fields;  ///< Error fields
};

} // namespace shotgun
