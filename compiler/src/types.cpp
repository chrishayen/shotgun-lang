#include "types.hpp"

namespace shotgun {

ResolvedTypePtr ResolvedType::make_void() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Void;
    return t;
}

ResolvedTypePtr ResolvedType::make_int() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Int;
    return t;
}

ResolvedTypePtr ResolvedType::make_bool() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Bool;
    return t;
}

ResolvedTypePtr ResolvedType::make_str() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Str;
    return t;
}

ResolvedTypePtr ResolvedType::make_char() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Char;
    return t;
}

ResolvedTypePtr ResolvedType::make_f32() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::F32;
    return t;
}

ResolvedTypePtr ResolvedType::make_f64() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::F64;
    return t;
}

ResolvedTypePtr ResolvedType::make_u32() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::U32;
    return t;
}

ResolvedTypePtr ResolvedType::make_u64() {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::U64;
    return t;
}

ResolvedTypePtr ResolvedType::make_array(ResolvedTypePtr elem) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Array;
    t->element_type = elem;
    return t;
}

ResolvedTypePtr ResolvedType::make_map(ResolvedTypePtr key, ResolvedTypePtr value) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Map;
    t->type_args.push_back(key);    // type_args[0] = key type
    t->type_args.push_back(value);  // type_args[1] = value type
    return t;
}

ResolvedTypePtr ResolvedType::make_optional(ResolvedTypePtr elem) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Optional;
    t->element_type = elem;
    return t;
}

ResolvedTypePtr ResolvedType::make_channel(ResolvedTypePtr elem) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Channel;
    t->element_type = elem;
    return t;
}

ResolvedTypePtr ResolvedType::make_function(std::vector<ResolvedTypePtr> params, ResolvedTypePtr ret) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Function;
    t->param_types = std::move(params);
    t->return_type = ret;
    return t;
}

ResolvedTypePtr ResolvedType::make_struct(const std::string& name, std::vector<ResolvedTypePtr> type_args) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Struct;
    t->name = name;
    t->type_args = std::move(type_args);
    return t;
}

ResolvedTypePtr ResolvedType::make_variant(const std::string& name, std::vector<ResolvedTypePtr> type_args) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Variant;
    t->name = name;
    t->type_args = std::move(type_args);
    return t;
}

ResolvedTypePtr ResolvedType::make_error(const std::string& name) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::Error;
    t->name = name;
    return t;
}

ResolvedTypePtr ResolvedType::make_type_param(const std::string& name) {
    auto t = std::make_shared<ResolvedType>();
    t->kind = Kind::TypeParam;
    t->param_name = name;
    return t;
}

bool ResolvedType::equals(const ResolvedType& other) const {
    if (kind != other.kind) return false;

    switch (kind) {
        case Kind::Void:
        case Kind::Int:
        case Kind::Bool:
        case Kind::Str:
        case Kind::Char:
        case Kind::F32:
        case Kind::F64:
        case Kind::U32:
        case Kind::U64:
            return true;

        case Kind::Array:
        case Kind::Optional:
        case Kind::Channel:
            return element_type->equals(*other.element_type);

        case Kind::Map:
            // Compare key and value types stored in type_args
            return type_args[0]->equals(*other.type_args[0]) &&
                   type_args[1]->equals(*other.type_args[1]);

        case Kind::Function:
            if (param_types.size() != other.param_types.size()) return false;
            for (size_t i = 0; i < param_types.size(); i++) {
                if (!param_types[i]->equals(*other.param_types[i])) return false;
            }
            return return_type->equals(*other.return_type);

        case Kind::Struct:
        case Kind::Variant:
        case Kind::Error:
            if (name != other.name) return false;
            if (type_args.size() != other.type_args.size()) return false;
            for (size_t i = 0; i < type_args.size(); i++) {
                if (!type_args[i]->equals(*other.type_args[i])) return false;
            }
            return true;

        case Kind::TypeParam:
            return param_name == other.param_name;
    }
    return false;
}

std::string ResolvedType::to_string() const {
    switch (kind) {
        case Kind::Void: return "void";
        case Kind::Int: return "int";
        case Kind::Bool: return "bool";
        case Kind::Str: return "str";
        case Kind::Char: return "char";
        case Kind::F32: return "f32";
        case Kind::F64: return "f64";
        case Kind::U32: return "u32";
        case Kind::U64: return "u64";

        case Kind::Array:
            return element_type->to_string() + "[]";

        case Kind::Map:
            return "Map<" + type_args[0]->to_string() + ", " + type_args[1]->to_string() + ">";

        case Kind::Optional:
            return element_type->to_string() + "?";

        case Kind::Channel:
            return "chan " + element_type->to_string();

        case Kind::Function: {
            std::string s = "fn(";
            for (size_t i = 0; i < param_types.size(); i++) {
                if (i > 0) s += ", ";
                s += param_types[i]->to_string();
            }
            s += ")";
            if (return_type && return_type->kind != Kind::Void) {
                s += " " + return_type->to_string();
            }
            return s;
        }

        case Kind::Struct:
        case Kind::Variant:
        case Kind::Error: {
            std::string s = name;
            if (!type_args.empty()) {
                s += "<";
                for (size_t i = 0; i < type_args.size(); i++) {
                    if (i > 0) s += ", ";
                    s += type_args[i]->to_string();
                }
                s += ">";
            }
            return s;
        }

        case Kind::TypeParam:
            return param_name;
    }
    return "unknown";
}

bool ResolvedType::has_type_params() const {
    if (kind == Kind::TypeParam) return true;

    if (element_type && element_type->has_type_params()) return true;
    if (return_type && return_type->has_type_params()) return true;

    for (const auto& p : param_types) {
        if (p->has_type_params()) return true;
    }
    for (const auto& a : type_args) {
        if (a->has_type_params()) return true;
    }
    return false;
}

ResolvedTypePtr ResolvedType::substitute(const std::unordered_map<std::string, ResolvedTypePtr>& subst) const {
    if (kind == Kind::TypeParam) {
        auto it = subst.find(param_name);
        if (it != subst.end()) return it->second;
        return make_type_param(param_name);
    }

    switch (kind) {
        case Kind::Void:
        case Kind::Int:
        case Kind::Bool:
        case Kind::Str:
        case Kind::Char:
        case Kind::F32:
        case Kind::F64:
        case Kind::U32:
        case Kind::U64:
            return std::make_shared<ResolvedType>(*this);

        case Kind::Array:
            return make_array(element_type->substitute(subst));

        case Kind::Map:
            return make_map(type_args[0]->substitute(subst), type_args[1]->substitute(subst));

        case Kind::Optional:
            return make_optional(element_type->substitute(subst));

        case Kind::Channel:
            return make_channel(element_type->substitute(subst));

        case Kind::Function: {
            std::vector<ResolvedTypePtr> new_params;
            for (const auto& p : param_types) {
                new_params.push_back(p->substitute(subst));
            }
            return make_function(new_params, return_type->substitute(subst));
        }

        case Kind::Struct: {
            std::vector<ResolvedTypePtr> new_args;
            for (const auto& a : type_args) {
                new_args.push_back(a->substitute(subst));
            }
            return make_struct(name, new_args);
        }

        case Kind::Variant: {
            std::vector<ResolvedTypePtr> new_args;
            for (const auto& a : type_args) {
                new_args.push_back(a->substitute(subst));
            }
            return make_variant(name, new_args);
        }

        case Kind::Error:
            return make_error(name);

        case Kind::TypeParam:
            return make_type_param(param_name);
    }
    return nullptr;
}

} // namespace shotgun
