#include "symbol_table.hpp"

namespace shotgun {

SymbolTable::SymbolTable() {
    // Start with global scope
    scopes_.push_back(Scope{});
}

void SymbolTable::push_scope() {
    Scope s;
    s.parent = &scopes_.back();
    scopes_.push_back(s);
}

void SymbolTable::pop_scope() {
    if (scopes_.size() > 1) {
        scopes_.pop_back();
    }
}

bool SymbolTable::declare_var(const std::string& name, ResolvedTypePtr type, bool is_const) {
    auto& current = scopes_.back();
    if (current.variables.count(name)) {
        return false; // Already declared in this scope
    }
    current.variables[name] = VarEntry{name, type, is_const, true};
    return true;
}

std::optional<VarEntry> SymbolTable::lookup_var(const std::string& name) const {
    // Search from innermost to outermost scope
    for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
        auto var_it = it->variables.find(name);
        if (var_it != it->variables.end()) {
            return var_it->second;
        }
    }
    return std::nullopt;
}

bool SymbolTable::is_declared_in_current_scope(const std::string& name) const {
    return scopes_.back().variables.count(name) > 0;
}

void SymbolTable::declare_function(const std::string& name, const FunctionSig& sig) {
    functions_[name] = sig;
}

std::optional<FunctionSig> SymbolTable::lookup_function(const std::string& name) const {
    auto it = functions_.find(name);
    if (it != functions_.end()) {
        return it->second;
    }
    return std::nullopt;
}

void SymbolTable::declare_struct(const std::string& name, const StructDef& def) {
    structs_[name] = def;
}

void SymbolTable::declare_variant(const std::string& name, const VariantDef& def) {
    variants_[name] = def;
}

void SymbolTable::declare_error(const std::string& name, const ErrorDef& def) {
    errors_[name] = def;
}

void SymbolTable::declare_trait(const std::string& name, const TraitDef& def) {
    traits_[name] = def;
}

std::optional<StructDef> SymbolTable::lookup_struct(const std::string& name) const {
    auto it = structs_.find(name);
    if (it != structs_.end()) {
        return it->second;
    }
    return std::nullopt;
}

std::optional<VariantDef> SymbolTable::lookup_variant(const std::string& name) const {
    auto it = variants_.find(name);
    if (it != variants_.end()) {
        return it->second;
    }
    return std::nullopt;
}

std::optional<ErrorDef> SymbolTable::lookup_error(const std::string& name) const {
    auto it = errors_.find(name);
    if (it != errors_.end()) {
        return it->second;
    }
    return std::nullopt;
}

std::optional<TraitDef> SymbolTable::lookup_trait(const std::string& name) const {
    auto it = traits_.find(name);
    if (it != traits_.end()) {
        return it->second;
    }
    return std::nullopt;
}

void SymbolTable::declare_method(const std::string& type_name, const std::string& method_name, const FunctionSig& sig) {
    methods_[type_name + "." + method_name] = sig;
}

std::optional<FunctionSig> SymbolTable::lookup_method(const std::string& type_name, const std::string& method_name) const {
    auto it = methods_.find(type_name + "." + method_name);
    if (it != methods_.end()) {
        return it->second;
    }
    return std::nullopt;
}

void SymbolTable::declare_impl(const std::string& type_name, const std::string& trait_name) {
    impls_[type_name + ":" + trait_name] = true;
}

bool SymbolTable::has_impl(const std::string& type_name, const std::string& trait_name) const {
    return impls_.count(type_name + ":" + trait_name) > 0;
}

std::optional<FunctionSig> SymbolTable::lookup_builtin_method(ResolvedTypePtr type, const std::string& method) const {
    // Array methods
    if (type->kind == ResolvedType::Kind::Array) {
        if (method == "len") {
            return FunctionSig{"len", {}, {}, ResolvedType::make_int(), true};
        }
        if (method == "push") {
            return FunctionSig{"push", {}, {{"value", type->element_type}}, ResolvedType::make_void(), true};
        }
    }

    // String methods
    if (type->kind == ResolvedType::Kind::Str) {
        if (method == "len") {
            return FunctionSig{"len", {}, {}, ResolvedType::make_int(), true};
        }
        if (method == "at") {
            return FunctionSig{"at", {}, {{"index", ResolvedType::make_int()}}, ResolvedType::make_char(), true};
        }
        if (method == "contains") {
            return FunctionSig{"contains", {}, {{"substr", ResolvedType::make_str()}}, ResolvedType::make_bool(), true};
        }
        if (method == "starts_with") {
            return FunctionSig{"starts_with", {}, {{"prefix", ResolvedType::make_str()}}, ResolvedType::make_bool(), true};
        }
        if (method == "ends_with") {
            return FunctionSig{"ends_with", {}, {{"suffix", ResolvedType::make_str()}}, ResolvedType::make_bool(), true};
        }
        if (method == "find") {
            return FunctionSig{"find", {}, {{"substr", ResolvedType::make_str()}}, ResolvedType::make_int(), true};
        }
        if (method == "slice") {
            return FunctionSig{"slice", {}, {{"start", ResolvedType::make_int()}, {"end", ResolvedType::make_int()}}, ResolvedType::make_str(), true};
        }
        if (method == "split") {
            return FunctionSig{"split", {}, {{"sep", ResolvedType::make_str()}}, ResolvedType::make_array(ResolvedType::make_str()), true};
        }
        if (method == "trim") {
            return FunctionSig{"trim", {}, {}, ResolvedType::make_str(), true};
        }
        if (method == "to_upper") {
            return FunctionSig{"to_upper", {}, {}, ResolvedType::make_str(), true};
        }
        if (method == "to_lower") {
            return FunctionSig{"to_lower", {}, {}, ResolvedType::make_str(), true};
        }
        if (method == "replace") {
            return FunctionSig{"replace", {}, {{"old", ResolvedType::make_str()}, {"new", ResolvedType::make_str()}}, ResolvedType::make_str(), true};
        }
    }

    // Map methods (for Map<K, V> type)
    if (type->kind == ResolvedType::Kind::Map && type->type_args.size() == 2) {
        auto key_type = type->type_args[0];
        auto val_type = type->type_args[1];

        if (method == "len") {
            return FunctionSig{"len", {}, {}, ResolvedType::make_int(), true};
        }
        if (method == "has") {
            return FunctionSig{"has", {}, {{"key", key_type}}, ResolvedType::make_bool(), true};
        }
        if (method == "get") {
            return FunctionSig{"get", {}, {{"key", key_type}}, val_type, true};
        }
        if (method == "delete") {
            return FunctionSig{"delete", {}, {{"key", key_type}}, ResolvedType::make_void(), true};
        }
    }

    // Channel methods
    if (type->kind == ResolvedType::Kind::Channel) {
        if (method == "send") {
            return FunctionSig{"send", {}, {{"value", type->element_type}}, ResolvedType::make_void(), true};
        }
        if (method == "recv") {
            return FunctionSig{"recv", {}, {}, type->element_type, true};
        }
    }

    return std::nullopt;
}

} // namespace shotgun
