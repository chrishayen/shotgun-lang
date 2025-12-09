#pragma once

#include "token.hpp"
#include <memory>
#include <string>
#include <vector>
#include <optional>
#include <variant>

namespace shotgun {

// Forward declarations
struct Expr;
struct Stmt;
struct Type;
struct Pattern;

using ExprPtr = std::unique_ptr<Expr>;
using StmtPtr = std::unique_ptr<Stmt>;
using TypePtr = std::unique_ptr<Type>;
using PatternPtr = std::unique_ptr<Pattern>;

// === Types ===

struct Type {
    SourceLoc loc;

    struct Named {
        std::string name;
        std::vector<TypePtr> type_args; // for generics like Box<int>
    };

    struct Array {
        TypePtr element;
    };

    struct Optional {
        TypePtr inner;
    };

    struct Function {
        std::vector<TypePtr> params;
        TypePtr return_type; // nullptr for void
    };

    struct Channel {
        TypePtr element;
    };

    struct Result {
        TypePtr value;
        std::string error_type;
    };

    std::variant<Named, Array, Optional, Function, Channel, Result> kind;
};

// === Patterns (for match) ===

struct Pattern {
    SourceLoc loc;

    struct Literal {
        Token value; // Int, Float, String, Char, True, False
    };

    struct Ident {
        std::string name;
    };

    struct Wildcard {};

    struct Tuple {
        std::vector<PatternPtr> elements;
    };

    struct Variant {
        std::string type_name;      // e.g., "Option" (may be empty if using 'using')
        std::string variant_name;   // e.g., "Some"
        std::vector<std::pair<std::string, std::string>> bindings; // field -> var
    };

    std::variant<Literal, Ident, Wildcard, Tuple, Variant> kind;
};

// === Expressions ===

// String part for interpolated strings
struct StringPart {
    bool is_expr;           // true if this is an interpolation
    std::string literal;    // literal text (if !is_expr)
    // expr is stored separately in StringLit::exprs
};

struct Expr {
    SourceLoc loc;

    struct IntLit { int64_t value; };
    struct FloatLit { double value; };
    struct StringLit {
        std::string value;                      // simple string (no interpolation)
        std::vector<StringPart> parts;          // for f-strings
        std::vector<std::unique_ptr<Expr>> exprs; // expressions for interpolation
    };
    struct CharLit { char value; };
    struct BoolLit { bool value; };
    struct NoneLit {};

    struct Ident { std::string name; };

    struct Binary {
        ExprPtr left;
        TokenKind op;
        ExprPtr right;
    };

    struct Unary {
        TokenKind op;
        ExprPtr operand;
    };

    struct Call {
        ExprPtr callee;
        std::vector<ExprPtr> args;
        std::vector<TypePtr> type_args; // for explicit generics: func::<int>(x)
    };

    struct Index {
        ExprPtr object;
        ExprPtr index;
    };

    struct Field {
        ExprPtr object;
        std::string name;
    };

    struct StructLit {
        std::string type_name;
        std::vector<TypePtr> type_args;
        std::vector<std::pair<std::string, ExprPtr>> fields;
    };

    struct ArrayLit {
        std::vector<ExprPtr> elements;
    };

    struct Lambda {
        std::vector<std::pair<std::string, TypePtr>> params;
        TypePtr return_type; // nullptr for inferred
        std::vector<StmtPtr> body;
    };

    struct Match {
        ExprPtr subject;
        std::string using_type; // for 'using' clause
        std::vector<std::pair<PatternPtr, ExprPtr>> arms;
    };

    struct Or {
        ExprPtr left;
        ExprPtr right; // fallback value, or nullptr for 'or return'
        bool is_return;
        bool is_error;
        std::string error_type;
        std::vector<std::pair<std::string, ExprPtr>> error_fields;
    };

    struct Cast {
        ExprPtr value;
        TypePtr target_type;
    };

    struct Is {
        ExprPtr value;
        std::string variant_name;
    };

    struct Block {
        std::vector<StmtPtr> stmts;
        ExprPtr result; // final expression (may be nullptr)
    };

    struct ChanRecv {
        ExprPtr channel;
    };

    std::variant<
        IntLit, FloatLit, StringLit, CharLit, BoolLit, NoneLit,
        Ident, Binary, Unary, Call, Index, Field,
        StructLit, ArrayLit, Lambda, Match, Or, Cast, Is, Block, ChanRecv
    > kind;
};

// === Statements ===

struct Stmt {
    SourceLoc loc;

    struct VarDecl {
        std::string name;
        TypePtr type;       // nullptr for inferred (:=)
        ExprPtr init;
        bool is_const;
    };

    struct Assign {
        ExprPtr target;
        TokenKind op; // Assign, PlusEq, MinusEq, etc.
        ExprPtr value;
    };

    struct ExprStmt {
        ExprPtr expr;
    };

    struct Return {
        ExprPtr value; // nullptr for bare return
    };

    struct If {
        ExprPtr condition;
        std::vector<StmtPtr> then_block;
        std::vector<StmtPtr> else_block; // may be empty
    };

    struct While {
        ExprPtr condition;
        std::vector<StmtPtr> body;
    };

    struct For {
        std::string var;
        std::string var2; // for map iteration: for k, v in map
        ExprPtr iterable;
        std::vector<StmtPtr> body;
    };

    struct Break {};
    struct Continue {};

    struct Go {
        ExprPtr call;
    };

    struct ChanSend {
        ExprPtr channel;
        ExprPtr value;
    };

    std::variant<
        VarDecl, Assign, ExprStmt, Return,
        If, While, For, Break, Continue,
        Go, ChanSend
    > kind;
};

// === Top-level Declarations ===

struct Field {
    std::string name;
    TypePtr type;
    bool optional;
};

struct Param {
    std::string name;
    TypePtr type;
};

struct FnDecl {
    SourceLoc loc;
    std::string name;
    std::vector<std::string> type_params;
    std::vector<Param> params;
    TypePtr return_type; // nullptr for void
    std::vector<StmtPtr> body;
};

struct StructDecl {
    SourceLoc loc;
    std::string name;
    std::vector<std::string> type_params;
    std::vector<Field> fields;
};

struct VariantDecl {
    SourceLoc loc;
    std::string name;
    std::vector<std::string> type_params;

    struct Case {
        std::string name;
        std::vector<Field> fields;
    };
    std::vector<Case> cases;
};

struct TraitDecl {
    SourceLoc loc;
    std::string name;

    struct Method {
        std::string name;
        std::vector<Param> params;
        TypePtr return_type;
    };
    std::vector<Method> methods;
};

struct ImplDecl {
    SourceLoc loc;
    std::string type_name;
    std::vector<std::string> type_params;
    std::string trait_name; // empty for methods, set for trait impl
    std::vector<FnDecl> methods;
};

struct ErrorDecl {
    SourceLoc loc;
    std::string name;
    std::vector<Field> fields;
};

struct UseDecl {
    SourceLoc loc;
    std::vector<std::string> modules;
};

// === Program ===

using Decl = std::variant<FnDecl, StructDecl, VariantDecl, TraitDecl, ImplDecl, ErrorDecl, UseDecl>;

struct Program {
    std::vector<Decl> decls;
};

} // namespace shotgun
