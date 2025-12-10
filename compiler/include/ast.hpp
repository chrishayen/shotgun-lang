/**
 * @file ast.hpp
 * @brief Abstract Syntax Tree definitions for the Shotgun language.
 *
 * This file defines all AST node types including:
 * - Type representations (Named, Array, Optional, Function, Channel, Result)
 * - Patterns for match expressions
 * - Expressions (literals, binary/unary ops, calls, etc.)
 * - Statements (variable decls, assignments, control flow)
 * - Top-level declarations (functions, structs, variants, traits, impls)
 */

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

using ExprPtr = std::unique_ptr<Expr>;    ///< Owned pointer to expression node
using StmtPtr = std::unique_ptr<Stmt>;    ///< Owned pointer to statement node
using TypePtr = std::unique_ptr<Type>;    ///< Owned pointer to type node
using PatternPtr = std::unique_ptr<Pattern>;  ///< Owned pointer to pattern node

// === Types ===

/**
 * @brief AST node representing a type annotation.
 *
 * Types in Shotgun can be:
 * - Named: Simple or generic types (int, str, Box<T>)
 * - Array: Array types (int[])
 * - Optional: Nullable types (str?)
 * - Function: Function types (fn(int) str)
 * - Channel: Channel types (chan int)
 * - Result: Result types (T or Error)
 */
struct Type {
    SourceLoc loc;  ///< Source location of this type

    /// @brief Named type (e.g., int, str, Person, Box<int>)
    struct Named {
        std::string name;                    ///< Type name
        std::vector<TypePtr> type_args;      ///< Generic type arguments
    };

    /// @brief Array type (e.g., int[])
    struct Array {
        TypePtr element;  ///< Element type
    };

    /// @brief Optional type (e.g., str?)
    struct Optional {
        TypePtr inner;  ///< Inner type
    };

    /// @brief Function type (e.g., fn(int, str) bool)
    struct Function {
        std::vector<TypePtr> params;  ///< Parameter types
        TypePtr return_type;          ///< Return type (nullptr for void)
    };

    /// @brief Channel type (e.g., chan int)
    struct Channel {
        TypePtr element;  ///< Element type
    };

    /// @brief Result type (e.g., int or Error)
    struct Result {
        TypePtr value;            ///< Success type
        std::string error_type;   ///< Error type name
    };

    /// @brief The kind of type this node represents
    std::variant<Named, Array, Optional, Function, Channel, Result> kind;
};

// === Patterns (for match) ===

/**
 * @brief Pattern node for match expressions.
 *
 * Patterns are used in match arms to destructure and bind values.
 * Supported patterns:
 * - Literal: Matches a specific value (42, "hello", true)
 * - Ident: Binds the value to a variable
 * - Wildcard: Matches anything (_)
 * - Tuple: Matches tuples ((a, b))
 * - Variant: Matches variant cases (Some { value: v })
 */
struct Pattern {
    SourceLoc loc;  ///< Source location

    /// @brief Literal pattern (e.g., 42, "hello", true)
    struct Literal {
        Token value;  ///< The literal token (Int, Float, String, Char, True, False)
    };

    /// @brief Identifier pattern that binds the matched value
    struct Ident {
        std::string name;  ///< Variable name to bind
    };

    /// @brief Wildcard pattern (_) that matches anything
    struct Wildcard {};

    /// @brief Tuple pattern (e.g., (a, b, c))
    struct Tuple {
        std::vector<PatternPtr> elements;  ///< Element patterns
    };

    /// @brief Variant pattern for matching sum type cases
    struct Variant {
        std::string type_name;       ///< Variant type name (may be empty with 'using')
        std::string variant_name;    ///< Case name (e.g., "Some", "None")
        std::vector<std::pair<std::string, std::string>> bindings;  ///< field -> variable bindings
    };

    /// @brief The kind of pattern this node represents
    std::variant<Literal, Ident, Wildcard, Tuple, Variant> kind;
};

// === Expressions ===

/**
 * @brief Part of an interpolated f-string.
 *
 * F-strings like f"Hello, {name}!" are split into parts:
 * - Literal parts (is_expr=false): "Hello, " and "!"
 * - Expression parts (is_expr=true): index into StringLit::exprs
 */
struct StringPart {
    bool is_expr;         ///< True if this is an interpolation slot
    std::string literal;  ///< Literal text (only used when is_expr=false)
};

/**
 * @brief Expression AST node.
 *
 * Expressions compute values and include:
 * - Literals (int, float, string, char, bool, none)
 * - Identifiers and field access
 * - Binary and unary operations
 * - Function calls and indexing
 * - Struct and array literals
 * - Lambdas and match expressions
 * - Special forms (or, is, cast)
 */
struct Expr {
    SourceLoc loc;  ///< Source location

    struct IntLit { int64_t value; };       ///< Integer literal (e.g., 42)
    struct FloatLit { double value; };      ///< Float literal (e.g., 3.14)
    /// @brief String literal with optional interpolation
    struct StringLit {
        std::string value;                       ///< Simple string content
        std::vector<StringPart> parts;           ///< F-string parts
        std::vector<std::unique_ptr<Expr>> exprs;  ///< Interpolated expressions
    };
    struct CharLit { char value; };         ///< Character literal (e.g., 'a')
    struct BoolLit { bool value; };         ///< Boolean literal (true/false)
    struct NoneLit {};                      ///< None literal

    struct Ident { std::string name; };     ///< Identifier reference

    /// @brief Binary operation (e.g., a + b, x == y)
    struct Binary {
        ExprPtr left;     ///< Left operand
        TokenKind op;     ///< Operator
        ExprPtr right;    ///< Right operand
    };

    /// @brief Unary operation (e.g., -x, !flag)
    struct Unary {
        TokenKind op;     ///< Operator
        ExprPtr operand;  ///< Operand
    };

    /// @brief Function/method call (e.g., foo(x), obj.method(y))
    struct Call {
        ExprPtr callee;                   ///< Function being called
        std::vector<ExprPtr> args;        ///< Arguments
        std::vector<TypePtr> type_args;   ///< Explicit generic args (func::<int>(x))
    };

    /// @brief Index operation (e.g., arr[i], map["key"])
    struct Index {
        ExprPtr object;  ///< Object being indexed
        ExprPtr index;   ///< Index expression
    };

    /// @brief Field access (e.g., person.name)
    struct Field {
        ExprPtr object;     ///< Object
        std::string name;   ///< Field name
    };

    /// @brief Struct literal (e.g., Person { name: "Alice", age: 30 })
    struct StructLit {
        std::string type_name;                             ///< Struct type name
        std::vector<TypePtr> type_args;                    ///< Generic type args
        std::vector<std::pair<std::string, ExprPtr>> fields;  ///< Field initializers
    };

    /// @brief Array literal (e.g., [1, 2, 3])
    struct ArrayLit {
        std::vector<ExprPtr> elements;  ///< Array elements
    };

    /// @brief Anonymous function (e.g., fn(int x) { return x * 2 })
    struct Lambda {
        std::vector<std::pair<std::string, TypePtr>> params;  ///< Parameters
        TypePtr return_type;                                   ///< Return type (nullptr = inferred)
        std::vector<StmtPtr> body;                            ///< Function body
    };

    /// @brief Match expression for pattern matching
    struct Match {
        ExprPtr subject;                                    ///< Value being matched
        std::string using_type;                             ///< Optional 'using' type hint
        std::vector<std::pair<PatternPtr, ExprPtr>> arms;   ///< Match arms
    };

    /// @brief Or expression for optional/error handling
    struct Or {
        ExprPtr left;        ///< Optional/result expression
        ExprPtr right;       ///< Fallback value (nullptr for 'or return')
        bool is_return;      ///< True for 'or return'
        bool is_error;       ///< True for 'or error'
        std::string error_type;  ///< Error type name
        std::vector<std::pair<std::string, ExprPtr>> error_fields;  ///< Error fields
    };

    /// @brief Type cast (e.g., x as int)
    struct Cast {
        ExprPtr value;        ///< Value to cast
        TypePtr target_type;  ///< Target type
    };

    /// @brief Variant tag check (e.g., opt is Some)
    struct Is {
        ExprPtr value;          ///< Value to check
        std::string variant_name;  ///< Expected variant tag
    };

    /// @brief Block expression with optional result
    struct Block {
        std::vector<StmtPtr> stmts;  ///< Statements in block
        ExprPtr result;              ///< Final expression (may be nullptr)
    };

    /// @brief Channel receive operation
    struct ChanRecv {
        ExprPtr channel;  ///< Channel to receive from
    };

    /// @brief The kind of expression this node represents
    std::variant<
        IntLit, FloatLit, StringLit, CharLit, BoolLit, NoneLit,
        Ident, Binary, Unary, Call, Index, Field,
        StructLit, ArrayLit, Lambda, Match, Or, Cast, Is, Block, ChanRecv
    > kind;
};

// === Statements ===

/**
 * @brief Statement AST node.
 *
 * Statements perform actions and control flow:
 * - Variable declarations (int x = 5, y := 10)
 * - Assignments (x = y, x += 1)
 * - Expression statements (function calls, etc.)
 * - Control flow (if, while, for, return, break, continue)
 * - Concurrency (go, channel send)
 */
struct Stmt {
    SourceLoc loc;  ///< Source location

    /// @brief Variable declaration (e.g., int x = 5, y := 10, const z = 100)
    struct VarDecl {
        std::string name;   ///< Variable name
        TypePtr type;       ///< Explicit type (nullptr for inferred := syntax)
        ExprPtr init;       ///< Initializer expression
        bool is_const;      ///< True for const declarations
    };

    /// @brief Assignment statement (e.g., x = y, x += 1)
    struct Assign {
        ExprPtr target;   ///< Assignment target (identifier, index, field)
        TokenKind op;     ///< Operator (Assign, PlusEq, MinusEq, etc.)
        ExprPtr value;    ///< Value being assigned
    };

    /// @brief Expression statement (expression evaluated for side effects)
    struct ExprStmt {
        ExprPtr expr;  ///< Expression
    };

    /// @brief Return statement
    struct Return {
        ExprPtr value;  ///< Return value (nullptr for bare return)
    };

    /// @brief If statement with optional else
    struct If {
        ExprPtr condition;              ///< Condition expression
        std::vector<StmtPtr> then_block;  ///< Then branch
        std::vector<StmtPtr> else_block;  ///< Else branch (may be empty)
    };

    /// @brief While loop
    struct While {
        ExprPtr condition;          ///< Loop condition
        std::vector<StmtPtr> body;  ///< Loop body
    };

    /// @brief For loop (e.g., for x in arr, for k, v in map)
    struct For {
        std::string var;            ///< Loop variable
        std::string var2;           ///< Second variable for map iteration
        ExprPtr iterable;           ///< Collection to iterate
        std::vector<StmtPtr> body;  ///< Loop body
    };

    struct Break {};     ///< Break statement
    struct Continue {};  ///< Continue statement

    /// @brief Go statement for goroutine spawning
    struct Go {
        ExprPtr call;  ///< Function call to run concurrently
    };

    /// @brief Channel send operation
    struct ChanSend {
        ExprPtr channel;  ///< Channel to send to
        ExprPtr value;    ///< Value to send
    };

    /// @brief The kind of statement this node represents
    std::variant<
        VarDecl, Assign, ExprStmt, Return,
        If, While, For, Break, Continue,
        Go, ChanSend
    > kind;
};

// === Top-level Declarations ===

/// @brief Struct/variant field definition
struct Field {
    std::string name;  ///< Field name
    TypePtr type;      ///< Field type
    bool optional;     ///< True if field is optional (T?)
};

/// @brief Function parameter definition
struct Param {
    std::string name;  ///< Parameter name
    TypePtr type;      ///< Parameter type (nullptr for self)
};

/// @brief Function declaration
struct FnDecl {
    SourceLoc loc;                           ///< Source location
    std::string name;                        ///< Function name
    std::vector<std::string> type_params;    ///< Generic type parameters
    std::vector<Param> params;               ///< Function parameters
    TypePtr return_type;                     ///< Return type (nullptr for void)
    std::vector<StmtPtr> body;               ///< Function body
};

/// @brief Struct type declaration
struct StructDecl {
    SourceLoc loc;                           ///< Source location
    std::string name;                        ///< Struct name
    std::vector<std::string> type_params;    ///< Generic type parameters
    std::vector<Field> fields;               ///< Struct fields
};

/// @brief Variant (sum type) declaration
struct VariantDecl {
    SourceLoc loc;                           ///< Source location
    std::string name;                        ///< Variant name
    std::vector<std::string> type_params;    ///< Generic type parameters

    /// @brief A case in the variant
    struct Case {
        std::string name;          ///< Case name
        std::vector<Field> fields;  ///< Case fields
    };
    std::vector<Case> cases;  ///< Variant cases
};

/// @brief Trait declaration (interface definition)
struct TraitDecl {
    SourceLoc loc;       ///< Source location
    std::string name;    ///< Trait name

    /// @brief Method signature in the trait
    struct Method {
        std::string name;          ///< Method name
        std::vector<Param> params;  ///< Method parameters
        TypePtr return_type;        ///< Return type
    };
    std::vector<Method> methods;  ///< Trait methods
};

/// @brief Implementation block (methods or trait impl)
struct ImplDecl {
    SourceLoc loc;                           ///< Source location
    std::string type_name;                   ///< Type being extended
    std::vector<std::string> type_params;    ///< Generic type parameters
    std::string trait_name;                  ///< Trait being implemented (empty for plain methods)
    std::vector<FnDecl> methods;             ///< Method implementations
};

/// @brief Error type declaration
struct ErrorDecl {
    SourceLoc loc;              ///< Source location
    std::string name;           ///< Error type name
    std::vector<Field> fields;  ///< Error fields
};

/// @brief Import declaration
struct UseDecl {
    SourceLoc loc;                     ///< Source location
    std::vector<std::string> modules;  ///< Modules to import
};

// === Program ===

/// @brief Union type for all top-level declarations
using Decl = std::variant<FnDecl, StructDecl, VariantDecl, TraitDecl, ImplDecl, ErrorDecl, UseDecl>;

/// @brief Root AST node representing an entire program
struct Program {
    std::vector<Decl> decls;  ///< All top-level declarations
};

} // namespace shotgun
