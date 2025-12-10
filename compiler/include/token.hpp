/**
 * @file token.hpp
 * @brief Token definitions for the Shotgun language lexer.
 *
 * This file defines the token types and structures used by the lexer
 * to represent the lexical elements of Shotgun source code.
 */

#pragma once

#include <string>
#include <variant>
#include <cstdint>

namespace shotgun {

/**
 * @brief Enumeration of all token types in the Shotgun language.
 *
 * Tokens are categorized into:
 * - Literals (Int, Float, String, FString, Char)
 * - Identifiers
 * - Keywords (fn, struct, if, else, etc.)
 * - Type keywords (int, str, bool, etc.)
 * - Operators (+, -, *, /, ==, etc.)
 * - Delimiters ({, }, (, ), [, ])
 * - Special tokens (Newline, Eof, Invalid)
 */
enum class TokenKind {
    // Literals
    Int,
    Float,
    String,
    FString,    // f"..." with interpolation
    Char,

    // Identifier
    Ident,

    // Keywords
    Fn,
    Struct,
    Variant,
    Trait,
    Impl,
    Error,
    Return,
    Break,
    Continue,
    If,
    Else,
    For,
    While,
    In,
    Is,
    Match,
    Go,
    Chan,
    Wait,
    Or,
    And,
    Not,
    Self,
    None,
    True,
    False,
    Const,
    Uses,
    Using,
    As,

    // Type keywords
    TInt,
    TStr,
    TBool,
    TChar,
    TF32,
    TF64,
    TU32,
    TU64,
    Map,

    // Operators
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    Eq,             // ==
    NotEq,          // !=
    Lt,             // <
    Gt,             // >
    LtEq,           // <=
    GtEq,           // >=
    AndAnd,         // &&
    OrOr,           // ||
    Bang,           // !
    Assign,         // =
    PlusEq,         // +=
    MinusEq,        // -=
    StarEq,         // *=
    SlashEq,        // /=
    ColonEq,        // :=
    ColonColon,     // ::
    Arrow,          // ->
    Dot,            // .
    Comma,          // ,
    Colon,          // :
    Semicolon,      // ;
    Question,       // ?

    // Delimiters
    LBrace,         // {
    RBrace,         // }
    LParen,         // (
    RParen,         // )
    LBracket,       // [
    RBracket,       // ]

    // Special
    Newline,
    Eof,
    Invalid,
};

/**
 * @brief Represents a location in source code.
 *
 * Used for error reporting and debugging to pinpoint where
 * tokens and AST nodes originate in the source file.
 */
struct SourceLoc {
    std::string file;  ///< Source file path
    int line;          ///< Line number (1-based)
    int column;        ///< Column number (1-based)
};

/**
 * @brief Represents a lexical token from source code.
 *
 * Tokens are produced by the Lexer and consumed by the Parser.
 * Each token contains its type, the original text (lexeme),
 * source location, and optionally a parsed literal value.
 */
struct Token {
    TokenKind kind;      ///< The type of this token
    std::string lexeme;  ///< The original text from source
    SourceLoc loc;       ///< Location in source file

    /**
     * @brief Parsed literal value for numeric, string, and char tokens.
     *
     * - monostate: No value (for keywords, operators, etc.)
     * - int64_t: Integer literal value
     * - double: Floating-point literal value
     * - std::string: String literal content (escape sequences processed)
     * - char: Character literal value
     */
    std::variant<std::monostate, int64_t, double, std::string, char> value;
};

/**
 * @brief Returns a human-readable name for a token kind.
 * @param kind The token kind to name.
 * @return A C-string with the token name (e.g., "Int", "Plus", "Fn").
 */
const char* token_kind_name(TokenKind kind);

} // namespace shotgun
