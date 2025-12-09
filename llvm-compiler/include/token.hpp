#pragma once

#include <string>
#include <variant>
#include <cstdint>

namespace shotgun {

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

struct SourceLoc {
    std::string file;
    int line;
    int column;
};

struct Token {
    TokenKind kind;
    std::string lexeme;
    SourceLoc loc;

    // Literal values
    std::variant<std::monostate, int64_t, double, std::string, char> value;
};

const char* token_kind_name(TokenKind kind);

} // namespace shotgun
