#include "lexer.hpp"
#include <iostream>
#include <vector>
#include <stdexcept>

using namespace shotgun;

int tests_run = 0;
int tests_passed = 0;

void expect_eq(TokenKind expected, TokenKind actual, const char* msg) {
    if (expected != actual) {
        throw std::runtime_error(std::string(msg) + ": expected " +
            token_kind_name(expected) + ", got " + token_kind_name(actual));
    }
}

void expect_eq(const std::string& expected, const std::string& actual, const char* msg) {
    if (expected != actual) {
        throw std::runtime_error(std::string(msg) + ": expected '" + expected + "', got '" + actual + "'");
    }
}

void expect_eq(int64_t expected, int64_t actual, const char* msg) {
    if (expected != actual) {
        throw std::runtime_error(std::string(msg) + ": expected " +
            std::to_string(expected) + ", got " + std::to_string(actual));
    }
}

void expect_eq(int expected, int actual, const char* msg) {
    if (expected != actual) {
        throw std::runtime_error(std::string(msg) + ": expected " +
            std::to_string(expected) + ", got " + std::to_string(actual));
    }
}

void expect_eq(char expected, char actual, const char* msg) {
    if (expected != actual) {
        throw std::runtime_error(std::string(msg) + ": expected '" +
            std::string(1, expected) + "', got '" + std::string(1, actual) + "'");
    }
}

#define RUN_TEST(name) do { \
    tests_run++; \
    try { \
        name(); \
        tests_passed++; \
        std::cout << "  PASS: " << #name << "\n"; \
    } catch (const std::exception& e) { \
        std::cout << "  FAIL: " << #name << " - " << e.what() << "\n"; \
    } \
} while(0)

// === Tests ===

void test_empty_input() {
    Lexer lexer("");
    auto tok = lexer.next_token();
    expect_eq(TokenKind::Eof, tok.kind, "empty input should produce EOF");
}

void test_keywords() {
    Lexer lexer("fn struct variant trait impl error return break continue if else for while in is match go chan wait or self none true false const uses using as");

    std::vector<TokenKind> expected = {
        TokenKind::Fn, TokenKind::Struct, TokenKind::Variant, TokenKind::Trait,
        TokenKind::Impl, TokenKind::Error, TokenKind::Return, TokenKind::Break,
        TokenKind::Continue, TokenKind::If, TokenKind::Else, TokenKind::For,
        TokenKind::While, TokenKind::In, TokenKind::Is, TokenKind::Match,
        TokenKind::Go, TokenKind::Chan, TokenKind::Wait, TokenKind::Or,
        TokenKind::Self, TokenKind::None, TokenKind::True, TokenKind::False,
        TokenKind::Const, TokenKind::Uses, TokenKind::Using, TokenKind::As,
        TokenKind::Eof
    };

    for (auto exp : expected) {
        auto tok = lexer.next_token();
        expect_eq(exp, tok.kind, "keyword mismatch");
    }
}

void test_type_keywords() {
    Lexer lexer("int str bool char f32 f64 u32 u64 Map");

    std::vector<TokenKind> expected = {
        TokenKind::TInt, TokenKind::TStr, TokenKind::TBool, TokenKind::TChar,
        TokenKind::TF32, TokenKind::TF64, TokenKind::TU32, TokenKind::TU64,
        TokenKind::Map, TokenKind::Eof
    };

    for (auto exp : expected) {
        auto tok = lexer.next_token();
        expect_eq(exp, tok.kind, "type keyword mismatch");
    }
}

void test_identifiers() {
    Lexer lexer("foo bar_baz _private camelCase PascalCase x123");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "identifier");
    expect_eq(std::string("foo"), tok.lexeme, "lexeme");

    tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "identifier with underscore");
    expect_eq(std::string("bar_baz"), tok.lexeme, "lexeme");

    tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "private identifier");
    expect_eq(std::string("_private"), tok.lexeme, "lexeme");
}

void test_integers() {
    Lexer lexer("0 42 123456");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::Int, tok.kind, "zero");
    expect_eq(int64_t(0), std::get<int64_t>(tok.value), "zero value");

    tok = lexer.next_token();
    expect_eq(TokenKind::Int, tok.kind, "42");
    expect_eq(int64_t(42), std::get<int64_t>(tok.value), "42 value");

    tok = lexer.next_token();
    expect_eq(TokenKind::Int, tok.kind, "large int");
    expect_eq(int64_t(123456), std::get<int64_t>(tok.value), "large int value");
}

void test_floats() {
    Lexer lexer("3.14 0.5 123.456");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::Float, tok.kind, "pi");

    tok = lexer.next_token();
    expect_eq(TokenKind::Float, tok.kind, "0.5");

    tok = lexer.next_token();
    expect_eq(TokenKind::Float, tok.kind, "123.456");
}

void test_strings() {
    Lexer lexer(R"("hello" "world" "with spaces")");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::String, tok.kind, "string");
    expect_eq(std::string("hello"), std::get<std::string>(tok.value), "string value");

    tok = lexer.next_token();
    expect_eq(TokenKind::String, tok.kind, "string");
    expect_eq(std::string("world"), std::get<std::string>(tok.value), "string value");
}

void test_string_escapes() {
    Lexer lexer(R"("hello\nworld" "tab\there" "quote\"here" "brace\{x\}")");

    auto tok = lexer.next_token();
    expect_eq(std::string("hello\nworld"), std::get<std::string>(tok.value), "newline escape");

    tok = lexer.next_token();
    expect_eq(std::string("tab\there"), std::get<std::string>(tok.value), "tab escape");

    tok = lexer.next_token();
    expect_eq(std::string("quote\"here"), std::get<std::string>(tok.value), "quote escape");

    tok = lexer.next_token();
    expect_eq(std::string("brace{x}"), std::get<std::string>(tok.value), "brace escape");
}

void test_chars() {
    Lexer lexer("'a' 'Z' '\\n' '\\t'");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::Char, tok.kind, "char a");
    expect_eq('a', std::get<char>(tok.value), "char a value");

    tok = lexer.next_token();
    expect_eq(TokenKind::Char, tok.kind, "char Z");
    expect_eq('Z', std::get<char>(tok.value), "char Z value");

    tok = lexer.next_token();
    expect_eq(TokenKind::Char, tok.kind, "newline char");
    expect_eq('\n', std::get<char>(tok.value), "newline char value");

    tok = lexer.next_token();
    expect_eq(TokenKind::Char, tok.kind, "tab char");
    expect_eq('\t', std::get<char>(tok.value), "tab char value");
}

void test_operators() {
    Lexer lexer("+ - * / % == != < > <= >= && || ! = += -= *= /= := :: -> . , : ; ?");

    std::vector<TokenKind> expected = {
        TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash,
        TokenKind::Percent, TokenKind::Eq, TokenKind::NotEq, TokenKind::Lt,
        TokenKind::Gt, TokenKind::LtEq, TokenKind::GtEq, TokenKind::AndAnd,
        TokenKind::OrOr, TokenKind::Bang, TokenKind::Assign, TokenKind::PlusEq,
        TokenKind::MinusEq, TokenKind::StarEq, TokenKind::SlashEq, TokenKind::ColonEq,
        TokenKind::ColonColon, TokenKind::Arrow, TokenKind::Dot, TokenKind::Comma,
        TokenKind::Colon, TokenKind::Semicolon, TokenKind::Question, TokenKind::Eof
    };

    for (auto exp : expected) {
        auto tok = lexer.next_token();
        expect_eq(exp, tok.kind, "operator mismatch");
    }
}

void test_delimiters() {
    Lexer lexer("{ } ( ) [ ]");

    std::vector<TokenKind> expected = {
        TokenKind::LBrace, TokenKind::RBrace, TokenKind::LParen, TokenKind::RParen,
        TokenKind::LBracket, TokenKind::RBracket, TokenKind::Eof
    };

    for (auto exp : expected) {
        auto tok = lexer.next_token();
        expect_eq(exp, tok.kind, "delimiter mismatch");
    }
}

void test_newlines() {
    Lexer lexer("a\nb\nc");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "a");

    tok = lexer.next_token();
    expect_eq(TokenKind::Newline, tok.kind, "newline 1");

    tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "b");

    tok = lexer.next_token();
    expect_eq(TokenKind::Newline, tok.kind, "newline 2");

    tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "c");
}

void test_comments() {
    Lexer lexer("a // this is a comment\nb");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "a");

    tok = lexer.next_token();
    expect_eq(TokenKind::Newline, tok.kind, "newline after comment");

    tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "b");
}

void test_source_locations() {
    Lexer lexer("a\nbc");

    auto tok = lexer.next_token();
    expect_eq(1, tok.loc.line, "line 1");
    expect_eq(1, tok.loc.column, "column 1");

    tok = lexer.next_token(); // newline
    tok = lexer.next_token(); // bc
    expect_eq(2, tok.loc.line, "line 2");
    expect_eq(1, tok.loc.column, "column 1 on line 2");
}

void test_function_declaration() {
    Lexer lexer("fn add(int a, int b) int { return a + b }");

    std::vector<TokenKind> expected = {
        TokenKind::Fn, TokenKind::Ident, TokenKind::LParen,
        TokenKind::TInt, TokenKind::Ident, TokenKind::Comma,
        TokenKind::TInt, TokenKind::Ident, TokenKind::RParen,
        TokenKind::TInt, TokenKind::LBrace, TokenKind::Return,
        TokenKind::Ident, TokenKind::Plus, TokenKind::Ident,
        TokenKind::RBrace, TokenKind::Eof
    };

    for (auto exp : expected) {
        auto tok = lexer.next_token();
        expect_eq(exp, tok.kind, "function declaration token");
    }
}

void test_struct_declaration() {
    Lexer lexer("Person :: struct { name str }");

    std::vector<TokenKind> expected = {
        TokenKind::Ident, TokenKind::ColonColon, TokenKind::Struct,
        TokenKind::LBrace, TokenKind::Ident, TokenKind::TStr,
        TokenKind::RBrace, TokenKind::Eof
    };

    for (auto exp : expected) {
        auto tok = lexer.next_token();
        expect_eq(exp, tok.kind, "struct declaration token");
    }
}

void test_variable_inference() {
    Lexer lexer("x := 42");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::Ident, tok.kind, "x");

    tok = lexer.next_token();
    expect_eq(TokenKind::ColonEq, tok.kind, ":=");

    tok = lexer.next_token();
    expect_eq(TokenKind::Int, tok.kind, "42");
    expect_eq(int64_t(42), std::get<int64_t>(tok.value), "42 value");
}

void test_invalid_token() {
    Lexer lexer("@");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::Invalid, tok.kind, "invalid token");
}

void test_and_not_keywords() {
    Lexer lexer("and not");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::And, tok.kind, "and keyword");

    tok = lexer.next_token();
    expect_eq(TokenKind::Not, tok.kind, "not keyword");
}

void test_fstring() {
    Lexer lexer("f\"Hello {name}!\"");

    auto tok = lexer.next_token();
    expect_eq(TokenKind::FString, tok.kind, "fstring token");
    expect_eq(std::string("Hello {name}!"), std::get<std::string>(tok.value), "fstring value");
}

int main() {
    std::cout << "Running lexer tests...\n";

    RUN_TEST(test_empty_input);
    RUN_TEST(test_keywords);
    RUN_TEST(test_type_keywords);
    RUN_TEST(test_identifiers);
    RUN_TEST(test_integers);
    RUN_TEST(test_floats);
    RUN_TEST(test_strings);
    RUN_TEST(test_string_escapes);
    RUN_TEST(test_chars);
    RUN_TEST(test_operators);
    RUN_TEST(test_delimiters);
    RUN_TEST(test_newlines);
    RUN_TEST(test_comments);
    RUN_TEST(test_source_locations);
    RUN_TEST(test_function_declaration);
    RUN_TEST(test_struct_declaration);
    RUN_TEST(test_variable_inference);
    RUN_TEST(test_invalid_token);
    RUN_TEST(test_and_not_keywords);
    RUN_TEST(test_fstring);

    std::cout << "\n" << tests_passed << "/" << tests_run << " tests passed\n";
    return tests_passed == tests_run ? 0 : 1;
}
