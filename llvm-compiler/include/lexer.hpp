#pragma once

#include "token.hpp"
#include <string>
#include <vector>

namespace shotgun {

class Lexer {
public:
    Lexer(const std::string& source, const std::string& filename = "<input>");

    Token next_token();
    std::vector<Token> tokenize();

private:
    char peek() const;
    char peek_next() const;
    char advance();
    bool at_end() const;
    bool match(char expected);

    void skip_whitespace();
    void skip_line_comment();

    Token make_token(TokenKind kind);
    Token make_token(TokenKind kind, const std::string& lexeme);
    Token error_token(const std::string& message);

    Token scan_identifier();
    Token scan_number();
    Token scan_string();
    Token scan_fstring();
    Token scan_char();

    TokenKind check_keyword(const std::string& ident);

    std::string source_;
    std::string filename_;
    size_t start_ = 0;
    size_t current_ = 0;
    int line_ = 1;
    int column_ = 1;
    int start_column_ = 1;
};

} // namespace shotgun
