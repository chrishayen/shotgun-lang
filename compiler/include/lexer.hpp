/**
 * @file lexer.hpp
 * @brief Lexical analyzer for the Shotgun language.
 *
 * The Lexer converts source code into a stream of tokens that can be
 * consumed by the Parser. It handles keywords, operators, literals,
 * and whitespace/comment skipping.
 */

#pragma once

#include "token.hpp"
#include <string>
#include <vector>

namespace shotgun {

/**
 * @brief Lexical analyzer that tokenizes Shotgun source code.
 *
 * The Lexer scans source code character by character and produces
 * tokens representing language constructs. It tracks source locations
 * for error reporting and handles escape sequences in strings.
 *
 * Usage:
 * @code
 * Lexer lexer(source_code, "filename.bs");
 * std::vector<Token> tokens = lexer.tokenize();
 * @endcode
 */
class Lexer {
public:
    /**
     * @brief Constructs a lexer for the given source code.
     * @param source The source code to tokenize.
     * @param filename The filename for error reporting (default: "<input>").
     */
    Lexer(const std::string& source, const std::string& filename = "<input>");

    /**
     * @brief Scans and returns the next token from the source.
     * @return The next token, or an Eof token at end of input.
     */
    Token next_token();

    /**
     * @brief Tokenizes the entire source and returns all tokens.
     * @return A vector of all tokens, ending with an Eof token.
     */
    std::vector<Token> tokenize();

private:
    /// @name Character Navigation
    /// @{
    char peek() const;       ///< Returns current character without advancing.
    char peek_next() const;  ///< Returns next character without advancing.
    char advance();          ///< Advances and returns current character.
    bool at_end() const;     ///< Returns true if at end of source.
    bool match(char expected);  ///< Advances if current char matches expected.
    /// @}

    /// @name Whitespace and Comments
    /// @{
    void skip_whitespace();    ///< Skips spaces, tabs (but not newlines).
    void skip_line_comment();  ///< Skips from // to end of line.
    /// @}

    /// @name Token Construction
    /// @{
    Token make_token(TokenKind kind);  ///< Creates token with current lexeme.
    Token make_token(TokenKind kind, const std::string& lexeme);  ///< Creates token with custom lexeme.
    Token error_token(const std::string& message);  ///< Creates an Invalid token with error message.
    /// @}

    /// @name Scanning Methods
    /// @{
    Token scan_identifier();  ///< Scans identifier or keyword.
    Token scan_number();      ///< Scans integer or float literal.
    Token scan_string();      ///< Scans string literal with escape sequences.
    Token scan_fstring();     ///< Scans f-string with interpolation.
    Token scan_char();        ///< Scans character literal.
    /// @}

    /**
     * @brief Checks if an identifier is a reserved keyword.
     * @param ident The identifier to check.
     * @return The keyword TokenKind, or Ident if not a keyword.
     */
    TokenKind check_keyword(const std::string& ident);

    std::string source_;      ///< Source code being tokenized
    std::string filename_;    ///< Filename for error reporting
    size_t start_ = 0;        ///< Start position of current token
    size_t current_ = 0;      ///< Current position in source
    int line_ = 1;            ///< Current line number (1-based)
    int column_ = 1;          ///< Current column number (1-based)
    int start_column_ = 1;    ///< Column where current token started
};

} // namespace shotgun
