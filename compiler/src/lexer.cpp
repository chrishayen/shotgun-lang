#include "lexer.hpp"
#include <unordered_map>
#include <cctype>

namespace shotgun {

static const std::unordered_map<std::string, TokenKind> keywords = {
    {"fn", TokenKind::Fn},
    {"struct", TokenKind::Struct},
    {"variant", TokenKind::Variant},
    {"trait", TokenKind::Trait},
    {"impl", TokenKind::Impl},
    {"error", TokenKind::Error},
    {"return", TokenKind::Return},
    {"break", TokenKind::Break},
    {"continue", TokenKind::Continue},
    {"if", TokenKind::If},
    {"else", TokenKind::Else},
    {"for", TokenKind::For},
    {"while", TokenKind::While},
    {"in", TokenKind::In},
    {"is", TokenKind::Is},
    {"match", TokenKind::Match},
    {"go", TokenKind::Go},
    {"chan", TokenKind::Chan},
    {"wait", TokenKind::Wait},
    {"or", TokenKind::Or},
    {"and", TokenKind::And},
    {"not", TokenKind::Not},
    {"self", TokenKind::Self},
    {"none", TokenKind::None},
    {"true", TokenKind::True},
    {"false", TokenKind::False},
    {"const", TokenKind::Const},
    {"uses", TokenKind::Uses},
    {"using", TokenKind::Using},
    {"as", TokenKind::As},
    // Type keywords
    {"int", TokenKind::TInt},
    {"str", TokenKind::TStr},
    {"bool", TokenKind::TBool},
    {"char", TokenKind::TChar},
    {"f32", TokenKind::TF32},
    {"f64", TokenKind::TF64},
    {"u32", TokenKind::TU32},
    {"u64", TokenKind::TU64},
    {"Map", TokenKind::Map},
};

Lexer::Lexer(const std::string& source, const std::string& filename)
    : source_(source), filename_(filename) {}

char Lexer::peek() const {
    if (at_end()) return '\0';
    return source_[current_];
}

char Lexer::peek_next() const {
    if (current_ + 1 >= source_.size()) return '\0';
    return source_[current_ + 1];
}

char Lexer::advance() {
    char c = source_[current_++];
    if (c == '\n') {
        line_++;
        column_ = 1;
    } else {
        column_++;
    }
    return c;
}

bool Lexer::at_end() const {
    return current_ >= source_.size();
}

bool Lexer::match(char expected) {
    if (at_end() || source_[current_] != expected) return false;
    advance();
    return true;
}

void Lexer::skip_whitespace() {
    while (!at_end()) {
        char c = peek();
        switch (c) {
            case ' ':
            case '\t':
            case '\r':
                advance();
                break;
            case '\n':
                return; // newlines are significant
            case '/':
                if (peek_next() == '/') {
                    skip_line_comment();
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

void Lexer::skip_line_comment() {
    while (!at_end() && peek() != '\n') {
        advance();
    }
}

Token Lexer::make_token(TokenKind kind) {
    std::string lexeme = source_.substr(start_, current_ - start_);
    return Token{kind, lexeme, {filename_, line_, start_column_}, {}};
}

Token Lexer::make_token(TokenKind kind, const std::string& lexeme) {
    return Token{kind, lexeme, {filename_, line_, start_column_}, {}};
}

Token Lexer::error_token(const std::string& message) {
    return Token{TokenKind::Invalid, message, {filename_, line_, start_column_}, {}};
}

TokenKind Lexer::check_keyword(const std::string& ident) {
    auto it = keywords.find(ident);
    if (it != keywords.end()) {
        return it->second;
    }
    return TokenKind::Ident;
}

Token Lexer::scan_identifier() {
    while (!at_end() && (std::isalnum(peek()) || peek() == '_')) {
        advance();
    }
    std::string ident = source_.substr(start_, current_ - start_);

    // Check for f-string: f"..."
    if (ident == "f" && peek() == '"') {
        advance(); // consume "
        return scan_fstring();
    }

    TokenKind kind = check_keyword(ident);
    Token tok = make_token(kind);
    if (kind == TokenKind::True) {
        tok.value = int64_t(1);
    } else if (kind == TokenKind::False) {
        tok.value = int64_t(0);
    }
    return tok;
}

Token Lexer::scan_fstring() {
    // Scan f-string content, keeping track of {} for interpolation
    // Returns the raw content (including {expr}) as the value
    std::string value;
    int brace_depth = 0;

    while (!at_end() && (peek() != '"' || brace_depth > 0)) {
        if (peek() == '\n' && brace_depth == 0) {
            return error_token("Unterminated f-string");
        }
        if (peek() == '\\') {
            advance(); // consume backslash
            if (at_end()) {
                return error_token("Unterminated escape sequence");
            }
            char escaped = advance();
            switch (escaped) {
                case 'n': value += '\n'; break;
                case 't': value += '\t'; break;
                case 'r': value += '\r'; break;
                case '\\': value += '\\'; break;
                case '"': value += '"'; break;
                case '{': value += "\\{"; break;  // escaped brace
                case '}': value += "\\}"; break;  // escaped brace
                default:
                    return error_token("Unknown escape sequence");
            }
        } else if (peek() == '{') {
            brace_depth++;
            value += advance();
        } else if (peek() == '}') {
            brace_depth--;
            if (brace_depth < 0) {
                return error_token("Unmatched '}' in f-string");
            }
            value += advance();
        } else {
            value += advance();
        }
    }

    if (at_end()) {
        return error_token("Unterminated f-string");
    }

    advance(); // closing "
    Token tok = make_token(TokenKind::FString);
    tok.value = value;
    return tok;
}

Token Lexer::scan_number() {
    bool is_float = false;

    while (!at_end() && std::isdigit(peek())) {
        advance();
    }

    // Check for decimal
    if (peek() == '.' && std::isdigit(peek_next())) {
        is_float = true;
        advance(); // consume '.'
        while (!at_end() && std::isdigit(peek())) {
            advance();
        }
    }

    std::string num_str = source_.substr(start_, current_ - start_);
    Token tok = make_token(is_float ? TokenKind::Float : TokenKind::Int);

    if (is_float) {
        tok.value = std::stod(num_str);
    } else {
        tok.value = std::stoll(num_str);
    }

    return tok;
}

Token Lexer::scan_string() {
    std::string value;

    while (!at_end() && peek() != '"') {
        if (peek() == '\n') {
            return error_token("Unterminated string");
        }
        if (peek() == '\\') {
            advance(); // consume backslash
            if (at_end()) {
                return error_token("Unterminated escape sequence");
            }
            char escaped = advance();
            switch (escaped) {
                case 'n': value += '\n'; break;
                case 't': value += '\t'; break;
                case 'r': value += '\r'; break;
                case '\\': value += '\\'; break;
                case '"': value += '"'; break;
                case '{': value += '{'; break;
                case '}': value += '}'; break;
                default:
                    return error_token("Unknown escape sequence");
            }
        } else {
            value += advance();
        }
    }

    if (at_end()) {
        return error_token("Unterminated string");
    }

    advance(); // closing "
    Token tok = make_token(TokenKind::String);
    tok.value = value;
    return tok;
}

Token Lexer::scan_char() {
    if (at_end()) {
        return error_token("Unterminated character literal");
    }

    char value;
    if (peek() == '\\') {
        advance();
        if (at_end()) {
            return error_token("Unterminated escape sequence");
        }
        char escaped = advance();
        switch (escaped) {
            case 'n': value = '\n'; break;
            case 't': value = '\t'; break;
            case 'r': value = '\r'; break;
            case '\\': value = '\\'; break;
            case '\'': value = '\''; break;
            default:
                return error_token("Unknown escape sequence");
        }
    } else {
        value = advance();
    }

    if (at_end() || peek() != '\'') {
        return error_token("Unterminated character literal");
    }
    advance(); // closing '

    Token tok = make_token(TokenKind::Char);
    tok.value = value;
    return tok;
}

Token Lexer::next_token() {
    skip_whitespace();

    start_ = current_;
    start_column_ = column_;

    if (at_end()) {
        return make_token(TokenKind::Eof, "");
    }

    char c = advance();

    // Identifiers and keywords
    if (std::isalpha(c) || c == '_') {
        return scan_identifier();
    }

    // Numbers
    if (std::isdigit(c)) {
        return scan_number();
    }

    switch (c) {
        case '\n': return make_token(TokenKind::Newline);

        case '(': return make_token(TokenKind::LParen);
        case ')': return make_token(TokenKind::RParen);
        case '{': return make_token(TokenKind::LBrace);
        case '}': return make_token(TokenKind::RBrace);
        case '[': return make_token(TokenKind::LBracket);
        case ']': return make_token(TokenKind::RBracket);
        case ',': return make_token(TokenKind::Comma);
        case '.': return make_token(TokenKind::Dot);
        case ';': return make_token(TokenKind::Semicolon);
        case '?': return make_token(TokenKind::Question);

        case '+':
            return make_token(match('=') ? TokenKind::PlusEq : TokenKind::Plus);
        case '-':
            if (match('>')) return make_token(TokenKind::Arrow);
            return make_token(match('=') ? TokenKind::MinusEq : TokenKind::Minus);
        case '*':
            return make_token(match('=') ? TokenKind::StarEq : TokenKind::Star);
        case '/':
            return make_token(match('=') ? TokenKind::SlashEq : TokenKind::Slash);
        case '%':
            return make_token(TokenKind::Percent);

        case '=':
            return make_token(match('=') ? TokenKind::Eq : TokenKind::Assign);
        case '!':
            return make_token(match('=') ? TokenKind::NotEq : TokenKind::Bang);
        case '<':
            return make_token(match('=') ? TokenKind::LtEq : TokenKind::Lt);
        case '>':
            return make_token(match('=') ? TokenKind::GtEq : TokenKind::Gt);

        case '&':
            if (match('&')) return make_token(TokenKind::AndAnd);
            return error_token("Expected '&&'");
        case '|':
            if (match('|')) return make_token(TokenKind::OrOr);
            return error_token("Expected '||'");

        case ':':
            if (match(':')) return make_token(TokenKind::ColonColon);
            if (match('=')) return make_token(TokenKind::ColonEq);
            return make_token(TokenKind::Colon);

        case '"': return scan_string();
        case '\'': return scan_char();

        default:
            return error_token("Unexpected character");
    }
}

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;
    while (true) {
        Token tok = next_token();
        tokens.push_back(tok);
        if (tok.kind == TokenKind::Eof) break;
    }
    return tokens;
}

const char* token_kind_name(TokenKind kind) {
    switch (kind) {
        case TokenKind::Int: return "Int";
        case TokenKind::Float: return "Float";
        case TokenKind::String: return "String";
        case TokenKind::FString: return "FString";
        case TokenKind::Char: return "Char";
        case TokenKind::Ident: return "Ident";
        case TokenKind::Fn: return "Fn";
        case TokenKind::Struct: return "Struct";
        case TokenKind::Variant: return "Variant";
        case TokenKind::Trait: return "Trait";
        case TokenKind::Impl: return "Impl";
        case TokenKind::Error: return "Error";
        case TokenKind::Return: return "Return";
        case TokenKind::Break: return "Break";
        case TokenKind::Continue: return "Continue";
        case TokenKind::If: return "If";
        case TokenKind::Else: return "Else";
        case TokenKind::For: return "For";
        case TokenKind::While: return "While";
        case TokenKind::In: return "In";
        case TokenKind::Is: return "Is";
        case TokenKind::Match: return "Match";
        case TokenKind::Go: return "Go";
        case TokenKind::Chan: return "Chan";
        case TokenKind::Wait: return "Wait";
        case TokenKind::Or: return "Or";
        case TokenKind::And: return "And";
        case TokenKind::Not: return "Not";
        case TokenKind::Self: return "Self";
        case TokenKind::None: return "None";
        case TokenKind::True: return "True";
        case TokenKind::False: return "False";
        case TokenKind::Const: return "Const";
        case TokenKind::Uses: return "Uses";
        case TokenKind::Using: return "Using";
        case TokenKind::As: return "As";
        case TokenKind::TInt: return "TInt";
        case TokenKind::TStr: return "TStr";
        case TokenKind::TBool: return "TBool";
        case TokenKind::TChar: return "TChar";
        case TokenKind::TF32: return "TF32";
        case TokenKind::TF64: return "TF64";
        case TokenKind::TU32: return "TU32";
        case TokenKind::TU64: return "TU64";
        case TokenKind::Map: return "Map";
        case TokenKind::Plus: return "Plus";
        case TokenKind::Minus: return "Minus";
        case TokenKind::Star: return "Star";
        case TokenKind::Slash: return "Slash";
        case TokenKind::Percent: return "Percent";
        case TokenKind::Eq: return "Eq";
        case TokenKind::NotEq: return "NotEq";
        case TokenKind::Lt: return "Lt";
        case TokenKind::Gt: return "Gt";
        case TokenKind::LtEq: return "LtEq";
        case TokenKind::GtEq: return "GtEq";
        case TokenKind::AndAnd: return "AndAnd";
        case TokenKind::OrOr: return "OrOr";
        case TokenKind::Bang: return "Bang";
        case TokenKind::Assign: return "Assign";
        case TokenKind::PlusEq: return "PlusEq";
        case TokenKind::MinusEq: return "MinusEq";
        case TokenKind::StarEq: return "StarEq";
        case TokenKind::SlashEq: return "SlashEq";
        case TokenKind::ColonEq: return "ColonEq";
        case TokenKind::ColonColon: return "ColonColon";
        case TokenKind::Arrow: return "Arrow";
        case TokenKind::Dot: return "Dot";
        case TokenKind::Comma: return "Comma";
        case TokenKind::Colon: return "Colon";
        case TokenKind::Semicolon: return "Semicolon";
        case TokenKind::Question: return "Question";
        case TokenKind::LBrace: return "LBrace";
        case TokenKind::RBrace: return "RBrace";
        case TokenKind::LParen: return "LParen";
        case TokenKind::RParen: return "RParen";
        case TokenKind::LBracket: return "LBracket";
        case TokenKind::RBracket: return "RBracket";
        case TokenKind::Newline: return "Newline";
        case TokenKind::Eof: return "Eof";
        case TokenKind::Invalid: return "Invalid";
    }
    return "Unknown";
}

} // namespace shotgun
