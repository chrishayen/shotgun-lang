#include "lexer.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include <iostream>
#include <stdexcept>

using namespace shotgun;

int tests_run = 0;
int tests_passed = 0;

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

Program parse(const std::string& code) {
    Lexer lexer(code);
    Parser parser(lexer);
    auto prog = parser.parse();
    if (parser.has_errors()) {
        std::string msg = "Parse errors:";
        for (const auto& e : parser.errors()) {
            msg += "\n  " + e;
        }
        throw std::runtime_error(msg);
    }
    return prog;
}

void expect_ok(const std::string& code) {
    auto prog = parse(code);
    Sema sema;
    if (!sema.analyze(prog)) {
        std::string msg = "Sema errors:";
        for (const auto& e : sema.errors()) {
            msg += "\n  " + e;
        }
        throw std::runtime_error(msg);
    }
}

void expect_error(const std::string& code, const std::string& expected_substr = "") {
    auto prog = parse(code);
    Sema sema;
    if (sema.analyze(prog)) {
        throw std::runtime_error("Expected error but analysis succeeded");
    }
    if (!expected_substr.empty()) {
        bool found = false;
        for (const auto& e : sema.errors()) {
            if (e.find(expected_substr) != std::string::npos) {
                found = true;
                break;
            }
        }
        if (!found) {
            throw std::runtime_error("Expected error containing '" + expected_substr + "'");
        }
    }
}

// === Tests ===

void test_empty_program() {
    expect_ok("");
}

void test_simple_function() {
    expect_ok("fn main() { }");
}

void test_function_with_return() {
    expect_ok("fn add(int a, int b) int { return a + b }");
}

void test_var_decl_explicit() {
    expect_ok("fn main() { int x = 42 }");
}

void test_var_decl_inferred() {
    expect_ok("fn main() { x := 42 }");
}

void test_var_type_mismatch() {
    expect_error("fn main() { int x = \"hello\" }", "Cannot assign");
}

void test_undefined_var() {
    expect_error("fn main() { x = 5 }", "Undefined variable");
}

void test_const_reassign() {
    expect_error("fn main() {\n  const x = 5\n  x = 10\n}", "constant");
}

void test_binary_ops() {
    expect_ok("fn main() { x := 1 + 2 * 3 }");
}

void test_comparison_ops() {
    expect_ok("fn main() { b := 1 < 2 }");
}

void test_logical_ops() {
    expect_ok("fn main() { b := true && false }");
}

void test_if_stmt() {
    expect_ok("fn main() { if true { x := 1 } }");
}

void test_if_non_bool() {
    expect_error("fn main() { if 42 { } }", "Condition must be bool");
}

void test_while_stmt() {
    expect_ok("fn main() { while true { break } }");
}

void test_for_array() {
    expect_ok("fn main() {\n  nums := [1, 2, 3]\n  for n in nums { x := n }\n}");
}

void test_break_outside_loop() {
    expect_error("fn main() { break }", "outside of loop");
}

void test_continue_outside_loop() {
    expect_error("fn main() { continue }", "outside of loop");
}

void test_return_type_mismatch() {
    expect_error("fn get() int { return \"hello\" }", "Return type mismatch");
}

void test_struct_decl() {
    expect_ok("Person :: struct { name str, age int }");
}

void test_struct_literal() {
    expect_ok(R"(
Person :: struct { name str, age int }
fn main() {
    p := Person { name: "Alice", age: 30 }
}
)");
}

void test_struct_field_access() {
    expect_ok(R"(
Person :: struct { name str, age int }
fn main() {
    p := Person { name: "Alice", age: 30 }
    n := p.name
}
)");
}

void test_unknown_struct_field() {
    expect_error(R"(
Person :: struct { name str }
fn main() {
    p := Person { name: "Alice" }
    x := p.foo
}
)", "Unknown field");
}

void test_method_decl() {
    expect_ok(R"(
Person :: struct { name str }
Person :: greet(self) str {
    return self.name
}
)");
}

void test_method_call() {
    expect_ok(R"(
Person :: struct { name str }
Person :: greet(self) str {
    return self.name
}
fn main() {
    p := Person { name: "Alice" }
    s := p.greet()
}
)");
}

void test_array_literal() {
    expect_ok("fn main() { arr := [1, 2, 3] }");
}

void test_array_index() {
    expect_ok("fn main() {\n  arr := [1, 2, 3]\n  x := arr[0]\n}");
}

void test_array_methods() {
    expect_ok("fn main() {\n  arr := [1, 2, 3]\n  n := arr.len()\n}");
}

void test_string_methods() {
    expect_ok("fn main() {\n  s := \"hello\"\n  n := s.len()\n}");
}

void test_function_call() {
    expect_ok(R"(
fn add(int a, int b) int {
    return a + b
}
fn main() {
    x := add(1, 2)
}
)");
}

void test_function_wrong_args() {
    expect_error(R"(
fn add(int a, int b) int { return a + b }
fn main() { add(1) }
)", "expects 2 arguments");
}

void test_variant_decl() {
    expect_ok("Option :: variant { Some { value int }, None }");
}

void test_lambda() {
    expect_ok("fn main() { f := fn(int x) { return x * 2 } }");
}

void test_optional_assign() {
    expect_ok("fn main() {\n  str? name = \"Alice\"\n}");
}

void test_or_fallback() {
    expect_ok(R"(
fn main() {
    str? maybe = "test"
    s := maybe or "default"
}
)");
}

void test_generic_struct() {
    expect_ok(R"(
Box<T> :: struct { value T }
fn main() {
    b := Box { value: 42 }
}
)");
}

void test_match_expr() {
    expect_ok(R"(
fn main() {
    x := 1
    msg := match x {
        0 -> "zero"
        1 -> "one"
        _ -> "other"
    }
}
)");
}

void test_go_stmt() {
    expect_ok(R"(
fn worker() { }
fn main() { go worker() }
)");
}

int main() {
    std::cout << "Running sema tests...\n";

    RUN_TEST(test_empty_program);
    RUN_TEST(test_simple_function);
    RUN_TEST(test_function_with_return);
    RUN_TEST(test_var_decl_explicit);
    RUN_TEST(test_var_decl_inferred);
    RUN_TEST(test_var_type_mismatch);
    RUN_TEST(test_undefined_var);
    RUN_TEST(test_const_reassign);
    RUN_TEST(test_binary_ops);
    RUN_TEST(test_comparison_ops);
    RUN_TEST(test_logical_ops);
    RUN_TEST(test_if_stmt);
    RUN_TEST(test_if_non_bool);
    RUN_TEST(test_while_stmt);
    RUN_TEST(test_for_array);
    RUN_TEST(test_break_outside_loop);
    RUN_TEST(test_continue_outside_loop);
    RUN_TEST(test_return_type_mismatch);
    RUN_TEST(test_struct_decl);
    RUN_TEST(test_struct_literal);
    RUN_TEST(test_struct_field_access);
    RUN_TEST(test_unknown_struct_field);
    RUN_TEST(test_method_decl);
    RUN_TEST(test_method_call);
    RUN_TEST(test_array_literal);
    RUN_TEST(test_array_index);
    RUN_TEST(test_array_methods);
    RUN_TEST(test_string_methods);
    RUN_TEST(test_function_call);
    RUN_TEST(test_function_wrong_args);
    RUN_TEST(test_variant_decl);
    RUN_TEST(test_lambda);
    RUN_TEST(test_optional_assign);
    RUN_TEST(test_or_fallback);
    RUN_TEST(test_generic_struct);
    RUN_TEST(test_match_expr);
    RUN_TEST(test_go_stmt);

    std::cout << "\n" << tests_passed << "/" << tests_run << " tests passed\n";
    return tests_passed == tests_run ? 0 : 1;
}
