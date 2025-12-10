#include "lexer.hpp"
#include "parser.hpp"
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

void expect_no_errors(const Parser& p) {
    if (p.has_errors()) {
        std::string msg = "Parse errors:";
        for (const auto& e : p.errors()) {
            msg += "\n  " + e;
        }
        throw std::runtime_error(msg);
    }
}

void expect_decl_count(const Program& p, size_t count) {
    if (p.decls.size() != count) {
        throw std::runtime_error("Expected " + std::to_string(count) +
            " declarations, got " + std::to_string(p.decls.size()));
    }
}

// === Tests ===

void test_empty_program() {
    Lexer lexer("");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);
    expect_decl_count(prog, 0);
}

void test_simple_function() {
    Lexer lexer("fn main() { }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);
    expect_decl_count(prog, 1);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    if (!fn) throw std::runtime_error("Expected FnDecl");
    if (fn->name != "main") throw std::runtime_error("Expected name 'main'");
    if (!fn->params.empty()) throw std::runtime_error("Expected no params");
}

void test_function_with_params() {
    Lexer lexer("fn add(int a, int b) int { return a + b }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);
    expect_decl_count(prog, 1);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    if (!fn) throw std::runtime_error("Expected FnDecl");
    if (fn->name != "add") throw std::runtime_error("Expected name 'add'");
    if (fn->params.size() != 2) throw std::runtime_error("Expected 2 params");
    if (!fn->return_type) throw std::runtime_error("Expected return type");
}

void test_function_no_parens() {
    Lexer lexer("fn main { }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);
    expect_decl_count(prog, 1);
}

void test_struct_decl() {
    Lexer lexer("Person :: struct { name str, age int }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);
    expect_decl_count(prog, 1);

    auto* s = std::get_if<StructDecl>(&prog.decls[0]);
    if (!s) throw std::runtime_error("Expected StructDecl");
    if (s->name != "Person") throw std::runtime_error("Expected name 'Person'");
    if (s->fields.size() != 2) throw std::runtime_error("Expected 2 fields");
}

void test_variant_decl() {
    Lexer lexer("Option :: variant { Some { value int }, None }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);
    expect_decl_count(prog, 1);

    auto* v = std::get_if<VariantDecl>(&prog.decls[0]);
    if (!v) throw std::runtime_error("Expected VariantDecl");
    if (v->name != "Option") throw std::runtime_error("Expected name 'Option'");
    if (v->cases.size() != 2) throw std::runtime_error("Expected 2 cases");
}

void test_method_decl() {
    Lexer lexer("Person :: greet(self) str { return \"hello\" }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);
    expect_decl_count(prog, 1);

    auto* impl = std::get_if<ImplDecl>(&prog.decls[0]);
    if (!impl) throw std::runtime_error("Expected ImplDecl");
    if (impl->type_name != "Person") throw std::runtime_error("Expected type 'Person'");
    if (impl->methods.size() != 1) throw std::runtime_error("Expected 1 method");
}

void test_var_decl_explicit_type() {
    Lexer lexer("fn main() { int x = 42 }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    if (fn->body.size() != 1) throw std::runtime_error("Expected 1 stmt");

    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    if (!var) throw std::runtime_error("Expected VarDecl");
    if (var->name != "x") throw std::runtime_error("Expected name 'x'");
    if (!var->type) throw std::runtime_error("Expected explicit type");
}

void test_var_decl_inferred() {
    Lexer lexer("fn main() { x := 42 }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    if (!var) throw std::runtime_error("Expected VarDecl");
    if (var->type) throw std::runtime_error("Expected inferred type (nullptr)");
}

void test_const_decl() {
    Lexer lexer("fn main() { const MAX = 100 }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    if (!var) throw std::runtime_error("Expected VarDecl");
    if (!var->is_const) throw std::runtime_error("Expected const");
}

void test_if_stmt() {
    Lexer lexer("fn main() { if x > 0 { print(x) } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* if_stmt = std::get_if<Stmt::If>(&fn->body[0]->kind);
    if (!if_stmt) throw std::runtime_error("Expected If");
    if (if_stmt->then_block.empty()) throw std::runtime_error("Expected then block");
}

void test_if_else() {
    Lexer lexer("fn main() { if x > 0 { a() } else { b() } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* if_stmt = std::get_if<Stmt::If>(&fn->body[0]->kind);
    if (!if_stmt) throw std::runtime_error("Expected If");
    if (if_stmt->else_block.empty()) throw std::runtime_error("Expected else block");
}

void test_while_stmt() {
    Lexer lexer("fn main() { while x > 0 { x = x - 1 } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* while_stmt = std::get_if<Stmt::While>(&fn->body[0]->kind);
    if (!while_stmt) throw std::runtime_error("Expected While");
}

void test_for_stmt() {
    Lexer lexer("fn main() { for n in nums { print(n) } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* for_stmt = std::get_if<Stmt::For>(&fn->body[0]->kind);
    if (!for_stmt) throw std::runtime_error("Expected For");
    if (for_stmt->var != "n") throw std::runtime_error("Expected var 'n'");
}

void test_for_map_iteration() {
    Lexer lexer("fn main() { for k, v in map { print(k) } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* for_stmt = std::get_if<Stmt::For>(&fn->body[0]->kind);
    if (!for_stmt) throw std::runtime_error("Expected For");
    if (for_stmt->var != "k") throw std::runtime_error("Expected var 'k'");
    if (for_stmt->var2 != "v") throw std::runtime_error("Expected var2 'v'");
}

void test_binary_expr() {
    Lexer lexer("fn main() { x := 1 + 2 * 3 }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    auto* bin = std::get_if<Expr::Binary>(&var->init->kind);
    if (!bin) throw std::runtime_error("Expected Binary");
    if (bin->op != TokenKind::Plus) throw std::runtime_error("Expected Plus at top");
}

void test_call_expr() {
    Lexer lexer("fn main() { foo(1, 2) }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* expr_stmt = std::get_if<Stmt::ExprStmt>(&fn->body[0]->kind);
    auto* call = std::get_if<Expr::Call>(&expr_stmt->expr->kind);
    if (!call) throw std::runtime_error("Expected Call");
    if (call->args.size() != 2) throw std::runtime_error("Expected 2 args");
}

void test_field_access() {
    Lexer lexer("fn main() { x := p.name }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    auto* field = std::get_if<Expr::Field>(&var->init->kind);
    if (!field) throw std::runtime_error("Expected Field");
    if (field->name != "name") throw std::runtime_error("Expected field 'name'");
}

void test_index_expr() {
    Lexer lexer("fn main() { x := arr[0] }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    auto* idx = std::get_if<Expr::Index>(&var->init->kind);
    if (!idx) throw std::runtime_error("Expected Index");
}

void test_struct_literal() {
    Lexer lexer("fn main() { p := Person { name: \"Alice\", age: 30 } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    auto* lit = std::get_if<Expr::StructLit>(&var->init->kind);
    if (!lit) throw std::runtime_error("Expected StructLit");
    if (lit->type_name != "Person") throw std::runtime_error("Expected type 'Person'");
    if (lit->fields.size() != 2) throw std::runtime_error("Expected 2 fields");
}

void test_array_literal() {
    Lexer lexer("fn main() { arr := [1, 2, 3] }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    auto* arr = std::get_if<Expr::ArrayLit>(&var->init->kind);
    if (!arr) throw std::runtime_error("Expected ArrayLit");
    if (arr->elements.size() != 3) throw std::runtime_error("Expected 3 elements");
}

void test_match_expr() {
    Lexer lexer("fn main() { x := match n { 0 -> \"zero\", _ -> \"other\" } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    auto* match = std::get_if<Expr::Match>(&var->init->kind);
    if (!match) throw std::runtime_error("Expected Match");
    if (match->arms.size() != 2) throw std::runtime_error("Expected 2 arms");
}

void test_lambda() {
    Lexer lexer("fn main() { f := fn(int x) { return x * 2 } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* var = std::get_if<Stmt::VarDecl>(&fn->body[0]->kind);
    auto* lambda = std::get_if<Expr::Lambda>(&var->init->kind);
    if (!lambda) throw std::runtime_error("Expected Lambda");
    if (lambda->params.size() != 1) throw std::runtime_error("Expected 1 param");
}

void test_uses_decl() {
    Lexer lexer("uses: foo, bar.baz");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);
    expect_decl_count(prog, 1);

    auto* use = std::get_if<UseDecl>(&prog.decls[0]);
    if (!use) throw std::runtime_error("Expected UseDecl");
    if (use->modules.size() != 2) throw std::runtime_error("Expected 2 modules");
}

void test_go_stmt() {
    Lexer lexer("fn main() { go worker() }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* go = std::get_if<Stmt::Go>(&fn->body[0]->kind);
    if (!go) throw std::runtime_error("Expected Go");
}

void test_return_stmt() {
    Lexer lexer("fn main() { return 42 }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* ret = std::get_if<Stmt::Return>(&fn->body[0]->kind);
    if (!ret) throw std::runtime_error("Expected Return");
    if (!ret->value) throw std::runtime_error("Expected return value");
}

void test_break_continue() {
    Lexer lexer("fn main() { while true { break } }");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    auto* while_stmt = std::get_if<Stmt::While>(&fn->body[0]->kind);
    auto* brk = std::get_if<Stmt::Break>(&while_stmt->body[0]->kind);
    if (!brk) throw std::runtime_error("Expected Break");
}

void test_assignment() {
    Lexer lexer("fn main() {\n  x = 1\n  x += 2\n}");
    Parser parser(lexer);
    auto prog = parser.parse();
    expect_no_errors(parser);

    auto* fn = std::get_if<FnDecl>(&prog.decls[0]);
    if (fn->body.size() != 2) throw std::runtime_error("Expected 2 stmts");
    auto* assign1 = std::get_if<Stmt::Assign>(&fn->body[0]->kind);
    if (!assign1) throw std::runtime_error("Expected Assign");
    if (assign1->op != TokenKind::Assign) throw std::runtime_error("Expected =");
    auto* assign2 = std::get_if<Stmt::Assign>(&fn->body[1]->kind);
    if (!assign2) throw std::runtime_error("Expected second Assign");
    if (assign2->op != TokenKind::PlusEq) throw std::runtime_error("Expected +=");
}

int main() {
    std::cout << "Running parser tests...\n";

    RUN_TEST(test_empty_program);
    RUN_TEST(test_simple_function);
    RUN_TEST(test_function_with_params);
    RUN_TEST(test_function_no_parens);
    RUN_TEST(test_struct_decl);
    RUN_TEST(test_variant_decl);
    RUN_TEST(test_method_decl);
    RUN_TEST(test_var_decl_explicit_type);
    RUN_TEST(test_var_decl_inferred);
    RUN_TEST(test_const_decl);
    RUN_TEST(test_if_stmt);
    RUN_TEST(test_if_else);
    RUN_TEST(test_while_stmt);
    RUN_TEST(test_for_stmt);
    RUN_TEST(test_for_map_iteration);
    RUN_TEST(test_binary_expr);
    RUN_TEST(test_call_expr);
    RUN_TEST(test_field_access);
    RUN_TEST(test_index_expr);
    RUN_TEST(test_struct_literal);
    RUN_TEST(test_array_literal);
    RUN_TEST(test_match_expr);
    RUN_TEST(test_lambda);
    RUN_TEST(test_uses_decl);
    RUN_TEST(test_go_stmt);
    RUN_TEST(test_return_stmt);
    RUN_TEST(test_break_continue);
    RUN_TEST(test_assignment);

    std::cout << "\n" << tests_passed << "/" << tests_run << " tests passed\n";
    return tests_passed == tests_run ? 0 : 1;
}
