/**
 * @file main.cpp
 * @brief Shotgun compiler command-line interface.
 *
 * This file implements the shotgun CLI with the following commands:
 * - build: Compile source to native executable
 * - check: Type check without code generation
 * - emit-ir: Output LLVM IR for debugging
 * - parse: Parse and show declaration count
 * - test: Run test suite
 *
 * The compiler pipeline is:
 * 1. Lexer: Source -> Tokens
 * 2. Parser: Tokens -> AST
 * 3. Import resolution: Merge imported modules
 * 4. Sema: Type checking and semantic analysis
 * 5. CodeGen: AST -> LLVM IR
 * 6. LLVM: IR -> Object file
 * 7. Linker: Object file -> Executable
 */

#include "lexer.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "codegen.hpp"

#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>

#include <toml++/toml.hpp>

#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <set>
#include <filesystem>
#include <algorithm>

/// Prints usage information to stderr.
void print_usage() {
    std::cerr << "Usage: shotgun <command> [options] <file>\n";
    std::cerr << "\nCommands:\n";
    std::cerr << "  build <file>           Compile to executable\n";
    std::cerr << "  build <file> -o <out>  Compile with custom output name\n";
    std::cerr << "  check <file>           Type check only\n";
    std::cerr << "  emit-ir <file>         Output LLVM IR\n";
    std::cerr << "  parse <file>           Parse and show AST\n";
    std::cerr << "  test [dir]             Run tests (default: tests/)\n";
}

/**
 * @brief Reads entire file contents into a string.
 * @param path Path to the file.
 * @return File contents, or empty string on error.
 */
std::string read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file) {
        std::cerr << "Error: cannot open file '" << path << "'\n";
        return "";
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

/**
 * @brief Extracts the base name from a file path (without extension).
 * @param path File path (e.g., "/path/to/file.bs").
 * @return Base name (e.g., "file").
 */
std::string get_base_name(const std::string& path) {
    size_t last_slash = path.find_last_of("/\\");
    std::string filename = (last_slash == std::string::npos) ? path : path.substr(last_slash + 1);

    size_t last_dot = filename.find_last_of('.');
    if (last_dot != std::string::npos) {
        return filename.substr(0, last_dot);
    }
    return filename;
}

/**
 * @brief Gets the directory containing a file.
 * @param path File path.
 * @return Parent directory path.
 */
std::string get_directory(const std::string& path) {
    std::filesystem::path p(path);
    return p.parent_path().string();
}

/**
 * @brief Finds all .bs files in a directory (non-recursive).
 * @param dir Directory to search.
 * @return Sorted list of .bs file paths.
 */
std::vector<std::string> find_bs_files(const std::string& dir) {
    std::vector<std::string> files;
    if (dir.empty() || !std::filesystem::exists(dir)) {
        return files;
    }
    for (const auto& entry : std::filesystem::directory_iterator(dir)) {
        if (entry.is_regular_file() && entry.path().extension() == ".bs") {
            files.push_back(entry.path().string());
        }
    }
    std::sort(files.begin(), files.end());
    return files;
}

/**
 * @brief Finds all .bs files recursively in a directory.
 * @param dir Directory to search.
 * @return Sorted list of .bs file paths.
 */
std::vector<std::string> find_test_files(const std::string& dir) {
    std::vector<std::string> files;
    if (!std::filesystem::exists(dir)) {
        return files;
    }
    for (const auto& entry : std::filesystem::recursive_directory_iterator(dir)) {
        if (entry.is_regular_file() && entry.path().extension() == ".bs") {
            files.push_back(entry.path().string());
        }
    }
    std::sort(files.begin(), files.end());
    return files;
}

/**
 * @brief Finds all test functions (prefixed with "test_") in a program.
 * @param prog The parsed program.
 * @return List of test function names.
 */
std::vector<std::string> find_test_functions(const shotgun::Program& prog) {
    std::vector<std::string> tests;
    for (const auto& decl : prog.decls) {
        if (auto* fn = std::get_if<shotgun::FnDecl>(&decl)) {
            if (fn->name.rfind("test_", 0) == 0) {  // starts with "test_"
                tests.push_back(fn->name);
            }
        }
    }
    return tests;
}

/**
 * @brief Reads the tests_dir setting from shotgun.toml.
 * @param project_root Project root directory.
 * @return Configured tests directory, or "tests" as default.
 */
std::string read_tests_dir_config(const std::string& project_root) {
    std::filesystem::path toml_path = std::filesystem::path(project_root) / "shotgun.toml";
    if (!std::filesystem::exists(toml_path)) {
        return "tests";
    }

    try {
        auto config = toml::parse_file(toml_path.string());
        if (auto tests_dir = config["tests_dir"].value<std::string>()) {
            return *tests_dir;
        }
    } catch (const toml::parse_error&) {
        // Silently fall back to default on parse error
    }
    return "tests";
}

/**
 * @brief Finds the package name from shotgun.toml.
 *
 * Searches upward from start_dir to find shotgun.toml and
 * reads the "name" field.
 *
 * @param start_dir Directory to start searching from.
 * @return Package name, or empty string if not found.
 */
std::string find_package_name(const std::string& start_dir) {
    std::filesystem::path dir = start_dir.empty()
        ? std::filesystem::current_path()
        : std::filesystem::absolute(start_dir);

    while (!dir.empty()) {
        std::filesystem::path toml_path = dir / "shotgun.toml";
        if (std::filesystem::exists(toml_path)) {
            try {
                auto config = toml::parse_file(toml_path.string());
                if (auto name = config["name"].value<std::string>()) {
                    return *name;
                }
            } catch (const toml::parse_error&) {
                // Continue searching in parent directories
            }
        }
        if (dir.parent_path() == dir) break;
        dir = dir.parent_path();
    }
    return "";
}

/**
 * @brief Finds the project root directory (where shotgun.toml is located).
 *
 * Searches upward from start_dir to find shotgun.toml.
 *
 * @param start_dir Directory to start searching from.
 * @return Project root path, or start_dir if no shotgun.toml found.
 */
std::string find_project_root(const std::string& start_dir) {
    // Use current directory if start_dir is empty
    std::filesystem::path dir = start_dir.empty()
        ? std::filesystem::current_path()
        : std::filesystem::absolute(start_dir);

    while (!dir.empty()) {
        std::filesystem::path toml = dir / "shotgun.toml";
        if (std::filesystem::exists(toml)) {
            return dir.string();
        }
        if (dir.parent_path() == dir) break;
        dir = dir.parent_path();
    }
    // Return the original directory (made absolute) if no shotgun.toml found
    return start_dir.empty()
        ? std::filesystem::current_path().string()
        : std::filesystem::absolute(start_dir).string();
}

/**
 * @brief Resolves a module name to a file path.
 *
 * Converts module names like "testpkg.utils" to file paths
 * like "/path/to/project/utils.bs".
 *
 * @param module Module name (e.g., "mypackage.utils").
 * @param project_root Project root directory.
 * @param pkg_name Package name to strip from module path.
 * @return Resolved file path.
 */
std::string resolve_module_path(const std::string& module, const std::string& project_root, const std::string& pkg_name) {
    // Module format: pkgname.submodule or just submodule
    std::string path = module;

    // Replace dots with path separators, handling package prefix
    if (!pkg_name.empty() && path.find(pkg_name + ".") == 0) {
        path = path.substr(pkg_name.length() + 1);
    }

    // Replace remaining dots with path separators
    for (char& c : path) {
        if (c == '.') c = '/';
    }

    return project_root + "/" + path + ".bs";
}

/**
 * @brief Processes imports and merges declarations into the program.
 *
 * Handles both explicit 'uses' imports and Go-style automatic imports
 * of all .bs files in the same directory (when auto_import_same_dir is true).
 *
 * @param program The program to merge imports into.
 * @param source_dir Source file's directory.
 * @param imported Set of already-imported file paths (to avoid duplicates).
 * @param errors Vector to collect error messages.
 * @param auto_import_same_dir If true, auto-import all .bs files in source_dir.
 * @return True on success, false on error.
 */
bool process_imports(shotgun::Program& program, const std::string& source_dir,
                     std::set<std::string>& imported, std::vector<std::string>& errors,
                     bool auto_import_same_dir = false) {
    std::string project_root = find_project_root(source_dir);
    std::string pkg_name = find_package_name(source_dir);

    // Auto-import all .bs files in the same directory (Go-style package)
    // Only do this if there's a shotgun.toml in the directory (i.e., it's a real package)
    bool has_pkg_config = !pkg_name.empty();
    if (auto_import_same_dir && !source_dir.empty() && has_pkg_config) {
        auto bs_files = find_bs_files(source_dir);
        for (const auto& file : bs_files) {
            std::string abs_path = std::filesystem::absolute(file).string();

            // Skip if already imported (including the main file)
            if (imported.count(abs_path)) continue;
            imported.insert(abs_path);

            std::string file_source = read_file(file);
            if (file_source.empty()) {
                errors.push_back("Cannot read file '" + file + "'");
                return false;
            }

            shotgun::Lexer lexer(file_source, file);
            shotgun::Parser parser(lexer);
            auto file_program = parser.parse();

            if (parser.has_errors()) {
                for (const auto& err : parser.errors()) {
                    errors.push_back(err);
                }
                return false;
            }

            // Recursively process imports in the file (don't auto-import its dir again)
            if (!process_imports(file_program, get_directory(file), imported, errors, false)) {
                return false;
            }

            // Merge declarations (add before current program decls, except UseDecls)
            std::vector<shotgun::Decl> merged;
            for (auto& decl : file_program.decls) {
                if (!std::holds_alternative<shotgun::UseDecl>(decl)) {
                    merged.push_back(std::move(decl));
                }
            }
            for (auto& decl : program.decls) {
                merged.push_back(std::move(decl));
            }
            program.decls = std::move(merged);
        }
    }

    // Collect UseDecls
    std::vector<shotgun::UseDecl> uses;
    for (const auto& decl : program.decls) {
        if (auto* use = std::get_if<shotgun::UseDecl>(&decl)) {
            uses.push_back(*use);
        }
    }

    // Process each import
    for (const auto& use : uses) {
        for (const auto& module : use.modules) {
            std::string mod_path = resolve_module_path(module, project_root, pkg_name);

            // Skip if already imported
            if (imported.count(mod_path)) continue;
            imported.insert(mod_path);

            // Check file exists
            if (!std::filesystem::exists(mod_path)) {
                errors.push_back("Cannot find module '" + module + "' (looked for " + mod_path + ")");
                return false;
            }

            // Read and parse the module
            std::string mod_source = read_file(mod_path);
            if (mod_source.empty()) {
                errors.push_back("Cannot read module '" + module + "'");
                return false;
            }

            shotgun::Lexer lexer(mod_source, mod_path);
            shotgun::Parser parser(lexer);
            auto mod_program = parser.parse();

            if (parser.has_errors()) {
                for (const auto& err : parser.errors()) {
                    errors.push_back(err);
                }
                return false;
            }

            // Recursively process imports in the module
            if (!process_imports(mod_program, get_directory(mod_path), imported, errors)) {
                return false;
            }

            // Merge declarations (add module decls before current program decls, except UseDecls)
            std::vector<shotgun::Decl> merged;
            for (auto& decl : mod_program.decls) {
                if (!std::holds_alternative<shotgun::UseDecl>(decl)) {
                    merged.push_back(std::move(decl));
                }
            }
            for (auto& decl : program.decls) {
                merged.push_back(std::move(decl));
            }
            program.decls = std::move(merged);
        }
    }

    return true;
}

// Forward declarations for command handlers
int cmd_parse(const std::string& filename, const std::string& source);
int cmd_check(const std::string& filename, const std::string& source);
int cmd_emit_ir(const std::string& filename, const std::string& source);
int cmd_build(const std::string& filename, const std::string& source, const std::string& output);
int cmd_test(const std::string& tests_dir);

/**
 * @brief Main entry point for the Shotgun compiler.
 *
 * Parses command-line arguments and dispatches to the appropriate
 * command handler (parse, check, emit-ir, build, or test).
 *
 * @param argc Argument count.
 * @param argv Argument values.
 * @return 0 on success, 1 on error.
 */
int main(int argc, char* argv[]) {
    if (argc < 2) {
        print_usage();
        return 1;
    }

    std::string command = argv[1];

    if (command == "help" || command == "--help" || command == "-h") {
        print_usage();
        return 0;
    }

    // Handle test command specially - doesn't require a file argument
    if (command == "test") {
        std::string project_root = find_project_root("");
        std::string tests_dir = read_tests_dir_config(project_root);

        // Allow command-line override
        if (argc >= 3) {
            tests_dir = argv[2];
        }

        // Make path absolute if relative
        if (!std::filesystem::path(tests_dir).is_absolute()) {
            tests_dir = project_root + "/" + tests_dir;
        }

        return cmd_test(tests_dir);
    }

    if (argc < 3) {
        print_usage();
        return 1;
    }

    std::string filename = argv[2];
    std::string source = read_file(filename);
    if (source.empty() && filename != "-") {
        return 1;
    }

    // Parse output option for build
    std::string output;
    if (command == "build") {
        for (int i = 3; i < argc; i++) {
            if (std::string(argv[i]) == "-o" && i + 1 < argc) {
                output = argv[i + 1];
                i++;
            }
        }
        if (output.empty()) {
            output = get_base_name(filename);
        }
    }

    if (command == "parse") {
        return cmd_parse(filename, source);
    } else if (command == "check") {
        return cmd_check(filename, source);
    } else if (command == "emit-ir") {
        return cmd_emit_ir(filename, source);
    } else if (command == "build") {
        return cmd_build(filename, source, output);
    } else {
        std::cerr << "Unknown command: " << command << "\n";
        print_usage();
        return 1;
    }
}

/**
 * @brief Parses a source file and reports the number of declarations.
 *
 * Used for debugging the parser without type checking or code generation.
 *
 * @param filename Source file path (for error messages).
 * @param source Source code contents.
 * @return 0 on success, 1 on parse error.
 */
int cmd_parse(const std::string& filename, const std::string& source) {
    shotgun::Lexer lexer(source, filename);
    shotgun::Parser parser(lexer);
    auto program = parser.parse();

    if (parser.has_errors()) {
        for (const auto& err : parser.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    std::cout << "Parsed " << program.decls.size() << " declarations\n";
    return 0;
}

/**
 * @brief Type checks a source file without generating code.
 *
 * Runs the full frontend (parse, import resolution, semantic analysis)
 * but stops before code generation.
 *
 * @param filename Source file path.
 * @param source Source code contents.
 * @return 0 on success, 1 on error.
 */
int cmd_check(const std::string& filename, const std::string& source) {
    shotgun::Lexer lexer(source, filename);
    shotgun::Parser parser(lexer);
    auto program = parser.parse();

    if (parser.has_errors()) {
        for (const auto& err : parser.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    // Process imports (with auto-import for same-directory files)
    std::set<std::string> imported;
    imported.insert(std::filesystem::absolute(filename).string());
    std::vector<std::string> import_errors;
    if (!process_imports(program, get_directory(filename), imported, import_errors, true)) {
        for (const auto& err : import_errors) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    shotgun::Sema sema;
    if (!sema.analyze(program)) {
        for (const auto& err : sema.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    std::cout << "Type check passed\n";
    return 0;
}

/**
 * @brief Compiles a source file and outputs LLVM IR to stdout.
 *
 * Useful for debugging code generation and understanding the
 * generated LLVM IR.
 *
 * @param filename Source file path.
 * @param source Source code contents.
 * @return 0 on success, 1 on error.
 */
int cmd_emit_ir(const std::string& filename, const std::string& source) {
    shotgun::Lexer lexer(source, filename);
    shotgun::Parser parser(lexer);
    auto program = parser.parse();

    if (parser.has_errors()) {
        for (const auto& err : parser.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    // Process imports (with auto-import for same-directory files)
    std::set<std::string> imported;
    imported.insert(std::filesystem::absolute(filename).string());
    std::vector<std::string> import_errors;
    if (!process_imports(program, get_directory(filename), imported, import_errors, true)) {
        for (const auto& err : import_errors) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    shotgun::Sema sema;
    if (!sema.analyze(program)) {
        for (const auto& err : sema.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    shotgun::CodeGen codegen(filename);
    if (!codegen.generate(program, sema.symbols())) {
        for (const auto& err : codegen.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    std::cout << codegen.get_ir();
    return 0;
}

/**
 * @brief Compiles a source file to a native executable.
 *
 * Full compilation pipeline:
 * 1. Parse source code
 * 2. Resolve imports
 * 3. Type check
 * 4. Generate LLVM IR
 * 5. Emit object file
 * 6. Link with system linker (cc)
 *
 * @param filename Source file path.
 * @param source Source code contents.
 * @param output Output executable name.
 * @return 0 on success, 1 on error.
 */
int cmd_build(const std::string& filename, const std::string& source, const std::string& output) {
    // Initialize LLVM targets
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    // Parse
    shotgun::Lexer lexer(source, filename);
    shotgun::Parser parser(lexer);
    auto program = parser.parse();

    if (parser.has_errors()) {
        for (const auto& err : parser.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    // Process imports (with auto-import for same-directory files)
    std::set<std::string> imported;
    imported.insert(std::filesystem::absolute(filename).string());
    std::vector<std::string> import_errors;
    if (!process_imports(program, get_directory(filename), imported, import_errors, true)) {
        for (const auto& err : import_errors) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    // Semantic analysis
    shotgun::Sema sema;
    if (!sema.analyze(program)) {
        for (const auto& err : sema.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    // Code generation
    shotgun::CodeGen codegen(filename);
    if (!codegen.generate(program, sema.symbols())) {
        for (const auto& err : codegen.errors()) {
            std::cerr << err << "\n";
        }
        return 1;
    }

    // Get target
    llvm::Triple target_triple(llvm::sys::getDefaultTargetTriple());
    codegen.module()->setTargetTriple(target_triple);

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple.str(), error);
    if (!target) {
        std::cerr << "Error: " << error << "\n";
        return 1;
    }

    auto cpu = "generic";
    auto features = "";
    llvm::TargetOptions opt;
    auto target_machine = target->createTargetMachine(
        target_triple, cpu, features, opt, llvm::Reloc::PIC_);

    codegen.module()->setDataLayout(target_machine->createDataLayout());

    // Emit object file
    std::string obj_filename = output + ".o";
    std::error_code ec;
    llvm::raw_fd_ostream dest(obj_filename, ec, llvm::sys::fs::OF_None);

    if (ec) {
        std::cerr << "Could not open file: " << ec.message() << "\n";
        return 1;
    }

    llvm::legacy::PassManager pass;
    auto file_type = llvm::CodeGenFileType::ObjectFile;

    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
        std::cerr << "Target machine cannot emit object file\n";
        return 1;
    }

    pass.run(*codegen.module());
    dest.flush();

    // Link with clang (or cc)
    std::string link_cmd = "cc " + obj_filename + " -o " + output + " -lm 2>&1";
    int link_result = std::system(link_cmd.c_str());

    // Clean up object file
    std::remove(obj_filename.c_str());

    if (link_result != 0) {
        std::cerr << "Linking failed\n";
        return 1;
    }

    std::cout << "Built: " << output << "\n";
    return 0;
}

/**
 * @brief Runs the test suite.
 *
 * Finds all .bs files in the tests directory, locates functions
 * prefixed with "test_", and runs each one as an independent test.
 * For each test:
 * 1. Parse the file
 * 2. Inject a main() that calls the test function
 * 3. Compile to temporary executable
 * 4. Run and check exit code (0 = pass)
 *
 * @param tests_dir Directory containing test files.
 * @return 0 if all tests pass, 1 if any fail.
 */
int cmd_test(const std::string& tests_dir) {
    // Initialize LLVM targets
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::cout << "========================================\n";
    std::cout << "Shotgun Test Runner\n";
    std::cout << "========================================\n\n";

    // Find all test files
    auto test_files = find_test_files(tests_dir);
    if (test_files.empty()) {
        std::cerr << "No test files found in " << tests_dir << "/\n";
        return 1;
    }

    int total_tests = 0;
    int passed_tests = 0;
    int failed_tests = 0;

    // Temp directory for compiled tests
    std::string temp_dir = "/tmp/shotgun_tests";
    std::filesystem::create_directories(temp_dir);

    for (const auto& file : test_files) {
        std::string source = read_file(file);
        if (source.empty()) continue;

        // Parse the file
        shotgun::Lexer lexer(source, file);
        shotgun::Parser parser(lexer);
        auto program = parser.parse();

        if (parser.has_errors()) {
            std::cerr << "Parse errors in " << file << ":\n";
            for (const auto& err : parser.errors()) {
                std::cerr << "  " << err << "\n";
            }
            continue;
        }

        // Find test functions in this file
        auto test_funcs = find_test_functions(program);
        if (test_funcs.empty()) continue;

        // Run each test function
        for (const auto& test_name : test_funcs) {
            total_tests++;

            // Re-parse for fresh AST
            shotgun::Lexer test_lexer(source, file);
            shotgun::Parser test_parser(test_lexer);
            auto test_prog = test_parser.parse();

            // Process imports
            std::set<std::string> test_imported;
            test_imported.insert(std::filesystem::absolute(file).string());
            std::vector<std::string> test_import_errors;
            if (!process_imports(test_prog, get_directory(file), test_imported, test_import_errors, true)) {
                std::cout << "\033[0;31m[FAIL]\033[0m " << file << "::" << test_name;
                std::cout << " (import error)\n";
                for (const auto& err : test_import_errors) {
                    std::cerr << "       " << err << "\n";
                }
                failed_tests++;
                continue;
            }

            // Remove existing main() and add test harness
            test_prog.decls.erase(
                std::remove_if(test_prog.decls.begin(), test_prog.decls.end(),
                    [](const shotgun::Decl& d) {
                        if (auto* fn = std::get_if<shotgun::FnDecl>(&d)) {
                            return fn->name == "main";
                        }
                        return false;
                    }),
                test_prog.decls.end()
            );

            // Create main() that calls the test function and returns 0
            shotgun::FnDecl main_fn;
            main_fn.name = "main";
            main_fn.loc = {file, 0, 0};
            main_fn.return_type = std::make_unique<shotgun::Type>();
            main_fn.return_type->kind = shotgun::Type::Named{"int"};

            // Create call expression: test_name()
            auto callee_expr = std::make_unique<shotgun::Expr>();
            callee_expr->kind = shotgun::Expr::Ident{test_name};
            callee_expr->loc = {file, 0, 0};

            auto call_expr = std::make_unique<shotgun::Expr>();
            call_expr->kind = shotgun::Expr::Call{std::move(callee_expr), {}, {}};
            call_expr->loc = {file, 0, 0};

            auto call_stmt = std::make_unique<shotgun::Stmt>();
            call_stmt->kind = shotgun::Stmt::ExprStmt{std::move(call_expr)};
            call_stmt->loc = {file, 0, 0};
            main_fn.body.push_back(std::move(call_stmt));

            // Create return 0
            auto zero_expr = std::make_unique<shotgun::Expr>();
            zero_expr->kind = shotgun::Expr::IntLit{0};
            zero_expr->loc = {file, 0, 0};

            auto ret_stmt = std::make_unique<shotgun::Stmt>();
            ret_stmt->kind = shotgun::Stmt::Return{std::move(zero_expr)};
            ret_stmt->loc = {file, 0, 0};
            main_fn.body.push_back(std::move(ret_stmt));

            test_prog.decls.push_back(std::move(main_fn));

            // Semantic analysis
            shotgun::Sema sema;
            if (!sema.analyze(test_prog)) {
                std::cout << "\033[0;31m[FAIL]\033[0m " << file << "::" << test_name;
                std::cout << " (type error)\n";
                for (const auto& err : sema.errors()) {
                    std::cerr << "       " << err << "\n";
                }
                failed_tests++;
                continue;
            }

            // Code generation
            shotgun::CodeGen codegen(file);
            if (!codegen.generate(test_prog, sema.symbols())) {
                std::cout << "\033[0;31m[FAIL]\033[0m " << file << "::" << test_name;
                std::cout << " (codegen error)\n";
                for (const auto& err : codegen.errors()) {
                    std::cerr << "       " << err << "\n";
                }
                failed_tests++;
                continue;
            }

            // Compile to executable
            std::string safe_name = test_name;
            std::replace(safe_name.begin(), safe_name.end(), '/', '_');
            std::string temp_exe = temp_dir + "/" + safe_name;
            std::string temp_obj = temp_exe + ".o";

            llvm::Triple target_triple(llvm::sys::getDefaultTargetTriple());
            codegen.module()->setTargetTriple(target_triple);

            std::string error;
            auto target = llvm::TargetRegistry::lookupTarget(target_triple.str(), error);
            if (!target) {
                std::cout << "\033[0;31m[FAIL]\033[0m " << file << "::" << test_name;
                std::cout << " (target error: " << error << ")\n";
                failed_tests++;
                continue;
            }

            llvm::TargetOptions opt;
            auto target_machine = target->createTargetMachine(
                target_triple, "generic", "", opt, llvm::Reloc::PIC_);
            codegen.module()->setDataLayout(target_machine->createDataLayout());

            std::error_code ec;
            llvm::raw_fd_ostream dest(temp_obj, ec, llvm::sys::fs::OF_None);
            if (ec) {
                std::cout << "\033[0;31m[FAIL]\033[0m " << file << "::" << test_name;
                std::cout << " (file error)\n";
                failed_tests++;
                continue;
            }

            llvm::legacy::PassManager pass;
            if (target_machine->addPassesToEmitFile(pass, dest, nullptr,
                llvm::CodeGenFileType::ObjectFile)) {
                std::cout << "\033[0;31m[FAIL]\033[0m " << file << "::" << test_name;
                std::cout << " (emit error)\n";
                failed_tests++;
                continue;
            }

            pass.run(*codegen.module());
            dest.flush();

            // Link
            std::string link_cmd = "cc " + temp_obj + " -o " + temp_exe + " -lm 2>/dev/null";
            if (std::system(link_cmd.c_str()) != 0) {
                std::cout << "\033[0;31m[FAIL]\033[0m " << file << "::" << test_name;
                std::cout << " (link error)\n";
                std::remove(temp_obj.c_str());
                failed_tests++;
                continue;
            }
            std::remove(temp_obj.c_str());

            // Execute the test
            int result = std::system((temp_exe + " 2>&1").c_str());
            std::remove(temp_exe.c_str());

            if (result == 0) {
                std::cout << "\033[0;32m[PASS]\033[0m " << file << "::" << test_name << "\n";
                passed_tests++;
            } else {
                std::cout << "\033[0;31m[FAIL]\033[0m " << file << "::" << test_name << "\n";
                failed_tests++;
            }
        }
    }

    // Summary
    std::cout << "\n========================================\n";
    if (failed_tests == 0) {
        std::cout << "Results: \033[0;32m" << passed_tests << " passed\033[0m, ";
        std::cout << total_tests << " total\n";
    } else {
        std::cout << "Results: \033[0;32m" << passed_tests << " passed\033[0m, ";
        std::cout << "\033[0;31m" << failed_tests << " failed\033[0m, ";
        std::cout << total_tests << " total\n";
    }
    std::cout << "========================================\n";

    return (failed_tests > 0) ? 1 : 0;
}
