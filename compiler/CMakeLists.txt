cmake_minimum_required(VERSION 3.16)
project(shotgun VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Dependencies:
# - LLVM (for code generation)
# - tomlplusplus (header-only, for config file parsing)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Project includes
include_directories(${CMAKE_SOURCE_DIR}/include)

# Compiler executable
add_executable(shotgun
    src/main.cpp
    src/lexer.cpp
    src/parser.cpp
    src/types.cpp
    src/symbol_table.cpp
    src/sema.cpp
    src/codegen.cpp
)

# Link LLVM (use shared library on systems like Arch)
target_link_libraries(shotgun LLVM)

# For static linking (release builds)
option(STATIC_BUILD "Build with static linking" OFF)
if(STATIC_BUILD)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
    set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
endif()

# Tests
enable_testing()

add_executable(test_lexer
    tests/test_lexer.cpp
    src/lexer.cpp
)
target_include_directories(test_lexer PRIVATE ${CMAKE_SOURCE_DIR}/include)
add_test(NAME lexer_tests COMMAND test_lexer)

add_executable(test_parser
    tests/test_parser.cpp
    src/lexer.cpp
    src/parser.cpp
)
target_include_directories(test_parser PRIVATE ${CMAKE_SOURCE_DIR}/include)
add_test(NAME parser_tests COMMAND test_parser)

add_executable(test_sema
    tests/test_sema.cpp
    src/lexer.cpp
    src/parser.cpp
    src/types.cpp
    src/symbol_table.cpp
    src/sema.cpp
)
target_include_directories(test_sema PRIVATE ${CMAKE_SOURCE_DIR}/include)
add_test(NAME sema_tests COMMAND test_sema)

add_executable(test_codegen
    tests/test_codegen.cpp
    src/lexer.cpp
    src/parser.cpp
    src/types.cpp
    src/symbol_table.cpp
    src/sema.cpp
    src/codegen.cpp
)
target_include_directories(test_codegen PRIVATE ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(test_codegen LLVM)
add_test(NAME codegen_tests COMMAND test_codegen)
