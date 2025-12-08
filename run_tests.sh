#!/bin/bash
# Shotgun Compiler Tests
# Run from project root: ./run_tests.sh

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS_DIR="$PROJECT_ROOT/tests"
BOOTSTRAP_TESTS_DIR="$PROJECT_ROOT/bootstrap_tests"
SHOTGUN="$PROJECT_ROOT/bootstrap/_build/default/bin/main.exe"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0
TOTAL=0

# Test a file that should compile successfully
test_valid() {
    local file="$1"
    local name=$(basename "$file" .bs)
    local dir=$(basename $(dirname "$file"))
    TOTAL=$((TOTAL + 1))

    if "$SHOTGUN" check "$file" > /dev/null 2>&1; then
        echo -e "${GREEN}[PASS]${NC} $dir/$name"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}[FAIL]${NC} $dir/$name - expected to compile"
        FAILED=$((FAILED + 1))
    fi
}

# Test a file that should fail to compile
test_error() {
    local file="$1"
    local name=$(basename "$file" .bs)
    TOTAL=$((TOTAL + 1))

    if "$SHOTGUN" check "$file" > /dev/null 2>&1; then
        echo -e "${RED}[FAIL]${NC} errors/$name - expected to fail but compiled"
        FAILED=$((FAILED + 1))
    else
        echo -e "${GREEN}[PASS]${NC} errors/$name - correctly rejected"
        PASSED=$((PASSED + 1))
    fi
}

echo "========================================"
echo "Shotgun Compiler Tests"
echo "========================================"
echo ""

# Check if compiler exists
if [ ! -f "$SHOTGUN" ]; then
    echo -e "${RED}Error: Compiler not found at $SHOTGUN${NC}"
    echo "Run 'make build' first"
    exit 1
fi

echo "========================================"
echo "Language Tests (tests/)"
echo "========================================"
echo ""

echo "--- Basic Tests ---"
for f in "$TESTS_DIR"/basic/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Struct Tests ---"
for f in "$TESTS_DIR"/structs/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Function Tests ---"
for f in "$TESTS_DIR"/functions/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Method Tests ---"
for f in "$TESTS_DIR"/methods/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Trait Tests ---"
for f in "$TESTS_DIR"/traits/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Variant Tests ---"
for f in "$TESTS_DIR"/variants/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Control Flow Tests ---"
for f in "$TESTS_DIR"/control_flow/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Generics Tests ---"
for f in "$TESTS_DIR"/generics/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Map Tests ---"
for f in "$TESTS_DIR"/maps/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Recursive Type Tests ---"
for f in "$TESTS_DIR"/recursive/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Closure Tests ---"
for f in "$TESTS_DIR"/closures/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Array Tests ---"
for f in "$TESTS_DIR"/arrays/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "--- Error Tests (should fail) ---"
for f in "$TESTS_DIR"/errors/*.bs; do
    [ -f "$f" ] && test_error "$f"
done

echo ""
echo "--- Import Tests ---"
if [ -d "$TESTS_DIR/imports" ]; then
    TOTAL=$((TOTAL + 1))
    if (cd "$TESTS_DIR/imports" && "$SHOTGUN" check main.bs > /dev/null 2>&1); then
        echo -e "${GREEN}[PASS]${NC} imports/multi-file"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}[FAIL]${NC} imports/multi-file - expected to compile"
        FAILED=$((FAILED + 1))
    fi
fi

if [ -d "$TESTS_DIR/imports_bad" ]; then
    TOTAL=$((TOTAL + 1))
    if (cd "$TESTS_DIR/imports_bad" && "$SHOTGUN" check bad_import.bs > /dev/null 2>&1); then
        echo -e "${RED}[FAIL]${NC} imports/bad_import - expected to fail but compiled"
        FAILED=$((FAILED + 1))
    else
        echo -e "${GREEN}[PASS]${NC} imports/bad_import - correctly rejected"
        PASSED=$((PASSED + 1))
    fi
fi

echo ""
echo "========================================"
echo "Bootstrap Compiler Tests (bootstrap_tests/)"
echo "========================================"
echo ""

echo "--- Bootstrap Shotgun Tests ---"
for f in "$BOOTSTRAP_TESTS_DIR"/*.bs; do
    [ -f "$f" ] && test_valid "$f"
done

echo ""
echo "========================================"
if [ $FAILED -eq 0 ]; then
    echo -e "Results: ${GREEN}$PASSED passed${NC}, $FAILED failed, $TOTAL total"
else
    echo -e "Results: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}, $TOTAL total"
fi
echo "========================================"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
