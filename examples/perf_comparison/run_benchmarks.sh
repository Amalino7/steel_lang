#!/bin/bash

# Path to the lua interpreter (provided by user)
# If not provided as an environment variable, use 'luajit' as default
LUA_INTERPRETER=${LUA_PATH_VAR:-luajit}

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Building Steel in release mode...${NC}"
cargo build --release --quiet

run_bench() {
    local name=$1
    local steel_file=$2
    local py_file=$3
    local lua_file=$4

    echo -e "\n${GREEN}Benchmark: $name${NC}"
    echo "------------------------------------------------"
    
    echo -n "Steel:  "
    /usr/bin/time -f "%e seconds" ./target/release/steel_lang "$steel_file" 2>&1 > /dev/null | xargs echo
    
    echo -n "Python: "
    /usr/bin/time -f "%e seconds" python3 "$py_file" 2>&1 > /dev/null | xargs echo
    
    echo -n "Lua:    "
    /usr/bin/time -f "%e seconds" "$LUA_INTERPRETER" -joff "$lua_file" 2>&1 > /dev/null | xargs echo
}

# Ensure we are in the project root if script is run from examples/perf_comparison
if [[ -f "../../Cargo.toml" ]]; then
    cd ../../
fi

# Run the benchmarks
run_bench "Fibonacci(32)" "examples/perf_comparison/fib.steel" "examples/perf_comparison/fib.py" "examples/perf_comparison/fib.lua"
run_bench "Collatz(100,000)" "examples/perf_comparison/collatz.steel" "examples/perf_comparison/collatz.py" "examples/perf_comparison/collatz.lua"
run_bench "Memory Allocation (1M objects)" "examples/perf_comparison/alloc.steel" "examples/perf_comparison/alloc.py" "examples/perf_comparison/alloc.lua"
run_bench "Virtual Dispatch (1M calls)" "examples/perf_comparison/dispatch.steel" "examples/perf_comparison/dispatch.py" "examples/perf_comparison/dispatch.lua"
run_bench "String Concat (50k items)" "examples/perf_comparison/string_concat.steel" "examples/perf_comparison/string_concat.py" "examples/perf_comparison/string_concat.lua"
run_bench "Closure Overhead (1M calls)" "examples/perf_comparison/closure.steel" "examples/perf_comparison/closure.py" "examples/perf_comparison/closure.lua"
run_bench "List Operations (1M pushes)" "examples/perf_comparison/list_ops.steel" "examples/perf_comparison/list_ops.py" "examples/perf_comparison/list_ops.lua"
run_bench "Enum Pattern Match (1M matches)" "examples/perf_comparison/enum_match.steel" "examples/perf_comparison/enum_match.py" "examples/perf_comparison/enum_match.lua"
run_bench "Map Operations (1M inserts)" "examples/perf_comparison/map_ops.steel" "examples/perf_comparison/map_ops.py" "examples/perf_comparison/map_ops.lua"
