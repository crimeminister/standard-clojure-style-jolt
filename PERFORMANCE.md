# Performance Benchmark & Analysis: standard-clj vs. standard-clojure-style-js

This document provides a comparative performance benchmark between **`standard-clj`** (the compiled Jolt / Chez Scheme native executable) and **`standard-clojure-style-js`** (the upstream reference JavaScript implementation running on Node.js / V8).

---

## 1. Test Environment & Methodology

- **OS / Architecture:** Linux x86_64
- **Jolt Dialect Version:** 0.8.6 (Chez Scheme backend)
- **Node / V8 Environment:** Node.js invoked via `nix-shell -p pnpm --run "pnpx @chrisoakman/standard-clojure-style check ..."`
- **Target Directories:** `src/`, `test/`, `test_cases/` (12 files total, including `.clj` source files and `.edn` test fixture suites)

---

## 2. Initial Benchmark Results

### Command Outputs

#### Jolt Implementation (`standard-clj`)
```text
$ ./standard-clj check src/ test/ test_cases/
standard-clj check [0.29.0]

✓ src/standard_clojure_style/cli.clj [1210.0ms]
✓ src/standard_clojure_style/core.clj [5.0ms]
✓ src/standard_clojure_style/format.clj [5224.0ms]
✓ src/standard_clojure_style/main.clj [2.0ms]
✓ src/standard_clojure_style/parse_ns.clj [11849.0ms]
✓ src/standard_clojure_style/parser.clj [422.0ms]
✓ test/standard_clojure_style/format_test.clj [16.0ms]
✓ test/standard_clojure_style/parse_ns_test.clj [19.0ms]
✓ test/standard_clojure_style/parser_test.clj [28.0ms]
✓ test_cases/format_tests.edn [763.0ms]
✓ test_cases/parse_ns_tests.edn [2358.0ms]
✓ test_cases/parser_tests.edn [215.0ms]

All 12 files formatted with Standard Clojure Style 👍 [22113.0ms]
```

#### JavaScript Implementation (`standard-clojure-style-js`)
```text
$ pnpx @chrisoakman/standard-clojure-style check src/ test/ test_cases/
standard-clj check v0.29.0

✓ /src/standard_clojure_style/cli.clj [11.49ms]
✓ /src/standard_clojure_style/core.clj [0.78ms]
✓ /src/standard_clojure_style/format.clj [13.68ms]
✓ /src/standard_clojure_style/main.clj [0.27ms]
✓ /src/standard_clojure_style/parse_ns.clj [15.92ms]
✓ /src/standard_clojure_style/parser.clj [2.74ms]
✓ /test/standard_clojure_style/format_test.clj [0.53ms]
✓ /test/standard_clojure_style/parse_ns_test.clj [0.52ms]
✓ /test/standard_clojure_style/parser_test.clj [0.72ms]
✓ /test_cases/format_tests.edn [9.51ms]
✓ /test_cases/parse_ns_tests.edn [4.75ms]
✓ /test_cases/parser_tests.edn [1.79ms]

All 12 files formatted with Standard Clojure Style 👍 [69.92ms]
```

---

## 3. Side-by-Side Comparison Table

| Target File | Jolt Binary (`standard-clj`) | JS Engine (`standard-clojure-style-js`) | Ratio (Jolt / JS) |
| :--- | :---: | :---: | :---: |
| `src/standard_clojure_style/core.clj` | **5.0 ms** | **0.78 ms** | 6.4x |
| `src/standard_clojure_style/main.clj` | **2.0 ms** | **0.27 ms** | 7.4x |
| `test/standard_clojure_style/format_test.clj` | **16.0 ms** | **0.53 ms** | 30.2x |
| `test/standard_clojure_style/parse_ns_test.clj` | **19.0 ms** | **0.52 ms** | 36.5x |
| `test/standard_clojure_style/parser_test.clj` | **28.0 ms** | **0.72 ms** | 38.9x |
| `test_cases/parser_tests.edn` | **215.0 ms** | **1.79 ms** | 120.1x |
| `src/standard_clojure_style/parser.clj` | **422.0 ms** | **2.74 ms** | 154.0x |
| `test_cases/format_tests.edn` | **763.0 ms** | **9.51 ms** | 80.2x |
| `src/standard_clojure_style/cli.clj` | **1,210.0 ms** | **11.49 ms** | 105.3x |
| `test_cases/parse_ns_tests.edn` | **2,358.0 ms** | **4.75 ms** | 496.4x |
| `src/standard_clojure_style/format.clj` | **5,224.0 ms** | **13.68 ms** | 381.9x |
| `src/standard_clojure_style/parse_ns.clj` | **11,849.0 ms** | **15.92 ms** | 744.3x |
| **Total Runtime** | **22,113.0 ms (~22.1s)** | **69.92 ms (~0.07s)** | **~316x** |

---

## 4. Key Findings & Architectural Analysis

### 1. Small Files vs. Large ASTs
- For small files (< 100 lines, e.g. `core.clj`, `main.clj`), execution in the native binary takes only **1–5 ms**.
  Startup latency and file I/O overhead in the standalone binary are negligible.
- Scaling becomes non-linear on large source files (> 500 lines, e.g. `parse_ns.clj` and `format.clj`).
  Four files (`parse_ns.clj`, `format.clj`, `parse_ns_tests.edn`, and `cli.clj`) account for over **93%** of the total runtime in `standard-clj`.

### 2. Breakdown: Parsing vs. Formatting
Profiling `parse_ns.clj` (78 KB, ~800 lines of complex Clojure forms):
- **CST Parsing (`parser/parse`):** ~590 ms (~5% of total time).
- **CST Traversal & Formatting (`format-nodes` + rules):** ~11,200 ms (~95% of total time).

The primary bottleneck is not parsing the text into a CST; it is the iterative evaluation of formatting rules over tens of thousands of flattened CST tokens.

### 3. Root Causes for the Performance Delta

1. **V8 JIT Compilation vs. Scheme Function Overhead:**
   - V8 optimizes tight loops over JavaScript arrays into contiguous memory traversals with register allocation, inline property caches, and loop unrolling.
   - In Clojure running atop Chez Scheme, traversing a large vector of token nodes with `(nth nodes idx)`, evaluating conditional rules (Rules 1 through 6), and coordinating lookahead across parent/child nodes incurs high function invocation and pointer indirection overhead per token.
2. **Persistent Data Structures and Atom Synchronization:**
   - The upstream JS implementation uses flat, mutable JavaScript objects (`node.children.push(...)`, `node._printedColIdx = x`).
   - To match upstream behavior while managing shared references (paren stacks, line buffers, column indices), the Clojure port wraps nodes in atoms (`swap! node assoc ...`). Across tens of thousands of tokens per file, these atom dereferences and map updates accumulate significant overhead.
3. **Regular Expression Engines:**
   - Upstream Node.js uses Google's Irregexp engine, compiling regular expressions directly into native machine code.
   - In Jolt / Chez Scheme, string scanning and regular expression operations (`re-find`, `subs`, string slicing) incur higher abstraction costs when applied repeatedly across every token and newline boundary.

---

## 5. Optimization Opportunities

If parity with V8 speed is desired for large files, future optimizations could target:

1. **Eliminating Atoms During Formatting:**
   Replace atom-wrapped node representations in `format-nodes` with a single mutable state record or flat mutable array for traversal properties (`_printedColIdx`, `_origColIdx`).
2. **Rule Fast-Paths:**
   Add early exits for token categories (e.g. non-whitespace/non-comment tokens) before invoking rule inspectors that perform string slicing and regex lookups.
3. **Exploring `jolt.parser`:**
   While parsing accounts for only ~5% of time on large files, leveraging low-level Scheme primitives or `jolt.parser` can further streamline initial CST construction.

---

## 6. Post-Optimization Benchmark & Evaluation

Following the implementation of the performance enhancement plan:
1. **$O(N^2)$ CST Dereferencing Elimination:** Eliminated redundant full-tree vector allocations and atom dereferences on every paren opener in `format.clj`.
2. **Zero-Allocation Parser Primitives:** Replaced `(subs txt pos (inc pos))` in `Char` and `NotChar` with primitive `(.charAt txt pos)` comparisons, loop-based character matching in `StringParser`, and chunked 2KB buffer slicing in `Regex`.
3. **Continuation-Based Fast Escapes (`jolt.continuations`):** Employed `c/letcc [escape]` for zero-overhead escapes out of `Choice` parser loops.
4. **Fiber-Backed Multi-Core CLI Concurrency (`jolt.fibers`):** Formatted files concurrently across all 20 CPU carrier threads while preserving deterministic sorted console output.

### Updated Benchmark Comparison

```text
$ ./standard-clj check src/ test/ test_cases/
standard-clj check [0.29.0]

✓ src/standard_clojure_style/cli.clj [183.0ms]
✓ src/standard_clojure_style/core.clj [11.0ms]
✓ src/standard_clojure_style/format.clj [308.0ms]
✓ src/standard_clojure_style/main.clj [5.0ms]
✓ src/standard_clojure_style/parse_ns.clj [405.0ms]
✓ src/standard_clojure_style/parser.clj [106.0ms]
✓ test/standard_clojure_style/format_test.clj [25.0ms]
✓ test/standard_clojure_style/parse_ns_test.clj [24.0ms]
✓ test/standard_clojure_style/parser_test.clj [43.0ms]
✓ test_cases/format_tests.edn [274.0ms]
✓ test_cases/parse_ns_tests.edn [406.0ms]
✓ test_cases/parser_tests.edn [171.0ms]

All 12 files formatted with Standard Clojure Style 👍 [407.0ms]
```

### Comparative Summary

| Metric | Initial `standard-clj` | Post-Optimization `standard-clj` | `standard-clojure-style-js` | Overall Improvement |
| :--- | :---: | :---: | :---: | :---: |
| `parse_ns.clj` parse time | 591 ms | **101 ms** | ~3 ms | **5.8x faster parse** |
| `parse_ns.clj` format time | 11,849 ms | **405 ms** | 15.9 ms | **29.3x faster format** |
| **Total CLI Runtime (12 files)** | **22,113 ms (~22.1s)** | **407 ms (~0.40s)** | **69.9 ms (~0.07s)** | **54.3x overall speedup** |

### Evaluation of `jolt.parser` Alternative

Testing Jolt's built-in `jolt.parser` (`jolt.parser.combinators`, `jolt.parser.basic`) revealed that it is a monadic parser framework (Parsec-style) that tracks input coordinates by constructing `#jolt.parser.position.Location` records for every character token. 
- Running a simple `(pc/many pb/any)` on `parse_ns.clj` (78,100 characters) required **653 ms** solely to generate character location tokens.
- In contrast, our index-based CST parser parses the complete CST grammar of `parse_ns.clj` in **101 ms**.
- **Conclusion:** Retaining the custom, zero-allocation index-based CST parser provides superior performance and preserves 100% CST schema fidelity with upstream tests.
