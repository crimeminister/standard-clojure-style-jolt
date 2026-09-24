# Performance Benchmark & Analysis: standard-clj vs. standard-clojure-style-js

This document provides a comparative performance benchmark between **`standard-clj`** (the compiled Jolt / Chez Scheme native executable) and **`standard-clojure-style-js`** (the upstream reference JavaScript implementation running on Node.js / V8).

---

## 1. Test Environment & Methodology

- **OS / Architecture:** Linux x86_64
- **Jolt Dialect Version:** 0.8.12 (Chez Scheme backend; updated from 0.8.11, 0.8.10, 0.8.9, 0.8.8, and 0.8.6)
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
5. **Clojure Transients for CST Flattening & Collection Accumulation:** Replaced atom-wrapped vectors, maps, and sets in high-frequency CST traversals and AST extraction loops with Clojure transient collections (`transient`, `conj!`, `assoc!`, `persistent!`). Specifically:
   - **`flatten-tree`:** Traverses the CST directly into a transient vector instead of invoking `(swap! nodes conj %)` across tens of thousands of AST nodes. In microbenchmarks on `parse_ns.clj` (~26,000 nodes, 50 iterations), transient tree flattening required **49.0 ms** in Jolt 0.8.12 (50.5 ms in 0.8.11, 52.7 ms in 0.8.10, 56.3 ms in 0.8.9) vs. **167.2 ms** with atoms (~3.4x speedup) with zero atom synchronization overhead.
   - **AST Metadata & Require Processing:** Utilized transient vectors and sets in `get-metadata-strings-from-meta-node`, `parse-gen-class-exposes`, `sort-ns-result`, `get-platforms-from-array`, `only-one-require-per-platform`, `format-renames-list`, and `get-refer-clojure-keys`.
   - **Avoiding Transient Overhead on Tiny Collections:** Benchmarking revealed that for loops that almost always yield 0 or 1 elements (such as newline paren-slurping look-ahead), Clojure's interned empty vector singleton `[]` is allocation-free and faster than instantiating a transient wrapper. Transients were therefore targeted specifically at bulk collection construction where their $O(1)$ amortized in-place mutation provides genuine speedups.

### Updated Benchmark Comparison (Jolt 0.8.12)

```text
$ ./standard-clj check src/ test/ test_cases/
standard-clj check [0.29.0]

✓ src/standard_clojure_style/cli.clj [73.0ms]
✓ src/standard_clojure_style/core.clj [3.0ms]
✓ src/standard_clojure_style/format.clj [151.0ms]
✓ src/standard_clojure_style/main.clj [2.0ms]
✓ src/standard_clojure_style/parse_ns.clj [196.0ms]
✓ src/standard_clojure_style/parser.clj [48.0ms]
✓ test/standard_clojure_style/format_test.clj [15.0ms]
✓ test/standard_clojure_style/parse_ns_test.clj [11.0ms]
✓ test/standard_clojure_style/parser_test.clj [10.0ms]
✓ test_cases/format_tests.edn [113.0ms]
✓ test_cases/parse_ns_tests.edn [191.0ms]
✓ test_cases/parser_tests.edn [89.0ms]

All 12 files formatted with Standard Clojure Style 👍 [197.0ms]
```

### Comparative Summary

| Metric | Initial `standard-clj` (v0.8.6 Baseline) | Post-Optimization `standard-clj` (v0.8.6) | Historical `standard-clj` (v0.8.8) | Historical `standard-clj` (v0.8.9) | Historical `standard-clj` (v0.8.10) | Historical `standard-clj` (v0.8.11) | Fresh Evaluation `standard-clj` (v0.8.12) | `standard-clojure-style-js` | Overall Improvement (vs Baseline) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| `parse_ns.clj` parse time | 591 ms | 101 ms | 95 ms | 41 ms | 37 ms | 33 ms | **32 ms** | ~3 ms | **18.5x faster parse** |
| `parse_ns.clj` tree flatten (50 runs) | ~170 ms | ~170 ms | ~170 ms | 56.3 ms | 52.7 ms | 50.5 ms | **49.0 ms** | N/A | **3.4x faster flatten** |
| `parse_ns.clj` format time | 11,849 ms | 405 ms | 388 ms | 215 ms | 209 ms | 203 ms | **196 ms** | 15.9 ms | **60.5x faster format** |
| **Total CLI Runtime (12 files)** | **22,113 ms (~22.1s)** | **407 ms (~0.41s)** | **389 ms (~0.39s)** | **216 ms (~0.22s)** | **210 ms (~0.21s)** | **204 ms (~0.20s)** | **197 ms (~0.20s)** | **69.9 ms (~0.07s)** | **112.2x overall speedup** |

### Evaluation of `jolt.parser` Alternative

Testing Jolt's built-in `jolt.parser` (`jolt.parser.combinators`, `jolt.parser.basic`) revealed that it is a monadic parser framework (Parsec-style) that tracks input coordinates by constructing `#jolt.parser.position.Location` records for every character token. 
- Running a simple `(pc/many pb/any)` on `parse_ns.clj` (78,100 characters) in Jolt 0.8.12 required **722 ms** (compared to 565 ms in 0.8.11, 601 ms in 0.8.10, 615 ms in 0.8.9, 646 ms in 0.8.8, and 653 ms in 0.8.6) solely to generate character location tokens.
- In contrast, our index-based CST parser parses the complete CST grammar of `parse_ns.clj` in **32 ms** (down from 33 ms in 0.8.11, 37 ms in 0.8.10, 41 ms in 0.8.9, 95 ms in 0.8.8, and 101 ms in 0.8.6).
- **Conclusion:** Retaining the custom, zero-allocation index-based CST parser provides superior performance and preserves 100% CST schema fidelity with upstream tests.

---

## Appendix: Historical Results

For historical tracking and comparison, post-optimization benchmark results gathered under previous Jolt versions are preserved below.

### Historical Results (Jolt 0.8.11)

#### CLI Output (Jolt 0.8.11)

```text
$ ./standard-clj check src/ test/ test_cases/
standard-clj check [0.29.0]

✓ src/standard_clojure_style/cli.clj [98.0ms]
✓ src/standard_clojure_style/core.clj [9.0ms]
✓ src/standard_clojure_style/format.clj [155.0ms]
✓ src/standard_clojure_style/main.clj [1.0ms]
✓ src/standard_clojure_style/parse_ns.clj [203.0ms]
✓ src/standard_clojure_style/parser.clj [51.0ms]
✓ test/standard_clojure_style/format_test.clj [17.0ms]
✓ test/standard_clojure_style/parse_ns_test.clj [14.0ms]
✓ test/standard_clojure_style/parser_test.clj [23.0ms]
✓ test_cases/format_tests.edn [111.0ms]
✓ test_cases/parse_ns_tests.edn [193.0ms]
✓ test_cases/parser_tests.edn [71.0ms]

All 12 files formatted with Standard Clojure Style 👍 [204.0ms]
```

#### Metrics Recorded Under Jolt 0.8.11

- **`parse_ns.clj` Parse Time:** 33 ms
- **`parse_ns.clj` Tree Flatten (50 runs):** 50.5 ms
- **`parse_ns.clj` Format Time:** 203.0 ms
- **Total CLI Runtime (12 files):** 204.0 ms
- **`jolt.parser` Combinator `(pc/many pb/any)`:** 565 ms

### Historical Results (Jolt 0.8.10)

#### CLI Output (Jolt 0.8.10)

```text
$ ./standard-clj check src/ test/ test_cases/
standard-clj check [0.29.0]

✓ src/standard_clojure_style/cli.clj [82.0ms]
✓ src/standard_clojure_style/core.clj [7.0ms]
✓ src/standard_clojure_style/format.clj [167.0ms]
✓ src/standard_clojure_style/main.clj [1.0ms]
✓ src/standard_clojure_style/parse_ns.clj [209.0ms]
✓ src/standard_clojure_style/parser.clj [64.0ms]
✓ test/standard_clojure_style/format_test.clj [15.0ms]
✓ test/standard_clojure_style/parse_ns_test.clj [14.0ms]
✓ test/standard_clojure_style/parser_test.clj [17.0ms]
✓ test_cases/format_tests.edn [120.0ms]
✓ test_cases/parse_ns_tests.edn [206.0ms]
✓ test_cases/parser_tests.edn [78.0ms]

All 12 files formatted with Standard Clojure Style 👍 [210.0ms]
```

#### Metrics Recorded Under Jolt 0.8.10

- **`parse_ns.clj` Parse Time:** 37 ms
- **`parse_ns.clj` Tree Flatten (50 runs):** 52.7 ms
- **`parse_ns.clj` Format Time:** 209.0 ms
- **Total CLI Runtime (12 files):** 210.0 ms
- **`jolt.parser` Combinator `(pc/many pb/any)`:** 601 ms

### Historical Results (Jolt 0.8.9)

#### CLI Output (Jolt 0.8.9)

```text
$ ./standard-clj check src/ test/ test_cases/
standard-clj check [0.29.0]

✓ src/standard_clojure_style/cli.clj [84.0ms]
✓ src/standard_clojure_style/core.clj [5.0ms]
✓ src/standard_clojure_style/format.clj [165.0ms]
✓ src/standard_clojure_style/main.clj [2.0ms]
✓ src/standard_clojure_style/parse_ns.clj [215.0ms]
✓ src/standard_clojure_style/parser.clj [59.0ms]
✓ test/standard_clojure_style/format_test.clj [11.0ms]
✓ test/standard_clojure_style/parse_ns_test.clj [18.0ms]
✓ test/standard_clojure_style/parser_test.clj [23.0ms]
✓ test_cases/format_tests.edn [125.0ms]
✓ test_cases/parse_ns_tests.edn [213.0ms]
✓ test_cases/parser_tests.edn [98.0ms]

All 12 files formatted with Standard Clojure Style 👍 [216.0ms]
```

#### Metrics Recorded Under Jolt 0.8.9

- **`parse_ns.clj` Parse Time:** 41 ms
- **`parse_ns.clj` Tree Flatten (50 runs):** 56.3 ms
- **`parse_ns.clj` Format Time:** 215.0 ms
- **Total CLI Runtime (12 files):** 216.0 ms
- **`jolt.parser` Combinator `(pc/many pb/any)`:** 615 ms

### Historical Results (Jolt 0.8.8)

#### CLI Output (Jolt 0.8.8)

```text
$ ./standard-clj check src/ test/ test_cases/
standard-clj check [0.29.0]

✓ src/standard_clojure_style/cli.clj [170.0ms]
✓ src/standard_clojure_style/core.clj [10.0ms]
✓ src/standard_clojure_style/format.clj [302.0ms]
✓ src/standard_clojure_style/main.clj [5.0ms]
✓ src/standard_clojure_style/parse_ns.clj [388.0ms]
✓ src/standard_clojure_style/parser.clj [104.0ms]
✓ test/standard_clojure_style/format_test.clj [21.0ms]
✓ test/standard_clojure_style/parse_ns_test.clj [28.0ms]
✓ test/standard_clojure_style/parser_test.clj [27.0ms]
✓ test_cases/format_tests.edn [291.0ms]
✓ test_cases/parse_ns_tests.edn [388.0ms]
✓ test_cases/parser_tests.edn [162.0ms]

All 12 files formatted with Standard Clojure Style 👍 [389.0ms]
```

#### Metrics Recorded Under Jolt 0.8.8

- **`parse_ns.clj` Parse Time:** 95 ms
- **`parse_ns.clj` Format Time:** 388.0 ms
- **Total CLI Runtime (12 files):** 389.0 ms
- **`jolt.parser` Combinator `(pc/many pb/any)`:** 646 ms

### Historical Results (Jolt 0.8.6)

#### Post-Optimization CLI Output (Jolt 0.8.6)

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

#### Metrics Recorded Under Jolt 0.8.6

- **`parse_ns.clj` Parse Time:** 101 ms
- **`parse_ns.clj` Format Time:** 405.0 ms
- **Total CLI Runtime (12 files):** 407.0 ms
- **`jolt.parser` Combinator `(pc/many pb/any)`:** 653 ms

