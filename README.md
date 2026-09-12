# Standard Clojure Style (Jolt port)

A port of [Standard Clojure Style](https://github.com/oakmac/standard-clojure-style-js) to Clojure running on [Jolt](https://jolt-lang.net/), compiled to a standalone native binary via Chez Scheme.

## Features

- **100% Test Suite Compatibility**:
  - Parser: 141/141 passing tests
  - Namespace Parser: 83/83 passing tests
  - Formatter: 236/236 passing tests
- **Zero JVM overhead**: Compiles to a self-contained native executable (`standard-clj`) running on Chez Scheme.
- **Full CLI**: supports `check`, `fix`, `list`, stdin (`fix -`), `--include`, `--ignore`, `--file-ext`, and `.standard-clj.edn` / `.standard-clj.json` configurations.
- **Clojure Library API**: `format`, `parse`, and `parse-ns`.

## Building the Binary

Prerequisites: `jolt` (v0.8+).

### Optimized Release Build (Recommended)
Whole-program tree-shaking (`--closed-world`), direct linking (`--direct-link`), stripped inspector metadata (`--opt`), and fast LZ4 vfasl image (`:boot :fast`):

```sh
# Using the deps.edn task:
jolt build-release

# Or using the build command directly with alias:
jolt build -m standard-clojure-style.main -A:build-release --opt --closed-world --direct-link -o target/release/standard-clj
```

### Debug Build
Fast compilation retaining Chez inspector symbols and source frame locations for troubleshooting:

```sh
# Using the deps.edn task:
jolt build-debug

# Or using the build command directly with alias:
jolt build -m standard-clojure-style.main -A:build-debug --dev -o target/debug/standard-clj
```

### Default / Ad-hoc Build

```sh
jolt build -m standard-clojure-style.main -o standard-clj
```

## CLI Usage

```sh
# Check files without modifying
./standard-clj check src/ test/

# Fix files in place
./standard-clj fix src/ test/

# Format code from stdin
echo '(ns foo.bar (:require [b] [a]))' | ./standard-clj fix -

# List matched files
./standard-clj list src/
./standard-clj list src/ --output json
./standard-clj list src/ --output edn

# Configuration and filtering
./standard-clj check --include "src/**/*.clj" --ignore "src/generated/"
./standard-clj check --config ./config/standard-clj.edn
```

## Running Tests

```sh
# Run all test suites:
jolt test

# Or run individual test suites:
jolt -A:test -m standard-clojure-style.parser-test
jolt -A:test -m standard-clojure-style.parse-ns-test
jolt -A:test -m standard-clojure-style.format-test
```

## Library API

```clojure
(require '[standard-clojure-style.core :as scs])

;; Format Clojure code
(scs/format "(def   x   1)")
;;=> {:status "success", :out "(def x 1)"}

;; Parse into AST
(scs/parse "(def x 1)")

;; Parse ns form
(scs/parse-ns "(ns foo.bar (:require [clojure.string :as str]))")
```

## License

ISC License (matching upstream standard-clojure-style-js).
