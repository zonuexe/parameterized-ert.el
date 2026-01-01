# parameterized-ert.el

## Overview

`parameterized-ert` adds a lightweight parameterization layer on top of [ERT: Emacs Lisp Regression Testing]. It provides a wrapper macro and provider helpers that allow you to write concise, data-driven tests without the boilerplate of manual loops.

**Key Features:**

 * **Injected Arguments**: Parameters are passed directly to the test body.
 * **Flexible Providers**: Support for list-based, generator-based, and property-based (randomized) data.
 * **Runtime Resolution**: Parameters are resolved at runtime, keeping macro expansion small and enabling dynamic test generation.

## Basic Usage

Without parameterization, you often have to write boilerplate code to iterate over test cases:

```elisp
(ert-deftest test-add-dolist ()
  "Run multiple cases without parameterized-ert."
  (dolist (case '((:expected 2 :a 1 :b 1)
                  (:expected 4 :a 2 :b 2)
                  (:expected -1 :a 1 :b -2)))
    (let ((expected (plist-get case :expected))
          (a (plist-get case :a))
          (b (plist-get case :b)))
      (should (eq expected (+ a b))))))
```

With `parameterized-ert-deftest`, arguments are injected directly into the test scope:

```elisp
(parameterized-ert-deftest test-add (expected a b)
  "Run multiple cases with parameterized-ert."
  :parameters '((:expected 2 :a 1 :b 1)
                ("2 + 2 should be 4" :expected 4 :a 2 :b 2) ;; labels improve readability
                (-1 1 -2) ;; positional values are also accepted
                ("0 + 0 should be 0" 0 0 0)) ;; labels also work with positional values
  (should (eq expected (+ a b))))
```

## Defining Providers

A **provider** is a function that returns a list of parameter plists or a [generator][Generators].

Use these keywords inside `parameterized-ert-deftest`:

 * `:providers`: A list of provider functions (symbols, lambdas, or helper calls).
 * `:parameters`: A static list of parameter entries (plists or positional lists).
 * `:parameterize-continue-on-failure`: If non-nil, the test continues running subsequent cases even after a failure, reporting all failures at the end.

Example:

```elisp
(parameterized-ert-deftest test-add-outside-providers (expected a b)
  :providers (list #'example-add-list-provider
                   #'example-add-generator-provider)
  (should (eq expected (+ a b))))

(defun example-add-list-provider ()
  "Return parameter sets for `test-add'."
  (cl-loop for a from 5 upto 6
           append
           (cl-loop for b from 10 upto 11
                    collect (list :expected (+ a b) :a a :b b))))

(iter-defun example-add-generator-provider ()
  "Yield parameter sets for `test-add'."
  (cl-loop for a from 7 upto 8
           do (cl-loop for b from 20 upto 21
                       do (iter-yield (list :expected (+ a b) :a a :b b)))))
```

## Property-Based Testing

Property-based testing focuses on properties that should hold for many inputs instead of enumerating fixed examples. In `parameterized-ert`, a [QuickCheck]-style helper generates inputs and runs the same test repeatedly.

`parameterized-ert`-property creates a provider from a plist of `cl-typep` types.

```elisp
(parameterized-ert-deftest test-add-property (a b)
  :providers (list (parameterized-ert-property
                    '(:a 'integer :b '(integer 0 10))
                    :times 100 :seed 42))
  (should (eq (+ a b) (+ b a))))
```

For a [QuickCheck]-style wrapper, use `parameterized-ert-property-quickcheck`, which defines an ERT test for a predicate function:

```elisp
(parameterized-ert-property-quickcheck
 #'identity '(:argument (or integer float string symbol))
 :max-success 1000 :seed 123)

(parameterized-ert-property-quickcheck
 #'identity '(:argument integer)
 :max-success 1000 :seed 123
 :test #'eq)

(parameterized-ert-property-quickcheck
 #'reverse '(:argument (member nil (1 2 3) (a b c) (1) (x y)))
 :test (lambda (actual xs) (equal xs (reverse actual))))
```

## Provider Helpers

### `parameterized-ert-map-zip`

Builds a provider from plist keys where list values are zipped by index.

- `list-or-function`: pass a list or a function
  - list values must have the same length across keys
  - function values receive `params` (a plist of current values) and return one value

```elisp
(parameterized-ert-deftest test-twice (expected input)
  :providers (list (parameterized-ert-map-zip
                    :expected '(0 2 4 6 8 10)
                    :input    '(0 1 2 3 4 5)))
  (should (eq expected (* 2 input))))
```

This runs six cases with pairs `(0, 0)` `(2, 1)` `(4, 2)` `(6, 3)` `(8, 4)` `(10, 5)`.

```elisp
(parameterized-ert-deftest test-twice-complex (expected input)
  :providers (list (parameterized-ert-map-zip
                    :expected (lambda (params)
                                (let ((input (plist-get params :input)))
                                  (+ input input))))
                    :input (number-sequence 0 5)))
  (should (eq expected (* 2 input))))
```

This is a verbose version of `test-twice`; it runs the same six pairs.

### `parameterized-ert-map-product`

Maps a function over the Cartesian product of lists.

```elisp
(parameterized-ert-deftest test-cons-multi (expected i j k)
  :providers (list (parameterized-ert-map-product
                    (lambda (v1 v2 v3) (list :expected (list v1 v2 v3)
                                             :i v1 :j v2 :k v3))
                    :v1 '(a b)
                    :v2 '(x y z)
                    :v3 '(1 2 3 4)))
  (should (eq expected (cons i (cons j (cons k nil))))))
```

> [!WARNING]
> Large input lists can create a combinatorial explosion.

## Terminology for xUnit Users

If you are used to other xUnit-style frameworks:
- **Test case**: `ert-deftest` / `parameterized-ert-deftest`.
- **Parameterized test / Theory**: `parameterized-ert-deftest` with `:parameters` or `:providers`.
- **Data provider**: a provider function passed via `:providers`.
- **Test data rows**: parameter plists (or positional lists) supplied by a provider.

Terminology differs across frameworks. `parameterized-ert` avoids copying any
one framework's naming and instead uses neutral terms like "parameters" and
"providers" to keep the API clear and consistent.

### For JUnit Users

- `@ValueSource`: use `:parameters` with positional values (e.g., `(1 2 3)`).
- `@MethodSource`: use `:providers` with a function symbol or lambda.
- `@CsvSource`/`@CsvFileSource`: use plist rows in `:parameters` or `parameterized-ert-map-zip`.

### For PHPUnit Users

- `#[DataProvider]` / `@dataProvider`: use `:providers` with a function symbol or lambda.
- `#[TestWith]` / `@testWith`: use `:parameters` with plist rows or positional values.

### For NUnit Users

- `[TestCase]`: use `:parameters` with plist rows or positional values.
- `[TestCaseSource]`: use `:providers` with a function symbol or lambda.
- `[Values]` / `[ValueSource]`: use `:parameters` with positional values.
- `[Combinatorial]`: use `parameterized-ert-map-product`.

### For pytest Users

- `@pytest.mark.parametrize`: use `:parameters` with plist rows or positional values.
- Indirect parametrization or fixtures: use `:providers` with a function symbol or lambda.

### For Jest Users

- `test.each` / `describe.each`: use `:parameters` with plist rows or positional values.

## Design Decisions: Comparison with ert-parametrized.el

I developed this package because I believe ERT needs a more idiomatic parameterization layer. While [svjson/ert-parametrized.el] is excellent, our design philosophies differ:

| Feature      | ert-parametrized.el               | parameterized-ert (This Package)          |
|--------------|-----------------------------------|-------------------------------------------|
| Architecture | Static (Macro-expansion time)     | Dynamic (Runtime looping)                 |
| Granularity  | One case = One ERT test           | Many cases = One ERT test                 |
| Data Source  | DSL-based (`:generator`, `:eval`) | Idiomatic Lisp (`:providers`, functions)" |
| Flexibility  | Fixed at expansion time           | Resolved at runtime (Lazy/Generative)     |
| API Style    | Nested Tag-based DSL              | Flat Keyword-based (matches ert-deftest)  |

### Strengths of ert-parametrized.el

 * **Granular Reporting**: Each case is registered as a separate ERT test, making failures identifiable at the top-level list.
 * **Native Filtering**: You can use ERT's built-in selector to run specific cases by name.
 * **Explicit Intent**: The DSL's `:generator` and `:eval` tags clearly signal where data is coming from to users familiar with other xUnit frameworks.

### Why `parameterized-ert`?

"Many specialized static tests" versus "one test that loops many parameters" is a classic trade-off. While the former integrates deeply with ERT's UI, the latter—used by this package—offers several advantages for Emacs Lisp developers:

 * **Less Visual Noise**: Avoids mandatory :eval or :quote tags inside the test definition. It’s "just Lisp."
 * **Runtime Power**: Since providers are resolved at runtime, you can generate data based on the current environment or use lazy generators for massive datasets without bloating the .elc file.
 * **UI Cleanliness** : Keeping a single test name prevents the ERT results buffer from being overwhelmed when running hundreds of generated cases (e.g., in Property-Based Testing).

> [!NOTE]
> By default, `parameterized-ert` continues running all cases even if some fail (equivalent to `:parameterize-continue-on-failure t`). This ensures you get a complete report of all failing parameters in a single test run, rather than stopping at the first error.

```elisp
;; Prior art
(ert-parametrized-deftest generator-example (input expected)
    (("%d-multiplied-by-2-equals-%d"
      (:generator (:eval (number-sequence 0 10)))
      (:generator (:eval '(0 2 4 6 8 10 12 14 16 18 20)))))
  (should (equal (* input 2) expected)))

;; This package
(parameterized-ert-deftest providers-map-zip-example (input expected)
  ;; Just zipping two lists.
  :providers (list (parameterized-ert-map-zip
                    :input (number-sequence 0 10)
                    :expected '(0 2 4 6 8 10 12 14 16 18 20)))
  (should (equal (* input 2) expected)))
```

<details><summary>More options</summary>

``` elisp
(parameterized-ert-deftest parameters-values-example (input expected)
  ;; Plain positional values.
  :parameters '((0 0) (1 2) (2 4) (3 6) (4 8) (5 10)
                (6 12) (7 14) (8 16) (9 18) (10 20))
  (should (equal (* input 2) expected)))

(parameterized-ert-deftest parameters-mapcar-example (input expected)
  ;; Just mapping a function.
  :parameters (mapcar (lambda (n) (list :input n :expected (+ n n)))
                      (number-sequence 0 10))
  (should (equal (* input 2) expected)))
```

</details>

`parameterized-ert` does not use a specialized DSL; it is just Emacs Lisp. While it lacks a dedicated "matrix" syntax, you can achieve the same results using helper functions or QuickCheck-style sampling for a more modern approach.

``` elisp
;; Prior art
(ert-parametrized-deftest-matrix test-matrix-with-generators--produces-even-numbers
    (test-number multiplier)
    ((("num-%s"
       (:generator (:eval (number-sequence 1 5)))))
     (("multiplied-by-%s"
       (:generator (:eval (number-sequence 2 10 2))))))
  (should (cl-evenp (* test-number multiplier))))

;; This package
(parameterized-ert-deftest test-matrix-with-map-product (test-number multiplier)
  :providers (list (parameterized-ert-map-product
                    #'list
                    :test-number (number-sequence 1 5)
                    :multiplier (number-sequence 2 10 2)))
  (should (cl-evenp (* test-number multiplier))))

;; Different behavior, but QuickCheck-style sampling also works.
(parameterized-ert-property-quickcheck
    (lambda (test-number multiplier) (* test-number multiplier))
    '(:test-number (integer 1 5) :multiplier (member 2 4 6 8 10))
  :max-success 100
  :test (lambda (actual &rest _) (cl-evenp actual)))
```

Ultimately, I believe a keyword-based API aligns more naturally with the existing `ert-deftest` than a separate tagged DSL, making it a better candidate for potential upstreaming.

[svjson/ert-parametrized.el]: https://github.com/svjson/ert-parametrized.el

## Copyright

This package is licensed under [GNU General Public License, version 3](https://www.gnu.org/licenses/gpl-3.0).

    parameterized-ert.el  Copyright (C) 2026  USAMI Kenta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

[ERT: Emacs Lisp Regression Testing]: https://www.gnu.org/software/emacs/manual/html_node/ert/
[Generators]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Generators.html
[QuickCheck]: https://en.wikipedia.org/wiki/QuickCheck
