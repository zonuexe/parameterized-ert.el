# parameterized-ert.el

## Overview

This package adds a small parameterized layer on top of [ERT: Emacs Lisp
Regression Testing]. It provides a wrapper macro and provider helpers so
you can write concise multi-case tests.

Without parameterization you often write boilerplate:

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

With `parameterized-ert-deftest` the arguments are injected directly:

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

A **provider** is a function that returns either a list of parameter plists
or a [generator][Generators].

Use these keywords inside `parameterized-ert-deftest`:
- `:providers` takes a list of provider functions. Each entry can be a function
  symbol, an inline `lambda`, or a provider helper call (which returns a lambda).
- `:parameters` takes a list of parameter entries. You can pass plists
  (`(:expected 2 :a 1 :b 1)`) or positional value lists (`(2 1 1)`); pick the
  style that keeps the test readable.
- `:parameterize-continue-on-failure` controls failure aggregation. When non-nil,
  all cases run and failures are reported together; when nil, the first failure
  stops the test.

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

Property-based testing focuses on *properties* that should hold for many inputs,
instead of enumerating fixed examples. In `parameterized-ert`, a QuickCheck-style
helper generates inputs and runs the same test repeatedly.

`parameterized-ert-property` creates a provider from a plist of `cl-typep` types.
Use it when you want randomized inputs instead of fixed examples.

```elisp
(parameterized-ert-deftest test-add-property (a b)
  :providers (list (parameterized-ert-property
                    '(:a 'integer :b '(integer 0 10))
                    :times 100 :seed 42))
  (should (eq (+ a b) (+ b a))))
```

For a [QuickCheck]-style wrapper, use `parameterized-ert-property-quickcheck`,
which defines a generated ERT test for a predicate function. If you omit
`:test`, the check still verifies that the property runs without signaling
an error for each generated input. When you provide `:test`, it validates the
returned value. The `:test` function receives the computed result first,
followed by the generated arguments.

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

Use `parameterized-ert-map-zip` to build a provider from plist keys.
List values are zipped by index. Function values receive the current plist
and return a single derived value.

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

`parameterized-ert-map-product` maps a function over the Cartesian product
of lists and returns the produced parameter plists.

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

## Design Decisions

When I started this package during winter break, I learned that
[svjson/ert-parametrized.el] and the [Reddit thread][ert-parametrized.el r/emacs]
had been published just a month earlier. I have long believed ERT needed
parameterization, so I am grateful for that work. After reading its README,
I realized many of its design choices differ from mine. Those differences are
significant enough that a PR would not reconcile them, so I note them here.

I also want to highlight a few strengths of `ert-parametrized.el`:
- Each case becomes its own ERT test, which makes failures easy to spot.
- Case names can map to test names, making targeted runs and filtering simple.
- The DSL is explicit and consistent, which can help teams standardize style.
- Data generation intent is visible at a glance (`:generator`, `:eval`), which
  can be approachable for users coming from other xUnit frameworks.

- Core model
  - `ert-parametrized.el`: expands into many ERT tests at macroexpansion time;
    each case becomes a separate test name.
  - `parameterized-ert`: loops parameters inside a single ERT test; cases are
    distinguished by labels.
- Case representation
  - `ert-parametrized.el`: tag-based DSL using `:fun`, `:eval`, `:quote`,
    `:generator`, and related forms.
  - `parameterized-ert`: plain Emacs Lisp plus `:parameters`, `:providers`,
    and provider helpers.
- Data generation
  - `ert-parametrized.el`: `:generator` is `eval`ed at macroexpansion; Cartesian
    matrices are built at expansion time.
  - `parameterized-ert`: providers run at runtime; generators are lazy.
- Test naming and identity
  - `ert-parametrized.el`: sanitizes case names into individual test names.
  - `parameterized-ert`: one test name; cases are labeled.
- Extensibility
  - `ert-parametrized.el`: explicit DSL where "case = test".
  - `parameterized-ert`: enumerate `:parameters` and extend with `:providers`
    plus helper functions.
- Relationship between tests and parameters
  - `ert-parametrized.el`: parameters are fixed at expansion time.
  - `parameterized-ert`: parameters are resolved at runtime; property tests use
    `seed` for reproducibility.
- Runtime code footprint
  - `ert-parametrized.el`: expands into many small `ert-deftest` forms, with no
    shared runtime loop.
  - `parameterized-ert`: emits a single test that loops at runtime, so the
    macro expansion is smaller but the runtime loop is always present.

"Many specialized static tests" versus "one test that loops many parameters" is
a trade-off. When running `make test` in a terminal, the difference is small,
but keeping a single test makes it easier to run all parameterized cases from
`M-x ert`.

Personally, I find the DSL's evaluation model to be less idiomatic for Emacs
Lisp, and the frequent `:eval`/`:quote` tags add visual noise.

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

This may be called [TMTOWTDI][] ("there's more than one way to do it"), but the
point is not a DSL: it is just Emacs Lisp. Keeping the style consistent is up
to the test author.

This package does not have a dedicated "matrix" syntax, but the combination of
`:providers` and helper functions covers that use case.

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

Finally, if parameterized tests are ever discussed for upstream ERT, I believe
an API based on argument lists and `:keyword` options aligns more naturally with
`ert-deftest` than a separate tagged DSL.

[TMTOWTDI]: https://en.wiktionary.org/wiki/TMTOWTDI
[svjson/ert-parametrized.el]: https://github.com/svjson/ert-parametrized.el
[ert-parametrized.el r/emacs]: https://www.reddit.com/r/emacs/comments/1pfvj6y/ertparametrizedel_parametrized_test_macros_for_ert/


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
