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
