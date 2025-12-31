;;; example.el --- Code example for parameterized-ert  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple walkthrough of registering parameters and expanding a
;; parameterized-ert test into a plain ERT test.

;;; Code:

(require 'parameterized-ert)

;; Register a test definition (normally done by the macro).
(setq parameterized-ert--tests
      '((test-add :label ":expected %S :a %S :b %S" :args (expected a b))))

(plist-get (alist-get 'test-add parameterized-ert--tests) :args)

;; Register parameter sets (label + values).
(setq parameterized-ert--parameters '((test-add (":expected 4 :a 2 :b 2" 4 2 2))))

;; Minimal parameterized test definition.
(parameterized-ert-deftest test-add (expected a b)
  ""
  (should (eq expected (+ a b))))

;; Equivalent expanded ERT test.
(ert-deftest test-add ()
  (cl-loop for (label expected a b) in (parameterized-ert-get-parameters 'test-add)
           do (should (equal (list label expected)
                             (list label (+ a b))))))

;; Provide parameters using keyword/value pairs.
(parameterized-ert-provide 'test-add '((:expected 2 :a 1 :b 1)))

;;; example.el ends here
