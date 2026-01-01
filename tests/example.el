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
(require 'parameterized-ert-property)
(require 'generator)

;; Register a test definition (normally done by the macro).
;; (setq parameterized-ert--tests
;;       '((test-add :label ":expected %S :a %S :b %S" :args (expected a b))))

;; (plist-get (alist-get 'test-add parameterized-ert--tests) :args)

;; Register parameter sets (label + values).
;; (setq parameterized-ert--parameters '((test-add (":expected 4 :a 2 :b 2" 4 2 2))))

;; Minimal parameterized test definition.
(parameterized-ert-deftest test-add (expected a b)
  ""
  :parameters `((:expected -2 :a 1 :b -3)
                (:expected 2 :a ,(abs -1) :b ,(abs -1)))
  :providers (list #'example-add-provider #'example-add-provider-2
                   (iter-lambda () (iter-yield (list :expected 10 :a 4 :b 6))))
  (should (eq expected (+ a b))))

(parameterized-ert-deftest test-plus1 (expected input)
  ""
  :providers (list (parameterized-ert-xxx
                    :expected (number-sequence 1 6)
                    :input (number-sequence 0 5)))
  (should (eq expected (1+ input))))

(parameterized-ert-deftest test-cons-multi (expected v1 v2 v3)
  ""
  :providers (list (parameterized-ert-map-product
                    (lambda (v1 v2 v3) (list v1 v2 v3))
                    :v1 '(a b)
                    :v2 '(x y)
                    :v3 '(1 2)))
  (should (eq expected (cons v1 (cons v2 (cons v2 nil))))))

(parameterized-ert-deftest test-twice (expected input)
  ""
  :parameterize-continue-on-failure nil
  :providers (list (parameterized-ert-map-zip
                    :expected (lambda (params) (let ((input (plist-get params :input)))
                                                 (+ input input)))
                    :input (number-sequence 0 5)))
  (should (eq expected (* 2 input))))

(parameterized-ert-deftest test-add-property (a b)
  ""
  :providers (list (parameterized-ert-property '(:a 'integer :b 'integer) :times 100))
  (should (eq (+ a b) (+ b a))))

(parameterized-ert-property-quickcheck #'identity '(:argument (or integer float string symbol))
  :test #'eq
  :max-success 10000)

(parameterized-ert-property-quickcheck #'reverse '(:argument (member nil (1 2 3) (a b c) (1) (x y)))
   :max-success 20
   :test (lambda (actual xs) (equal xs (reverse actual))))

;; Equivalent expanded ERT test.
;; (ert-deftest test-add ()
;;   (cl-loop for (label expected a b) in (parameterized-ert-get-parameters 'test-add)
;;            do (should (equal (list label expected)
;;                              (list label (+ a b))))))

(defun example-add-provider ()
  "Return parameter sets for `test-add'."
  (cl-loop for a from 0 upto 2
           append
           (cl-loop for b from 0 upto 2
                    collect (list :expected (+ a b) :a a :b b))))

(iter-defun example-add-provider-2 ()
  "Yield parameter sets for `test-add'."
  (cl-loop for a from 5 upto 6
           do (cl-loop for b from 10 upto 11
                       do (iter-yield (list :expected (+ a b) :a a :b b)))))

;; Provide parameters using keyword/value pairs.
(parameterized-ert-add-parameter 'test-add '(:expected 2 :a 1 :b 1))
(parameterized-ert-set-parameters 'test-add '((:expected 3 :a 1 :b 2)))
;; Register lazy parameter provider functions.
(parameterized-ert-add-provider 'test-add #'example-add-provider)
(parameterized-ert-set-providers 'test-add (list #'example-add-provider-2))

;; wrong data
;; (parameterized-ert-provide 'test-add '((:expected 4 :a 1 :b 1)))

;;; example.el ends here
