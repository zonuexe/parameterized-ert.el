;;; parameterized-ert-test.el --- Test case for parameterized-ert  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: maint

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

;; Tests for parameterized-ert macros and parameter registration helpers.

;;; Code:
(require 'ert)
(require 'cond-star)
(require 'generator)
(require 'parameterized-ert)

(ert-deftest test--build-label-format ()
  (should (string= "" (parameterized-ert--build-label-format '())))
  (should (string=
           ":a %S :b %S"
           (parameterized-ert--build-label-format '(a b)))))

(ert-deftest test-parameterized-ert-macro ()
  (let ((form (macroexpand
               '(parameterized-ert-deftest test-add (expected a b)
                  "This is a test for addition."
                  (should (eq expected (+ a b)))))))
    (cond*
     ((match* (cons 'progn rest) form))
     ((match* (list setf-form deftest) rest))
     ((pcase* `(ert-deftest ,test-name () ,docstring . ,body) deftest)
      (should (equal
               '(setf (alist-get 'test-add parameterized-ert--tests)
                      (list :args '(expected a b)
                            :label ":expected %S :a %S :b %S"))
               setf-form))
      (should (eq 'test-add test-name))
      (should (string= "This is a test for addition." docstring))
      (should (equal
               '((cl-loop for (label expected a b) in (parameterized-ert-get-parameters 'test-add)
                          do (progn (should (eq expected (+ a b))))))
               body)))
     (t
      (ert-fail (format "Unexpected macro expansion: %S" form))))))

(ert-deftest test-parameterized-ert-provider-lazy ()
  (let ((parameterized-ert--tests '())
        (parameterized-ert--parameters '())
        (counter 0))
    (setf (alist-get 'lazy-test parameterized-ert--tests)
          (list :args '(a b)
                :label (parameterized-ert--build-label-format '(a b))))
    (parameterized-ert-add-provider
     'lazy-test
     (lambda ()
       (setq counter (1+ counter))
       '((:a 1 :b 2))))
    (should (eq 0 counter))
    (should (equal '((":a 1 :b 2" 1 2))
                   (parameterized-ert-get-parameters 'lazy-test)))
    (should (eq 1 counter))
    (should (equal '((":a 1 :b 2" 1 2))
                   (parameterized-ert-get-parameters 'lazy-test)))
    (should (eq 1 counter))))

(iter-defun parameterized-ert--test-provider-generator ()
  (iter-yield '(:a 3 :b 4)))

(ert-deftest test-parameterized-ert-provider-generator ()
  (let ((parameterized-ert--tests '())
        (parameterized-ert--parameters '()))
    (setf (alist-get 'gen-test parameterized-ert--tests)
          (list :args '(a b)
                :label (parameterized-ert--build-label-format '(a b))))
    (parameterized-ert-add-provider
     'gen-test
     #'parameterized-ert--test-provider-generator)
    (should (equal '((":a 3 :b 4" 3 4))
                   (parameterized-ert-get-parameters 'gen-test)))))

(ert-deftest test-parameterized-ert-macro-parameters-and-providers ()
  (let ((parameterized-ert--tests '())
        (parameterized-ert--parameters '()))
    (let ((form (macroexpand
                 '(parameterized-ert-deftest test-add (expected a b)
                    ""
                    :parameters '((:expected 2 :a 1 :b 1))
                    :providers (list (lambda () '((:expected 3 :a 1 :b 2))))
                    (should (eq expected (+ a b)))))))
      (should (equal
               '(progn
                  (setf (alist-get 'test-add parameterized-ert--tests)
                        (list :args '(expected a b)
                              :label ":expected %S :a %S :b %S"))
                  (parameterized-ert-add-parameters
                   'test-add
                   '((:expected 2 :a 1 :b 1)))
                  (parameterized-ert-add-providers
                   'test-add
                   (list (lambda () '((:expected 3 :a 1 :b 2)))))
                  (ert-deftest test-add ()
                    ""
                    (cl-loop for (label expected a b) in (parameterized-ert-get-parameters 'test-add)
                             do (progn (should (eq expected (+ a b)))))))
               form)))))

(provide 'parameterized-ert-test)
;;; parameterized-ert-test.el ends here
