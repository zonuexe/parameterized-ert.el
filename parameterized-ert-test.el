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
(require 'generator)
(require 'parameterized-ert)
(require 'parameterized-ert-property)

(ert-deftest test--product ()
  (should (equal '((1 a x)) (parameterized-ert--product '(1) '(a) '(x))))
  (should (equal '((1 a) (1 b) (2 a) (2 b)) (parameterized-ert--product '(1 2) '(a b))))
  (should (equal '((1 a x) (1 a y) (1 a z) (1 b x) (1 b y) (1 b z) (1 c x) (1 c y) (1 c z)
                   (2 a x) (2 a y) (2 a z) (2 b x) (2 b y) (2 b z) (2 c x) (2 c y) (2 c z)
                   (3 a x) (3 a y) (3 a z) (3 b x) (3 b y) (3 b z) (3 c x) (3 c y) (3 c z))
                 (parameterized-ert--product '(1 2 3) '(a b c) '(x y z)))))

(ert-deftest test--build-label-format ()
  (should (string= "" (parameterized-ert--build-label-format '())))
  (should (string=
           ":a %S :b %S"
           (parameterized-ert--build-label-format '(a b)))))

(ert-deftest test-parameterized-ert-macro ()
  (should (equal
           '(progn
              (setf (alist-get 'test-add parameterized-ert--tests)
                    (list :args '(expected a b)
                          :label ":expected %S :a %S :b %S"))
              (ert-deftest test-add ()
                "This is a test for addition."
                (cl-loop for (label expected a b) in (parameterized-ert-get-parameters 'test-add)
                         do (progn (should (eq expected (+ a b)))))))
           (macroexpand
            '(parameterized-ert-deftest test-add (expected a b)
               "This is a test for addition."
               (should (eq expected (+ a b))))))))

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

(ert-deftest test-parameterized-ert-property-provider ()
  (let* ((provider (parameterized-ert-property
                    '(:a 'integer :b '(integer 0 1))
                    :times 3 :seed 42))
         (sample-1 (funcall provider))
         (sample-2 (funcall provider)))
    (should (equal sample-1 sample-2))
    (dolist (params sample-1)
      (should (cl-typep (plist-get params :a) 'integer))
      (should (cl-typep (plist-get params :b) '(integer 0 1))))))

(ert-deftest test-parameterized-ert-map-product ()
  (should (equal
           '((:v1 a :v2 x)
             (:v1 a :v2 y)
             (:v1 b :v2 x)
             (:v1 b :v2 y))
           (funcall
            (parameterized-ert-map-product
             (lambda (v1 v2) (list :v1 v1 :v2 v2))
             :v1 '(a b) :v2 '(x y))))))

(ert-deftest test-parameterized-ert-map-zip ()
  (should (equal
           '((:expected 0 :input 0)
             (:expected 2 :input 1)
             (:expected 4 :input 2))
           (funcall
            (parameterized-ert-map-zip
             :expected (lambda (params) (let ((input (plist-get params :input)))
                                          (+ input input)))
             :input '(0 1 2))))))

(ert-deftest test-parameterized-ert-map-zip-no-lambda ()
  (should (equal
           '((:expected 0 :input 0)
             (:expected 2 :input 1)
             (:expected 4 :input 2))
           (funcall
            (parameterized-ert-map-zip
             :expected '(0 2 4)
             :input '(0 1 2))))))

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
                  (parameterized-ert-set-parameters
                   'test-add
                   '((:expected 2 :a 1 :b 1)))
                  (parameterized-ert-set-providers
                   'test-add
                   (list (lambda () '((:expected 3 :a 1 :b 2)))))
                  (ert-deftest test-add ()
                    ""
                    (cl-loop for (label expected a b) in (parameterized-ert-get-parameters 'test-add)
                             do (progn (should (eq expected (+ a b)))))))
               form)))))

(provide 'parameterized-ert-test)
;;; parameterized-ert-test.el ends here
