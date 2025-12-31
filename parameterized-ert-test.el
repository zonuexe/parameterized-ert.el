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
(require 'parameterized-ert)

(ert-deftest test--build-label-format ()
  (should (string= "" (parameterized-ert--build-label-format '())))
  (should (string=
           ":a %S :b %S"
           (parameterized-ert--build-label-format '(a b)))))

(ert-deftest test-parameterized-ert-macro ()
  (should (equal
           '(ert-deftest test-add ()
              )
           (parameterized-ert--deftest-1 'test-add '(expected a b) '("" '(should (eq expected (+ a b))))))))

(provide 'parameterized-ert-test)
;;; parameterized-ert-test.el ends here
