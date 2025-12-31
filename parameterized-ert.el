;;; parameterized-ert.el --- Parameterized testing extension for ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: maint, lisp
;; Homepage: https://github.com/zonuexe/parameterized-ert.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; License: GPL-3.0-or-later

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

;; Provide a parameterized test macro for ERT and helpers to register
;; labeled parameter sets for each test.

;;; Code:
(require 'ert)
(eval-when-compile
  (require 'cl-lib))

(defvar parameterized-ert--tests '())

(defvar parameterized-ert--parameters '())

(defun parameterized-ert--split-docstring-keys-body (docstring-keys-and-body)
  "Split DOCSTRING-KEYS-AND-BODY into docstring, keyword list, and body.
Return a list of the form (DOCSTRING KEYS BODY), where DOCSTRING can be nil,
KEYS is a flat keyword/value list, and BODY is the remaining forms."
  (let ((rest docstring-keys-and-body)
        docstring
        keys)
    (when (stringp (car rest))
      (setq docstring (car rest))
      (setq rest (cdr rest)))
    (while (keywordp (car rest))
      (let ((key (car rest))
            (value (cadr rest)))
        (setq rest (cddr rest))
        (push key keys)
        (push value keys)))
    (list docstring (nreverse keys) rest)))

(defun parameterized-ert--build-label-format (arguments)
  "Build a default label format string from ARGUMENTS.
Each argument name becomes a \":name %S\" segment."
  (mapconcat (lambda (n) (format "%s %%S" (intern (concat ":" (symbol-name n))))) arguments " "))

(defun parameterized-ert--compose-param-list (arguments parameters)
  "Compose argument values for ARGUMENTS from plist PARAMETERS.
Accepts either keyword or symbol keys in PARAMETERS."
  (cl-loop for name in arguments
           for const = (intern (concat ":" (symbol-name name)))
           collect (or (plist-get parameters const) (plist-get parameters name))))

(defun parameterized-ert-provide (name parameters)
  "Register PARAMETERS for the parameterized test NAME.
PARAMETERS is a list of parameter specs, each providing values for the
argument list registered by `parameterized-ert-deftest'."
  (cl-check-type name symbol)
  (cl-check-type parameters list)
  (let* ((data (alist-get name parameterized-ert--tests))
         (arguments (plist-get data :args))
         (label-format (plist-get data :label))
         (expected-length (length arguments)))
    (cl-loop for param in parameters
             for param-length = (length param)
             do (let (label param-list)
                  (cond ((eq param-length expected-length)
                         (setq param-list param))
                        ((eq param-length (* 2 expected-length))
                         (setq param-list (parameterized-ert--compose-param-list arguments param)))
                        ((eq param-length (1+ (* 2 expected-length)))
                         (setq label (car param))
                         (setq param-list (parameterized-ert--compose-param-list arguments (cdr param))))
                        ((error "Unexpected parameter: %S" param)))
                  (unless label
                    (setq label (apply #'format label-format param-list)))
                  (let ((current (alist-get name parameterized-ert--parameters)))
                    (setf (alist-get label current) param-list)
                    (setf (alist-get name parameterized-ert--parameters) current))))))

(defun parameterized-ert-get-parameters (name)
  "Return the parameter list for NAME as (LABEL . VALUES) entries."
  (let ((params (alist-get name parameterized-ert--parameters)))
    (cl-loop for (label . values) in params
             collect (cons label values))))

(cl-defmacro parameterized-ert-deftest (name args &body docstring-keys-and-body)
  "Define a parameterized ERT test NAME with ARGS.
DOCSTRING-KEYS-AND-BODY accepts the same format as `ert-deftest', with an
optional :label keyword to override the default label format."
  (declare (debug (&define [&name "test@p11d" symbolp]
			   sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  (when (memq 'label args)
    (error "parameterized-ert-deftest: `label` is reserved for the generated label"))
  (let* ((split (parameterized-ert--split-docstring-keys-body docstring-keys-and-body))
         (docstring (nth 0 split))
         (keys (nth 1 split))
         (body (nth 2 split))
         (label-format nil)
         (ert-keys '()))
    (while keys
      (let ((key (pop keys))
            (value (pop keys)))
        (if (eq key :label)
            (setq label-format value)
          (setq ert-keys (append ert-keys (list key value))))))
    (unless label-format
      (setq label-format (parameterized-ert--build-label-format args)))
    `(progn
       (setf (alist-get ',name parameterized-ert--tests)
             (list :args ',args :label ,label-format))
       (ert-deftest ,name ()
         ,@(when docstring (list docstring))
         ,@ert-keys
         (cl-loop for (label ,@args) in (parameterized-ert-get-parameters ',name)
                  do (progn ,@body))))))

(provide 'parameterized-ert)
;;; parameterized-ert.el ends here
