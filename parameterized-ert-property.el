;;; parameterized-ert-property.el --- Property-based generators for parameterized-ert  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  USAMI Kenta

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

;; Property-based input generators for parameterized-ert.

;;; Code:
(require 'cl-lib)
(require 'parameterized-ert)

(defvar parameterized-ert-property-default-times 100
  "Default number of samples for property-based providers.")

(defvar parameterized-ert-property-default-integer-min -10
  "Default minimum integer used for unbounded integer types.")

(defvar parameterized-ert-property-default-integer-max 10
  "Default maximum integer used for unbounded integer types.")

(defvar parameterized-ert-property-default-float-min -10.0
  "Default minimum float used for unbounded float/real/number types.")

(defvar parameterized-ert-property-default-float-max 10.0
  "Default maximum float used for unbounded float/real/number types.")

(defvar parameterized-ert-property-max-retries 100
  "Maximum retries for generating a value that satisfies a type.")

(defvar parameterized-ert-property-default-string-min-length 0
  "Default minimum string length for generated strings.")

(defvar parameterized-ert-property-default-string-max-length 8
  "Default maximum string length for generated strings.")

(defvar parameterized-ert-property-default-string-char-min 32
  "Default minimum character code for generated strings.")

(defvar parameterized-ert-property-default-string-char-max 126
  "Default maximum character code for generated strings.")

(defun parameterized-ert-property--normalize-type (type)
  "Normalize quoted TYPE into its literal value."
  (if (and (consp type) (eq (car type) 'quote))
      (cadr type)
    type))

(defun parameterized-ert-property--normalize-spec (spec)
  "Normalize quoted SPEC into its literal value."
  (if (and (consp spec) (eq (car spec) 'quote))
      (cadr spec)
    spec))

(defun parameterized-ert-property--range-bounds (low high min max)
  "Return inclusive LOW/HIGH bounds with defaults using MIN/MAX.
LOW and HIGH can be `*' or a list like (N) to represent exclusive bounds."
  (let ((lower (if (eq low '*) min low))
        (upper (if (eq high '*) max high)))
    (when (and (consp lower) (eq (length lower) 1))
      (setq lower (1+ (car lower))))
    (when (and (consp upper) (eq (length upper) 1))
      (setq upper (1- (car upper))))
    (list lower upper)))

(defun parameterized-ert-property--random-integer (state min max)
  "Return a random integer between MIN and MAX, inclusive, using STATE."
  (let* ((range (1+ (- max min)))
         (offset (cl-random range state)))
    (+ min offset)))

(defun parameterized-ert-property--random-float (state min max)
  "Return a random float between MIN and MAX using STATE."
  (+ min (* (cl-random 1.0 state) (- max min))))

(defun parameterized-ert-property--random-string (state)
  "Return a random string using STATE."
  (let* ((length (parameterized-ert-property--random-integer
                  state
                  parameterized-ert-property-default-string-min-length
                  parameterized-ert-property-default-string-max-length))
         (chars (cl-loop repeat length
                         collect (parameterized-ert-property--random-integer
                                  state
                                  parameterized-ert-property-default-string-char-min
                                  parameterized-ert-property-default-string-char-max))))
    (apply #'string chars)))

(defun parameterized-ert-property--generator-for-type (type)
  "Return a generator function for TYPE."
  (setq type (parameterized-ert-property--normalize-type type))
  (pcase type
    ('t (lambda (state)
          (parameterized-ert-property--random-integer
           state
           parameterized-ert-property-default-integer-min
           parameterized-ert-property-default-integer-max)))
    ('nil (error "Cannot generate values for type nil"))
    ('null (lambda (_state) nil))
    ('atom (lambda (state)
             (parameterized-ert-property--random-integer
              state
              parameterized-ert-property-default-integer-min
              parameterized-ert-property-default-integer-max)))
    ('symbol
     (lambda (state)
       (make-symbol (parameterized-ert-property--random-string state))))
    ((or 'integer 'fixnum)
     (lambda (state)
       (parameterized-ert-property--random-integer
        state
        parameterized-ert-property-default-integer-min
        parameterized-ert-property-default-integer-max)))
    ((or 'real 'number)
     (lambda (state)
       (parameterized-ert-property--random-integer
        state
        parameterized-ert-property-default-integer-min
        parameterized-ert-property-default-integer-max)))
    ('float
     (lambda (state)
       (parameterized-ert-property--random-float
        state
        parameterized-ert-property-default-float-min
        parameterized-ert-property-default-float-max)))
    ((or 'character 'string-char)
     (lambda (state)
       (parameterized-ert-property--random-integer state 0 255)))
    ('string
     (lambda (state)
       (parameterized-ert-property--random-string state)))
    (`(integer ,low ,high)
     (pcase-let ((`(,lower ,upper)
                  (parameterized-ert-property--range-bounds
                   low high
                   parameterized-ert-property-default-integer-min
                   parameterized-ert-property-default-integer-max)))
       (lambda (state)
         (parameterized-ert-property--random-integer state lower upper))))
    (`(float ,low ,high)
     (pcase-let ((`(,lower ,upper)
                  (parameterized-ert-property--range-bounds
                   low high
                   parameterized-ert-property-default-float-min
                   parameterized-ert-property-default-float-max)))
       (lambda (state)
         (parameterized-ert-property--random-float state lower upper))))
    (`(real ,low ,high)
     (pcase-let ((`(,lower ,upper)
                  (parameterized-ert-property--range-bounds
                   low high
                   parameterized-ert-property-default-float-min
                   parameterized-ert-property-default-float-max)))
       (lambda (state)
         (parameterized-ert-property--random-float state lower upper))))
    (`(number ,low ,high)
     (pcase-let ((`(,lower ,upper)
                  (parameterized-ert-property--range-bounds
                   low high
                   parameterized-ert-property-default-float-min
                   parameterized-ert-property-default-float-max)))
       (lambda (state)
         (parameterized-ert-property--random-float state lower upper))))
    (`(member . ,values)
     (lambda (state)
       (nth (cl-random (length values) state) values)))
    (`(cl-member . ,values)
     (lambda (state)
       (nth (cl-random (length values) state) values)))
    (`(or . ,types)
     (let ((gens (mapcar #'parameterized-ert-property--generator-for-type types)))
       (lambda (state)
         (let ((gen (nth (cl-random (length gens) state) gens)))
           (funcall gen state)))))
    (`(and . ,types)
     (let ((gen (parameterized-ert-property--generator-for-type (car types))))
       (lambda (state)
         (cl-loop repeat parameterized-ert-property-max-retries
                  for value = (funcall gen state)
                  when (cl-typep value type)
                  return value
                  finally (error "Failed to generate value for type %S" type)))))
    (`(not ,subtype)
     (let ((gen (parameterized-ert-property--generator-for-type 't)))
       (lambda (state)
         (cl-loop repeat parameterized-ert-property-max-retries
                  for value = (funcall gen state)
                  unless (cl-typep value subtype)
                  return value
                  finally (error "Failed to generate value for type %S" type)))))
    (`(satisfies ,_pred)
     (error "Unsupported type for generation: %S" type))
    (_
     (error "Unsupported type for generation: %S" type))))

(defun parameterized-ert-property--generate-value (type state)
  "Generate a value for TYPE using STATE."
  (let ((gen (parameterized-ert-property--generator-for-type type)))
    (funcall gen state)))

(defun parameterized-ert-property--sample (spec times seed)
  "Return TIMES parameter set list for SPEC using SEED."
  (let* ((state (cl-make-random-state (or seed (sxhash (list spec times))))))
    (cl-loop repeat times
             collect (let ((pairs spec)
                           (result nil))
                       (while pairs
                         (let* ((key (pop pairs))
                                (type (pop pairs))
                                (value (parameterized-ert-property--generate-value type state)))
                           (setq result (plist-put result key value))))
                       result))))

(cl-defun parameterized-ert-property (spec &key times seed)
  "Return a provider function for SPEC.
SPEC is a plist of keyword/type pairs for `cl-typep' types.
TIMES controls the sample count; SEED fixes the random stream."
  (let ((count (or times parameterized-ert-property-default-times)))
    (lambda ()
      (parameterized-ert-property--sample spec count seed))))

(defun parameterized-ert-property--spec-keys (spec)
  "Return the keyword keys from SPEC."
  (let ((plist (parameterized-ert-property--normalize-spec spec)))
    (unless (and (listp plist) (cl-evenp (length plist)))
      (error "Property spec must be a plist: %S" spec))
    (cl-loop for key in plist by #'cddr collect key)))

(defun parameterized-ert-property--keyword-to-symbol (keyword)
  "Return a symbol created from KEYWORD."
  (unless (keywordp keyword)
    (error "Expected keyword, got: %S" keyword))
  (intern (substring (symbol-name keyword) 1)))

(defun parameterized-ert-property--quickcheck-name (property name)
  "Return a test NAME for PROPERTY.
PROPERTY should be a function symbol or a `(function SYMBOL)' form."
  (cond
   (name name)
   ((symbolp property)
    (intern (format "test-quickcheck-%s" property)))
   ((and (consp property)
         (eq (car property) 'function)
         (symbolp (cadr property)))
    (intern (format "test-quickcheck-%s" (cadr property))))
   (t
    (error "Provide :name for quickcheck property %S" property))))

(cl-defmacro parameterized-ert-property-quickcheck (property spec &key max-success seed name test)
  "Define a generated ERT test for PROPERTY and SPEC.
PROPERTY is a function or function symbol that should return non-nil.
SPEC is a plist of keyword/type pairs for `parameterized-ert-property'.
MAX-SUCCESS controls the number of samples, and SEED fixes the random stream.
NAME overrides the generated test name.
When TEST is non-nil, it is called as (TEST ACTUAL ARG1 ARG2 ...)."
  (declare (indent 2))
  (let* ((keys (parameterized-ert-property--spec-keys spec))
         (args (mapcar #'parameterized-ert-property--keyword-to-symbol keys))
         (test-name (parameterized-ert-property--quickcheck-name property name))
         (property-form (if (symbolp property)
                            `(function ,property)
                          property)))
    `(parameterized-ert-deftest ,test-name ,args
       :providers (list (parameterized-ert-property ,spec
                                                    :times ,max-success
                                                    :seed ,seed))
       (let ((result (apply ,property-form (list ,@args))))
         (if ,test
             (should (apply ,test (cons result (list ,@args))))
           result)))))

(provide 'parameterized-ert-property)
;;; parameterized-ert-property.el ends here
