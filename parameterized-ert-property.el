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

(defun parameterized-ert-property--normalize-type (type)
  "Normalize quoted TYPE into its literal value."
  (if (and (consp type) (eq (car type) 'quote))
      (cadr type)
    type))

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

(provide 'parameterized-ert-property)
;;; parameterized-ert-property.el ends here
