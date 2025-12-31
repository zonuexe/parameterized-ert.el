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
(require 'generator)
(eval-when-compile
  (require 'cl-lib))

(defvar parameterized-ert--tests '())

(defvar parameterized-ert--parameters '())

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

(defun parameterized-ert--normalize-entry (entry)
  "Normalize ENTRY into a plist with :params and :providers."
  (if (and (listp entry) (plist-member entry :params))
      entry
    (list :params entry :providers nil)))

(defun parameterized-ert--generator-to-list (generator)
  "Collect values from GENERATOR into a list."
  (let (items)
    (condition-case nil
        (while t
          (push (iter-next generator) items))
      (iter-end-of-sequence nil))
    (when (fboundp 'iter-close)
      (iter-close generator))
    (nreverse items)))

(defun parameterized-ert--provider-output-as-list (value)
  "Normalize provider output VALUE into a list."
  (cond
   ((null value) nil)
   ((listp value) value)
   ((functionp value)
    (parameterized-ert--provider-output-as-list (funcall value)))
   (t
    (parameterized-ert--generator-to-list value))))

(defun parameterized-ert--normalize-type (type)
  "Normalize quoted TYPE into its literal value."
  (if (and (consp type) (eq (car type) 'quote))
      (cadr type)
    type))

(defun parameterized-ert--range-bounds (low high min max)
  "Return inclusive LOW/HIGH bounds with defaults.
LOW and HIGH can be `*' or a list like (N) to represent exclusive bounds."
  (let ((lower (if (eq low '*) min low))
        (upper (if (eq high '*) max high)))
    (when (and (consp lower) (eq (length lower) 1))
      (setq lower (1+ (car lower))))
    (when (and (consp upper) (eq (length upper) 1))
      (setq upper (1- (car upper))))
    (list lower upper)))

(defun parameterized-ert--random-integer (state min max)
  "Return a random integer between MIN and MAX, inclusive."
  (let* ((range (1+ (- max min)))
         (offset (random range state)))
    (+ min offset)))

(defun parameterized-ert--random-float (state min max)
  "Return a random float between MIN and MAX."
  (let* ((scale 1000000.0)
         (offset (random (truncate scale) state)))
    (+ min (* (/ offset scale) (- max min)))))

(defun parameterized-ert--generator-for-type (type)
  "Return a generator function for TYPE."
  (setq type (parameterized-ert--normalize-type type))
  (pcase type
    ('t (lambda (state)
          (parameterized-ert--random-integer
           state
           parameterized-ert-property-default-integer-min
           parameterized-ert-property-default-integer-max)))
    ('nil (error "Cannot generate values for type nil"))
    ('null (lambda (_state) nil))
    ('atom (lambda (state)
             (parameterized-ert--random-integer
              state
              parameterized-ert-property-default-integer-min
              parameterized-ert-property-default-integer-max)))
    ((or 'integer 'fixnum)
     (lambda (state)
       (parameterized-ert--random-integer
        state
        parameterized-ert-property-default-integer-min
        parameterized-ert-property-default-integer-max)))
    ((or 'real 'number)
     (lambda (state)
       (parameterized-ert--random-integer
        state
        parameterized-ert-property-default-integer-min
        parameterized-ert-property-default-integer-max)))
    ('float
     (lambda (state)
       (parameterized-ert--random-float
        state
        parameterized-ert-property-default-float-min
        parameterized-ert-property-default-float-max)))
    ((or 'character 'string-char)
     (lambda (state)
       (parameterized-ert--random-integer state 0 255)))
    (`(integer ,low ,high)
     (pcase-let ((`(,lower ,upper)
                  (parameterized-ert--range-bounds
                   low high
                   parameterized-ert-property-default-integer-min
                   parameterized-ert-property-default-integer-max)))
       (lambda (state)
         (parameterized-ert--random-integer state lower upper))))
    (`(float ,low ,high)
     (pcase-let ((`(,lower ,upper)
                  (parameterized-ert--range-bounds
                   low high
                   parameterized-ert-property-default-float-min
                   parameterized-ert-property-default-float-max)))
       (lambda (state)
         (parameterized-ert--random-float state lower upper))))
    (`(real ,low ,high)
     (pcase-let ((`(,lower ,upper)
                  (parameterized-ert--range-bounds
                   low high
                   parameterized-ert-property-default-float-min
                   parameterized-ert-property-default-float-max)))
       (lambda (state)
         (parameterized-ert--random-float state lower upper))))
    (`(number ,low ,high)
     (pcase-let ((`(,lower ,upper)
                  (parameterized-ert--range-bounds
                   low high
                   parameterized-ert-property-default-float-min
                   parameterized-ert-property-default-float-max)))
       (lambda (state)
         (parameterized-ert--random-float state lower upper))))
    (`(member . ,values)
     (lambda (state)
       (nth (random (length values) state) values)))
    (`(cl-member . ,values)
     (lambda (state)
       (nth (random (length values) state) values)))
    (`(or . ,types)
     (let ((gens (mapcar #'parameterized-ert--generator-for-type types)))
       (lambda (state)
         (let ((gen (nth (random (length gens) state) gens)))
           (funcall gen state)))))
    (`(and . ,types)
     (let ((gen (parameterized-ert--generator-for-type (car types))))
       (lambda (state)
         (cl-loop repeat parameterized-ert-property-max-retries
                  for value = (funcall gen state)
                  when (cl-typep value type)
                  return value
                  finally (error "Failed to generate value for type %S" type)))))
    (`(not ,subtype)
     (let ((gen (parameterized-ert--generator-for-type 't)))
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

(defun parameterized-ert--generate-value (type state)
  "Generate a value for TYPE using STATE."
  (let ((gen (parameterized-ert--generator-for-type type)))
    (funcall gen state)))

(defun parameterized-ert--property-sample (spec times seed)
  "Return TIMES parameter sets for SPEC using SEED."
  (let* ((state (make-random-state (or seed (sxhash (list spec times))))))
    (cl-loop repeat times
             collect (let ((pairs spec)
                           (result nil))
                       (while pairs
                         (let* ((key (pop pairs))
                                (type (pop pairs))
                                (value (parameterized-ert--generate-value type state)))
                           (setq result (plist-put result key value))))
                       result))))

(cl-defun parameterized-ert-property (spec &key times seed)
  "Return a provider function for SPEC.
SPEC is a plist of keyword/type pairs for `cl-typep' types.
TIMES controls the sample count; SEED fixes the random stream."
  (let ((count (or times parameterized-ert-property-default-times)))
    (lambda ()
      (parameterized-ert--property-sample spec count seed))))

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

(defun parameterized-ert--add-parameters (name parameters current)
  "Return CURRENT with PARAMETERS for NAME merged in."
  (let* ((data (alist-get name parameterized-ert--tests))
         (arguments (plist-get data :args))
         (label-format (plist-get data :label))
         (expected-length (length arguments))
         (result (or current '())))
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
                  (setf (alist-get label result) param-list)))
    result))

(defun parameterized-ert-add-parameter (name parameter)
  "Register a single PARAMETER for the parameterized test NAME."
  (parameterized-ert-add-parameters name (list parameter)))

(defun parameterized-ert-add-parameters (name parameters)
  "Register PARAMETERS for the parameterized test NAME."
  (cl-check-type name symbol)
  (cl-check-type parameters list)
  (let* ((entry (parameterized-ert--normalize-entry
                 (alist-get name parameterized-ert--parameters)))
         (params (parameterized-ert--add-parameters
                  name parameters (plist-get entry :params))))
    (setf (alist-get name parameterized-ert--parameters)
          (plist-put entry :params params))))

(defun parameterized-ert-add-provider (name provider)
  "Register a lazy PROVIDER function for the parameterized test NAME."
  (parameterized-ert-add-providers name (list provider)))

(defun parameterized-ert-add-providers (name providers)
  "Register PROVIDERS as lazy parameter functions for NAME."
  (cl-check-type name symbol)
  (cl-check-type providers list)
  (unless (cl-every #'functionp providers)
    (error "Providers must be functions: %S" providers))
  (let* ((entry (parameterized-ert--normalize-entry
                 (alist-get name parameterized-ert--parameters)))
         (current (plist-get entry :providers)))
    (setf (alist-get name parameterized-ert--parameters)
          (plist-put entry :providers (append current providers)))))

(defun parameterized-ert-provide (name parameters)
  "Register PARAMETERS for the parameterized test NAME.
PARAMETERS can be a list of parameter specs or a provider function.
A provider function is evaluated lazily when parameters are requested.
Provider functions may return lists or generator objects."
  (cond
   ((functionp parameters)
    (parameterized-ert-add-provider name parameters))
   ((listp parameters)
    (parameterized-ert-add-parameters name parameters))
   (t
    (error "Unexpected parameters: %S" parameters))))

(defun parameterized-ert-get-parameters (name)
  "Return the parameter list for NAME as (LABEL . VALUES) entries."
  (let* ((entry (parameterized-ert--normalize-entry
                 (alist-get name parameterized-ert--parameters)))
         (providers (plist-get entry :providers))
         (params (plist-get entry :params)))
    (when providers
      (setq params (parameterized-ert--add-parameters
                    name
                    (apply #'append
                           (mapcar (lambda (provider)
                                     (parameterized-ert--provider-output-as-list
                                      (funcall provider)))
                                   providers))
                    params))
      (setq entry (plist-put entry :params params))
      (setq entry (plist-put entry :providers nil))
      (setf (alist-get name parameterized-ert--parameters) entry))
    (cl-loop for (label . values) in (or params '())
             collect (cons label values))))

(cl-defmacro parameterized-ert-deftest (name args &body docstring-keys-and-body)
  "Define a parameterized ERT test NAME with ARGS.
DOCSTRING-KEYS-AND-BODY accepts the same format as `ert-deftest', with an
optional :label keyword to override the default label format.
It also supports :parameters and :providers to register inputs."
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
         (parameters nil)
         (providers nil)
         (ert-keys '()))
    (while keys
      (let ((key (pop keys))
            (value (pop keys)))
        (if (eq key :label)
            (setq label-format value)
          (cond
           ((eq key :parameters)
            (setq parameters value))
           ((eq key :providers)
            (setq providers value))
           (t
            (setq ert-keys (append ert-keys (list key value))))))))
    (unless label-format
      (setq label-format (parameterized-ert--build-label-format args)))
    `(progn
       (setf (alist-get ',name parameterized-ert--tests)
             (list :args ',args :label ,label-format))
       ,@(when parameters
           `((parameterized-ert-add-parameters ',name ,parameters)))
       ,@(when providers
           `((parameterized-ert-add-providers ',name ,providers)))
       (ert-deftest ,name ()
         ,@(when docstring (list docstring))
         ,@ert-keys
         (cl-loop for (label ,@args) in (parameterized-ert-get-parameters ',name)
                  do (progn ,@body))))))

(provide 'parameterized-ert)
;;; parameterized-ert.el ends here
