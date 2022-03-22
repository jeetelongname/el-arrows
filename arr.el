;;; arr.el --- An implementaion of some threading macro's -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jeetaditya Chatterjee
;;
;; Author: Jeetaditya Chatterjee <https://github.com/jeetelongname>
;; Maintainer: Jeetaditya Chatterjee <jeetelongname@gmail.com>
;; Created: March 18, 2022
;; Modified: March 18, 2022
;; Version: 0.0.1
;; Keywords: convenience extensions lisp
;; Homepage: https://github.com/jeetelongname/el-arrows
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This package adds a bunch of threading macro's
;;
;;
;;; Code:
(require 'cl-lib)


;;; Normal threading macro's
;;;;;; Internal
(defun arr--simple-inserter (insert-fun)
  "Takes an INSERT-FUN. will return a builder function used to expand pipeline."
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fun acc next)
      (list next acc))))

(defun arr--insert-first (arg surround)
  "Insert ARG into the list form SURROUND as its first argument, after the operator."
  (cl-list* (car surround)
            arg
            (cdr surround)))

(defun arr--insert-last (arg surround)
  "Insert ARG into the list form SURROUND as its last argument."
  (append surround (list arg)))

;;;; Macro's
(defmacro arr-> (initial-form &rest forms)
  "Insert INITIAL-FORM as first argument into the first of FORMS.
The result into the next, etc., before evaluation.
FORMS are treated as list designators.
Identical in functionality to the builtin `thread-first'"
  (cl-reduce (arr--simple-inserter #'arr--insert-first)
             forms
             :initial-value initial-form))

(defmacro arr->> (initial-form &rest forms)
  "Like `arr->', but the INITIAL-FORM are inserted as last argument in FORMS.
Identical in functionality to the builtin `thread-last'"
  (cl-reduce (arr--simple-inserter #'arr--insert-last)
             forms
             :initial-value initial-form))

;;; Diamond macro's
;;;; Internal
(defun arr--diamond-inserter (insert-fun)
  "Takes an INSERT-FUN. will return a builder function used to expand pipeline.
Takes into account placeholders."
  (arr--simple-inserter (lambda (acc next)
                          (cl-case (cl-count-if #'arr--<>p next)
                            (0 (funcall insert-fun acc next))
                            (1 (cl-substitute-if acc #'arr--<>p next))
                            (t (let ((r (gensym "R")))
                                 `(let ((,r ,acc))
                                    ,(cl-substitute-if r #'arr--<>p next))))))))

(defun arr--<>p (form)
  "Predicate identifying the placeholders in FORMs for the -<> and -<>> macros."
  (and (symbolp form)
       (string= form "<>")))

(defmacro arr-<> (initial-form &rest forms)
  "Like `arr->' but FORMS can have placeholders `<>' in an arbitrary location.
This only applys to the top level and not in INITIAL-FORM.
Each such symbol is substituted by the primary result of the form
accumulated so far, instead of it being inserted as first argument.  Also known
as diamond wand."
  (cl-reduce (arr--diamond-inserter #'arr--insert-first)
             forms
             :initial-value initial-form))

(defmacro arr-<>> (initial-form &rest forms)
  "Like `arr->>' but FORMS can have placeholders `<>' in an arbitrary location.
This only applys to the top level and not in INITIAL-FORM.
Each such symbol is substituted by the primary result of the form
accumulated so far, instead of it being inserted as last argument.  Also known
as diamond spear."
  (cl-reduce (arr--diamond-inserter #'arr--insert-last)
             forms
             :initial-value initial-form))

;;; Maybe/nil short-circuiting macros
;;;; Internal
(defun arr--?-inserter (insert-fun)
  "Generate reduction function for nill short circeting function.
Takes INSERT-FUN to generate actual code."
  (lambda (acc next)
    (cl-destructuring-bind (let* bindings var) acc
      `(,let* (,@bindings
               (,var (when ,var
                       ,(funcall insert-fun var next))))
         ,var))))

(defun arr--expand-maybe (initial-form forms insert-fun)
  "Perform nil short circting reduction.
Acts like other reductions in that it takes an INITIAL-FORM
and threads it through FORMS at the direction of INSERT-FUN."
  (let ((var (gensym "maybe")))
    (cl-reduce (arr--?-inserter insert-fun)
               forms
               :initial-value `(let* ((,var ,initial-form))
                                 ,var))))

(defmacro arr-?> (initial-form &rest forms)
  "Like `arr->' but will short circut if any of FORMS, incuding INITIAL-FORM, if nil."
  (arr--expand-maybe initial-form forms (arr--simple-inserter #'arr--insert-first)))

(defmacro arr-?>> (initial-form &rest forms)
  "Like `arr->>' but will short circut if any of FORMS, incuding INITIAL-FORM, if nil."
  (arr--expand-maybe initial-form forms (arr--simple-inserter #'arr--insert-last)))

(defmacro arr-<?> (initial-form &rest forms)
  "Like `arr-<>' but will short circut if any of FORMS, incuding INITIAL-FORM, if nil."
  (arr--expand-maybe initial-form forms (arr--diamond-inserter #'arr--insert-first)))

(defmacro arr-<?>> (initial-form &rest forms)
  "Like `arr-<?>' but will short circut if any of FORMS, incuding INITIAL-FORM, if nil."
  (arr--expand-maybe initial-form forms (arr--diamond-inserter #'arr--insert-last)))

(defmacro arr->* (&rest forms)
  "Like `arr->' but the initial-form is passed in as the last in FORMS.
This is meant to be used in composition with `arr->>`,
where the argument is passed in as the last one.

Example:

    (arr->> 3
         (/ 12)
         (arr->* (/ 2)))
    => 2"
  `(arr-> ,@(append (last forms) (butlast forms))))

;;; fn varients

(defmacro arr-fn-> (&rest forms)
  "Return a `lambda' that takes in one argument and threads it through FORMS using `arr->'."
  `(lambda (x)
     (arr-> x ,@forms)))

(defmacro arr-fn->> (&rest forms)
  "Return a `lambda' that takes in one argument and threads it through FORMS using `arr->>'."
  `(lambda (x)
     (arr->> x ,@forms)))

(defmacro arr-fn-<> (&rest forms)
  "Return a `lambda' that takes in one argument and threads it through FORMS using `arr-<>'."
  `(lambda (x)
     (arr-<> x ,@forms)))

(defmacro arr-fn-<>> (&rest forms)
  "Return a `lambda' that takes in one argument and threads it through FORMS using `arr-<>>'."
  `(lambda (x)
     (arr-<>> x ,@forms)))

(defmacro arr-fn-?> (&rest forms)
  "Return a `lambda' that takes in one argument and threads it through FORMS using `arr-?>'."
  `(lambda (x)
     (arr-?> x ,@forms)))

(defmacro arr-fn-?>> (&rest forms)
  "Return a `lambda' that takes in one argument and threads it through FORMS using `arr-?>>'."
  `(lambda (x)
     (arr-?>> x ,@forms)))

(defmacro arr-fn-<?> (&rest forms)
  "Return a `lambda' that takes in one argument and threads it through FORMS using `arr-<?>'."
  `(lambda (x)
     (arr-<?> x ,@forms)))

(defmacro arr-fn-<?>> (&rest forms)
  "Return a `lambda' that takes in one argument and threads it through FORMS using `arr-<?>>'."
  `(lambda (x)
     (arr-<?>> x ,@forms)))

;;; helper functions
(cl-defun arr-inspect (value &optional &key print-fn label)
  "Like the `identity' function but will allow for printing of the VALUE.
Can have an optional LABEL to identify inspect calls.
Useful when debugging pipelines.
pass in your own PRINT-FN to use other interfaces.
Prints to *Messages* by default."
  (if print-fn
      (funcall print-fn value)
    (message "%s: %s" (if label "value" label) (prin1-to-string value)))
  value)

(provide 'arr)
;;; arr.el ends here
