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

(defun arr--simple-inserter (insert-fun)
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

(defmacro arr-> (initial-form &rest forms)
  "Insert INITIAL-FORM as first argument into the first of FORMS.
The result into the next, etc., before evaluation.  FORMS are treated as list designators.
Identical in functionality to the builtin `thread-first'"
  (cl-reduce (arr--simple-inserter #'arr--insert-first)
             forms
             :initial-value initial-form))

(defmacro arr->> (initial-form &rest forms)
  "Like ->, but the INITIAL-FORM are inserted as last argument in FORMS.
Identical in functionality to the builtin `thread-last'"
  (cl-reduce (arr--simple-inserter #'arr--insert-last)
             forms
             :initial-value initial-form))

(defun arr--diamond-inserter (insert-fun)
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
  "Like ->, but if a form in FORMS has one or more symbols named <> as top-level element.
Each such symbol is substituted by the primary result of the form
accumulated so far, instead of it being inserted as first argument.  Also known
as diamond wand."
  (cl-reduce (arr--diamond-inserter #'arr--insert-first)
             forms
             :initial-value initial-form))

(defmacro arr-<>> (initial-form &rest forms)
  "Like -<>, but if a form has no symbol named <>, the insertion is done at the
end like in ->>.  Also known as diamond spear."
  (cl-reduce (arr--diamond-inserter #'arr--insert-last)
             forms
             :initial-value initial-form))

(defmacro arr-lambda-> (&rest forms)
  "Return a lambda for FORMS."
  `(lambda (x)
     (arr-> x ,@forms)))

(provide 'arr)
;;; arr.el ends here
