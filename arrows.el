;;; arrows.el --- An implementaion of some threading macro's -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jeetaditya Chatterjee
;;
;; Author: Jeetaditya Chatterjee <https://github.com/jeetelongname>
;; Maintainer: Jeetaditya Chatterjee <jeetelongname@gmail.com>
;; Created: March 18, 2022
;; Modified: March 18, 2022
;; Version: 0.0.1
;; Keywords: convenience extensions lisp
;; Homepage: https://github.com/jeetelongname/arrows
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

(defun arrows--simple-inserter (insert-fun)
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fun acc next)
      (list next acc))))

(defun arrows--insert-first (arg surround)
  "Insert ARG into the list form SURROUND as its first argument, after the operator."
  (cl-list* (car surround)
            arg
            (cdr surround)))

(defun arrows--insert-last (arg surround)
  "Insert ARG into the list form SURROUND as its last argument."
  (append surround (list arg)))

(defmacro arrows-> (initial-form &rest forms)
  "Insert INITIAL-FORM as first argument into the first of FORMS.
The result into the next, etc., before evaluation.  FORMS are treated as list designators.
Identical in functionality to the builtin `thread-first'"
  (cl-reduce (arrows--simple-inserter #'arrows--insert-first)
             forms
             :initial-value initial-form))

(defmacro arrows->> (initial-form &rest forms)
  "Like ->, but the INITIAL-FORM are inserted as last argument in FORMS.
Identical in functionality to the builtin `thread-last'"
  (cl-reduce (arrows--simple-inserter #'arrows--insert-last)
             forms
             :initial-value initial-form))

(defun arrows--diamond-inserter (insert-fun)
  (arrows--simple-inserter (lambda (acc next)
                     (cl-case (cl-count-if #'arrows--<>p next)
                       (0 (funcall insert-fun acc next))
                       (1 (cl-substitute-if acc #'arrows--<>p next))
                       (t (let ((r (gensym "R")))
                            `(let ((,r ,acc))
                               ,(cl-substitute-if r #'arrows--<>p next))))))))

(defun arrows--<>p (form)
  "Predicate identifying the placeholders in FORMs for the -<> and -<>> macros."
  (and (symbolp form)
       (string= form "<>")))

(defmacro arrows-<> (initial-form &rest forms)
  "Like ->, but if a form in FORMS has one or more symbols named <> as top-level element.
Each such symbol is substituted by the primary result of the form
accumulated so far, instead of it being inserted as first argument.  Also known
as diamond wand."
  (cl-reduce (arrows--diamond-inserter #'arrows--insert-first)
             forms
             :initial-value initial-form))

(defmacro arrows-<>> (initial-form &rest forms)
  "Like -<>, but if a form has no symbol named <>, the insertion is done at the
end like in ->>.  Also known as diamond spear."
  (cl-reduce (arrows--diamond-inserter #'arrows--insert-last)
             forms
             :initial-value initial-form))

(provide 'arrows)
;;; arrows.el ends here
