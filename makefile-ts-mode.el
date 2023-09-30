;;; makefile-ts-mode.el --- Major mode for makefiles -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/makefile-ts-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Created: 29 September 2023
;; Keywords: make languages tree-sitter

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;; Description:
;;
;; This package defines a tree-sitter enabled major mode for Make that provides
;; support for indentation, font-locking, imenu, and structural navigation.
;;
;; The tree-sitter grammar compatible with this package can be found at
;; https://github.com/alemuller/tree-sitter-make.
;;
;;; Installation:
;;
;; Add the following entry to `treesit-language-source-alist':
;;    
;;     (add-to-list
;;      'treesit-language-source-alist
;;      '(make "https://github.com/alemuller/tree-sitter-make")
;;
;; and call `treesit-install-language-grammar' to do the installation.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'treesit)

;;; Syntax

(defvar makefile-ts-mode--syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()    " st)
    (modify-syntax-entry ?\) ")(    " st)
    (modify-syntax-entry ?\[ "(]    " st)
    (modify-syntax-entry ?\] ")[    " st)
    (modify-syntax-entry ?\{ "(}    " st)
    (modify-syntax-entry ?\} "){    " st)
    (modify-syntax-entry ?\' "\"    " st)
    (modify-syntax-entry ?\` "\"    " st)
    (modify-syntax-entry ?#  "<     " st)
    (modify-syntax-entry ?\n ">     " st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?$ "." st)
    st)
  "Syntax table used in `makefile-ts-mode'.")

;;; Indentation

(defvar makefile-ts-mode--indent-rules
  '((make
     ((parent-is "makefile") parent 0)
     ;; ((node-is ")") parent-bol 0)
     ;; ((node-is "}") parent-bol 0)
     ;; ((node-is "]") parent-bol 0)
     ;; ((parent-is "block") parent-bol makefile-ts-mode-indent-level)
     ;; ((node-is "else") parent-bol 0)
     (no-node no-indent)
     (catch-all no-indent)))
  "Tree-sitter indentation rules for make.")

;;; Font-Lock

(defvar makefile-ts-mode--feature-list
  '(( comment definition)
    ( keyword string builtin directive)
    ( assignment function variable escape-sequence)
    ( bracket delimiter operator error))
  "`treesit-font-lock-feature-list' for `makefile-ts-mode'.")

(defvar makefile-ts-mode--includes '("include" "sinclude" "-include"))
(defvar makefile-ts-mode--conditions
  '("if" "ifeq" "ifneq" "ifdef" "ifndef" "else" "endif" "or" "and"))

(defvar makefile-ts-mode--keywords
  '("define"
    "endef"
    "vpath"
    "undefine"
    "export"
    "unexport"
    "override"
    "private"
    ;; "load"
    )
  "Make keywords for tree-sitter font-locking.")

(defvar makefile-ts-mode--builtin-functions
  ;; rx string-start
  ;; or
  ;; tree-sitter grammar
  '("abspath" "addprefix" "addsuffix" "and" "basename" "call" "dir" "error"
   "eval" "file" "filter" "filter-out" "findstring" "firstword" "flavor" "foreach"
   "if" "info" "join" "lastword" "notdir" "or" "origin" "patsubst" "realpath"
   "sort" "strip" "subst" "suffix" "value" "warning" "wildcard" "word" "wordlist"
   "words")
  ;; make-mode
  ;; "subst" "patsubst" "strip" "findstring" "filter" "filter-out" "sort" "dir"
  ;; "notdir" "suffix" "basename" "addprefix" "addsuffix" "join" "word" "words"
  ;; "firstword" "wildcard" "foreach" "origin" "shell"
  ;; string-end
  "Make builtin functions for tree-sitter font-locking.")

(defvar makefile-ts-mode--automatic-vars
  '("@" "%" "<" "?" "^" "+" "/" "*" "D" "F")
  "Make automatic variables.")

(defvar makefile-ts-mode--builtin-variables '()
  "Make builtin variables for tree-sitter font-locking.")

(defvar makefile-ts-mode--operators
  '("=" ":=" "::=" "?=" "+="            ; assignment ops
    "!=" "@" "-" "+")
  "Make operators for font-locking.")

(defvar makefile-ts-mode--delimiter
  '( ":" "&:" "::" "|" ";" "\"" "'" ",")
  "Make delimiter for font-locking.")

;; (defvar makefile-ts-mode--assignment-query
;;   (when (treesit-available-p)
;;     (treesit-query-compile 'make '((identifier) @id)))
;;   "Query to capture identifiers in assignment_exp.")

;; (defun makefile-ts-mode--fontify-assignment-lhs (node override start end &rest _)
;;   "Fontify the lhs NODE of an assignment_exp.
;; For OVERRIDE, START, END, see `treesit-font-lock-rules'."
;;   (dolist (node (treesit-query-capture
;;                  node makefile-ts-mode--assignment-query nil nil t))
;;     (treesit-fontify-with-override
;;      (treesit-node-start node) (treesit-node-end node)
;;      (pcase (treesit-node-type node)
;;        ("identifier" 'font-lock-variable-use-face))
;;      override start end)))

(defvar makefile-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'make
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'make
   :feature 'string
   '([(text) (string) (raw_text)] @font-lock-string-face
     (variable_assignment (word) @font-lock-string-face))
   
   :language 'make
   :feature 'keyword
   `([,@makefile-ts-mode--keywords] @font-lock-keyword-face
     ;; ["@include"] @font-lock-preprocessor-face
     )

   ;; `makefile-special-targets-list'
   ;; `makefile-runtime-macros-list'
   ;; `makefile-gmake-statements'
   ;; builtin functions, builtin variables, automatic variables
   :language 'make
   :feature 'builtin
   `((function_call
      function: _ @var (:match ,makefile-ts-mode--builtin-functions @var)
      @font-lock-function-call-face)
     (automatic_variable
      ["$" "$$"]
      _ @var (:match ,@makefile-ts-mode--automatic-vars @var)
      @font-lock-constant-face)
     ;; ["print" "printf"] @font-lock-builtin-face
     ;; ((identifier) @var (:match ,makefile-ts-mode--builtin-variables @var))
     ;; @font-lock-variable-name-face
     )

   ;; TODO:
   ;; - targets
   ;; - conditionals
   ;; - includes
   ;; - negation char
   ;; - special targets
   
   ;; rules, macros
   ;; :language 'make
   ;; :feature 'definition
   ;; '()

   ;; $(function ...) $(shell ...) differently? `makefile-shell'
   ;; `makefile-gnumake-functions-alist'
   :language 'make
   :feature 'function
   '((function_call
      function: _ @font-lock-function-call-face
      (arguments
       argument: (_) :* @font-lock-variable-use-face)))
   
   ;; TODO:
   ;; - RECIPEPREFIX_assignment
   ;; - VPATH_assignment
   :language 'make
   :feature 'assignment
   '((shell_assignment
      name: (word) @font-lock-variable-use-face)

     ;; (variable_assignment
     ;;  ;; TODO: lists
     ;;  target_or_pattern: (list) :?
     ;;  name: (word) @font-lock-variable-use-face
     ;;  value: (text) :?)
     )
     
   ;; heredocs
   ;; :language 'make
   ;; :feature 'literal
   ;; '((number) @font-lock-number-face
   ;;   [(regex_constant) (regex_flags)] @font-lock-constant-face)

   :language 'make
   :feature 'directive
   '(
     ;; (define_directive
     ;;  name: (_) @font-lock-function-name-face
     ;;  value: (_) :?)
     
     (undefine_directive
      variable: (_) @font-lock-variable-use-face)

     (ifdef_directive
      variable: (_) @font-lock-variable-use-face)

     (ifndef_directive
      variable: (_) @font-lock-variable-use-face)
     ;; TODO:
     ;; (export_directive
     ;;  variables: (list))
     ;; (override_directive)
     ;; (unexport_directive
     ;;  variables: (list))
     )

   ;; variable references in targets/strings/comments
   :language 'make
   :feature 'variable
   '((substitution_reference
      text: (_) @font-lock-variable-name-face
      pattern: (_)
      replacement: (_))

     (variable_reference (_) @font-lock-variable-use-face))
   
   :language 'make
   :feature 'bracket
   '(["(" ")" "{" "}"] @font-lock-bracket-face)

   :language 'make
   :feature 'delimiter
   '(["," ";" ":" "::" "&:" "'" "|"] @font-lock-delimiter-face)

   :language 'make
   :feature 'operator
   `([,@makefile-ts-mode--operators] @font-lock-operator-face
     ;; 'n' in ifndef ifneq etc
     ;; "!" @font-lock-negation-char-face
     )

   :language 'make
   :feature 'escape-sequence
   :override t
   '([(escape) "\\"] @font-lock-escape-face)

   :language 'make
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)
   ;; TODO: highlight whitespace lines?? `makefile-space'
   ;; directives
   )
  "Tree-sitter font-lock settings for make.")

;;; TODO: Navigation
(defun makefile-ts-mode--defun-name (node)
  (treesit-node-text
   (treesit-node-child-by-field-name node "identifier")))

;;; `makefile-gmake-statements'
(defvar makefile-ts-mode--sentence-nodes nil)
(defvar makefile-ts-mode--sexp-nodes nil)
(defvar makefile-ts-mode--text-nodes nil)

(define-derived-mode makefile-ts-mode prog-mode "Make"
  "Major mode for editing make source code."
  :group 'make
  :syntax-table makefile-ts-mode--syntax-table
  (when (treesit-ready-p 'make)
    (treesit-parser-create 'make)

    (setq-local comment-start "#")
    (setq-local comment-end "")
    (setq-local comment-start-skip "#+[ \t]*")

    ;; Indentation
    (setq-local treesit-simple-indent-rules makefile-ts-mode--indent-rules)

    ;; Font-Locking
    (setq-local treesit-font-lock-settings makefile-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list makefile-ts-mode--feature-list)

    ;; TODO: Navigation
    (setq-local treesit-defun-prefer-top-level t)
    (setq-local treesit-defun-name-function #'makefile-ts-mode--defun-name)
    (setq-local treesit-defun-type-regexp nil) ;; (rx bos "module")
    
    ;; navigation objects
    (setq-local treesit-thing-settings
                `((make
                   (sexp ,makefile-ts-mode--sexp-nodes)
                   (sentence ,makefile-ts-mode--sentence-nodes)
                   (text ,makefile-ts-mode--text-nodes))))

    ;; TODO: Imenu
    ;; `makefile-imenu-generic-expression'
    (setq-local treesit-simple-imenu-settings nil)

    ;; Dabbrev.
    (setq-local dabbrev-abbrev-skip-leading-regexp "\\$")

    ;; TODO: Filling.
    (setq-local fill-paragraph-function 'makefile-fill-paragraph)

    ;; TABs
    (setq indent-tabs-mode t)
    
    (treesit-major-mode-setup)))

(when (treesit-ready-p 'make)
  (add-to-list 'auto-mode-alist '("\\.[Mm]akefile\\'" . makefile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.m\\(ak\\)?k\\'" . makefile-ts-mode)))

(provide 'makefile-ts-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; makefile-ts-mode.el ends here
