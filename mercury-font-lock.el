;;; mercury-font-lock.el --- Font locking module for Mercury mode.

;; Copyright (C) 2019 Ludvig Böklin

;; Author: Ludvig Böklin <ludvig.boklin@protonmail.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/metal-mercury-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:
(require 'font-lock)

(defgroup mercury-font-lock nil
  "Font locking for Mercury code."
  :group 'faces)

(defface mercury-font-lock-operators
  '((t :inherit font-lock-builtin-face))
  "The default face used to highlight operators inside expressions."
  :group 'mercury-font-lock)

(defcustom mercury-font-lock-operators-face 'mercury-font-lock-operators
  "The face used to highlight operators inside expressions.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'mercury-font-lock)

(defface mercury-font-lock-multiline-list-delimiters
  '((t :inherit font-lock-keyword-face))
  "The default face used to highlight brackets and commas in multiline lists."
  :group 'mercury-font-lock)

(defcustom mercury-font-lock-multiline-list-delimiters-face 'mercury-font-lock-multiline-list-delimiters
  "The face used to highlight brackets and commas in multilist lists.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'mercury-font-lock)

(defconst mercury--keywords
  '(;; keywords taken from the language reference manual.
    ;; allowed declarations
    "type" "solver" "pred" "func" "inst" "mode" "typeclass"
    "instance" "pragma" "promise" "initialise" "finalise"
    "mutable" "module" "interface" "implementation" "import_module"
    "use_module" "include_module" "end_module"
    ;; determinism
    "erroneous" "failure" "det" "semidet" "nondet"
    "multi" "cc_nondet" "cc_multi"
    ;; reserved inst names
    "=<" "any" "bound" "bound_unique" "clobbered" "clobbered_any"
    "free" "ground" "is" "mostly_clobbered" "mostly_unique"
    "mostly_unique_any" "not_reached" "unique" "unique_any"
    ;; reserved mode names (excluding ones already included)
    "=" ">>" "any_func" "any_pred"
    ;; reserved operators (excluding ones already included)
    "." "!" "!." "!:" "@" "^" "^" "event" ":" "‘op ‘" "**" "-" "\\"
    "*" "/" "//" "<<" "div" "mod" "rem" "for" "+" "+" "++"
    "-" "--" "/\\" "\\/" ".." ":=" "=^" "<" "=.." "=:="
    "==" "=\\=" ">" ">=" "@<" "@=<" "@>" "@>=" "\\=" "\\=="
    "~=" "and" "or" "impure" "semipure" "\\\\+"
    "not" "when" "~" "<=" "<=>" "=>"
    "all" "arbitrary" "atomic" "disable_warning" "disable_warnings"
    "promise_equivalent_solutions" "promise_equivalent_solution_sets"
    "promise_exclusive" "promise_exclusive_exhaustive"
    "promise_exhaustive" "promise_impure" "promise_pure"
    "promise_semipure" "require_complete_switch"
    "require_switch_arms_det" "require_switch_arms_semidet"
    "require_switch_arms_multi" "require_switch_arms_nondet"
    "require_switch_arms_cc_multi" "require_switch_arms_cc_nondet"
    "require_switch_arms_erroneous" "require_switch_arms_failure"
    "require_det" "require_semidet"
    "require_multi" "require_nondet" "require_cc_multi"
    "require_cc_nondet" "require_erroneous" "require_failure"
    "trace" "try" "some" "," "&" "->" ";" "or_else" "then" "if" "else"
    "::" "==>" "where" "--->" "catch" "type" "solver" "catch_any"
    "initialise" "initialize" "finalise" "finalize" "inst" "instance"
    "mode" "module" "pragma" "promise" "rule" "typeclass" "use_module"
    "-->" ":-" "?-"
    )
  "Reserved keywords.")

(defconst mercury--regexp-keywords
  (regexp-opt mercury--keywords 'symbols)
  "A regular expression representing the reserved keywords.")

(defconst mercury--font-lock-keywords
  (cons mercury--regexp-keywords font-lock-keyword-face)
  "Highlighting for keywords.")

(defconst mercury--regexp-function
  "\\(^\\|func \\|pred \\|mode \\)\\(\\<[a-z][0-9A-Za-z_]*\\)"
  "A regular expression representing function names.")

(defconst mercury--font-lock-functions
  (cons mercury--regexp-function '(2 font-lock-function-name-face))
  "Highlighting for function names.")

(defconst mercury--regexp-variable
  "\\<[_A-Z][0-9A-Za-z_]*"
  "A regular expression representing variable names.")

(defconst mercury--font-lock-variables
  (cons mercury--regexp-variable font-lock-variable-name-face)
  "Highlighting for function names.")

(defconst mercury--regexp-operators
  "[-+*/\\\\<>=:@^&.;,]+"
  "A regular expression representing operators inside expressions.")

(defconst mercury--font-lock-operators
  (cons mercury--regexp-operators 'mercury-font-lock-operators-face)
  "Highlighting for operators inside expressions.")

(defconst mercury--regexp-multiline-list-comma-closing-brackets
  (concat "^[[:space:]]*" (regexp-opt '("," ";" "]" "}" ")") t))
  "A regular expression representing separators and closing brackets in
multiline lists, disjunctions, tuples and records.")

(defconst mercury--font-lock-multiline-list-comma-closing-brackets
  (cons mercury--regexp-multiline-list-comma-closing-brackets
        '(1 mercury-font-lock-multiline-list-delimiters-face))
  "Highlighting for separators and closing brackets in
multiline lists, disjunctions, tuples and records.")

(defun mercury--match-multiline-list-opening-bracket (limit)
  "Highlighting search function for opening brackets in multiline lists, disjunctions and records.
Also highlights opening brackets without a matching bracket."
  (when (mercury--search-forward-opening-bracket limit)
    (let ((opening (point))
          (eol (line-end-position))
          (closing (mercury--search-forward-closing-bracket)))
      (if (or (= closing opening) (> closing eol))
          (progn
            (set-match-data (match-data))
            (goto-char (+ 1 opening))
            t)
        (mercury--match-multiline-list-opening-bracket limit)))))

(defun mercury--search-forward-opening-bracket (limit)
  "Go to the next opening bracket up to LIMIT."
  (if (search-forward-regexp (regexp-opt '("[" "{" "(")) limit t)
      (progn
        (backward-char)
        t)))

(defun mercury--search-forward-closing-bracket ()
  "Go to the next matching bracket, assuming that the cursor is on an opening bracket."
  (ignore-errors
    (save-match-data
      (forward-sexp)))
  (point))

(defconst mercury--font-lock-multiline-list-opening-brackets
  '(mercury--match-multiline-list-opening-bracket (0 mercury-font-lock-multiline-list-delimiters-face))
  "Highlighting for opening brackets in multiline lists and records.")

(defconst mercury--font-lock-highlighting
  (list (list (cons "%.*" 'font-lock-comment-face)
              mercury--font-lock-keywords
              (cons "\\(\\!\\)[[:upper:]$]+[[:lower:]_$]*" ;; e.g. !IO
                    '(1 font-lock-keyword-face))
              (cons (concat
                     "\\<"
                     "\\(0[box]?[0-9][0-9_]*" ;; int literals
                     "\\|[0-9]+\\(\\.[0-9]+\\(e\\([+-][0-9]+\\)?\\)?\\)?" ;; float literals
                     "\\)"
                     "\\>")
                    '(1 font-lock-constant-face))
              mercury--font-lock-functions
              mercury--font-lock-variables

              ;; types, modes and modules
              (cons (concat
                     "\\<\\(?:type\\|::\\|:\\|[({,=]\\) *"
                     "\\([[:lower:]][[:alpha:]_0-9]+\\)\\>")
                    '(1 font-lock-type-face))
              (cons (concat
                     "\\([[:lower:]][[:alpha:]_0-9]+\\)"
                     "\\(?:::\\| *[,})\\.]\\| is\\)")
                    '(1 font-lock-type-face))

              mercury--font-lock-multiline-list-comma-closing-brackets
              mercury--font-lock-multiline-list-opening-brackets
              mercury--font-lock-operators
              )))
        ;; nil nil))

(defun turn-on-mercury-font-lock ()
  "Turn on Mercury font lock."
  (setq font-lock-multiline t)
  (set (make-local-variable 'font-lock-defaults) mercury--font-lock-highlighting))

(provide 'mercury-font-lock)
;;; mercury-font-lock.el ends here
