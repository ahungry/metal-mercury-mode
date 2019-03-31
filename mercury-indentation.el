;;; mercury-indentation.el --- indentation module for Mercury Mode -*- lexical-binding: t -*-

;; Copyright (C) 2013-2019  Kristof Bastiaensen, Gergely Risko, Matthew Carter, Ludvig Böklin

;; Author: Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>
;; Author: Gergely Risko <errge@nilcons.com>
;; Author: Matthew Carter <m@ahungry.com>
;; Author: Ludvig Böklin <ludvig.boklin@protonmail.com>
;; Keywords: indentation mercury
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

;; Installation:
;;
;; To turn indentation on for all Mercury buffers under Mercury mode add
;; this to your configuration file:
;;
;;     (add-hook metal-mercury-mode-hook 'mercury-indentation-mode)
;;
;; Otherwise, call `mercury-indentation-mode'.

(require 'cl-lib)
(require 'dash)

;;; Code:
;;;###autoload
(defgroup mercury-indentation nil
  "Mercury indentation."
  :link '(custom-manual "(metal-mercury-mode)Indentation")
  :group 'mercury
  :prefix "mercury-indentation-")

(defvar mercury-indentation-default-tab-width 4)

(defcustom mercury-indentation-electric-flag nil
  "Non-nil means insertion of some characters may auto reindent the line.
If the variable `electric-indent-mode' is non-nil then this variable is
overridden."
  :type 'symbol
  :group 'mercury-indentation)
(make-variable-buffer-local 'mercury-indentation-electric-flag)

(defvar mercury-indentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'newline-and-indent)
    map)
  "Keymap for `mercury-indentation-mode'.")


;;;###autoload
(define-minor-mode mercury-indentation-mode
  "Mercury indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs."
  :keymap mercury-indentation-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'indent-region-function)

  (when mercury-indentation-mode
    (setq-local indent-line-function #'mercury-indentation-indent-line)))

;;;###autoload
(defun turn-on-mercury-indentation ()
  "Turn on the mercury-indentation minor mode."
  (interactive)
  (mercury-indentation-mode t))

(defun mercury-indentation-indent-line ()
  "Indent the current line as mercury code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0) ; Check for rule 1
    (let ((not-indented t) cur-indent)
      ;; Check for rule 2 - End of a block (unindent)
      (if (looking-at "^[ \t]*\\(;.*\\|),?\\|then.*\\|else.*\\)")
          (progn
            (let ((undo-mult 1))
              (save-excursion
                (when (looking-at "^[\t ]*),?") ;; on closing paren
                  (progn
                    (when (looking-at "[\t ]*\\(if\\|then\\|else\\).*$")
                      (setq undo-mult 2)))))
              ;; Step back a line and check indent level
              (save-excursion
                (forward-line -1)
                ;; if not inline (because when inline we're not yet indented) or open paren, unindent,
                ;; otherwise do not change
                (if (or (not (looking-at (concat
                                          "^[\t ]*"
                                          "\\(("
                                          "\\|;"
                                          "\\|\\<if\\>"
                                          "\\|\\<then\\>"
                                          "\\|\\<else\\>"
                                          "\\)"
                                          ".+$")))
                        (and (looking-at "^[\t ]*(.+$")
                             (not (parens-match '> (thing-at-point 'line t)))))
                    (progn
                      (setq cur-indent
                            (- (current-indentation)
                               (* undo-mult mercury-indentation-default-tab-width))))
                  (progn
                    (setq undo-mult 1)
                    (setq cur-indent (current-indentation)))))
              (if (< cur-indent 0)
                  (setq cur-indent 0))
              ))
        (save-excursion
          (while not-indented ;; While loop backtracks up to last indent match
            (forward-line -1)
            (if (looking-at ".*\\. *$") ; Check for rule 3
                (progn
                  (setq cur-indent 0)
                  (setq not-indented nil))
              ;; Check for rule 4
              ;; if previous line was an ;, if/then/else, opening/closing paren, = or :-,
              ;; then if close paren, unindent, else indent.
              (let ((line (replace-regexp-in-string "\".*\"" "" (thing-at-point 'line t))))
                (if (or (string-match (concat "^[ \t]*"
                                            "\\(;"
                                            "\\|.*\\<if\\>"
                                            "\\|.*\\<then\\>"
                                            "\\|.*\\<else\\>"
                                            "\\)"
                                            ".*$")
                                    line)
                      (parens-match '> line)
                      (and (parens-match '< line)
                           (not (string-match "^[\t ]*),?" line)))
                      (string-match (concat "^.*"
                                            "\\(:-"
                                            "\\|="
                                            "\\)"
                                            " *$")
                                    line))
                    (progn
                      ;; if last line was closing (but not ending a conjunction), unindent,
                      ;; else indent.
                      (if (parens-match '< line)
                          (progn
                            (if (string-match "^[\t ]*),?" line)
                                (setq cur-indent (current-indentation))
                              (setq cur-indent
                                  (- (current-indentation)
                                     mercury-indentation-default-tab-width)))
                            (setq not-indented nil))
                        (progn
                          (setq cur-indent
                                (+ (current-indentation)
                                   mercury-indentation-default-tab-width))
                          (setq not-indented nil))
                        ))
                  ;; if nothing in particular was found on previous line
                  ;; but not commented or empty, do not change indent.
                  (when (not (string-match "^[\t ]*\\(%\\|\\)$" line))
                    (progn
                      (setq cur-indent
                            (current-indentation))
                      (setq not-indented nil))))
                (if (bobp) ; Check for rule 5
                    (progn
                      (setq not-indented nil))))))))
      (if (> cur-indent 0)
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defun parens-match (comp str)
  "Return result comparing with COMP number of opening parens with closing in STR."
  (let
      ((matches-in-string #'(lambda (regex str)
                              (-filter #'(lambda (x) (not (eq "" x)))
                                       (split-string str regex)))))
    (funcall comp
             (length (funcall matches-in-string "(" str))
             (length (funcall matches-in-string ")" str)))))

(provide 'mercury-indentation)

;;; mercury-indentation.el ends here
