;;; metal-mercury-mode.el --- Concise mercury major mode

;; Copyright (C) 2016  Matthew Carter

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/metal-mercury-mode
;; Version: 0.0.1
;; Keywords: ahungry emacs mercury prolog
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is not part of GNU Emacs

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Prolog mode is very large and old/complicated - this aims to be a
;; dead simple implementation for mercury-mode that fixes many
;; indentation/highlighting issues that are in prolog-mode when working
;; with mercury files.

;;; News:

;;;; Changes since 0.0.0:
;; - Created the project

;;; Code:

;; Mode bootstrapped from tutorial here: https://www.emacswiki.org/emacs/ModeTutorial#toc1

(require 'cl-lib)
(require 'mercury-font-lock)

(defvar metal-mercury-mode-hook nil)
(defvar metal-mercury-mode-default-tab-width 2)

(defvar metal-mercury-mode-compile-function
  (lambda (module-name)
    (cl-concatenate 'string "mmc --make " module-name))
  "Command that when given MODULE-NAME (hello.m module-name would be hello) will compile the mercury file.")

(defvar metal-mercury-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map (kbd "C-c C-c") 'metal-mercury-mode-compile)
    (define-key map (kbd "C-c C-r") 'metal-mercury-mode-runner)
    map)
  "Keymap for metal mercury major mode.")

(defun metal-mercury-mode-indent-line ()
  "Indent the current line as mercury code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0) ; Check for rule 1
    (let ((not-indented t) cur-indent)
      ;; Check for rule 2 - End of a block (unindent)
      (if (looking-at "^[ \t]*\\(;.*\\|then\\|else\\|)\\)")
          (progn
            (let ((undo-mult 1))

              ;; When we close on a parenthesis, unindent twice - why?
              (when (looking-at "[\t ]*)[\\.]*") (setq undo-mult 1))

              ;; When we close on then/else single lines, don't unindent at all
              (when (looking-at "[\t ]*\\(then\\|else\\)")
                (save-excursion
                  (forward-line -1)
                  (when (looking-at "[\t ]*\\(if\\|then\\)") (setq undo-mult 0))
                  ))

              ;; Step back a line and check indent level
              (save-excursion
                (forward-line -1)
                (setq cur-indent (- (current-indentation) (* undo-mult metal-mercury-mode-default-tab-width)))
                ;;(message (format "unindenting to %s" cur-indent))
                )
              (if (< cur-indent 0)
                  (setq cur-indent 0))
              ))
        (save-excursion
          (while not-indented ;; While loop backtracks up to last indent match
            (forward-line -1)
            (if (looking-at ".*[\\.]$") ; Check for rule 3
                (progn
                  (message "Reset indenting based on rule 3, EOL dot")
                  (setq cur-indent 0);;(current-indentation))
                  (setq not-indented nil))
              ;; Check for rule 4
              (if (or (looking-at "^[ \t]*\\(;.*\\|if\\|then\\|else\\)$") ;; End of line matches to indent
                      (looking-at "^[ \t]*),$") ;; Closing of switch statements
                      (looking-at "^.*\\(([^)]*\\|\\[[^\\]]*\\|:-\\)$")) ;; Predicate or open paren/bracket
                  (progn
                    (if (looking-at "^[\t ]*),$")
                        (setq cur-indent (current-indentation))
                      (setq cur-indent (+ (current-indentation) metal-mercury-mode-default-tab-width)))
                    (message (thing-at-point 'line t))
                    (setq not-indented nil))
                (if (bobp) ; Check for rule 5
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defvar metal-mercury-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for metal-mercury-mode.")

;; Run mercury files with the push of a button
(defun metal-mercury-mode-compile ()
  "Compile and run the active mercury file."
  (interactive)
  (save-buffer)
  (let ((module-name (replace-regexp-in-string ".*\\/\\(.*?\\)\\..*" "\\1" (buffer-file-name))))
    (compile (funcall metal-mercury-mode-compile-function module-name))
    (switch-to-buffer-other-window "*compilation*")))

(defun metal-mercury-mode-runner ()
  "Compile and run the active mercury file."
  (interactive)
  (save-buffer)
  (let ((module-name (replace-regexp-in-string ".*\\/\\(.*?\\)\\..*" "\\1" (buffer-file-name))))
    (compile (funcall metal-mercury-mode-compile-function module-name))
    (shell-command (cl-concatenate 'string "./" module-name) "MERCURY-RUNNER")
    (switch-to-buffer-other-window "MERCURY-RUNNER")))

(defun metal-mercury-mode ()
  "Major mode for editing mercury files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table metal-mercury-mode-syntax-table)
  (use-local-map metal-mercury-mode-map)
  (set (make-local-variable 'indent-line-function)
       'metal-mercury-mode-indent-line)

  (setq-local comment-start "%")
  (setq-local comment-end "")
  (setq-local paragraph-separate "\\(\r\t\n\\|-}\\)$")

  (setq major-mode 'metal-mercury-mode)
  (setq mode-name "Mercury")
  (turn-on-mercury-font-lock)
  (run-hooks 'metal-mercury-mode-hook))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.m\\'" . metal-mercury-mode))
  )

(provide 'metal-mercury-mode)

;;; metal-mercury-mode.el ends here
