;;; metal-mercury-mode.el --- Concise mercury major mode

;; Copyright (C) 2016-2019  Matthew Carter, Ludvig Böklin

;; Author: Ludvig Böklin <ludvig.boklin@protonmail.com>
;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/lboklin/metal-mercury-mode
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
(require 'mercury-indentation)
(require 'subr-x)
(require 'dash)

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

(defvar metal-mercury-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?%  "<" st)
    (modify-syntax-entry ?\n ">" st)

    ;; matching parens
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    ;; " and ' for literal strings
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\' "\"'" st)
    (modify-syntax-entry ?\\ "/" st)

    ;; operator chars get punctuation syntax
    (mapc #'(lambda (ch) (modify-syntax-entry ch "." st))
          "[-+*/\\\\<>=:@^&.;,]+")

    ;; _ can be part of names, so give it symbol constituent syntax
    (modify-syntax-entry ?_ "_" st)
    st))

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

;;;###autoload
(define-derived-mode metal-mercury-mode prog-mode "Mercury"
  "Major mode for editing mercury files."
  ;; (interactive)
  ;; (kill-all-local-variables)
  (set-syntax-table metal-mercury-mode-syntax-table)
  (use-local-map metal-mercury-mode-map)

  (setq-local comment-start "%")
  (setq-local comment-end "")
  (setq-local paragraph-separate "\\(\r\t\n\\|-}\\)$")

  (setq major-mode 'metal-mercury-mode)
  (setq mode-name "Mercury")
  (turn-on-mercury-font-lock)
  (run-hooks 'metal-mercury-mode-hook)
  (mercury-indentation-mode))

(defcustom metal-mercury-mode-hook '(mercury-indentation-mode)
  "List of functions to run after metal-mercury-mode is enabled."
  :group 'mercury
  :type 'hook
  :options '(subword-mode
             flyspell-prog-mode
             mercury-indentation-mode
             highlight-uses-mode
             imenu-add-menubar-index))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m\\'" . metal-mercury-mode))


(provide 'metal-mercury-mode)

;;; metal-mercury-mode.el ends here
