;;; foreign-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 ivan
;;
;; Author: ivan <ivan@ivan-Prestige-14-A10SC>
;; Maintainer: ivan <ivan@ivan-Prestige-14-A10SC>
;; Created: September 29, 2024
;; Modified: September 29, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ivan/foreign-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar foreign-mode-hook nil)

(defun foreign-mode-next-item ()
  "Go to editing next item."
  (interactive)
  (re-search-forward "^.+ \\w+ - \\(\\w+\\)?$"))

(defun foreign-mode-prev-item ()
  "Go to editing prev item."
  (interactive)
  (re-search-backward "^.+ \\w+ - \\(\\w+\\)?$")
  (end-of-line))

(defvar foreign-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'foreign-mode-next-item)
    (define-key map (kbd "<backtab>") #'foreign-mode-prev-item)
    map)
  "Keymap of foreign major mode.")

(defvar foreign-mode-syntax-table
  (make-syntax-table))

(defconst foreign-mode-font-lock-keywords-1
  (list
   '("\\((.+)\\)"  . (1 font-lock-function-name-face))
   '("\\(\\[.]\\)"  . (1 font-lock-keyword-face)))
  "Minimal highlighting foreign entities.")

(defvar foreign-mode-font-lock-keywords foreign-mode-font-lock-keywords-1
  "Default highlighting foreign entities.")

(define-derived-mode foreign-mode prog-mode "FOREIGN"
  "Major mode for editing foreign buffer."
  (set-syntax-table foreign-mode-syntax-table)
  (use-local-map foreign-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(foreign-mode-font-lock-keywords))
  (setq major-mode 'foreign-mode)
  (setq mode-name "FOREIGN")
  (run-hooks 'foreign-mode-hook))

(provide 'foreign-mode)
;;; foreign-mode.el ends here
