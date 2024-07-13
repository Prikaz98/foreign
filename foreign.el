;;; package --- Summary
;;; Commentary:
;;;   some functions which help me edit text well
;;; functions/foreign.el -*- lexical-binding: t; -*-
;;;
;;; Code:
(require 'org)

(defun foreign--current-line-is-heading? ()
  (save-excursion
    (let ((start)
        (end)
        (line))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq end (point))
    (setq line (buffer-substring-no-properties start end))
    (string-match-p "*" line))))

(defun foreign--copy-all-content ()
  (save-excursion
    (let ((content nil)
          (line))
      (forward-line)
      (while (ignore-errors
               (setq line (foreign--current-line))
               (setq content (cons (s-trim (substring line 2 (length line))) content))
               (org-next-item)))
      (string-join content "\n"))))

(defgroup foreign nil
  "Options for foreign."
  :group 'foreign)

(setq foreign--answers nil)
(setq foreign--check-box "[ ]")
(setq foreign--check-box-checked "[X]")

(defcustom foreign-text-increasing 2
  "After open new buffer increase text on this value"
  :group 'foreign
  :type 'number)

(defun foreign--content-to-touples (content)
  (thread-last
    (split-string content "\n")
    (-map (lambda (row) (split-string row "-")))))


(defun foreign--start-learning (header)
  (let ((content (foreign--copy-all-content))
        (prepared-content)
        (time (format-time-string "%Y-%m-%d %H-%M" (current-time))))
    (setq foreign--answers (foreign--content-to-touples content))
    (switch-to-buffer (concat "foreign-learning *" header "* *" time "*"))
    (display-line-numbers-mode)
    (text-scale-increase foreign-text-increasing)
    (thread-last
      foreign--answers
      (-map 'car)
      (-map (lambda (row) (concat foreign--check-box " " row "- \n")))
      (-sort 'string<)
      (string-join)
      (insert))
    (goto-char (point-min))))

(defun foreign--find-answer-by-key (key)
  (thread-last
    foreign--answers
    (-find (lambda (coll)
             (cl-destructuring-bind (key_ answer) coll
               (s-contains? key key_ t))))
    (-last 'identity)))

(defun foreign--answer-is-not-right? (answer)
  (cl-destructuring-bind (key-checked answer) (split-string answer "-")
    (let ((key (s-trim (substring key-checked (length foreign--check-box))))
          (right-answer))
      (setq right-answer (foreign--find-answer-by-key key))
      (if right-answer
          (if (and answer
                   (not (string-empty-p (s-trim answer)))
                   (s-contains? (s-trim answer) (s-trim right-answer) t))
              nil
            right-answer)
        (message (concat "Couldn't find " key))))))

(defun foreign--current-line ()
  (let ((start)
        (end))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq end (point))
    (buffer-substring-no-properties start end)))

(defun foreign--check-line ()
  (let ((right-answer (foreign--answer-is-not-right? (foreign--current-line))))
    (if (not right-answer)
        (progn
          (beginning-of-line)
          (kill-region (point) (+ (point) (length foreign--check-box)))
          (insert foreign--check-box-checked)
          t)
      (progn
        (end-of-line)
        (insert (concat " (" (s-trim right-answer) ")"))))))

(defun foreign-check-answers ()
  "Finish learnin session.

Prints result and toggle checkboxs of answers."
  (interactive)
  (when (not (s-contains-p "Statistic" (buffer-string)))
    (save-excursion
      (goto-char (point-min))
      (let ((right-count 0)
            (all-count 0))
        (while (s-contains? foreign--check-box (foreign--current-line))
          (setq all-count (+ all-count 1))
          (setq right-count (+ right-count (if (foreign--check-line) 1 0)))
          (forward-line))
        (end-of-line)
        (insert (if (= all-count right-count)
                    "\nYou are absolutely right!!!"
                  (concat "\nStatistic right:"
                          (number-to-string right-count)
                          " wrong:"
                          (number-to-string (- all-count right-count)))))))))

(defun foreign-start-learning ()
  "Start learning session.

Copys all heading content. Expected content rows of `'word - translation`'.
When you wrote all translations you can call `'foreign-check-answers`'
to get a result of training"
  (interactive)
  (if (foreign--current-line-is-heading?)
      (foreign--start-learning (org-get-heading))
    (message "You should stay on header which you would like to learn")))

(provide 'foreign)
;;; foreign.el
