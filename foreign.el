;;; package --- Summary
;;; Commentary:
;;;   some functions which help me edit text well
;;; functions/foreign.el -*- lexical-binding: t; -*-
;;;
;;; Code:
(require 'cc-defs)
(require 'dash)
(require 'foreign-mode)
(require 'org)
(require 's)

(defun foreign--current-line ()
  "Return current line."
  (let ((start)
        (end))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq end (point))
    (buffer-substring-no-properties start end)))

(defun foreign--current-line-is-heading? ()
  "Does the current line is heading?"
  (save-excursion
    (let ((line (foreign--current-line)))
      (or
       (string-match-p "^*+\s+\\w+.+$" line)
       (string-match-p "^\\(\s+\\)?[-+] .+$" line)))))

(defun foreign--normalize-line (str)
  "Remove extra symbols of the beginning of the line.

STR - string which will be normalized"
  (let ((remove-from-string
         (lambda (rgx target)
           (replace-regexp-in-string rgx "" target))))
    (thread-last
      str
      (funcall remove-from-string "^\s?+[-+*]\s+"))))

(defun foreign--copy-all-content ()
  "Select all content under the current heading at point.

Select only org-list-items"
  (save-excursion
    (let ((content nil)
          (line))
      (forward-line)
      (while (ignore-errors
               (setq line (foreign--current-line))
               (setq content (cons (foreign--normalize-line line) content))
               (org-next-item)))
      (string-join content "\n"))))

(defgroup foreign nil
  "Options for foreign."
  :group 'foreign)

(defvar foreign--answers nil)
(defconst foreign--check-box "[ ]")
(defconst foreign--check-box-checked "[X]")

(defun foreign--content-to-touples (content)
  "CONTENT of org entity."
  (thread-last
    (split-string content "\n")
    (-map (lambda (row) (-map 's-trim (split-string row "-"))))))

(defun foreign--shuffle (coll)
  "Shuffle COLL."
  (let ((acc)
        (rest (vconcat coll))
        (roll))
    (dotimes (_ (length rest))
      (setq roll (random (length rest)))
      (setq acc (cons (elt rest roll) acc))
      (setq rest (vconcat (seq-take rest roll) (seq-drop rest (1+ roll)))))
    acc))

(defun foreign--start-learning (header)
  "Create learning session by current HEADING."
  (let ((content (foreign--copy-all-content))
        (prepared-content)
        (time (format-time-string "%Y-%m-%d %H-%M" (current-time))))
    (setq foreign--answers (foreign--content-to-touples content))
    (switch-to-buffer (concat "foreign-learning *" header "* *" time "*"))
    (thread-last
      foreign--answers
      (-map 'car)
      (-map (lambda (row) (concat foreign--check-box " " row " - \n")))
      (foreign--shuffle)
      (string-join)
      (insert))
    (goto-char (point-min))
    (end-of-line)
    (foreign-mode)))

(defun foreign--find-answer-by-key (key)
  "Look for an answer by KEY and return it."
  (thread-last
    foreign--answers
    (-find (lambda (coll)
             (cl-destructuring-bind (key_ answer) coll
               (s-equals? key key_))))
    (-last 'identity)))

(defun foreign--answer-is-wrong (answer)
  "Check answer on correct by ANSWER."
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

(defun foreign--check-line ()
  "Check current line of correction."
  (let ((right-answer (foreign--answer-is-wrong (foreign--current-line))))
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
  (when (and (eq major-mode #'foreign-mode) (not (s-contains-p "Statistic" (buffer-string))))
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
