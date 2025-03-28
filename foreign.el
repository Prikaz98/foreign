;;; package --- Summary
;;; Commentary:
;;;   Application to learn new vocabulary in foreign language.
;;; functions/foreign.el -*- lexical-binding: t; -*-
;;; Require: Emacs version >= 29.3
;;; Code:

(require 'cc-defs)
(require 'dash)
(require 'foreign-mode)
(require 'org)
(require 'rect)

(defgroup foreign nil
  "Options for foreign."
  :group 'foreign)

(defvar foreign--answers nil)
(defvar foreign--position nil)
(defconst foreign--check-box "[ ]")
(defconst foreign--check-box-checked "[X]")
(defconst foreign-entity-tag "foreign")

(defun foreign--string-contains? (str1 str2 &optional ignore-case)
  "Search STR2 in STR1."
  (with-temp-buffer
    (insert str1)
    (goto-char (point-min))
    (let ((case-fold-search ignore-case))
      (ignore-error 'search-failed
        (search-forward str2)
        t))))

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
       (string-match-p (concat "^*+.+" foreign-entity-tag) line))))

(defun foreign--normalize-line (str)
  "Remove extra symbols of the beginning of the line.

STR - string which will be normalized"
  (let ((remove-from-string
         (lambda (rgx target)
           (replace-regexp-in-string rgx "" target))))
    (->> str
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

(defun foreign--content-to-touples (content swap?)
  "CONTENT of org entity.

SWAP - boolean value signs of swapping target word with its translation."
  (->> (split-string content "\n")
       (-map (lambda (row) (-map 'string-trim (split-string row "-"))))
       (-map (lambda (pair)
               (if swap?
                   (list (cadr pair) (car pair))
                 pair)))))

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

(defun foreign--start-learning (header swap?)
  "Create learning session by current HEADER."
  (let ((current-point (point))
        (content (foreign--copy-all-content))
        (prepared-content)
        (time (format-time-string "%Y-%m-%d %H-%M" (current-time))))
    (setq foreign--position (list :place current-point :buffer-name (buffer-name)))
    (setq foreign--answers (foreign--content-to-touples content swap?))
    (switch-to-buffer (concat "foreign-learning *" header "* *" time "*"))
    (->> foreign--answers
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
  (->> foreign--answers
       (-find (lambda (coll)
                (cl-destructuring-bind (key_ answer) coll
                  (string= key key_))))
       (-last 'identity)))

(defun foreign--answer-is-wrong (answer)
  "Check answer on correct by ANSWER."
  (cl-destructuring-bind (key-checked answer) (split-string answer "-")
    (let ((key (string-trim (substring key-checked (length foreign--check-box))))
          (right-answer))
      (setq right-answer (foreign--find-answer-by-key key))
      (if right-answer
          (if (and answer
                   (not (string-empty-p (string-trim answer)))
                   (foreign--string-contains? (string-trim right-answer) (string-trim answer) t))
              nil
            right-answer)
        (message (concat "Couldn't find " key))))))

(defun foreign--check-line (max-line)
  "Check current line of correction.

MAX-LINE need to indent RIGHT-ANSWER"
  (let* ((curr-line (foreign--current-line))
         (curr-line-size (length curr-line))
         (right-answer (foreign--answer-is-wrong curr-line))
         (padding (spaces-string (+ 2 max-line (* -1 curr-line-size)))))
    (if (not right-answer)
        (progn
          (beginning-of-line)
          (kill-region (point) (+ (point) (length foreign--check-box)))
          (insert foreign--check-box-checked)
          t)
      (progn
        (end-of-line)
        (insert (concat padding "(" (string-trim right-answer) ")"))))))

(defun foreign-put-tag ()
  "Put sign that means it is a foreign entity."
  (interactive)
  (org-set-tags foreign-entity-tag))

(defun foreign--put-last-statistics (all right wrong)
  "Store ALL, RIGHT and WRONG result."
  (when (and foreign--position (yes-or-no-p "Would you like to leave session?"))
    (save-excursion
      (switch-to-buffer (plist-get foreign--position :buffer-name))
      (goto-char (plist-get foreign--position :place))
      (let* ((start (progn
                     (beginning-of-line)
                     (point)))
             (end (progn
                     (re-search-forward foreign-entity-tag)
                     (- (point) (length foreign-entity-tag) 2)))
             (line (buffer-substring-no-properties start end))
             (new-line))
        (when (string-match-p "^*+\s+\\w+.+$" line) ;;is org heading like * Org
          (setq new-line (replace-regexp-in-string "\s+\\[[0-9]+/.[0-9]+/.[0-9]+]" "" line))
          (kill-region start end)
          (beginning-of-line)
          (insert (concat
           new-line
                   " ["
                   (number-to-string all)
                   "/+"
                   (number-to-string right)
                   "/-"
                   (number-to-string wrong)
                   "]")))))))

(defun foreign--max-line-size ()
  "Return max line size in the buffer."
  (let ((max-size 0)
        (next-size))
    (goto-char (point-min))
    (while (not (eobp))
     (setq next-size (length (foreign--current-line)))
     (when (> next-size max-size) (setq max-size next-size))
     (forward-line))
    max-size))

(defun foreign-check-answers ()
  "Finish learning session.

Prints result and toggle checkbox of answers."
  (interactive)
  (when (and (eq major-mode #'foreign-mode) (not (foreign--string-contains? (buffer-string) "Statistic")))
    (let ((right-count 0)
          (all-count 0)
          (max-line (foreign--max-line-size)))
      (save-excursion
        (goto-char (point-min))
        (while (foreign--string-contains? (foreign--current-line) foreign--check-box)
          (setq all-count (+ all-count 1))
          (setq right-count (+ right-count (if (foreign--check-line max-line) 1 0)))
          (forward-line))
        (end-of-line)
        (insert (if (= all-count right-count)
                    "\nYou are absolutely right!!!"
                  (concat "\nStatistic right:"
                          (number-to-string right-count)
                          " wrong:"
                          (number-to-string (- all-count right-count))))))
      (foreign--put-last-statistics all-count right-count (- all-count right-count)))))

(defun foreign-start-learning (&optional swap)
  "Start learning session.

Copy all heading content. Expected content rows of `'word - translation`'.
When you wrote all translations you can call `'foreign-check-answers`'
to get a result of training.
SWAP - boolean value signs of swapping target word with its translation."
  (interactive)
  (setq swap (yes-or-no-p "Swap?"))
  (if (foreign--current-line-is-heading?)
      (foreign--start-learning (org-get-heading) swap)
    (message "You should stay on header which you would like to learn")))

(provide 'foreign)
;;; foreign.el
