;;; word-list-minor-mode.el ---

(defvar word-list-minor-mode-map (make-keymap))
(define-minor-mode word-list-minor-mode
  "A minor mode for working with word list files and showing the template at the same time."
  nil " L" word-list-minor-mode-map

  (cond
   (word-list-minor-mode	(add-hook   	'post-command-hook 'word-list/open-word nil t))
   (t                   	(remove-hook	'post-command-hook 'word-list/open-word t))))

(defvar word-list/prepared 	nil)
(defvar word-list/last-line	0)
(defvar word-list/enable   	nil)


(defun word-list/mark-item ()
  (interactive)
  (let ((current-word (word-list/find-current-word)))
    (save-excursion
      (end-of-line)
      (unless (= (preceding-char) ??)
        (insert "?")))))

(defun word-list/unmark-item ()
  (interactive)
  (let ((current-word (word-list/find-current-word)))
    (save-excursion
      (end-of-line)
      (when (= (preceding-char) ??)
        (delete-char -1)))))

(defun word-list/mark-item-all ()
  (interactive)
  (let ((current-word (word-list/find-current-word)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (string= current-word
                       (word-list/find-current-word))
          (end-of-line)
          (unless (= (preceding-char) ??)
            (insert "?")))
        (forward-line 1)))
    (message "%s marked." current-word)))

(defun word-list/unmark-item-all ()
  (interactive)
  (let ((current-word (word-list/find-current-word)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (string= current-word
                       (word-list/find-current-word))
          (end-of-line)
          (when (= (preceding-char) ??)
            (delete-char -1)))
        (forward-line 1)))
    (message "%s unmarked." current-word)))

(defun word-list/find-current-word ()
  (catch 'done
    ;; look for beginning word
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "^[ ]*\\([^ ]+\\)[ ]|" (point-at-eol) t)
        (throw 'done (word-list/simplify-word (match-string-no-properties 1)))))

    ;; try a word in brackets
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "^[ \t]*\\+.*<\\([^ ]+\\)[ ]*/[ ]*[^>]*>" (point-at-eol) t)
        (throw 'done (word-list/simplify-word (match-string-no-properties 1)))))

    ;; or first word of a plus line otherwise
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "^[ \t]*\\+\\sw* \\([^ ]+\\)" (point-at-eol) t)
        (throw 'done (word-list/simplify-word (match-string-no-properties 1)))))

    nil))

(defun word-list/simplify-word (word)
  (replace-regexp-in-string "[^a-zA-ZäöüÄÖÜß-]" "" word))

(defun word-list/auto-turn-on ()
  (interactive)

  (if word-list/enable
      (progn
        (setq word-list/enable nil)
        (remove-hook 'find-file-hooks 'word-list/maybe-turn-on)
        (message "not gonna auto-turn-on word-list-mode"))
    (progn
      (setq word-list/enable t)
      (add-hook 'find-file-hooks 'word-list/maybe-turn-on)
      (message "gonna auto-turn-on word-list-mode"))))

(defun word-list/maybe-turn-on ()
  (when (s-starts-with?
         (expand-file-name "~/zg/palettes/") (buffer-file-name))
    (word-list-minor-mode 1)))

(defvar word-list/plus-directory "~/gpreds/" "where the templates-* files are")
(defvar word-list/plus-files
  '("templates-nouns"
    "templates-verbs"
    "templates-adjectives"
    "templates-adverbs")
  "files to check for plus lines")

(defun word-list/open-word ()
  (interactive)

  (let ((current-line (line-number-at-pos))
        current-word
        (case-fold-search nil))
    (setq word-list/prepared nil)

    (when (not (= current-line word-list/last-line))
      (setq word-list/last-line current-line)

      (setq current-word (word-list/find-current-word))

      (when (and current-word
                 (not (s-blank? current-word)))

        ;; ensure open window
        (when (< (count-windows nil) 2)
          (split-window-right))
        (other-window 1)

        ;; find match
        (unless
            (catch 'done
              (--each word-list/plus-files
                (let ((b (get-buffer it)))
                  (if b
                      (switch-to-buffer  b)
                    (find-file (concat word-list/plus-directory it))))
                (goto-char (point-min))
                (when (re-search-forward (concat "^\\[ " current-word) nil t)
                  (recenter 0)
                  (prompt/fold-plus-lines)
                  (throw 'done t))))
          (message "«%s» not found :(" current-word))

        ;; go back to other window
        (other-window -1))
      )))


(provide 'word-list-minor-mode)
;;; word-list-minor-mode.el ends here
