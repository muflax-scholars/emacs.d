;;; corpus-minor-mode.el ---

(require 'dired-open)
(require 'hideshow)

(defvar corpus-minor-mode-map (make-keymap))
(define-minor-mode corpus-minor-mode
  "A minor mode for working with corpus file and moving stuff to templates."
  nil " C" corpus-minor-mode-map)

(defvar corpus/window-margin 12)

;; opening corpus files

(defun corpus/prepare-corpus-file ()
  (interactive)

  ;; fix display
  (goto-char (point-min))
  (when (re-search-forward "^\\[ \\(object\\|verb\\) sentences" nil t)
    (forward-line 1)
    (hs-hide-level 1))

  (re-search-forward "^\\[ sentences" nil t)
  (prompt/next-example-block)

  (corpus-minor-mode 1))

(defun corpus/open-corpus-file ()
  (interactive)

  (let (cur-word
        (dir prompt/corpus-file-dir)
        (case-fold-search nil))
    (case last-command
      ('corpus/open-corpus-file ;; just go to the next file
       (dired-next))
      (t ;; open correct file
       (setq cur-word (word-list/simplify-word
                       (first (s-match "^\\S-+" (which-function)))))
       (other-window 1)
       (find-file dir)
       (goto-char (point-min))
       (forward-line 4)
       (re-search-forward (concat "[0-9]+ - " cur-word "\\b"))
       (dired-open-file)))

    (corpus/prepare-corpus-file)
    ;; (other-window 1)
    ))

(defun corpus/open-next-corpus-file ()
  (interactive)
  (dired-next)

  (corpus/prepare-corpus-file))

(defun corpus/open-prev-corpus-file ()
  (interactive)
  (dired-prev)

  (corpus/prepare-corpus-file))

;; editing

(defun corpus/copy-example-block-over ()
  (interactive)
  (let* ((block-pos     	(prompt/size-of-current-block))
         (example-block 	(apply 'prompt/grab-block-string block-pos))
         (example-regexp	"^[ \t]+[?!+><]")
         (plus-regexp   	"^[ \t]+[+]")
         (line-regexp   	"^[ \t]+[?!<>#*]")
         end)

    ;; switch window
    (other-window 1)

    ;; find end of block over there as well
    (setq end
          (catch 'done
            (while t
              (beginning-of-line)
              (if (looking-at-p line-regexp)
                  (forward-line)
                (progn
                  (forward-line -1)
                  (end-of-line)
                  (throw 'done (point)))))))

    (unless (save-excursion
              (beginning-of-line)
              (looking-at-p plus-regexp))
      (newline-and-indent))
    (newline-and-indent)
    (insert example-block)
    (indent-region end (point))
    (corpus/clean-prompt-lines-region end (point))

    ;; switch back to other window
    (other-window 1)

    ;; go to next block, if possible
    (forward-line)
    (while (not (looking-at-p example-regexp))
      (forward-line))
    (beginning-of-line-text)))

(defun corpus/clean-prompt-lines-region (beg end)
  (let ((cur beg)
        eol
        (line-regexp "^[ \t]+[?!]")
        (lines 0))
    (save-excursion
      (while (< cur end)
        (goto-char cur)

        (when (looking-at-p line-regexp)
          (setq eol (min (point-at-eol) end))
          (setq lines (1+ lines))

          ;; remove garbage in the beginning
          (goto-char cur)
          (beginning-of-line-text)
          (when (looking-at "\\([\" -]+\\)")
            (replace-match "" nil nil))

          ;; remove useless {}
          (goto-char cur)
          (while (re-search-forward "{\\([^} \t]+\\)}" eol t)
            (replace-match "\\1" nil nil))

          ;; remove []
          (goto-char cur)
          (while (re-search-forward "\\[\\([^]]+\\)\\]" eol t)
            (replace-match "\\1" nil nil))

          ;; remove <> blocks
          (goto-char cur)
          (while (re-search-forward "<[^>]+>" eol t)
            (replace-match "" nil nil))

          ;; remove space before punctuation
          (goto-char cur)
          (while (re-search-forward "\\(\\sw\\)[ \t]+\\([.?!,]\\)" eol t)
            (replace-match "\\1\\2" nil nil))

          ;; show some status
          (when (= (mod lines 100) 0)
            (message "%d lines done..." lines)
            (sit-for 0.01))
          )

        (goto-char cur)
        (forward-line)
        (setq cur (point))
        ))))

(defun corpus/clean-prompt-lines ()
  (interactive)
  (corpus/clean-prompt-lines-region
   (if mark-active (region-beginning) (point-at-bol))
   (if mark-active (region-end)       (point-at-eol))))

(defun corpus/add-example-to-plus-line ()
  (interactive)

  ;; position cursor in templates file
  (other-window 1)
  (prompt/select-plus-line)
  (prompt/end-of-plus-block)
  (other-window 1)

  (corpus/copy-example-block-over)

  ;; refold? unsure; let's not for now
  ;; (other-window 1)
  ;; (fold-plus-lines)
  ;; (other-window 1)

  )

(provide 'corpus-minor-mode)
;;; corpus-minor-mode.el ends here
