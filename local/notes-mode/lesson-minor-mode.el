;;; lesson-minor-mode.el ---

(require 'ample-regexps)

(defvar lesson-minor-mode-map (make-keymap))
(define-minor-mode lesson-minor-mode
  "A minor mode for presenting lessons."
  nil " ?!" lesson-minor-mode-map

  (if lesson-minor-mode
      (progn
        (read-only-mode 1)

        (if hl-line-mode
            (setq lesson/hl-line-mode-was-on t)
          (setq lesson/hl-line-mode-was-on nil)
          (hl-line-mode 1))

        (set-face-background 'cursor "red")

        (smooth-scrolling-mode nil))
    (progn
      (lesson/unhighlight-blocks)
      (read-only-mode 0)

      (unless lesson/hl-line-mode-was-on
        (hl-line-mode 0))
      (setq lesson/hl-line-mode-was-on nil)

      (set-face-background 'cursor lesson/cursor-old-color)

      (smooth-scrolling-mode t))
    ))

(defvar lesson/window-margin 15)
(defvar lesson/scroll-margin 0)
(defvar lesson/hl-line-mode-was-on nil)
(defvar lesson/cursor-old-color (face-background 'cursor))

(define-arx lesson/rx
  '(
    (lesson-block	(seq bol (* blank) (any "!" "?" "$")))
    (blank-line  	(seq bol (* blank) eol))
    (lesson-file 	(seq bol "L" (+ digit)))
    (sentence    	(seq bol (* blank) "!" (? (any "!" "?")) (+ blank) (+ nonl)))
    (translation 	(seq sentence "\t?"))
    (section     	(seq bol "[ #" (any "S" "IS" "SP") (or (+ blank) eol)))
    (times       	(seq "{" (* nonl) "}"))
    ))

;; blocks

(defun lesson/find-next-block ()
  (save-excursion
    (lesson/end-of-block)
    (re-search-forward (lesson/rx lesson-block) nil t)

    (lesson/current-block)))

(defun lesson/find-prev-block ()
  (save-excursion
    (lesson/beginning-of-block)
    (re-search-backward (lesson/rx lesson-block) nil t)

    (lesson/current-block)))

(defun lesson/current-block ()
  (let (block-beg
        block-end)
    (save-excursion
      (lesson/beginning-of-block)
      (setq block-beg (point))

      (lesson/end-of-block)
      (setq block-end (point)))

    (list block-beg block-end)))

(defun lesson/beginning-of-block ()
  (interactive)

  (let (moved)
    (beginning-of-line)
    (while (and (looking-at (lesson/rx lesson-block))
                (not (bobp)))
      (setq moved t)
      (forward-line -1))
    (when moved (forward-line 1))))

(defun lesson/end-of-block ()
  (interactive)

  (let (moved)
    (beginning-of-line)
    (while (and (looking-at (lesson/rx lesson-block))
                (not (eobp)))
      (setq moved t)
      (forward-line 1))
    (when moved (forward-char -1))))

(defun lesson/next-thing ()
  (interactive)

  (let ((cur-block (lesson/current-block))
        in-block)

    (--each (overlays-at (point))
      (when (overlay-get it 'lesson-block)
        (setq in-block t)))

    (cond
     (in-block (cond ((= (point-at-eol)
                         (second cur-block))
                      ;; go to next block
                      (lesson/next-block))
                     (t
                      ;; go to next line in block
                      (forward-line 1)
                      )))
     (t
      ;; find block
      (apply 'lesson/start-block cur-block)
      ))))

(defun lesson/prev-thing ()
  (interactive)

  (let ((cur-block (lesson/current-block))
        in-block)

    (--each (overlays-at (point))
      (when (overlay-get it 'lesson-block)
        (setq in-block t)))

    (cond
     (in-block (cond ((= (point-at-bol)
                         (first cur-block))
                      ;; go to prev block
                      (lesson/prev-block))
                     (t
                      ;; go to prev line in block
                      (forward-line -1)
                      )))
     (t
      ;; find block
      (apply 'lesson/start-block (lesson/find-prev-block))
      (lesson/end-of-block)
      ))))

(defun lesson/next-block ()
  (interactive)

  (apply 'lesson/start-block (lesson/find-next-block)))

(defun lesson/prev-block ()
  (interactive)

  (apply 'lesson/start-block (lesson/find-prev-block)))

(defun lesson/start-block (beg end)
  (lesson/highlight-block beg end)
  (goto-char beg))

(defun lesson/smooth-recenter ()
  (when (< (smooth-scroll-lines-below-point)
           lesson/window-margin)
    (recenter (- lesson/window-margin))))

(defun lesson/highlight-block (beg end)
  ;; clear old highlights
  (lesson/unhighlight-blocks)

  ;; add new highlight
  (let ((ov (make-overlay beg (1+ end))))
    (overlay-put ov 'lesson-block t)
    (overlay-put ov 'priority 10000)
    ;; (overlay-put ov 'face '(:background "#f6f6f6"))
    ))

(defun lesson/unhighlight-blocks ()
  (interactive)

  (remove-overlays (point-min) (point-max) 'lesson-block t))

(defun lesson/goto-middle-of-screen ()
  (goto-char (window-start))
  (forward-line (/ (window-height)
                   2))
  (beginning-of-line))

(defun lesson/scroll-up-block ()
  (interactive)

  (save-excursion
    (let ((cur (point)))
      (lesson/goto-middle-of-screen)
      (goto-char (max (point) cur)))

    (goto-char (first (lesson/find-next-block)))
    (lesson/smooth-recenter)))

(defun lesson/scroll-down-block ()
  (interactive)

  (save-excursion
    (let ((cur (point)))
      (lesson/goto-middle-of-screen)
      (goto-char (min (point) cur)))

    (goto-char (first (lesson/find-prev-block)))
    (lesson/smooth-recenter)))

(defun lesson/scroll-up-line ()
  (interactive)

  (unless (<= (smooth-scroll-lines-above-point)
           lesson/scroll-margin)
    (scroll-up-line)))

(defun lesson/scroll-down-line ()
  (interactive)

  (unless (<= (1- (smooth-scroll-lines-below-point))
              lesson/scroll-margin)
    (scroll-down-line)))

(defun lesson/add-translations-to-block ()
  (interactive)

  (trim-whitespace)

  (let* ((cur-block (lesson/current-block))
         (beg (first 	cur-block))
         (end (second	cur-block))
         )
    (save-excursion
      (goto-char beg)

      (while (< (point) end)
        (when (looking-at (lesson/rx sentence))
          (unless (looking-at (lesson/rx translation))
            (end-of-line)
            (when (= (preceding-char) ?\t)
              (delete-char -1)
              (setq end (- end 1)))
            (insert "\t? ")
            (setq end (+ end 3))))

        (forward-line 1)))

    (elastic-align-region beg end))

  (end-of-line))

;; verbs

(defvar lesson/umers
  (mapcar 'symbol-name
          '(schlafen fahren tragen schlagen graben laufen fallen fangen waschen blasen lassen verlassen wachsen laden braten raten geraten halten stoßen saufen)))
(defvar lesson/ioters
  (mapcar 'symbol-name
          '(sehen stehlen befehlen empfehlen geschehen gebären treffen helfen werfen sterben sprechen brechen stechen schwellen dreschen melken werben verderben erlöschen quellen schrecken erschrecken bergen geben nehmen lesen vergessen messen essen fressen schmelzen treten flechten fechten gelten schelten bersten)))

(defun lesson/mark-strong-verbs ()
  (interactive)

  (let* ((case-fold-search	nil)
         (regexp-skel     	"^\\([ \t]*<.+\\b[a-zäöüß]*\\(?:%s\\)\\b.*\\)$")
         (regexp-umers    	(format regexp-skel (s-join "\\|" lesson/umers)))
         (regexp-ioters   	(format regexp-skel (s-join "\\|" lesson/ioters)))
         )

    (--each `((,regexp-umers 	"umer")
              (,regexp-ioters	"ioter"))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (first it) nil t)
          (replace-match
           (concat "\\1\t" (second it))))))))

;; file navigation

(defun lesson/next-lesson ()
  (interactive)

  (let ((start-buffer (current-buffer)))
    (dired-next)
    (unless (eq (current-buffer)
                start-buffer)
      (if (lesson/lesson-file?)
          (lesson-minor-mode 1)
        (dired-prev)))))

(defun lesson/prev-lesson ()
  (interactive)

  (let ((start-buffer (current-buffer)))
    (dired-prev)
    (unless (eq (current-buffer)
                start-buffer)
      (if (lesson/lesson-file?)
          (lesson-minor-mode 1)
        (dired-next)))))

(defun lesson/lesson-file? ()
  (s-match (lesson/rx lesson-file)
           (file-name-nondirectory (buffer-file-name))))

;; maintenance
(defun lesson/find-sentences (beg end)
  (let (sentences)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end)
        (when (looking-at "^[ \t]*![!?]?[ \t]+.+")
          (setq sentences (cons (point) sentences)))
        (forward-line 1)))
    (reverse sentences)))

(defun lesson/count-sentences (beg end)
  (length (lesson/find-sentences beg end)))

(defun lesson/time-estimate (count)
  (if (> count 0)
      (* 5 (+ 1 (/ count
                   (* 5 5))))
    0))

(defun lesson/format-minutes (minutes)
  (format "%d:%02d" (/ minutes 60) (% minutes 60)))

(defun lesson/find-time-marks (beg end)
  (let (marks)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end)
        (when (and (looking-at-p (lesson/rx section))
                   (re-search-forward (lesson/rx times) (point-at-eol) t))
          (setq marks (cons (match-data 0) marks)))
        (forward-line 1)))
      ;; marks are returned backwards so we can easily replace them without worrying about offsets
    marks))

(defun lesson/update-time-mark (count-section count-total mark)
  (let* ((section  	(lesson/format-minutes (lesson/time-estimate count-section)))
         (total    	(lesson/format-minutes (lesson/time-estimate count-total)))
         (time-mark	(format "{%s - %s}" section total))
         (beg      	(first mark))
         (end      	(second mark)))
    (save-excursion
      (goto-char beg)
      (unless (string= (buffer-substring beg end)
                       time-mark)
        (delete-region beg end)
        (insert time-mark)))))

(defun lesson/update-time-marks ()
  (interactive)
  (let ((sentences	(lesson/find-sentences 	(point-min) (point-max)))
        (marks    	(lesson/find-time-marks	(point-min) (point-max)))
        count
        (last-section (point-max))
        )
        (save-excursion
          (dolist (mark marks)
            (let ((beg (first mark)))
              (setq count-total  	(length (--filter (<= it last-section) sentences))
                    count-section	(length (--filter (and (>= it beg)
                                                          (<= it last-section))
                                                     sentences))
                    last-section beg)
              (lesson/update-time-mark count-section count-total mark))))))

(defun lesson/remove-reading-mark ()
  (interactive)
  (save-excursion
    (flush-lines "^>>[ \t]*$" (point-min) (point-max))))

(defun lesson/remove-hash-mark ()
  (interactive)
  (save-excursion
    (flush-lines "^##[ \t]*$" (point-min) (point))))

(defun lesson/set-reading-mark ()
  (interactive)
  (lesson/remove-reading-mark)
  (lesson/remove-hash-mark)
  (save-excursion
    (beginning-of-line)
    (insert ">>\n")))

(provide 'lesson-minor-mode)
;;; lesson-minor-mode.el ends here
