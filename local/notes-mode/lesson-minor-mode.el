;;; lesson-minor-mode.el ---

(require 'ample-regexps)

(defvar lesson-minor-mode-map (make-keymap))
(define-minor-mode lesson-minor-mode
  "A minor mode for presenting lessons."
  nil " ?!" lesson-minor-mode-map

  (if lesson-minor-mode
      (read-only-mode 1)
    (read-only-mode 0)))

(defvar lesson/window-margin 15)

(define-arx lesson/rx
  '(
    (lesson-block	(seq bol (* blank) (not (any "[" "]" "+" "$" blank "\n"))))
    (blank-line  	(seq bol (* blank) eol))
    ))

(defun lesson/find-next-block ()
  (let (block-beg
        block-end)
    (save-excursion
      (unless (re-search-forward (lesson/rx blank-line) nil t)
        (re-search-backward (lesson/rx blank-line) nil t))

      (re-search-forward (lesson/rx lesson-block) nil t)
      (beginning-of-line)
      (setq block-beg (point))

      (while (and (looking-at (lesson/rx lesson-block))
                  (not (eobp)))
        (forward-line 1))

      (unless (eobp)
        (forward-char -1))

      (setq block-end (point)))

    (list block-beg block-end)))

(defun lesson/find-prev-block ()
  (save-excursion
    (re-search-backward (lesson/rx blank-line)  	nil t)
    (re-search-backward (lesson/rx lesson-block)	nil t)
    (re-search-backward (lesson/rx blank-line)  	nil t)

    (lesson/find-next-block)))

;; FIXME: can't select the very first block of the file, if it's not whitespace-separated

(defun lesson/next-thing ()
  (interactive)

  (let (cur-block
        in-block
        )
    (save-excursion
      (re-search-backward (lesson/rx blank-line) nil t)
      (setq cur-block (lesson/find-next-block)))

    (--each (overlays-at (point))
      (when (overlay-get it 'lesson-block)
        (setq in-block t)))

    (cond
     (in-block (cond ((= (point-at-eol)
                         (second cur-block))
                      ;; go to next block
                      (lesson/next-block))
                     ;; (lesson/smooth-recenter)
                     (t
                      ;; go to next line in block
                      (forward-line 1)
                      ;; (lesson/smooth-recenter)
                      )))
     (t
      ;; find block
      (apply 'lesson/start-block cur-block)
      ;; (lesson/smooth-recenter)
      ))

    ))

(defun lesson/next-block ()
  (interactive)

  (apply 'lesson/start-block (lesson/find-next-block)))

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
    (overlay-put ov 'face '(:background "#f6f6f6"))))

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

(defun prev-lesson-block ()
  (interactive)

  (apply 'lesson/start-block (lesson/find-prev-block)))

(defun next-lesson-block ()
  (interactive)

  (apply 'lesson/start-block (lesson/find-next-block)))


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



(provide 'lesson-minor-mode)
;;; lesson-minor-mode.el ends here
