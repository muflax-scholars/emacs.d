;;; elastic-tabstops.el ---

;; works (kinda) like Elastic Tabstops in Sublime

;; bugs:
;; - can't add spaces in current cell
;; - newline broken at EOF
;; - doesn't work nicely with indent
;; - delete should be smart: first only delete to [\t\n], second delete these too


(eval-when-compile 'cl-lib)
(eval-when-compile 'dash)

;; tabs separate block, spaces don't count
(defconst elastic-line-separator-regexp "[ ]*[\t]")

(cl-defstruct elastic-scope
  beg end     	; size of the scope
  widths-lines	; width of each cell in each line
  target)     	; target width of each cell

(defun elastic-tabs-width-of-line ()
  "Returns list of cell widths in current line."
  (let (widths
        start
        last
        width
        (end (point-at-eol)))

    (save-excursion
      (beginning-of-line)
      (setq start	(point))
      (setq last 	start)

      ;; width starts after the tab, and ends before the (spaces and) tab
      (while (re-search-forward elastic-line-separator-regexp end t)
        ;; calculate width
        (setq width (- (match-beginning 0) last))

        ;; add to list and proceed
        (setq last  	(match-end 0)
              widths	(cons width widths)))

      ;; add last item, if possible and we have at least one other field
      (when (and (< (point) end)
                 widths)
        (goto-char end)
        (skip-chars-backward " \t")
        (setq width 	(- (point) last)
              widths	(cons width widths))))

    (nreverse widths)))

(defun elastic-calc-max (a b)
  "Returns list of size (max a b) with the max-value of both list at that position."
  (let ((a-val 	(first a))
        (b-val 	(first b))
        (a-rest	(rest a))
        (b-rest	(rest b))
        ret)
    (while (or a-val b-val)
      (setq ret (cons (max (or a-val 0) (or b-val 0)) ret)
            ;; eat the list
            a-val 	(first a-rest)
            b-val 	(first b-rest)
            a-rest	(rest a-rest)
            b-rest	(rest b-rest)))

    (nreverse ret)))

(defun elastic-block-scope ()
  "Returns elastic scope of the current elastic block, or nil."
  (let (beg
        end
        widths-line
        widths-lines
        target)

    ;; walk upwards (and note the beginning)
    (save-excursion
      (beginning-of-line)

      (while (and (setq widths-line (elastic-tabs-width-of-line))
                  (not (bobp)))
        (setq beg         	(point)
              widths-lines	(cons widths-line widths-lines)
              target      	(elastic-calc-max widths-line target))
        (forward-line -1))
      (setq end (point-at-eol)))

    ;; walk downwards, if we have a block
    (when beg
      (save-excursion
        (forward-line 1)

        ;; reverse list for faster adding
        (setq widths-lines (nreverse widths-lines))

        (while (and (setq widths-line (elastic-tabs-width-of-line))
                    (not (eobp)))
          (setq widths-lines	(cons widths-line widths-lines)
                target      	(elastic-calc-max widths-line target))
          (forward-line 1))

        ;; put it back in normal order (and note the end)
        (setq widths-lines (nreverse widths-lines)
              end (point-at-eol))))

    ;; return scope
    (if beg
        (make-elastic-scope :beg         	beg
                            :end         	end
                            :widths-lines	widths-lines
                            :target      	target)
      nil)))

(defun elastic-align-block (scope)
  "align given block"

  (when scope
    (let ((beg   	(elastic-scope-beg         	scope))
          (target	(elastic-scope-target      	scope))
          (lines 	(elastic-scope-widths-lines	scope)))

      (save-excursion
        (goto-char beg)
        (--each lines
          (let (diff start)
            (--each (-zip it target)
              ;; it -> (width, target width)

              ;; get into position
              (forward-char (first it))

              ;; delete any other spaces
              (setq start (point))
              (skip-chars-forward "^\t\n")
              (delete-region start (point))

              ;; not yet in the last cell
              (when (= (following-char) ?\t)
                ;; add correct number of spaces
                (setq diff (- (rest it) (first it)))
                (insert-char ?\s diff)

                ;; get into position for the next cell
                (forward-char 1))))


          ;; get ready for next line
          (forward-line 1))
        ))))

(defun elastic-align-current ()
  "elastic-align current block"
  (elastic-align-block (elastic-block-scope)))

(defun elastic-align-region (beg end)
  "elastic-align selected region"
  (let (scope)
    (save-excursion
      (goto-char beg)

      (while (<= (point) end)
        (setq scope (elastic-block-scope))
        (when scope
          (elastic-align-block scope)
          (goto-char (elastic-scope-end scope)))
        (forward-line 1)))))

(defun elastic-align-region-or-current ()
  "Elastic-align current or selected block(s)."
  (interactive)
  (if (and mark-active
           (/= (point) (mark)))
      (elastic-align-region (point) (mark))
    (elastic-align-current)))

(defun elastic-align-change-hook (beg end len)
  "Called by the change hook."
  (save-excursion
    (elastic-align-region beg end)))

(define-minor-mode elastic-tabstops-minor-mode
  "Automatically adjusts elastic tabstops as you type."
  nil " »" nil

  (if elastic-tabstops-minor-mode
      (add-hook 	'after-change-functions 'elastic-align-change-hook t t)
    (remove-hook	'after-change-functions 'elastic-align-change-hook t)))

(provide 'elastic-tabstops)
;;; elastic-tabstops.el ends here
