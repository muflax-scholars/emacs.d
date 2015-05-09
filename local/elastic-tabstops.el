;;; elastic-tabstops.el ---

;; works (kinda) like Elastic Tabstops in Sublime

;; bugs with minor mode:
;; - can't add spaces in current cell
;; general bugs:
;; - mc might want a workaround to speed it up
;; - tabs have one physical / two visual width, and that messes up some width calc; use "current column"?

(eval-when-compile 'cl-lib)

;; tabs separate block, spaces don't count
(defconst elastic-line-separator-regexp "[ ]*[\t]")

(cl-defstruct elastic-scope
  beg end     	; size of the scope
  widths-lines	; width of each cell in each line
  target)     	; target width of each cell

(defcustom elastic-extend-columns nil
  "If non-nil, add tabs to the end of lines if necessary so that all lines have the same number of columns.")
(make-variable-buffer-local 'elastic-extend-columns)

(defcustom elastic-tab-align-modes '()
  "modes that align elastic tabstops during indent")

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

      ;; skip indentation
      (skip-chars-forward "\t")

      ;; width starts after the tab, and ends before the (spaces and) tab
      (while (re-search-forward elastic-line-separator-regexp end t)
        ;; calculate width
        (setq width (- (match-beginning 0) last))

        ;; add to list and proceed
        (setq last  	(match-end 0)
              widths	(cons width widths)))

      ;; add last item, if possible and we have at least one previous field
      (when widths
        (goto-char end)
        (skip-chars-backward " \t" last)
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
        target
        (first-line t)
        )

    ;; walk upwards (and note the beginning)
    (save-excursion
      (beginning-of-line)

      (catch :done
        (while (setq widths-line
                     (elastic-tabs-width-of-line))
          (setq beg         	(point)
                widths-lines	(cons widths-line widths-lines)
                target      	(elastic-calc-max widths-line target))

          ;; go up if we can, or stop
          (if (not (bobp))
              (forward-line -1)
            (throw :done t))))

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

    ;; cut off empty end of target
    (when target
      (setq target (nreverse target))
      (when (= 0 (head target))
        (while (= 0 (head target))
          (setq target (tail target)))
        (setq target (cons 0 target)))
      (setq target (nreverse target)))

    ;; return scope
    (if beg
        (make-elastic-scope :beg         	beg
                            :end         	end
                            :widths-lines	widths-lines
                            :target      	target)
      nil)))

(defun elastic-align-block (scope)
  "Align given SCOPE."

  (when scope
    (let ((beg   	(elastic-scope-beg         	scope))
          (target	(elastic-scope-target      	scope))
          (lines 	(elastic-scope-widths-lines	scope))
          target-width target-left
          cell-width
          start diff)

      (save-excursion
        (goto-char beg)

        (dolist (line lines)
          (setq target-left target)

          ;; iterate through all target cells
          (while (and target-left
                      (or line elastic-extend-columns))
            (setq target-width	(or (head target-left) 0)
                  target-left 	(tail target-left)
                  cell-width  	(head line)
                  line        	(tail line)
                  diff        	0)

            ;; jump over existing cell
            (when cell-width (forward-char cell-width))
            (setq start (point))

            ;; skip over existing spaces so we don't cause anything to flicker
            (skip-chars-forward " ")

            ;; add spaces unless we are in the last cell or are supposed to extend them
            (when (or line
                      (and elastic-extend-columns target-left)
                      (and (> target-width 0) (= (following-char) ?\t)))
              (setq diff (- (- target-width (or cell-width 0))
                            (- (point) start))))

            (cond ((> diff 0)	(insert-char ?\s	diff))
                  ((< diff 0)	(delete-char    	diff)))

            ;; get into position for the next cell, and maybe put in the new cell
            (when (and target-left
                       (or line elastic-extend-columns))
              (if (= (following-char) ?\t)
                  (forward-char 1)
                (insert ?\t)))
            )

          ;; delete trailing whitespace
          (setq start (point))
          (skip-chars-forward " \t")
          (delete-char (- start (point)))

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
      (goto-char (min beg end))

      (while (and (<= (point) (max beg end))
                  (not (eobp)))
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

(defmacro elastic-advice-command (command-name)
  "Align tabstops after running COMMAND-NAME."
  `(defadvice ,command-name (after elastic-tabstops activate)
     (when (member major-mode elastic-tab-align-modes)
       (elastic-align-current))))

;; TODO should be smarter and hijack the command's arguments
(defmacro elastic-advice-command-region (command-name)
  "Align tabstops in active region after running COMMAND-NAME."
  `(defadvice ,command-name (after elastic-tabstops activate)
     (when (member major-mode elastic-tab-align-modes)
       (elastic-align-region (point) (mark)))))

(defun elastic-strip-redundant-tabs-region (beg end)
  (interactive "r")

  (save-excursion
    (goto-char beg)
    (while (re-search-forward (rx (>= 2 "\t")) end t)
      (replace-match "\t" nil nil))))

(defun elastic-strip-redundant-tabs ()
  (interactive)
  (elastic-strip-redundant-tabs-region (point-min) (point-max)))

(defun elastic-turn-on-extended-columns ()
  (set (make-local-variable 'elastic-extend-columns) t))

(define-minor-mode elastic-tabstops-minor-mode
  "Automatically adjusts elastic tabstops as you type."
  nil " »" nil

  (if elastic-tabstops-minor-mode
      (add-hook 	'after-change-functions 'elastic-align-change-hook t t)
    (remove-hook	'after-change-functions 'elastic-align-change-hook t)))

(provide 'elastic-tabstops)
;;; elastic-tabstops.el ends here
