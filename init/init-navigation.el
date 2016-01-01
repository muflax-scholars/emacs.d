;; buffer navigation

;; move to beginning of text on line
(defun smart-beginning-of-line ()
  "Move point to one step left towards the beginning of the line.

Point stops at elastic tab stops, beginning of the visual line and eventually the real beginning of the line."
  (interactive)

  (let ((positions
         (list
          ;; first text
          (save-excursion (beginning-of-line-text)
                          (point))

          ;; visual line
          (save-excursion (beginning-of-visual-line)
                          (point))

          ;; real line
          (line-beginning-position))))

    ;; filter it
    (setq positions (--filter (< it (point))
                              positions))

    (when positions
      (goto-char (apply 'max positions)))))

(defun smart-end-of-line ()
  "Move point to one step right towards the end of the line.

Point stops at elastic tab stops, end of the visual line and eventually the real end of the line."
  (interactive)

  (let ((positions
         (list
          ;; last text
          (save-excursion (end-of-line)
                          (skip-syntax-backward "-" (line-beginning-position))
                          (point))

          ;; visual line
          (save-excursion (end-of-visual-line)
                          (point))

          ;; real line
          (line-end-position))))

    ;; filter it
    (setq positions (--filter (> it (point))
                              positions))

    (when positions
      (goto-char (apply 'min positions)))))

;; org-mode has similar behavior built-in, so use it instead
(load-after 'org-mode
  (setq org-special-ctrl-a/e t))

;; handle camelcase better
(require 'subword)
(add-hook 'prog-mode-hook 'subword-mode)

(defun toggle-subword-mode ()
  "Switch between subword/superword-mode."
  (interactive)
  (if global-subword-mode
      (if (fboundp 'global-superword-mode)
          (global-superword-mode 1)
        (global-subword-mode -1))
    (global-subword-mode 1)))

(provide 'init-navigation)
