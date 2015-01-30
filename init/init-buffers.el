;; buffer management

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; undo window changes
(require 'winner)
(winner-mode 1)

;; scratchpad buffers
(require 'scratch)

;; don't spam *Scratch*
(setq initial-scratch-message nil)

;; clean up buffers every once in a while
(require 'midnight)
(midnight-delay-set 'midnight-delay 0)

;; sticky windows
(require 'sticky-windows)

;; tree sidebar
(require 'neotree)
(setq neo-show-header nil)

(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p)
              (not p))
         (widen))

        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))

        ((and (fboundp 'org-narrow-to-subtree)
              (derived-mode-p 'org-mode)
              (org-narrow-to-subtree)))

        (t
         (narrow-to-defun))))

;; navigate windows
(require 'buffer-move)

(defalias 'focus-next-window 'other-window)

(defun focus-prev-window ()
  (interactive)
  (other-window -1))

(defun split-window-above ()
  (interactive)
  (split-window-below)
  (buf-move-down))

(defun split-window-left ()
  (interactive)
  (split-window-right)
  (buf-move-right))

(provide 'init-buffers)
