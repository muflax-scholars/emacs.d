;; buffer management

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; undo window changes
(require 'winner)
(winner-mode 1)

;; don't spam *Scratch*
(setq initial-scratch-message nil)

;; recreate scratch when saving it
(require 'scratch-ext)
(setq scratch-ext-log-directory (emacs-d "scratch/"))

;; clean up buffers every once in a while
(require 'midnight)
(midnight-delay-set 'midnight-delay 0)

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

(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      `(("Default"
         ("Dired"    	(mode . dired-mode))
         ("Magit"    	(name . "\*magit-.*\*"))
         ("Temporary"	(name . "\*.*\*"))
         )))
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-use-other-window t)

(provide 'init-buffers)
