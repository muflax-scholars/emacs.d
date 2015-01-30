;;; shell scripting

;; make zsh aliases work
(require 'shell-command)
(setq shell-command-switch "-lc")
;; tab-completion for shell-command
;; FIXME not working yet, but meh
(shell-command-completion-mode)

;; better handling than M-| / M-!
(defun generalized-shell-command (command arg)
  "Unifies `shell-command' and `shell-command-on-region'.
You have:
- (no arg)    	run command and place output
- (C-u)       	... don't chomp output
- (region)    	replace region with output from command
- (C-u region)	... and print to minibuffer" ; TODO: make this also chomp
  (interactive (list (read-from-minibuffer "$ " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; no active region, so just output the output
        (if (eq arg nil)
            (insert (chomp (shell-command-to-string command)))
          (shell-command command t))
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region p m command t t)
        (shell-command-on-region p m command)))))

(load-lazy '(eshell) "eshell"
  ;; used core modules
  (setq eshell-modules-list
        '(eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-unix))

  (require 'esh-help)
  (setup-esh-help-eldoc)
  (add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)

  (require 'esh-buf-stack)
  (setup-eshell-buf-stack)

  (require 'mv-shell)

  ;; completion
  (setq eshell-show-lisp-completions	nil)
  (setq eshell-show-lisp-alternative	t)

  ;; prompt
  (setq eshell-scroll-to-bottom-on-input 	'this)
  (setq eshell-scroll-to-bottom-on-output	'this)

  ;; history
  (setq eshell-history-size        	10000)
  (setq eshell-buffer-maximum-lines	100000)

  (defun eshell-current-directory (&optional arg)
    (interactive)
    (let ((eshell-buffer-name (concat "*eshell*-" default-directory)))
      (eshell arg)))
  )

(provide 'init-shell)
