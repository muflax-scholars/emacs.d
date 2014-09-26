;;; shell scripting

(setup-lazy '(eshell) "eshell"
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

  (setup "esh-help"
    (setup-esh-help-eldoc)
    (add-hook 'eshell-mode-hook 'turn-on-eldoc-mode))

  (setup "esh-buf-stack"
    (setup-eshell-buf-stack))

  ;; completion
  (setq eshell-show-lisp-completions	nil)
  (setq eshell-show-lisp-alternative	t)

  ;; prompt
  (setq eshell-scroll-to-bottom-on-input 	'this)
  (setq eshell-scroll-to-bottom-on-output	'this)

  ;; history
  (setq eshell-history-size        	10000)
  (setq eshell-buffer-maximum-lines	100000)



  )

(provide 'init-shell)
