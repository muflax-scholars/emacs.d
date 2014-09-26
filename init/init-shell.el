;;; shell scripting

(setup-lazy '(eshell) "eshell"
  (setup "esh-help"
    (setup-esh-help-eldoc)
    (add-hook 'eshell-mode-hook 'turn-on-eldoc-mode))

  (setup "esh-buf-stack"
    (setup-eshell-buf-stack))

  )

(provide 'init-shell)
