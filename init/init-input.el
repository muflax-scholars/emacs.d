;; complicated input methods

;; input methods, including a direct mozc binding to avoid ibus (requires mozc install)
(require 'custom-input-methods)

;; default to no input method
(setq default-input-method nil)
(defun turn-on-default-input-method ()
  (set-input-method default-input-method))
(add-hook 'text-mode-hook       	'turn-on-default-input-method)
(add-hook 'prog-mode-hook       	'turn-on-default-input-method)
(add-hook 'dired-mode-hook      	'turn-on-default-input-method)
(add-hook 'eshell-mode-hook     	'turn-on-default-input-method)
(add-hook 'minibuffer-setup-hook	'turn-on-default-input-method)
(add-hook 'occur-mode-hook      	'turn-on-default-input-method)
(add-hook 'phi-search-init-hook 	'turn-on-default-input-method)

;; don't underline partial input
(setq input-method-highlight-flag nil)

;;don't spam the minibuffer
(setq input-method-verbose-flag 'complex-only)

(defun clear-input-method ()
  (interactive)
  (set-input-method nil))

(defmacro set-input-method-fun (name)
  `(defun ,(intern (format "set-input-method-%s" name)) ()
     (interactive)
     (set-input-method ,name)))

(set-input-method-fun "muflax-latin")
(set-input-method-fun "muflax-cyrillic")
(set-input-method-fun "muflax-turkish")
(set-input-method-fun "muflax-greek")

(provide 'init-input)
