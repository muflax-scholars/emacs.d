;; auto-completion nonsense

;; snippets
(setup "yasnippet"
  (setq yas-snippet-dirs
        '(
          "~/.emacs.d/snippets"
          "~/spoiler/languages/.snippets"
          ))
  (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
  (define-key yas-keymap (kbd "C-o") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-O") 'yas-next-field)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (setq yas-indent-line 'fixed)
  (define-key yas-minor-mode-map (kbd "C-c C-o") 'yas-reload-all)
  (yas-global-mode 1))

;; auto-yasnippet
(setup-after "yasnippet"
  (global-set-key (kbd "C-c ~")   'aya-create)
  (global-set-key (kbd "C-c C-~") 'aya-expand)
  (setup-lazy '(aya-create aya-expand) "auto-yasnippet"))

;; auto completion
(setup "auto-complete-config"
  (add-to-list 'ac-modes 'enh-ruby-mode)
  (add-to-list 'ac-modes 'go-mode)
  ;; (add-to-list 'ac-modes 'notes-mode)

  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")
  (setq ac-auto-show-menu nil)
  (setq ac-ignore-case nil)
  (ac-config-default)

  ;; saner keys
  (setq ac-use-menu-map nil)
  (ac-set-trigger-key "C-t")
  (global-set-key (kbd "C-t") 'ac-trigger-key-command)
  (define-key ac-completing-map "\t"        nil)
  (define-key ac-completing-map [tab]       nil)
  (define-key ac-completing-map (kbd "<Tab>") nil)
  (define-key ac-completing-map [up]        nil)
  (define-key ac-completing-map [down]      nil)
  (define-key ac-completing-map (kbd "M-n") nil)
  (define-key ac-completing-map (kbd "M-p") nil)
  (define-key ac-completing-map (kbd "C-t") 'ac-next)
  (define-key ac-completing-map (kbd "M-t") 'ac-previous)
  (define-key ac-completing-map [return]    nil)
  (define-key ac-completing-map "\r"        nil)
  (define-key ac-completing-map (kbd "C-j") 'ac-complete)
  )

;; fancy go autocompletion
(setup-after "go-mode"
  (setup "go-autocomplete"))

(provide 'init-auto-completion-nonsense)
