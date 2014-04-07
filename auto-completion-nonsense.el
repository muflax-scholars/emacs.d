;; auto-completion nonsense

;; snippets
(setup "yasnippet"
  (setq yas-snippet-dirs "~/.emacs.d/snippets")
  (define-key yas-minor-mode-map (kbd "C-$") 'yas-expand)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (yas-global-mode 1))

;; auto-yasnippet
(setup "auto-yasnippet"
  (global-set-key (kbd "C-c ~")   'aya-create)
  (global-set-key (kbd "C-c C-~") 'aya-expand))

;; auto completion
(setup "auto-complete-config"
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (add-to-list 'ac-modes 'enh-ruby-mode)
  ;; (add-to-list 'ac-modes 'text-mode)
  ;; (add-to-list 'ac-modes 'markdown-mode)
  ;; (add-to-list 'ac-modes 'notes-mode)
  ;; (add-to-list 'ac-modes 'org-mode)
  (setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")
  (setq ac-auto-show-menu nil)
  (setq ac-ignore-case nil)
  (ac-config-default)

  ;; disabling Yasnippet completion
  (defun yasnippet-snippets-from-table (table)
    (with-no-warnings
      (let ((hashtab (ac-yasnippet-table-hash table))
            (parent (ac-yasnippet-table-parent table))
            candidates)
        (maphash (lambda (key value)
                   (push key candidates))
                 hashtab)
        (identity candidates)
        )))
  (defun yasnippet-get-all-snippets ()
    (let (candidates)
      (maphash
       (lambda (kk vv) (push (yasnippet-snippets-from-table vv) candidates)) yas--tables)
      (apply 'append candidates))
    )
  (setq ac-ignores (concatenate 'list ac-ignores (yasnippet-get-all-snippets)))
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
  (define-key ac-completing-map (kbd "C-j") 'ac-complete))

;; fancy go autocompletion
(setup-after "auto-complete"
  (setup "go-autocomplete"))

(provide 'auto-completion-nonsense)
