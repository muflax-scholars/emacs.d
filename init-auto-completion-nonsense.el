;; auto-completion nonsense

;; snippets
(setup "yasnippet"
  (setq yas-snippet-dirs
        '(
          "~/.emacs.d/snippets"
          "~/spoiler/languages/.snippets"
          ))

  ;; inter-field navigation
  (defun yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))

  (defun yas-choose-from-file (filename)
    "Loads choices for 'yas-choose-value' from file, treating each line as an option."
    (if (file-exists-p filename)
        (yas-choose-value (read-lines filename))
      '("")))

  (defun yas-choose-from-command (command)
    "Loads choices for 'yas-choose-value' from shell command, treating each line as an option."

    (yas-choose-value (split-string (shell-command-to-string command) "\n" t)))

  ;; options
  (setq yas-indent-line 'fixed)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (setq yas-triggers-in-field t)

  ;; no dropdowns
  (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

  (yas-global-mode 1))

;; auto-yasnippet
(setup-after "yasnippet"
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
  (ac-config-default))

;; fancy go autocompletion
(setup-after "go-mode"
  (setup "go-autocomplete"))

(provide 'init-auto-completion-nonsense)
