;; load path 
;; site-lisp stores manually maintained packages
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
         (default-directory my-lisp-dir))
    (progn
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; elpa
(require 'package)
(add-to-list 'package-archives '("tromey"    . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("sunrise"   . "http://joseito.republika.pl/sunrise-commander/"))
(package-initialize)

;; selective hooks for either terminals or X windows
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
  `after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
               'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)
(add-hook 'after-init-hook (lambda () (run-after-make-frame-hooks (selected-frame))))

;; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defvar use-bright-theme nil
  "Whether to use the bright or dark theme")

(defvar bright-theme 'tango
  "Bright theme to use")
(defvar dark-theme 'molokai
  "Bright theme to use")

(load-theme bright-theme t t)
(load-theme dark-theme t t)

(defun set-color-theme ()
  "sets appropriate color theme"
  (interactive)
  (if window-system
      (if use-bright-theme
          (progn (disable-theme dark-theme) (enable-theme bright-theme))
        (progn (disable-theme bright-theme) (enable-theme dark-theme)))))

(defun toggle-bright-theme ()
  "toggles between bright and dark theme"
  (interactive)
  (progn
    (if use-bright-theme (setq use-bright-theme nil) (setq use-bright-theme t))
    (set-color-theme)))
(global-set-key "\C-c\C-t" 'toggle-bright-theme)

(add-hook 'after-make-window-system-frame-hooks 'set-color-theme)
(add-hook 'after-make-console-frame-hooks       (lambda() (enable-theme dark-theme)))

;; fonts
(defvar small-font  "6x10")
(defvar normal-font "6x13")
(defvar big-font    "-gnu-unifont-*")
(defvar font-list (list
                   small-font
                   normal-font
                   big-font))
(defvar current-font normal-font)

(defun set-window-font ()
  (set-frame-font current-font))
(add-hook 'after-make-window-system-frame-hooks 'set-window-font)

(defun cycle-fonts ()
  "cycles through font list"
  (interactive)

  (let (currentState)
    ;; states starts from 1.
    (setq currentState (if (get this-command 'state) (get this-command 'state) 1))
    (setq current-font (nth (1- currentState) font-list))
    (put this-command 'state (1+ (% currentState (length font-list))))
    (set-window-font)))
(global-set-key "\C-c\C-f" 'cycle-fonts)

;; shortcut for the fonts
(defun use-big-font ()
  "use big font"
  (interactive)
  (setq current-font big-font)
  (set-window-font))
(defun use-normal-font ()
  "use normal font"
  (interactive)
  (setq current-font normal-font)
  (set-window-font))
(defun use-small-font ()
  "use small font"
  (interactive)
  (setq current-font small-font)
  (set-window-font))
(global-set-key (kbd "C-c <f1>") 'use-small-font)
(global-set-key (kbd "C-c <f2>") 'use-normal-font)
(global-set-key (kbd "C-c <f3>") 'use-big-font)

;; auctex
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; scrolling
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-progressive-speed nil)
;; smooth scrolling with margin
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
;; necessary or scrolling is really slow
(setq-default bidi-display-reordering  nil)
(setq auto-window-vscroll nil)

;; try to keep windows within a max margin
(require 'automargin)
(setq automargin-target-width 120)
(automargin-mode)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c k") 'mc/edit-lines)
(global-set-key (kbd "C-c >")   'mc/mark-next-like-this)
(global-set-key (kbd "C-c <")   'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-k") 'mc/mark-all-like-this)
(require 'phi-search)
(require 'phi-search-mc)
(define-key phi-search-default-map (kbd "C-c >") 'phi-search-mc/mark-next)
(define-key phi-search-default-map (kbd "C-c <") 'phi-search-mc/mark-previous)
(define-key phi-search-default-map (kbd "C-c C-k") 'phi-search-mc/mark-all)

;; support for bookmarks
(require 'breadcrumb)
(global-set-key (kbd "C-c m") 'bc-set)
(global-set-key (kbd "M-SPC") 'bc-previous)
(global-set-key (kbd "M-S-SPC") 'bc-next)
(setq bc-bookmark-limit 1000)
(setq bc-bookmark-file (expand-file-name "~/.emacs.d/cache/breadcrumb"))
;; normal bookmarks
(setq bookmark-default-file "~/.emacs.d/cache/bookmarks")

;; show #colors in matching color
(require 'rainbow-mode)

;; undo-tree like in vim
(require 'undo-tree)
(global-undo-tree-mode)

;; undo highlighting
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; safety
(setq make-backup-files nil)
(defvar autosave-dir (expand-file-name "~/.emacs.d/cache/autosave-dir/"))
(setq auto-save-list-file-prefix "~/.emacs-saves/cache/auto-save-list/.saves-")
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)

;; save location inside buffer
(require 'saveplace)
(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq-default save-place t)

;; optical stuff
(blink-cursor-mode -1)
(setq inhibit-splash-screen t)
;; shows current selected region
(setq-default transient-mark-mode t)
(global-font-lock-mode t)
(set-scroll-bar-mode 'right)
(setq frame-title-format "%b - emacs")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode '(0 . 1))

;; text stuff
(setq default-major-mode 'org-mode)
(prefer-coding-system 'utf-8)
(setq undo-limit 1000000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)
;; don't hard-wrap text, but use nice virtual wrapping
(setq-default fill-column 80)
(global-visual-line-mode 1)
(require 'adaptive-wrap-prefix)
(global-adaptive-wrap-prefix-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; parentheses are connected and their content highlighted
(show-paren-mode 1)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(require 'highlight-parentheses)
(defun turn-on-highlight-parentheses () (highlight-parentheses-mode 1))
(add-hook 'emacs-lisp-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'lisp-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'java-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'python-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'c-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'enh-ruby-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'haskell-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'text-mode-hook 'turn-on-highlight-parentheses)

;; fix mod4 bug
(define-key special-event-map (kbd "<key-17>") 'ignore)
(define-key special-event-map (kbd "<M-key-17>") 'ignore) 

;; key bindings
(mouse-wheel-mode t)   
(global-set-key "\C-cg" 'goto-line)
(global-set-key (kbd "C-c SPC") 'comment-dwim)
(global-set-key (kbd "C-c C-SPC") 'comment-dwim)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-cn" 'next-error)  
(global-set-key "\C-cp" 'previous-error) 
(global-set-key "\M-t"  'find-tag)
(global-set-key "\C-ci" 'indent-region)
;; because we navigate via cursor keys, we can put something more useful on the
;; default navigational keys
(global-set-key "\C-p" 'undo-tree-undo)
(global-set-key "\M-p" 'undo-tree-redo)
(global-set-key "\C-n" 'other-window)
(global-set-key "\M-n" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)
(global-set-key "\M-f" 'forward-sentence)
(global-set-key "\M-b" 'backward-sentence)

;; better search/replace
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(global-set-key "\C-cr" 'vr/query-replace)

;; copy end of line, like C-k
(defun copy-line ()
  (interactive)
  (set 'this-command 'copy-to-kill)
  (save-excursion
    (set-mark (point))
    (if (= (point) (line-end-position))
        (forward-line)
      (goto-char (line-end-position)))
    (if (eq last-command 'copy-to-kill)
        (append-next-kill))
    (kill-ring-save (mark) (point))))
(global-set-key "\M-k" 'copy-line)

;; move to beginning of text on line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line. If point was
already at that position, move point to beginning of line.

If visual-line-mode is on, then also jump to beginning of real line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point))
        (vispos (point)))
        
        (beginning-of-visual-line)
        (setq vispos (point))
        (beginning-of-line-text)

        (if (and (> vispos (point))
                 (not (= oldpos vispos)))
            (goto-char vispos)
          (when (= oldpos (point))
             (beginning-of-line)))))
(global-set-key "\C-a" 'smart-beginning-of-line)

(defun smart-end-of-line ()
  "Move point to end of visual line or, if already there, to end of logical line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
        
        (end-of-visual-line)
        (when (= oldpos (point))
          (end-of-line))))
(global-set-key "\C-e" 'smart-end-of-line)

(setq org-special-ctrl-a/e t)

;; calendar
(setq calendar-week-start-day 1) ; monday
(setq european-calendar-style 't) ; sanity

;; enable useful command
(put 'narrow-to-region 'disabled nil)

;; associate non-standardish interpreters with modes
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))
(add-to-list 'interpreter-mode-alist '("ruby18" .  enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby19" .  enh-ruby-mode))

;; save minibuffer history
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/cache/history")
(setq savehist-additional-variables '(search-ring 
                                       regexp-search-ring 
                                       kill-ring 
                                       compile-command))

;; number windows, i.e. M-1 .. M-0 to jump to window
(require 'window-numbering)
(window-numbering-mode 1)

;; snippets
(setq yas-snippet-dirs "~/.emacs.d/snippets")
(require 'yasnippet)
(define-key yas-minor-mode-map (kbd "C-t") 'yas-next-field-or-maybe-expand)
(define-key yas-minor-mode-map (kbd "M-t") 'yas-prev-field)
;; turn on yasnippet only in some modes
(add-hook 'prog-mode-hook '(lambda () (yas-minor-mode)))
(add-hook 'markdown-mode-hook '(lambda () (yas-minor-mode)))

; auto-yasnippet
(require 'auto-yasnippet)
(global-set-key (kbd "C-c ~")   'aya-create)
(global-set-key (kbd "C-c C-~") 'aya-expand)

;; text completion
;; ;; auto completion
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (add-to-list 'ac-modes 'text-mode)
;; (add-to-list 'ac-modes 'markdown-mode)
;; (add-to-list 'ac-modes 'org-mode)
;; (setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")
;; (setq ac-use-menu-map t)
;; (setq ac-auto-show-menu nil) 
;; (setq ac-ignore-case nil) 
;; (ac-config-default)
;; ;; disabling Yasnippet completion
;; (defun epy-snips-from-table (table)
;;   (with-no-warnings
;;     (let ((hashtab (ac-yasnippet-table-hash table))
;;           (parent (ac-yasnippet-table-parent table))
;;           candidates)
;;       (maphash (lambda (key value)
;;                  (push key candidates))
;;                hashtab)
;;       (identity candidates)
;;       )))
;; (defun epy-get-all-snips ()
;;   (let (candidates)
;;     (maphash
;;      (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas--tables)
;;     (apply 'append candidates))
;;   )
;; (setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))

;; ido and smex (ido for M-x)
;; ido
(require 'ido)
(require 'ido-ubiquitous)
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq ido-everywhere t)
(setq ido-use-filename-at-point nil)
(global-set-key "\C-x\M-f" 'find-file-at-point)
(setq ido-use-url-at-point nil)
(setq ido-use-virtual-buffers t)
(setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
(setq ido-ignore-buffers
      '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
        "^\*compilation" "^\*GTAGS" "^session\.*" "^\*ECB" "^\*"))
(setq ido-case-fold t) ; case insensitive
(setq ido-enable-last-directory-history t)
(setq ido-max-work-directory-list 30)
(setq ido-max-work-file-list 100)
(setq ido-create-new-buffer 'always)
(setq ido-max-directory-size 1000000) ; load bigger dirs, too
(setq confirm-nonexistent-file-or-buffer nil)
(ido-mode 1)
;; smex
(require 'smex)
(smex-initialize)
(global-set-key "\M-x" 'smex)
(global-set-key "\M-X" 'smex-major-mode-commands)

;; recent files
(require 'recentf)
(setq recentf-max-saved-items 1000)
(setq recentf-save-file "~/.emacs.d/cache/recentf")
(setq recentf-exclude (append recentf-exclude 
  '("\.emacs\.d/cache" 
    "\.emacs\.d/elpa")))
(recentf-mode 1)
;; file completion
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key "\C-x\C-r" 'recentf-ido-find-file)

;; yet another savior
(defun jesus ()
  "Because Jesus saves."
  (interactive)
  (save-buffer))

;; search wort at point, like vim
(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                            (progn (skip-syntax-backward "w_") (point))
                            (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
        (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)
(global-set-key (kbd "C-c *") 'my-isearch-word-at-point)

;; indentation
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
;; automatically turn on indenting
(electric-indent-mode 1)
;; also when yanked
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key "\C-y" 'yank-and-indent)

;; undo hardwrapped markdown
(defun unfill-region (begin end)
  "Remove all line breaks in a region but leave paragraphs, 
  indented text (quotes, code) and lists intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\\>-\n]\\)" "\\1 \\2" nil begin end))
(global-set-key "\M-Q" 'unfill-region)

;; insert new line *after* the current one
(defun next-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'next-newline-and-indent)

;; deleting
;; delete spaces when killing a line
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line. 
  Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
    (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; more useful kill-ring
(setq kill-ring-max 200)
(require 'kill-ring-search)
(global-set-key "\M-\C-y" 'kill-ring-search)
(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1))
(global-set-key "\M-Y" 'yank-pop-reverse)

;; c style (1TBS, but guess offsets for other files)
(setq c-default-style "k&r" c-basic-offset tab-width)
(require 'guess-offset)
(global-set-key (kbd "M-RET") 'c-indent-new-comment-line)

;; use automatic file headers
;; #TODO recognize name automagically
;; #TODO add end-of-buffer
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)
(define-auto-insert "\\.sh$"  "sh")
(define-auto-insert "\\.py$"  "python")
(define-auto-insert "\\.hs$"  "haskell")
(define-auto-insert "\\.pl$"  "perl")
(define-auto-insert "\\.rb$"  "ruby")
(define-auto-insert "\\.c$"   "c")
(define-auto-insert "\\.cpp$" "cpp")

;; spell checker
(require 'wcheck-mode)
(setq ispell-really-hunspell t)
(setq  wcheck-timer-idle .2)
(define-key global-map "\C-cs" 'wcheck-actions)
(setq-default
 wcheck-language "English"
 wcheck-language-data '(("English"
                         (program . "/usr/bin/enchant")
                         (args . ("-l" "-d" "en_US"))
                         (action-program . "/usr/bin/enchant")
                         (action-args "-a" "-d" "en_US")
                         (action-parser . enchant-suggestions-menu))
                        ("German"
                         (program . "/usr/bin/enchant")
                         (args . ("-l" "-d" "de"))
                         (action-program . "/usr/bin/enchant")
                         (action-args "-a" "-d" "de")
                         (action-parser . enchant-suggestions-menu))
                        ("French"
                         (program . "/usr/bin/enchant")
                         (args . ("-l" "-d" "fr"))
                         (action-program . "/usr/bin/enchant")
                         (action-args "-a" "-d" "fr")
                         (action-parser . enchant-suggestions-menu))
                        ))
;; add to dictionary functionality
(defun enchant-suggestions-menu (marked-text)
  (cons (cons "[Add]" 'enchant-add-to-dictionary)
                  (wcheck-parser-ispell-suggestions)))

(defvar enchant-dictionaries-dir "~/.config/enchant")

(defun enchant-add-to-dictionary (marked-text)
  (let* ((word (aref marked-text 0))
         (language (aref marked-text 4))
         (file (let ((code (nth 1 (member "-d" (wcheck-query-language-data
                                                language 'action-args)))))
                 (when (stringp code)
                   (concat (file-name-as-directory enchant-dictionaries-dir)
                           code ".dic")))))

    (when (and file (file-writable-p file))
      (with-temp-buffer
        (insert word) (newline)
        (append-to-file (point-min) (point-max) file)
        (message "Added word \"%s\" to the %s dictionary"
                 word language)))))


;; enable spell-check in certain modes
(defun turn-on-spell-check () 
  (wcheck-mode 1))
(add-hook 'text-mode-hook 'turn-on-spell-check)
(add-hook 'markdown-mode-hook 'turn-on-spell-check)
(add-hook 'org-mode-hook 'turn-on-spell-check)

;; disable version control in emacs
(require 'vc)
(setq vc-handled-backends ())

;; markdown
(require 'markdown-mode)
(setq markdown-command "kramdown")
(add-to-list 'auto-mode-alist '("\\.pdc$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\bREADME$"   . markdown-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(defun no-electric-indent-yaml ()
  (electric-indent-mode -1)
  (define-key yaml-mode-map [(return)] 'newline-and-indent))
(add-hook 'yaml-mode-hook 'no-electric-indent-yaml)

;; user data
(setq user-mail-address "mail@muflax.com")
(setq user-full-name "muflax")

;; org-mode (use private version)
;; #FIXME (tab) for org-cycle is disabled directly in the library; this should
;; probably be some unset here.
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; loaded so that we can diminish it later
(require 'org-indent)
;; proper indentation / folding
(setq org-startup-indented nil)
(setq org-hide-leading-stars t)
(setq org-indent-indentation-per-level 2)
(setq org-startup-folded 'content)
(setq org-blank-before-new-entry '(
  (heading . nil)
  (plain-list-item . auto)))
(defun no-electric-indent-org ()
  (electric-indent-mode -1)
  (define-key org-mode-map [(return)] 'newline-and-indent))
(add-hook 'org-mode-hook 'no-electric-indent-org)
;; tag column
(setq org-tags-column -70)
;; dependencies
(setq org-enforce-todo-dependencies t)
;; make clock history persistent
(setq org-clock-persist 'history)
(setq org-clock-persist-file "~/.emacs.d/cache/org-clock-save.el")
(org-clock-persistence-insinuate)
;; spoiler files
(defadvice org-todo-list (before org-todo-list-reload activate compile)
  "Scan for org files whenever todo list is loaded."
  ; 'find' is faster and has better control than lisp
  (setq org-agenda-files (mapcar 'abbreviate-file-name (split-string
    (shell-command-to-string "find ~/spoiler -type f -name \"*.org\" | sort")
      "\n"))))
;; todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "WAITING(w)" "DONE(d)")))
;; priorities
(setq org-default-priority 67) ;C
;; code block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh         . t)
   (ruby       . t)
   (python     . t)
   (haskell    . t)))
(add-to-list 'org-src-lang-modes '("ruby" . enh-ruby))
(add-to-list 'org-src-lang-modes '("r"    . enh-ruby))
(add-to-list 'org-src-lang-modes '("h"    . haskell))
(add-to-list 'org-src-lang-modes '("s"    . sh))
(add-to-list 'org-src-lang-modes '("p"    . python))
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
;; keybindings
(define-key global-map "\C-ct" 'org-todo-list)
(org-defkey org-mode-map "\C-c\C-t" (lambda () (interactive) (org-todo "TODO")))
(org-defkey org-mode-map "\C-c\C-w" (lambda () (interactive) (org-todo "WAITING")))
(org-defkey org-mode-map "\C-c\C-d" (lambda () (interactive) (org-todo "DONE")))
;; shortcut for C-u C-c C-l
(defun org-insert-file-link () (interactive) (org-insert-link '(4)))
(define-key global-map "\C-cf" 'org-insert-file-link)
(define-key global-map "\C-cl" 'org-store-link)
;; go to last position before C-c C-o
(define-key global-map "\C-co" 'org-mark-ring-goto)
;; some templates
(setcdr (assoc "c" org-structure-template-alist)
        '("#+BEGIN_COMMENT\n?\n#+END_COMMENT"))
(add-to-list 'org-structure-template-alist
             '("r"
               "#+BEGIN_SRC ruby\n?\n#+END_SRC"
               "<src lang=\"ruby\">\n\n</src>"))

;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)

;; new python mode
(require 'python)
(defun no-electric-indent-python ()
  ;; TODO should be a generic macro or something
  (electric-indent-mode -1)
  (define-key python-mode-map [(return)] 'newline-and-indent))
(add-hook 'python-mode-hook 'no-electric-indent-python)

;; haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; ruby mode
;; enhanced ruby mode
(setq enh-ruby-program "ruby")
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
;; better colors for warnings
(defface erm-syn-warnline
  '((t (:underline "orange")))
  "Face used for marking warning lines."
  :group 'enh-ruby)
(defface erm-syn-errline
  '((t (:underline "red")))
  "Face used for marking error lines."
  :group 'enh-ruby) 
;; misc stuff
(require 'yari)        ; ri documentation tool
(require 'ruby-block)  ; show what block an end belongs to
(require 'inf-ruby)    ; run ruby in emacs buffer
(require 'robe)        ; better code navigation and inf-ruby extensions
(require 'rinari)      ; rails
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
(setq ruby-indent-level tab-width)
;; Rake files are Ruby, too
(add-to-list 'auto-mode-alist '("\\.rake$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"   . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
;; erb
(require 'rhtml-mode)
(add-to-list 'auto-mode-alist '("\\.erb$"     . rhtml-mode))
;; pry
(require 'pry)
;; optional suggestions
(global-set-key [S-f9] 'pry-intercept)
(global-set-key [f9]   'pry-intercept-rerun)


;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; highlight current symbol at point in buffer (like Eclipse)
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode 0) ; don't activate by default

;; edit symbol in multiple places simultaneously
(require 'iedit)
(global-set-key "\C-ce" 'iedit-mode)

;; align
(require 'align)
;; definitions for ruby code
;; fixes the most egregious mistake in detecting regions (hashes), but should be properly generalized at some point
(setq align-region-separate "\\(^\\s-*[{}]?\\s-*$\\)\\|\\(=\\s-*[][{}()]\\s-*$\\)")
(defconst align-ruby-modes '(enh-ruby-mode)
  "align-perl-modes is a variable defined in `align.el'.")
(defconst ruby-align-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes . '(enh-ruby-mode))
     (repeat . t))
    (ruby-string-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
     (modes . '(enh-ruby-mode))
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
     (modes . '(enh-ruby-mode))))
  "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")
(add-to-list 'align-perl-modes 'enh-ruby-mode)
(add-to-list 'align-dq-string-modes 'enh-ruby-mode)
(add-to-list 'align-sq-string-modes 'enh-ruby-mode)
(add-to-list 'align-open-comment-modes 'enh-ruby-mode)
(dolist (it ruby-align-rules-list)
  (add-to-list 'align-rules-list it))
;; align current region
(global-set-key "\C-c=" 'align-current)

;; diff- mode (better colors)
(require 'diff-mode-)

;; "*" from vim
;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
        (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(global-set-key (kbd "C-*") 'isearch-forward-at-point)

;; wrap search
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; make mouse more usable
(setq make-pointer-invisible t)
(setq mouse-yank-at-point t)

;; semantic (code parser)
(require 'semantic)
(setq semanticdb-default-save-directory "~/.emacs.d/cache/semanticdb")
(semantic-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-completions-mode 1)
;; (global-set-key "\C-cf" 'semantic-ia-show-summary)

;; ecb (code browser)
(require 'ecb-autoloads)
;; fix for emacs 24
(unless (boundp 'stack-trace-on-error)
  (defvar stack-trace-on-error nil)) 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-safe-themes (quote ("d1a8e9ed9fb1509571f5720fa1c114f11449c62b0b300d10ad430c2827d768f5" "55886d3d1eecb8c054eb779fd5b7f41dddaa81dc5d62395faa1cdc49d2b511e4" "636dcdc34ef0f8491da6ef3804970f53ba3dfddc7bc7fbe320f109f810197498" "b2919f14bf56b73f75c723364b2e84b20575a659444733fdcc701ab0346724f4" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" default)))
 '(ecb-fix-window-size (quote auto))
 '(ecb-layout-name "left15")
 '(ecb-layout-window-sizes (quote (("left14" (ecb-speedbar-buffer-name 33 . 45) (ecb-history-buffer-name 33 . 23)) ("left15" (ecb-speedbar-buffer-name 33 . 34) (ecb-methods-buffer-name 33 . 34)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-process-non-semantic-files t)
 '(ecb-tip-of-the-day nil)
 '(ecb-toggle-layout-sequence (quote ("left14" "left15")))
 '(ecb-use-speedbar-instead-native-tree-buffer (quote dir))
 '(ecb-window-width 33)
 '(fci-rule-color "#f6f0e1")
 '(linum-format " %7i ")
 '(safe-local-variable-values (quote ((encoding . utf-8) (eval set-input-method (quote muflax-latin)))))
 '(vc-annotate-background "#f6f0e1")
 '(vc-annotate-color-map (quote ((20 . "#e43838") (40 . "#f71010") (60 . "#f8ffc5") (80 . "#ab9c3a") (100 . "#ef8300") (120 . "#a0682d") (140 . "#1c9e28") (160 . "#3cb368") (180 . "#028902") (200 . "#008b45") (220 . "#077707") (240 . "#409e9f") (260 . "#528d8d") (280 . "#1fb3b3") (300 . "#2c53ca") (320 . "#0000ff") (340 . "#0505cc") (360 . "#a020f0"))))
 '(vc-annotate-very-old-color "#a020f0"))
;; keys
(global-set-key "\C-c;" 'ecb-minor-mode)
;; speedbar
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)
;; get some parsing for ruby
(require 'imenu)
(setq imenu-auto-rescan t)

;; if no region is active, act on current line
(require 'whole-line-or-region)
(setq whole-line-or-region-extensions-alist 
  '((comment-dwim whole-line-or-region-comment-dwim-2 nil)
    (copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
    (kill-region whole-line-or-region-kill-region nil)
    (kill-ring-save whole-line-or-region-kill-ring-save nil)
    (yank whole-line-or-region-yank nil)
  ))
(whole-line-or-region-mode 1)

;; better rectangle functionality
(require 'phi-rectangle)
(phi-rectangle-mode)

;; tramp (remote files)
(setq tramp-default-method "ssh")
(require 'tramp)
(setq tramp-persistency-file-name "~/.emacs.d/cache/tramp")

;; unset unwanted keys
(when (eq window-system 'x)
  (if (eq (key-binding "\C-x\C-z") 'suspend-frame)
    (global-unset-key "\C-x\C-z"))
  (if (eq (key-binding "\C-z") 'suspend-frame)
    (global-unset-key "\C-z")))
(if (eq (key-binding [(insert)]) 'overwrite-mode)
  (global-unset-key [(insert)]))
(if (eq (key-binding [(insertchar)]) 'overwrite-mode)
  (global-unset-key [(insertchar)]))

;; shell stuff
(setq sh-basic-offset tab-width)

;; nxml stuff
(setq nxml-child-indent tab-width)

;; highlight current line
(defface hl-line '((t (:background "Gray")))
         "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)

;; nyan nyan nyan
(require 'nyan-mode)
(nyan-mode t)
(setq nyan-bar-length 15)

;; input methods
(load "custom-input-methods.el" nil t t)

;; ibus (works exactly like normal ibus, but has better integration)
(require 'ibus) 
(add-hook 'after-init-hook 'ibus-mode-on)
(global-set-key (kbd "<kanji>") 'ibus-toggle)

;; analog to delete-file
(defun delete-current-file ()
  "Delete the file associated with the current buffer."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (y-or-n-p (concat "Delete file: " currentFile))
      (kill-buffer (current-buffer))
      (delete-file currentFile)
      (message (concat "Deleted file: " currentFile))
      )))

;; semi-port of surround.vim
(require 'surround)
(global-set-key "\C-cd" 'surround-delete)
(global-set-key "\C-cD" 'surround-delete-within)
(global-set-key "\C-c\M-d" 'surround-change)

;; move lines like in org-mode
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>")   'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; move buffers
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")  'buf-move-down)
(global-set-key (kbd "<C-S-left>")  'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; undo window changes
(require 'winner)
(winner-mode 1)

;; ace-jump (hint-style navigation)
(require 'ace-jump-mode)
(global-set-key (kbd "C-c j")  'ace-jump-mode)

;; cookies
(setq url-cookie-file "~/.emacs.d/cache/url/cookies")

;; lua
(require 'lua-mode)
(setq lua-indent-level 2)

;; (s)css
(setq scss-compile-at-save nil)
(setq css-indent-level 2)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "<C-prior>")  'er/expand-region)
(global-set-key (kbd "<C-next>") 'er/contract-region)

;; better handling than M-| / M-!
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun generalized-shell-command (command arg)
  "Unifies `shell-command' and `shell-command-on-region'. 
You have: 
- (no arg) run command and place output
- (C-u)    ... don't chomp output
- (region) replace region with output from command
- (C-u region) ... and print to minibuffer" ;; TODO: make this also chomp
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
(global-set-key (kbd "C-|") 'generalized-shell-command)
(global-set-key (kbd "C-\\") 'generalized-shell-command) ;; terminal bug

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; scratchpad buffers
(require 'scratch)
;; don't want to remember which key I used
(global-set-key (kbd "C-c b")   'scratch)
(global-set-key (kbd "C-c C-b") 'scratch) 
;; don't start in lisp
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; sync lines between buffers
(defun sync-lines ()
  (interactive)
  (goto-line (line-number-at-pos) (window-buffer (next-window)))
  (goto-line (line-number-at-pos) (window-buffer (next-window))))
(global-set-key (kbd "C-c C-l") 'sync-lines)

;; increment / decrement thing at point
(require 'increment)
(global-set-key (kbd "C-c C-+") 'increment-integer-at-point)
(global-set-key (kbd "C-c C--") 'decrement-integer-at-point)

;; handle camelcase better
(global-subword-mode 1)

;; mark stuff like FIXME
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
;; misbehaving modes
(add-hook 'enh-ruby-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook 'fic-mode)

;; perspectives / workspaces (has to be loaded late)
;; (require 'persp-mode)
;; (setq persp-save-dir (expand-file-name "~/.emacs.d/cache/persp-confs"))
;; (setq persp-set-last-persp-for-new-frames nil)
;; (setq persp-auto-save-num-of-backups 10)
;; (persp-mode t)

;; clean up modeline and hide standard minor modes
;; should be last so all modes are already loaded
(require 'diminish)
(diminish 'abbrev-mode)
;; (diminish 'auto-complete-mode "AC")
(diminish 'auto-fill-function "AF")
(diminish 'auto-revert-mode)
(diminish 'fic-mode)
(diminish 'global-visual-line-mode)
(diminish 'highlight-parentheses-mode)
(diminish 'ibus-mode)
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(diminish 'volatile-highlights-mode)
(diminish 'whole-line-or-region-mode)
(diminish 'yas-minor-mode)
(diminish 'ruby-block-mode)
