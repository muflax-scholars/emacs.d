;; load path 
; site-lisp stores foreign packages, emacs.d only my own
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
         (default-directory my-lisp-dir))
    (progn
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; color 
(require 'color-theme)
; only load used color theme
(require 'color-theme-almost-monokai)
;(color-theme-initialize)

; makes sure each frame has its own unique color
(setq color-theme-is-global nil)

; use different color scheme depending on whether we run in X or a terminal
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
(add-hook 'after-init-hook
          (lambda ()
            (run-after-make-frame-hooks (selected-frame))))

(add-hook 'after-make-window-system-frame-hooks 'color-theme-almost-monokai)
;(add-hook 'after-make-console-frame-hooks 'color-theme-emacs-nw)

;; auctex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; icicles
;(require 'icicles)
;(icy-mode 1)

;; smooth scrolling with margin
(require 'smooth-scrolling)
(setq scroll-margin 0)
(setq smooth-scroll-margin 5)
(setq mouse-wheel-progressive-speed nil)

;; support for bookmarks
(require 'breadcrumb)
(global-set-key (kbd "C-c j") 'bc-set)
(global-set-key (kbd "M-SPC") 'bc-previous)
(setq bc-bookmark-limit 50)
(setq bc-bookmark-file (expand-file-name "~/.emacs.d/breadcrumb"))

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
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave-dir/"))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)

;; save location inside buffer
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; optical stuff
(blink-cursor-mode -1)
(setq inhibit-splash-screen t)
; shows current selected region
(setq-default transient-mark-mode t)
(global-font-lock-mode t)
(set-scroll-bar-mode 'right)
(setq frame-title-format "%b - emacs")
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq scroll-preserve-screen-position t)

;; fonts
(defun set-window-fonts ()
  (set-frame-font "Anonymous Pro-9")
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208 '("Kochi Gothic" . "unicode-bmp"))
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0212 '("Kochi Gothic" . "unicode-bmp"))
  ; prevent using the Japanese fonts for European text
  (set-fontset-font (frame-parameter nil 'font)
                    'latin-iso8859-1  '("Anonymous Pro" . "unicode-bmp"))
  (set-fontset-font (frame-parameter nil 'font)
                    'latin-iso8859-15 '("Anonymous Pro" . "unicode-bmp"))
  )
(add-hook 'after-make-window-system-frame-hooks 'set-window-fonts)

;; text stuff
(setq default-major-mode 'org-mode)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(prefer-coding-system 'utf-8)
(setq undo-limit 1000000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)

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
(add-hook 'ruby-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'haskell-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'text-mode-hook 'turn-on-highlight-parentheses)

;; fix mod4 bug
(define-key special-event-map (kbd "<key-17>") 'ignore)
(define-key special-event-map (kbd "<M-key-17>") 'ignore) 

;; key bindings
(mouse-wheel-mode t)   
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-cn" 'next-error)  
(global-set-key "\C-cp" 'previous-error) 
(global-set-key "\C-cr" 'query-replace-regexp)
(global-set-key "\C-t"  'find-tag)
(global-set-key "\C-ci" 'indent-region)
; because we navigate via cursor keys, we can put something more useful on the
; default navigational keys
(global-set-key "\C-p" 'undo-tree-undo)
(global-set-key "\M-p" 'undo-tree-redo)
(global-set-key "\C-n" 'other-window)
(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)
(global-set-key "\M-f" 'forward-sentence)
(global-set-key "\M-b" 'backward-sentence)
; move to beginning of text on line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

  Move point to the first non-whitespace character on this line.
  If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (beginning-of-line-text)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key "\C-a" 'smart-beginning-of-line)

;; calendar
(setq calendar-week-start-day 1) ; monday
(setq european-calendar-style 't) ; sanity

;; enable useful command
(put 'narrow-to-region 'disabled nil)

;; associate non-standardish interpreters with modes
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))
(add-to-list 'interpreter-mode-alist '("ruby18" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby19" . ruby-mode))

;; save minibuffer history
(savehist-mode 1)
(setq savehist-additional-variables '(search-ring 
                                       regexp-search-ring 
                                       kill-ring 
                                       compile-command))

;; ido and smex (ido for M-x)
(require 'ido)
; fuzzy matching
(setq ido-enable-flex-matching t)
(require 'smex)
(smex-initialize)
(global-set-key "\M-x" 'smex)
(global-set-key "\M-X" 'smex-major-mode-commands)

;; number windows, i.e. M-1 .. M-0 to jump to window
(require 'window-numbering)
(window-numbering-mode 1)

;; snippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; text completion
; auto completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(defun ac-text-setup ()
;(setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
;(add-hook 'text-mode-hook 'ac-text-setup)
;(add-hook 'markdown-mode-hook 'ac-text-setup)
;(add-hook 'org-mode-hook 'ac-text-setup)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'markdown-mode)
(add-to-list 'ac-modes 'org-mode)
(setq ac-comphist-file "~/.emacs.d/ac-comphist.dat")
(setq ac-use-menu-map t)
(setq ac-auto-show-menu nil) 
(setq ac-ignore-case nil) 
(ac-config-default)
; disabling Yasnippet completion
(defun epy-snips-from-table (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (identity candidates)
      )))
(defun epy-get-all-snips ()
  (let (candidates)
    (maphash
     (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas/tables)
    (apply 'append candidates))
  )
(setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))

;; just some saviors
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
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
; automatically turn on indenting
(define-key global-map (kbd "RET") 'newline-and-indent)
; also when yanked
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key "\C-y" 'yank-and-indent)

;; insert new line *after* the current one
(defun next-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'next-newline-and-indent)

;; deleting
; delete spaces when killing a line
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line. 
  Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
    (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)
; if no mark is set, act on current line
(defadvice kill-ring-save (around slick-copy activate)
           "When called interactively with no active region, copy a single line instead."
           (if (or (region-active-p) (not (called-interactively-p)))
             ad-do-it
             (kill-new (buffer-substring (line-beginning-position)
                                         (line-beginning-position 2))
                       nil '(yank-line))
             (message "Copied line")))

(defadvice kill-region (around slick-copy activate)
           "When called interactively with no active region, kill a single line instead."
           (if (or (region-active-p) (not (called-interactively-p)))
             ad-do-it
             (kill-new (filter-buffer-substring (line-beginning-position)
                                                (line-beginning-position 2) t)
                       nil '(yank-line))))

(defun yank-line (string)
  "Insert STRING above the current line."
  (beginning-of-line)
  (unless (= (elt string (1- (length string))) ?\n)
    (save-excursion (insert "\n")))
  (insert string))

;; more useful kill-ring
(setq kill-ring-max 200)
(require 'kill-ring-search)
(global-set-key "\M-\C-y" 'kill-ring-search)
(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1))
(global-set-key "\M-Y" 'yank-pop-reverse)

;; c style (1TBS, but guess offsets for other files)
(setq c-default-style "k&r" c-basic-offset 2)
(require 'guess-offset)
(global-set-key (kbd "M-RET") 'c-indent-new-comment-line)

;; mark stuff like FIXME
(require 'fixme-mode)
(fixme-mode 1)

;; use automatic file headers
; #TODO recognize name automagically
; #TODO add end-of-buffer
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
(setq-default ispell-program-name "aspell")
(setq ispell-process-directory (expand-file-name "~/"))
(setq ispell-dictionary "english")
(setq ispell-silently-savep t)
; faster checking
(setq ispell-list-command "list")
; enable flyspell in certain modes
; #FIXME disabled for speed issues
;(defun turn-on-flyspell () 
;(flyspell-mode 1))
;(add-hook 'text-mode-hook 'turn-on-flyspell)
;(add-hook 'markdown-mode-hook 'turn-on-flyspell)
;(add-hook 'org-mode-hook 'turn-on-flyspell)

;; git
(require 'magit)
(require 'gitsum)
(require 'format-spec)
(require 'git-blame)

;; markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.pdc$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; user data
(setq user-mail-address "mail@muflax.com")
(setq user-full-name "muflax")


;; org-mode (use private version)
; #FIXME (tab) for org-cycle is disabled directly in the library; this should
; probably be some unset here.
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
; proper indentation / folding
; loaded so that we can diminish it later
(require 'org-indent)
(setq org-startup-indented t)
(setq org-startup-folded nil)
; dependencies
(setq org-enforce-todo-dependencies t)
; make clock history persistent
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
; spoiler files
(require 'find-lisp)
(if (file-accessible-directory-p "~/spoiler") (progn
                                                (setq org-default-notes-file "~/spoiler/capture.org")
                                                (setq org-agenda-files (find-lisp-find-files "~/spoiler" "\\.org$"))
                                                ;(add-to-list 'auto-mode-alist '("^/home/amon/spoiler/" . org-mode))
                                                ))
; todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
        (sequence "BUG(b)" "|" "FIXED(f)")))
(setq org-log-done 'note)
; priorities
(setq org-default-priority 67) ;C


;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)

;; clean up modeline and hide standard minor modes
; should be last so all modes are already loaded
(require 'diminish)
;(diminish 'auto-complete-mode "AC")
(diminish 'auto-fill-function "AF")
(diminish 'abbrev-mode)
(diminish 'auto-revert-mode)
(diminish 'fixme-mode)
(diminish 'highlight-parentheses-mode)
(diminish 'org-indent-mode)
(diminish 'undo-tree-mode)
(diminish 'volatile-highlights-mode)
(diminish 'yas/minor-mode)

;; new python mode
;(setq load-path (cons "~/.emacs.d/site-lisp/python-mode" load-path))
(require 'python)

;; haskell mode
(load "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; ruby mode
(require 'yari)
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
(setq ruby-indent-level 2)
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'reindent-then-newline-and-indent)))
; Rake files are Ruby, too
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
; rinari
(require 'rinari)

;; align
(require 'align)
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
                (regexp . ",\\(\\s-*\\)[^# \t\n]")
                (repeat . t)
                (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
                (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
                (repeat . t)
                (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-assignment-literal
                (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
                (repeat . t)
                (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list          ;TODO add to rcodetools.el
             '(ruby-xmpfilter-mark
                (regexp . "\\(\\s-*\\)# => [^#\t\n]")
                (repeat . nil)
                (modes  . '(ruby-mode))))
(global-set-key "\C-ca" 'align-current)

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

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'postforward)

;; don't hide mouse
(setq make-pointer-invisible nil)

;; semantic (code parser)
(require 'semantic)
(semantic-mode 1)
(global-set-key "\C-cf" 'semantic-ia-show-summary)

;; ecb (code browser)
(require 'ecb)
(custom-set-variables
 '(ecb-options-version "2.40")
 '(ecb-tree-indent 2)
 '(ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
 '(ecb-tree-buffer-style 'ascii-guides)
 '(ecb-tip-of-the-day nil)
 '(ecb-window-width 20)
 '(ecb-fix-window-size 'auto)
 '(ecb-layout-name "left15")
 '(ecb-process-non-semantic-files t)
)
; get some parsing for ruby
(require 'imenu)
(setq imenu-auto-rescan t)
