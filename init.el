;; load path; site-lisp stores manually maintained packages
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))

;; elpa
(require 'package)
(add-to-list 'package-archives '("tromey"    . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
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

(defvar use-bright-theme t
  "Whether to use the bright or dark theme")

(defvar bright-theme 'leuven
  "Bright theme to use")
(defvar dark-theme 'twilight-anti-bright
  "Bright theme to use")

(load-theme bright-theme t)
(load-theme dark-theme t)
(disable-theme bright-theme)
(disable-theme dark-theme)

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

(add-hook 'after-make-window-system-frame-hooks 'set-color-theme)
(add-hook 'after-make-console-frame-hooks       'set-color-theme)

;; fonts
(defvar small-font  "Anonymous Pro 8")
(defvar normal-font "-Misc-Fixed-*-14-*-iso8859-1")
(defvar big-font    "Monaco 10")
(defvar huge-font   "Monaco 11")
(defvar font-list (list
                   small-font
                   normal-font
                   big-font
                   huge-font))
(defvar current-font big-font)

(defun set-window-font ()
  (set-frame-font current-font))
(add-hook 'after-make-window-system-frame-hooks 'set-window-font)

;; shortcut for the fonts
(defun use-huge-font ()
  "use huge font"
  (interactive)
  (setq current-font huge-font)
  (set-window-font))
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
(global-set-key (kbd "C-c <f4>") 'use-huge-font)

;; auctex
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; scrolling
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-error-top-bottom t)
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
(global-set-key (kbd "C-c d")            'mc/edit-lines)
(global-set-key (kbd "<C-down>")         'mc/mark-next-like-this)
(global-set-key (kbd "<C-up>")           'mc/mark-previous-like-this)
(global-set-key (kbd "<M-C-down>")       'mc/skip-to-next-like-this)
(global-set-key (kbd "<M-C-up>")         'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c C-d")          'mc/mark-all-dwim)
(global-set-key (kbd "C-c >")            'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c <")            'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-<mouse-1>")    'mc/add-cursor-on-click)
(global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click)
(require 'phi-search)
(require 'phi-search-mc)
(global-set-key (kbd "C-c C-s") 'phi-search)
(global-set-key (kbd "C-c C-r") 'phi-search-backward)
(define-key phi-search-default-map (kbd "<C-down>")   'phi-search-mc/mark-next)
(define-key phi-search-default-map (kbd "<C-up>")     'phi-search-mc/mark-previous)
(define-key phi-search-default-map (kbd "C-c C-k")    'phi-search-mc/mark-all)
;; <ret> inserts a newline; C-j exits (a bit more convenient that way)
(eval-after-load "multiple-cursors-core"
  '(progn
     (define-key mc/keymap (kbd "<return>") nil)
     (define-key mc/keymap (kbd "C-j") 'multiple-cursors-mode)))
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
(setq inhibit-splash-screen t)
;; shows current selected region
(setq-default transient-mark-mode t)
(global-font-lock-mode t)
(setq jit-lock-stealth-time 5)
(set-scroll-bar-mode 'right)
(setq frame-title-format "%b")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode '(1 . 10))

;; blinking cursor
(require 'heartbeat-cursor)
(blink-cursor-mode -1)
;; (heartbeat-cursor-mode)
;; (setq-default cursor-type '(bar . 2))
(setq-default cursor-type 'box)

;; text stuff
(setq default-major-mode 'notes-mode)
(prefer-coding-system 'utf-8)
(setq undo-limit 1000000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)
;; don't hard-wrap text, but use nice virtual wrapping
(setq-default fill-column 80)
(global-visual-line-mode 1)
(require 'adaptive-wrap)
(global-adaptive-wrap-prefix-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; nested parentheses are highlighted when inside of them
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

;; generic key bindings
(mouse-wheel-mode t)
(global-set-key (kbd "C-c g")     'goto-line)
(global-set-key (kbd "C-c SPC")   'comment-dwim)
(global-set-key (kbd "C-c C-SPC") 'comment-dwim)
(global-set-key (kbd "C-c c")     'comment-region)
(global-set-key (kbd "C-c u")     'uncomment-region)
(global-set-key (kbd "C-c n")     'next-error)
(global-set-key (kbd "C-c p")     'previous-error)
(global-set-key (kbd "M-t")       'find-tag)
(global-set-key (kbd "C-c i")     'indent-region)
;; analogous to C-S-x
(global-set-key (kbd "C-S-M-x")   'eval-buffer)

;; because we navigate via cursor keys, we can put something more useful on the default navigational keys
(global-set-key (kbd "C-z")    'undo-tree-undo)
(global-set-key (kbd "M-z")    'undo-tree-redo)
(global-set-key (kbd "C-n")    'other-window)
(global-set-key (kbd "C-p")    (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-n")    'undo-tree-undo)
(global-set-key (kbd "M-p")    'undo-tree-redo)
(global-set-key (kbd "C-f")    'forward-word)
(global-set-key (kbd "C-b")    'backward-word)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>")  'end-of-buffer)
;; make C-Backspace "work" in terminal
(global-set-key (kbd "S-<f7>") 'backward-kill-word)

;; allowed key components
(require 'free-keys)
(setq free-keys-keys
      (concat "abcdefghijklmnopqrstuvwxyzßł"
              "ABCDEFGHIJKLMNOPQRSTUVWXYZẞŁ"
              "1234567890"
              "!@#$%^&*()-=[]{};'\\:\"|,./<>?`~_+"
              ))

;; unset unwanted default keys, so they show up in free-keys
(loop for key in `(
                   (,(kbd "C-x C-z")          suspend-frame)
                   (,(kbd "C-z")              suspend-frame)
                   ([(insert)]                overwrite-mode)
                   ([(insertchar)]            overwrite-mode)
                   (,(kbd "C-v")              scroll-up-command)
                   (,(kbd "M-v")              scroll-down-command)
                   (,(kbd "C-]")              abort-recursive-edit)
                   (,(kbd "C-@")              set-mark-command)
                   (,(kbd "<C-down-mouse-1>") mouse-buffer-menu)
                   (,(kbd "<C-down-mouse-2>") facemenu-menu)
                   (,(kbd "<S-down-mouse-1>") mouse-appearance-menu)
                   (,(kbd "C-x C-t")          transpose-lines)
                   (,(kbd "C-x C-q")          read-only-mode)
                   (,(kbd "C-x C-o")          delete-blank-lines)
                   (,(kbd "C-x C-n")          set-goal-column)
                   (,(kbd "C-x TAB")          indent-rigidly)
                   (,(kbd "C-x C-e")          eval-last-sexp)
                   (,(kbd "C-x C-d")          list-directory)
                   (,(kbd "C-x C-@")          pop-global-mark)
                   (,(kbd "C-x SPC")          gud-break)
                   (,(kbd "C-x #")            server-edit)
                   (,(kbd "C-x $")            set-selective-display)
                   (,(kbd "C-x '")            expand-abbrev)
                   (,(kbd "C-x <")            scroll-left)
                   (,(kbd "C-x =")            what-cursor-position)
                   (,(kbd "C-x >")            scroll-right)
                   (,(kbd "C-x [")            backward-page)
                   (,(kbd "C-x ]")            forward-page)
                   (,(kbd "C-x ^")            enlarge-window)
                   (,(kbd "C-x `")            next-error)
                   (,(kbd "C-x l")            count-lines-page)
                   (,(kbd "C-x v")            vc-prefix-map)
                   (,(kbd "C-x {")            shrink-window-horizontally)
                   (,(kbd "C-x }")            enlarge-window-horizontally)
                   (,(kbd "C-M-@")            mark-sexp)
                   (,(kbd "C-M-d")            down-list)
                   (,(kbd "C-M-l")            reposition-window)
                   (,(kbd "C-M-n")            forward-list)
                   (,(kbd "C-M-p")            backward-list)
                   (,(kbd "C-M-t")            transpose-sexps)
                   (,(kbd "C-M-u")            backward-up-list)
                   (,(kbd "C-M-v")            scroll-other-window)
                   (,(kbd "C-M-\\")           indent-region)
                   (,(kbd "M-$")              ispell-word)
                   (,(kbd "M-%")              query-replace)
                   (,(kbd "M-'")              abbrev-prefix-mark)
                   (,(kbd "M-(")              insert-parentheses)
                   (,(kbd "M-)")              move-past-close-and-reindent)
                   (,(kbd "M-*")              pop-tag-mark)
                   (,(kbd "M-.")              find-tag)
                   (,(kbd "M-,")              tags-loop-continue)
                   (,(kbd "M-/")              dabbrev-expand)
                   (,(kbd "M-=")              count-words-region)
                   (,(kbd "M-@")              mark-word)
                   (,(kbd "M-\\")             delete-horizontal-space)
                   (,(kbd "M-`")              tmm-menubar)
                   (,(kbd "M-a")              backward-sentence)
                   (,(kbd "M-e")              forward-sentence)
                   (,(kbd "M-m")              back-to-indentation)
                   (,(kbd "M-o")              facemenu-keymap)
                   (,(kbd "M-r")              move-to-window-line-top-bottom)
                   (,(kbd "M-{")              backward-paragraph)
                   (,(kbd "M-}")              forward-paragraph)
                   (,(kbd "M-~")              not-modified)
                   (,(kbd "C-M-S-v")          scroll-other-window-down)
                   (,(kbd "C-M-%")            query-replace-regexp)
                   (,(kbd "C-M-.")            find-tag-regexp)
                   (,(kbd "C-M-/")            dabbrev-completion)
                   )
      collect (if (eq (key-binding (first key)) (second key))
                  (global-unset-key (first key))))

;; fuck you, you key-stealing whore
(defun unbreak-stupid-map (stupid-map)
  (define-key stupid-map (kbd "C-c") nil))

;; better search/replace
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(global-set-key (kbd "C-c r") 'vr/query-replace)
(defun vr/query-replace-from-beginning ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'vr/query-replace)))
(global-set-key (kbd "C-c R") 'vr/query-replace-from-beginning)

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

;; append to last kill
;; (global-set-key (kbd "C-c C-w") (lambda () (interactive) (append-next-kill) (whole-line-or-region-kill-region)))
;; (global-set-key (kbd "C-c w")   (lambda () (interactive) (append-next-kill) (whole-line-or-region-kill-ring-save)))

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

;; org-mode has similar behavior built-in, so use it instead
(setq org-special-ctrl-a/e t)

;; calendar
(setq calendar-week-start-day 1) ; monday
(setq european-calendar-style 't) ; sanity

;; enable useful command
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

;; associate non-standardish interpreters with modes
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))
(add-to-list 'interpreter-mode-alist '("ruby18"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby19"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby20"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby21"  . enh-ruby-mode))

;; save minibuffer history
(savehist-mode 1)
(setq history-length         1000)
(setq search-ring-max        1000)
(setq regexp-search-ring-max 1000)
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
(define-key yas-minor-mode-map (kbd "C-$") 'yas-expand)
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(yas-global-mode 1)

;; auto-yasnippet
(require 'auto-yasnippet)
(global-set-key (kbd "C-c ~")   'aya-create)
(global-set-key (kbd "C-c C-~") 'aya-expand)

;; auto completion
(require 'auto-complete-config)
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
(define-key ac-completing-map (kbd "C-j") 'ac-complete)

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to its full path."
  (interactive "*fInsert file name: \nP")
  (cond ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert (file-relative-name filename)))))
(global-set-key "\C-c\C-i" 'insert-file-name)

;; ido and smex (ido for M-x)
;; ido
(require 'ido)
(require 'ido-ubiquitous)
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq ido-everywhere t)
(setq ido-use-filename-at-point nil)
(setq ido-use-url-at-point nil)
(global-set-key "\C-x\M-f" 'find-file-at-point)
(setq ido-use-virtual-buffers t)
(setq ido-default-file-method 'selected-window) ; ignore buffers in different frames
(setq ido-default-buffer-method 'selected-window) ; ignore buffers in different frames
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

;; use regexp search and selected region (if any) by default
(defun region-as-string ()
  (buffer-substring (region-beginning)
                    (region-end)))

(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'regexp-search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward-regexp))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'regexp-search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward-regexp))

(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)
(global-set-key (kbd "C-S-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)
;; make backspace more intuitive
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

;; normalize search string so unicode diacritics work normally
(defun isearch-normalize-string ()
  (interactive)
  (let* ((string (ucs-normalize-NFKC-string isearch-string)))
    (setq isearch-string string
          isearch-message (mapconcat 'isearch-text-char-description string ""))
    (isearch-search-and-update)))
(define-key isearch-mode-map (kbd "C-c C-c") 'isearch-normalize-string)
(define-key isearch-mode-map (kbd "C-c C-w") 'isearch-toggle-word)
(define-key isearch-mode-map (kbd "C-c C-r") 'isearch-toggle-regexp)
(define-key isearch-mode-map (kbd "C-c C-i") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-c C-s") 'isearch-toggle-symbol)
(define-key isearch-mode-map (kbd "C-c C-SPC") 'isearch-toggle-lax-whitespace)
(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)

;; search wort at point, like vim
(defun isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun isearch-yank-word-hook ()
  (when (equal this-command 'isearch-word-at-point)
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

(add-hook 'isearch-mode-hook 'isearch-yank-word-hook)
(global-set-key (kbd "C-c *") 'isearch-word-at-point)

;; indentation
(setq-default tab-width 2)
;; don't use tabs normally, except for a few special modes
(setq-default indent-tabs-mode nil)
(defun use-tabs () (setq indent-tabs-mode t))
(add-hook 'notes-mode-hook 'use-tabs)
;; insert literal tab
(global-set-key (kbd "<C-tab>") (lambda () (interactive)
                                  (insert "\t")))

;; replacement for orgtbl by using " | " as separator
(require 'delim-col)
(setq delimit-columns-str-separator " | ")
(setq delimit-columns-separator " +| +")
(setq delimit-columns-format 'separator)
(setq delimit-columns-extra nil)

(defun delimit-columns-current ()
  "Delimit columns of current table."
  (interactive)

  (let ((beg)
        (end)
        (start)
        (regexp "^[^ \t\n]+.* | .*$"))
    (save-excursion
      ;; initialize region on the current line
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))

      (setq start beg)

      ;; find beginning of block
      (goto-char start)
      (while (and (looking-at regexp)
                  (not (bobp)))
        (setq beg (point))
        (forward-line -1))

      ;; find end of block
      (goto-char start)
      (while (and (looking-at regexp)
                  (not (eobp)))
        (end-of-line)
        (setq end (point))
        (forward-line 1))
      )

    ;; call alignment function
    (delimit-columns-region beg end)
    ))
(global-set-key (kbd "C-c t") 'delimit-columns-current)
(global-set-key (kbd "C-c T") 'delimit-columns-region)


;; automatically indent on return, except in a few modes that have similar stuff by default
(electric-indent-mode 1)
;; TODO should be a generic macro or something
(defun no-electric-indent-yaml ()
  (electric-indent-mode -1)
  (define-key yaml-mode-map [(return)] 'newline-and-indent))
(add-hook 'yaml-mode-hook 'no-electric-indent-yaml)
(defun no-electric-indent-python ()
  (electric-indent-mode -1)
  (define-key python-mode-map [(return)] 'newline-and-indent))
(add-hook 'python-mode-hook 'no-electric-indent-python)

;; also indent when yanked
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key "\C-y" 'yank-and-indent)

;; undo hardwrapped regions (mostly markdown)
(defun unfill-region (begin end)
  "Remove all line breaks in a region but leave paragraphs,
  indented text (quotes, code) and lists intact."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\\>-\n]\\)" "\\1 \\2" nil begin end))
(global-set-key "\M-Q" 'unfill-region)

;; insert new line *after* the current one
(defun next-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'next-newline-and-indent)

;; move files to trash instead
(setq delete-by-moving-to-trash t)

;; delete spaces when killing a line
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
  Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; delete all space before point up to beginning of line or non-whitespace char
(require 'hungry-delete)
(global-hungry-delete-mode)
(defun literal-delete-char (&optional arg)
  (interactive "P")
  (delete-char 1))
(defun literal-delete-backward-char (&optional arg)
  (interactive "P")
  (delete-backward-char 1))
(global-set-key (kbd "C-d") 'literal-delete-char)
(global-set-key (kbd "M-d") 'literal-delete-backward-char)

;; more useful kill-ring
(setq kill-ring-max 2000)
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
(define-auto-insert "\\.go$"  "go")

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

;; make it possible to toggle wcheck on/off globally
;; TODO have it disable wcheck in open buffers too
(defvar use-spell-check t)
(defun disable-spell-check ()
  "turns spell-check off globally"
  (interactive)
  (setq use-spell-check nil)
  (dolist (buffer (buffer-list))
    (wcheck-buffer-lang-proc-data-update buffer nil))
  (wcheck-mode 0))
(defun enable-spell-check ()
  "turns spell-check off globally"
  (interactive)
  (setq use-spell-check t)
  (wcheck-mode 1))
(global-set-key (kbd "C-c <f5>")   'disable-spell-check)
(global-set-key (kbd "C-c C-<f5>") 'enable-spell-check)
(global-set-key (kbd "C-c <f6>")   'wcheck-mode)

(defun turn-on-spell-check ()
  (if use-spell-check (wcheck-mode 1)))

;; enable spell-check in certain modes
(add-hook 'markdown-mode-hook 'turn-on-spell-check)
(add-hook 'org-mode-hook      'turn-on-spell-check)

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

;; notes-mode
(require 'notes-mode)
(add-to-list 'auto-mode-alist '("\\.txt$"      . notes-mode))
(add-to-list 'auto-mode-alist '("\\.notes$"    . notes-mode))
(add-to-list 'auto-mode-alist '("\\.script$"   . notes-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; user data
(setq user-mail-address "mail@muflax.com")
(setq user-full-name "muflax")

;; org-mode (use private version)
;; #FIXME (tab) for org-cycle is disabled directly in the library; this should probably be some unset here.
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; loaded so that we can diminish it later
(require 'org-indent)
;; proper indentation / folding
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-indent-indentation-per-level 2)
(setq org-startup-folded 'content)
(setq org-blank-before-new-entry '(
                                   (heading . nil)
                                   (plain-list-item . auto)))
;; tag column
(setq org-tags-column -70)
;; dependencies
(setq org-enforce-todo-dependencies t)
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
(org-defkey org-mode-map "\C-c\C-t" (lambda () (interactive) (org-todo "TODO")))
(org-defkey org-mode-map "\C-c\C-w" (lambda () (interactive) (org-todo "WAITING")))
(org-defkey org-mode-map "\C-c\C-d" (lambda () (interactive) (org-todo "DONE")))
;; shortcut for C-u C-c C-l
(defun org-insert-file-link () (interactive) (org-insert-link '(4)))
;; some templates
(setcdr (assoc "c" org-structure-template-alist)
        '("#+BEGIN_COMMENT\n?\n#+END_COMMENT"))
(add-to-list 'org-structure-template-alist
             '("r"
               "#+BEGIN_SRC ruby\n?\n#+END_SRC"
               "<src lang=\"ruby\">\n\n</src>"))

;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)
;; also revert dired
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; new python mode
(require 'python)
(setq python-indent-offset 2)

;; haskell mode
(require 'haskell-mode)
(require 'haskell-doc)
(require 'haskell-indentation)
(require 'inf-haskell)
(require 'ghc)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
(define-key haskell-mode-map (kbd "C-c ?")   'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-?") 'haskell-process-do-info)

;; ruby mode
;; enhanced ruby mode
(require 'enh-ruby-mode)
(setq enh-ruby-program "~/.rbenv/shims/ruby")
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
;; better colors for warnings
(defface erm-syn-warnline
  '((t (:underline "orange")))
  "Face used for marking warning lines."
  :group 'enh-ruby)
(defface erm-syn-errline
  '((t (:background "pink")))
  "Face used for marking error lines."
  :group 'enh-ruby)
(unbreak-stupid-map enh-ruby-mode-map)
(define-key enh-ruby-mode-map (kbd "C-c C-n") 'enh-ruby-find-error)
(define-key enh-ruby-mode-map (kbd "C-c C-p") 'enh-ruby-beginning-of-defun)
;; misc stuff
(require 'yari)        ; ri documentation tool
(require 'ruby-block)  ; show what block an end belongs to
(require 'inf-ruby)    ; run ruby in emacs buffer
(require 'robe)        ; better code navigation and inf-ruby extensions
(require 'rinari)      ; rails
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
;; better indenting
(setq ruby-block-highlight-toggle t)
(setq ruby-indent-level tab-width)
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-deep-indent-paren nil)
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
(define-key enh-ruby-mode-map (kbd "<S-f9>") 'pry-intercept)
(define-key enh-ruby-mode-map (kbd "<f9>")   'pry-intercept-rerun)
;; auto-completion
(add-hook 'robe-mode-hook 'robe-ac-setup)
;; documentation
(define-key enh-ruby-mode-map (kbd "C-c ?") 'yari)

;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; highlight current symbol at point in buffer (like Eclipse)
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode 0) ; don't activate by default

;; edit symbol in multiple places simultaneously
(require 'iedit)
(global-set-key (kbd "C-c ;") 'iedit-mode)
(global-set-key (kbd "C-c C-;") 'iedit-mode-toggle-on-function)

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
(add-to-list 'align-perl-modes         'enh-ruby-mode)
(add-to-list 'align-dq-string-modes    'enh-ruby-mode)
(add-to-list 'align-sq-string-modes    'enh-ruby-mode)
(add-to-list 'align-open-comment-modes 'enh-ruby-mode)
(dolist (it ruby-align-rules-list)
  (add-to-list 'align-rules-list it))
;; haskell alignments
(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

(defun align-region-or-current ()
  "Align current selected region or implied region if nothing is selected."
  (interactive)
  (if (and mark-active
           (/= (point) (mark)))
      (align (point) (mark))
    (align-current)))
;; align current region
(global-set-key (kbd "C-c =") 'align-region-or-current)

;; repeat regex (teh fuck ain't that the default?!)
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))
(global-set-key (kbd "C-c C-=") 'align-repeat)

;; diff- mode (better colors)
(require 'diff-mode-)

;; a slightly saner diff command
(require 'ediff)
(setq ediff-diff-options "-w")

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

;; custom variables because fuck emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8) (eval set-input-method (quote muflax-latin))))))

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

;; tramp (remote files)
(setq tramp-default-method "ssh")
(require 'tramp)
(setq tramp-persistency-file-name "~/.emacs.d/cache/tramp")

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
(global-set-key (kbd "C-c 1") (lambda () (interactive)
                                (set-input-method nil)))
(global-set-key (kbd "C-c 2") (lambda () (interactive)
                                (set-input-method "muflax-cyrillic")))
(global-set-key (kbd "C-c 3") (lambda () (interactive)
                                (set-input-method "muflax-turkish")))
(global-set-key (kbd "C-c 4") (lambda () (interactive)
                                (set-input-method "muflax-greek")))
(global-set-key (kbd "C-c C-m") 'toggle-input-method)

;; ibus (works exactly like normal ibus, but has better integration)
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)
(global-set-key (kbd "<kanji>")   'ibus-toggle)
(global-set-key (kbd "S-<kanji>") 'ibus-next-input-method)

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
(global-set-key (kbd "C-c j")   'ace-jump-mode)
(global-set-key (kbd "C-c C-g") 'ace-jump-line-mode)

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
(global-set-key (kbd "<C-prior>") 'er/expand-region)
(global-set-key (kbd "<C-next>")  'er/contract-region)

;; make zsh aliases work
(setq shell-command-switch "-lc")
;; tab-completion for shell-command
;; FIXME not working yet, but meh
(require 'shell-command)
(shell-command-completion-mode)

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
(global-set-key (kbd "C-\\") 'generalized-shell-command) ; terminal bug

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; highlight some whitespace
(require 'whitespace)
(setq whitespace-style (quote (face tabs tab-mark)))

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; scratchpad buffers
(require 'scratch)
;; don't want to remember which key I used
(global-set-key (kbd "C-c C-b")   'scratch)
;; don't start in lisp
(setq initial-major-mode 'notes-mode)
(setq initial-scratch-message nil)

;; increment / decrement thing at point
(require 'increment)
(global-set-key (kbd "C-c C-+") 'increment-integer-at-point)
(global-set-key (kbd "C-c C--") 'decrement-integer-at-point)

;; rotate / toggle text
(require 'rotate-text)
(global-set-key (kbd "C-c C-t") 'rotate-text)
(add-to-list 'rotate-text-words   '("true" "false"))
(add-to-list 'rotate-text-words   '("yes" "no"))
(add-to-list 'rotate-text-words   '("t" "nil"))
(add-to-list 'rotate-text-words   '("if" "unless"))
(add-to-list 'rotate-text-words   '("map" "each"))
(add-to-list 'rotate-text-words   '("select" "reject"))
(add-to-list 'rotate-text-symbols '("?" "!"))

;; handle camelcase better
(global-subword-mode 1)

;; mark stuff like FIXME
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
;; misbehaving modes
(add-hook 'enh-ruby-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook 'fic-mode)

;; folding
(require 'hideshow)
(require 'hideshowvis)
(require 'fold-dwim)
(define-key global-map (kbd "C-c C-f") 'fold-dwim-toggle)
(define-key global-map (kbd "C-c f")   'fold-dwim-hide-all)
(define-key global-map (kbd "C-c F") 'fold-dwim-show-all)
(add-hook 'enh-ruby-hook   'hs-minor-mode)
(add-hook 'notes-mode-hook 'hs-minor-mode)

;; fast navigation
(require 'imenu)
(require 'idomenu)
(require 'imenu-anywhere)
(define-key global-map (kbd "C-c [")   'idomenu)
(define-key global-map (kbd "C-c C-[") 'idomenu)
(define-key global-map (kbd "C-c ]")   'imenu-anywhere)
(define-key global-map (kbd "C-c C-]") 'imenu-anywhere)
(set-default 'imenu-auto-rescan t)

(defun imenu-flush-cache ()
  "Flushes imenu cache."
  (interactive)
  (setq imenu--index-alist nil))

;; recentering
(setq recenter-positions '(2 middle))
(add-hook 'imenu-after-jump-hook 'recenter-top-bottom)

;; smart parentheses
(require 'smartparens-config)
(smartparens-global-mode t)
(setq sp-autoescape-string-quote nil)
(setq sp-highlight-pair-overlay nil)
(setq sp-autoinsert-pair nil)
(setq sp-autodelete-pair nil)
;; parenthesis highlighting behavior
(show-paren-mode 1)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
;; keybindings
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
(define-key sp-keymap (kbd "M-f") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-b") 'sp-backward-symbol)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "C-<right>") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
(define-key sp-keymap (kbd "C-c k") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-c C-k") 'sp-rewrap-sexp)
(define-key sp-keymap (kbd "S-<left>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "S-<right>") 'sp-select-next-thing)
(define-key sp-keymap (kbd "C-c |") 'sp-split-sexp)
(define-key sp-keymap (kbd "C-c C-|") 'sp-join-sexp)

;; move to beginning of text on line
(defun sp-kill-to-end-of-sexp ()
  "Kill everything in the sexp without unbalancing it."
  (interactive)
  (save-excursion
    (set-mark (point))
    (sp-end-of-sexp)
    (kill-region (mark) (point))))
;; move to beginning of text on line
(defun sp-kill-to-beginning-of-sexp ()
  "Kill everything in the sexp without unbalancing it."
  (interactive)
  (save-excursion
    (set-mark (point))
    (sp-beginning-of-sexp)
    (kill-region (mark) (point))))
;; move to beginning of text on line
(defun sp-copy-to-end-of-sexp ()
  "Copy everything in the sexp without unbalancing it."
  (interactive)
  (save-excursion
    (set-mark (point))
    (sp-end-of-sexp)
    (kill-ring-save (mark) (point))))
;; move to beginning of text on line
(defun sp-copy-to-beginning-of-sexp ()
  "copy everything in the sexp without unbalancing it."
  (interactive)
  (save-excursion
    (set-mark (point))
    (sp-beginning-of-sexp)
    (kill-ring-save (mark) (point))))
(define-key sp-keymap (kbd "C-c a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-c e") 'sp-end-of-sexp)
(define-key sp-keymap (kbd "C-c C-a") 'sp-kill-to-beginning-of-sexp)
(define-key sp-keymap (kbd "C-c C-e") 'sp-kill-to-end-of-sexp)
(define-key sp-keymap (kbd "C-c M-a") 'sp-copy-to-beginning-of-sexp)
(define-key sp-keymap (kbd "C-c M-e") 'sp-copy-to-end-of-sexp)

;; markdown-mode
(sp-with-modes '(markdown-mode)
  (sp-local-pair "*" "*" :actions '(wrap autoskip))
  (sp-local-pair "_" "_" :actions '(wrap autoskip)))

;; notes-mode
(sp-with-modes '(notes-mode)
  (sp-local-pair "*" "*" :actions '(wrap autoskip))
  (sp-local-pair "/" "/" :actions '(wrap autoskip))
  (sp-local-pair "<" ">" :actions '(wrap autoskip))
  (sp-local-pair "[" "]")
  (sp-local-pair "'" "'" :actions '(wrap autoskip))
  (sp-local-pair "`" "`" :actions '(wrap autoskip)))

;; overwrite |pipe| handling in ruby
(sp-with-modes '(enh-ruby-mode ruby-mode)
  (sp-local-pair "|" "|" :pre-handlers nil))

;; perspectives / workspaces (has to be loaded late)
(require 'persp-mode)
(setq persp-save-dir (expand-file-name "~/.emacs.d/cache/persp-confs"))
(setq persp-set-last-persp-for-new-frames nil)
(setq persp-auto-save-num-of-backups 10)
;; (persp-mode t)

;; load raw text in a basic mode (for performance reasons)
(add-to-list 'auto-mode-alist '("\\.log$" . fundamental-mode))

;; load minimap mode
;; (require 'minimap)

;; normalize unicode in buffer
(defun normalize-unicode-in-buffer ()
  "Normalize Unicode in buffer to NFKC form."
  (interactive)
  (save-excursion
    (ucs-normalize-NFKC-region (point-min) (point-max))))

;; normalize buffer before saving
(add-hook 'before-save-hook 'normalize-unicode-in-buffer)

;; clean up buffers every once in a while
(require 'midnight)
(midnight-delay-set 'midnight-delay "0:00am")

;; dired
(require 'dired)
(require 'wdired)
(require 'dired-x)
(require 'dired-details)
(require 'dired-details+)
;; move files between split panes
(setq dired-dwim-target t)
;; reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
        (eval `(defadvice ,it (after revert-buffer activate)
                 (revert-buffer))))
(global-set-key (kbd "C-c C-j") 'dired-jump)
(define-key dired-mode-map (kbd "C-c C-c")  'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "<insert>") 'dired-mark)
;; C-a goes to filename
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))
(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
(define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
;; M-up goes to first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))
(define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)
;; M-down goes to last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))
(define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)
(define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; don't use shift to mark things; smartparens overwrites this anyway, but be explicit about it
(setq shift-select-mode nil)

;; transparently open compressed files
(auto-compression-mode t)

;; UTF-8 please
(setq locale-coding-system   'utf-8) ; pretty
(set-terminal-coding-system  'utf-8) ; pretty
(set-keyboard-coding-system  'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system        'utf-8) ; with sugar on top

;; normally smartparens wraps selected text, but if input is not a pair, just overwrite the text
(delete-selection-mode 1)

;; allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; a bit more relaxed garbage collection
(setq gc-cons-threshold 20000000)

;; make sure we always know what's happening when eval-ing things
(setq eval-expression-print-level nil)

;; when popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; automatically create directories if necessary
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; avoid splitting horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 90)

;; don't warn about impossible undo
(require 'warnings)
(setq warning-suppress-types (append '(undo discard-info) warning-suppress-types))

;; eldoc, ie function signatures in the minibuffer
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; go-lang
(require 'go-mode)
(require 'go-autocomplete)
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'before-save-hook #'gofmt-before-save)
(setq gofmt-command "goimports")
(unbreak-stupid-map go-mode-map)

;; clean up modeline and hide standard minor modes
;; should be last so all modes are already loaded
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'auto-complete-mode)
(diminish 'auto-fill-function "AF")
(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
(diminish 'fic-mode)
(diminish 'global-visual-line-mode)
(diminish 'global-whitespace-mode " »")
(diminish 'haskell-doc-mode)
(diminish 'haskell-indentation-mode)
(diminish 'highlight-parentheses-mode)
(diminish 'hs-minor-mode)
(diminish 'ibus-mode " I")
(diminish 'ruby-block-mode)
(diminish 'smartparens-mode)
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(diminish 'volatile-highlights-mode)
(diminish 'whitespace-mode "WS")
(diminish 'whole-line-or-region-mode)
(diminish 'yas-minor-mode)
