;;-----------
;; load path 
;;-----------
;; elisp stores foreign packages, emacs.d only my own
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/elisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; ELPA package manager
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;-------
;; color 
;;-------
(require 'color-theme)
(color-theme-initialize)

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

;;--------
;; auctex
;;--------
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;;---------
;; icicles
;;---------
;(require 'icicles)
;(icy-mode 1)

;;-----------------
;; general options
;;-----------------

;; safety
(setq make-backup-files nil)
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave-dir/"))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)

;; optical stuff
(blink-cursor-mode -1)
(setq inhibit-splash-screen t)
; shows current selected region
(setq-default transient-mark-mode t)
(global-font-lock-mode t)
(set-scroll-bar-mode 'right)
(setq frame-title-format "%b - emacs")
(tool-bar-mode -1)

;; fonts
(defun set-window-fonts ()
    (set-default-font "Anonymous Pro-9")
    (set-fontset-font (frame-parameter nil 'font)
        'japanese-jisx0208 '("Kochi Gothic" . "unicode-bmp"))
    (set-fontset-font (frame-parameter nil 'font)
        'japanese-jisx0212 '("Kochi Gothic" . "unicode-bmp"))
)
(add-hook 'after-make-window-system-frame-hooks 'set-window-fonts)

;; text stuff
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq fill-column 80)
(prefer-coding-system 'utf-8)
(setq undo-limit 1000000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)
(setq default-major-mode 'text-mode)

;; parentheses are connected and their content highlighted
(show-paren-mode 1)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'expression)
(setq show-paren-delay 0.1)

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
; because we navigate via cursor keys, we can put something more useful on the
; default navigational keys
(global-set-key "\C-p" 'undo)
(global-set-key "\C-n" 'other-window)
(global-set-key "\M-n" 'execute-extended-command)
(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)
(global-set-key "\M-f" 'forward-sentence)
(global-set-key "\M-b" 'backward-sentence)

;; random stuff
(setq calendar-week-start-day 1) ; monday
(setq european-calendar-style 't) ; sanity

;; enable useful command
(put 'narrow-to-region 'disabled nil)
