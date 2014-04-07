;; how emacs looks

;; prettier defaults
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
;; shows current selected region
(setq-default transient-mark-mode t)
(global-font-lock-mode t)
(setq jit-lock-stealth-time 5)
(setq frame-title-format "%b")
(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)
(set-fringe-mode '(1 . 10))

;; color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defvar bright-theme 'leuven               "Bright theme to use")
(defvar dark-theme   'twilight-anti-bright "Dark theme to use")

(defvar use-bright-theme t "Whether to use the bright or dark theme")
(if use-bright-theme (load-theme bright-theme t)
  (load-theme dark-theme t))

(defun toggle-bright-theme ()
  "toggles between bright and dark theme"
  (interactive)
  (if use-bright-theme (progn
                         (setq use-bright-theme nil)
                         (disable-theme bright-theme)
                         (load-theme dark-theme t))
    (progn
      (setq use-bright-theme t)
      (disable-theme dark-theme)
      (load-theme bright-theme t))))

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

;; undo highlighting
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; show #colors in matching color
(require 'rainbow-mode)

;; blinking cursor
;; (require 'heartbeat-cursor)
(blink-cursor-mode -1)
;; (heartbeat-cursor-mode)
;; TODO should be a bar, as soon as multiple-coursors figures out a way to draw that
;; (setq-default cursor-type '(bar . 2))
(setq-default cursor-type 'box)

;; highlight current line
(defface hl-line '((t (:background "Gray")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)

;; nyan nyan nyan
(require 'nyan-mode)
(nyan-mode t)
(setq nyan-bar-length 15)

;; load minimap mode
;; (require 'minimap)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; avoid splitting horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 90)

;; don't warn about impossible undo
(require 'warnings)
(setq warning-suppress-types (append '(undo discard-info) warning-suppress-types))

(provide 'look)
