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

(defun load-correct-theme ()
  "Loads appropriate theme."
  (interactive)
  (if use-bright-theme (load-theme bright-theme t)
    (load-theme dark-theme t)))

(load-correct-theme)

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

;; highlight current line (face necessary so the theme can overwrite it)
(defface hl-line '((t (:background nil)))
  "Face to use for `hl-line-face'." :group 'hl-line)

(setup "hl-line"
  (setq hl-line-face 'hl-line)
  (global-hl-line-mode t))

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
(setup "smooth-scrolling"
  (setq smooth-scroll-margin 5)
  (setq scroll-margin 0)
  (setq scroll-conservatively 10000)
  ;; necessary or scrolling is really slow
  (setq-default bidi-display-reordering  nil)
  (setq auto-window-vscroll nil))

;; try to keep windows within a max margin
(setup "automargin"
  (setq automargin-target-width 120)
  (automargin-mode))

;; undo highlighting
(setup "volatile-highlights"
  (volatile-highlights-mode t))

;; show #colors in matching color
(setup-lazy '(rainbow-mode) "rainbow-mode")

;; blinking cursor
(blink-cursor-mode -1)
;; (setup "heartbeat-cursor"
;;   (heartbeat-cursor-mode))
;; TODO should be a bar, as soon as multiple-coursors figures out a way to draw that
;; (setq-default cursor-type '(bar . 2))
(setq-default cursor-type 'box)

;; nyan nyan nyan
(setup "nyan-mode"
  (nyan-mode t)
  (setq nyan-bar-length 15))

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; avoid splitting horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 90)

;; don't warn about impossible undo
(setup "warnings"
  (setq warning-suppress-types (append '(undo discard-info) warning-suppress-types)))

;; minimap (badly broken, but One Day(tm), man...
;; non-annoying minimap that lacks a bunch of cool features
;; (require 'minimap)

;; minimap that flickers
;; (require 'sublimity)
;; (require 'sublimity)
;; (require 'sublimity-map)
;; (sublimity-map-set-delay 10)
;; (sublimity-mode 1)

;; clean up modeline and hide standard minor modes
(setup "diminish"
  (setup-after "auto-complete"         (diminish 'auto-complete-mode))
  (setup-after "autorevert"            (diminish 'auto-revert-mode))
  (setup-after "eldoc"                 (diminish 'eldoc-mode))
  (setup-after "fic-mode"              (diminish 'fic-mode))
  (setup-after "guide-key"             (diminish 'guide-key-mode))
  (setup-after "haskell-doc"           (diminish 'haskell-doc-mode))
  (setup-after "haskell-indentation"   (diminish 'haskell-indentation-mode))
  (setup-after "hideshow"              (diminish 'hs-minor-mode))
  (setup-after "highlight-parentheses" (diminish 'highlight-parentheses-mode))
  (setup-after "ruby-block"            (diminish 'ruby-block-mode))
  (setup-after "simple"                (diminish 'auto-fill-function "AF"))
  (setup-after "simple"                (diminish 'global-visual-line-mode))
  (setup-after "simple"                (diminish 'visual-line-mode))
  (setup-after "smartparens"           (diminish 'smartparens-mode))
  (setup-after "undo-tree"             (diminish 'undo-tree-mode))
  (setup-after "volatile-highlights"   (diminish 'volatile-highlights-mode))
  (setup-after "whitespace"            (diminish 'global-whitespace-mode "WS"))
  (setup-after "whitespace"            (diminish 'whitespace-mode " »"))
  (setup-after "whole-line-or-region"  (diminish 'whole-line-or-region-mode))
  (setup-after "yasnippet"             (diminish 'yas-minor-mode)))

(provide 'look)
