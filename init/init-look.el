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

;; indentation
(setq-default tab-width 2)

;; scroll bar
(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)
(set-fringe-mode '(1 . 10))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

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

(defun run-after-make-frame-hooks-current-frame ()
  (run-after-make-frame-hooks (selected-frame)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)
(add-hook 'after-init-hook 'run-after-make-frame-hooks-current-frame)

;; color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setup "leuven-theme") ; force-load it here so we have all faces set up

(defvar bright-theme 'leuven               "Bright theme to use")
(defvar dark-theme   'twilight-anti-bright "Dark theme to use")

(defvar use-bright-theme t "Whether to use the bright or dark theme")

(defun load-correct-theme ()
  "Loads appropriate theme."
  (interactive)
  (if use-bright-theme (load-theme bright-theme t)
    (load-theme dark-theme t)))

(when (pretty-load?)
  (load-correct-theme))

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

;; highlight current line
(setup "hl-line"
  (global-hl-line-mode t))

;; fonts
(defvar small-font  "Anonymous Pro 8")
(defvar normal-font "Fantasque Sans Mono 10")
(defvar big-font    "Fantasque Sans Mono 11")
(defvar huge-font   "Fantasque Sans Mono 13")
(defvar font-list '(
                    small-font
                    normal-font
                    big-font
                    huge-font))
(defvar current-font normal-font)

(defun set-window-font ()
  (set-frame-font current-font))

(add-hook 'after-make-window-system-frame-hooks 'set-window-font)

;; shortcut functions for the fonts
(defmacro use-font (font)
  `(defun ,(intern (format "use-%s" font)) ()
     ,(format "Use font set in '%s'" font)
     (interactive)
     (setq current-font ,font)
     (set-window-font)))

(loop for font in font-list collect (eval `(use-font ,font)))

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
  (setq automargin-target-width 120))

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
  (setq nyan-bar-length 10))

;; line-number-mode
(setq line-number-display-width 1000)

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

;; highlight some whitespace
(setup "whitespace"
  (setq whitespace-style '(face tabs tab-mark))
  (setq whitespace-display-mappings
        `(
          (space-mark   ?\s    [?\u00B7] [?.]) ; space      - centered dot
          (space-mark   ?\xA0  [?\u00A4] [?_]) ; hard space - currency
          (newline-mark ?\n    [?$ ?\n])       ; eol        - dollar sign

          ;; consistent spacing of tab
          (tab-mark ?\t ; tab - bar
                    [?\| ,@(make-list (1- tab-width) ?\s)]
                    [?\| ,@(make-list (1- tab-width) ?\s)])
          )))

;; clean up modeline and hide standard minor modes
(setup "diminish"
  (setup-after "abbrev"                (diminish 'abbrev-mode))
  (setup-after "anzu"                  (diminish 'anzu-mode))
  (setup-after "auto-complete"         (diminish 'auto-complete-mode))
  (setup-after "autorevert"            (diminish 'auto-revert-mode))
  (setup-after "eldoc"                 (diminish 'eldoc-mode))
  (setup-after "fic-mode"              (diminish 'fic-mode))
  (setup-after "guide-key"             (diminish 'guide-key-mode))
  (setup-after "haskell-doc"           (diminish 'haskell-doc-mode))
  (setup-after "haskell-indentation"   (diminish 'haskell-indentation-mode))
  (setup-after "hideshow"              (diminish 'hs-minor-mode))
  (setup-after "highlight-parentheses" (diminish 'highlight-parentheses-mode))
  (setup-after "magit"                 (diminish 'magit-auto-revert-mode))
  (setup-after "ruby-block"            (diminish 'ruby-block-mode))
  (setup-after "simple"                (diminish 'auto-fill-function "AF"))
  (setup-after "simple"                (diminish 'global-visual-line-mode))
  (setup-after "simple"                (diminish 'visual-line-mode))
  (setup-after "slime"                 (diminish 'slime-mode))
  (setup-after "smartparens"           (diminish 'smartparens-mode))
  (setup-after "subword"               (diminish 'subword-mode))
  (setup-after "subword"               (diminish 'superword-mode))
  (setup-after "undo-tree"             (diminish 'undo-tree-mode))
  (setup-after "volatile-highlights"   (diminish 'volatile-highlights-mode))
  (setup-after "whitespace"            (diminish 'global-whitespace-mode "WS"))
  (setup-after "whitespace"            (diminish 'whitespace-mode " |"))
  (setup-after "whole-line-or-region"  (diminish 'whole-line-or-region-mode))
  (setup-after "yasnippet"             (diminish 'yas-minor-mode)))

;; clean up way-too-long major modes
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "lisp-mode"     emacs-lisp-mode "EL")
(rename-modeline "sh-script"     sh-mode         "sh")
(rename-modeline "ruby-mode"     ruby-mode       "RB")
(rename-modeline "enh-ruby-mode" enh-ruby-mode   "RB+")

(provide 'init-look)
