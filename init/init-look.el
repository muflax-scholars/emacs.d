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
(add-to-list 'custom-theme-load-path (emacs-d "themes/"))
(require 'leuven-theme) ; force-load it here so we have all faces set up

(defvar bright-theme	'leuven              	"Bright theme to use")
(defvar dark-theme  	'twilight-anti-bright	"Dark theme to use")

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
(require 'hl-line)

;; fonts
(defvar small-font 	"Fantasque Sans Mono 8")
(defvar normal-font	"Fantasque Sans Mono 10")
(defvar big-font   	"Fantasque Sans Mono 11")
(defvar huge-font  	"Fantasque Sans Mono 13")
(defvar font-list '(small-font
                    normal-font
                    big-font
                    huge-font))
(defvar default-font normal-font)

(defun set-window-font (&optional font)
  (set-frame-font (or font default-font)))

(add-hook 'after-make-window-system-frame-hooks 'set-window-font)

;; shortcut functions for the fonts
(defmacro use-font (font)
  `(defun ,(intern (format "use-%s" font)) ()
     ,(format "Use font set in '%s'" font)
     (interactive)
     (set-window-font ,font)))

(loop for font in font-list collect (eval `(use-font ,font)))

;; scrolling
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-error-top-bottom t)
;; necessary or scrolling is really slow
(setq-default bidi-display-reordering nil)
(setq auto-window-vscroll nil)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin	5)
(setq scroll-margin       	5)
(setq scroll-conservatively 10000)

;; try to keep windows within a max margin
(require 'automargin)
(setq automargin-target-width 120)

;; undo highlighting
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; show #colors in matching color
(require 'rainbow-mode)
(defadvice rainbow-mode (after rainbow-mode-refresh activate)
  (font-lock-fontify-buffer))

;; blinking cursor
(blink-cursor-mode -1)
(setq-default cursor-type 'box)

;; nyan nyan nyan
(require 'nyan-mode)
(nyan-mode t)
(setq nyan-bar-length 10)

;; line-number-mode
(setq line-number-display-width 1000)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; avoid splitting horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 90)

;; don't warn about impossible undo
(require 'warnings)
(setq warning-suppress-types (append '(undo discard-info) warning-suppress-types))

;; light-weight default whitespace highlighting
(require 'leerzeichen)

;; nested parentheses are highlighted when inside of them
(require 'highlight-parentheses)
(defun turn-on-highlight-parentheses ()
  (highlight-parentheses-mode 1))
(add-hook 'prog-mode-hook    	'turn-on-highlight-parentheses)
(add-hook 'enh-ruby-mode-hook	'turn-on-highlight-parentheses)

;; parenthesis highlighting behavior
(require 'paren)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'expression)
(setq show-paren-delay 0.125)
(show-paren-mode 1)

;; don't hard-wrap text, but use nice virtual wrapping
(require 'adaptive-wrap)
(setq-default fill-column 80)
(global-visual-line-mode 1)
(global-adaptive-wrap-prefix-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; clean up modeline and hide standard minor modes
(defmacro diminish-minor-mode (package mode &optional short-name)
  `(load-after ,package
     (when (fboundp ,mode)
       (diminish ,mode ,(or short-name "")))))

;; clean up way-too-long major modes
(defmacro diminish-major-mode (package-name mode new-name)
  `(load-after ,package-name
     '(defadvice ,mode (after diminish-major-mode activate)
        (setq mode-name ,new-name))))

(require 'diminish)
(diminish-minor-mode   	'abbrev               	'abbrev-mode               	    	)
(diminish-minor-mode   	'anzu                 	'anzu-mode                 	    	)
(diminish-minor-mode   	'auto-complete        	'auto-complete-mode        	    	)
(diminish-minor-mode   	'autorevert           	'auto-revert-mode          	    	)
(diminish-minor-mode   	'eldoc                	'eldoc-mode                	    	)
(diminish-minor-mode   	'fic-mode             	'fic-mode                  	    	)
(diminish-minor-mode   	'guide-key            	'guide-key-mode            	    	)
(diminish-minor-mode   	'haskell-doc          	'haskell-doc-mode          	    	)
(diminish-minor-mode   	'haskell-indentation  	'haskell-indentation-mode  	    	)
(diminish-minor-mode   	'hideshow             	'hs-minor-mode             	    	)
(diminish-minor-mode   	'highlight-parentheses	'highlight-parentheses-mode	    	)
(diminish-minor-mode   	'magit                	'magit-auto-revert-mode    	    	)
(diminish-minor-mode   	'ruby-block           	'ruby-block-mode           	    	)
(diminish-minor-mode   	'simple               	'auto-fill-function        	"AF"	)
(diminish-minor-mode   	'simple               	'visual-line-mode          	    	)
;; (diminish-minor-mode	'simple               	'global-visual-line-mode   	    	)
(diminish-minor-mode   	'slime                	'slime-mode                	    	)
(diminish-minor-mode   	'smartparens          	'smartparens-mode          	    	)
(diminish-minor-mode   	'subword              	'subword-mode              	    	)
;; (diminish-minor-mode	'subword              	'superword-mode            	    	)
(diminish-minor-mode   	'undo-tree            	'undo-tree-mode            	    	)
(diminish-minor-mode   	'volatile-highlights  	'volatile-highlights-mode  	    	)
(diminish-minor-mode   	'whitespace           	'global-whitespace-mode    	"WS"	)
(diminish-minor-mode   	'whitespace           	'whitespace-mode           	"ws"	)
(diminish-minor-mode   	'whole-line-or-region 	'whole-line-or-region-mode 	    	)
(diminish-minor-mode   	'yasnippet            	'yas-minor-mode            	    	)
(diminish-minor-mode   	'leerzeichen          	'leerzeichen-mode          	" |"	)

(diminish-major-mode	'lisp-mode    	emacs-lisp-mode	"EL" 	)
(diminish-major-mode	'sh-script    	sh-mode        	"sh" 	)
(diminish-major-mode	'ruby-mode    	ruby-mode      	"RB" 	)
(diminish-major-mode	'enh-ruby-mode	enh-ruby-mode  	"RB+"	)

(provide 'init-look)
