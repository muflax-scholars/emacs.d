;; key bindings

;; define what keys we're gonna open up for mangling later
(setq assignable-normal-keys
      (split-string (concat
                     "abcdefghijklmnopqrstuvwxyzß"
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZẞ"
                     "1234567890"
                     "!@#$%^&*()-=[]{};'\\:\"|,./<>?`~_+"
                     ) "" t))

(setq assignable-special-keys
      ;; keys that kbd needs to figure out
      '("<f1>"	"<f6>" 	"<f11>"	"<f16>"
        "<f2>"	"<f7>" 	"<f12>"	"<f17>"
        "<f3>"	"<f8>" 	"<f13>"	"<f18>"
        "<f4>"	"<f9>" 	"<f14>"	"<f19>"
        "<f5>"	"<f10>"	"<f15>"	"<f20>"

        "<mouse-1>"	"<down-mouse-1>"	"<drag-mouse-1>"	"<double-mouse-1>"
        "<mouse-2>"	"<down-mouse-2>"	"<drag-mouse-2>"	"<double-mouse-2>"
        "<mouse-3>"	"<down-mouse-3>"	"<drag-mouse-3>"	"<double-mouse-3>"
        "<mouse-4>"	"<down-mouse-4>"	"<drag-mouse-4>"	"<double-mouse-4>"
        "<mouse-5>"	"<down-mouse-5>"	"<drag-mouse-5>"	"<double-mouse-5>"
        "<mouse-6>"	"<down-mouse-6>"	"<drag-mouse-6>"	"<double-mouse-6>"
        "<mouse-7>"	"<down-mouse-7>"	"<drag-mouse-7>"	"<double-mouse-7>"
        "<mouse-8>"	"<down-mouse-8>"	"<drag-mouse-8>"	"<double-mouse-8>"
        "<mouse-9>"	"<down-mouse-9>"	"<drag-mouse-9>"	"<double-mouse-9>"

        "<up>"       	"<down>"  	"<left>"  	"<right>"
        "<next>"     	"<prior>" 	"<home>"  	"<end>"
        "<backspace>"	"<delete>"	"<insert>"	"<insertchar>"
        "<ret>"      	"<return>"	"<tab>"   	"<escape>"
        "<menu>"     	"<kanji>" 	"<backtab>"

        "SPC"	"TAB"	"DEL"	"ESC"
        ))

(setq assignable-normal-modifiers
      ;; valid for all keys
      '("C-"  	"C-S-"
        "M-"  	"M-S-"
        "C-M-"	"C-M-S-"))

(setq assignable-special-modifiers
      ;; only valid with special keys
      '("" "S-"))

;; C-A and C-S-A should be synonymous (despite terminal limitations)
(defadvice kbd (before simplified-shifted-keys (keys) activate)
  (let ((case-fold-search nil))
    (setq keys (replace-regexp-in-string
                "C-\\([A-Z]\\)\\([ ]\\|$\\)"
                "S-\\1" keys t nil 1))))

(defun unset-keymap-by-keys (keys modifiers keymap)
  "Unsets all MODIFIERS + KEYS in KEYMAP, except for self-insert-commands, which you probably want."
  (let ((full-key))

    (loop for modifier in modifiers collect
          (loop for key in keys collect
                (progn
                  (setq full-key (kbd (concat modifier key)))
                  (unless (eq (lookup-key keymap full-key)
                              'self-insert-command)
                    (define-key keymap full-key nil)))))))

(defun copy-keymap-by-keys (keys modifiers from to)
  "Copies all MODIFIERS + KEYS from one keymap (FROM) to another (TO)."
  (let ((full-key)
        (command))

    (loop for modifier in modifiers collect
          (loop for key in keys collect
                (progn
                  (setq full-key (kbd (concat modifier key)))
                  (setq command (lookup-key from full-key))
                  (when command
                    (define-key to full-key command)))))))

(defun mangle-keys (command &rest maps)
  "operate on all assignable keys in a given keymap"
  (apply command
         (append
          assignable-normal-keys
          assignable-special-keys)
         assignable-normal-modifiers
         maps)

  (apply command
         assignable-special-keys
         assignable-special-modifiers
         maps))

(defun unset-complete-keymap (&rest maps)
  "unset all assignable keys in given keymap(s)"
  (dolist (map maps)
    (mangle-keys 'unset-keymap-by-keys map)))

(defun copy-complete-keymap (from to)
  "copies over all assignable keys from one keymap to another"
  (mangle-keys 'copy-keymap-by-keys from to))

(defun overshadowed-terminal-command (command alt-key)
  "Executes COMMAND when called outside a terminal, or the command under ALT-KEY if we're in a terminal and can't normally reach that key. That obviously shadows the original COMMAND, but at least you get to use ALT-KEY normally."
  (if (display-graphic-p)
      (call-interactively command)
    (progn
      (setq command (lookup-key global-map (kbd alt-key)))
      (when command
        (call-interactively command)))))

(defun repeat-command (command)
  "Call command, and immediately go into repeat mode."
  (interactive)
  (let ((repeat-message-function  'ignore))
    (setq last-repeatable-command  command)
    (repeat nil)))

;; define prefix keys
(defun set-prefix-key (map key command)
  (define-prefix-command command)
  (define-key map (kbd key) command))

(defun kd (map &rest bindings)
  "Really short list version of 'key-def'."
  (dolist (binding bindings)
    (apply 'key-def map binding)))

(defun* key-def (map key command &key
                     (type    	'simple)
                     (alt-key 	nil)
                     (debug   	nil)
                     (help    	nil)
                     (override	nil))
  "Short, unified key definition."
  (interactive)

  ;; sanity check
  (when (and debug
             (symbolp command)
             (not (fboundp command)))
    (message "key error: «%s» -> «%s» (undefined)"
             key command))

  ;; help for which-key
  (when help
    (when (boundp 'which-key-description-replacement-alist)
      (add-to-list 'which-key-description-replacement-alist
                   (cons (symbol-name command)
                         (format "%s (%s)" command help)))))

  (case type
    ;; Creates a prefix map for the key.
    ('prefix
     (set-prefix-key map key command))

    ;; C-i is indistinguishable from <tab> in a terminal, so pressing C-i in a terminal just invokes whatever is under <tab>. Otherwise you'd lose <tab> entirely there, and it's more important than C-i. This section automagically makes that work, by invoking ALT-KEY's command in a terminal instead.
    ('terminal
     (let ((new-key (s-replace "C-" "H-" key)))
       ;; move the key out of the way
       (define-key input-decode-map (kbd key) (kbd new-key))

       ;; split the command so it still works in a terminal
       (define-key map (kbd new-key)
         (eval `(lambda () (interactive)
                  (overshadowed-terminal-command ',command ,alt-key))))))

    ;; lispy modal key
    ('lispy
     (lispy-define-key map key command `(:override ,override)))

    ;; Plain key with a simple command.
    ('simple
     (define-key map (kbd key) command))

    ;; error
    (t
     (error "wrong key type: %s" type))))

;; un-fuck-up with a shotgun modes that steal C-c badly
(defun unbreak-stupid-map (stupid-map)
  (define-key stupid-map (kbd "C-c") nil))

(defun nuke-keymap (map &optional mode)
  ;; nuke the keymap so we can start over
  (set map (make-sparse-keymap))
  (when (and mode (boundp mode))
    (setcdr (assq mode minor-mode-map-alist)
            (symbol-value map))))

(load-after 'python       	(unbreak-stupid-map	python-mode-map))
(load-after 'enh-ruby-mode	(unbreak-stupid-map	enh-ruby-mode-map))
(load-after 'go-mode      	(unbreak-stupid-map	go-mode-map))
(load-after 'flycheck     	(unbreak-stupid-map	flycheck-mode-map))
(load-after 'conf-mode    	(unbreak-stupid-map	conf-mode-map))

;; fix mod4 bug
(define-key special-event-map (kbd "<key-17>")  	'ignore)
(define-key special-event-map (kbd "<M-key-17>")	'ignore)

;; find unused keys
(require 'free-keys)
;; allowed key components
(setq free-keys-keys (apply 'concat assignable-normal-keys))

;; prefix help
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right)
(setq which-key-separator " ")
(setq which-key-unicode-correction 0)
(setq which-key-max-description-length nil)
(setq which-key-idle-delay 0.5)
(setq which-key-special-keys nil)
(setq which-key-echo-keystrokes 0.1)
(setq which-key-show-prefix 'echo)

;; built-ins prefix maps restated for clarity
(defvar global-map)
(defvar ctl-x-map)
(defvar help-map)
(defvar mode-specific-map)
(defvar universal-argument-map)

(require 'wdired)

;; unset a lot of default keys so we can properly re-assign them later
(loop for map in (list
                  global-map
                  ctl-x-map
                  mode-specific-map
                  special-mode-map
                  wdired-mode-map
                  )
      collect (unset-complete-keymap map))

;; global keys

;; mouse
(kd global-map
    '("C-S-<mouse-1>"   	mc/add-cursor-on-click)
    '("C-<down-mouse-1>"	mc/add-cursor-on-click)
    '("<double-mouse-1>"	mouse-set-point)
    '("<down-mouse-1>"  	mouse-drag-region)
    '("<drag-mouse-1>"  	mouse-set-region)
    '("<mouse-1>"       	mouse-set-point)
    '("<mouse-2>"       	mouse-yank-primary)
    '("<mouse-3>"       	fold-dwim-toggle)
    '("<mouse-4>"       	mwheel-scroll)
    '("<mouse-5>"       	mwheel-scroll)

    ;; navigation keys
    '("C-<down>" 	mc/mark-next-like-this)
    '("C-<left>" 	er/contract-region)
    '("C-<right>"	er/expand-region)
    '("C-<up>"   	mc/mark-previous-like-this)

    '("C-<end>"  	end-of-buffer)
    '("C-<home>" 	beginning-of-buffer)
    '("C-<next>" 	dired-next)
    '("C-<prior>"	dired-prev)

    '("M-<down>" 	md/move-lines-down)
    '("M-<up>"   	md/move-lines-up)

    '("C-M-<down>" 	buf-move-down)
    '("C-M-<left>" 	buf-move-left)
    '("C-M-<right>"	buf-move-right)
    '("C-M-<up>"   	buf-move-up)

    '("<down>" 	next-line)
    '("<left>" 	left-char)
    '("<right>"	right-char)
    '("<up>"   	previous-line)

    '("<end>"  	end-of-buffer)
    '("<home>" 	beginning-of-buffer)
    '("<next>" 	scroll-up-command)
    '("<prior>"	scroll-down-command)

    ;; special keys
    '("C-<backspace>"	smart-backward-kill-word)
    '("C-<delete>"   	smart-kill-word)
    '("C-<return>"   	md/duplicate-down)
    '("C-<tab>"      	literal-tab)

    '("C-SPC"	set-mark-command)

    '("M-<tab>"	elastic-align-region-or-current)

    '("<f3>"       	kmacro-start-macro-or-insert-counter)
    '("<f4>"       	kmacro-end-or-call-macro)
    '("<backspace>"	delete-backward-char)
    '("<delete>"   	delete-char) ; make DEL always work like intended

    '("S-<backspace>" 	kill-beginning-of-line)
    '("S-<delete>"    	kill-line)
    '("S-<insert>"    	whole-line-or-region-yank)
    '("S-<insertchar>"	whole-line-or-region-yank)

    '("C-S-<backspace>"	kill-beginning-of-line-and-join-backward)
    '("C-S-<delete>"   	kill-and-join-forward)

    '("M-<backspace>"	copy-beginning-of-line)
    '("M-<delete>"   	copy-line)

    '("RET"  	newline)
    '("SPC"  	self-insert-command)
    '("S-SPC"	set-mark-command)
    '("TAB"  	indent-for-tab-command)

    ;; punctuation keys
    '("C--" 	negative-argument)
    '("C-\\"	generalized-shell-command) ; terminal bug, same as C-|
    '("C-|" 	generalized-shell-command)

    ;; numbers
    '("C-0"	digit-argument)
    '("C-1"	digit-argument)
    '("C-2"	digit-argument)
    '("C-3"	digit-argument)
    '("C-4"	digit-argument)
    '("C-5"	digit-argument)
    '("C-6"	digit-argument)
    '("C-7"	digit-argument)
    '("C-8"	digit-argument)
    '("C-9"	digit-argument)

    ;; alphabet
    '("C-a"   	smart-beginning-of-line)
    '("C-A"   	beginning-of-line)
    '("C-b"   	backward-word)
    '("C-c"   	mode-specific-command-prefix)
    ;; '("C-d"	ac-trigger-key)
    '("C-e"   	smart-end-of-line)
    '("C-E"   	end-of-line)
    '("C-f"   	forward-word)
    '("C-g"   	keyboard-quit) ; also change quit-char if you wanna move it
    '("C-h"   	help-command)
    '("C-i"   	indent-for-tab-command :type terminal :alt-key "TAB")
    '("C-j"   	newline-and-indent)
    '("C-k"   	kill-and-join-forward)
    '("C-K"   	sp-kill-hybrid-sexp)
    '("C-l"   	recenter-top-bottom)
    '("C-m"   	newline :type terminal :alt-key "RET")
    '("C-n"   	focus-next-window)
    '("C-N"   	focus-prev-window)
    '("C-o"   	yas-expand)
    '("C-p"   	mc-prefix-map :type prefix)
    '("C-q"   	quoted-insert)
    '("C-r"   	window-prefix-map :type prefix)
    '("C-s"   	search-prefix-map :type prefix)
    '("C-t"   	save-buffer)
    '("C-u"   	universal-argument)
    '("C-v"   	visual-prefix-map :type prefix)
    '("C-w"   	whole-line-or-region-kill-region)
    '("C-W"   	sp-kill-sexp)
    '("C-x"   	command-prefix-map :type prefix)
    '("C-y"   	yank-and-indent)
    '("C-Y"   	yank)
    '("C-z"   	undo-tree-undo)
    '("C-Z"   	undo-tree-redo)

    ;; to convert
    '("M-y"	yank-pop)
    '("M-Y"	yank-pop-reverse)

    ;; done
    '("M-k"	copy-line)
    '("M-n"	undo-tree-undo)
    '("M-p"	undo-tree-redo)
    '("M-x"	smex)
    '("M-w"	whole-line-or-region-kill-ring-save)
    '("M-W"	sp-copy-sexp)
    '("M-z"	undo-tree-redo)
    )

;; less commonly used functions
(kd command-prefix-map
    '("C-+"	text-scale-adjust)
    '("C--"	text-scale-adjust)
    '("C-="	text-scale-adjust)
    '("C-0"	text-scale-adjust)
    '("C-b"	ibuffer)
    '("C-c"	save-buffers-kill-terminal)
    '("C-f"	find-file)
    '("C-g"	abort-recursive-edit)
    '("C-j"	dired-jump)
    '("C-o"	yas-reload-all)
    '("C-r"	recentf-ido-find-file)
    '("C-s"	save-buffer)
    '("C-v"	find-alternate-file)
    '("C-w"	write-file)
    '("C-x"	smex)
    '("SPC"	eval-prefix-map :type prefix)
    '("+"  	balance-windows)
    '("b"  	switch-to-buffer)
    '("c"  	case-prefix-map :type prefix)
    '("d"  	debug-prefix-map :type prefix)
    '("f"  	font-prefix-map :type prefix)
    '("g"  	magit-status)
    '("h"  	mark-whole-buffer)
    '("i"  	indent-region)
    '("k"  	kill-buffer)
    '("m"  	number-prefix-map :type prefix)
    '("M"  	macro-prefix-map :type prefix)
    '("p"  	paradox-list-packages)
    '("s"  	save-some-buffers)
    '("t"  	input-prefix-map :type prefix)
    )

(kd mode-specific-map
    '("C-w"	kill-with-append)
    '("SPC"	whole-line-or-region-comment-dwim-2)
    '("c"  	comment-region)
    '("u"  	uncomment-region)
    '("w"  	copy-with-append)
    )

;; search
(kd search-prefix-map
    '("C-r"	isearch-backward-use-region)
    '("C-s"	isearch-forward-use-region)
    '("*"  	isearch-forward-symbol)
    '("b"  	isearch-backward-regexp)
    '("B"  	isearch-backward-use-region)
    '("e"  	next-error)
    '("E"  	previous-error)
    '("i"  	idomenu)
    '("I"  	imenu-anywhere)
    '("l"  	goto-line)
    '("o"  	occur)
    '("p"  	phi-search)
    '("P"  	phi-search-backward)
    '("r"  	vr/query-replace)
    '("R"  	vr/query-replace-from-beginning)
    '("s"  	isearch-forward-use-region)
    '("S"  	isearch-forward-regexp)
    '("y"  	kill-ring-search)
    '("["  	idomenu)
    )

;; with active search
(load-after 'phi-search
  (require 'phi-search-mc)
  (kd phi-search-default-map
      '("C-<down>"  	phi-search-mc/mark-next)
      '("C-<up>"    	phi-search-mc/mark-previous)
      '("C-<return>"	phi-search-mc/mark-here)
      '("C-p SPC"   	phi-search-mc/mark-all)
      ))

(load-after "isearch"
  (kd isearch-mode-map
      ;; make backspace more intuitive
      '("<backspace>"	isearch-del-char)

      '("S-<down>" 	isearch-repeat-forward)
      '("S-<left>" 	isearch-repeat-backward)
      '("S-<right>"	isearch-repeat-forward)
      '("S-<up>"   	isearch-repeat-backward)

      '("C-c C-c"	isearch-normalize-string)
      '("C-c SPC"	isearch-toggle-lax-whitespace)
      '("C-c c"  	isearch-toggle-case-fold)
      '("C-c i"  	isearch-toggle-case-fold)
      '("C-c o"  	isearch-occur)
      '("C-c r"  	isearch-toggle-regexp)
      '("C-c s"  	isearch-toggle-symbol)
      '("C-c w"  	isearch-toggle-word)

      '("C-r" 	isearch-repeat-backward)
      '("C-s" 	isearch-repeat-forward)
      '("C-w" 	isearch-yank-word-or-char)
      '("C-y" 	isearch-yank-kill)
      '("C-\\"	isearch-toggle-input-method)
      ))

(load-after "replace" ;; occur
  (kd occur-mode-map
      '("C-c C-c"	occur-edit-mode)
      '("C-c C-f"	next-error-follow-minor-mode)

      '("C-o"	occur-mode-display-occurrence)
      '("RET"	occur-mode-goto-occurrence)
      '("c"  	clone-buffer)
      '("e"  	occur-edit-mode)
      '("n"  	occur-next)
      '("o"  	occur-mode-goto-occurrence-other-window)
      '("p"  	occur-prev)
      '("r"  	occur-rename-buffer)
      ))

;; eval
(kd eval-prefix-map
    '("SPC"	eval-defun)
    '("b"  	eval-buffer)
    '("d"  	eval-defun)
    '("e"  	eval-expression)
    '("r"  	eval-region)
    '("R"  	eval-and-replace)
    )

;; debug
(kd debug-prefix-map
    '("d"	edebug-defun)
    '("g"	profiler-stop)
    '("r"	profiler-report)
    '("s"	profiler-start)
    )

(load-after 'yasnippet
  ;; saner trigger key
  (define-key yas-minor-mode-map [(tab)]    	nil)
  (define-key yas-minor-mode-map (kbd "TAB")	nil)

  (kd yas-keymap
      '("<return>"	yas/exit-all-snippets)
      '("C-a"     	yas/goto-start-of-active-field)
      '("C-e"     	yas/goto-end-of-active-field)
      '("C-o"     	yas-next-field-or-maybe-expand)
      '("C-O"     	yas-next-field)
      ))

;; auto-completion
(load-after 'auto-complete-config
  (setq ac-use-menu-map nil)
  (ac-set-trigger-key "C-d")

  ;; unset stupid keys
  (define-key ac-completing-map "\t"         	nil)
  (define-key ac-completing-map [tab]        	nil)
  (define-key ac-completing-map (kbd "<Tab>")	nil)
  (define-key ac-completing-map [up]         	nil)
  (define-key ac-completing-map [down]       	nil)
  (define-key ac-completing-map (kbd "M-n")  	nil)
  (define-key ac-completing-map (kbd "M-p")  	nil)
  (define-key ac-completing-map [return]     	nil)
  (define-key ac-completing-map "\r"         	nil)

  (kd ac-completing-map
      '("C-j"	ac-complete)
      '("C-d"	ac-next)
      '("C-D"	ac-previous)
      ))

;; multiple cursors
(kd mc-prefix-map
    '("SPC"	mc/mark-all-dwim)
    '("b"  	mc/skip-to-previous-like-this)
    '("c"  	mc/compare-chars-forward)
    '("C"  	mc/compare-chars-backward)
    '("d"  	mc/remove-duplicated-cursors)
    '("f"  	mc/skip-to-next-like-this)
    '("g"  	mc-jump-char)
    '("l"  	mc/edit-lines)
    '("m"  	mc/mark-more-like-this-extended)
    '("n"  	mc/insert-numbers)
    '("R"  	mc/reverse-regions)
    '("r"  	set-rectangular-region-anchor)
    '("s"  	mc/sort-regions)
    '("u"  	mc/remove-current-cursor)
    '("y"  	mc/many-to-one-yank-indent)
    '("Y"  	mc/many-to-one-yank)
    )

(load-after 'multiple-cursors-core
  ;; <ret> inserts a newline; C-j exits (a bit more convenient that way)
  (kd mc/keymap
      '("<return>"	nil)
      '("C-j"     	multiple-cursors-mode)

      '("C-p h"	mc-hide-unmatched-lines-mode)
      ))

;; handle comments
(load-after 'cc-mode
  (kd c-mode-map
      '("C-j"	c-indent-new-comment-line)
      ))

;; input methods
(kd input-prefix-map
    '("SPC"	toggle-input-method)
    '("0"  	clear-input-method)
    '("c"  	set-input-method-muflax-cyrillic)
    '("g"  	set-input-method-muflax-greek)
    '("j"  	set-input-method-japanese-mozc)
    '("l"  	set-input-method-muflax-latin)
    '("m"  	set-input-method-muflax-latin)
    '("s"  	toggle-subword-mode)
    '("t"  	set-input-method-muflax-turkish)
    )

;; window config
(kd window-prefix-map
    '("<down>"  	split-window-below)
    '("<left>"  	split-window-left)
    '("<right>" 	split-window-right)
    '("<up>"    	split-window-above)
    '("<return>"	delete-other-windows)
    '("C-r"     	delete-other-windows)
    '("S-SPC"   	kill-buffer-and-window)
    '("SPC"     	delete-window)
    '("="       	balance-windows)
    '("b"       	winner-undo)
    '("f"       	winner-redo)
    '("k"       	delete-other-windows)
    '("w"       	delete-window)
    )

;; arithmetic
(kd number-prefix-map
    '("<down>"	number/decrement)
    '("<up>"  	number/increment)
    '("+"     	number/add)
    '("-"     	number/sub)
    '("*"     	number/multiply)
    '("/"     	number/divide)
    '("="     	number/eval)
    '("0"     	number/pad)
    '("a"     	number/add)
    '("d"     	number/divide)
    '("e"     	number/eval)
    '("m"     	number/multiply)
    '("p"     	number/pad)
    '("s"     	number/sub)
    )

;; visual display
(kd visual-prefix-map
    '("C-f"	fold-dwim-toggle      	:help "toggle current bracket")
    '("SPC"	hs-show-block         	:help "unfold current bracket")
    '("f"  	hs-fold-levels        	:help "fold all brackets")
    '("F"  	fold-dwim-show-all    	:help "unfold all brackets")
    '("h"  	hl-line-mode          	:help "highlight current line")
    '("l"  	toggle-truncate-lines 	:help "truncate lines")
    '("s"  	whitespace-fold-levels	:help "fold whitespace")
    '("S"  	whitespace-fold-reset 	:help "unfold whitespace")
    '("w"  	leerzeichen-mode      	:help "show whitespace")
    )

;; org-mode
(load-after 'org
  (org-defkey org-mode-map (kbd "C-c C-d")	'org-todo-done)
  (org-defkey org-mode-map (kbd "C-c C-t")	'org-todo-todo)
  (org-defkey org-mode-map (kbd "C-c C-w")	'org-todo-waiting))

;; dired
(load-after 'dired
  (load-after 'wdired
    (kd dired-mode-map
        '("C-c C-c" 	wdired-change-to-wdired-mode)
        '("<insert>"	dired-mark)
        '("."       	dired-omit-mode)

        ;; C-a goes to filename
        '(	"C-a"	dired-back-to-start-of-files)
        )

    (kd wdired-mode-map
        '("C-a"    	dired-back-to-start-of-files)
        '("C-c C-c"	wdired-finish-edit)
        '("C-c C-k"	wdired-abort-changes)
        )

    ;; M-up goes to first file
    (define-key dired-mode-map 	[remap beginning-of-buffer]	'dired-back-to-top)
    (define-key wdired-mode-map	[remap beginning-of-buffer]	'dired-back-to-top)
    (define-key dired-mode-map 	[remap smart-up]           	'dired-back-to-top)

    ;; M-down goes to last file
    (define-key dired-mode-map 	[remap end-of-buffer]	'dired-jump-to-bottom)
    (define-key dired-mode-map 	[remap smart-down]   	'dired-jump-to-bottom)
    (define-key wdired-mode-map	[remap end-of-buffer]	'dired-jump-to-bottom)))

(load-after 'magit
  ;; needed because of fullscreen override
  (kd magit-status-mode-map
      '("q"	magit-quit-session)
      '("W"	magit-toggle-whitespace)))

;; case changes
(kd case-prefix-map
    '("SPC"	toggle-title-case)
    '("c"  	toggle-upcase)
    '("u"  	upcase-word-or-region)
    '("d"  	downcase-word-or-region)
    '("l"  	downcase-word-or-region)
    '("t"  	title-case-word-or-region)
    '("T"  	toggle-title-case)
    )

;; general lisp
(load-after 'lisp-mode
  ;; operate on sexps
  (kd lisp-mode-shared-map
      '("<return>"  	sp-newline)
      '("M-<return>"	sp-clone-sexp)

      '("C->"	sp-forward-slurp-sexp)
      '("C-<"	sp-backward-slurp-sexp)
      '("M-<"	sp-forward-barf-sexp)
      '("M->"	sp-backward-barf-sexp)

      '("M-("	lispy-wrap-round)
      '("M-)"	lispy-close-round-and-newline)

      '("C-("	sp-down-sexp)
      '("C-)"	sp-up-sexp)

      '("C-;"	sotlisp-comment-or-uncomment-sexp)
      ))

(load-after 'racket-mode
  (set-keymap-parent racket-mode-map lisp-mode-shared-map)

  (kd racket-mode-map
      ;; '("C-c SPC"	racket-run)
      '("C-c C-c"   	racket-run)
      '("C-c r"     	racket-repl)
      '("C-c C-r"   	racket-run-and-switch-to-repl)
      '("C-c C-p"   	racket-profile)
      '("C-c C-t"   	racket-test)

      '("C-c s d"	racket-send-definition)
      '("C-c s e"	racket-send-last-sexp)
      '("C-c s r"	racket-send-region)

      '("C-c e d"	racket-expand-definition)
      '("C-c e e"	racket-expand-last-sexp)
      '("C-c e r"	racket-expand-region)
      '("C-c e e"	racket-expand-again)

      '("C-c C-x C-f"	racket-open-require-path)
      '("M-C-u"      	racket-backward-up-list)

      '("C-c C-d"	racket-doc)
      '("C-c C-."	racket-describe)
      '("C-c f"  	racket-fold-all-tests)
      '("C-c F"  	racket-unfold-all-tests)

      ;; let's try this
      '(")"	racket-insert-closing)
      '("]"	racket-insert-closing)
      '("}"	racket-insert-closing)
      ))

;; fonts
(kd font-prefix-map
    '("1"	use-small-font)
    '("2"	use-normal-font)
    '("3"	use-big-font)
    '("4"	use-huge-font)
    )

;; special read-only buffers
(kd special-mode-map
    '(" "     	scroll-up-command)
    '("<"     	beginning-of-buffer)
    '(">"     	end-of-buffer)
    '("?"     	describe-mode)
    '("g"     	revert-buffer)
    '("h"     	describe-mode)
    '("q"     	quit-window)
    )

(load-after 'prompt-minor-mode
  (kd prompt-minor-mode-map
      '("<f12>"    	corpus/open-corpus-file)
      '("<mouse-9>"	corpus/open-corpus-file)

      '("<f10>"	prompt/bunf-current-line)

      ;; navigation
      '("C-c t a"  	prompt/beginning-of-plus-block)
      '("C-c t e"  	prompt/end-of-plus-block)
      '("C-c t C-a"	prompt/beginning-of-example-grab-bracket)
      '("C-c t C-e"	prompt/end-of-example-grab-bracket)

      '("C-S-f" 	prompt/next-block)
      '("C-S-b" 	prompt/prev-block)
      ;; '("M-f"	prompt/next-bracket)
      ;; '("M-b"	prompt/prev-bracket)

      ;; folding
      '("C-c t f a"	prompt/fold-show-adjectives)
      '("C-c t f v"	prompt/fold-show-verbs)
      '("C-c t f n"	prompt/fold-show-nouns)
      '("C-c t f r"	prompt/fold-rest)
      '("C-c t f i"	prompt/fold-invert)
      '("C-c t f f"	prompt/fold-hide-all)
      '("C-c t f F"	prompt/fold-show-all)

      ;; moving blocks
      '("<insert>"    	prompt/copy-example-block)
      '("C-S-w"       	prompt/kill-example-block)
      '("C-<left>"    	prompt/select-example-block)
      ;; '("M-<prior>"	prompt/move-example-block-up)
      ;; '("M-<next>" 	prompt/move-example-block-down)

      ;; cleaning
      '("C-c t c"  	prompt/convert-all-dict-entry)
      '("<f5>"     	prompt/convert-dict-entry)
      '("C-c t SPC"	prompt/convert-dict-entry)

      '("C-c C-c"	prompt/change-plus-slot)
      '("C-c C-d"	prompt/delete-plus-slot)
      '("C-c o"  	prompt/make-slot-optional)
      '("<f13>"  	prompt/make-slot-optional)
      ))

(load-after 'corpus-minor-mode
  (kd corpus-minor-mode-map

      ;; open corpus file
      '("<f12>"  	corpus/open-next-corpus-file)
      '("C-<f12>"	corpus/open-next-corpus-file)

      ;; navigation in corpus
      '("n"	prompt/next-example-block)
      '("r"	prompt/prev-example-block)
      '("h"	(lambda () (interactive (prompt/next-example-block 5))))
      '("g"	(lambda () (interactive (prompt/prev-example-block 5))))

      '("a"	prompt/next-example-block)
      '("i"	prompt/prev-example-block)
      '("l"	(lambda () (interactive (prompt/next-example-block 5))))
      '("v"	(lambda () (interactive (prompt/prev-example-block 5))))

      '("<down>"   	prompt/next-example-block)
      '("<up>"     	prompt/prev-example-block)
      '("<left>"   	corpus/add-example-to-plus-line)
      '("<right>"  	corpus/copy-example-block-over)
      '("C-<down>" 	next-line)
      '("C-<up>"   	previous-line)
      '("C-<left>" 	left-char)
      '("C-<right>"	right-char)

      '("s"	prompt/next-example-grab-bracket)
      '("k"	prompt/prev-example-grab-bracket)
      '("o"	prompt/next-example-grab-bracket)
      '("w"	prompt/prev-example-grab-bracket)

      '("b"	prompt/beginning-of-example-grab-bracket)
      '("m"	prompt/end-of-example-grab-bracket)

      ;; moving stuff over
      '("e"         	corpus/add-example-to-plus-line)
      '("C-<insert>"	corpus/copy-example-block-over)
      '("<mouse-8>" 	corpus/copy-example-block-over)
      ))

(load-after 'word-list-minor-mode
  (kd word-list-minor-mode-map
      '("<f12>"	word-list/open-word)

      '("<left>"      	word-list/unmark-item)
      '("<right>"     	word-list/mark-item)
      ;; '("M-<left>" 	word-list/unmark-item-all)
      ;; '("M-<right>"	word-list/mark-item-all)
      ))

(load-after 'lesson-minor-mode
  (kd lesson-minor-mode-map
      ;; navigation in corpus
      '("C-<left>" 	lesson/prev-block)
      '("C-<up>"   	lesson/prev-block)
      '("C-<right>"	lesson/next-block)
      '("C-<down>" 	lesson/next-block)

      '("M-<left>" 	lesson/prev-lesson)
      '("M-<up>"   	lesson/prev-lesson)
      '("M-<right>"	lesson/next-lesson)
      '("M-<down>" 	lesson/next-lesson)

      '("<left>" 	lesson/scroll-down-line)
      '("<up>"   	lesson/scroll-down-line)
      '("<right>"	lesson/scroll-up-line)
      '("<down>" 	lesson/scroll-up-line)

      '("C-<SPC>"	lesson/prev-thing)
      '("<SPC>"  	lesson/next-thing)
      '("<kanji>"	recenter-top-bottom)
      '("<menu>" 	lesson-minor-mode)
      '("q"      	lesson-minor-mode)
      ))

(load-after 'phi-grep
  (kd phi-grep-mode-map
      '("RET"	phi-grep-exit)
      ))

(load-after 'notes-mode
  (kd notes-mode-map
      '("RET"	newline-and-indent)
      '("C-j"	newline)

      '("C-c C-t"	lesson/update-time-marks)
      '("C-c C-c"	lesson/add-translations-to-block)
      '("C-c r"  	lesson/set-reading-mark)

      ))

(load-after 'eww-mode
  (kd eww-mode-map
      '("S-SPC"	set-mark-command)
      '("C-SPC"	set-mark-command)
      ))

(load-after 'ido
  (defun ido-my-keys ()
    (kd ido-common-completion-map
        '("SPC"	nil)
        ))

  (add-hook 'ido-setup-hook 'ido-my-keys))

(load-after 'adjust-parens
  (nuke-keymap 'adjust-parens-mode-map 'adjust-parens-mode)
  (kd adjust-parens-mode-map
      '("M-<left>" 	lisp-dedent-adjust-parens)
      '("M-<right>"	lisp-indent-adjust-parens)
      ))

(load-after 'lispy
  (nuke-keymap 'lispy-mode-map 'lispy-mode)
  (kd lispy-mode-map
      ;; jump 	
      ;; '("s"	lispy-ace-symbol)
      ;; '("S"	lispy-ace-symbol-replace)
      ;; '("-"	lispy-ace-subword)
      ;; '("p"	lispy-ace-paren)

      ;; marking	
      ;; '("m"  	lispy-mark-list)

      ;; insert	
      ;; '(";" 	lispy-comment)

      ;; pairs	
      ;; '("("	lispy-parens)
      ;; '(")"	lispy-right-nostring)

      ;; navigation	
      ;; '("["     	lispy-backward)
      ;; '("]"     	lispy-forward)
      ;; '("C-3"   	lispy-right)

      ;; killing	
      ;; '("C-k"	lispy-kill)           	;; better default?
      ;; '("C-K"	kill-and-join-forward)	
      ;; change 	
      )

  (nuke-keymap 'lispy-eb-keymap)
  (kd lispy-eb-keymap
      '("C-c C-c"	lispy-eb-finalize)
      ))

(load-after 'sly
  ;; start fresh
  (nuke-keymap 'sly-mode-map        	'sly-mode)
  (nuke-keymap 'sly-editing-mode-map	'sly-editing-mode)
  (nuke-keymap 'sly-mrepl-mode-map  	'sly-mrepl-mode)
  (nuke-keymap 'sly-prefix-map)

  (kd sly-mode-map
      '("C-c C-s"	sly)
      ;; '("M-."      	'sly-edit-definition)
      ;; '("M-,"      	'sly-pop-find-definition-stack)
      ;; '("M-_"      	'sly-edit-uses)    ; for German layout
      ;; '("M-?"      	'sly-edit-uses)    ; for USian layout
      ;; '("C-x 4 ."  	'sly-edit-definition-other-window)
      ;; '("C-x 5 ."  	'sly-edit-definition-other-frame)
      ;; '("C-x C-e"  	'sly-eval-last-expression)
      ;; '("C-M-x"    	'sly-eval-defun)
      ;; '("C-c TAB"  	'sly-complete-symbol)
      ;; '("C-c C-p"  	'sly-pprint-eval-last-expression)
      ;; '("C-c C-m"  	'sly-expand-1)
      ;; '("C-c M-m"  	'sly-macroexpand-all)
      ;; '("C-c C-u"  	'sly-undefine-function)
      ;; '("C-c C-r"  	'sly-eval-region)
      ;; '("C-c :"    	'sly-interactive-eval)
      ;; '("C-c C-e"  	'sly-interactive-eval)
      ;; '("C-c E"    	'sly-edit-value)
      ;; '("C-c C-l"  	'sly-load-file)
      ;; '("C-c C-b"  	'sly-interrupt)
      ;; '("C-c M-d"  	'sly-disassemble-symbol)
      ;; '("C-c C-t"  	'sly-toggle-trace-fdefinition)
      ;; '("C-c I"    	'sly-inspect)
      ;; '("C-c C-x t"	'sly-list-threads)
      ;; '("C-c C-x n"	'sly-next-connection)
      ;; '("C-c C-x c"	'sly-list-connections)
      ;; '("C-c C-x p"	'sly-prev-connection)
      ;; '("C-c <"    	'sly-list-callers)
      ;; '("C-c >"    	'sly-list-callees)
      ;; '("C-c C-d"  	sly-doc-map)
      ;; '("C-c C-w"  	sly-who-map)
      ;; '("C-c C-s"  	sly-selector-map)
      )

  (kd sly-editing-mode-map
      ;; '("M-p"    	'sly-previous-note)
      ;; '("M-n"    	'sly-next-note)
      ;; '("C-c M-c"	'sly-remove-notes)
      ;; '("C-c C-k"	'sly-compile-and-load-file)
      ;; '("C-c M-k"	'sly-compile-file)
      ;; '("C-c C-c"	'sly-compile-defun)
      )

  (kd sly-mrepl-mode-map
      ;; '("RET"	'sly-mrepl-return)
      ;; '([return]        'sly-mrepl-return)
      ;; '("TAB"    	'sly-indent-and-complete-symbol)
      ;; '("C-c C-b"	'sly-interrupt)
      ;; '("C-c C-c"	'sly-interrupt)
      ;; '("M-p"    	'sly-mrepl-previous-input-or-button)
      ;; '("M-n"    	'sly-mrepl-next-input-or-button)
      ;; '("C-M-p"  	'sly-button-backward)
      ;; '("C-M-n"  	'sly-button-forward)
      )

  )

(provide 'init-keys)
