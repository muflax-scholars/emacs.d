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
        "<menu>"     	"<kanji>"

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

(defun unset-complete-keymap (map)
  "unset all assignable keys in given keymap"
  (mangle-keys 'unset-keymap-by-keys map))

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
  (--each bindings
    (apply 'key-def map it)))

(defun key-def (map key command &optional type alt-key)
  "Short, unified key definition."
  (interactive)

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

    ;; Plain key with a simple command.
    (t
     (define-key map (kbd key) command))))

;; un-fuck-up with a shotgun modes that steal C-c badly
(defun unbreak-stupid-map (stupid-map)
  (define-key stupid-map (kbd "C-c") nil))

(setup-after "python"       	(unbreak-stupid-map python-mode-map))
(setup-after "enh-ruby-mode"	(unbreak-stupid-map enh-ruby-mode-map))
(setup-after "go-mode"      	(unbreak-stupid-map go-mode-map))
(setup-after "flycheck"     	(unbreak-stupid-map flycheck-mode-map))
(setup-after "conf-mode"    	(unbreak-stupid-map conf-mode-map))

;; fix mod4 bug
(define-key special-event-map (kbd "<key-17>")  	'ignore)
(define-key special-event-map (kbd "<M-key-17>")	'ignore)

;; find unused keys
(setup-lazy '(free-keys) "free-keys"
  ;; allowed key components
  (setq free-keys-keys (apply 'concat assignable-normal-keys)))

;; built-ins prefix maps restated for clarity
(defvar global-map)
(defvar ctl-x-map)
(defvar help-map)
(defvar mode-specific-map)
(defvar universal-argument-map)
(defvar narrow-map)

;; unset a lot of default keys so we can properly re-assign them later
(loop for map in (list
                  global-map
                  ctl-x-map
                  mode-specific-map
                  narrow-map
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
    '("M-<left>" 	er/mark-symbol)
    '("M-<right>"	er/mark-defun)
    '("M-<up>"   	md/move-lines-up)

    '("M-<next>" 	dired-next)
    '("M-<prior>"	dired-prev)

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
    '("C-<backspace>"	backward-kill-word)
    '("C-<delete>"   	kill-word)
    '("C-<menu>"     	nav-global-mode)
    '("C-<return>"   	md/duplicate-down)
    '("C-<tab>"      	literal-tab)

    '("C-SPC"	set-mark-command)

    '("M-<backspace>"	sp-unwrap-sexp)
    '("M-<delete>"   	sp-unwrap-sexp)

    '("<f1>"       	help-command)
    '("<f2>"       	save-buffer)
    '("<f3>"       	kmacro-start-macro-or-insert-counter)
    '("<f4>"       	kmacro-end-or-call-macro)
    '("<f11>"      	automargin-mode)
    '("<backspace>"	delete-backward-char)
    '("<delete>"   	delete-char) ; make DEL always work like intended
    '("<kanji>"    	toggle-input-method)
    '("<menu>"     	nav-minor-mode)

    '("S-<f2>"        	save-some-buffers)
    '("S-<f7>"        	backward-kill-word) ; make C-Backspace "work" in terminal
    '("S-<backspace>" 	literal-delete-backward-char)
    '("S-<delete>"    	literal-delete-char)
    '("S-<insert>"    	whole-line-or-region-yank)
    '("S-<insertchar>"	whole-line-or-region-yank)

    '("RET"  	newline)
    '("SPC"  	self-insert-command)
    '("S-SPC"	set-mark-command)
    '("TAB"  	indent-for-tab-command)

    ;; punctuation keys
    '("C--" 	negative-argument)
    '("C-\\"	generalized-shell-command) ; terminal bug, same as C-|
    '("C-|" 	generalized-shell-command)

    '("M--"	negative-argument)
    '("M-^"	delete-indentation)
    '("M-|"	shell-command-on-region)

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
    '("M-0"	digit-argument)
    '("M-1"	digit-argument)
    '("M-2"	digit-argument)
    '("M-3"	digit-argument)
    '("M-4"	digit-argument)
    '("M-5"	digit-argument)
    '("M-6"	digit-argument)
    '("M-7"	digit-argument)
    '("M-8"	digit-argument)
    '("M-9"	digit-argument)

    ;; alphabet
    '("C-a"	smart-beginning-of-line)
    '("C-b"	backward-word)
    '("C-c"	mode-specific-command-prefix)
    '("C-d"	kill-without-append)
    '("C-e"	smart-end-of-line)
    '("C-f"	forward-word)
    '("C-g"	keyboard-quit) ; also change quit-char if you wanna move it
    '("C-h"	help-command)
    '("C-i"	indent-for-tab-command terminal "TAB")
    '("C-j"	newline-and-indent)
    '("C-k"	kill-and-join-forward)
    '("C-l"	recenter-top-bottom)
    '("C-m"	newline terminal "RET")
    '("C-n"	focus-next-window)
    '("C-N"	focus-prev-window)
    '("C-o"	yas-expand)
    '("C-O"	next-newline-and-indent)
    '("C-p"	mc-prefix-map prefix)
    '("C-q"	quoted-insert)
    '("C-r"	window-prefix-map prefix)
    '("C-s"	search-prefix-map prefix)
    '("C-t"	ac-trigger-key-command)
    '("C-u"	universal-argument)
    '("C-v"	visual-prefix-map prefix)
    '("C-w"	kill-region)
    '("C-x"	command-prefix-map prefix)
    '("C-y"	yank-and-indent)
    '("C-Y"	yank)
    '("C-z"	undo-tree-undo)

    '("M-b"	sp-backward-symbol)
    '("M-d"	blank-line)
    '("M-f"	sp-forward-symbol)
    '("M-h"	mark-paragraph)
    '("M-j"	indent-new-comment-line)
    '("M-k"	copy-line)
    '("M-n"	undo-tree-undo)
    '("M-o"	yas-insert-snippet)
    '("M-p"	undo-tree-redo)
    '("M-q"	fill-region)
    '("M-Q"	unfill-region)
    '("M-t"	find-tag)
    '("M-w"	kill-ring-save)
    '("M-x"	smex)
    '("M-X"	smex-major-mode-commands)
    '("M-y"	yank-pop)
    '("M-Y"	yank-pop-reverse)
    '("M-z"	undo-tree-redo)
    )

;; less commonly used functions
(kd command-prefix-map
    '("C-+"	text-scale-adjust)
    '("C--"	text-scale-adjust)
    '("C-="	text-scale-adjust)
    '("C-0"	text-scale-adjust)
    '("C-b"	list-buffers)
    '("C-c"	save-buffers-kill-terminal)
    '("C-f"	find-file)
    '("C-g"	abort-recursive-edit)
    '("C-r"	recentf-ido-find-file)
    '("C-s"	save-buffer)
    '("C-v"	find-alternate-file)
    '("C-w"	write-file)
    '("C-x"	exchange-point-and-mark)
    '("M-f"	find-file-at-point)
    '("SPC"	eval-prefix-map prefix)
    '("#"  	server-edit)
    '("("  	sexp-prefix-map prefix)
    '("+"  	balance-windows)
    '("a"  	align-prefix-map prefix)
    '("b"  	switch-to-buffer)
    '("c"  	case-prefix-map prefix)
    '("d"  	debug-prefix-map prefix)
    '("f"  	font-prefix-map prefix)
    '("g"  	magit-status)
    '("h"  	mark-whole-buffer)
    '("i"  	indent-region)
    '("k"  	kill-buffer)
    '("m"  	number-prefix-map prefix)
    '("M"  	macro-prefix-map prefix)
    '("n"  	narrow-map)
    '("p"  	paradox-list-packages)
    '("r"  	rectangle-prefix-map prefix)
    '("R"  	register-prefix-map prefix)
    '("s"  	save-some-buffers)
    '("t"  	input-prefix-map prefix)
    '("w"  	spell-check-prefix-map prefix)
    '("x"  	helm-prefix-map prefix)
    '("z"  	repeat)
    '("Z"  	repeat-complex-command)
    )

(kd mode-specific-map
    '("C-<tab>"	sp-indent-adjust-sexp)
    '("<tab>"  	sp-dedent-adjust-sexp)

    '("C-SPC"	comment-dwim)
    '("C-~"  	aya-expand)
    '("C-j"  	dired-jump)
    '("C-o"  	yas-reload-all)
    '("C-t"  	rotate-text)
    '("C-w"  	kill-with-append)
    '("~"    	aya-create)
    '("SPC"  	comment-dwim)
    '("c"    	comment-region)
    '("u"    	uncomment-region)
    '("w"    	copy-with-append)
    )

;; search
(kd search-prefix-map
    '("C-r"	isearch-backward-use-region)
    '("C-s"	isearch-forward-use-region)
    '("SPC"	er/mark-defun)
    '("*"  	isearch-forward-symbol)
    '("b"  	isearch-backward-regexp)
    '("B"  	isearch-backward-use-region)
    '("d"  	er/mark-defun)
    '("e"  	next-error)
    '("E"  	previous-error)
    '("g"  	jump-prefix-map prefix)
    '("i"  	idomenu)
    '("I"  	imenu-anywhere)
    '("o"  	occur)
    '("p"  	phi-search)
    '("p"  	phi-search)
    '("P"  	phi-search-backward)
    '("r"  	vr/query-replace)
    '("R"  	vr/query-replace-from-beginning)
    '("s"  	isearch-forward-use-region)
    '("S"  	isearch-forward-regexp)
    '("w"  	er/mark-symbol)
    '("y"  	kill-ring-search)
    '("["  	idomenu)
    '("]"  	imenu-anywhere)
    )

;; ace-jump
(kd jump-prefix-map
    '("b"	ace-jump-buffer)
    '("c"	ace-jump-char-mode)
    '("g"	ace-jump-mode)
    '("l"	ace-jump-line-mode)
    '("n"	goto-line)
    '("w"	ace-window)
    )

;; with active search
(setup-after "phi-search"
  (setup "phi-search-mc"
    (kd phi-search-default-map
        '("C-<down>"  	phi-search-mc/mark-next)
        '("C-<up>"    	phi-search-mc/mark-previous)
        '("C-<return>"	phi-search-mc/mark-here)
        '("C-p SPC"   	phi-search-mc/mark-all)
        )))

(setup-after "isearch"
  ;; make backspace more intuitive
  (kd isearch-mode-map
      '("<backspace>"	isearch-del-char)

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

(setup-after "replace" ;; occur
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
    )

;; debug
(kd debug-prefix-map
    '("d"	edebug-defun)
    '("g"	profiler-stop)
    '("r"	profiler-report)
    '("s"	profiler-start)
    )

(setup-after "yasnippet"
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
(setup-after "auto-complete-config"
  (setq ac-use-menu-map nil)
  (ac-set-trigger-key "C-t")

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
      '("C-t"	ac-next)
      '("M-t"	ac-previous)
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
    '("i"  	iedit-mode)
    '("I"  	iedit-mode-toggle-on-function)
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

(setup-after "multiple-cursors-core"
  ;; <ret> inserts a newline; C-j exits (a bit more convenient that way)
  (kd mc/keymap
      '("<return>"	nil)
      '("C-j"     	multiple-cursors-mode)

      '("C-p h"	mc-hide-unmatched-lines-mode)
      ))

;; handle comments
(setup-after "cc-mode"
  (kd c-mode-map
      '("M-RET"	c-indent-new-comment-line)
      ))

;; aligning things
(kd align-prefix-map
    '("SPC"	align-repeat)
    '("a"  	align-region-or-current)
    '("r"  	align-repeat)
    '("t"  	delimit-columns-current)
    '("T"  	delimit-columns-region)
    '("w"  	align-whitespace)
    )

;; spell-check
(kd spell-check-prefix-map
    '("SPC"	wcheck-actions)
    '("d"  	disable-spell-check)
    '("e"  	enable-spell-check)
    '("w"  	wcheck-mode)
    )

;; input methods
(kd input-prefix-map
    '("SPC"	toggle-input-method)
    '("0"  	clear-input-method)
    '("c"  	set-input-method-muflax-cyrillic)
    '("g"  	set-input-method-muflax-greek)
    '("j"  	set-input-method-japanese-mozc)
    '("l"  	set-input-method-muflax-latin)
    '("m"  	set-input-method-muflax-latin)
    '("s"  	toggle-subword-mode )
    '("t"  	set-input-method-muflax-turkish)
    )

;; window config
(kd window-prefix-map
    '("<down>"  	split-window-below)
    '("<left>"  	split-window-left)
    '("<right>" 	split-window-right)
    '("<up>"    	split-window-above)
    '("<return>"	sticky-window-delete-other-windows)
    '("C-r"     	sticky-window-delete-other-windows)
    '("S-SPC"   	kill-buffer-and-window)
    '("SPC"     	sticky-window-delete-window)
    '("="       	balance-windows)
    '("b"       	winner-undo)
    '("f"       	winner-redo)
    '("k"       	sticky-window-delete-other-windows)
    '("n"       	neotree-toggle)
    '("s"       	scratch)
    '("v"       	sticky-window-keep-window-visible)
    '("w"       	sticky-window-delete-window)
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
    '("C-f"	fold-dwim-toggle)
    '("C-y"	yafolding-toggle-element)
    '("SPC"	hs-show-block)
    '("f"  	hs-fold-levels)
    '("F"  	fold-dwim-show-all)
    '("s"  	whitespace-fold-levels)
    '("S"  	whitespace-fold-reset)
    '("y"  	yafolding-hide-all)
    '("Y"  	yafolding-show-all)
    '("w"  	whitespace-mode)
    )

;; smartparens
(kd sexp-prefix-map
    '("C-<tab>"	sp-indent-adjust-sexp)
    '("<tab>"  	sp-dedent-adjust-sexp)
    '("C-a"    	sp-kill-to-beginning-of-sexp)
    '("C-e"    	sp-kill-to-end-of-sexp)
    '("M-a"    	sp-copy-to-beginning-of-sexp)
    '("M-e"    	sp-copy-to-end-of-sexp)
    '("|"      	sp-split-sexp)
    '("a"      	sp-beginning-of-sexp)
    '("e"      	sp-end-of-sexp)
    '("j"      	sp-join-sexp)
    '("k"      	sp-kill-sexp)
    '("r"      	sp-rewrap-sexp)
    '("u"      	sp-unwrap-sexp)
    '("U"      	sp-backward-unwrap-sexp)
    '("w"      	sp-copy-sexp)
    )

;; narrowing
(kd narrow-map
    '("SPC"	narrow-or-widen-dwim)
    '("d"  	narrow-to-defun)
    '("n"  	narrow-to-region)
    '("p"  	narrow-to-page)
    '("s"  	sp-narrow-to-sexp)
    '("w"  	widen)
    )

;; macro
(kd macro-prefix-map
    '("a"	kmacro-add-counter)
    '("b"	kmacro-bind-to-key)
    '("c"	kmacro-set-counter)
    '("e"	edit-kbd-macro)
    '("l"	kmacro-edit-lossage)
    '("m"	kmacro-end-or-call-macro-repeat)
    '("n"	kmacro-name-last-macro)
    '("s"	kmacro-start-macro-or-insert-counter)
    '("t"	insert-kbd-macro)
    )

;; helm
(setup-after "helm"
  (kd helm-map
      '("C-w"	subword-backward-kill)
      '("M-w"	helm-yank-text-at-point)
      ))

;; (kd global-map
;;     '("M-x"	helm-M-x)
;;     )

(kd helm-prefix-map
    '("C-o"	helm-swoop)
    '("e"  	helm-flycheck)
    '("f"  	helm-find-files)
    '("g"  	helm-do-grep)
    '("o"  	helm-occur)
    '("t"  	helm-cmd-t)
    )

;; org-mode
(setup-after "org"
  (org-defkey org-mode-map (kbd "C-c C-d")	'org-todo-done)
  (org-defkey org-mode-map (kbd "C-c C-t")	'org-todo-todo)
  (org-defkey org-mode-map (kbd "C-c C-w")	'org-todo-waiting))

;; haskell
(setup-after "haskell-mode"
  (kd haskell-mode-map
      '("C-c ?"  	haskell-process-do-type)
      '("C-c C-?"	haskell-process-do-info)))

;; ruby
(setup-after "enh-ruby-mode"
  (kd enh-ruby-mode-map
      '("C-c ?"	yari)))

(setup-after "dired"
  (setup-after "wdired"
    (kd dired-mode-map
        '("C-c C-c" 	wdired-change-to-wdired-mode)
        '("<insert>"	dired-mark)
        '("."       	dired-omit-mode)

        ;; C-a goes to filename
        '(	"C-a"	dired-back-to-start-of-files)
        )

    (kd wdired-mode-map
        '("C-a"	dired-back-to-start-of-files)
        )

    ;; M-up goes to first file
    (define-key dired-mode-map 	[remap beginning-of-buffer]	'dired-back-to-top)
    (define-key wdired-mode-map	[remap beginning-of-buffer]	'dired-back-to-top)
    (define-key dired-mode-map 	[remap smart-up]           	'dired-back-to-top)

    ;; M-down goes to last file
    (define-key dired-mode-map 	[remap end-of-buffer]	'dired-jump-to-bottom)
    (define-key dired-mode-map 	[remap smart-down]   	'dired-jump-to-bottom)
    (define-key wdired-mode-map	[remap end-of-buffer]	'dired-jump-to-bottom)))

;; golang
(setup-after "go-mode"
  (kd go-mode-map
      '("M-t"	godef-jump)
      '("M-T"	godef-jump-other-window)))

(setup-after "magit"
  ;; needed because of fullscreen override
  (kd magit-status-mode-map
      '("q"	magit-quit-session)
      '("W"	magit-toggle-whitespace)))

;; rectangle selection
(kd rectangle-prefix-map
    '("M-w"	copy-rectangle-as-kill)
    '("c"  	clear-rectangle)
    '("d"  	delete-rectangle)
    '("k"  	kill-rectangle)
    '("n"  	rectangle-number-lines)
    '("o"  	open-rectangle)
    '("r"  	copy-rectangle-to-register)
    '("t"  	string-rectangle)
    '("y"  	yank-rectangle)
    )

;; registers (still unused)
(kd register-prefix-map
    '("SPC"	point-to-register)
    '("+"  	increment-register)
    '("b"  	bookmark-jump)
    '("f"  	frame-configuration-to-register)
    '("g"  	insert-register)
    '("i"  	insert-register)
    '("j"  	jump-to-register)
    '("l"  	bookmark-bmenu-list)
    '("m"  	bookmark-set)
    '("n"  	number-to-register)
    '("s"  	copy-to-register)
    '("w"  	window-configuration-to-register)
    '("x"  	copy-to-register)
    )

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
(setup-after "lisp-mode"
  ;; operate on sexps
  (kd lisp-mode-shared-map
      ;; TODO these might want to be remaps instead for consistency

      ;; '("C-<return>" md/duplicate-down)

      '("C-("	sp-down-sexp)
      '("C-)"	sp-up-sexp)

      '("C-k"	sp-kill-hybrid-sexp)

      '("M-a"	sp-beginning-of-sexp)
      '("M-b"	sp-forward-sexp)
      '("M-e"	sp-end-of-sexp)
      '("M-f"	sp-backward-sexp)

      '("C-;"	sp-comment)
      '("C-<"	sp-dedent-adjust-sexp)
      '("C->"	sp-indent-adjust-sexp)
      '("C-^"	sp-raise-sexp)

      '("<return>"	sp-newline)


      ;; sp-kill-to-beginning-of-sexp
      ;; sp-kill-to-end-of-sexp
      ;; sp-copy-to-beginning-of-sexp
      ;; sp-copy-to-end-of-sexp

      ))

;; slime
(setup-after "slime"
  ;; disable slime default keys
  (setq slime-parent-bindings	nil
        slime-prefix-bindings	nil
        slime-editing-keys   	nil
        slime-keys           	nil
        slime-doc-bindings   	nil
        slime-who-bindings   	nil)

  ;; (re-)setup keymaps
  (slime-init-keymaps)

  ;; define actual keys
  (kd slime-doc-map
      '()
      ;; '((?a slime-apropos)
      ;;   (?z slime-apropos-all)
      ;;   (?p slime-apropos-package)
      ;;   (?d slime-describe-symbol)
      ;;   (?f slime-describe-function)
      ;;   (?h slime-documentation-lookup)
      ;;   (?~ common-lisp-hyperspec-format)
      ;;   (?# common-lisp-hyperspec-lookup-reader-macro)))
      )
  (kd slime-who-map
      '()
      ;; '((?c slime-who-calls)
      ;;   (?w slime-calls-who)
      ;;   (?r slime-who-references)
      ;;   (?b slime-who-binds)
      ;;   (?s slime-who-sets)
      ;;   (?m slime-who-macroexpands)
      ;;   (?a slime-who-specializes)))
      )
  (kd slime-prefix-map
      '()
      ;; '(("\C-r"  slime-eval-region)
      ;;   (":"     slime-interactive-eval)
      ;;   ("\C-e"  slime-interactive-eval)
      ;;   ("E"     slime-edit-value)
      ;;   ("\C-l"  slime-load-file)
      ;;   ("\C-b"  slime-interrupt)
      ;;   ("\M-d"  slime-disassemble-symbol)
      ;;   ("\C-t"  slime-toggle-trace-fdefinition)
      ;;   ("I"     slime-inspect)
      ;;   ("\C-xt" slime-list-threads)
      ;;   ("\C-xn" slime-cycle-connections)
      ;;   ("\C-xc" slime-list-connections)
      ;;   ("<"     slime-list-callers)
      ;;   (">"     slime-list-callees)
      ;;   ;; Include DOC keys...
      ;;   ("\C-d"  slime-doc-map)
      ;;   ;; Include XREF WHO-FOO keys...
      ;;   ("\C-w"  slime-who-map)
      )
  (kd slime-parent-map
      '()
      ;; '(("\M-."      slime-edit-definition)
      ;;   ("\M-,"      slime-pop-find-definition-stack)
      ;;   ("\M-_"      slime-edit-uses)    ; for German layout
      ;;   ("\M-?"      slime-edit-uses)    ; for USian layout
      ;;   ("\C-x4."	 slime-edit-definition-other-window)
      ;;   ("\C-x5."	 slime-edit-definition-other-frame)
      ;;   ("\C-x\C-e"  slime-eval-last-expression)
      ;;   ("\C-\M-x"   slime-eval-defun)
      ;;   ;; Include PREFIX keys...
      ;;   ("\C-c"	 slime-prefix-map)))

      )
  (kd slime-editing-map
      '()
      ;; ("\M-\t"      slime-complete-symbol)
      ;; (" "          slime-space)
      ;; ;; Evaluating
      ;; ;;("\C-x\M-e" slime-eval-last-expression-display-output :inferior t)
      ;; ("\C-c\C-p"   slime-pprint-eval-last-expression)
      ;; ;; Macroexpand
      ;; ("\C-c\C-m"   slime-expand-1)
      ;; ("\C-c\M-m"   slime-macroexpand-all)
      ;; ;; Misc
      ;; ("\C-c\C-u"   slime-undefine-function)
      ;; (,(kbd "C-M-.")   slime-next-location)
      ;; (,(kbd "C-M-,")   slime-previous-location)
      ;; ;; Obsolete, redundant bindings
      ;; ("\C-c\C-i" slime-complete-symbol)
      ;; ;;("\M-*" pop-tag-mark) ; almost to clever

      )
  (kd slime-mode-map
      '()
      ;; ("\M-p"       slime-previous-note)
      ;; ("\M-n"       slime-next-note)
      ;; ("\C-c\M-c"   slime-remove-notes)
      ;; ("\C-c\C-k"   slime-compile-and-load-file)
      ;; ("\C-c\M-k"   slime-compile-file)
      ;; ("\C-c\C-c"   slime-compile-defun)))
      )
  )

;; sp-insert-pair
;; sp-next-sexp
;; sp-previous-sexp

;; sp-splice-sexp-killing-backward
;; sp-splice-sexp-killing-forward

;; sp-forward-slurp-sexp
;; sp-forward-barf-sexp

;; sp-absorb-sexp
;; sp-emit-sexp

;; sp-extract-before-sexp
;; sp-extract-after-sexp

;; sp-indent-defun

;; fonts
(kd font-prefix-map
    '("<f1>"	use-small-font)
    '("<f2>"	use-normal-font)
    '("<f3>"	use-big-font)
    '("<f4>"	use-huge-font)
    )

(provide 'init-keys)
