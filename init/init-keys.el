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
      '("<f1>"  "<f6>"  "<f11>" "<f16>"
        "<f2>"  "<f7>"  "<f12>" "<f17>"
        "<f3>"  "<f8>"  "<f13>" "<f18>"
        "<f4>"  "<f9>"  "<f14>" "<f19>"
        "<f5>"  "<f10>" "<f15>" "<f20>"

        "<mouse-1>" "<down-mouse-1>" "<drag-mouse-1>" "<double-mouse-1>"
        "<mouse-2>" "<down-mouse-2>" "<drag-mouse-2>" "<double-mouse-2>"
        "<mouse-3>" "<down-mouse-3>" "<drag-mouse-3>" "<double-mouse-3>"
        "<mouse-4>" "<down-mouse-4>" "<drag-mouse-4>" "<double-mouse-4>"
        "<mouse-5>" "<down-mouse-5>" "<drag-mouse-5>" "<double-mouse-5>"
        "<mouse-6>" "<down-mouse-6>" "<drag-mouse-6>" "<double-mouse-6>"
        "<mouse-7>" "<down-mouse-7>" "<drag-mouse-7>" "<double-mouse-7>"
        "<mouse-8>" "<down-mouse-8>" "<drag-mouse-8>" "<double-mouse-8>"
        "<mouse-9>" "<down-mouse-9>" "<drag-mouse-9>" "<double-mouse-9>"

        "<up>" "<down>" "<left>" "<right>"
        "<next>" "<prior>" "<home>" "<end>"
        "<backspace>" "<delete>" "<insert>" "<insertchar>"
        "<ret>" "<return>" "<tab>" "<escape>"
        "<menu>" "<kanji>"

        "SPC" "TAB" "DEL" "ESC"
        ))

(setq assignable-normal-modifiers
      ;; valid for all keys
      '("C-" "C-S-"
        "M-" "M-S-"
        "C-M-" "C-M-S-"))

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

(setup-after "python"        (unbreak-stupid-map python-mode-map))
(setup-after "enh-ruby-mode" (unbreak-stupid-map enh-ruby-mode-map))
(setup-after "go-mode"       (unbreak-stupid-map go-mode-map))
(setup-after "flycheck"      (unbreak-stupid-map flycheck-mode-map))
(setup-after "conf-mode"     (unbreak-stupid-map conf-mode-map))

;; fix mod4 bug
(define-key special-event-map (kbd "<key-17>")   'ignore)
(define-key special-event-map (kbd "<M-key-17>") 'ignore)

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
(key-def global-map "C-S-<mouse-1>"    'mc/add-cursor-on-click)
(key-def global-map "C-<down-mouse-1>" 'mc/add-cursor-on-click)
(key-def global-map "<double-mouse-1>" 'mouse-set-point)
(key-def global-map "<down-mouse-1>"   'mouse-drag-region)
(key-def global-map "<drag-mouse-1>"   'mouse-set-region)
(key-def global-map "<mouse-1>"        'mouse-set-point)
(key-def global-map "<mouse-2>"        'mouse-yank-primary)
(key-def global-map "<mouse-3>"        'fold-dwim-toggle)
(key-def global-map "<mouse-4>"        'mwheel-scroll)
(key-def global-map "<mouse-5>"        'mwheel-scroll)

;; navigation keys
(key-def global-map "C-<down>"  'mc/mark-next-like-this)
(key-def global-map "C-<left>"  'er/contract-region)
(key-def global-map "C-<right>" 'er/expand-region)
(key-def global-map "C-<up>"    'mc/mark-previous-like-this)

(key-def global-map "C-<end>"   'end-of-buffer)
(key-def global-map "C-<home>"  'beginning-of-buffer)
(key-def global-map "C-<next>"  'dired-next)
(key-def global-map "C-<prior>" 'dired-prev)

(key-def global-map "M-<down>"  'md/move-lines-down)
(key-def global-map "M-<left>"  'er/mark-symbol)
(key-def global-map "M-<right>" 'er/mark-defun)
(key-def global-map "M-<up>"    'md/move-lines-up)

(key-def global-map "M-<next>"  'scroll-other-window)
(key-def global-map "M-<prior>" 'scroll-other-window-down)

(key-def global-map "C-M-<down>"  'buf-move-down)
(key-def global-map "C-M-<left>"  'buf-move-left)
(key-def global-map "C-M-<right>" 'buf-move-right)
(key-def global-map "C-M-<up>"    'buf-move-up)

(key-def global-map "<down>"  'next-line)
(key-def global-map "<left>"  'left-char)
(key-def global-map "<right>" 'right-char)
(key-def global-map "<up>"    'previous-line)

(key-def global-map "<end>"    'end-of-buffer)
(key-def global-map "<home>"   'beginning-of-buffer)
(key-def global-map "<next>"   'scroll-up-command)
(key-def global-map "<prior>"  'scroll-down-command)

;; special keys
(key-def global-map "C-<backspace>" 'backward-kill-word)
(key-def global-map "C-<delete>"    'kill-word)
(key-def global-map "C-<menu>"      'nav-global-mode)
(key-def global-map "C-<return>"    'md/duplicate-down)
(key-def global-map "C-<tab>"       'literal-tab)

(key-def global-map "C-SPC" 'set-mark-command)

(key-def global-map "M-<backspace>" 'sp-unwrap-sexp)
(key-def global-map "M-<delete>"    'sp-unwrap-sexp)

(key-def global-map "<f1>"        'help-command)
(key-def global-map "<f2>"        'save-buffer)
(key-def global-map "<f3>"        'kmacro-start-macro-or-insert-counter)
(key-def global-map "<f4>"        'kmacro-end-or-call-macro)
(key-def global-map "<f11>"       'automargin-mode)
(key-def global-map "<backspace>" 'delete-backward-char)
(key-def global-map "<delete>"    'delete-char) ; make DEL always work like intended
(key-def global-map "<kanji>"     'toggle-input-method)
(key-def global-map "<menu>"      'nav-minor-mode)

(key-def global-map "S-<f2>"         'save-some-buffers)
(key-def global-map "S-<f7>"         'backward-kill-word) ; make C-Backspace "work" in terminal
(key-def global-map "S-<backspace>"  'literal-delete-backward-char)
(key-def global-map "S-<delete>"     'literal-delete-char)
(key-def global-map "S-<insert>"     'whole-line-or-region-yank)
(key-def global-map "S-<insertchar>" 'whole-line-or-region-yank)

(key-def global-map "RET"   'newline)
(key-def global-map "SPC"   'self-insert-command)
(key-def global-map "S-SPC" 'set-mark-command)
(key-def global-map "TAB"   'indent-for-tab-command)

;; punctuation keys
(key-def global-map "C-("  'sp-narrow-to-sexp)
(key-def global-map "C-)"  'widen)
(key-def global-map "C--"  'negative-argument)
(key-def global-map "C-\\" 'generalized-shell-command) ; terminal bug, same as C-|
(key-def global-map "C-|"  'generalized-shell-command)

(key-def global-map "M--" 'negative-argument)
(key-def global-map "M-^" 'delete-indentation)
(key-def global-map "M-|" 'shell-command-on-region)

;; numbers
(key-def global-map "C-0"  'digit-argument)
(key-def global-map "C-1"  'digit-argument)
(key-def global-map "C-2"  'digit-argument)
(key-def global-map "C-3"  'digit-argument)
(key-def global-map "C-4"  'digit-argument)
(key-def global-map "C-5"  'digit-argument)
(key-def global-map "C-6"  'digit-argument)
(key-def global-map "C-7"  'digit-argument)
(key-def global-map "C-8"  'digit-argument)
(key-def global-map "C-9"  'digit-argument)
(key-def global-map "M-0"  'digit-argument)
(key-def global-map "M-1"  'digit-argument)
(key-def global-map "M-2"  'digit-argument)
(key-def global-map "M-3"  'digit-argument)
(key-def global-map "M-4"  'digit-argument)
(key-def global-map "M-5"  'digit-argument)
(key-def global-map "M-6"  'digit-argument)
(key-def global-map "M-7"  'digit-argument)
(key-def global-map "M-8"  'digit-argument)
(key-def global-map "M-9"  'digit-argument)

;; alphabet
(key-def global-map "C-a" 'smart-beginning-of-line)
(key-def global-map "C-b" 'backward-word)
(key-def global-map "C-c" 'mode-specific-command-prefix)
(key-def global-map "C-d" 'kill-without-append)
(key-def global-map "C-e" 'smart-end-of-line)
(key-def global-map "C-f" 'forward-word)
(key-def global-map "C-g" 'keyboard-quit) ; also change quit-char if you wanna move it
(key-def global-map "C-h" 'help-command)
(key-def global-map "C-i" 'indent-for-tab-command 'terminal "TAB")
(key-def global-map "C-j" 'newline-and-indent)
(key-def global-map "C-k" 'kill-and-join-forward)
(key-def global-map "C-l" 'recenter-top-bottom)
(key-def global-map "C-m" 'newline 'terminal "RET")
(key-def global-map "C-n" 'focus-next-window)
(key-def global-map "C-N" 'focus-prev-window)
(key-def global-map "C-o" 'yas-expand)
(key-def global-map "C-O" 'next-newline-and-indent)
(key-def global-map "C-p" 'mc-prefix-map      'prefix)
(key-def global-map "C-q" 'quoted-insert)
(key-def global-map "C-r" 'window-prefix-map  'prefix)
(key-def global-map "C-s" 'search-prefix-map  'prefix)
(key-def global-map "C-t" 'ac-trigger-key-command)
(key-def global-map "C-u" 'universal-argument)
(key-def global-map "C-v" 'folding-prefix-map 'prefix)
(key-def global-map "C-w" 'kill-region)
(key-def global-map "C-x" 'Control-X-prefix)
(key-def global-map "C-y" 'yank-and-indent)
(key-def global-map "C-Y" 'yank)
(key-def global-map "C-z" 'undo-tree-undo)

(key-def global-map "M-b" 'sp-backward-symbol)
(key-def global-map "M-d" 'blank-line)
(key-def global-map "M-f" 'sp-forward-symbol)
(key-def global-map "M-h" 'mark-paragraph)
(key-def global-map "M-j" 'indent-new-comment-line)
(key-def global-map "M-k" 'copy-line)
(key-def global-map "M-n" 'undo-tree-undo)
(key-def global-map "M-o" 'yas-insert-snippet)
(key-def global-map "M-p" 'undo-tree-redo)
(key-def global-map "M-q" 'fill-region)
(key-def global-map "M-Q" 'unfill-region)
(key-def global-map "M-t" 'find-tag)
(key-def global-map "M-w" 'kill-ring-save)
(key-def global-map "M-x" 'smex)
(key-def global-map "M-X" 'smex-major-mode-commands)
(key-def global-map "M-y" 'yank-pop)
(key-def global-map "M-Y" 'yank-pop-reverse)
(key-def global-map "M-z" 'undo-tree-redo)

;; less commonly used functions
(key-def ctl-x-map "C-+" 'text-scale-adjust)
(key-def ctl-x-map "C--" 'text-scale-adjust)
(key-def ctl-x-map "C-=" 'text-scale-adjust)
(key-def ctl-x-map "C-0" 'text-scale-adjust)
(key-def ctl-x-map "C-b" 'list-buffers)
(key-def ctl-x-map "C-c" 'save-buffers-kill-terminal)
(key-def ctl-x-map "C-f" 'find-file)
(key-def ctl-x-map "C-r" 'recentf-ido-find-file)
(key-def ctl-x-map "C-s" 'save-buffer)
(key-def ctl-x-map "C-v" 'find-alternate-file)
(key-def ctl-x-map "C-w" 'write-file)
(key-def ctl-x-map "C-x" 'exchange-point-and-mark)
(key-def ctl-x-map "M-f" 'find-file-at-point)
(key-def ctl-x-map "SPC" 'eval-prefix-map        'prefix)
(key-def ctl-x-map "#"   'server-edit)
(key-def ctl-x-map "+"   'balance-windows)
(key-def ctl-x-map "a"   'align-prefix-map       'prefix)
(key-def ctl-x-map "b"   'switch-to-buffer)
(key-def ctl-x-map "c"   'case-prefix-map        'prefix)
(key-def ctl-x-map "d"   'debug-prefix-map       'prefix)
(key-def ctl-x-map "g"   'magit-status)
(key-def ctl-x-map "h"   'mark-whole-buffer)
(key-def ctl-x-map "k"   'kill-buffer)
(key-def ctl-x-map "m"   'macro-prefix-map       'prefix)
(key-def ctl-x-map "n"   'narrow-map)
(key-def ctl-x-map "p"   'paradox-list-packages)
(key-def ctl-x-map "r"   'rectangle-prefix-map   'prefix)
(key-def ctl-x-map "R"   'register-prefix-map    'prefix)
(key-def ctl-x-map "s"   'save-some-buffers)
(key-def ctl-x-map "t"   'input-prefix-map       'prefix)
(key-def ctl-x-map "w"   'spell-check-prefix-map 'prefix)
(key-def ctl-x-map "x"   'helm-prefix-map        'prefix)
(key-def ctl-x-map "z"   'repeat)
(key-def ctl-x-map "Z"   'repeat-complex-command)

;; mode-specific stuff
(key-def mode-specific-map "<f1>" 'use-small-font)
(key-def mode-specific-map "<f2>" 'use-normal-font)
(key-def mode-specific-map "<f3>" 'use-big-font)
(key-def mode-specific-map "<f4>" 'use-huge-font)

(key-def mode-specific-map "C-<tab>" 'sp-indent-adjust-sexp)
(key-def mode-specific-map "<tab>"   'sp-dedent-adjust-sexp)

(key-def mode-specific-map "C-~"   'aya-expand)
(key-def mode-specific-map "C-SPC" 'comment-dwim)
(key-def mode-specific-map "C-g"   'abort-recursive-edit)
(key-def mode-specific-map "C-j"   'dired-jump)
(key-def mode-specific-map "C-o"   'yas-reload-all)
(key-def mode-specific-map "C-t"   'rotate-text)
(key-def mode-specific-map "C-w"   'kill-with-append)

(key-def mode-specific-map "SPC" 'comment-dwim)
(key-def mode-specific-map "~"   'aya-create)
(key-def mode-specific-map "c"   'comment-region)
(key-def mode-specific-map "i"   'indent-region)
(key-def mode-specific-map "m"   'number-prefix-map 'prefix)
(key-def mode-specific-map "n"   'next-error)
(key-def mode-specific-map "p"   'previous-error)
(key-def mode-specific-map "s"   'sexp-prefix-map   'prefix)
(key-def mode-specific-map "u"   'uncomment-region)
(key-def mode-specific-map "w"   'copy-with-append)

;; search
(key-def search-prefix-map "C-r" 'isearch-backward-use-region)
(key-def search-prefix-map "C-s" 'isearch-forward-use-region)
(key-def search-prefix-map "SPC" 'er/mark-defun)
(key-def search-prefix-map "*"   'isearch-forward-symbol)
(key-def search-prefix-map "b"   'isearch-backward-regexp)
(key-def search-prefix-map "B"   'isearch-backward-use-region)
(key-def search-prefix-map "d"   'er/mark-defun)
(key-def search-prefix-map "g"   'jump-prefix-map 'prefix)
(key-def search-prefix-map "i"   'idomenu)
(key-def search-prefix-map "I"   'imenu-anywhere)
(key-def search-prefix-map "o"   'occur)
(key-def search-prefix-map "p"   'phi-search)
(key-def search-prefix-map "p"   'phi-search)
(key-def search-prefix-map "P"   'phi-search-backward)
(key-def search-prefix-map "r"   'vr/query-replace)
(key-def search-prefix-map "R"   'vr/query-replace-from-beginning)
(key-def search-prefix-map "s"   'isearch-forward-use-region)
(key-def search-prefix-map "S"   'isearch-forward-regexp)
(key-def search-prefix-map "w"   'er/mark-symbol)
(key-def search-prefix-map "y"   'kill-ring-search)
(key-def search-prefix-map "["   'idomenu)
(key-def search-prefix-map "]"   'imenu-anywhere)

;; ace-jump
(key-def jump-prefix-map "b" 'ace-jump-buffer)
(key-def jump-prefix-map "c" 'ace-jump-char-mode)
(key-def jump-prefix-map "g" 'ace-jump-mode)
(key-def jump-prefix-map "l" 'ace-jump-line-mode)
(key-def jump-prefix-map "n" 'goto-line)
(key-def jump-prefix-map "w" 'ace-window)

;; with active search
(setup-after "phi-search"
  (setup "phi-search-mc"
    (key-def phi-search-default-map "C-<down>"   'phi-search-mc/mark-next)
    (key-def phi-search-default-map "C-<up>"     'phi-search-mc/mark-previous)
    (key-def phi-search-default-map "C-<return>" 'phi-search-mc/mark-here)
    (key-def phi-search-default-map "C-p SPC"    'phi-search-mc/mark-all)
    ))

(setup-after "isearch"
  ;; make backspace more intuitive
  (key-def isearch-mode-map "<backspace>" 'isearch-del-char)

  (key-def isearch-mode-map "C-c C-c" 'isearch-normalize-string)
  (key-def isearch-mode-map "C-c SPC" 'isearch-toggle-lax-whitespace)
  (key-def isearch-mode-map "C-c c"   'isearch-toggle-case-fold)
  (key-def isearch-mode-map "C-c i"   'isearch-toggle-case-fold)
  (key-def isearch-mode-map "C-c o"   'isearch-occur)
  (key-def isearch-mode-map "C-c r"   'isearch-toggle-regexp)
  (key-def isearch-mode-map "C-c s"   'isearch-toggle-symbol)
  (key-def isearch-mode-map "C-c w"   'isearch-toggle-word)

  (key-def isearch-mode-map "C-r"  'isearch-repeat-backward)
  (key-def isearch-mode-map "C-s"  'isearch-repeat-forward)
  (key-def isearch-mode-map "C-w"  'isearch-yank-word-or-char)
  (key-def isearch-mode-map "C-y"  'isearch-yank-kill)
  (key-def isearch-mode-map "C-\\" 'isearch-toggle-input-method)
  )

(setup-after "replace" ;; occur
  (key-def occur-mode-map "C-c C-c" 'occur-edit-mode)
  (key-def occur-mode-map "C-c C-f" 'next-error-follow-minor-mode)

  (key-def occur-mode-map "C-o" 'occur-mode-display-occurrence)
  (key-def occur-mode-map "RET" 'occur-mode-goto-occurrence)
  (key-def occur-mode-map "c"   'clone-buffer)
  (key-def occur-mode-map "e"   'occur-edit-mode)
  (key-def occur-mode-map "n"   'occur-next)
  (key-def occur-mode-map "o"   'occur-mode-goto-occurrence-other-window)
  (key-def occur-mode-map "p"   'occur-prev)
  (key-def occur-mode-map "r"   'occur-rename-buffer)
  )

;; eval
(key-def eval-prefix-map "SPC" 'eval-defun)
(key-def eval-prefix-map "b"   'eval-buffer)
(key-def eval-prefix-map "d"   'eval-defun)
(key-def eval-prefix-map "e"   'eval-expression)
(key-def eval-prefix-map "r"   'eval-region)

;; debug
(key-def debug-prefix-map "d" 'edebug-defun)
(key-def debug-prefix-map "g" 'profiler-stop)
(key-def debug-prefix-map "r" 'profiler-report)
(key-def debug-prefix-map "s" 'profiler-start)

(setup-after "yasnippet"
  ;; saner trigger key
  (define-key yas-minor-mode-map [(tab)]     nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (key-def yas-keymap "<return>" 'yas/exit-all-snippets)
  (key-def yas-keymap "C-a"      'yas/goto-start-of-active-field)
  (key-def yas-keymap "C-e"      'yas/goto-end-of-active-field)
  (key-def yas-keymap "C-o"      'yas-next-field-or-maybe-expand)
  (key-def yas-keymap "C-O"      'yas-next-field)
  )

;; auto-completion
(setup-after "auto-complete-config"
  (setq ac-use-menu-map nil)
  (ac-set-trigger-key "C-t")

  ;; unset stupid keys
  (define-key ac-completing-map "\t"          nil)
  (define-key ac-completing-map [tab]         nil)
  (define-key ac-completing-map (kbd "<Tab>") nil)
  (define-key ac-completing-map [up]          nil)
  (define-key ac-completing-map [down]        nil)
  (define-key ac-completing-map (kbd "M-n")   nil)
  (define-key ac-completing-map (kbd "M-p")   nil)
  (define-key ac-completing-map [return]      nil)
  (define-key ac-completing-map "\r"          nil)

  (key-def ac-completing-map "C-t" 'ac-next)
  (key-def ac-completing-map "M-t" 'ac-previous)
  (key-def ac-completing-map "C-j" 'ac-complete)
  )

;; multiple cursors
(key-def mc-prefix-map "SPC" 'mc/mark-all-dwim)
(key-def mc-prefix-map "b"   'mc/skip-to-previous-like-this)
(key-def mc-prefix-map "c"   'mc/compare-chars-forward)
(key-def mc-prefix-map "C"   'mc/compare-chars-backward)
(key-def mc-prefix-map "d"   'mc/remove-duplicated-cursors)
(key-def mc-prefix-map "f"   'mc/skip-to-next-like-this)
(key-def mc-prefix-map "g"   'mc-jump-char)
(key-def mc-prefix-map "i"   'iedit-mode)
(key-def mc-prefix-map "I"   'iedit-mode-toggle-on-function)
(key-def mc-prefix-map "l"   'mc/edit-lines)
(key-def mc-prefix-map "m"   'mc/mark-more-like-this-extended)
(key-def mc-prefix-map "n"   'mc/insert-numbers)
(key-def mc-prefix-map "R"   'mc/reverse-regions)
(key-def mc-prefix-map "r"   'set-rectangular-region-anchor)
(key-def mc-prefix-map "s"   'mc/sort-regions)
(key-def mc-prefix-map "u"   'mc/remove-current-cursor)
(key-def mc-prefix-map "y"   'mc/many-to-one-yank-indent)
(key-def mc-prefix-map "Y"   'mc/many-to-one-yank)

(setup-after "multiple-cursors-core"
  ;; <ret> inserts a newline; C-j exits (a bit more convenient that way)
  (key-def mc/keymap "<return>" nil)
  (key-def mc/keymap "C-j"      'multiple-cursors-mode)

  (key-def mc/keymap "C-p h" 'mc-hide-unmatched-lines-mode)
  )

;; handle comments
(setup-after "cc-mode"
  (key-def c-mode-map "M-RET" 'c-indent-new-comment-line)
  )

;; aligning things
(key-def align-prefix-map "SPC" 'align-repeat)
(key-def align-prefix-map "a"   'align-region-or-current)
(key-def align-prefix-map "r"   'align-repeat)
(key-def align-prefix-map "t"   'delimit-columns-current)
(key-def align-prefix-map "T"   'delimit-columns-region)
(key-def align-prefix-map "w"   'align-whitespace)

;; spell-check
(key-def spell-check-prefix-map "SPC" 'wcheck-actions)
(key-def spell-check-prefix-map "d"   'disable-spell-check)
(key-def spell-check-prefix-map "e"   'enable-spell-check)
(key-def spell-check-prefix-map "w"   'wcheck-mode)

;; input methods
(key-def input-prefix-map "SPC" 'toggle-input-method)
(key-def input-prefix-map "0"   'clear-input-method)
(key-def input-prefix-map "c"   'set-input-method-muflax-cyrillic)
(key-def input-prefix-map "g"   'set-input-method-muflax-greek)
(key-def input-prefix-map "j"   'set-input-method-japanese-mozc)
(key-def input-prefix-map "l"   'set-input-method-muflax-latin)
(key-def input-prefix-map "m"   'set-input-method-muflax-latin)
(key-def input-prefix-map "s"   'toggle-subword-mode )
(key-def input-prefix-map "t"   'set-input-method-muflax-turkish)

;; window config
(key-def window-prefix-map "<down>"   'split-window-below)
(key-def window-prefix-map "<left>"   'split-window-left)
(key-def window-prefix-map "<right>"  'split-window-right)
(key-def window-prefix-map "<up>"     'split-window-above)
(key-def window-prefix-map "<return>" 'sticky-window-delete-other-windows)
(key-def window-prefix-map "C-r"      'sticky-window-delete-other-windows)
(key-def window-prefix-map "S-SPC"    'kill-buffer-and-window)
(key-def window-prefix-map "SPC"      'sticky-window-delete-window)
(key-def window-prefix-map "="        'balance-windows)
(key-def window-prefix-map "b"        'winner-undo)
(key-def window-prefix-map "f"        'winner-redo)
(key-def window-prefix-map "k"        'sticky-window-delete-other-windows)
(key-def window-prefix-map "n"        'neotree-toggle)
(key-def window-prefix-map "s"        'scratch)
(key-def window-prefix-map "v"        'sticky-window-keep-window-visible)
(key-def window-prefix-map "w"        'sticky-window-delete-window)

;; arithmetic
(key-def number-prefix-map "<down>" 'number/decrement)
(key-def number-prefix-map "<up>"   'number/increment)
(key-def number-prefix-map "+"      'number/add)
(key-def number-prefix-map "-"      'number/sub)
(key-def number-prefix-map "*"      'number/multiply)
(key-def number-prefix-map "/"      'number/divide)
(key-def number-prefix-map "="      'number/eval)
(key-def number-prefix-map "0"      'number/pad)
(key-def number-prefix-map "a"      'number/add)
(key-def number-prefix-map "d"      'number/divide)
(key-def number-prefix-map "e"      'number/eval)
(key-def number-prefix-map "m"      'number/multiply)
(key-def number-prefix-map "p"      'number/pad)
(key-def number-prefix-map "s"      'number/sub)

;; folding
(key-def folding-prefix-map "C-f" 'fold-dwim-toggle)
(key-def folding-prefix-map "C-y" 'yafolding-toggle-element)
(key-def folding-prefix-map "SPC" 'fold-dwim-show-all)
(key-def folding-prefix-map "f"   'hs-fold-levels)
(key-def folding-prefix-map "F"   'fold-dwim-show-all)
(key-def folding-prefix-map "s"   'whitespace-fold-levels)
(key-def folding-prefix-map "S"   'whitespace-fold-reset)
(key-def folding-prefix-map "y"   'yafolding-hide-all)
(key-def folding-prefix-map "Y"   'yafolding-show-all)

;; smartparens
(key-def sexp-prefix-map "C-<tab>" 'sp-indent-adjust-sexp)
(key-def sexp-prefix-map "<tab>"   'sp-dedent-adjust-sexp)
(key-def sexp-prefix-map "C-a" 'sp-kill-to-beginning-of-sexp)
(key-def sexp-prefix-map "C-e" 'sp-kill-to-end-of-sexp)
(key-def sexp-prefix-map "M-a" 'sp-copy-to-beginning-of-sexp)
(key-def sexp-prefix-map "M-e" 'sp-copy-to-end-of-sexp)
(key-def sexp-prefix-map "a"   'sp-beginning-of-sexp)
(key-def sexp-prefix-map "e"   'sp-end-of-sexp)
(key-def sexp-prefix-map "k"   'sp-kill-sexp)
(key-def sexp-prefix-map "r"   'sp-rewrap-sexp)
(key-def sexp-prefix-map "u"   'sp-unwrap-sexp)
(key-def sexp-prefix-map "U"   'sp-backward-unwrap-sexp)
(key-def sexp-prefix-map "w"   'sp-copy-sexp)

;; narrowing
(key-def narrow-map "SPC" 'narrow-or-widen-dwim)
(key-def narrow-map "d"   'narrow-to-defun)
(key-def narrow-map "n"   'narrow-to-region)
(key-def narrow-map "p"   'narrow-to-page)
(key-def narrow-map "s"   'sp-narrow-to-sexp)
(key-def narrow-map "w"   'widen)

;; macro
(key-def macro-prefix-map "a" 'kmacro-add-counter)
(key-def macro-prefix-map "b" 'kmacro-bind-to-key)
(key-def macro-prefix-map "c" 'kmacro-set-counter)
(key-def macro-prefix-map "e" 'edit-kbd-macro)
(key-def macro-prefix-map "l" 'kmacro-edit-lossage)
(key-def macro-prefix-map "m" 'kmacro-end-or-call-macro-repeat)
(key-def macro-prefix-map "n" 'kmacro-name-last-macro)
(key-def macro-prefix-map "s" 'kmacro-start-macro-or-insert-counter)
(key-def macro-prefix-map "t" 'insert-kbd-macro)

;; helm
(setup-after "helm"
  (key-def helm-map "C-w" 'subword-backward-kill)
  (key-def helm-map "M-w" 'helm-yank-text-at-point)
  )

;; (key-def global-map "M-x" 'helm-M-x)

(key-def helm-prefix-map "C-o" 'helm-swoop)
(key-def helm-prefix-map "e"   'helm-flycheck)
;; (key-def helm-prefix-map "f"   'helm-find-files)
(key-def helm-prefix-map "g"   'helm-do-grep)
(key-def helm-prefix-map "o"   'helm-occur)
(key-def helm-prefix-map "t"   'helm-cmd-t)

;; org-mode
(setup-after "org"
  (org-defkey org-mode-map (kbd "C-c C-d") 'org-todo-done)
  (org-defkey org-mode-map (kbd "C-c C-t") 'org-todo-todo)
  (org-defkey org-mode-map (kbd "C-c C-w") 'org-todo-waiting))

;; haskell
(setup-after "haskell-mode"
  (key-def haskell-mode-map "C-c ?"   'haskell-process-do-type)
  (key-def haskell-mode-map "C-c C-?" 'haskell-process-do-info))

;; ruby
(setup-after "enh-ruby-mode"
  (key-def enh-ruby-mode-map "C-c ?" 'yari))

(setup-after "dired"
  (key-def dired-mode-map "C-c C-c"  'wdired-change-to-wdired-mode)
  (key-def dired-mode-map "<insert>" 'dired-mark)
  (key-def dired-mode-map "."        'dired-omit-mode)

  ;; C-a goes to filename
  (key-def dired-mode-map  "C-a" 'dired-back-to-start-of-files)
  (key-def wdired-mode-map "C-a" 'dired-back-to-start-of-files)

  ;; M-up goes to first file
  (key-def dired-mode-map  or 'remap 'beginning-of-buffer 'dired-back-to-top)
  (key-def wdired-mode-map or 'remap 'beginning-of-buffer 'dired-back-to-top)
  (key-def dired-mode-map  or 'remap 'smart-up            'dired-back-to-top)

  ;; M-down goes to last file
  (key-def dired-mode-map  or 'remap 'end-of-buffer 'dired-jump-to-bottom)
  (key-def dired-mode-map  or 'remap 'smart-down    'dired-jump-to-bottom)
  (key-def wdired-mode-map or 'remap 'end-of-buffer 'dired-jump-to-bottom))

;; golang
(setup-after "go-mode"
  (key-def go-mode-map "M-t" 'godef-jump)
  (key-def go-mode-map "M-T" 'godef-jump-other-window))

(setup-after "magit"
  ;; needed because of fullscreen override
  (key-def magit-status-mode-map "q" 'magit-quit-session)
  (key-def magit-status-mode-map "W" 'magit-toggle-whitespace))

;; rectangle selection
(key-def rectangle-prefix-map "M-w" 'copy-rectangle-as-kill)
(key-def rectangle-prefix-map "c"   'clear-rectangle)
(key-def rectangle-prefix-map "d"   'delete-rectangle)
(key-def rectangle-prefix-map "k"   'kill-rectangle)
(key-def rectangle-prefix-map "n"   'rectangle-number-lines)
(key-def rectangle-prefix-map "o"   'open-rectangle)
(key-def rectangle-prefix-map "r"   'copy-rectangle-to-register)
(key-def rectangle-prefix-map "t"   'string-rectangle)
(key-def rectangle-prefix-map "y"   'yank-rectangle)

;; registers (still unused)
(key-def register-prefix-map "SPC" 'point-to-register)
(key-def register-prefix-map "+"   'increment-register)
(key-def register-prefix-map "b"   'bookmark-jump)
(key-def register-prefix-map "f"   'frame-configuration-to-register)
(key-def register-prefix-map "g"   'insert-register)
(key-def register-prefix-map "i"   'insert-register)
(key-def register-prefix-map "j"   'jump-to-register)
(key-def register-prefix-map "l"   'bookmark-bmenu-list)
(key-def register-prefix-map "m"   'bookmark-set)
(key-def register-prefix-map "n"   'number-to-register)
(key-def register-prefix-map "s"   'copy-to-register)
(key-def register-prefix-map "w"   'window-configuration-to-register)
(key-def register-prefix-map "x"   'copy-to-register)

;; case changes
(key-def case-prefix-map "SPC" 'toggle-title-case)
(key-def case-prefix-map "c"   'toggle-upcase)
(key-def case-prefix-map "u"   'upcase-word-or-region)
(key-def case-prefix-map "d"   'downcase-word-or-region)
(key-def case-prefix-map "l"   'downcase-word-or-region)
(key-def case-prefix-map "t"   'title-case-word-or-region)
(key-def case-prefix-map "T"   'toggle-title-case)

(provide 'init-keys)
