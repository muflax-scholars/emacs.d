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
      '(""
        "C-" "C-S-"
        "M-" "M-S-"
        "C-M-" "C-M-S-"))

(setq assignable-special-modifiers
      ;; only valid with special keys
      '("S-"))

(defun unset-keymap-by-keys (keys modifiers keymap)
  "unset all mod+keys in given keymap"
  (let ((full-key))

    (loop for modifier in modifiers collect
          (loop for key in keys collect
                (progn
                  (setq full-key (kbd (concat modifier key)))
                  (define-key keymap full-key nil))))))

(defun copy-keymap-by-keys (keys modifiers from to)
  "copies over all mod+keys from one keymap to another"
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
  (interactive)
  (if (display-graphic-p)
      (call-interactively command)
    (progn
      (setq command (lookup-key (current-global-map) (kbd alt-key)))
      (when command
        (call-interactively command)))))

(defun repeat-command (command)
  "Call command, and immediately go into repeat mode."
  (interactive)
  (let ((repeat-message-function  'ignore))
    (setq last-repeatable-command  command)
    (repeat nil)))

;; unset unwanted default keys
(loop for key in `(
                   (,(kbd "C-x C-z")          suspend-frame)
                   (,(kbd "C-z")              suspend-frame)
                   ([(insert)]                overwrite-mode)
                   ([(insertchar)]            overwrite-mode)
                   (,(kbd "C-p")              previous-line)
                   (,(kbd "C-r")              isearch-backward)
                   (,(kbd "C-s")              isearch-forward)
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
                   (,(kbd "C-x d")            dired)
                   (,(kbd "C-x l")            count-lines-page)
                   (,(kbd "C-x m")            compose-mail)
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
                   (,(kbd "M-l")              downcase-word)
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
                   (,(kbd "C-t")              transpose-chars)
                   )
      collect (if (eq (key-binding (first key)) (second key))
                  (global-unset-key (first key))))

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

;; make DEL always work like intended
(normal-erase-is-backspace-mode 1)

;; find unused keys
(setup-lazy '(free-keys) "free-keys"
  ;; allowed key components
  (setq free-keys-keys (apply 'concat assignable-normal-keys)))

;; built-ins prefix maps restated for clarity
(defvar old-global-map (copy-keymap (current-global-map)) "accessible backup in case shit breaks badly")
(defvar ctl-x-map)
(defvar help-map)
(defvar mode-specific-map)

;; define prefix keys
(defun set-prefix-key (map key command)
  (define-prefix-command command)
  (define-key map (kbd key) command))

(set-prefix-key (current-global-map) "C-p" 'mc-prefix-map)
(set-prefix-key (current-global-map) "C-r" 'window-prefix-map)
(set-prefix-key (current-global-map) "C-v" 'folding-prefix-map)
(set-prefix-key (current-global-map) "C-s"   'search-prefix-map)

(set-prefix-key ctl-x-map            "SPC" 'eval-prefix-map)
(set-prefix-key ctl-x-map            "a"   'align-prefix-map)
(set-prefix-key ctl-x-map            "c"   'helm-prefix-map)
(set-prefix-key ctl-x-map            "d"   'debug-prefix-map)
(set-prefix-key ctl-x-map            "m"   'macro-prefix-map)
(set-prefix-key ctl-x-map            "t"   'input-prefix-map)
(set-prefix-key ctl-x-map            "w"   'spell-check-prefix-map)

(set-prefix-key mode-specific-map    "m"   'number-prefix-map )
(set-prefix-key mode-specific-map    "s"   'sexp-prefix-map)

(set-prefix-key search-prefix-map    "g"   'jump-prefix-map)

;; commenting
(define-key mode-specific-map (kbd "SPC")   'comment-dwim)
(define-key mode-specific-map (kbd "C-SPC") 'comment-dwim)
(define-key mode-specific-map (kbd "c")     'comment-region)
(define-key mode-specific-map (kbd "u")     'uncomment-region)

;; code navigation
(define-key mode-specific-map (kbd "n") 'next-error)
(define-key mode-specific-map (kbd "p") 'previous-error)
(global-set-key (kbd "M-t") 'find-tag)

;; eval
(define-key eval-prefix-map (kbd "d")   'eval-defun)
(define-key eval-prefix-map (kbd "SPC") 'eval-defun)
(define-key eval-prefix-map (kbd "b")   'eval-buffer)
(define-key eval-prefix-map (kbd "e")   'eval-expression)
(define-key eval-prefix-map (kbd "r")   'eval-region)

;; debug
(define-key debug-prefix-map (kbd "d") 'edebug-defun)
(define-key debug-prefix-map (kbd "s") 'profiler-start)
(define-key debug-prefix-map (kbd "r") 'profiler-report)
(define-key debug-prefix-map (kbd "g") 'profiler-stop)

;; undo
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "M-z") 'undo-tree-redo)
(global-set-key (kbd "M-n") 'undo-tree-undo)
(global-set-key (kbd "M-p") 'undo-tree-redo)

;; because we navigate via cursor keys, we can put something more useful on the default navigational keys
(global-set-key (kbd "C-n")   'other-window)
(global-set-key (kbd "C-S-n") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-f")   'forward-word)
(global-set-key (kbd "C-b")   'backward-word)

;; obvious keys
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>")  'end-of-buffer)

;; make C-Backspace "work" in terminal
(global-set-key (kbd "S-<f7>") 'backward-kill-word)

;; get out of recursive edit
(define-key mode-specific-map (kbd "C-g") 'abort-recursive-edit)

;; save some strokes
(global-set-key (kbd "<f2>")   'save-buffer)
(global-set-key (kbd "S-<f2>") 'save-some-buffers)

;; mark
(global-set-key (kbd "S-<SPC>") 'set-mark-command)

;; font switches
(define-key mode-specific-map (kbd "<f1>") 'use-small-font)
(define-key mode-specific-map (kbd "<f2>") 'use-normal-font)
(define-key mode-specific-map (kbd "<f3>") 'use-big-font)
(define-key mode-specific-map (kbd "<f4>") 'use-huge-font)

;; better fullscreen toggle
(global-set-key (kbd "<f11>") 'automargin-mode)

(setup-after "yasnippet"
  ;; saner trigger key
  (define-key yas-minor-mode-map [(tab)]     nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "M-o") 'yas-insert-snippet)

  (define-key yas-keymap (kbd "C-o")      'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-S-o")    'yas-next-field)
  (define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)
  (define-key yas-keymap (kbd "C-e")      'yas/goto-end-of-active-field)
  (define-key yas-keymap (kbd "C-a")      'yas/goto-start-of-active-field)

  ;; quick reloads
  (define-key yas-minor-mode-map (kbd "C-c C-o") 'yas-reload-all))

;; auto-yasnippet
(define-key mode-specific-map (kbd "~")   'aya-create)
(define-key mode-specific-map (kbd "C-~") 'aya-expand)

;; auto-completion
(setup-after "auto-complete-config"
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
  (define-key ac-completing-map (kbd "C-j") 'ac-complete))

;; multiple-cursors
(global-set-key (kbd "<C-down>")         'mc/mark-next-like-this)
(global-set-key (kbd "<C-up>")           'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-<mouse-1>")    'mc/add-cursor-on-click)
(global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click)

(define-key mc-prefix-map (kbd "l")   'mc/edit-lines)
(define-key mc-prefix-map (kbd "f")   'mc/skip-to-next-like-this)
(define-key mc-prefix-map (kbd "b")   'mc/skip-to-previous-like-this)
(define-key mc-prefix-map (kbd "SPC") 'mc/mark-all-dwim)
(define-key mc-prefix-map (kbd "m")   'mc/mark-more-like-this-extended)
(define-key mc-prefix-map (kbd "r")   'set-rectangular-region-anchor)
(define-key mc-prefix-map (kbd "n")   'mc/insert-numbers)
(define-key mc-prefix-map (kbd "s")   'mc/sort-regions)
(define-key mc-prefix-map (kbd "R")   'mc/reverse-regions)
(define-key mc-prefix-map (kbd "c")   'mc/compare-chars-forward)
(define-key mc-prefix-map (kbd "C")   'mc/compare-chars-backward)
(define-key mc-prefix-map (kbd "u")   'mc/remove-current-cursor)
(define-key mc-prefix-map (kbd "d")   'mc/remove-duplicated-cursors)
(define-key mc-prefix-map (kbd "g")   'mc-jump-char)

(setup-after "multiple-cursors-core"
  ;; <ret> inserts a newline; C-j exits (a bit more convenient that way)
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-j")      'multiple-cursors-mode)

  (define-key mc/keymap (kbd "C-p h") 'mc-hide-unmatched-lines-mode))

(define-key mc-prefix-map (kbd "i") 'iedit-mode)
(define-key mc-prefix-map (kbd "I") 'iedit-mode-toggle-on-function)

;; editing
(global-set-key (kbd "M-k")           'copy-line)
(global-set-key (kbd "C-y")           'yank-and-indent)
(global-set-key (kbd "C-S-y")         'yank)
(global-set-key (kbd "M-S-y")         'yank-pop-reverse)
(global-set-key (kbd "C-S-o")         'next-newline-and-indent)
(global-set-key (kbd "C-k")           'kill-and-join-forward)
(global-set-key (kbd "<S-delete>")    'literal-delete-char)
(global-set-key (kbd "<S-backspace>") 'literal-delete-backward-char)
(global-set-key (kbd "C-d")           'kill-without-append)
(global-set-key (kbd "M-d")           'blank-line)

(define-key mode-specific-map (kbd "i")   'indent-region)
(define-key mode-specific-map (kbd "C-w") 'kill-with-append)
(define-key mode-specific-map (kbd "w")   'copy-with-append)

;; handle comments
(setup-after "cc-mode"
  (define-key c-mode-map (kbd "M-RET") 'c-indent-new-comment-line))

;; move lines like in org-mode
(global-set-key (kbd "M-<up>")     'md/move-lines-up)
(global-set-key (kbd "M-<down>")   'md/move-lines-down)
(global-set-key (kbd "C-<return>") 'md/duplicate-down)

;; move buffers
(global-set-key (kbd "<C-M-up>")    'buf-move-up)
(global-set-key (kbd "<C-M-down>")  'buf-move-down)
(global-set-key (kbd "<C-M-left>")  'buf-move-left)
(global-set-key (kbd "<C-M-right>") 'buf-move-right)

;; insert literal tab
(global-set-key (kbd "<C-tab>") (lambda () (interactive)
                                  (insert "\t")))
;; navigation
(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "C-e") 'smart-end-of-line)

;; aligning things
(define-key align-prefix-map (kbd "a")   'align-region-or-current)
(define-key align-prefix-map (kbd "SPC") 'align-repeat)
(define-key align-prefix-map (kbd "r")   'align-repeat)
;; aligning tables
(define-key align-prefix-map (kbd "t") 'delimit-columns-current)
(define-key align-prefix-map (kbd "T") 'delimit-columns-region)

;; undo hardwrapped regions (mostly markdown)
(global-set-key (kbd "M-S-q") 'unfill-region)

;; spell-check
(define-key spell-check-prefix-map (kbd "SPC") 'wcheck-actions)
(define-key spell-check-prefix-map (kbd "d")   'disable-spell-check)
(define-key spell-check-prefix-map (kbd "e")   'enable-spell-check)
(define-key spell-check-prefix-map (kbd "w")   'wcheck-mode)

;; aligning

;; input methods
(global-set-key (kbd "<kanji>") 'toggle-input-method)

(define-key input-prefix-map (kbd "0")   'clear-input-method)
(define-key input-prefix-map (kbd "t")   'set-input-method-muflax-latin)
(define-key input-prefix-map (kbd "l")   'set-input-method-muflax-latin)
(define-key input-prefix-map (kbd "c")   'set-input-method-muflax-cyrillic)
(define-key input-prefix-map (kbd "t")   'set-input-method-muflax-turkish)
(define-key input-prefix-map (kbd "g")   'set-input-method-muflax-greek)
(define-key input-prefix-map (kbd "j")   'set-input-method-japanese-mozc)
(define-key input-prefix-map (kbd "SPC") 'toggle-input-method)

;; window config
(define-key window-prefix-map (kbd "b")        'winner-undo)
(define-key window-prefix-map (kbd "f")        'winner-redo)
(define-key window-prefix-map (kbd "w")        'sticky-window-delete-window)
(define-key window-prefix-map (kbd "SPC")      'sticky-window-delete-window)
(define-key window-prefix-map (kbd "k")        'sticky-window-delete-other-windows)
(define-key window-prefix-map (kbd "C-r")      'sticky-window-delete-other-windows)
(define-key window-prefix-map (kbd "<return>") 'sticky-window-delete-other-windows)
(define-key window-prefix-map (kbd "<down>")   'split-window-below)
(define-key window-prefix-map (kbd "<right>")  'split-window-right)
(define-key window-prefix-map (kbd "v")        'sticky-window-keep-window-visible)
(define-key window-prefix-map (kbd "n")        'neotree-toggle)
(define-key window-prefix-map (kbd "s")        'scratch)

;; expand region
(global-set-key (kbd "C-<right>") 'er/expand-region)
(global-set-key (kbd "C-<left>")  'er/contract-region)
(global-set-key (kbd "M-<right>") 'er/mark-defun)
(global-set-key (kbd "M-<left>")  'er/mark-symbol)

(define-key search-prefix-map (kbd "d")   'er/mark-defun)
(define-key search-prefix-map (kbd "SPC") 'er/mark-defun)
(define-key search-prefix-map (kbd "w>")  'er/mark-symbol)

;; shell commands
(global-set-key (kbd "C-|")  'generalized-shell-command)
(global-set-key (kbd "C-\\") 'generalized-shell-command) ; terminal bug

;; arithmetic
(define-key number-prefix-map (kbd "<up>")   'number/increment)
(define-key number-prefix-map (kbd "<down>") 'number/decrement)
(define-key number-prefix-map (kbd "+")      'number/add)
(define-key number-prefix-map (kbd "a")      'number/add)
(define-key number-prefix-map (kbd "-")      'number/sub)
(define-key number-prefix-map (kbd "s")      'number/sub)
(define-key number-prefix-map (kbd "*")      'number/multiply)
(define-key number-prefix-map (kbd "m")      'number/multiply)
(define-key number-prefix-map (kbd "/")      'number/divide)
(define-key number-prefix-map (kbd "d")      'number/divide)
(define-key number-prefix-map (kbd "0")      'number/pad)
(define-key number-prefix-map (kbd "p")      'number/pad)
(define-key number-prefix-map (kbd "=")      'number/eval)
(define-key number-prefix-map (kbd "e")      'number/eval)

;; rotate text
(define-key mode-specific-map (kbd "C-t") 'rotate-text)

;; folding
(global-set-key (kbd "<mouse-3>") 'fold-dwim-toggle)

(define-key folding-prefix-map (kbd "C-f") 'fold-dwim-toggle)
(define-key folding-prefix-map (kbd "f")   'hs-fold-levels)
(define-key folding-prefix-map (kbd "F")   'fold-dwim-show-all)
(define-key folding-prefix-map (kbd "SPC") 'fold-dwim-show-all)

(define-key folding-prefix-map (kbd "s")   'whitespace-fold-levels)
(define-key folding-prefix-map (kbd "S")   'whitespace-fold-reset)

(define-key folding-prefix-map (kbd "C-y") 'yafolding-toggle-element)
(define-key folding-prefix-map (kbd "y")   'yafolding-hide-all)
(define-key folding-prefix-map (kbd "Y")   'yafolding-show-all)

(setup-after "smartparens-config"
  ;; navigation
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "M-f")   'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-b")   'sp-backward-symbol)

  (define-key sexp-prefix-map (kbd "a") 'sp-beginning-of-sexp)
  (define-key sexp-prefix-map (kbd "e") 'sp-end-of-sexp)

  ;; killing
  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

  (define-key sexp-prefix-map (kbd "C-a") 'sp-kill-to-beginning-of-sexp)
  (define-key sexp-prefix-map (kbd "C-e") 'sp-kill-to-end-of-sexp)
  (define-key sexp-prefix-map (kbd "M-a") 'sp-copy-to-beginning-of-sexp)
  (define-key sexp-prefix-map (kbd "M-e") 'sp-copy-to-end-of-sexp)
  (define-key sexp-prefix-map (kbd "k")   'sp-kill-sexp)
  (define-key sexp-prefix-map (kbd "w")   'sp-copy-sexp)

  ;; wrapping
  (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)

  (define-key sexp-prefix-map (kbd "u") 'sp-unwrap-sexp)
  (define-key sexp-prefix-map (kbd "U") 'sp-backward-unwrap-sexp)
  (define-key sexp-prefix-map (kbd "r") 'sp-rewrap-sexp)

  ;; adjusting
  (define-key sp-keymap (kbd "C-c C-<tab>") 'sp-indent-adjust-sexp)
  (define-key sp-keymap (kbd "C-c <tab>")   'sp-dedent-adjust-sexp)

  (define-key sexp-prefix-map (kbd "C-<tab>") 'sp-indent-adjust-sexp)
  (define-key sexp-prefix-map (kbd "<tab>")   'sp-dedent-adjust-sexp)

  ;; narrowing
  (define-key narrow-map (kbd "s") 'sp-narrow-to-sexp)

  ;; TODO generalize C-( to narrow-or-region
  (define-key sp-keymap (kbd "C-(") 'sp-narrow-to-sexp)
  (define-key sp-keymap (kbd "C-)") 'widen)
  )

;; case changes
(global-set-key (kbd "M-c") 'toggle-title-case)
(global-set-key (kbd "M-u") 'toggle-upcase)

;; macro
(define-key macro-prefix-map (kbd "C-t") 'insert-kbd-macro)
(define-key macro-prefix-map (kbd "C-n") 'kmacro-name-last-macro)
(define-key macro-prefix-map (kbd "C-b") 'kmacro-bind-to-key)

;; faster navigation experimentation
(global-set-key (kbd "<menu>")   'nav-minor-mode)
(global-set-key (kbd "C-<menu>") 'nav-global-mode)

;; narrowing
(define-key narrow-map (kbd "SPC") 'narrow-or-widen-dwim)

;; yank search
(define-key search-prefix-map (kbd "y") 'kill-ring-search)

;; ace-jump
(define-key jump-prefix-map (kbd "n") 'goto-line)
(define-key jump-prefix-map (kbd "b") 'ace-jump-buffer)
(define-key jump-prefix-map (kbd "c") 'ace-jump-char-mode)
(define-key jump-prefix-map (kbd "g") 'ace-jump-mode)
(define-key jump-prefix-map (kbd "l") 'ace-jump-line-mode)
(define-key jump-prefix-map (kbd "w") 'ace-window)

;; search
(define-key search-prefix-map (kbd "p")   'phi-search)
(define-key search-prefix-map (kbd "P")   'phi-search-backward)
(define-key search-prefix-map (kbd "s")   'isearch-forward-use-region)
(define-key search-prefix-map (kbd "b")   'isearch-backward-use-region)
(define-key search-prefix-map (kbd "S")   'isearch-forward-regexp)
(define-key search-prefix-map (kbd "b")   'isearch-backward-regexp)
(define-key search-prefix-map (kbd "C-s") 'isearch-forward-use-region)
(define-key search-prefix-map (kbd "C-r") 'isearch-backward-use-region)

(setup-after "phi-search"
  (setup "phi-search-mc"
    (define-key phi-search-default-map (kbd "<C-down>")   'phi-search-mc/mark-next)
    (define-key phi-search-default-map (kbd "<C-up>")     'phi-search-mc/mark-previous)
    (define-key phi-search-default-map (kbd "<C-return>") 'phi-search-mc/mark-here)
    (define-key phi-search-default-map (kbd "C-p SPC")    'phi-search-mc/mark-all)))

(setup-after "isearch"
  ;; make backspace more intuitive
  (define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

  (define-key isearch-mode-map (kbd "C-c C-c")   'isearch-normalize-string)
  (define-key isearch-mode-map (kbd "C-c C-w")   'isearch-toggle-word)
  (define-key isearch-mode-map (kbd "C-c C-r")   'isearch-toggle-regexp)
  (define-key isearch-mode-map (kbd "C-c C-i")   'isearch-toggle-case-fold)
  (define-key isearch-mode-map (kbd "C-c C-s")   'isearch-toggle-symbol)
  (define-key isearch-mode-map (kbd "C-c C-SPC") 'isearch-toggle-lax-whitespace)
  (define-key isearch-mode-map (kbd "C-c C-o")   'isearch-occur))

;; regex
(define-key search-prefix-map (kbd "r") 'vr/query-replace)
(define-key search-prefix-map (kbd "R") 'vr/query-replace-from-beginning)

;; M-x
(global-set-key (kbd "M-x")   'smex)
(global-set-key (kbd "M-S-x") 'smex-major-mode-commands)

;; open files
(define-key ctl-x-map (kbd "M-f") 'find-file-at-point)
(define-key ctl-x-map (kbd "C-r") 'recentf-ido-find-file)

(define-key search-prefix-map (kbd "[") 'idomenu)
(define-key search-prefix-map (kbd "i") 'idomenu)
(define-key mode-specific-map (kbd "]") 'imenu-anywhere)
(define-key mode-specific-map (kbd "I") 'imenu-anywhere)

;; helm
(setup-after "helm"
  (define-key helm-map (kbd "C-w") 'subword-backward-kill)
  (define-key helm-map (kbd "M-w") 'helm-yank-text-at-point))

(define-key helm-prefix-map (kbd "t")   'helm-cmd-t)
(define-key helm-prefix-map (kbd "g")   'helm-do-grep)
(define-key helm-prefix-map (kbd "o")   'helm-occur)
(define-key helm-prefix-map (kbd "e")   'helm-flycheck)
(define-key helm-prefix-map (kbd "C-o") 'helm-swoop)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (define-key helm-prefix-map (kbd "f") 'helm-find-files)

;; org-mode
(setup-after "org"
  (org-defkey org-mode-map (kbd "C-c C-t") (lambda () (interactive) (org-todo "TODO")))
  (org-defkey org-mode-map (kbd "C-c C-w") (lambda () (interactive) (org-todo "WAITING")))
  (org-defkey org-mode-map (kbd "C-c C-d") (lambda () (interactive) (org-todo "DONE"))))

;; haskell
(setup-after "haskell-mode"
  (define-key haskell-mode-map (kbd "C-c ?")   'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-?") 'haskell-process-do-info))

;; ruby
(setup-after "enh-ruby-mode"
  (define-key enh-ruby-mode-map (kbd "C-c ?") 'yari))

;; dired
(define-key mode-specific-map (kbd "C-j") 'dired-jump)
(global-set-key (kbd "C-<next>")          'dired-next)
(global-set-key (kbd "C-<prior>")         'dired-prev)

(setup-after "dired"
  (define-key dired-mode-map (kbd "C-c C-c")  'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "<insert>") 'dired-mark)
  (define-key dired-mode-map (kbd ".")        'dired-omit-mode)

  ;; C-a goes to filename
  (define-key dired-mode-map  (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

  ;; M-up goes to first file
  (define-key dired-mode-map  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map  (vector 'remap 'smart-up)            'dired-back-to-top)

  ;; M-down goes to last file
  (define-key dired-mode-map  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map  (vector 'remap 'smart-down)    'dired-jump-to-bottom)
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

;; golang
(setup-after "go-mode"
  (define-key go-mode-map (kbd "M-t")   'godef-jump)
  (define-key go-mode-map (kbd "M-S-t") 'godef-jump-other-window))

;; magit
(define-key ctl-x-map (kbd "g") 'magit-status)

(setup-after "magit"
  ;; needed because of fullscreen override
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

;; package list
(define-key ctl-x-map (kbd "p") 'paradox-list-packages)

(provide 'init-keys)
