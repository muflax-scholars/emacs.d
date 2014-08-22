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

;; keep an accessible backup in case shit breaks badly
(setq old-global-map (copy-keymap global-map))

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

;; unset unwanted default keys, so they show up in free-keys
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

;; fix mod4 bug
(define-key special-event-map (kbd "<key-17>")   'ignore)
(define-key special-event-map (kbd "<M-key-17>") 'ignore)

;; make DEL always work like intended
(normal-erase-is-backspace-mode 1)

;; un-fuck-up with a shotgun modes that steal C-c badly
(defun unbreak-stupid-map (stupid-map)
  (define-key stupid-map (kbd "C-c") nil))

(setup-after "python"        (unbreak-stupid-map python-mode-map))
(setup-after "enh-ruby-mode" (unbreak-stupid-map enh-ruby-mode-map))
(setup-after "go-mode"       (unbreak-stupid-map go-mode-map))
(setup-after "flycheck"      (unbreak-stupid-map flycheck-mode-map))
(setup-after "conf-mode"     (unbreak-stupid-map conf-mode-map))

(setup-lazy '(free-keys) "free-keys"
  ;; allowed key components
  (setq free-keys-keys (apply 'concat assignable-normal-keys)))

;; help for more obscure prefix keys
(setup "guide-key"
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x c" "C-x C-k"))
  (guide-key-mode 1)
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom))

;; commenting
(global-set-key (kbd "C-c SPC")   'comment-dwim)
(global-set-key (kbd "C-c C-SPC") 'comment-dwim)
(global-set-key (kbd "C-c c")     'comment-region)
(global-set-key (kbd "C-c u")     'uncomment-region)

;; code navigation
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)
(global-set-key (kbd "M-t")   'find-tag)

;; eval
(global-set-key (kbd "C-x SPC d")   'eval-defun)
(global-set-key (kbd "C-x SPC SPC") 'eval-defun)
(global-set-key (kbd "C-x SPC b")   'eval-buffer)
(global-set-key (kbd "C-x SPC e")   'eval-expression)
(global-set-key (kbd "C-x SPC r")   'eval-region)

;; debug
(global-set-key (kbd "C-x d d") 'edebug-defun)
(global-set-key (kbd "C-x d s") 'profiler-start)
(global-set-key (kbd "C-x d r") 'profiler-report)
(global-set-key (kbd "C-x d g") 'profiler-stop)

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
(global-set-key (kbd "C-c C-g") 'abort-recursive-edit)

;; save some strokes
(global-set-key (kbd "<f2>")   'save-buffer)
(global-set-key (kbd "S-<f2>") 'save-some-buffers)

;; mark
(global-set-key (kbd "S-<SPC>") 'set-mark-command)

;; font switches
(global-set-key (kbd "C-c <f1>") 'use-small-font)
(global-set-key (kbd "C-c <f2>") 'use-normal-font)
(global-set-key (kbd "C-c <f3>") 'use-big-font)
(global-set-key (kbd "C-c <f4>") 'use-huge-font)

;; better fullscreen toggle
(global-set-key (kbd "<f11>") 'automargin-mode)

(setup-after "yasnippet"
  ;; saner trigger key
  (define-key yas-minor-mode-map [(tab)]     nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "M-o") 'yas-insert-snippet)
  (define-key yas-keymap (kbd "C-o")         'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-S-o")       'yas-next-field)
  (define-key yas-keymap (kbd "<return>")    'yas/exit-all-snippets)
  (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
  (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

  ;; quick reloads
  (define-key yas-minor-mode-map (kbd "C-c C-o") 'yas-reload-all))

;; auto-yasnippet
(global-set-key (kbd "C-c ~")   'aya-create)
(global-set-key (kbd "C-c C-~") 'aya-expand)

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
(global-set-key (kbd "C-p l")            'mc/edit-lines)
(global-set-key (kbd "<C-down>")         'mc/mark-next-like-this)
(global-set-key (kbd "<C-up>")           'mc/mark-previous-like-this)
(global-set-key (kbd "C-p f")            'mc/skip-to-next-like-this)
(global-set-key (kbd "C-p b")            'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-p SPC")          'mc/mark-all-dwim)
(global-set-key (kbd "C-p m")            'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-<mouse-1>")    'mc/add-cursor-on-click)
(global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-p r")            'set-rectangular-region-anchor)
(global-set-key (kbd "C-p n")            'mc/insert-numbers)
(global-set-key (kbd "C-p s")            'mc/sort-regions)
(global-set-key (kbd "C-p R")            'mc/reverse-regions)

(global-set-key (kbd "C-p c")          'mc/compare-chars-forward)
(global-set-key (kbd "C-p C")          'mc/compare-chars-backward)
(global-set-key (kbd "C-p u")          'mc/remove-current-cursor)
(global-set-key (kbd "C-p d")          'mc/remove-duplicated-cursors)

(global-set-key (kbd "C-p g")          'mc-jump-char)

(setup-after "multiple-cursors-core"
  ;; <ret> inserts a newline; C-j exits (a bit more convenient that way)
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-j") 'multiple-cursors-mode)

  (define-key mc/keymap (kbd "C-p h")      'mc-hide-unmatched-lines-mode))

(global-set-key (kbd "C-p i") 'iedit-mode)
(global-set-key (kbd "C-p I") 'iedit-mode-toggle-on-function)

;; editing
(global-set-key (kbd "M-k") 'copy-line)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-y")   'yank-and-indent)
(global-set-key (kbd "C-S-y") 'yank)
(global-set-key (kbd "M-S-y") 'yank-pop-reverse)
(global-set-key (kbd "C-S-o") 'next-newline-and-indent)
(global-set-key (kbd "C-k") 'kill-and-join-forward)

(global-set-key (kbd "<S-delete>")    'literal-delete-char)
(global-set-key (kbd "<S-backspace>") 'literal-delete-backward-char)

(global-set-key (kbd "C-d")     'kill-without-append)
(global-set-key (kbd "M-d")     'blank-line)
(global-set-key (kbd "C-c C-w") 'kill-with-append)
(global-set-key (kbd "C-c w")   'copy-with-append)

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

;; aligning tables
(global-set-key (kbd "C-x a t") 'delimit-columns-current)
(global-set-key (kbd "C-x a T") 'delimit-columns-region)

;; undo hardwrapped regions (mostly markdown)
(global-set-key (kbd "M-S-q") 'unfill-region)

;; spell-check
(global-set-key (kbd "C-x w SPC") 'wcheck-actions)
(global-set-key (kbd "C-x w d")   'disable-spell-check)
(global-set-key (kbd "C-x w e")   'enable-spell-check)
(global-set-key (kbd "C-x w w")   'wcheck-mode)

;; aligning
(global-set-key (kbd "C-x a a")   'align-region-or-current)
(global-set-key (kbd "C-x a SPC") 'align-repeat)
(global-set-key (kbd "C-x a r")   'align-repeat)

;; input methods
(global-set-key (kbd "C-x t 0") (lambda () (interactive) (set-input-method nil)))
(global-set-key (kbd "C-x t t") (lambda () (interactive) (set-input-method "muflax-latin")))
(global-set-key (kbd "C-x t l") (lambda () (interactive) (set-input-method "muflax-latin")))
(global-set-key (kbd "C-x t c") (lambda () (interactive) (set-input-method "muflax-cyrillic")))
(global-set-key (kbd "C-x t t") (lambda () (interactive) (set-input-method "muflax-turkish")))
(global-set-key (kbd "C-x t g") (lambda () (interactive) (set-input-method "muflax-greek")))
(global-set-key (kbd "C-x t j") (lambda () (interactive) (set-input-method "japanese-mozc")))

(global-set-key (kbd "C-x t SPC") 'toggle-input-method)
(global-set-key (kbd "<kanji>") 'toggle-input-method)

;; window config
(global-set-key (kbd "C-r b") 'winner-undo)
(global-set-key (kbd "C-r f") 'winner-redo)

;; expand region
(global-set-key (kbd "C-<right>") 'er/expand-region)
(global-set-key (kbd "C-<left>")  'er/contract-region)
(global-set-key (kbd "M-<right>") 'er/mark-defun)
(global-set-key (kbd "M-<left>")  'er/mark-symbol)
(global-set-key (kbd "C-c s d")   'er/mark-defun)
(global-set-key (kbd "C-c s SPC") 'er/mark-defun)
(global-set-key (kbd "C-c s w>")  'er/mark-symbol)

;; shell commands
(global-set-key (kbd "C-|") 'generalized-shell-command)
(global-set-key (kbd "C-\\") 'generalized-shell-command) ; terminal bug

;; scratch buffer
(global-set-key (kbd "C-r s") 'scratch)

;; arithmetic
(global-set-key (kbd "C-c m +")      'number/add)
(global-set-key (kbd "C-c m a")      'number/add)
(global-set-key (kbd "C-c m <up>")   (lambda () (interactive) (number/add (number-read "1"))))
(global-set-key (kbd "C-c m -")      'number/sub)
(global-set-key (kbd "C-c m s")      'number/sub)
(global-set-key (kbd "C-c m <down>") (lambda () (interactive) (number/sub (number-read "1"))))
(global-set-key (kbd "C-c m *")      'number/multiply)
(global-set-key (kbd "C-c m m")      'number/multiply)
(global-set-key (kbd "C-c m /")      'number/divide)
(global-set-key (kbd "C-c m d")      'number/divide)
(global-set-key (kbd "C-c m 0")      'number/pad)
(global-set-key (kbd "C-c m p")      'number/pad)
(global-set-key (kbd "C-c m =")      'number/eval)
(global-set-key (kbd "C-c m e")      'number/eval)

;; rotate text
(global-set-key (kbd "C-c C-t") 'rotate-text)

;; folding
(define-key global-map (kbd "C-v C-f")   'fold-dwim-toggle)
(define-key global-map (kbd "<mouse-3>") 'fold-dwim-toggle)
(define-key global-map (kbd "C-v f")     'hs-fold-levels)
(define-key global-map (kbd "C-v F")     'fold-dwim-show-all)
(define-key global-map (kbd "C-v SPC")   'fold-dwim-show-all)

(define-key global-map (kbd "C-v s")     'whitespace-fold-levels)
(define-key global-map (kbd "C-v S")     'whitespace-fold-reset)

(define-key global-map (kbd "C-v C-y")   'yafolding-toggle-element)
(define-key global-map (kbd "C-v y")     'yafolding-hide-all)
(define-key global-map (kbd "C-v Y")     'yafolding-show-all)

(setup-after "smartparens-config"
  ;; navigation
  (define-key sp-keymap (kbd "C-c a")         'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c e")         'sp-end-of-sexp)
  (define-key sp-keymap (kbd "C-c s a")       'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c s e")       'sp-end-of-sexp)
  (define-key sp-keymap (kbd "C-M-f")         'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b")         'sp-backward-sexp)
  (define-key sp-keymap (kbd "M-f")           'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-b")           'sp-backward-symbol)

  ;; killing
  (define-key sp-keymap (kbd "C-c s C-a")     'sp-kill-to-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c s C-e")     'sp-kill-to-end-of-sexp)
  (define-key sp-keymap (kbd "C-c s M-a")     'sp-copy-to-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c s M-e")     'sp-copy-to-end-of-sexp)
  (define-key sp-keymap (kbd "C-M-k")         'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w")         'sp-copy-sexp)
  (define-key sp-keymap (kbd "C-c s k")       'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-c s w")       'sp-copy-sexp)

  ;; wrapping
  (define-key sp-keymap (kbd "M-<delete>")    'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "C-c s u")       'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "C-c s U")       'sp-backward-unwrap-sexp)
  (define-key sp-keymap (kbd "C-c s r")       'sp-rewrap-sexp)

  ;; adjusting
  (define-key sp-keymap (kbd "C-c C-<tab>")   'sp-indent-adjust-sexp)
  (define-key sp-keymap (kbd "C-c <tab>")     'sp-dedent-adjust-sexp)
  (define-key sp-keymap (kbd "C-c s C-<tab>") 'sp-indent-adjust-sexp)
  (define-key sp-keymap (kbd "C-c s <tab>")   'sp-dedent-adjust-sexp)

  ;; narrowing
  (define-key sp-keymap (kbd "C-x n s")       'sp-narrow-to-sexp)
  ;; TODO generalize C-( to narrow-or-region
  (define-key sp-keymap (kbd "C-(")           'sp-narrow-to-sexp)
  (define-key sp-keymap (kbd "C-)")           'widen)
  )

;; case changes
(global-set-key (kbd "M-c") 'toggle-title-case)
(global-set-key (kbd "M-u") 'toggle-upcase)

;; macro
;; macro key bindings
(global-set-key (kbd "C-x m C-t") 'insert-kbd-macro)
(global-set-key (kbd "C-x m C-n") 'kmacro-name-last-macro)
(global-set-key (kbd "C-x m C-b") 'kmacro-bind-to-key)

;; window management
(global-set-key (kbd "C-r w")        'sticky-window-delete-window)
(global-set-key (kbd "C-r SPC")      'sticky-window-delete-window)
(global-set-key (kbd "C-r k")        'sticky-window-delete-other-windows)
(global-set-key (kbd "C-r C-r")      'sticky-window-delete-other-windows)
(global-set-key (kbd "C-r <return>") 'sticky-window-delete-other-windows)
(global-set-key (kbd "C-r <down>")   'split-window-below)
(global-set-key (kbd "C-r <right>")  'split-window-right)
(global-set-key (kbd "C-r v")        'sticky-window-keep-window-visible)
(global-set-key (kbd "C-r n") 'neotree-toggle)

;; faster navigation experimentation
(global-set-key (kbd "<menu>")   'nav-minor-mode)
(global-set-key (kbd "C-<menu>") 'nav-global-mode)

;; narrowing
(global-set-key (kbd "C-x n SPC") 'narrow-or-widen-dwim)

;; yank search
(global-set-key (kbd "M-C-y") 'kill-ring-search)
(global-set-key (kbd "C-s y") 'kill-ring-search)

;; ace-jump
(global-set-key (kbd "C-s g n") 'goto-line)
(global-set-key (kbd "C-s g b") 'ace-jump-buffer)
(global-set-key (kbd "C-s g c") 'ace-jump-char-mode)
(global-set-key (kbd "C-s g g") 'ace-jump-mode)
(global-set-key (kbd "C-s g l") 'ace-jump-line-mode)
(global-set-key (kbd "C-s g w") 'ace-window)

;; search
(global-set-key (kbd "C-s p") 'phi-search)
(global-set-key (kbd "C-s P") 'phi-search-backward)
(global-set-key (kbd "C-s s")   'isearch-forward-use-region)
(global-set-key (kbd "C-s b")   'isearch-backward-use-region)
(global-set-key (kbd "C-s S")   'isearch-forward-regexp)
(global-set-key (kbd "C-s b")   'isearch-backward-regexp)
(global-set-key (kbd "C-s C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-s C-r") 'isearch-backward-use-region)

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
(global-set-key (kbd "C-s r") 'vr/query-replace)
(global-set-key (kbd "C-s R") 'vr/query-replace-from-beginning)

;; M-x
(global-set-key (kbd "M-x")   'smex)
(global-set-key (kbd "M-S-x") 'smex-major-mode-commands)

;; open files
(global-set-key (kbd "C-x M-f") 'find-file-at-point)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(global-set-key (kbd "C-s [") 'idomenu)
(global-set-key (kbd "C-s i") 'idomenu)
(global-set-key (kbd "C-c ]") 'imenu-anywhere)
(global-set-key (kbd "C-c I") 'imenu-anywhere)

;; helm
(setup-after "helm-config"
  (define-key helm-map (kbd "C-w")  'subword-backward-kill)
  (define-key helm-map (kbd "M-w")  'helm-yank-text-at-point))

(global-set-key (kbd "C-x c t")   'helm-cmd-t)
(global-set-key (kbd "C-x c g")   'helm-do-grep)
(global-set-key (kbd "C-x c o")   'helm-occur)
(global-set-key (kbd "C-x c e")   'helm-flycheck)
(global-set-key (kbd "C-x c C-o") 'helm-swoop)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

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
(global-set-key (kbd "C-c C-j")   'dired-jump)
(global-set-key (kbd "C-<next>")  'dired-next)
(global-set-key (kbd "C-<prior>") 'dired-prev)

(setup-after "dired"
  (define-key dired-mode-map (kbd "C-c C-c")  'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "<insert>") 'dired-mark)
  (define-key dired-mode-map (kbd ".") 'dired-omit-mode)

  ;; C-a goes to filename
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

  ;; M-up goes to first file
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)

  ;; M-down goes to last file
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

;; golang
(setup-after "go-mode"
  (define-key go-mode-map (kbd "M-t")   'godef-jump)
  (define-key go-mode-map (kbd "M-S-t") 'godef-jump-other-window))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setup-after "magit"
  ;; needed because of fullscreen override
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

;; package list
(global-set-key (kbd "C-x p") 'paradox-list-packages)

(provide 'init-keys)
