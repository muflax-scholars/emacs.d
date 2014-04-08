;; generic editing features

;; safety
(setq make-backup-files nil)
(defvar autosave-dir (expand-file-name "~/.emacs.d/cache/autosave-dir/"))
(setq auto-save-list-file-prefix "~/.emacs-saves/cache/auto-save-list/.saves-")
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)
;; move files to trash instead
(setq delete-by-moving-to-trash t)

;; more useful kill-ring
(setq kill-ring-max 2000)
(setup "kill-ring-search"
  (global-set-key "\M-\C-y" 'kill-ring-search))
(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1))
(global-set-key "\M-Y" 'yank-pop-reverse)

;; save location inside buffer
(setup "saveplace"
  ;; save minibuffer history
  (savehist-mode 1)
  (setq history-length         1000)
  (setq search-ring-max        1000)
  (setq regexp-search-ring-max 1000)
  (setq save-place-file "~/.emacs.d/cache/saveplace")
  (setq-default save-place t)
  (setq savehist-file "~/.emacs.d/cache/history")
  (setq savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        kill-ring
                                        compile-command)))


;; text stuff
(setq undo-limit 1000000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)

;; don't hard-wrap text, but use nice virtual wrapping
(setup "adaptive-wrap"
  (setq-default fill-column 80)
  (global-visual-line-mode 1)
  (global-adaptive-wrap-prefix-mode 1)
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)))

;; enable useful command
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

;; UTF-8 please
(setq locale-coding-system   'utf-8) ; pretty
(set-terminal-coding-system  'utf-8) ; pretty
(set-keyboard-coding-system  'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system        'utf-8) ; with sugar on top

;; fix mod4 bug
(define-key special-event-map (kbd "<key-17>")   'ignore)
(define-key special-event-map (kbd "<M-key-17>") 'ignore)

;; commenting
(global-set-key (kbd "C-c SPC")   'comment-dwim)
(global-set-key (kbd "C-c C-SPC") 'comment-dwim)
(global-set-key (kbd "C-c c")     'comment-region)
(global-set-key (kbd "C-c u")     'uncomment-region)

;; code navigation
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)
(global-set-key (kbd "M-t")   'find-tag)

;; analogous to C-S-x
(global-set-key (kbd "C-S-M-x") 'eval-buffer)

;; undo
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "M-z") 'undo-tree-redo)
(global-set-key (kbd "M-n") 'undo-tree-undo)
(global-set-key (kbd "M-p") 'undo-tree-redo)

;; because we navigate via cursor keys, we can put something more useful on the default navigational keys
(global-set-key (kbd "C-n") 'other-window)
(global-set-key (kbd "C-p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-f") 'forward-word)
(global-set-key (kbd "C-b") 'backward-word)

;; obvious keys
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>")  'end-of-buffer)

;; make C-Backspace "work" in terminal
(global-set-key (kbd "S-<f7>") 'backward-kill-word)

;; goto and hint-style navigation
(setup-lazy '(ace-jump-mode ace-jump-char-mode ace-jump-line-mode) "ace-jump-mode")
(setup-lazy '(ace-jump-buffer) "ace-jump-buffer")
(setup-lazy '(ace-link) "ace-link")
(setup-lazy '(ace-window) "ace-window")

(global-set-key (kbd "M-g M-g") 'goto-line)
(global-set-key (kbd "M-g l")   'goto-line)
(global-set-key (kbd "M-g b")   'ace-jump-buffer)
(global-set-key (kbd "M-g c")   'ace-jump-char-mode)
(global-set-key (kbd "M-g g")   'ace-jump-mode)
(global-set-key (kbd "M-g s")   'ace-jump-line-mode)
(global-set-key (kbd "M-g w")   'ace-window)

(setup-after "ace-window"
  ;; help pages don't have other input, so skip the M-g prefix
  (setup "info"
    (define-key Info-mode-map "l" 'ace-link-info))
  (setup "help-mode"
    (define-key help-mode-map "l" 'ace-link-help)))

;; get out of recursive edit
(global-set-key (kbd "C-c C-g") 'abort-recursive-edit)

;; allowed key components
(setup "free-keys"
  (setq free-keys-keys
        (concat "abcdefghijklmnopqrstuvwxyzßł"
                "ABCDEFGHIJKLMNOPQRSTUVWXYZẞŁ"
                "1234567890"
                "!@#$%^&*()-=[]{};'\\:\"|,./<>?`~_+"
                )))

;; unset unwanted default keys, so they show up in free-keys
(cl-loop for key in `(
                   (,(kbd "C-x C-z")          suspend-frame)
                   (,(kbd "C-z")              suspend-frame)
                   ([(insert)]                overwrite-mode)
                   ([(insertchar)]            overwrite-mode)
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
                   (,(kbd "C-x l")            count-lines-page)
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
                   )
         collect (if (eq (key-binding (first key)) (second key))
                     (global-unset-key (first key))))

;; multiple cursors
(setq mc/list-file "~/.emacs.d/mc-lists.el")
(setup "multiple-cursors"
  (global-set-key (kbd "C-c d")            'mc/edit-lines)
  (global-set-key (kbd "<C-down>")         'mc/mark-next-like-this)
  (global-set-key (kbd "<C-up>")           'mc/mark-previous-like-this)
  (global-set-key (kbd "<M-C-down>")       'mc/skip-to-next-like-this)
  (global-set-key (kbd "<M-C-up>")         'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-c C-d")          'mc/mark-all-dwim)
  (global-set-key (kbd "C-c >")            'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-c <")            'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-S-<mouse-1>")    'mc/add-cursor-on-click)
  (global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click))

(setup-lazy '(phi-search phi-search-backward) "phi-search")
(global-set-key (kbd "C-c C-s") 'phi-search)
(global-set-key (kbd "C-c C-r") 'phi-search-backward)

(setup-after "phi-search"
  (setup "phi-search-mc"
    (define-key phi-search-default-map (kbd "<C-down>") 'phi-search-mc/mark-next)
    (define-key phi-search-default-map (kbd "<C-up>")   'phi-search-mc/mark-previous)
    (define-key phi-search-default-map (kbd "C-c C-k")  'phi-search-mc/mark-all)))

;; <ret> inserts a newline; C-j exits (a bit more convenient that way)
(setup-after "multiple-cursors-core"
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-j") 'multiple-cursors-mode))

;; support for bookmarks (broken; resurrect this at some point...)
;; (require 'breadcrumb)
;; (global-set-key (kbd "C-c m") 'bc-set)
;; (global-set-key (kbd "M-SPC") 'bc-previous)
;; (global-set-key (kbd "M-S-SPC") 'bc-next)
;; (setq bc-bookmark-limit 1000)
;; (setq bc-bookmark-file (expand-file-name "~/.emacs.d/cache/breadcrumb"))
;; ;; normal bookmarks
;; (setq bookmark-default-file "~/.emacs.d/cache/bookmarks")

;; undo-tree like in vim
(setup "undo-tree"
  (global-undo-tree-mode))

;; nested parentheses are highlighted when inside of them
(setup "highlight-parentheses"
  (defun turn-on-highlight-parentheses () (highlight-parentheses-mode 1))
  (add-hook 'prog-mode-hook 'turn-on-highlight-parentheses)
  (add-hook 'enh-ruby-mode-hook 'turn-on-highlight-parentheses)
  (add-hook 'text-mode-hook 'turn-on-highlight-parentheses))

;; better search/replace
(setup-lazy '(vr/query-replace vr/query-replace-from-beginning) "visual-regexp")

(setup-after "visual-regexp"
 (setup "visual-regexp-steroids"))

(defun vr/query-replace-from-beginning ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'vr/query-replace)))

(global-set-key (kbd "C-c r") 'vr/query-replace)
(global-set-key (kbd "C-c R") 'vr/query-replace-from-beginning)


;; copy end of line, like C-k
(defun copy-line ()
  (interactive)
  (set 'this-command 'copy-to-kill)
  (save-excursion
    (set-mark (point))
    (if (= (point) (line-end-position))
        (forward-line)
      (goto-char (line-end-position)))
    (if (eq last-command 'copy-to-kill)
        (append-next-kill))
    (kill-ring-save (mark) (point))))
(global-set-key "\M-k" 'copy-line)

;; append to last kill
;; (global-set-key (kbd "C-c C-w") (lambda () (interactive) (append-next-kill) (whole-line-or-region-kill-region 0)))
;; (global-set-key (kbd "C-c w")   (lambda () (interactive) (append-next-kill) (whole-line-or-region-kill-ring-save)))

;; move to beginning of text on line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line. If point was
already at that position, move point to beginning of line.

If visual-line-mode is on, then also jump to beginning of real line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point))
        (vispos (point)))

    (beginning-of-visual-line)
    (setq vispos (point))
    (beginning-of-line-text)

    (if (and (> vispos (point))
             (not (= oldpos vispos)))
        (goto-char vispos)
      (when (= oldpos (point))
        (beginning-of-line)))))
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(defun smart-end-of-line ()
  "Move point to end of visual line or, if already there, to end of logical line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))

    (end-of-visual-line)
    (when (= oldpos (point))
      (end-of-line))))
(global-set-key (kbd "C-e") 'smart-end-of-line)

;; org-mode has similar behavior built-in, so use it instead
(setup-after "org-mode"
  (setq org-special-ctrl-a/e t))

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to its full path."
  (interactive "*fInsert file name: \nP")
  (cond ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert (file-relative-name filename)))))
(global-set-key "\C-c\C-i" 'insert-file-name)

;; ido and smex (ido for M-x)
(setup "ido"
  (ido-mode 1)

  (setq ido-enable-flex-matching t) ; fuzzy matching
  (setq ido-use-filename-at-point nil)
  (setq ido-use-url-at-point nil)
  (global-set-key "\C-x\M-f" 'find-file-at-point)
  (setq ido-use-virtual-buffers t)
  (setq ido-default-file-method 'selected-window) ; ignore buffers in different frames
  (setq ido-default-buffer-method 'selected-window) ; ignore buffers in different frames
  (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
  (setq ido-ignore-buffers
        '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
          "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))
  (setq ido-case-fold t) ; case insensitive
  (setq ido-enable-last-directory-history t)
  (setq ido-max-work-directory-list 30)
  (setq ido-max-work-file-list 100)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-directory-size 1000000) ; load bigger dirs, too
  (setq confirm-nonexistent-file-or-buffer nil))

(setup-after "ido"
  (setup "ido-ubiquitous"
    (setq ido-everywhere t)))

;; smex
(setup "smex"
  (setq smex-save-file "~/.emacs.d/cache/smex-items")
  (smex-initialize)
  (global-set-key "\M-x" 'smex)
  (global-set-key "\M-X" 'smex-major-mode-commands))

;; recent files
(setup "recentf"
  (setq recentf-max-saved-items 1000)
  (setq recentf-save-file "~/.emacs.d/cache/recentf")
  (setq recentf-exclude (append recentf-exclude
                                '("\.emacs\.d/cache"
                                  "\.emacs\.d/packages")))
  (recentf-mode 1)

  ;; file completion
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))
  (global-set-key "\C-x\C-r" 'recentf-ido-find-file))

;; use regexp search and selected region (if any) by default
(defun region-as-string ()
  (buffer-substring (region-beginning)
                    (region-end)))

(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'regexp-search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward-regexp))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'regexp-search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward-regexp))

(global-set-key (kbd "C-s")   'isearch-forward-use-region)
(global-set-key (kbd "C-r")   'isearch-backward-use-region)
(global-set-key (kbd "C-S-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)
;; make backspace more intuitive
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

;; normalize search string so unicode diacritics work normally
(defun isearch-normalize-string ()
  (interactive)
  (let* ((string (ucs-normalize-NFKC-string isearch-string)))
    (setq isearch-string string
          isearch-message (mapconcat 'isearch-text-char-description string ""))
    (isearch-search-and-update)))
(define-key isearch-mode-map (kbd "C-c C-c") 'isearch-normalize-string)
(define-key isearch-mode-map (kbd "C-c C-w") 'isearch-toggle-word)
(define-key isearch-mode-map (kbd "C-c C-r") 'isearch-toggle-regexp)
(define-key isearch-mode-map (kbd "C-c C-i") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-c C-s") 'isearch-toggle-symbol)
(define-key isearch-mode-map (kbd "C-c C-SPC") 'isearch-toggle-lax-whitespace)
(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)

;; wrap search
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; unique buffer names
(setup "uniquify"
  (setq uniquify-buffer-name-style 'post-forward))

;; make mouse more usable
(mouse-wheel-mode t)
(setq make-pointer-invisible t)
(setq mouse-yank-at-point t)

;; yet another savior
(defun jesus ()
  "Because Jesus saves."
  (interactive)
  (save-buffer))

;; indentation
(setq-default tab-width 2)
(global-set-key (kbd "C-c i") 'indent-region)
;; don't use tabs normally, except for a few special modes
(setq-default indent-tabs-mode nil)
(defun use-tabs () (setq indent-tabs-mode t))
(add-hook 'notes-mode-hook 'use-tabs)
;; insert literal tab
(global-set-key (kbd "<C-tab>") (lambda () (interactive)
                                  (insert "\t")))

;; replacement for orgtbl by using " | " as separator
(setup-lazy '(delimit-columns-current delimit-columns-region) "delim-col"
  (setq delimit-columns-str-separator " | ")
  (setq delimit-columns-separator " +| +")
  (setq delimit-columns-format 'separator)
  (setq delimit-columns-extra nil))

(defun delimit-columns-current ()
  "Delimit columns of current table."
  (interactive)

  (let ((beg)
        (end)
        (start)
        (regexp "^[^ \t\n]+.* | .*$"))
    (save-excursion
      ;; initialize region on the current line
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))

      (setq start beg)

      ;; find beginning of block
      (goto-char start)
      (while (and (looking-at regexp)
                  (not (bobp)))
        (setq beg (point))
        (forward-line -1))

      ;; find end of block
      (goto-char start)
      (while (and (looking-at regexp)
                  (not (eobp)))
        (end-of-line)
        (setq end (point))
        (forward-line 1))
      )

    ;; call alignment function
    (delimit-columns-region beg end)
    ))
(global-set-key (kbd "C-c t") 'delimit-columns-current)
(global-set-key (kbd "C-c T") 'delimit-columns-region)

;; automatically indent on return, except in a few modes that have similar stuff by default
(electric-indent-mode 1)
;; TODO should be a generic macro or something
(defun no-electric-indent-yaml ()
  (electric-indent-mode -1)
  (define-key yaml-mode-map [(return)] 'newline-and-indent))
(add-hook 'yaml-mode-hook 'no-electric-indent-yaml)
(defun no-electric-indent-python ()
  (electric-indent-mode -1)
  (define-key python-mode-map [(return)] 'newline-and-indent))
(add-hook 'python-mode-hook 'no-electric-indent-python)

;; also indent when yanked
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key "\C-y" 'yank-and-indent)

;; undo hardwrapped regions (mostly markdown)
(defun unfill-region (begin end)
  "Remove all line breaks in a region but leave paragraphs,
  indented text (quotes, code) and lists intact."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\\>-\n]\\)" "\\1 \\2" nil begin end))
(global-set-key "\M-Q" 'unfill-region)

;; insert new line *after* the current one
(defun next-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'next-newline-and-indent)

;; delete spaces when killing a line
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
  Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; delete all space before point up to beginning of line or non-whitespace char
(setup "hungry-delete"
  (global-hungry-delete-mode)
  (defun literal-delete-char (&optional arg)
    (interactive "P")
    (delete-char 1))
  (defun literal-delete-backward-char (&optional arg)
    (interactive "P")
    (delete-backward-char 1))
  (global-set-key (kbd "C-d") 'literal-delete-char)
  (global-set-key (kbd "M-d") 'literal-delete-backward-char))

;; spell checker
(setup "wcheck-mode"
  (setq ispell-really-hunspell t)
  (setq  wcheck-timer-idle .2)
  (define-key global-map "\C-cs" 'wcheck-actions)
  (setq-default
   wcheck-language "English"
   wcheck-language-data '(("English"
                           (program . "/usr/bin/enchant")
                           (args . ("-l" "-d" "en_US"))
                           (action-program . "/usr/bin/enchant")
                           (action-args "-a" "-d" "en_US")
                           (action-parser . enchant-suggestions-menu))
                          ("German"
                           (program . "/usr/bin/enchant")
                           (args . ("-l" "-d" "de"))
                           (action-program . "/usr/bin/enchant")
                           (action-args "-a" "-d" "de")
                           (action-parser . enchant-suggestions-menu))
                          ("French"
                           (program . "/usr/bin/enchant")
                           (args . ("-l" "-d" "fr"))
                           (action-program . "/usr/bin/enchant")
                           (action-args "-a" "-d" "fr")
                           (action-parser . enchant-suggestions-menu))
                          ))
  ;; add to dictionary functionality
  (defun enchant-suggestions-menu (marked-text)
    (cons (cons "[Add]" 'enchant-add-to-dictionary)
          (wcheck-parser-ispell-suggestions)))

  (defvar enchant-dictionaries-dir "~/.config/enchant")

  (defun enchant-add-to-dictionary (marked-text)
    (let* ((word (aref marked-text 0))
           (language (aref marked-text 4))
           (file (let ((code (nth 1 (member "-d" (wcheck-query-language-data
                                                  language 'action-args)))))
                   (when (stringp code)
                     (concat (file-name-as-directory enchant-dictionaries-dir)
                             code ".dic")))))

      (when (and file (file-writable-p file))
        (with-temp-buffer
          (insert word) (newline)
          (append-to-file (point-min) (point-max) file)
          (message "Added word \"%s\" to the %s dictionary"
                   word language)))))

  ;; make it possible to toggle wcheck on/off globally
  ;; TODO have it disable wcheck in open buffers too
  (defvar use-spell-check t)
  (defun disable-spell-check ()
    "turns spell-check off globally"
    (interactive)
    (setq use-spell-check nil)
    (dolist (buffer (buffer-list))
      (wcheck-buffer-lang-proc-data-update buffer nil))
    (wcheck-mode 0))
  (defun enable-spell-check ()
    "turns spell-check off globally"
    (interactive)
    (setq use-spell-check t)
    (wcheck-mode 1))
  (global-set-key (kbd "C-c <f5>")   'disable-spell-check)
  (global-set-key (kbd "C-c C-<f5>") 'enable-spell-check)
  (global-set-key (kbd "C-c <f6>")   'wcheck-mode)

  (defun turn-on-spell-check ()
    (if use-spell-check (wcheck-mode 1)))

  ;; enable spell-check in certain modes
  (add-hook 'markdown-mode-hook 'turn-on-spell-check)
  (add-hook 'org-mode-hook      'turn-on-spell-check)
  )

;; disable version control in emacs
(setup "vc"
  (setq vc-handled-backends ()))

;; edit symbol in multiple places simultaneously
(setup "iedit"
  (global-set-key (kbd "C-c ;") 'iedit-mode)
  (global-set-key (kbd "C-c C-;") 'iedit-mode-toggle-on-function))

;; align
(setup "align"
  ;; definitions for ruby code
  ;; fixes the most egregious mistake in detecting regions (hashes), but should be properly generalized at some point
  (setq align-region-separate "\\(^\\s-*[{}]?\\s-*$\\)\\|\\(=\\s-*[][{}()]\\s-*$\\)")
  (defconst align-ruby-modes '(enh-ruby-mode)
    "align-perl-modes is a variable defined in `align.el'.")
  (defconst ruby-align-rules-list
    '((ruby-comma-delimiter
       (regexp . ",\\(\\s-*\\)[^/ \t\n]")
       (modes . '(enh-ruby-mode))
       (repeat . t))
      (ruby-string-after-func
       (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
       (modes . '(enh-ruby-mode))
       (repeat . t))
      (ruby-symbol-after-func
       (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
       (modes . '(enh-ruby-mode))))
    "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")
  (add-to-list 'align-perl-modes         'enh-ruby-mode)
  (add-to-list 'align-dq-string-modes    'enh-ruby-mode)
  (add-to-list 'align-sq-string-modes    'enh-ruby-mode)
  (add-to-list 'align-open-comment-modes 'enh-ruby-mode)
  (dolist (it ruby-align-rules-list)
    (add-to-list 'align-rules-list it))
  ;; haskell alignments
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))

  (defun align-region-or-current ()
    "Align current selected region or implied region if nothing is selected."
    (interactive)
    (if (and mark-active
             (/= (point) (mark)))
        (align (point) (mark))
      (align-current)))
  ;; align current region
  (global-set-key (kbd "C-c =") 'align-region-or-current)

  ;; repeat regex (teh fuck ain't that the default?!)
  (defun align-repeat (start end regexp)
    "Repeat alignment with respect to the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end
                  (concat "\\(\\s-*\\)" regexp) 1 1 t))
  (global-set-key (kbd "C-c C-=") 'align-repeat))

;; diff- mode (better colors)
(setup-lazy '(diff-mode) "diff-mode-")

;; a slightly saner diff command
(setup-lazy '(ediff-mode) "ediff"
  (setq ediff-diff-options "-w"))

;; if no region is active, act on current line
(setup "whole-line-or-region"
  (setq whole-line-or-region-extensions-alist
        '((comment-dwim whole-line-or-region-comment-dwim-2 nil)
          (copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
          (kill-region whole-line-or-region-kill-region nil)
          (kill-ring-save whole-line-or-region-kill-ring-save nil)
          (yank whole-line-or-region-yank nil)
          ))
  (whole-line-or-region-mode 1))

;; tramp (remote files)
(setup-after "tramp"
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name "~/.emacs.d/cache/tramp")
  ;; cookies
  (setq url-cookie-file "~/.emacs.d/cache/url/cookies"))

;; input methods, including a direct mozc binding to avoid ibus (requires mozc install)
(setup  "custom-input-methods")
(setup "mozc" (setq mozc-leim-title "あ"))

(global-set-key (kbd "C-c 1") (lambda () (interactive) (set-input-method nil)))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (set-input-method "muflax-cyrillic")))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (set-input-method "muflax-turkish")))
(global-set-key (kbd "C-c 4") (lambda () (interactive) (set-input-method "muflax-greek")))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (set-input-method "japanese-mozc")))
(global-set-key (kbd "C-c C-m") 'toggle-input-method)
(global-set-key (kbd "<kanji>") 'toggle-input-method)

;; analog to delete-file
(defun delete-current-file ()
  "Delete the file associated with the current buffer."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (y-or-n-p (concat "Delete file: " currentFile))
      (kill-buffer (current-buffer))
      (delete-file currentFile)
      (message (concat "Deleted file: " currentFile))
      )))

;; move lines like in org-mode
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>")   'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; move buffers
(setup "buffer-move"
  (global-set-key (kbd "<C-S-up>")    'buf-move-up)
  (global-set-key (kbd "<C-S-down>")  'buf-move-down)
  (global-set-key (kbd "<C-S-left>")  'buf-move-left)
  (global-set-key (kbd "<C-S-right>") 'buf-move-right))

;; undo window changes
(setup "winner"
  (winner-mode 1))

;; expand-region
(setup-lazy '(er/expand-region er/contract-region) "expand-region"
  (global-set-key (kbd "<C-prior>") 'er/expand-region)
  (global-set-key (kbd "<C-next>")  'er/contract-region))

;; make zsh aliases work
(setup "shell-command"
  (setq shell-command-switch "-lc")
;; tab-completion for shell-command
;; FIXME not working yet, but meh
  (shell-command-completion-mode)

  ;; better handling than M-| / M-!
  (defun chomp (str)
    "Chomp leading and tailing whitespace from STR."
    (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                         str)
      (setq str (replace-match "" t t str)))
    str)

  (defun generalized-shell-command (command arg)
    "Unifies `shell-command' and `shell-command-on-region'.
You have:
- (no arg) run command and place output
- (C-u)    ... don't chomp output
- (region) replace region with output from command
- (C-u region) ... and print to minibuffer" ;; TODO: make this also chomp
    (interactive (list (read-from-minibuffer "$ " nil nil nil 'shell-command-history)
                       current-prefix-arg))
    (let ((p (if mark-active (region-beginning) 0))
          (m (if mark-active (region-end) 0)))
      (if (= p m)
          ;; no active region, so just output the output
          (if (eq arg nil)
              (insert (chomp (shell-command-to-string command)))
            (shell-command command t))
        ;; Active region
        (if (eq arg nil)
            (shell-command-on-region p m command t t)
          (shell-command-on-region p m command)))))
  (global-set-key (kbd "C-|") 'generalized-shell-command)
  (global-set-key (kbd "C-\\") 'generalized-shell-command) ; terminal bug
  )

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; highlight some whitespace
(setup-lazy '(whitespace-mode global-whitespace-mode) "whitespace"
  (setq whitespace-style (quote (face tabs tab-mark))))

;; scratchpad buffers
(setup-lazy '(scratch) "scratch"
  (global-set-key (kbd "C-c C-b") 'scratch))

;; don't spam *Scratch*
(setq initial-scratch-message nil)

;; increment / decrement thing at point
(setup-lazy '(increment-integer-at-point decrement-integer-at-point) "increment"
  (global-set-key (kbd "C-c C-+") 'increment-integer-at-point)
  (global-set-key (kbd "C-c C--") 'decrement-integer-at-point))

;; rotate / toggle text
(setup-lazy '(rotate-text) "rotate-text"
  (add-to-list 'rotate-text-words   '("auto" "manual"))
  (add-to-list 'rotate-text-words   '("if" "unless"))
  (add-to-list 'rotate-text-words   '("map" "each"))
  (add-to-list 'rotate-text-words   '("on" "off"))
  (add-to-list 'rotate-text-words   '("select" "reject"))
  (add-to-list 'rotate-text-words   '("t" "nil"))
  (add-to-list 'rotate-text-words   '("true" "false"))
  (add-to-list 'rotate-text-words   '("var" "const"))
  (add-to-list 'rotate-text-words   '("yes" "no"))
  (add-to-list 'rotate-text-symbols '("?" "!")))
(global-set-key (kbd "C-c C-t") 'rotate-text)

;; handle camelcase better
(global-subword-mode 1)

;; folding
(setup "hideshow"
  (add-hook 'enh-ruby-hook   'hs-minor-mode)
  (add-hook 'notes-mode-hook 'hs-minor-mode))

(setup-after "hideshow"
  (setup-lazy '(hideshowvis-enable hideshowvis-minor-mode) "hideshowvis")
  (setup "fold-dwim"
    (define-key global-map (kbd "C-c C-f") 'fold-dwim-toggle)
    (define-key global-map (kbd "C-c f")   'fold-dwim-hide-all)
    (define-key global-map (kbd "C-c F") 'fold-dwim-show-all)))

;; fast navigation
(setup "imenu"
  (set-default 'imenu-auto-rescan t)

  (defun imenu-flush-cache ()
    "Flushes imenu cache."
    (interactive)
    (setq imenu--index-alist nil)))

(setup-after "imenu"
  (setup "idomenu"
    (define-key global-map (kbd "C-c [")   'idomenu)
    (define-key global-map (kbd "C-c C-[") 'idomenu))
  (setup "imenu-anywhere"
    (define-key global-map (kbd "C-c ]")   'imenu-anywhere)
    (define-key global-map (kbd "C-c C-]") 'imenu-anywhere)))

;; recentering
(setq recenter-positions '(2 middle))
(add-hook 'imenu-after-jump-hook 'recenter-top-bottom)

;; smart parentheses
(setup "smartparens-config"
  (smartparens-global-mode t)
  (setq sp-autoescape-string-quote nil)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-autoinsert-pair nil)
  (setq sp-autoskip-closing-pair nil)
  (setq sp-autodelete-pair nil)
  ;; parenthesis highlighting behavior
  (show-paren-mode 1)
  (setq blink-matching-paren-distance nil)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  ;; keybindings
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "M-f") 'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-b") 'sp-backward-symbol)
  (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
  (define-key sp-keymap (kbd "C-<left>") 'sp-add-to-next-sexp)
  (define-key sp-keymap (kbd "C-<right>") 'sp-add-to-previous-sexp)
  (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
  (define-key sp-keymap (kbd "C-c k") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "C-c C-k") 'sp-rewrap-sexp)
  (define-key sp-keymap (kbd "S-<left>") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "S-<right>") 'sp-select-next-thing)
  (define-key sp-keymap (kbd "C-c |") 'sp-split-sexp)
  (define-key sp-keymap (kbd "C-c C-|") 'sp-join-sexp)

  ;; move to beginning of text on line
  (defun sp-kill-to-end-of-sexp ()
    "Kill everything in the sexp without unbalancing it."
    (interactive)
    (save-excursion
      (set-mark (point))
      (sp-end-of-sexp)
      (kill-region (mark) (point))))
  ;; move to beginning of text on line
  (defun sp-kill-to-beginning-of-sexp ()
    "Kill everything in the sexp without unbalancing it."
    (interactive)
    (save-excursion
      (set-mark (point))
      (sp-beginning-of-sexp)
      (kill-region (mark) (point))))
  ;; move to beginning of text on line
  (defun sp-copy-to-end-of-sexp ()
    "Copy everything in the sexp without unbalancing it."
    (interactive)
    (save-excursion
      (set-mark (point))
      (sp-end-of-sexp)
      (kill-ring-save (mark) (point))))
  ;; move to beginning of text on line
  (defun sp-copy-to-beginning-of-sexp ()
    "copy everything in the sexp without unbalancing it."
    (interactive)
    (save-excursion
      (set-mark (point))
      (sp-beginning-of-sexp)
      (kill-ring-save (mark) (point))))
  (define-key sp-keymap (kbd "C-c a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c e") 'sp-end-of-sexp)
  (define-key sp-keymap (kbd "C-c C-a") 'sp-kill-to-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c C-e") 'sp-kill-to-end-of-sexp)
  (define-key sp-keymap (kbd "C-c M-a") 'sp-copy-to-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c M-e") 'sp-copy-to-end-of-sexp)

  ;; markdown-mode
  (sp-with-modes '(markdown-mode)
    (sp-local-pair "*" "*" :actions '(wrap autoskip))
    (sp-local-pair "_" "_" :actions '(wrap autoskip)))

  ;; notes-mode
  (sp-with-modes '(notes-mode)
    (sp-local-pair "*" "*" :actions '(wrap autoskip))
    (sp-local-pair "/" "/" :actions '(wrap autoskip))
    (sp-local-pair "<" ">" :actions '(wrap autoskip))
    (sp-local-pair "[" "]")
    (sp-local-pair "'" "'" :actions '(wrap autoskip))
    (sp-local-pair "`" "`" :actions '(wrap autoskip)))

  ;; overwrite |pipe| handling in ruby
  (sp-with-modes '(enh-ruby-mode ruby-mode)
    (sp-local-pair "|" "|" :pre-handlers nil))
  )

;; perspectives / workspaces (has to be loaded late)
;; FIXME broken
;; (setup "persp-mode"
;;   (setq persp-save-dir (expand-file-name "~/.emacs.d/cache/persp-confs"))
;;   (setq persp-set-last-persp-for-new-frames nil)
;;   (setq persp-auto-save-num-of-backups 10)
;;   (persp-mode t))

;; normalize unicode in buffer
(defun normalize-unicode-in-buffer ()
  "Normalize Unicode in buffer to NFKC form."
  (interactive)
  (save-excursion
    (ucs-normalize-NFKC-region (point-min) (point-max))))

;; normalize buffer before saving
(add-hook 'before-save-hook 'normalize-unicode-in-buffer)

;; clean up buffers every once in a while
(setup-lazy '(clean-buffer-list) "midnight")

;; don't use shift to mark things; smartparens overwrites this anyway, but be explicit about it
(setq shift-select-mode nil)

;; transparently open compressed files
(auto-compression-mode t)

;; normally smartparens wraps selected text, but if input is not a pair, just overwrite the text
(delete-selection-mode 1)

;; when popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; automatically create directories if necessary
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; help for more obscure prefix keys
(setup "guide-key"
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x c"))
  (guide-key-mode 1)
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom))

;; helm
(setup "helm-config")
  ;; (helm-mode t))
(setup-after "helm"
  (define-key helm-map (kbd "C-w")  'subword-backward-kill)
  (define-key helm-map (kbd "M-w")  'helm-yank-text-at-point)
  (global-set-key (kbd "C-x c t")   'helm-cmd-t)
  (global-set-key (kbd "C-x c g")   'helm-do-grep)
  (global-set-key (kbd "C-x c o")   'helm-occur)
  (global-set-key (kbd "C-x c C-o") 'helm-swoop)
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-ff-lynx-style-map nil
        helm-input-idle-delay 0.1
        helm-idle-delay 0.1
        helm-follow-mode-persistent t ))

(provide 'editing)
