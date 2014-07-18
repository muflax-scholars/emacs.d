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

;; save location inside buffer
(setup "saveplace"
  ;; save minibuffer history
  (savehist-mode 1)
  (setq kill-ring-max          1000)
  (setq history-length         1000)
  (setq search-ring-max        1000)
  (setq regexp-search-ring-max 1000)
  (setq kmacro-ring-max        1000)
  (setq save-place-file "~/.emacs.d/cache/saveplace")
  (setq-default save-place t)
  (setq savehist-file "~/.emacs.d/cache/history")
  (setq savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        kill-ring
                                        compile-command)))

;; save open buffers etc.
(setup "desktop"
  (setq desktop-path "~/.emacs.d/cache/desktop/")
  ;; (desktop-save-mode 1)
  )

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

;; reliable keys
(global-set-key (kbd "<f9>")  'eval-defun)
(global-set-key (kbd "<f10>") 'eval-buffer)
(global-set-key (kbd "<f11>") 'eval-expression)

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

;; get out of recursive edit
(global-set-key (kbd "C-c C-g") 'abort-recursive-edit)

;; save some strokes
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "S-<f2>") 'save-some-buffers)

;; mark
(global-set-key (kbd "S-<SPC>") 'set-mark-command)

;; make DEL always work like intended
(normal-erase-is-backspace-mode 1)

;; allowed key components
(setup-lazy '(free-keys) "free-keys"
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
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/cache/undo/")))
  (setq undo-tree-visualizer-timestamps t))

;; nested parentheses are highlighted when inside of them
(setup "highlight-parentheses"
  (defun turn-on-highlight-parentheses () (highlight-parentheses-mode 1))
  (add-hook 'prog-mode-hook     'turn-on-highlight-parentheses)
  (add-hook 'enh-ruby-mode-hook 'turn-on-highlight-parentheses))

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

;; move to beginning of text on line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line. If point was
already at that position, move point to beginning of line.

If visual-line-mode is on, then also jump to beginning of real line."
  (interactive)
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
  (interactive)
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
        (regexp "^[ \t]*[^ \t\n]+.* | .*$"))
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
    (delete-trailing-whitespace beg end)
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

(defun next-newline-and-indent ()
  "Insert new line *after* the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "C-O") 'next-newline-and-indent)

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (save-excursion
    (setq line (thing-at-point 'line))
    (end-of-line)
    (if (eobp)
        (newline)
      (forward-line))
    (insert line))
  (next-line))
(global-set-key (kbd "C-<return>") 'duplicate-line)

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
  (global-set-key (kbd "<S-delete>") 'literal-delete-char)
  (global-set-key (kbd "<S-backspace>") 'literal-delete-backward-char))

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

(defun kill-without-append (&optional arg)
  "kills line (region or whole) without appending it to the last kill"
  (interactive "P")
  (setq last-command nil)
  (whole-line-or-region-kill-region arg))

(defun blank-line ()
  "intelligently blanks the line"
  (interactive)
  (smart-beginning-of-line)
  (kill-line))

(defun kill-with-append (&optional arg)
  "kills line (region or whole) and appends it to last kill"
  (interactive "P")
  (append-next-kill)
  (whole-line-or-region-kill-region arg))

(defun copy-with-append (&optional arg)
  "kills line (region or whole) and appends it to last kill"
  (interactive "P")
  (append-next-kill)
  (whole-line-or-region-kill-ring-save arg))

(global-set-key (kbd "C-d")     'kill-without-append)
(global-set-key (kbd "M-d")     'blank-line)
(global-set-key (kbd "C-c C-w") 'kill-with-append)
(global-set-key (kbd "C-c w")   'copy-with-append)

;; tramp (remote files)
(setup-after "tramp"
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name "~/.emacs.d/cache/tramp")
  ;; cookies
  (setq url-cookie-file "~/.emacs.d/cache/url/cookies"))

;; input methods, including a direct mozc binding to avoid ibus (requires mozc install)
(setup "custom-input-methods")
(setup "mozc" (setq mozc-leim-title "あ"))

(global-set-key (kbd "C-c 0") (lambda () (interactive) (set-input-method nil)))
(global-set-key (kbd "C-c 1") (lambda () (interactive) (set-input-method "muflax-latin")))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (set-input-method "muflax-cyrillic")))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (set-input-method "muflax-turkish")))
(global-set-key (kbd "C-c 4") (lambda () (interactive) (set-input-method "muflax-greek")))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (set-input-method "japanese-mozc")))

(global-set-key (kbd "C-c C-m") 'toggle-input-method)
(global-set-key (kbd "<kanji>") 'toggle-input-method)

;; default to the diacritic smasher
(setq default-input-method "muflax-latin")
(defun turn-on-default-input-method ()
  (set-input-method default-input-method))
(add-hook 'text-mode-hook        'turn-on-default-input-method)
(add-hook 'prog-mode-hook        'turn-on-default-input-method)
(add-hook 'dired-mode-hook       'turn-on-default-input-method)
(add-hook 'minibuffer-setup-hook 'turn-on-default-input-method)
(add-hook 'occur-mode-hook       'turn-on-default-input-method)
(add-hook 'phi-search-init-hook  'turn-on-default-input-method)

;; don't underline partial input
(setq input-method-highlight-flag nil)

;;don't spam the minibuffer
(setq input-method-verbose-flag 'complex-only)

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
(setup-lazy '(md/move-lines-up md/move-lines-down) "move-dup")
(global-set-key (kbd "M-<up>")   'md/move-lines-up)
(global-set-key (kbd "M-<down>") 'md/move-lines-down)

;; move buffers
(setup "buffer-move"
  (global-set-key (kbd "<C-S-up>")    'buf-move-up)
  (global-set-key (kbd "<C-S-down>")  'buf-move-down)
  (global-set-key (kbd "<C-S-left>")  'buf-move-left)
  (global-set-key (kbd "<C-S-right>") 'buf-move-right))

;; undo window changes
(setup "winner"
  (winner-mode 1))

;; expand-region to mark stuff
(setup  "expand-region"
  (global-set-key (kbd "<C-prior>") 'er/expand-region)
  (global-set-key (kbd "<C-next>")  'er/contract-region)
  (global-set-key (kbd "S-<left>")  'er/mark-defun)
  (global-set-key (kbd "S-<right>") 'er/mark-symbol))

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

;; remove trailing whitespace on save, unless it's a big buffer
;; TODO make this fast so we can always run it
(defun maybe-trim-whitespace ()
  (when (not (> (buffer-size) 1000000))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'maybe-trim-whitespace)

;; highlight some whitespace
(setup-lazy '(whitespace-mode global-whitespace-mode) "whitespace"
  (setq whitespace-style (quote (face tabs tab-mark))))

;; scratchpad buffers
(setup-lazy '(scratch) "scratch")
(global-set-key (kbd "C-c C-b") 'scratch)

;; don't spam *Scratch*
(setq initial-scratch-message nil)

;; increment / decrement thing at point
(setup-lazy '(increment-integer-at-point decrement-integer-at-point) "increment")
(global-set-key (kbd "C-c C-+") 'increment-integer-at-point)
(global-set-key (kbd "C-c C--") 'decrement-integer-at-point)

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
  (add-hook 'prog-mode-hook  'hs-minor-mode)
  (add-hook 'notes-mode-hook 'hs-minor-mode))

(setup-after "hideshow"
  (setup-lazy '(hideshowvis-enable hideshowvis-minor-mode) "hideshowvis")
  (setup "fold-dwim"
    (define-key global-map (kbd "C-c C-f") 'fold-dwim-toggle)
    (define-key global-map (kbd "C-c f")   'hs-hide-level)
    (define-key global-map (kbd "C-c F")   'fold-dwim-show-all)))

;; parenthesis highlighting behavior
(show-paren-mode 1)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

;; smart parentheses
(setup "smartparens-config"
  (smartparens-global-mode t)
  (setq sp-autoescape-string-quote nil)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-autoinsert-pair nil)
  (setq sp-autoskip-opening-pair nil)
  (setq sp-autoskip-closing-pair nil)
  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-autodelete-pair nil)

  ;; navigation
  (define-key sp-keymap (kbd "C-c a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c e") 'sp-end-of-sexp)
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "M-f")   'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-b")   'sp-backward-symbol)

  ;; killing
  (define-key sp-keymap (kbd "C-c C-a") 'sp-kill-to-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c C-e") 'sp-kill-to-end-of-sexp)
  (define-key sp-keymap (kbd "C-c M-a") 'sp-copy-to-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c M-e") 'sp-copy-to-end-of-sexp)
  (define-key sp-keymap (kbd "C-c M-e") 'sp-copy-to-end-of-sexp)
  (define-key sp-keymap (kbd "C-M-k")   'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w")   'sp-copy-sexp)

  ;; wrapping
  (define-key sp-keymap (kbd "M-<delete>")    'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
  (define-key sp-keymap (kbd "C-c C-k")       'sp-rewrap-sexp)

  ;; adjusting
  (define-key sp-keymap (kbd "C-c C-<tab>") 'sp-indent-adjust-sexp)
  (define-key sp-keymap (kbd "C-c <tab>")   'sp-dedent-adjust-sexp)

  ;; narrowing
  (define-key sp-keymap (kbd "C-x n s") 'sp-narrow-to-sexp)
  ;; TODO generalize C-( to narrow-or-region
  (define-key sp-keymap (kbd "C-(")     'sp-narrow-to-sexp)
  (define-key sp-keymap (kbd "C-)")     'widen)

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

  ;; markdown-mode
  (sp-with-modes '(markdown-mode)
    (sp-local-pair "*" "*" :actions '(wrap))
    (sp-local-pair "_" "_" :actions '(wrap)))

  ;; notes-mode
  (sp-with-modes '(notes-mode)
    (sp-local-pair "*" "*" :actions '(wrap))
    (sp-local-pair "/" "/" :actions '(wrap))
    (sp-local-pair "<" ">" :actions '(wrap))
    (sp-local-pair "[" "]")
    (sp-local-pair "'" "'" :actions '(wrap))
    (sp-local-pair "`" "`" :actions '(wrap)))

  ;; overwrite |pipe| handling in ruby
  (sp-with-modes '(enh-ruby-mode ruby-mode)
    (sp-local-pair "|" "|" :pre-handlers nil))
  )

;; perspectives / workspaces (has to be loaded late)
;; FIXME stupid
;; (setup "persp-mode"
;;   (setq persp-save-dir (expand-file-name "~/.emacs.d/cache/persp-confs"))
;;   (setq persp-set-last-persp-for-new-frames nil)
;;   (setq persp-auto-save-num-of-backups 10)
;;   (setq wg-morph-on nil)
;;   (persp-mode t))

;; normalize unicode in buffer
(defun normalize-unicode-in-buffer ()
  "Normalize Unicode in buffer to NFKC form."
  (interactive)
  (save-excursion
    (ucs-normalize-NFC-region (point-min) (point-max))))

;; normalize buffer before saving
(add-hook 'before-save-hook 'normalize-unicode-in-buffer)

;; clean up buffers every once in a while
(setup-lazy '(clean-buffer-list) "midnight")

;; use shift to mark things
(setq shift-select-mode t)

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
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x c" "C-x C-k"))
  (guide-key-mode 1)
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom))

(setup-lazy '(keyboard-cat-mode) "keyboard-cat-mode")

(defun compact-blank-lines ()
  "replace multiple blank lines with a single one"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
      (replace-match "\n")
      (forward-char 1))))

(defun toggle-title-case ()
  "Toggle case of the word / region between lower case and title case."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]]") (put this-command 'state "lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "title"))
         (t (put this-command 'state "lower")))))

    (cond
     ((string= "lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "title"))
     ((string= "title" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "lower")))
    ))
(global-set-key (kbd "M-c") 'toggle-title-case)

(defun toggle-upcase ()
  "Toggle case of the word / region between lower case and upper case."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]]") (put this-command 'state "lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "upper"))
         (t (put this-command 'state "lower")))))

    (cond
     ((string= "lower" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "upper"))
     ((string= "upper" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "lower")))
    ))
(global-set-key (kbd "M-u") 'toggle-upcase)

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))

(defun copy-matching-lines (regexp &optional rstart rend interactive)
  "Copy lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command."
  (interactive
   (keep-lines-read-args "Copy lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (copy-region-as-kill (or rstart (line-beginning-position))
                             (or rend (point-max))))
      (kill-buffer))))

;; macro key bindings
(global-set-key (kbd "C-t C-t") 'insert-kbd-macro)
(global-set-key (kbd "C-t C-n") 'kmacro-name-last-macro)
(global-set-key (kbd "C-t C-b") 'kmacro-bind-to-key)

;; sticky windows
(setup "sticky-windows"
  (global-set-key (kbd "C-x 0") 'sticky-window-delete-window)
  (global-set-key (kbd "C-x 1") 'sticky-window-delete-other-windows)
  (global-set-key (kbd "C-x 9") 'sticky-window-keep-window-visible))

(setup-lazy '(neotree) "neotree"
  (setq neo-show-header nil))

(setup-lazy '(nav-minor-mode nav-global-mode) "nav-mode")
(global-set-key (kbd "<menu>")   'nav-minor-mode)
(global-set-key (kbd "C-<menu>") 'nav-global-mode)

(provide 'init-editing)
