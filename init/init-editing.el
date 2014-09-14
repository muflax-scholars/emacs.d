;; generic editing features

;; safety
(setq make-backup-files t)
(defvar autosave-dir (expand-file-name "~/.emacs.d/cache/autosave/"))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)
;; move files to trash instead
(setq delete-by-moving-to-trash t)

;; save location inside buffer
(setup "saveplace"
  ;; save minibuffer history
  (savehist-mode 1)
  (setq kill-ring-max         	1000)
  (setq history-length        	1000)
  (setq search-ring-max       	1000)
  (setq regexp-search-ring-max	1000)
  (setq kmacro-ring-max       	1000)
  (setq comint-input-ring-size	1000)

  (setq save-place-file	"~/.emacs.d/cache/saveplace")
  (setq savehist-file  	"~/.emacs.d/cache/history")

  (setq-default save-place t)
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
(setq undo-limit     	1000000)
(setq message-log-max	100000)
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
(put 'narrow-to-region	'disabled nil)
(put 'downcase-region 	'disabled nil)
(put 'upcase-region   	'disabled nil)

;; UTF-8 please
(setq locale-coding-system  	'utf-8) ; pretty
(set-terminal-coding-system 	'utf-8) ; pretty
(set-keyboard-coding-system 	'utf-8) ; pretty
(set-selection-coding-system	'utf-8) ; please
(prefer-coding-system       	'utf-8) ; with sugar on top

;; multiple cursors
(setq mc/list-file "~/.emacs.d/mc-lists.el")
(setup "multiple-cursors"
  (setup "mc-extras")
  (setup "mc-jump")

  (defun mc/many-to-one-yank ()
    "Yanks killed lines from multiple cursors into one position. Less messy than yank-rectangle."
    (interactive)
    (with-temp-buffer
      (yank-rectangle)
      (kill-ring-save (point-min) (point-max)))
    (yank))

  (defun mc/many-to-one-yank-indent ()
    "Yanks killed lines from multiple cursors into one position, and indents. See 'mc/many-to-one-yank'."
    (interactive)
    (mc/many-to-one-yank)
    (call-interactively 'indent-region))
  )

;; edit symbol in multiple places simultaneously
(setup-lazy '(iedit-mode iedit-mode-toggle-on-function) "iedit")

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
  (add-hook 'prog-mode-hook    	'turn-on-highlight-parentheses)
  (add-hook 'enh-ruby-mode-hook	'turn-on-highlight-parentheses))

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

(defun smart-end-of-line ()
  "Move point to end of visual line or, if already there, to end of logical line."
  (interactive)
  (let ((oldpos (point)))

    (end-of-visual-line)
    (when (= oldpos (point))
      (end-of-line))))

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

;; don't use tabs normally, except for a few special modes
(setq-default indent-tabs-mode nil)
(defun use-tabs () (setq indent-tabs-mode t))
(add-hook 'notes-mode-hook 'use-tabs)

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
        (begline)
        (endline)
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
    (setq begline (line-number-at-pos beg))
    (setq endline (line-number-at-pos end))
    (delimit-columns-region beg end)
    (save-excursion
      (goto-line begline)
      (setq beg (point))
      (goto-line endline)
      (end-of-line)
      (setq end (point)))
    (delete-trailing-whitespace beg end)
    ))

;; automatically indent on return
(electric-indent-mode 1)

;; also indent when yanked
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun unfill-region (begin end)
  "Remove all line breaks in a region but leave paragraphs,
  indented text (quotes, code) and lists intact."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\\>-\n]\\)" "\\1 \\2" nil begin end))

(defun next-newline-and-indent ()
  "Insert new line *after* the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; delete spaces when killing a line
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
  Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

;; delete all space before point up to beginning of line or non-whitespace char
(setup "hungry-delete"
  (global-hungry-delete-mode)
  (defun literal-delete-char (&optional arg)
    (interactive "P")
    (delete-char 1))
  (defun literal-delete-backward-char (&optional arg)
    (interactive "P")
    (delete-backward-char 1)))

;; spell checker
(setup "wcheck-mode"
  (setq ispell-really-hunspell t)
  (setq  wcheck-timer-idle .2)
  (setq-default
   wcheck-language "English"
   wcheck-language-data '(("English"
                           (program       	. "/usr/bin/enchant")
                           (args          	. ("-l" "-d" "en_US"))
                           (action-program	. "/usr/bin/enchant")
                           (action-args   	. ("-a" "-d" "en_US"))
                           (action-parser 	. enchant-suggestions-menu))
                          ("German"
                           (program       	. "/usr/bin/enchant")
                           (args          	. ("-l" "-d" "de"))
                           (action-program	. "/usr/bin/enchant")
                           (action-args   	. ("-a" "-d" "de"))
                           (action-parser 	. enchant-suggestions-menu))
                          ("French"
                           (program       	. "/usr/bin/enchant")
                           (args          	. ("-l" "-d" "fr"))
                           (action-program	. "/usr/bin/enchant")
                           (action-args   	. ("-a" "-d" "fr"))
                           (action-parser 	. enchant-suggestions-menu))
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
  (defun turn-on-spell-check ()
    (if use-spell-check (wcheck-mode 1)))

  ;; enable spell-check in certain modes
  (add-hook 'markdown-mode-hook	'turn-on-spell-check)
  (add-hook 'org-mode-hook     	'turn-on-spell-check))

;; align
(setup "align"
  ;; definitions for ruby code
  ;; fixes the most egregious mistake in detecting regions (hashes), but should be properly generalized at some point
  (setq align-region-separate "\\(^\\s-*[{}]?\\s-*$\\)\\|\\(=\\s-*[][{}()]\\s-*$\\)")
  (defconst align-ruby-modes '(enh-ruby-mode ruby-mode)
    "align-perl-modes is a variable defined in `align.el'.")
  (defconst ruby-align-rules-list
    '((ruby-comma-delimiter
       (regexp	. ",\\(\\s-*\\)[^/ \t\n]")
       (modes 	. '(enh-ruby-mode))
       (repeat	. t))
      (ruby-string-after-func
       (regexp	. "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
       (modes 	. '(enh-ruby-mode))
       (repeat	. t))
      (ruby-symbol-after-func
       (regexp	. "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
       (modes 	. '(enh-ruby-mode))))
    "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")
  (add-to-list 'align-perl-modes        	'enh-ruby-mode)
  (add-to-list 'align-dq-string-modes   	'enh-ruby-mode)
  (add-to-list 'align-sq-string-modes   	'enh-ruby-mode)
  (add-to-list 'align-open-comment-modes	'enh-ruby-mode)
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

  ;; repeat regex (teh fuck ain't that the default?!)
  (defun align-repeat (start end regexp)
    "Repeat alignment with respect to the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end
                  (concat "\\(\\s-*\\)" regexp) 1 1 t))

  (defun align-whitespace (start end)
    "Align region by whitespace."
    (interactive "r")
    (align-regexp start end (concat "\\(\\s-*\\)" "\\s-") 1 0 t))

  ;; align should always indent with spaces
  (defadvice align-areas (around fix-tab-indent activate)
    (let ((indent-tabs-mode nil))
      ad-do-it))
  )

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

;; tramp (remote files)
(setup-after "tramp"
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name "~/.emacs.d/cache/tramp")
  ;; cookies
  (setq url-cookie-file "~/.emacs.d/cache/url/cookies"))

(defun sudo-open-file (&optional arg)
  "Find a file and open it as root."
  (interactive "p")
  (if arg
      (find-file (concat "/sudo::" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(defun sudo-open-current-file ()
  "Edit the current file as root."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file
     (concat "/sudo::" (buffer-file-name (current-buffer))))
    (goto-char pos)))

;; input methods, including a direct mozc binding to avoid ibus (requires mozc install)
(defmacro set-input-method-fun (name)
  `(defun ,(intern (format "set-input-method-%s" name)) ()
     (interactive)
     (set-input-method ,name)))

(setup "custom-input-methods"
  (setup "mozc" (setq mozc-leim-title "あ"))

  ;; default to the diacritic smasher
  (setq default-input-method "muflax-latin")
  (defun turn-on-default-input-method ()
    (set-input-method default-input-method))
  (add-hook 'text-mode-hook       	'turn-on-default-input-method)
  (add-hook 'prog-mode-hook       	'turn-on-default-input-method)
  (add-hook 'dired-mode-hook      	'turn-on-default-input-method)
  (add-hook 'minibuffer-setup-hook	'turn-on-default-input-method)
  (add-hook 'occur-mode-hook      	'turn-on-default-input-method)
  (add-hook 'phi-search-init-hook 	'turn-on-default-input-method)

  ;; don't underline partial input
  (setq input-method-highlight-flag nil)

  ;;don't spam the minibuffer
  (setq input-method-verbose-flag 'complex-only)

  (defun clear-input-method ()
    (interactive) (set-input-method nil))

  (set-input-method-fun "muflax-latin")
  (set-input-method-fun "muflax-cyrillic")
  (set-input-method-fun "muflax-turkish")
  (set-input-method-fun "muflax-greek")
  (set-input-method-fun "japanese-mozc"))

;; analog to delete-file
(defun delete-current-file ()
  "Delete the file associated with the current buffer."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when (y-or-n-p (concat "Delete file: " current-file))
      (kill-buffer (current-buffer))
      (delete-file current-file)
      (message (concat "Deleted file: " current-file)))))

(defun reload-current-file ()
  "Close the current buffer and re-open it. Typically used to quickly clear buffer variables and overlays."
  (interactive)
  (let ((current-file (buffer-file-name)))
    ;; makes only sense if there's no file associated with the buffer
    (when current-file
      (when (kill-buffer (current-buffer))
        (find-file current-file)))))

;; move lines like in org-mode
(setup "move-dup")

;; move buffers
(setup "buffer-move")

;; undo window changes
(setup "winner"
  (winner-mode 1))

;; expand-region to mark stuff
(setup "expand-region"
  (setq expand-region-contract-fast-key "<left>")
  (setq expand-region-reset-fast-key    "SPC")

  ;; notes-mode speed-up
  (defun er/add-notes-mode-expansions ()
    "Adds notes-mode expansions for buffers in notes-mode"
    (set (make-local-variable 'er/try-expand-list)
         (default-value 'er/try-expand-list))
    (loop for fun in '(er/mark-email er/mark-url)
          collect (set 'er/try-expand-list
                       (remove fun er/try-expand-list))))

  (er/enable-mode-expansions 'notes-mode 'er/add-notes-mode-expansions))

;; make zsh aliases work
(setup "shell-command"
  (setq shell-command-switch "-lc")
;; tab-completion for shell-command
;; FIXME not working yet, but meh
  (shell-command-completion-mode)

  ;; better handling than M-| / M-!
  (defun generalized-shell-command (command arg)
    "Unifies `shell-command' and `shell-command-on-region'.
You have:
- (no arg)    	run command and place output
- (C-u)       	... don't chomp output
- (region)    	replace region with output from command
- (C-u region)	... and print to minibuffer" ; TODO: make this also chomp
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
  )

;; remove trailing whitespace on save, unless it's a big buffer
;; TODO make this fast so we can always run it
(defun maybe-trim-whitespace ()
  (when (not (> (buffer-size) 1000000))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'maybe-trim-whitespace)

;; scratchpad buffers
(setup-lazy '(scratch) "scratch")

;; don't spam *Scratch*
(setq initial-scratch-message nil)

;; work with numbers at point
(setup "number"
  (defun number/increment ()
    (interactive)
    (number/add (number-read "1")))
  (defun number/decrement ()
    (interactive)
    (number/subtract (number-read "1"))))

;; rotate / toggle text
(setup-lazy '(rotate-text) "rotate-text"
  (add-to-list 'rotate-text-words  	'("auto"  	"manual"))
  (add-to-list 'rotate-text-words  	'("if"    	"unless"))
  (add-to-list 'rotate-text-words  	'("map"   	"each"))
  (add-to-list 'rotate-text-words  	'("on"    	"off"))
  (add-to-list 'rotate-text-words  	'("select"	"reject"))
  (add-to-list 'rotate-text-words  	'("t"     	"nil"))
  (add-to-list 'rotate-text-words  	'("true"  	"false"))
  (add-to-list 'rotate-text-words  	'("var"   	"const"))
  (add-to-list 'rotate-text-words  	'("yes"   	"no"))
  (add-to-list 'rotate-text-symbols	'("?"     	"!")))

;; handle camelcase better
(global-subword-mode 1)

;; folding
(setup "hideshow"
  (setup-lazy '(hideshowvis-enable hideshowvis-minor-mode) "hideshowvis")
  (setup "fold-dwim")
  (setup "yafolding")

  (add-hook 'enh-ruby-hook  	'hs-minor-mode)
  (add-hook 'prog-mode-hook 	'hs-minor-mode)
  (add-hook 'notes-mode-hook	'hs-minor-mode)

  (setq hs-isearch-open t)

  (setq hs-fold-level 1)

  (defun hs-fold-increase ()
    (interactive)
    (set (make-local-variable 'hs-fold-level)
         (1+ hs-fold-level))
    (hs-hide-level hs-fold-level))

  (defun hs-fold-decrease ()
    (interactive)
    (set (make-local-variable 'hs-fold-level)
         (max (1- hs-fold-level) 1))
    (hs-hide-level hs-fold-level))

  (defun hs-fold-reset ()
    (interactive)
    (set (make-local-variable 'hs-fold-level) 0)
    (fold-dwim-show-all))

  (defun hs-fold-levels ()
    (interactive)

    (set (make-local-variable 'hs-fold-level) 1)
    (hs-hide-level hs-fold-level)

    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "<right>")	'hs-fold-increase)
       (define-key map (kbd "<left>") 	'hs-fold-decrease)
       (define-key map (kbd "SPC")    	'hs-fold-reset)
       map) t)
    (message "<right> to fold more, <left> to fold less, SPC to reset."))

  (setq whitespace-fold-level tab-width)

  (defun whitespace-fold-increase ()
    (interactive)
    (set (make-local-variable 'whitspace-fold-level)
         (+ whitespace-fold-level tab-width))
    (set-selective-display whitespace-fold-level))

  (defun whitespace-fold-decrease ()
    (interactive)
    (set (make-local-variable 'whitspace-fold-level)
         (max (- whitespace-fold-level tab-width) tab-width))
    (set-selective-display whitespace-fold-level))

  (defun whitespace-fold-reset ()
    (interactive)
    (set (make-local-variable 'whitspace-fold-level) 0)
    (set-selective-display whitespace-fold-level))

  (defun whitespace-fold-levels ()
    (interactive)

    (set (make-local-variable 'whitspace-fold-level) tab-width)
    (set-selective-display whitespace-fold-level)

    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "<right>")	'whitespace-fold-increase)
       (define-key map (kbd "<left>") 	'whitespace-fold-decrease)
       (define-key map (kbd "SPC")    	'whitespace-fold-reset)
       map) t)
    (message "<right> to fold more, <left> to fold less, SPC to reset."))
  )

;; parenthesis highlighting behavior
(show-paren-mode 1)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

;; smart parentheses
(setup "smartparens-config"
  (smartparens-global-mode t)
  (setq sp-highlight-pair-overlay nil)

  ;; don't do any insertion/deletion magic
  (setq       	sp-autoescape-string-quote             	nil)
  (setq       	sp-autoinsert-pair                     	nil)
  (setq       	sp-cancel-autoskip-on-backward-movement	nil)
  (setq       	sp-autodelete-pair                     	nil)
  (set-default	'sp-autoskip-opening-pair              	nil)
  (set-default	'sp-autoskip-closing-pair              	nil)

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
    (sp-local-pair "*"	"*"	:actions	'(wrap)	)
    (sp-local-pair "_"	"_"	:actions	'(wrap)	)
    )

  ;; notes-mode
  (sp-with-modes '(notes-mode)
    (sp-local-pair "*"	"*"	:actions	'(wrap)	)
    (sp-local-pair "/"	"/"	:actions	'(wrap)	)
    (sp-local-pair "<"	">"	:actions	'(wrap)	)
    (sp-local-pair "["	"]"	        	       	)
    (sp-local-pair "'"	"'"	:actions	'(wrap)	)
    (sp-local-pair "`"	"`"	:actions	'(wrap)	)
    )

  ;; overwrite |pipe| handling in ruby
  (sp-with-modes '(enh-ruby-mode ruby-mode)
    (sp-local-pair "|"	"|"	:pre-handlers	nil)	)
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
(setup "midnight"
  (midnight-delay-set 'midnight-delay 0))

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
  (let (region (word-or-region))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char (first region))
        (cond
         ((looking-at "[[:lower:]]") (put this-command 'state "lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "title"))
         (t (put this-command 'state "lower")))))

    (cond
     ((string= "lower" (get this-command 'state))
      (apply 'upcase-initials-region region)
      (put this-command 'state "title"))
     ((string= "title" (get this-command 'state))
      (apply 'downcase-region region)
      (put this-command 'state "lower")))
    ))

(defun toggle-upcase ()
  "Toggle case of the word / region between lower case and upper case."
  (interactive)
  (let (region (word-or-region))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char (first region))
        (cond
         ((looking-at "[[:lower:]]") (put this-command 'state "lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "upper"))
         (t (put this-command 'state "lower")))))

    (cond
     ((string= "lower" (get this-command 'state))
      (apply 'upcase-region region)
      (put this-command 'state "upper"))
     ((string= "upper" (get this-command 'state))
      (apply 'downcase-region region)
      (put this-command 'state "lower")))))



(defun title-case-word-or-region ()
  "Transform word/region into title case."
  (interactive)
  (apply 'upcase-initials-region (word-or-region)))

(defun upcase-word-or-region ()
  "Transform word/region into title case."
  (interactive)
  (apply 'upcase-region (word-or-region)))

(defun downcase-word-or-region ()
  "Transform word/region into title case."
  (interactive)
  (apply 'downcase-region (word-or-region)))

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

;; sticky windows
(setup "sticky-windows")

(setup-lazy '(neotree neotree-toggle) "neotree"
  (setq neo-show-header nil))

(setup-lazy '(nav-minor-mode nav-global-mode) "nav-mode")

(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

;; help extension
(setup "help-fns+")

;; navigate windows
(defalias 'focus-next-window 'other-window)
(defun focus-prev-window ()
  (interactive)
  (other-window -1))

(setup-after "buffer-move"
  (defun split-window-above ()
    (interactive)
    (split-window-below)
    (buf-move-down))

  (defun split-window-left ()
    (interactive)
    (split-window-right)
    (buf-move-right)))

;; alternative to C-q TAB for easier keybindings
(defun literal-tab ()
  (interactive)
  (insert "\t"))

(defun toggle-subword-mode ()
  "Switch between subword/superword-mode."
  (interactive)
  (if global-subword-mode
      (if (fboundp 'global-superword-mode)
          (global-superword-mode 1)
        (global-subword-mode -1))
    (global-subword-mode 1)))


(defvar elastic-tab-align-modes
  '(
    emacs-lisp-mode
    enh-ruby-mode
    notes-mode
    )
  "modes that align elastic tabstops during indent")

;; elastic tabstops
(setup "elastic-tabstops"
  (defmacro elastic-advice-command (command-name)
    `(defadvice ,command-name (after elastic-tabstops activate)
       (when (member major-mode elastic-tab-align-modes)
         (elastic-align-current))))

  ;; TODO should be smarter and hijack the command's arguments
  (defmacro elastic-advice-command-region (command-name)
    `(defadvice ,command-name (after elastic-tabstops activate)
       (when (member major-mode elastic-tab-align-modes)
         (elastic-align-region (point) (mark)))))

  (elastic-advice-command       	indent-for-tab-command)
  (elastic-advice-command       	literal-tab)
  (elastic-advice-command       	indent-according-to-mode)
  (elastic-advice-command-region	indent-region)
  )

(provide 'init-editing)
