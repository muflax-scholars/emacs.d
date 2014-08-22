;;; newlisp-mode.el --- newLISP editing mode for Emacs

;; Copyright (C) 2008-2013 KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru <shigeru.kb[at]gmail.com>
;; Version: 20140811.907
;; X-Original-Version: 0.3.1
;; Created: 2008-12-15
;; Keywords: language,lisp,newlisp
;; URL: https://github.com/kosh04/newlisp-mode

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Major mode for editing newLISP script.

;; - syntax highlighting
;; - keyword completion
;; - support for inferior processes

;;; Installation:

;; You should add this to .emacs file after putting it on your load-path:
;;
;;   (autoload 'newlisp-mode "newlisp-mode" "Major mode for newLISP files." t)
;;   ; or (require 'newlisp-mode)
;;   (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
;;   (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode))
;;
;;   ;; if needed
;;   (newlisp-mode-setup)
;;   (defun newlisp-mode-user-hook ()
;;     (setq comment-start "; ")
;;     (setq tab-width 8)
;;     (setq indent-tabs-mode nil))
;;   (add-hook 'newlisp-mode-hook 'newlisp-mode-user-hook)

;;; ChangeLog:

;; 2013-08-23
;; - add keyrowd bayes-query, bayes-train (v.10.5.3)
;;
;; 2013-06-06
;; - add keyword bigint, bigint?, $count (v.10.5.0)
;; - rename newlisp.el to newlisp-mode.el
;; - define-key use kbd macro
;;
;; 2013-02-26
;; - update keyword v.10.4.6
;;
;; 2012-06-04
;; - add newlisp-change-dir
;; - update keyword for newLISP v.10.4.3
;; - doc changes (ja -> en)
;;
;; 2011-06-23
;; - typo fixed
;;
;; 2011-01-28 version 0.3
;; - rewrite newlisp-complete-symbol function
;; - add newlisp--allow-lazy-eval
;;
;; 2010-10-15
;; - syntax-table fixed
;;
;; 2010-10-08 version 0.26
;; - temporary fix, for remote process buffer. (see XXX-1)
;; - add menu bar
;; - update keyword for newLISP v10.2.14
;; - fornt-lock fixed ([text], [cmd], unix-based-function-keywords)
;;
;; 2010-06-25 version 0.25
;; - now Emacs 21 available
;;
;; 2010-02-05
;; - add licence (GPLv3)
;;
;; 2009-09-30 version 0.2
;; - add complete-symbol
;;
;; 2009-07-05 version 0.1b
;; - update keyword for newLISP v10.1.0
;; - rename `*variable*' to `variable' (emacs namespace)
;;
;; 2009-06-05 version 0.1a
;; - font-lock fixed
;; - add newlisp-mode-syntax-table
;;
;; 2009-04-19 version 0.1
;; - add newlisp-mode, font-lock
;;
;; 2008-12-15 version 0.01
;; - first commit (add newlisp-mode)
;;

;;; Known Bugs:

;;; Todo:

;; - dynamic completion for user-defined var/func
;; - checkdoc
;; - defcustom
;; - display input-expr to *newlisp* buffer when using newlisp-eval
;; - avoid relying on lisp-mode indent

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'comint)                       ; comint-send-string
(require 'font-lock)

(defgroup newlisp nil
  "Newlisp source code editing functions."
  :group 'newlisp
  :prefix "newlisp-"                    ; or "nl-" ?
  :version "0.3.1")

;; (executable-find "newlisp") or "/usr/bin/newlisp"
(defcustom newlisp-command "newlisp"
  "Filename to use to run newlisp."
  :type 'string
  :group 'newlisp)

(defvar newlisp-switches "-C")

(defvar newlisp-load-init-p t)

(defvar newlisp-process-coding-system 'utf-8
  "Coding system used for process newLISP.
If you use newLISP version UTF-8 support, its value is `utf-8'.
Otherwise maybe `shift_jis'.")

(defun newlisp-process ()
  "Return newlisp process object.
If not running, then start new process."
  (let ((default-process-coding-system
         (cons #1=newlisp-process-coding-system #1#))
        (switches (split-string newlisp-switches " ")))
    (if (null newlisp-load-init-p)
        (pushnew "-n" switches :test #'equal))
    (condition-case err
        (get-buffer-process
         (apply #'make-comint "newlisp"
                newlisp-command nil switches))
      (error
       ;; XXX-1: (error "No process started")
       ;; don't remove the buffer, when error occurs during start process
       ;; comint.el bug ?
       (kill-buffer "*newlisp*")
       (error "%s" (error-message-string err))))
    ))

;;;###autoload
(defun newlisp-show-repl (&optional no-focus)
  "Display newlisp process buffer."
  (interactive "P")
  (let ((obuf (current-buffer)))
    (pop-to-buffer (process-buffer (newlisp-process)))
    (if no-focus (pop-to-buffer obuf))))

;;;###autoload
(defalias 'run-newlisp 'newlisp-show-repl)

(defvar newlisp--allow-lazy-eval t
  "If non-nil, suppress a readline tab-completion.")

(defun newlisp-eval (str-sexp)
  "Eval newlisp s-expression."
  (interactive "snewLISP Eval: ")
  (let ((proc (newlisp-process)))
    (labels ((sendln (str)
               (comint-send-string proc (concat str "\n"))))
      ;; Suppress TAB completion. [Tab] -> [Space]
      (if newlisp--allow-lazy-eval
          (setq str-sexp (replace-regexp-in-string
                          "\t\t" (make-string (* 2 tab-width) #x20)
                          str-sexp)))
      (cond
        ((string-match "\n" str-sexp)   ; multi-line expr
         (sendln "[cmd]")
         (sleep-for 0.05)               ; XXX
         (sendln str-sexp)
         (sendln "[/cmd]"))
        (:else
         (sendln str-sexp))))
    (newlisp-show-repl t)))

(defun newlisp-eval-region (from to)
  (interactive "r")
  (newlisp-eval (buffer-substring-no-properties from to)))

(defun newlisp-eval-last-sexp ()
  (interactive)
  (let ((opoint (point)))
    (unwind-protect
         (newlisp-eval-region (progn
                                ;; 'hoge
                                (unless (looking-at "\\_<")
                                  (backward-sexp))
                                (point))
                              (progn
                                (forward-sexp)
                                (point)))
      (goto-char (max (point) opoint)))))

(defun newlisp-eval-defun ()
  (interactive)
  (save-excursion
    (mark-defun)
    (newlisp-eval-region (region-beginning) (region-end))))

(defun newlisp-eval-buffer ()
  (interactive)
  (newlisp-eval-region (point-min) (point-max)))

(defun newlisp-load-file (file)
  "Load and translates newLISP from a FILE."
  (interactive (list
                (read-file-name "Load file: " (buffer-file-name))))
  (newlisp-eval (format "(load {%s})" (expand-file-name file))))

(defun newlisp-change-dir (dir)
  "Change the working directory."
  (interactive "DchangeDir: ")
  (newlisp-eval (format "(change-dir {%s})" (expand-file-name dir))))

(defun newlisp-restart-process ()
  "Restart a new clean newLISP process with same command-line params.
This function is not available on Win32."
  (interactive)
  (newlisp-eval "(reset true)"))

(defun newlisp-kill-process (&optional force)
  "Kill running process."
  (interactive "P")
  (if force
      (delete-process (newlisp-process))
      (newlisp-eval "(exit)")))

(defun newlisp-signal-process (&optional sigcode)
  "Send a signal to newlisp process. Default signal is TERM."
  (interactive "P")
  (or sigcode (setq sigcode 15))
  ;; e.g. (see `kill -l`)
  ;; 2) SIGINT  9) SIGKILL  15) SIGTERM
  (signal-process (newlisp-process) sigcode))

;; eval sync
;; (defun newlisp-eval-buffer (arg)
;;   (interactive "P")
;;   (setq arg (if arg (read-string "newLISP args: ") ""))
;;   (shell-command (format "%s \"%s\" %s"
;;                          newlisp-command
;;                          (buffer-file-name)
;;                          arg)
;;                  "*newLISP output*"))

;; (newlisp-eval (concat "(eval-string \"" (buffer-string) "\")" ))

;; eval async
(defun newlisp-execute-file (&optional cmd-args)
  (interactive (list (if current-prefix-arg
                         (read-string "execute args: " )
                         "")))
  ;; (setq arg (if arg (read-string "args: ") ""))
  (lexical-let ((outbuf (get-buffer-create "*newLISP output*")))
    (with-current-buffer outbuf (erase-buffer))
    (set-process-sentinel
     (start-process-shell-command "newlisp" outbuf
                                  newlisp-command
                                  (buffer-file-name)
                                  cmd-args)
     (lambda (process event)
       (cond ((zerop (buffer-size outbuf))
              (kill-buffer outbuf)
              (message "(no output)"))
             (:else
              (with-current-buffer outbuf
                (goto-char (point-min))
                (if (< (count-lines (point-min) (point-max)) 5)
                    (message "%s" (replace-regexp-in-string
                                   "\n+$" "" (buffer-string)))
                    (pop-to-buffer (process-buffer process))))))))
    ))

(defun newlisp-begin-cmd () (interactive) (insert "[cmd]") (comint-send-input))
(defun newlisp-end-cmd () (interactive) (insert "[/cmd]") (comint-send-input))
;; (define-key inferior-newlisp-mode-map "\C-c[" 'newlisp-begin-cmd)
;; (define-key inferior-newlisp-mode-map "\C-c]" 'newlisp-end-cmd)

(defun newlisp-debug-region (start end)
  (interactive "r")
  (error "Not implemented yet."))

;;
;; Keyword List
;;
(eval-when (compile load eval)
  (defvar newlisp-primitive-keywords
    '("!" "!=" "$" "%" "&" "*" "+" "++" "-" "--" "/"
      ":" "<" "<<" "<=" "=" ">" ">=" ">>" "^" "|" "~"
      "abs" "acos" "acosh" "add" "address" "amb" "and" "append" "append-file" "apply"
      "args" "array" "array-list" "array?" "asin" "asinh" "assoc" "atan" "atan2" "atanh" "atom?"
      "base64-dec" "base64-enc" "bayes-query" "bayes-train" "begin"
      "beta" "betai" "bigint" "bigint?" "bind" "binomial" "bits"
      "callback" "case" "catch" "ceil" "change-dir" "char" "chop" "clean" "close" "collect"
      "command-event" "cond" "cons" "constant" "context" "context?" "copy" "copy-file"
      "corr" "cos" "cosh" "count" "cpymem" "crc32" "crit-chi2" "crit-f" "crit-t" "crit-z"
      "current-line" "curry"
      "date" "date-list" "date-value" "debug" "dec" "def-new" "default" "delete"
      "delete-file" "delete-url" "destroy" "det" "device" "difference" "directory"
      "directory?" "div" "do-until" "do-while" "doargs" "dolist" "dostring" "dotimes"
      "dotree" "dump" "dup"
      "empty?" "encrypt" "ends-with" "env" "erf" "error-event" "eval" "eval-string"
      "even?" "exec" "exists" "exit" "exp" "expand" "explode" "extend"
      "factor" "fft" "file-info" "file?" "filter" "find" "find-all" "first" "flat"
      "float" "float?" "floor" "flt" "for" "for-all" "format" "fv"
      "gammai" "gammaln" "gcd" "get-char" "get-float" "get-int" "get-long" "get-string"
      "get-url" "global" "global?"
      "if" "if-not" "ifft" "import" "inc" "index" "inf?" "int" "integer" "integer?"
      "intersect" "invert" "irr"
      "join" "json-error" "json-parse"
      "kmeans-query" "kmeans-train"
      "lambda?" "last" "last-error" "legal?" "length" "let" "letex" "letn"
      "list" "list?" "load" "local" "log" "lookup" "lower-case"
      "macro" "macro?" "main-args" "make-dir" "map" "mat" "match" "max"
      "member" "min" "mod" "mul" "multiply"
      "NaN?"
      "net-accept" "net-close" "net-connect" "net-error" "net-eval" "net-interface"
      "net-ipv" "net-listen" "net-local" "net-lookup" "net-peek" "net-peer"
      "net-receive" "net-receive-from" "net-receive-udp" "net-select" "net-send"
      "net-send-to" "net-send-udp" "net-service" "net-sessions" "new" "nil?" "normal"
      "not" "now" "nper" "npv" "nth" "null?" "number?"
      "odd?" "open" "or"
      "pack" "parse" "pipe" "pmt" "pop" "pop-assoc" "post-url" "pow" "prefix"
      "pretty-print" "primitive?" "print" "println" "prob-chi2" "prob-f" "prob-t"
      "prob-z" "process" "prompt-event" "protected?" "push" "put-url" "pv"
      "quote" "quote?" "rand" "random" "randomize" "read" "read-buffer" "read-char"
      "read-expr" "read-file" "read-key" "read-line" "reader-event" "real-path"
      "ref" "ref-all" "regex" "regex-comp" "remove-dir" "rename-file" "replace"
      "reset" "rest" "reverse" "rotate" "round"
      "save" "search" "seed" "seek" "select" "self" "semaphore" "sequence" "series"
      "set" "set-locale" "set-ref" "set-ref-all" "setf" "setq" "sgn" "share" "signal"
      "silent" "sin" "sinh" "sleep" "slice" "sort" "source" "sqrt" "starts-with"
      "stats" "string" "string?" "struct" "sub" "swap" "sym" "symbol?" "symbols"
      "sys-error" "sys-info"
      "t-test" "tan" "tanh" "term" "throw" "throw-error" "time" "time-of-day"
      "timer" "title-case" "trace" "trace-highlight" "transpose" "trim" "true?"
      "unify" "union" "unique" "unless" "unpack" "until" "upper-case" "uuid"
      "when" "while" "write" "write-buffer" "write-char" "write-file" "write-line"
      "xfer-event" "xml-error" "xml-parse" "xml-type-tags"
      "zero?"
      ;; pre-defined
      "module"
      ;; UTF-8 version
      "read-utf8" "utf8" "utf8len" "unicode"
      )
    "newLISP primitive keyword list.")
  (defvar newlisp-lambda-keywords
    '("define" "lambda" "fn" "fn-macro" "define-macro" "lambda-macro"))
  (defvar newlisp-variable-keyword
    '("nil" "true" "ostype"
      "$args" "$count" "$idx" "$it" "$main-args" "$prompt-event"
      "$0" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
      "$10" "$11" "$12" "$13" "$14" "$15"))
  (defvar newlisp-context-keywords
    '("Class" "MAIN" "Tree"))
  (defvar newlisp-tag-keywords
    '("[text]" "[/text]" "[cmd]" "[/cmd]"))
  (defvar newlisp-unix-based-function-keywords
    '("abort" "peek" "fork" "wait-pid" "net-packet" "net-ping"
      "date-parse" "parse-date" "send" "spawn" "sync" "receive"))
  )

(defsubst newlisp-keywords (&optional use-process)
  "Return newLISP keyword list as string."
  (if use-process
      (car (read-from-string
            (shell-command-to-string
             (format "%s -n -e \"%s\"" newlisp-command
                     "(map term (filter (lambda (s) (global? s)) (symbols)))"))))
      (append newlisp-primitive-keywords
              newlisp-lambda-keywords
              (unless (eq system-type 'windows-nt)
                newlisp-unix-based-function-keywords)
              newlisp-variable-keyword
              newlisp-context-keywords)))

(defvar newlisp-obarray
  (let ((array (make-vector 401 0)))    ; more than keyword size
    (dolist (s (newlisp-keywords))
      (intern s array))
    array)
  "newLISP symbol table.")

(defvar newlisp-mode-hook nil)

(defvar newlisp-mode-map
  (let ((map (make-sparse-keymap "newlisp")))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map (kbd "M-:") 'newlisp-eval)
    (define-key map (kbd "M-C-x") 'newlisp-eval-defun)
    (define-key map (kbd "C-x C-e") 'newlisp-eval-last-sexp)
    (define-key map (kbd "C-c C-b") 'newlisp-eval-buffer)
    (define-key map (kbd "C-c C-r") 'newlisp-eval-region)
    (define-key map (kbd "C-c C-l") 'newlisp-load-file)
    (define-key map (kbd "C-c C-z") 'newlisp-show-repl)
    (define-key map (kbd "C-c C-i") 'newlisp-complete-symbol) ; or ESC TAB
    (define-key map (kbd "C-m") 'newline-and-indent)
    (define-key map (kbd "C-c <f4>") 'newlisp-kill-process)
    (define-key map (kbd "<f5>") 'newlisp-execute-file)
    map))

;; refer to prolog-mode
(easy-menu-define newlisp-mode-menu newlisp-mode-map
  "Menu for newLISP mode."
  '("newLISP"
    ["indent line" indent-according-to-mode t]
    ;; eval
    "---"
    ["Evaluate Last S-expression" newlisp-eval-last-sexp t]
    ["Evaluate Region" newlisp-eval-region :active mark-active]
    ["Evaluate Buffer" newlisp-eval-buffer t]
    "---"
    ["Run interactive newLISP session" run-newlisp t]
    ["Load This File" newlisp-load-file t]
    ["Execute This File" newlisp-execute-file t]
    ))

(defvar newlisp-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    ;; SYMBOL
    (modify-syntax-entry ?` "_   " table)
    (modify-syntax-entry ?, "_   " table)
    (modify-syntax-entry ?@ "_   " table)
    (modify-syntax-entry ?| "_   " table)
    (modify-syntax-entry ?\[ "_   " table)
    (modify-syntax-entry ?\] "_   " table)
    ;; STRING (match)
    (modify-syntax-entry ?\{ "(}   " table)
    (modify-syntax-entry ?\} "){   " table)
    ;; COMMENT
    (modify-syntax-entry ?# "<   " table)
    ;; ESCAPE
    ;;
    table))

;;;###autoload
(defun newlisp-mode ()
  "Major mode for editing newLISP code."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'newlisp-mode
        mode-name "newLISP")
  (use-local-map newlisp-mode-map)
  (lisp-mode-variables)
  (set-syntax-table newlisp-mode-syntax-table)
  ;; (set (make-local-variable (quote font-lock-defaults)) '(fn t nil nil fn))
  ;; (set (make-local-variable 'font-lock-keywords-case-fold-search) nil)
  ;; (set (make-local-variable 'comment-start) "# ")
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'newlisp-mode-hook)
      (run-hooks 'newlisp-mode-hook)))

(defsubst newlisp-find-symbol (string)
  "Locates a symbol whose name is STRING in a newLISP symbols."
  (intern-soft string newlisp-obarray))

(defun newlisp-complete-symbol ()
  "Perform completion on newLISP symbol preceding point."
  (interactive "*")
  ;; NOTE: newlisp's symbol is case-insensitive
  (let* ((completion-ignore-case nil)
         (from (save-excursion (backward-sexp) (point)))
         (to (point))
         (pattern (buffer-substring from to))
         (completion (try-completion pattern newlisp-obarray)))
    (cond ((eq completion t)
           (message "Sole completion"))
          ((null completion)
           (message "Can't find completion for \"%s\"" pattern)
           (ding))
          ((not (string= pattern completion))
           (delete-region from to)
           (insert completion))
          (:else
           (message "Making completion list...")
           (with-output-to-temp-buffer "*Completions*"
             (display-completion-list
              (all-completions pattern newlisp-obarray nil)
              pattern))
           (message "Making completion list...done"))
          )))

(defun newlisp-mode-setup ()
  (setq newlisp-process-coding-system
        (let ((res (shell-command-to-string
                    (format "%s -n -e \"(primitive? MAIN:utf8)\""
                            newlisp-command))))
          (if (string-match "true" res)
              'utf-8 'shift_jis)))
  (setq newlisp-primitive-keywords
        (car (read-from-string
              (shell-command-to-string
               (format "%s -n -e \"(map term (filter (fn (s) (global? s)) (symbols MAIN)))\""
                       newlisp-command)))))
  t)


;; FIXME: This code influence other lisp-mode indent.
(defmacro defindent (operator indentation)
  `(put ',operator 'lisp-indent-function ',indentation))

(defindent define 1)
(defindent fn 1)
(defindent begin 0)
(defindent local 1)
(defindent letex 1)
(defindent for 1)
(defindent lambda-macro 1)
(defindent define-macro 1)
(defindent until 1)
(defindent letn 1)
(defindent dostring 1)
(defindent doargs 1)
(defindent dotree 1)
(defindent do-while 1)
(defindent do-until 1)
(defindent if 0)

;; (defun newlisp-indent-function (indent-point state)
;;   ...
;;   (setq method (or (get (intern-soft function) 'newlisp-indent-function)
;;                    (get (intern-soft function) 'newlisp-indent-hook))))

;; $ html2txt $NEWLISPDIR/newlisp_manual.html -o newlisp_manual.txt
;; or use www-browser [File] -> [Save Page As (Text)]
(defvar newlisp-manual-text "newlisp_manual.txt")

(defvar newlisp-manual-html
  (or (dolist (path (list "/usr/share/doc/newlisp/manual_frame.html"
                          ;; When build newlisp `make install_home'
                          "~/share/doc/newlisp/manual_frame.html"
                          "C:/Program Files/newlisp/manual_frame.html"))
        (if (file-exists-p path)
            (return path)))
      "http://www.newlisp.org/downloads/manual_frame.html"))

(defun newlisp-switch-to-manual ()
  (interactive)
  (if (file-exists-p #1=newlisp-manual-text)
      (progn
        (pop-to-buffer (find-file-noselect #1#))
        (unless (eq major-mode 'newlisp-mode) (newlisp-mode))
        (toggle-read-only t))
      (error "manual %s not exist" #1#)))

(defun newlisp-browse-manual ()
  (interactive)
  (browse-url-of-file newlisp-manual-html))

(defun newlisp-lookup-manual (keyword)
  "Lookup newlisp reference manual."
  (interactive
    (list (let* ((s (newlisp-find-symbol
                     (current-word)
                     ;; (function-called-at-point)
                     ;; (thing-at-point 'symbol)
                     ))
                 (default (and s (symbol-name s))))
            ;; NOTE: Type "lambda?" from minibuffer -> l a m b d a C-q ?
            (completing-read (format "newLISP manual%s: "
                                     (if default
                                         (format " (default %s)" default)
                                         ""))
                             newlisp-obarray ; (newlisp-keywords)
                             nil t nil nil default))))
  (if (equal keyword "setf") (setq keyword "setq"))
  (if (equal keyword "parse-date") (setq keyword "date-parse"))
  (if (equal keyword "lambda") (setq keyword "fn"))

  (newlisp-switch-to-manual)
  (let ((opoint (point)) (found nil))
    (goto-char (point-min))
    (if (and (not (equal keyword ""))
             (search-forward-regexp
              ;; (foo)
              ;; (foo ...)
              ;; (foo-bar-baz) is NOT NEEDED
              ;; (concat "^\s+syntax: (" (regexp-quote keyword) "\s?")
              ;; e.g. "    define ! <#destructive>"
              ;; e.g. "    define-macro"
              ;; (format "^    %s\\(\s?.*\\)?$" (regexp-quote keyword))
              (format "^        syntax: (%s" (regexp-quote keyword))
              nil 'noerror))
        (progn
          (beginning-of-line)
          (forward-line -3)
          (recenter 0)
          (setq found t)))
    (unless found
      (message "not found %s" keyword)
      (goto-char opoint))))

;; (setf (get 'font-lock-add-keywords 'lisp-indent-function) 1)
;; lisp-mode.el:91
(font-lock-add-keywords 'newlisp-mode
  (list
   ;; (list "\\<\\(FIXME\\):" 1 font-lock-warning-face 'prepend)
   (cons (eval-when-compile (regexp-opt newlisp-primitive-keywords 'words))
         font-lock-keyword-face)
   ;; (eval-when-compile (regexp-opt newlisp-primitive-keywords 'words))
   (cons (eval-when-compile (regexp-opt newlisp-lambda-keywords 'words))
         font-lock-function-name-face)
   (cons (eval-when-compile (regexp-opt newlisp-variable-keyword 'words))
         font-lock-constant-face)
   (cons (eval-when-compile (regexp-opt newlisp-context-keywords 'words))
         font-lock-type-face)
   (cons (eval-when-compile (regexp-opt newlisp-tag-keywords)) ; not 'words
         font-lock-string-face)
   (cons (eval-when-compile (regexp-opt newlisp-unix-based-function-keywords 'words))
         (if (eq system-type 'windows-nt)
             font-lock-warning-face
             font-lock-keyword-face)
         ))
  )

(provide 'newlisp-mode)

;;; newlisp-mode.el ends here
