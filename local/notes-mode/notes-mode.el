;;; notes-mode.el --- Emacs Major mode for thinking in text
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; This mode implements the notation I use to write notes (and teaching scripts). It helps you write any documentation in the most desirable style, that of prompted examples, by annotating normal plain text. It works like this:
;;
;; [ line annotations
;;
;; You can mark a line as special by starting it with a dedicated character, like this:
;;
;; $ This line is an explanation. You can tell because it starts with a $.
;;
;; $ The next line is a quote. It starts with a >.
;; > I am a quote!
;; $ (Yes, you are.)
;;
;; % How do you start a quote?
;; @ With a >.
;;
;; $ As you can see, a % begins a prompt, whereas @ marks the expected reply or answer. They are typically used as a pair, but @ by itself is also used to show examples.
;;
;; Remaining options:
;;
;; + abstract concept, as demonstrated by the following lines
;; # comments
;; ! wrong, invalid
;; = equivalent, typically used to transform metaphors into literal structures
;;
;; ]
;;
;; [ highlighting
;;
;; emphasis:
;; @ I suspect stegosaurus was *really* dumb.
;;
;; placeholders:
;; @ I used to work for [big company] before I moved to Alaska.
;;
;; [ grab brackets
;;
;; Enclose text in any pair of parentheses. You can add headers too, like I'm been doing.
;;
;; ]
;; [ lists
;;
;; Works like Markdown, but only supports the "-" or "1." notation.
;;
;; ]
;; [ links
;;
;; Like Markdown.
;;
;; ]
;; [ footnotes
;;
;; Like extended Markdown, except also allows nesting.
;;
;; ]
;; [ indentation
;;
;; ]
;; [ folding
;;
;; ]
;; [ tables
;;
;; ]
;;
;; [ appendix
;;
;; The primary stylistic influences are:
;;
;; - Markdown (and the common extensions to it), including Jason R. Blevins
;;   <jrblevin@sdf.org>'s markdown-mode as a starting codebase.
;;
;; - Owen Richardson <owen.a.richardson@gmail.com>'s use of grab brackets and
;;   choice of line annotations. (<3)
;;
;; - Knuth's idea of literate programming, in a vague sense
;;
;; - the experience of writing language courses and needing a convenient notation for common constructions, like prompts
;; ]
;;
;;; Installation:
;;
;; Put the `notes-mode.el` somewhere in the load-path, typically something like `~/.emacs.d/notes-mode.el`.
;;
;; Add the following lines to your `.emacs` file to associate notes-mode
;; with `.txt` files:
;;
;;     (require 'notes-mode)
;;     (add-to-list 'auto-mode-alist '("\\.txt\\'" . notes-mode))

;;; Code:

(eval-when-compile (require 'cl))

;; Customizable Variables

(defvar notes-mode-hook nil
  "Hook run when entering Markdown mode.")

(defgroup notes nil
  "Major mode for editing text files in Notes format."
  :prefix "notes-"
  :group 'wp)

;; Font Lock

(require 'font-lock)

(defvar notes-italic-face                    'notes-italic-face)
(defvar notes-bold-face                      'notes-bold-face)
(defvar notes-placeholder-square-face        'notes-placeholder-square-face)
(defvar notes-placeholder-wiggly-face        'notes-placeholder-wiggly-face)
(defvar notes-placeholder-pointy-face        'notes-placeholder-pointy-face)
(defvar notes-annotation-prompt-face         'notes-annotation-prompt-face)
(defvar notes-annotation-reply-face          'notes-annotation-reply-face)
(defvar notes-annotation-quote-face          'notes-annotation-quote-face)
(defvar notes-annotation-abstract-face       'notes-annotation-abstract-face)
(defvar notes-annotation-comment-face        'notes-annotation-comment-face)
(defvar notes-annotation-model-face          'notes-annotation-model-face)
(defvar notes-annotation-wrong-face          'notes-annotation-wrong-face)
(defvar notes-annotation-equivalent-face     'notes-annotation-equivalent-face)
(defvar notes-annotation-transformation-face 'notes-annotation-transformation-face)
(defvar notes-bracket-face                   'notes-bracket-face)
(defvar notes-header-face                    'notes-header-face)
(defvar notes-list-face                      'notes-list-face)
(defvar notes-link-face                      'notes-link-face)
(defvar notes-reference-face                 'notes-reference-face)
(defvar notes-footnote-face                  'notes-footnote-face)

;; Customization

(defgroup notes-faces nil
  "Faces used in Notes Mode"
  :group 'notes
  :group 'faces)

(defface notes-italic-face
  '((t (:slant italic)))
  "Face for italic text."
  :group 'notes-faces)

(defface notes-bold-face
  '((t (:weight bold)))
  "Face for bold text."
  :group 'notes-faces)

(defface notes-placeholder-square-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for [] placeholder text."
  :group 'notes-faces)

(defface notes-placeholder-wiggly-face
  '((t (:inherit font-lock-string-face)))
  "Face for {} placeholder text."
  :group 'notes-faces)

(defface notes-placeholder-pointy-face
  '((t (:inherit font-lock-doc-face)))
  "Face for <> placeholder text."
  :group 'notes-faces)

(defface notes-bracket-face
  '((t (:inherit font-lock-builtin-name-face :weight bold)))
  "Base face for grab brackets."
  :group 'notes-faces)

(defface notes-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'notes-faces)

(defface notes-annotation-transformation-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for transformation annotation."
  :group 'notes-faces)

(defface notes-annotation-quote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for quote annotation."
  :group 'notes-faces)

(defface notes-annotation-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for comment annotations."
  :group 'notes-faces)

(defface notes-annotation-prompt-face
  '((t (:inherit font-lock-string-face)))
  "Face for prompt annotation."
  :group 'notes-faces)

(defface notes-annotation-reply-face
  '((t (:inherit font-lock-string-face)))
  "Face for reply annotation."
  :group 'notes-faces)

(defface notes-annotation-abstract-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Face for abstract annotation."
  :group 'notes-faces)

(defface notes-annotation-model-face
  '((t (:inherit font-lock-string-face)))
  "Face for model annotation."
  :group 'notes-faces)

(defface notes-annotation-wrong-face
  '((t (:inherit font-lock-warning-face)))
  "Face for wrong annotation."
  :group 'notes-faces)

(defface notes-annotation-equivalent-face
  '((t (:inherit font-lock-doc-face)))
  "Face for equivalent annotation."
  :group 'notes-faces)

(defface notes-list-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for list item markers."
  :group 'notes-faces)

(defface notes-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'notes-faces)

(defface notes-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'notes-faces)

(defface notes-reference-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'notes-faces)

;; Parsing

(defconst notes-regex-grab-bracket-square-start
  "^\\([ \t]*\\)\\[\\([ \t]+[^]\n]*\\)?$")

(defconst notes-regex-grab-bracket-wiggly-start
  "^\\([ \t]*\\){\\([ \t]+[^}\n]*\\)?$")

(defconst notes-regex-grab-bracket-square-stop
  "^\\([ \t]*\\)\\][ \t]*$")

(defconst notes-regex-grab-bracket-wiggly-stop
  "^\\([ \t]*\\)}[ \t]*$")

(defconst notes-regex-header
  "^\\([ \t]*\\)\\([\\[{][ \t]+\\)\\(.+\\)")

(defconst notes-regex-annotation-abstract
  "^\\([ \t]*\\)\\([+]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-annotation-transformation
  "^\\([ \t]*\\)\\([<>]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-annotation-comment
  "^\\([ \t]*\\)\\([#]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-annotation-equivalent
  "^\\([ \t]*\\)\\([=]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-annotation-model
  "^\\([ \t]*\\)\\([$]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-annotation-prompt
  "^\\([ \t]*\\)\\([%?]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-annotation-quote
  "^\\([ \t]*\\)\\([|]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-annotation-reply
  "^\\([ \t]*\\)\\([@!]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-annotation-wrong
  "^\\([ \t]*\\)\\([*]\\)\\(\\([ \t]+\\)\\(.*\\)\\|[ \t]*$\\)")

(defconst notes-regex-list
  "^\\([ \t]*\\)\\([0-9]+\\.\\|[-]\\)\\([ \t]+\\)")

(defconst notes-regex-bold
  "\\(^\\|[ \t]\\)\\([*]\\(.+?\\)[*]\\)")

(defconst notes-regex-italic
  "\\(^\\|[ \t]\\)\\([/]\\(.+?\\)[/]\\)")

(defconst notes-regex-placeholder-square
  "\\(\\(\\[\\]\\|\\[[^] \t\n][^]\n]*\\]\\)\\)")

(defconst notes-regex-placeholder-wiggly
  "\\(\\({}\\|{[^} \t\n][^}\n]*}\\)\\)")

(defconst notes-regex-placeholder-pointy
  "\\(\\(<>\\|<[^> \t\n][^>\n]*>\\)\\)")

;; Keywords

(defvar notes-mode-font-lock-keywords
  (list
   (cons notes-regex-list   '(2 notes-list-face))
   (cons notes-regex-bold   '(2 notes-bold-face))
   ;; (cons notes-regex-italic '(2 notes-italic-face))
   (cons notes-regex-header '(3 notes-header-face))

   (cons notes-regex-placeholder-square
         '(1 notes-placeholder-square-face))
   (cons notes-regex-placeholder-wiggly
         '(1 notes-placeholder-wiggly-face))
   (cons notes-regex-placeholder-pointy
         '(1 notes-placeholder-pointy-face))

   (cons notes-regex-annotation-abstract
         '((2 notes-annotation-abstract-face)
           (3 notes-annotation-abstract-face keep)))
   (cons notes-regex-annotation-transformation
         '((2 notes-annotation-transformation-face)
           (3 notes-annotation-transformation-face keep)))
   (cons notes-regex-annotation-comment
         '((2 notes-annotation-comment-face)
           (3 notes-annotation-comment-face keep)))
   (cons notes-regex-annotation-equivalent
         '((2 notes-annotation-equivalent-face)
           (3 notes-annotation-equivalent-face keep)))
   (cons notes-regex-annotation-model
         '((2 notes-annotation-model-face)
           (3 notes-annotation-model-face keep)))
   (cons notes-regex-annotation-prompt
         '((2 notes-annotation-prompt-face)))
   ;; (3 notes-annotation-prompt-face keep)))
   (cons notes-regex-annotation-quote
         '((2 notes-annotation-quote-face)
           (3 notes-annotation-quote-face keep)))
   (cons notes-regex-annotation-reply
         '((2 notes-annotation-reply-face)))
   ;; (3 notes-annotation-reply-face keep)))
   (cons notes-regex-annotation-wrong
         '((2 notes-annotation-wrong-face)))
   ;; (3 notes-annotation-wrong-face keep)))
   )
  "Syntax highlighting for Notes files.")

;; Notes Parsing Functions

(defun notes-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (point-at-eol) t)))

(defun notes-prev-line-blank-p ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-min))
        t
      (forward-line -1)
      (notes-cur-line-blank-p))))

(defun notes-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-max))
        t
      (forward-line 1)
      (notes-cur-line-blank-p))))

(defun notes-cur-line-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward "^[ \t]*" (point-at-eol) t)
    (current-column)))

(defun notes-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (cond
     ((> (line-number-at-pos) 1)
      (forward-line -1)
      (while (and (notes-cur-line-blank-p) (not (bobp)))
        (forward-line -1))

      (cond
       ;; blocks force increased indentation
       ((re-search-forward notes-regex-grab-bracket-square-start
                           (point-at-eol) t)
        (goto-char (match-end 1))
        (+ (current-column) tab-width))
       ((re-search-forward notes-regex-grab-bracket-wiggly-start
                           (point-at-eol) t)
        (goto-char (match-end 1))
        (+ (current-column) tab-width))

       ;; otherwise preserve indentation
       (t (notes-cur-line-indent))))

     ;; default to 0
     (t 0))))

(defun notes-next-line-indent ()
  "Return the number of leading whitespace characters in the next line."
  (save-excursion
    (forward-line 1)
    (notes-cur-line-indent)))

(defun notes-cur-non-list-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward notes-regex-list (point-at-eol) t)
      (current-column))))

(defun notes-prev-non-list-indent ()
  "Return position of the first non-list-marker on the previous line.
If the previous line is empty, check the line before that one, too."
  (save-excursion
    (forward-line -1)
    (when (notes-cur-line-blank-p)
       (forward-line -1))
    (notes-cur-non-list-indent)))

(defun notes--next-block ()
  "Move the point to the start of the next text block."
  (forward-line)
  (while (and (or (not (notes-prev-line-blank-p))
                  (notes-cur-line-blank-p))
              (not (eobp)))
    (forward-line)))

(defun notes--end-of-level (level)
  "Move the point to the end of region with indentation at least LEVEL."
  (let (indent)
    (while (and (not (< (setq indent (notes-cur-line-indent)) level))
                (not (>= indent (+ level 2)))
                (not (eobp)))
      (notes--next-block))
    (unless (eobp)
      ;; move back before any trailing blank lines
      (while (and (notes-prev-line-blank-p)
                  (not (bobp)))
        (forward-line -1))
      (forward-line -1)
      (end-of-line))))

(defun notes-cur-list-item-end (level)
  "Move to the end of the current list item with nonlist indentation LEVEL.
If the point is not in a list item, do nothing."
  (let (indent)
    (forward-line)
    (setq indent (notes-cur-line-indent))
    (while
        (cond
         ;; stop at end of the buffer
         ((eobp) nil)
         ;; continue if the current line is blank
         ((notes-cur-line-blank-p) t)
         ;; continue while indentation is the same or greater
         ((>= indent level) t)
         ;; stop if current indentation is less than list item
         ;; and the previous line was blank
         ((and (< indent level)
               (notes-prev-line-blank-p))
          nil)
         ;; stop at a new list item of the same or lesser indentation
         ((looking-at notes-regex-list) nil)
         ;; otherwise, continue
         (t t))
      (forward-line)
      (setq indent (notes-cur-line-indent)))
    ;; don't skip over whitespace for empty list items (marker and whitespace only), just move to end of whitespace
    (if (looking-back (concat notes-regex-list "\\s-*"))
          (goto-char (match-end 3))
      (skip-syntax-backward "-"))))

(defun notes-cur-list-item-bounds ()
  "Return bounds and indentation of the current list item.
Return a list of the form (begin end indent nonlist-indent). If the point is not inside a list item, return nil. Leave match data intact for `notes-regex-list'."
  (let (cur prev-begin prev-end indent nonlist-indent)
    ;; store current location
    (setq cur (point))
    ;; verify that cur is between beginning and end of item
    (save-excursion
      (if (looking-at notes-regex-list)
          (beginning-of-line)
        (end-of-line)
        (re-search-backward notes-regex-list nil t))
      (save-match-data
        (setq prev-begin (point))
        (setq indent (notes-cur-line-indent))
        (setq nonlist-indent (notes-cur-non-list-indent))
        (notes-cur-list-item-end nonlist-indent)
        (setq prev-end (point)))
      (if (and (>= cur prev-begin)
               (<= cur prev-end)
               nonlist-indent)
          (list prev-begin prev-end indent nonlist-indent)
        nil))))

;; Syntax Table

(defvar notes-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." tab)
    tab)
  "Syntax table for `notes-mode'.")

;; Indentation

(defun notes-indent-line ()
  "Indent the current line using some heuristics."
  (interactive)
  (indent-line-to (notes-calc-indent)))

(defun notes-calc-indent ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the default indentation level."
  (let (pos prev-line-pos)

    ;; figure out last line indent
    (setq prev-line-pos (notes-prev-line-indent))

    (setq pos
          (save-excursion
            (goto-char (point-at-bol))
            (cond
             ;; ends of block have a definite end
             ((looking-at notes-regex-grab-bracket-square-stop)
              (- prev-line-pos tab-width))
             ((looking-at notes-regex-grab-bracket-wiggly-stop)
              (- prev-line-pos tab-width))
             (t prev-line-pos))))

    pos))

;; Lists

(defun notes-insert-list-item (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If the point is inside ordered list, insert the next number followed by a period.  Use the previous list item to determine the amount of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is 4), increase the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is 16), decrease the indentation by one level."
  (interactive "p")
  (let (bounds marker end)
    (save-match-data
      (setq bounds (notes-cur-list-item-bounds))
      (if (not bounds)
          ;; when not in a list, start a new unordered one
          (progn
            (unless (notes-cur-line-blank-p)
              (insert "\n"))
            (insert "- "))
        ;; compute indentation for a new list item
        (setq marker (concat (match-string 2) (match-string 3)))
        (newline-and-indent)
        (cond
         ;; ordered list
         ((string-match "[0-9]" marker)
          (if (= arg 16) ;; starting a new column indented one more level
              (insert "1. ")
            ;; travel up to the last item and pick the correct number.  If the argument was nil, "new-indent = item-indent" is the same, so we don't need special treatment. Neat.
            (save-excursion
              (while (not (looking-at "^[ \t]*\\([0-9]+\\)\\."))
                (forward-line -1)))
            (insert (concat (int-to-string
                             (1+ (string-to-number (match-string 1))))
                            ". "))))
         ;; unordered list
         ((string-match "[-]" marker)
          (insert marker)))))))

;; Useful functions
(defun notes-find-annotations (style)
  "Find all annotation lines that match a pattern."
  (interactive "Mstyle: ")

  (let ((pattern (thing-at-point 'word)))
    (setq pattern (read-from-minibuffer "Pattern: " pattern))
    (occur (concat
            "^[ \t]*" (regexp-quote style)
            ".*" (ucs-normalize-NFKC-string pattern) ".*$")))
  )

;; Keymap

(defvar notes-mode-map
  (let ((map (make-keymap)))
    ;; Visibility cycling
    (define-key map "\C-i" 'notes-indent-line)
    (define-key map "\M-i" 'notes-shifttab)
    ;; Lists
    (define-key map (kbd "S-<return>") 'notes-insert-list-item)
    (define-key map (kbd "C-<return>") 'notes-insert-list-item)
    ;; Misc
    (define-key map (kbd "C-v +") (lambda () (interactive) (notes-find-annotations "+")))
    (define-key map (kbd "C-v ?") (lambda () (interactive) (notes-find-annotations "?")))
    (define-key map (kbd "C-v !") (lambda () (interactive) (notes-find-annotations "!")))
    (define-key map (kbd "C-v #") (lambda () (interactive) (notes-find-annotations "#")))
    (define-key map (kbd "C-v >") (lambda () (interactive) (notes-find-annotations ">")))
    (define-key map (kbd "C-v $") (lambda () (interactive) (notes-find-annotations "$")))
    (define-key map (kbd "C-c C-v") 'notes-find-annotations)
    map)
  "Keymap for Markdown major mode.")

;; Mode Definition

;;;###autoload
(define-derived-mode notes-mode text-mode "Notes"
  "Major mode for editing Notes files."

  ;; Ruby/C-style tab width
  (setq tab-width 2)

  ;; don't insert final newline by default
  (setq require-final-newline nil)

  ;; comments
  (set (make-local-variable 'comment-start)      "#")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'comment-end)        "")
  (set (make-local-variable 'comment-column)     0)

  ;; font lock
  (set (make-local-variable 'font-lock-defaults)
       '(notes-mode-font-lock-keywords))

  ;; no syntax can stretch multiple lines, which speeds up the font-lock
  (set (make-local-variable 'font-lock-multiline) nil)

  ;; make filling work with lists and annotations
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\. \\|[ \t]*[$?!<>=*+#%@|] \||[\t]+")

  ;; FIXME this isn't a good fix, but works for now...
  (set (make-local-variable 'adaptive-fill-regexp)
       "[ \t]*[*+-] \\|[ \t]*[0-9]+\\. \\|[ \t]*[$?!<>=*+#%@|] \\|[\t]+")

  ;; imenu support
  (set (make-local-variable 'imenu-generic-expression)
       '(
         (nil "^[[{] \\(.+\\)$" 1)
         ))

  ;; indentation
  (set (make-local-variable 'indent-line-function) #'notes-indent-line)

  )

(provide 'notes-mode)

;;; notes-mode.el ends here
