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
;; This mode implements the notation I use to write notes (and teaching scripts). It helps me write any documentation in the most desirable style, that of prompted examples, by annotating normal plain text.
;;
;; The other minor modes deal with particular editing problems during script development. They're mostly a big sprawling mess of helper functions.
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
;; - the experience of writing language courses
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
(require 'ample-regexps)

;; helper modes
(require 'conlang)
(require 'corpus-minor-mode)
(require 'lesson-minor-mode)
(require 'prompt-minor-mode)
(require 'word-list-minor-mode)

;; Customizable Variables

(defvar notes-mode-hook nil
  "Hook run when entering Markdown mode.")

(defgroup notes nil
  "Major mode for editing text files in Notes format."
  :prefix "notes-"
  :group 'wp)

;; Font Lock

(require 'font-lock)

(defvar notes-italic-face                   	'notes-italic-face)
(defvar notes-bold-face                     	'notes-bold-face)
(defvar notes-placeholder-square-face       	'notes-placeholder-square-face)
(defvar notes-placeholder-wiggly-face       	'notes-placeholder-wiggly-face)
(defvar notes-placeholder-pointy-face       	'notes-placeholder-pointy-face)
(defvar notes-annotation-prompt-face        	'notes-annotation-prompt-face)
(defvar notes-annotation-reply-face         	'notes-annotation-reply-face)
(defvar notes-annotation-plus-face          	'notes-annotation-plus-face)
(defvar notes-annotation-comment-face       	'notes-annotation-comment-face)
(defvar notes-annotation-say-face           	'notes-annotation-say-face)
(defvar notes-annotation-wrong-face         	'notes-annotation-wrong-face)
(defvar notes-annotation-transformation-face	'notes-annotation-transformation-face)
(defvar notes-special-lines-face            	'notes-special-lines-face)
(defvar notes-bracket-face                  	'notes-bracket-face)
(defvar notes-header-face                   	'notes-header-face)
(defvar notes-list-face                     	'notes-list-face)
(defvar notes-link-face                     	'notes-link-face)
(defvar notes-reference-face                	'notes-reference-face)
(defvar notes-footnote-face                 	'notes-footnote-face)

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

(defface notes-annotation-plus-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Face for abstract annotation."
  :group 'notes-faces)

(defface notes-annotation-say-face
  '((t (:inherit font-lock-string-face)))
  "Face for model annotation."
  :group 'notes-faces)

(defface notes-annotation-wrong-face
  '((t (:inherit font-lock-warning-face)))
  "Face for wrong annotation."
  :group 'notes-faces)

(defface notes-special-lines-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for special comment-y lines."
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

(define-arx notes-rx
  '(
    (indent  	(or (seq bol (* blank))
                   (+ "\t")))
    (indent-1	(group-n 1 indent))

    (annotation-2
     (:func (lambda (_form &rest args)
              `(group-n 2 (or ,@args) (* (not (any blank "\n")))))))

    (annotation-line-3
     (:func (lambda (_form &rest args)
              `(seq indent-1
                    (annotation-2 ,@args)
                    (group-n 3 (or (seq (+ blank) (* not-newline))
                                   (seq (* blank) eol)))))))

    (special-line-1
     (:func (lambda (_form &rest args)
              `(seq bol (* blank)
                    (group-n 1 (or ,@args))
                    (* blank) eol))))

    (open-bracket 	(any "[" "{"))
    (close-bracket	(any "]" "}"))
    ))


(defconst notes-regex-grab-bracket-square-start
  "^\\([ \t]*\\)\\[\\([ \t]*[^\n]+\\)?$")

(defconst notes-regex-grab-bracket-wiggly-start
  "^\\([ \t]*\\){\\([ \t]*[^\n]+\\)?$")

(defconst notes-regex-grab-bracket-square-stop
  "^\\([ \t]*\\)\\][ \t]*$")

(defconst notes-regex-grab-bracket-wiggly-stop
  "^\\([ \t]*\\)}[ \t]*$")

(defconst notes-regex-header
  "^\\([ \t]*\\)\\([\\[{][ \t]*\\)\\(.+\\)")

(defconst notes-regex-annotation-plus
  (notes-rx (annotation-line-3 "+")))

(defconst notes-regex-annotation-transformation
  (notes-rx (annotation-line-3 "<" ">")))

(defconst notes-regex-annotation-comment
  (notes-rx (annotation-line-3 "#")))

(defconst notes-regex-annotation-say
  (notes-rx (annotation-line-3 "$" "!!" "??")))

(defconst notes-regex-annotation-prompt
  (notes-rx (annotation-line-3 "%" "?")))

(defconst notes-regex-annotation-reply
  (notes-rx (annotation-line-3 "@" "!")))

(defconst notes-regex-annotation-wrong
  (notes-rx (annotation-line-3 "*")))

(defconst notes-regex-special-lines
  (notes-rx (special-line-1 "##" "lll" "sss")))

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
   (cons notes-regex-list	'(2 notes-list-face))
   (cons notes-regex-bold	'(2 notes-bold-face))

   (cons notes-regex-placeholder-square
         '(1 notes-placeholder-square-face))
   (cons notes-regex-placeholder-wiggly
         '(1 notes-placeholder-wiggly-face))
   (cons notes-regex-placeholder-pointy
         '(1 notes-placeholder-pointy-face))

   (cons notes-regex-header
         '((2 notes-header-face)
           (3 notes-header-face keep)))

   (cons notes-regex-special-lines
         '((1 notes-special-lines-face)))

   (cons notes-regex-annotation-plus
         '((2 notes-annotation-plus-face)
           (3 notes-annotation-plus-face keep)))
   (cons notes-regex-annotation-transformation
         '((2 notes-annotation-transformation-face)
           (3 notes-annotation-transformation-face keep)))
   (cons notes-regex-annotation-comment
         '((2 notes-annotation-comment-face)
           (3 notes-annotation-comment-face keep)))
   (cons notes-regex-annotation-say
         '((2 notes-annotation-say-face)
           (3 notes-annotation-say-face keep)))
   (cons notes-regex-annotation-prompt
         '((2 notes-annotation-prompt-face)))
   ;; (3 notes-annotation-prompt-face keep)))
   (cons notes-regex-annotation-reply
         '((2 notes-annotation-reply-face)))
   ;; (3 notes-annotation-reply-face keep)))
   (cons notes-regex-annotation-wrong
         '((2 notes-annotation-wrong-face)))
   ;; (3 notes-annotation-wrong-face keep)))
   )
  "Syntax highlighting for Notes files.")

;; Indentation

(defun notes-indent-line ()
  "Indent the current line using some heuristics, trying to preserve any elastic tabstops."
  (interactive)
  (let ((orig-point (point)))

    (beginning-of-line)
    (skip-chars-forward "\t")
    (delete-region (point-at-bol) (point))

    (beginning-of-line)
    (indent-to (notes-calc-indent))

    ;; don't lose position unless we're left of the indent point
    (when (> orig-point (point))
      (goto-char orig-point))))

(defun notes-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (point-at-eol) t)))

(defun notes-cur-line-indent ()
  "Return the number of tab indentations in the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward "^[\t]*" (point-at-eol) t)
    (current-column)))

(defun notes-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (catch 'pos
      (while (> (point) (point-min))
        (forward-line -1)

        (when (or (looking-at notes-regex-grab-bracket-square-start)
                  (looking-at notes-regex-grab-bracket-wiggly-start)
                  )
          (goto-char (match-end 1))
          (throw 'pos (+ (current-column) tab-width)))

        (when (or (looking-at notes-regex-grab-bracket-square-stop)
                  (looking-at notes-regex-grab-bracket-wiggly-stop)
                  )
          (goto-char (match-end 1))
          (throw 'pos (current-column)))
        )

      ;; default to 0
      (throw 'pos 0))))

(defun notes-calc-indent ()
  "Return the intended indent of the line."
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

    (max pos 0)))

;; Keymap

(defvar notes-mode-map (make-keymap) "Keymap for Markdown major mode.")

;; Mode Definition

;;;###autoload
(define-derived-mode notes-mode text-mode "+"
  "MAJOR mode for editing Notes files."

  ;; Ruby/C-style tab width
  (setq tab-width 2)

  ;; don't insert final newline by default
  (setq require-final-newline nil)

  ;; comments
  (set (make-local-variable 'comment-start)     	"#")
  (set (make-local-variable 'comment-start-skip)	"#+ *")
  (set (make-local-variable 'comment-end)       	"")
  (set (make-local-variable 'comment-column)    	0)

  ;; font lock
  (set (make-local-variable 'font-lock-defaults)
       '(notes-mode-font-lock-keywords))

  ;; no syntax can stretch multiple lines, which speeds up the font-lock
  (set (make-local-variable 'font-lock-multiline) nil)

  ;; make filling work with lists and annotations
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\. \\|[ \t]*[$?!<>=*+#%@|]\\sw* \\|[\t]+")

  ;; FIXME this isn't a good fix, but works for now...
  (set (make-local-variable 'adaptive-fill-regexp)
       "[ \t]*[-] \\|[ \t]*[0-9]+\\. \\|[ \t]*[$?!<>=*+#%@|]\\sw* \\|[\t]+")

  ;; imenu support
  (set (make-local-variable 'imenu-generic-expression)
       `(
         (nil ,(notes-rx bol indent "[" (* blank)
                         (group (* not-newline)) eol)
              1)
         ))

  ;; indentation
  (set (make-local-variable 'indent-line-function) #'notes-indent-line)
  (electric-indent-local-mode -1)

  ;; folding
  (add-to-list 'hs-special-modes-alist
               `(notes-mode ,(notes-rx open-bracket)
                            ,(notes-rx close-bracket)))

  )

(provide 'notes-mode)

;;; notes-mode.el ends here
