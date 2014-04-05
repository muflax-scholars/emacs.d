;;; markdown-mode.el --- Emacs Major mode for Markdown-formatted text files

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Simplified and custom version of Jason R. Blevins <jrblevin@sdf.org>'s
;; markdown-mode.
;;
;; I removed insertion and similar features I don't use. Indentation is saner,
;; as are footnotes. Some syntax problems with my personal markdown style have
;; been fixed.
;;
;; markdown-mode is a major mode for editing [Markdown][]-formatted
;; text files in GNU Emacs.  markdown-mode is free software, licensed
;; under the GNU GPL.
;;
;;  [Markdown]: http://daringfireball.net/projects/markdown/

;;; Installation:

;; Make sure to place `markdown-mode.el` somewhere in the load-path and add
;; the following lines to your `.emacs` file to associate markdown-mode
;; with `.text` files:
;;
;;     (autoload 'markdown-mode "markdown-mode"
;;        "Major mode for editing Markdown files" t)
;;     (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;;
;; There is no consensus on an official file extension so change `.text` to
;; `.mdwn`, `.md`, `.mdt`, or whatever you call your markdown files.


;;; Code:

(require 'outline)
(eval-when-compile (require 'cl))

;;; Constants =================================================================

;; tracks official version
(defconst markdown-mode-version "1.9"
  "Markdown mode version number.")

(defconst markdown-output-buffer-name "*markdown-output*"
  "Name of temporary buffer for markdown command output.")

;;; Customizable variables ====================================================

(defvar markdown-mode-hook nil
  "Hook run when entering Markdown mode.")

(defgroup markdown nil
  "Major mode for editing text files in Markdown format."
  :prefix "markdown-"
  :group 'wp
  :link '(url-link "http://jblevins.org/projects/markdown-mode/"))

(defcustom markdown-command "markdown"
  "Command to run markdown."
  :group 'markdown
  :type 'string)

(defcustom markdown-command-needs-filename nil
  "Set to non-nil if `markdown-command' does not accept input from stdin.
Instead, it will be passed a filename as the final command line
option.  As a result, you will only be able to run Markdown from
buffers which are visiting a file."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-hr-string "* * * * *"
  "String to use for horizonal rules."
  :group 'markdown
  :type 'string)

(defcustom markdown-bold-underscore nil
  "Use two underscores for bold instead of two asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-italic-underscore nil
  "Use underscores for italic instead of asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-indent-function 'markdown-indent-line
  "Function to use to indent."
  :group 'markdown
  :type 'function)

(defcustom markdown-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https"
    "imap" "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero"
    "rtsp" "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'markdown
  :type 'list)

(defcustom markdown-enable-math nil
  "Syntax highlighting for inline LaTeX expressions.
This will not take effect until Emacs is restarted."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-css-path ""
  "URL of CSS file to link to in the output XHTML."
  :group 'markdown
  :type 'string)

(defcustom markdown-xhtml-header-content ""
  "Additional content to include in the XHTML <head> block."
  :group 'markdown
  :type 'string)

(defcustom markdown-xhtml-standalone-regexp
  "^\\(\<\?xml\\|\<!DOCTYPE\\|\<html\\)"
  "Regexp indicating whether `markdown-command' output is standalone XHTML."
  :group 'markdown
  :type 'regexp)

(defcustom markdown-link-space-sub-char
  "_"
  "Character to use instead of spaces when mapping wiki links to filenames."
  :group 'markdown
  :type 'string)

;;; Font lock =================================================================

(require 'font-lock)

(defvar markdown-italic-face 'markdown-italic-face
  "Face name to use for italic text.")

(defvar markdown-bold-face 'markdown-bold-face
  "Face name to use for bold text.")

(defvar markdown-header-delimiter-face 'markdown-header-delimiter-face
  "Face name to use as a base for header delimiters.")

(defvar markdown-header-rule-face 'markdown-header-rule-face
  "Face name to use as a base for header rules.")

(defvar markdown-header-face 'markdown-header-face
  "Face name to use as a base for headers.")

(defvar markdown-header-face-1 'markdown-header-face-1
  "Face name to use for level-1 headers.")

(defvar markdown-header-face-2 'markdown-header-face-2
  "Face name to use for level-2 headers.")

(defvar markdown-header-face-3 'markdown-header-face-3
  "Face name to use for level-3 headers.")

(defvar markdown-header-face-4 'markdown-header-face-4
  "Face name to use for level-4 headers.")

(defvar markdown-header-face-5 'markdown-header-face-5
  "Face name to use for level-5 headers.")

(defvar markdown-header-face-6 'markdown-header-face-6
  "Face name to use for level-6 headers.")

(defvar markdown-inline-code-face 'markdown-inline-code-face
  "Face name to use for inline code.")

(defvar markdown-list-face 'markdown-list-face
  "Face name to use for list markers.")

(defvar markdown-blockquote-face 'markdown-blockquote-face
  "Face name to use for blockquote.")

(defvar markdown-pre-face 'markdown-pre-face
  "Face name to use for preformatted text.")

(defvar markdown-link-face 'markdown-link-face
  "Face name to use for links.")

(defvar markdown-missing-link-face 'markdown-missing-link-face
  "Face name to use for links where the linked file does not exist.")

(defvar markdown-reference-face 'markdown-reference-face
  "Face name to use for reference.")

(defvar markdown-footnote-face 'markdown-footnote-face
  "Face name to use for footnote identifiers.")

(defvar markdown-url-face 'markdown-url-face
  "Face name to use for URLs.")

(defvar markdown-link-title-face 'markdown-link-title-face
  "Face name to use for reference link titles.")

(defvar markdown-comment-face 'markdown-comment-face
  "Face name to use for HTML comments.")

(defvar markdown-math-face 'markdown-math-face
  "Face name to use for LaTeX expressions.")

(defvar markdown-erb-face 'markdown-erb-face
  "Face name to use for ERB templates.")

(defgroup markdown-faces nil
  "Faces used in Markdown Mode"
  :group 'markdown
  :group 'faces)

(defface markdown-italic-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
  "Face for italic text."
  :group 'markdown-faces)

(defface markdown-bold-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for bold text."
  :group 'markdown-faces)

(defface markdown-header-rule-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers rules."
  :group 'markdown-faces)

(defface markdown-header-delimiter-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers hash delimiter."
  :group 'markdown-faces)

(defface markdown-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'markdown-faces)

(defface markdown-header-face-1
  '((t (:inherit markdown-header-face)))
  "Face for level-1 headers."
  :group 'markdown-faces)

(defface markdown-header-face-2
  '((t (:inherit markdown-header-face)))
  "Face for level-2 headers."
  :group 'markdown-faces)

(defface markdown-header-face-3
  '((t (:inherit markdown-header-face)))
  "Face for level-3 headers."
  :group 'markdown-faces)

(defface markdown-header-face-4
  '((t (:inherit markdown-header-face)))
  "Face for level-4 headers."
  :group 'markdown-faces)

(defface markdown-header-face-5
  '((t (:inherit markdown-header-face)))
  "Face for level-5 headers."
  :group 'markdown-faces)

(defface markdown-header-face-6
  '((t (:inherit markdown-header-face)))
  "Face for level-6 headers."
  :group 'markdown-faces)

(defface markdown-inline-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'markdown-faces)

(defface markdown-list-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for list item markers."
  :group 'markdown-faces)

(defface markdown-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'markdown-faces)

(defface markdown-pre-face
  '((t (:inherit font-lock-constant-face)))
  "Face for preformatted text."
  :group 'markdown-faces)

(defface markdown-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'markdown-faces)

(defface markdown-missing-link-face
  '((t (:inherit font-lock-warning-face)))
  "Face for missing links."
  :group 'markdown-faces)

(defface markdown-reference-face
  '((t (:inherit font-lock-type-face)))
  "Face for link references."
  :group 'markdown-faces)

(defface markdown-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'markdown-faces)

(defface markdown-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs."
  :group 'markdown-faces)

(defface markdown-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'markdown-faces)

(defface markdown-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'markdown-faces)

(defface markdown-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'markdown-faces)

(defface markdown-erb-face
  '((t (:inherit font-lock-string-face)))
  "Face for ERB templates."
  :group 'markdown-faces)


(defconst markdown-regex-link-inline
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).")

(defconst markdown-regex-link-reference
  "\\(!?\\[[^]]+?\\]\\)[ ]?\\(\\[[^]]*?\\]\\)"
  "Regular expression for a reference link [text][id].")

(defconst markdown-regex-reference-definition
  "^ \\{0,3\\}\\(\\[[^^]+?\\]\\):\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a link definition [id]: ...")

(defconst markdown-regex-footnote
  "\\(\\[\\^.+?\\]\\)"
  "Regular expression for a footnote marker [^fn].")

(defconst markdown-regex-header
  "^\\(?:\\(.+\\)\n\\(===+\\)\\|\\(.+\\)\n\\(---+\\)\\|\\(#+\\)\\s-*\\(.*?\\)\\s-*?\\(#*\\)\\)$"
  "Regexp identifying Markdown headers.")

(defconst markdown-regex-header-1-atx
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1 atx-style (hash mark) headers.")

(defconst markdown-regex-header-2-atx
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2 atx-style (hash mark) headers.")

(defconst markdown-regex-header-3-atx
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3 atx-style (hash mark) headers.")

(defconst markdown-regex-header-4-atx
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4 atx-style (hash mark) headers.")

(defconst markdown-regex-header-5-atx
  "^\\(##### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 5 atx-style (hash mark) headers.")

(defconst markdown-regex-header-6-atx
  "^\\(###### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 6 atx-style (hash mark) headers.")

(defconst markdown-regex-header-1-setext
  "^\\(.*\\)\n\\(===+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst markdown-regex-header-2-setext
  "^\\(.*\\)\n\\(---+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

(defconst markdown-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching Markdown horizontal rules.")

(defconst markdown-regex-code
  "\\(^\\|[^\\]\\)\\(\\(`+\\)\\(\\(.\\|\n[^\n]\\)*?[^`]\\)\\3\\)"
  "Regular expression for matching inline code fragments.

The first group ensures that the leading backquote character
is not escaped.  The group \\(.\\|\n[^\n]\\) matches any
character, including newlines, but not two newlines in a row.")

(defconst markdown-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst markdown-regex-list
  "^\\([ \t]*\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\([ \t]+\\)"
  "Regular expression for matching list items.")

(defconst markdown-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text.")

(defconst markdown-regex-italic
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching italic text.")

(defconst markdown-regex-blockquote
  "^[ \t]*\\(>\\).*$"
  "Regular expression for matching blockquote lines.")

(defconst markdown-regex-line-break
  "  $"
  "Regular expression for matching line breaks.")

(defconst markdown-regex-uri
  (concat
   "\\(" (mapconcat 'identity markdown-uri-types "\\|")
   "\\):[^]\t\n\r<>,;() ]+")
  "Regular expression for matching inline URIs.")

(defconst markdown-regex-angle-uri
  (concat
   "\\(<\\)\\(\\(?:"
   (mapconcat 'identity markdown-uri-types "\\|")
   "\\):[^]\t\n\r<>,;()]+\\)\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst markdown-regex-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst markdown-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions.")

(defconst markdown-regex-latex-display
  "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions.")

(defconst markdown-regex-list-indent
  "^\\(\\s *\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\(\\s +\\)"
  "Regular expression for matching indentation of list items.")

(defconst markdown-regex-erb
  "\\(<%.+?%>\\)"
  "Regular expression for an ERB template <% %>.")

(defvar markdown-mode-font-lock-keywords-basic
  (list
   ;; we don't mark normal pre blocks 'cause we don't use them and it interferes
   ;; with footnotes
   ;; (cons 'markdown-match-pre-blocks '((0 markdown-pre-face)))
   (cons 'markdown-match-fenced-code-blocks '((0 markdown-pre-face)))
   (cons markdown-regex-blockquote 'markdown-blockquote-face)
   (cons markdown-regex-header-1-setext '((1 markdown-header-face-1)
                                          (2 markdown-header-rule-face)))
   (cons markdown-regex-header-2-setext '((1 markdown-header-face-2)
                                          (2 markdown-header-rule-face)))
   (cons markdown-regex-header-1-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-1)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-2-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-2)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-3-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-3)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-4-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-4)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-5-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-5)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-6-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-6)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-hr 'markdown-header-face)
   (cons 'markdown-match-comments '((0 markdown-comment-face t t)))
   (cons markdown-regex-code '(2 markdown-inline-code-face))
   (cons markdown-regex-angle-uri 'markdown-link-face)
   (cons markdown-regex-uri 'markdown-link-face)
   (cons markdown-regex-email 'markdown-link-face)
   (cons markdown-regex-list '(2 markdown-list-face))
   (cons markdown-regex-link-inline '((1 markdown-link-face t)
                                      (2 markdown-url-face t)))
   (cons markdown-regex-link-reference '((1 markdown-link-face t)
                                         (2 markdown-reference-face t)))
   (cons markdown-regex-reference-definition '((1 markdown-reference-face t)
                                               (2 markdown-url-face t)
                                               (3 markdown-link-title-face t)))
   (cons markdown-regex-footnote 'markdown-footnote-face)
   (cons markdown-regex-bold '(2 markdown-bold-face))
   (cons markdown-regex-italic '(2 markdown-italic-face))
   (cons markdown-regex-erb 'markdown-erb-face)
   )
  "Syntax highlighting for Markdown files.")

(defconst markdown-mode-font-lock-keywords-latex
  (list
   ;; Math mode $..$ or $$..$$
   (cons markdown-regex-latex-expression '(2 markdown-math-face))
   ;; Display mode equations with brackets: \[ \]
   (cons markdown-regex-latex-display 'markdown-math-face)
   ;; Equation reference (eq:foo)
   (cons "(eq:\\w+)" 'markdown-reference-face)
   ;; Equation reference \eqref{foo}
   (cons "\\\\eqref{\\w+}" 'markdown-reference-face))
  "Syntax highlighting for LaTeX fragments.")

(defvar markdown-mode-font-lock-keywords
  (append
   (if markdown-enable-math
       markdown-mode-font-lock-keywords-latex)
   markdown-mode-font-lock-keywords-basic)
  "Default highlighting expressions for Markdown mode.")

;; Footnotes
(defvar markdown-footnote-counter 0
  "Counter for footnote numbers.")
(make-variable-buffer-local 'markdown-footnote-counter)

(defconst markdown-footnote-chars
  "[[:alnum:]-]"
  "Regular expression maching any character that is allowed in a footnote identifier.")



;;; Compatibility =============================================================

;; Handle replace-regexp-in-string in XEmacs 21
(defun markdown-replace-regexp-in-string (regexp rep string)
  "Compatibility wrapper to provide `replace-regexp-in-string'.
Replace all matches for REGEXP with REP in STRING."
  (if (featurep 'xemacs)
      (replace-in-string string regexp rep)
    (replace-regexp-in-string regexp rep string)))



;;; Markdown parsing functions ================================================

(defun markdown-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (point-at-eol) t)))

(defun markdown-prev-line-blank-p ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-min))
        t
      (forward-line -1)
      (markdown-cur-line-blank-p))))

(defun markdown-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-max))
        t
      (forward-line 1)
      (markdown-cur-line-blank-p))))

(defun markdown-prev-line-indent-p ()
  "Return t if the previous line is indented and nil otherwise."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (if (re-search-forward "^\\s " (point-at-eol) t) t)))

(defun markdown-cur-line-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward "^\\s +" (point-at-eol) t)
    (current-column)))

(defun markdown-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (forward-line -1)
    (when (markdown-cur-line-blank-p)
      (forward-line -1))
    (markdown-cur-line-indent)))

(defun markdown-next-line-indent ()
  "Return the number of leading whitespace characters in the next line."
  (save-excursion
    (forward-line 1)
    (markdown-cur-line-indent)))

(defun markdown-cur-non-list-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward markdown-regex-list (point-at-eol) t)
      (current-column))))

(defun markdown-prev-non-list-indent ()
  "Return position of the first non-list-marker on the previous line.
If the previous line is empty, check the line before that one, too."
  (save-excursion
    (forward-line -1)
    (when (markdown-cur-line-blank-p)
       (forward-line -1))
    (markdown-cur-non-list-indent)))

(defun markdown--next-block ()
  "Move the point to the start of the next text block."
  (forward-line)
  (while (and (or (not (markdown-prev-line-blank-p))
                  (markdown-cur-line-blank-p))
              (not (eobp)))
    (forward-line)))

(defun markdown--end-of-level (level)
  "Move the point to the end of region with indentation at least LEVEL."
  (let (indent)
    (while (and (not (< (setq indent (markdown-cur-line-indent)) level))
                (not (>= indent (+ level 4)))
                (not (eobp)))
      (markdown--next-block))
    (unless (eobp)
      ;; Move back before any trailing blank lines
      (while (and (markdown-prev-line-blank-p)
                  (not (bobp)))
        (forward-line -1))
      (forward-line -1)
      (end-of-line))))

(defun markdown-prev-list-item (level)
  "Search backward from point for a list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev)
    (setq prev (point))
    (forward-line -1)
    (setq indent (markdown-cur-line-indent))
    (while
        (cond
         ;; Stop at beginning of buffer
         ((bobp) (setq prev nil))
         ;; Continue if current line is blank
         ((markdown-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at markdown-regex-list)
               (setq bounds (markdown-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq prev (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq prev nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the next is blank
         ((and (< indent level)
               (markdown-next-line-blank-p))
          (setq prev nil))
         ;; Stop at a header
         ((looking-at markdown-regex-header) (setq prev nil))
         ;; Stop at a horizontal rule
         ((looking-at markdown-regex-hr) (setq prev nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line -1)
      (setq indent (markdown-cur-line-indent)))
    prev))

(defun markdown-next-list-item (level)
  "Search forward from point for the next list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent next)
    (setq next (point))
    (forward-line)
    (setq indent (markdown-cur-line-indent))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) (setq prev nil))
         ;; Continue if the current line is blank
         ((markdown-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at markdown-regex-list)
               (setq bounds (markdown-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq next (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq next nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (markdown-prev-line-blank-p))
          (setq next nil))
         ;; Stop at a header
         ((looking-at markdown-regex-header) (setq next nil))
         ;; Stop at a horizontal rule
         ((looking-at markdown-regex-hr) (setq next nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (markdown-cur-line-indent)))
    next))

(defun markdown-cur-list-item-end (level)
  "Move to the end of the current list item with nonlist indentation LEVEL.
If the point is not in a list item, do nothing."
  (let (indent)
    (forward-line)
    (setq indent (markdown-cur-line-indent))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue if the current line is blank
         ((markdown-cur-line-blank-p) t)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (markdown-prev-line-blank-p))
          nil)
         ;; Stop at a new list item of the same or lesser indentation
         ((looking-at markdown-regex-list) nil)
         ;; Stop at a header
         ((looking-at markdown-regex-header) nil)
         ;; Stop at a horizontal rule
         ((looking-at markdown-regex-hr) nil)
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (markdown-cur-line-indent)))
    ;; Don't skip over whitespace for empty list items (marker and
    ;; whitespace only), just move to end of whitespace.
    (if (looking-back (concat markdown-regex-list "\\s-*"))
          (goto-char (match-end 3))
      (skip-syntax-backward "-"))))

(defun markdown-cur-list-item-bounds ()
  "Return bounds and indentation of the current list item.
Return a list of the form (begin end indent nonlist-indent).
If the point is not inside a list item, return nil.
Leave match data intact for `markdown-regex-list'."
  (let (cur prev-begin prev-end indent nonlist-indent)
    ;; Store current location
    (setq cur (point))
    ;; Verify that cur is between beginning and end of item
    (save-excursion
      (if (looking-at markdown-regex-list)
          (beginning-of-line)
        (end-of-line)
        (re-search-backward markdown-regex-list nil t))
      (save-match-data
        (setq prev-begin (point))
        (setq indent (markdown-cur-line-indent))
        (setq nonlist-indent (markdown-cur-non-list-indent))
        (markdown-cur-list-item-end nonlist-indent)
        (setq prev-end (point)))
      (if (and (>= cur prev-begin)
               (<= cur prev-end)
               nonlist-indent)
          (list prev-begin prev-end indent nonlist-indent)
        nil))))

;; From html-helper-mode
(defun markdown-match-comments (last)
  "Match HTML comments from the point to LAST."
  (cond ((search-forward "<!--" last t)
         (backward-char 4)
         (let ((beg (point)))
           (cond ((search-forward-regexp "--[ \t]*>" last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun markdown-match-pre-blocks (last)
  "Match Markdown pre blocks from point to LAST.
A region matches as if it is indented at least four spaces
relative to the nearest previous block of lesser non-list-marker
indentation."

  (let (cur-begin cur-end cur-indent prev-indent prev-list stop match found)
    ;; Don't start in the middle of a block
    (unless (and (bolp)
                 (markdown-prev-line-blank-p)
                 (not (markdown-cur-line-blank-p)))
      (markdown--next-block))

    ;; Move to the first full block in the region with indent 4 or more
    (while (and (not (>= (setq cur-indent (markdown-cur-line-indent)) 4))
                (not (>= (point) last)))
      (markdown--next-block))
    (setq cur-begin (point))
    (markdown--end-of-level cur-indent)
    (setq cur-end (point))
    (setq match nil)
    (setq stop (> cur-begin cur-end))

    (while (and (<= cur-end last) (not stop) (not match))
      ;; Move to the nearest preceding block of lesser (non-marker) indentation
      (setq prev-indent (+ cur-indent 1))
      (goto-char cur-begin)
      (setq found nil)
      (while (and (>= prev-indent cur-indent)
                  (not (and prev-list
                            (eq prev-indent cur-indent)))
                  (not (bobp)))

        ;; Move point to the last line of the previous block.
        (forward-line -1)
        (while (and (markdown-cur-line-blank-p)
                    (not (bobp)))
          (forward-line -1))

        ;; Update the indentation level using either the
        ;; non-list-marker indentation, if the previous line is the
        ;; start of a list, or the actual indentation.
        (setq prev-list (markdown-cur-non-list-indent))
        (setq prev-indent (or prev-list
                              (markdown-cur-line-indent)))
        (setq found t))

      ;; If the loop didn't execute
      (unless found
        (setq prev-indent 0))

      ;; Compare with prev-indent minus its remainder mod 4
      (setq prev-indent (- prev-indent (mod prev-indent 4)))

      ;; Set match data and return t if we have a match
      (if (>= cur-indent (+ prev-indent 4))
          ;; Match
          (progn
            (setq match t)
            (set-match-data (list cur-begin cur-end))
            ;; Leave point at end of block
            (goto-char cur-end)
            (forward-line))

        ;; Move to the next block (if possible)
        (goto-char cur-end)
        (markdown--next-block)
        (setq cur-begin (point))
        (setq cur-indent (markdown-cur-line-indent))
        (markdown--end-of-level cur-indent)
        (setq cur-end (point))
        (setq stop (equal cur-begin cur-end))))
    match))

(defun markdown-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (cond ((search-forward-regexp "^\\([~]\\{3,\\}\\)" last t)
         (beginning-of-line)
         (let ((beg (point)))
           (forward-line)
           (cond ((search-forward-regexp
                   (concat "^" (match-string 1) "~*") last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun markdown-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (re-search-backward "\n\n" nil t)))
      (when found
        (goto-char font-lock-end)
        (when (re-search-forward "\n\n" nil t)
          (beginning-of-line)
          (setq font-lock-end (point)))
        (setq font-lock-beg found)))))



;;; Syntax Table ==============================================================

(defvar markdown-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." tab)
    tab)
  "Syntax table for `markdown-mode'.")



;;; Indentation ====================================================================

(defun markdown-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(defun markdown-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `markdown-enter-key' or
`markdown-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `markdown-enter-key', by an initial call of
`markdown-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position."
  (interactive)
  (let ((positions (markdown-calc-indents))
        (cur-pos (current-column)))
    (if (not (equal this-command 'markdown-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (indent-line-to
       (markdown-indent-find-next-position cur-pos positions)))))

(defun markdown-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level."
  (let (pos prev-line-pos positions)

    ;; Previous line indent
    (setq prev-line-pos (markdown-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Previous non-list-marker indent
    (setq pos (markdown-prev-non-list-indent))
    (when pos
      (setq positions (cons pos positions))
      (setq positions (cons (+ pos tab-width) positions)))

    ;; Indentation of the previous line + tab-width
    (cond
     (prev-line-pos
      (setq positions (cons (+ prev-line-pos tab-width) positions)))
     (t
      (setq positions (cons tab-width positions))))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos
             (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of preceeding list item
    (setq pos
          (save-excursion
            (forward-line -1)
            (catch 'break
              (while (not (equal (point) (point-min)))
                (forward-line -1)
                (goto-char (point-at-bol))
                (when (re-search-forward markdown-regex-list (point-at-eol) t)
                  (throw 'break (length (match-string 1)))))
              nil)))
    (if (and pos (not (eq pos prev-line-pos)))
        (setq positions (cons pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    (reverse positions)))

(defun markdown-dedent-or-delete (arg)
  "Handle BACKSPACE by cycling through indentation points.
When BACKSPACE is pressed, if there is only whitespace
before the current point, then dedent the line one level.
Otherwise, do normal delete by repeating
`backward-delete-char-untabify' ARG times."
  (interactive "*p")
  (let ((cur-pos (current-column))
        (start-of-indention (save-excursion
                              (back-to-indentation)
                              (current-column))))
    (if (and (> cur-pos 0) (= cur-pos start-of-indention))
        (let ((result 0))
          (dolist (i (markdown-calc-indents))
            (when (< i cur-pos)
              (setq result (max result i))))
          (indent-line-to result))
      (backward-delete-char-untabify arg))))



;;; Keymap ====================================================================

(defvar markdown-mode-map
  (let ((map (make-keymap)))
    ;; Indentation
    (define-key map (kbd "<backspace>") 'markdown-dedent-or-delete)
    ;; Visibility cycling
    (define-key map "\C-i" 'markdown-cycle)
    (define-key map "\M-i" 'markdown-shifttab)
    ;; Lists
    (define-key map "\C-c\C-cn" 'markdown-cleanup-list-numbers)
    (define-key map (kbd "M-<up>") 'markdown-metaup)
    (define-key map (kbd "M-<down>") 'markdown-metadown)
    (define-key map (kbd "M-<left>") 'markdown-metaleft)
    (define-key map (kbd "M-<right>") 'markdown-metaright)
    (define-key map (kbd "M-<return>") 'markdown-insert-list-item)
    map)
  "Keymap for Markdown major mode.")

;;; Lists =====================================================================

(defun markdown-insert-list-item (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is 4),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is 16),
increase the indentation by one level."
  (interactive "p")
  (let (bounds item-indent marker indent new-indent end)
    (save-match-data
      (setq bounds (markdown-cur-list-item-bounds))
      (if (not bounds)
          ;; When not in a list, start a new unordered one
          (progn
            (unless (markdown-cur-line-blank-p)
              (insert "\n"))
            (insert "* "))
        ;; Compute indentation for a new list item
        (setq item-indent (nth 2 bounds))
        (setq marker (concat (match-string 2) (match-string 3)))
        (setq indent (cond
                      ((= arg 4) (max (- item-indent 4) 0))
                      ((= arg 16) (+ item-indent 4))
                      (t item-indent)))
        (setq new-indent (make-string indent 32))
        (goto-char (nth 1 bounds))
        (newline)
        (cond
         ;; Ordered list
         ((string-match "[0-9]" marker)
          (if (= arg 16) ;; starting a new column indented one more level
              (insert (concat new-indent "1. "))
            ;; travel up to the last item and pick the correct number.  If
            ;; the argument was nil, "new-indent = item-indent" is the same,
            ;; so we don't need special treatment. Neat.
            (save-excursion
              (while (not (looking-at (concat new-indent "\\([0-9]+\\)\\.")))
                (forward-line -1)))
            (insert (concat new-indent
                            (int-to-string (1+ (string-to-number (match-string 1))))
                            ". "))))
         ;; Unordered list
         ((string-match "[\\*\\+-]" marker)
          (insert (concat new-indent marker))))))))

(defun markdown-move-list-item-up ()
  "Move the current list item up in the list when possible."
  (interactive)
  (let (cur prev old)
    (when (setq cur (markdown-cur-list-item-bounds))
      (setq old (point))
      (goto-char (nth 0 cur))
      (if (markdown-prev-list-item (nth 3 cur))
          (progn
            (setq prev (markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 prev) (nth 1 prev)
                                     (nth 0 cur) (nth 1 cur) t)
                  (goto-char (+ (nth 0 prev) (- old (nth 0 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun markdown-move-list-item-down ()
  "Move the current list item down in the list when possible."
  (interactive)
  (let (cur next old)
    (when (setq cur (markdown-cur-list-item-bounds))
      (setq old (point))
      (if (markdown-next-list-item (nth 3 cur))
          (progn
            (setq next (markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 cur) (nth 1 cur)
                                     (nth 0 next) (nth 1 next) nil)
                  (goto-char (+ old (- (nth 1 next) (nth 1 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun markdown-promote-list-item ()
  "Indent the current list item."
  (interactive)
  (let ((bounds (markdown-cur-list-item-bounds))
        (ins 0))
    (when bounds
      (save-excursion
        (save-match-data
          (goto-char (nth 0 bounds))
          (while (< (point) (+ (nth 1 bounds) ins))
            (unless (markdown-cur-line-blank-p)
              (insert "    ")
              (setq ins (+ ins 4)))
            (forward-line)))))))

(defun markdown-demote-list-item ()
  "Unindent the current list item."
  (interactive)
  (let (bounds num del regexp)
    (when (setq bounds (markdown-cur-list-item-bounds))
      (save-excursion
        (save-match-data
          (goto-char (nth 0 bounds))
          (setq del 0)
          (when (looking-at "^[ ]\\{1,4\\}")
            (setq num (- (match-end 0) (match-beginning 0)))
            (setq regexp (format "^[ ]\\{1,%d\\}" num))
            (while (re-search-forward regexp (- (nth 1 bounds) del) t)
              (replace-match "" nil nil)
              (setq num (- (match-end 0) (match-beginning 0)))
              (setq del (+ del num))
              (forward-line))))))))

(defun markdown--cleanup-list-numbers-level (&optional pfx)
  "Update the numbering for level PFX (as a string of spaces).

Assume that the previously found match was for a numbered item in
a list."
  (let ((cpfx pfx)
        (idx 0)
        (continue t)
        (step t)
        (sep nil))
    (while (and continue (not (eobp)))
      (setq step t)
      (cond
       ((looking-at "^\\([\s-]*\\)[0-9]+\\. ")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ((string= cpfx pfx)
          (replace-match
           (concat pfx (number-to-string  (setq idx (1+ idx))) ". "))
          (setq sep nil))
         ;; indented a level
         ((string< pfx cpfx)
          (setq sep (markdown--cleanup-list-numbers-level cpfx))
          (setq step nil))
         ;; exit the loop
         (t
          (setq step nil)
          (setq continue nil))))

       ((looking-at "^\\([\s-]*\\)[^ \t\n\r].*$")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ;; reset if separated before
         ((string= cpfx pfx) (when sep (setq idx 0)))
         ((string< cpfx pfx)
          (setq step nil)
          (setq continue nil))))
       (t (setq sep t)))

      (when step
        (beginning-of-line)
        (setq continue (= (forward-line) 0))))
    sep))

(defun markdown-cleanup-list-numbers ()
  "Update the numbering of ordered lists."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (markdown--cleanup-list-numbers-level "")))


;;; Outline ===================================================================

;; The following visibility cycling code was taken from org-mode
;; by Carsten Dominik and adapted for markdown-mode.

; TODO better display, maybe some additional fold marker? 

(defvar markdown-cycle-global-status 1)
(defvar markdown-cycle-subtree-status nil)

;; Based on org-end-of-subtree from org.el
(defun markdown-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (funcall outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

;; Based on org-cycle from org.el.
(defun markdown-cycle (&optional arg)
  "Visibility cycling for Markdown mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, insert a tab using `indent-relative'."
  (interactive "P")
  (cond
   ((eq arg t) ;; Global cycling
    (cond
     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 2))
      ;; Move from overview to contents
      (hide-sublevels 1)
      (message "CONTENTS")
      (setq markdown-cycle-global-status 3))

     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 3))
      ;; Move from contents to all
      (show-all)
      (message "SHOW ALL")
      (setq markdown-cycle-global-status 1))

     (t
      ;; Defaults to overview
      (hide-body)
      (message "OVERVIEW")
      (setq markdown-cycle-global-status 2))))

   ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
    ;; At a heading: rotate between three different views
    (outline-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (outline-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (outline-end-of-heading)   (setq eoh (point))
        (markdown-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
        ;; Nothing is hidden behind this heading
        (message "EMPTY ENTRY")
        (setq markdown-cycle-subtree-status nil))
       ((>= eol eos)
        ;; Entire subtree is hidden in one line: open it
        (show-entry)
        (show-children)
        (message "CHILDREN")
        (setq markdown-cycle-subtree-status 'children))
       ((and (eq last-command this-command)
             (eq markdown-cycle-subtree-status 'children))
        ;; We just showed the children, now show everything.
        (show-subtree)
        (message "SUBTREE")
        (setq markdown-cycle-subtree-status 'subtree))
       (t
        ;; Default action: hide the subtree.
        (hide-subtree)
        (message "FOLDED")
        (setq markdown-cycle-subtree-status 'folded)))))

   (t
    (indent-for-tab-command))))

;; Based on org-shifttab from org.el.
(defun markdown-shifttab ()
  "Global visibility cycling.
Calls `markdown-cycle' with argument t."
  (interactive)
  (markdown-cycle t))

(defun markdown-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (cond
   ((match-end 1) 1)
   ((match-end 2) 2)
   ((- (match-end 0) (match-beginning 0)))))

;;; Structure Editing =========================================================

(defun markdown-metaup ()
  "Move list item up.
Calls `markdown-move-list-item-up'."
  (interactive)
  (markdown-move-list-item-up))

(defun markdown-metadown ()
  "Move list item down.
Calls `markdown-move-list-item-down'."
  (interactive)
  (markdown-move-list-item-down))

(defun markdown-metaleft ()
  "Unindent list item.
Calls `markdown-demote-list-item'."
  (interactive)
  (markdown-demote-list-item))

(defun markdown-metaright ()
  "Indent list item.
Calls `markdown-promote-list-item'."
  (interactive)
  (markdown-promote-list-item))


;;; Miscellaneous =============================================================

(defun markdown-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun markdown-nobreak-p ()
  "Return nil if it is acceptable to break the current line at the point."
  ;; inside in square brackets (e.g., link anchor text)
  (looking-back "\\[[^]]*"))



;;; Mode definition  ==========================================================

(defun markdown-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "markdown-mode, version %s" markdown-mode-version))

;;;###autoload
(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for editing Markdown files."
  ;; Natural Markdown tab width
  (setq tab-width 4)
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "<!--[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(markdown-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; Make filling work with lists (unordered, ordered, and definition) and quotes.
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|^[ \t]*[*+-] \\|^[ \t]*[0-9]+\\.\\|^[ \t]*: \\|^[ \t]*> ")
  ;; Outline mode
  (make-local-variable 'outline-regexp)
  (setq outline-regexp markdown-regex-header)
  (make-local-variable 'outline-level)
  (setq outline-level 'markdown-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p)
  ;; (setq indent-line-function markdown-indent-function)


  ;; Multiline font lock
  (add-hook 'font-lock-extend-region-functions
            'markdown-font-lock-extend-region)
  )

(provide 'markdown-mode)

;;; markdown-mode.el ends here
