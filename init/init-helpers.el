;; misc helper functions

;; common lisp stuff
(require 'cl-lib)

;; better convenience functions for strings and map/reduce/loop
(require 'dash)
(require 's)

;; sane regexes
(require 'rx)
(require 'ample-regexps)

;; replace obsolete flet
(require 'noflet)

;; some generic aliases that make elisp less painful
(defalias 'first 	'cl-first)
(defalias 'head  	'cl-first)
(defalias 'hd    	'cl-first)
(defalias 'second	'cl-second)
(defalias 'rest  	'cl-rest)
(defalias 'tail  	'cl-rest)
(defalias 'tl    	'cl-rest)
(defalias 'loop  	'cl-loop)
(defalias 'case  	'cl-case)
(defalias 'fn    	'lambda)

(defun read-lines (filename)
  "Return a list of lines of a file at FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun region-as-string ()
  "Return region as string."
  (buffer-substring (region-beginning)
                    (region-end)))

(defun word-or-region ()
  "Returns word boundary or selected region."
  (let (beg
        end
        (deactivate-mark 	nil)
        (case-fold-search	nil))

    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (setq beg (first bounds)
              end (rest bounds))))

    (list beg end)))

(defun pretty-load? ()
  "load stuff like themes that are only meaningful in window system?"
  (or (display-graphic-p)
      (daemonp)))

(provide 'init-helpers)
