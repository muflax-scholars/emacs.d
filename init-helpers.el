;; misc helper functions

(setup "s")

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


(provide 'init-helpers)
