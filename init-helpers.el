;; misc helper functions

(defun read-lines (filename)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))

(provide 'init-helpers)
