;;; conlang.el ---

(require 'cl-lib)
(require 'ample-regexps)
(require 'rx)

(define-arx conlang/rx
  '((prompt 	"^[ \t]*[!]")
    (lexicon	"^[ \t]*[?] ")
    (word   	"[::alpha::]+")
    ))

(defun conlang/lexicon-update (filename)
  "maintains a lexicon file"

  (let ((case-fold-search	nil)
        word
        words
        changed)
    (save-excursion
      (goto-char (point-min))

      (while (re-search-forward (conlang/rx prompt) nil t)
        (save-excursion
          (while (re-search-forward (conlang/rx word) (point-at-eol) t)
            (setq word (match-string 0))
            (add-to-list 'words word)
            )))

      ;; keep original word order though
      (setq words (nreverse words))

      (with-temp-buffer
        (insert-file-contents filename)

        (--each words
          (save-excursion
            (goto-char (point-min))
            (unless (re-search-forward
                     (concat (conlang/rx lexicon) it) nil t)
              (setq changed t)
              (goto-char (point-max))
              (unless (looking-at-p "[ \t]*$")
                (newline))
              (insert (format "! %s\t\n" it)))))

        (when changed
          (elastic-align-region (point-min) (point-max))
          (write-region (point-min) (point-max) filename))
        ))))

(defun conlang/lexicon-hook (filename)
  "update the lexicon every time you save the file"

  (add-hook 'after-save-hook
            (eval `(lambda () (conlang/lexicon-update ,filename)))
            t t))

(provide 'conlang)
;;; conlang.el ends here
