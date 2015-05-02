;; searches and a plethora of regex engines

;; general options
(setq case-fold-search nil)

;; more useful kill-ring
(require 'kill-ring-search)

(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1))

;; goto and hint-style navigation
(require 'ace-jump-mode)
(require 'ace-jump-buffer)
(setq ace-jump-mode-scope 'window)

;; use saner keys, and order
(setq ace-jump-mode-move-keys
      (loop for c in (split-string "enaritoschwklgvfudzbpmjyxq" "" t)
            collect (string-to-char c)))

(require 'phi-search)

;; better search/replace
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(defun vr/query-replace-from-beginning ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'vr/query-replace)))

(setq vr/engine 'emacs)

;; ido and smex (ido for M-x)
(require 'flx-ido)
(require 'ido-ubiquitous)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(setq ido-enable-flex-matching t) ; fuzzy matching
(setq ido-use-filename-at-point nil)
(setq ido-use-url-at-point nil)
(setq ido-use-virtual-buffers t)
(setq ido-default-file-method 'selected-window) ; ignore buffers in different frames
(setq ido-default-buffer-method 'selected-window) ; ignore buffers in different frames
(setq ido-save-directory-list-file (emacs-d "cache/ido.last"))
(setq ido-ignore-buffers
      '("\\` "
        "^\\*Backtrace\\*$"
        "^\\*.*Completions\\*$"
        "^\\*Compile-Log\\*$"
        "\\.elc$"))
(setq ido-case-fold t) ; case insensitive
(setq ido-enable-last-directory-history t)
(setq ido-max-work-directory-list 30)
(setq ido-max-work-file-list 100)
(setq ido-create-new-buffer 'always)
(setq ido-max-directory-size 1000000) ; load bigger dirs, too
(setq confirm-nonexistent-file-or-buffer nil)

;; smex
(require 'smex)
(setq smex-save-file (emacs-d "cache/smex-items"))
(smex-initialize)

;; recent files
(require 'recentf)
(setq recentf-max-saved-items 1000)
(setq recentf-save-file (emacs-d "cache/recentf"))
(setq recentf-exclude (append recentf-exclude
                              '("\\.emacs\\.d/cache"
                                "\\.emacs\\.d/packages")))
(recentf-mode 1)

;; file completion
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; use regexp search and selected region (if any) by default
(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'regexp-search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward-regexp))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'regexp-search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward-regexp))

;; normalize search string so unicode diacritics work normally
(defun isearch-normalize-string ()
  (interactive)
  (let* ((string (ucs-normalize-NFKC-string isearch-string)))
    (setq isearch-string string
          isearch-message (mapconcat 'isearch-text-char-description string ""))
    (isearch-search-and-update)))

(require 'occur-x)
(defun turn-off-occur-x-mode () (occur-x-mode -1))

(add-hook 'occur-mode-hook     	'turn-on-occur-x-mode)
(add-hook 'occur-edit-mode-hook	'turn-off-occur-x-mode)

;; wrap search
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; fast navigation
(require 'imenu)
(require 'idomenu)
(require 'imenu-anywhere)
(set-default 'imenu-auto-rescan t)

(defun imenu-flush-cache ()
  "Flushes imenu cache."
  (interactive)
  (setq imenu--index-alist nil))

;; recentering
(setq recenter-positions '(2 middle))
(add-hook 'imenu-after-jump-hook 'recenter-top-bottom)

;; better grep
(require 'phi-grep)
(setq phi-grep-window-height 40)
(setq phi-grep-make-backup-function nil)

(provide 'init-search)
