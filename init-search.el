;; searches and a plethora of regex engines

;; more useful kill-ring
(setup "kill-ring-search"
  (global-set-key "\M-\C-y" 'kill-ring-search))
(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1))
(global-set-key "\M-Y" 'yank-pop-reverse)

;; goto and hint-style navigation
(setup-lazy '(ace-jump-mode ace-jump-char-mode ace-jump-line-mode) "ace-jump-mode")
(setup-lazy '(ace-jump-buffer) "ace-jump-buffer")
(setup-lazy '(ace-link) "ace-link")
(setup-lazy '(ace-window) "ace-window")

(global-set-key (kbd "M-g M-g") 'goto-line)
(global-set-key (kbd "M-g l")   'goto-line)
(global-set-key (kbd "M-g b")   'ace-jump-buffer)
(global-set-key (kbd "M-g c")   'ace-jump-char-mode)
(global-set-key (kbd "M-g g")   'ace-jump-mode)
(global-set-key (kbd "M-g s")   'ace-jump-line-mode)
(global-set-key (kbd "M-g w")   'ace-window)

(setup-after "ace-window"
  ;; help pages don't have other input, so skip the M-g prefix
  (setup "info"
    (define-key Info-mode-map "l" 'ace-link-info))
  (setup "help-mode"
    (define-key help-mode-map "l" 'ace-link-help)))

(setup-lazy '(phi-search phi-search-backward) "phi-search")
(global-set-key (kbd "C-c C-s") 'phi-search)
(global-set-key (kbd "C-c C-r") 'phi-search-backward)

(setup-after "phi-search"
  (setup "phi-search-mc"
    (define-key phi-search-default-map (kbd "<C-down>") 'phi-search-mc/mark-next)
    (define-key phi-search-default-map (kbd "<C-up>")   'phi-search-mc/mark-previous)
    (define-key phi-search-default-map (kbd "C-c C-k")  'phi-search-mc/mark-all)))

;; support for bookmarks (broken; resurrect this at some point...)
;; (require 'breadcrumb)
;; (global-set-key (kbd "C-c m") 'bc-set)
;; (global-set-key (kbd "M-SPC") 'bc-previous)
;; (global-set-key (kbd "M-S-SPC") 'bc-next)
;; (setq bc-bookmark-limit 1000)
;; (setq bc-bookmark-file (expand-file-name "~/.emacs.d/cache/breadcrumb"))
;; ;; normal bookmarks
;; (setq bookmark-default-file "~/.emacs.d/cache/bookmarks")

;; better search/replace
(setup-lazy '(vr/query-replace vr/query-replace-from-beginning) "visual-regexp")

(setup-after "visual-regexp"
 (setup "visual-regexp-steroids"))

(defun vr/query-replace-from-beginning ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'vr/query-replace)))

(global-set-key (kbd "C-c r") 'vr/query-replace)
(global-set-key (kbd "C-c R") 'vr/query-replace-from-beginning)

;; ido and smex (ido for M-x)
(setup "flx-ido"
  (ido-mode 1)

  (setq ido-enable-flex-matching t) ; fuzzy matching
  (setq ido-use-filename-at-point nil)
  (setq ido-use-url-at-point nil)
  (global-set-key "\C-x\M-f" 'find-file-at-point)
  (setq ido-use-virtual-buffers t)
  (setq ido-default-file-method 'selected-window) ; ignore buffers in different frames
  (setq ido-default-buffer-method 'selected-window) ; ignore buffers in different frames
  (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
  (setq ido-ignore-buffers
        '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
          "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))
  (setq ido-case-fold t) ; case insensitive
  (setq ido-enable-last-directory-history t)
  (setq ido-max-work-directory-list 30)
  (setq ido-max-work-file-list 100)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-directory-size 1000000) ; load bigger dirs, too
  (setq confirm-nonexistent-file-or-buffer nil))

(setup-after "ido"
  (setup "ido-ubiquitous"
    (setq ido-everywhere t)))

;; smex
(setup "smex"
  (setq smex-save-file "~/.emacs.d/cache/smex-items")
  (smex-initialize)
  (global-set-key "\M-x" 'smex)
  (global-set-key "\M-X" 'smex-major-mode-commands))

;; recent files
(setup "recentf"
  (setq recentf-max-saved-items 1000)
  (setq recentf-save-file "~/.emacs.d/cache/recentf")
  (setq recentf-exclude (append recentf-exclude
                                '("\.emacs\.d/cache"
                                  "\.emacs\.d/packages")))
  (recentf-mode 1)

  ;; file completion
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))
  (global-set-key "\C-x\C-r" 'recentf-ido-find-file))

;; use regexp search and selected region (if any) by default
(defun region-as-string ()
  (buffer-substring (region-beginning)
                    (region-end)))

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

(global-set-key (kbd "C-s")   'isearch-forward-use-region)
(global-set-key (kbd "C-r")   'isearch-backward-use-region)
(global-set-key (kbd "C-S-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)
;; make backspace more intuitive
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

;; normalize search string so unicode diacritics work normally
(defun isearch-normalize-string ()
  (interactive)
  (let* ((string (ucs-normalize-NFKC-string isearch-string)))
    (setq isearch-string string
          isearch-message (mapconcat 'isearch-text-char-description string ""))
    (isearch-search-and-update)))
(define-key isearch-mode-map (kbd "C-c C-c") 'isearch-normalize-string)
(define-key isearch-mode-map (kbd "C-c C-w") 'isearch-toggle-word)
(define-key isearch-mode-map (kbd "C-c C-r") 'isearch-toggle-regexp)
(define-key isearch-mode-map (kbd "C-c C-i") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-c C-s") 'isearch-toggle-symbol)
(define-key isearch-mode-map (kbd "C-c C-SPC") 'isearch-toggle-lax-whitespace)
(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)

;; wrap search
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; fast navigation
(setup "imenu"
  (set-default 'imenu-auto-rescan t)

  (defun imenu-flush-cache ()
    "Flushes imenu cache."
    (interactive)
    (setq imenu--index-alist nil)))

(setup-after "imenu"
  (setup "idomenu"
    (define-key global-map (kbd "C-c [")   'idomenu)
    (define-key global-map (kbd "C-c C-[") 'idomenu))
  (setup "imenu-anywhere"
    (define-key global-map (kbd "C-c ]")   'imenu-anywhere)
    (define-key global-map (kbd "C-c C-]") 'imenu-anywhere)))

;; recentering
(setq recenter-positions '(2 middle))
(add-hook 'imenu-after-jump-hook 'recenter-top-bottom)

;; helm
(setup "helm-config")
  ;; (helm-mode t))
(setup-after "helm"
  (define-key helm-map (kbd "C-w")  'subword-backward-kill)
  (define-key helm-map (kbd "M-w")  'helm-yank-text-at-point)
  (global-set-key (kbd "C-x c t")   'helm-cmd-t)
  (global-set-key (kbd "C-x c g")   'helm-do-grep)
  (global-set-key (kbd "C-x c o")   'helm-occur)
  (global-set-key (kbd "C-x c C-o") 'helm-swoop)
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-ff-lynx-style-map nil
        helm-input-idle-delay 0.1
        helm-idle-delay 0.1
        helm-follow-mode-persistent t ))

(provide 'init-search)