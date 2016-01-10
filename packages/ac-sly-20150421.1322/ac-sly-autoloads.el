;;; ac-sly-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ac-sly" "ac-sly.el" (22161 57234 446422 105000))
;;; Generated autoloads from ac-sly.el

(defface ac-sly-menu-face '((t (:inherit ac-candidate-face))) "\
Face for slime candidate menu." :group (quote auto-complete))

(defface ac-sly-selection-face '((t (:inherit ac-selection-face))) "\
Face for the slime selected candidate." :group (quote auto-complete))

(defvar ac-source-sly-fuzzy '((init . ac-sly-init) (candidates . ac-source-sly-fuzzy-candidates) (candidate-face . ac-sly-menu-face) (selection-face . ac-sly-selection-face) (prefix . sly-symbol-start-pos) (symbol . "l") (match lambda (prefix candidates) candidates) (document . ac-sly-documentation)) "\
Source for fuzzy slime completion.")

(defvar ac-source-sly-simple '((init . ac-sly-init) (candidates . ac-source-sly-simple-candidates) (candidate-face . ac-sly-menu-face) (selection-face . ac-sly-selection-face) (prefix . sly-symbol-start-pos) (symbol . "l") (document . ac-sly-documentation) (match . ac-source-sly-case-correcting-completions)) "\
Source for slime completion.")

(autoload 'set-up-sly-ac "ac-sly" "\
Add an optionally-fuzzy slime completion source to `ac-sources'.

\(fn &optional FUZZY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ac-sly-autoloads.el ends here
