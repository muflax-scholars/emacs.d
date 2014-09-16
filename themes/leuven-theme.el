;;; leuven-theme.el --- Awesome Emacs color theme on white background

;; Copyright (C) 2003-2014 Free Software Foundation, Inc.

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/emacs-leuven-theme
;; Version: 20140113.328
;; X-Original-Version: 20140113.1205
;; Keywords: color theme

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Modified version of Leuven.

;;; Code:

(deftheme leuven
  "Non-stupid light background."

  ;; Create faces even if they don't exist yet.
  (put 'leuven 'theme-immediate t))

(let ((class '((class color) (min-colors 89)))
      ;; Leuven generic colors
      (cancel '(:slant italic :strike-through t :foreground "gray55"))
      (clock-line '(:box (:line-width 1 :color "#335EA8") :foreground "black" :background "#EEC900"))
      (code-block '(:foreground "#000088" :background "#FFFFE0"))
      (code-inline '(:foreground "#007300" :background "#EBF5EB"))
      (column '(:weight normal :slant normal :underline nil :strike-through nil :foreground "#E6AD4F" :background "#FFF2DE"))
      (diff-added '(:foreground "#008000" :background "#DDFFDD"))
      (diff-hunk-header '(:foreground "#990099" :background "#FFEEFF"))
      (diff-none '(:foreground "gray33"))
      (diff-removed '(:foreground "#A60000" :background "#FFDDDD"))
      (directory '(:weight bold :foreground "blue" :background "#FFFFD2"))
      (highlight-line '(:bold t :background nil)) ; #F5F5F5 or #CCDEED
      (link '(:underline t :foreground "#006FE0"))
      (mail-header-name '(:weight bold :foreground "black"))
      (marked-line '(:weight bold :foreground "white" :background "red"))
      (match '(:background "#FBE448"))
      (ol1 '(:weight bold :overline "#A7A7A7" :foreground "#3C3C3C" :background "#F0F0F0"))
      (ol2 '(:weight bold :overline "#123555" :foreground "#123555" :background "#E5F4FB"))
      (ol3 '(:weight bold :foreground "#005522" :background "#EFFFEF"))
      (ol4 '(:weight bold :slant normal :foreground "#EA6300"))
      (ol5 '(:weight bold :slant normal :foreground "#E3258D"))
      (ol6 '(:weight bold :slant italic :foreground "#0077CC"))
      (ol7 '(:weight bold :slant italic :foreground "#2EAE2C"))
      (ol8 '(:weight bold :slant italic :foreground "#FD8008"))
      (region '(:background "#ADD6FF")) ; #CCCCCC
      (shadow '(:foreground "#7F7F7F"))
      (string '(:foreground "#024c05" :background "#eff8e9")) ; #D0372D or #23238E or #20590C
      (subject '(:weight bold :foreground "#CF5D60"))
      (symlink '(:foreground "deep sky blue"))
      (volatile-highlight '(:background "#FBE448"))
      (vc-branch '(:box (:line-width 1 :color "#00CC33") :foreground "black" :background "#AAFFAA")))

  (custom-theme-set-faces
   'leuven

   `(default ((,class (:foreground "#000000" :background "#e6e6e6"))))
   `(bold ((,class (:weight bold :foreground "black"))))
   `(bold-italic ((,class (:weight bold :slant italic :foreground "black"))))
   `(italic ((,class (:slant italic :foreground "#1A1A1A"))))
   `(underline ((,class (:underline t))))
   `(cursor ((,class (:background "#0FB300"))))

   ;; Highlighting faces
   `(fringe ((,class (:foreground "#F7A421" :background "#F2F2F2"))))
   `(highlight ((,class ,volatile-highlight)))
   `(region ((,class ,region)))
   `(secondary-selection ((,class ,match))) ; used by Org-mode for highlighting matched entries and keywords
   `(isearch ((,class (:weight bold :foreground "#00AA00" :background "#99FF99"))))
   `(isearch-fail ((,class (:weight bold :foreground "black" :background "#FF9999"))))
   `(lazy-highlight ((,class (:weight bold :foreground "#990099" :background "#FF66FF"))))
   `(phi-search-match-face ((,class (:weight bold :foreground "#990099" :background "#FF66FF"))))
   `(phi-search-selection-face ((,class (:weight bold :foreground "#00AA00" :background "#99FF99"))))
   `(trailing-whitespace ((,class (:background "#F6EBFE"))))
   `(whitespace-hspace ((,class (:foreground "#D2D2D2"))))
   `(whitespace-indentation ((,class (:foreground "firebrick" :background "yellow"))))
   `(whitespace-line ((,class (:foreground "#CC0000" :background "#FFFF88"))))
   `(whitespace-space ((,class (:foreground "#b0b0b0"))))
   `(whitespace-tab ((,class (:foreground "#b0b0b0"))))
   `(whitespace-trailing ((,class (:weight bold :foreground "yellow" :background "red1"))))

   ;; Mode line faces
   `(mode-line ((,class (:box (:line-width 1 :color "#1A2F54") :foreground "#85CEEB" :background "#335EA8"))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color "#4E4E4C") :foreground "#F0F0EF" :background "#9B9C97"))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground "white"))))
   `(mode-line-emphasis ((,class (:weight bold :foreground "white"))))
   `(mode-line-highlight ((,class (:foreground "yellow"))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground "black" :background "gold"))))
   `(minibuffer-noticeable-prompt ((,class (:weight bold :foreground "black" :background "gold"))))
   `(escape-glyph ((,class (:foreground "#008ED1"))))
   `(error ((,class (:foreground "red"))))
   `(warning ((,class (:weight bold :foreground "orange"))))
   `(success ((,class (:foreground "green"))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#006FE0"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#8D8D84")))) ; #696969
   `(font-lock-comment-face ((,class (:slant italic :foreground "#555555" :background "#f7f7f7"))))
   `(font-lock-constant-face ((,class (:foreground "#D0372D"))))
   `(font-lock-doc-face ((,class (:foreground "#555555" :background "#f7f7f7"))))
   `(font-lock-doc-string-face ((,class (:foreground "#008000"))))
   `(font-lock-function-name-face ((,class (:weight normal :foreground "#006699"))))
   `(font-lock-keyword-face ((,class (:bold nil :foreground "#0000FF")))) ; #3654DC
   `(font-lock-preprocessor-face ((,class (:foreground "#808080"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold :inherit nil))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold :inherit nil))))
   `(font-lock-string-face ((,class ,string)))
   `(font-lock-type-face ((,class (:weight normal :foreground "#6434A3"))))
   `(font-lock-variable-name-face ((,class (:weight normal :foreground "#BA36A5")))) ; #800080
   `(font-lock-warning-face ((,class (:weight bold :foreground "red"))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground "#006FE0"))))
   `(link-visited ((,class (:underline t :foreground "#E5786D"))))
   `(button ((,class (:underline t :foreground "#006DAF"))))
   `(header-line ((,class (:weight bold :underline "black" :overline "black" :foreground "black" :background "#FFFF88"))))

   ;; Message faces
   `(message-header-name ((,class ,mail-header-name)))
   `(message-header-cc ((,class (:foreground "blue"))))
   `(message-header-other ((,class (:foreground "#3399CC"))))
   `(message-header-subject ((,class ,subject)))
   `(message-header-to ((,class (:foreground "blue"))))
   `(message-cited-text ((,class (:foreground "#5050B0"))))
   `(message-separator ((,class (:weight bold :foreground "red"))))
   `(message-header-newsgroups ((,class (:foreground "#3399CC"))))
   `(message-header-xheader ((,class (:foreground "#3399CC"))))
   `(message-mml ((,class (:foreground "forest green"))))

   ;; Diff
   `(diff-added ((,class ,diff-added)))
   `(diff-changed ((,class (:foreground "blue" :background "#DDDDFF"))))
   `(diff-context ((,class ,diff-none)))
   `(diff-file-header ((,class (:foreground "#0000CC" :background "#EAF2F5"))))
   `(diff-file1-hunk-header ((,class (:foreground "dark magenta" :background "#EAF2F5"))))
   `(diff-file2-hunk-header ((,class (:foreground "#2B7E2A" :background "#EAF2F5"))))
   `(diff-header ((,class (:foreground "#999999" :background "#EAF2F5"))))
   `(diff-hunk-header ((,class ,diff-hunk-header)))
   `(diff-index ((,class (:foreground "#4183C4" :background "#EAF2F5")))) ; my foreground is NOT applied!?
   `(diff-indicator-added ((,class (:foreground "#008000" :background "#AAFFAA"))))
   `(diff-indicator-changed ((,class (:foreground "black" :background "#AAAAFF"))))
   `(diff-indicator-removed ((,class (:foreground "#A60000" :background "#FFAAAA"))))
   `(diff-refine-change ((,class (:background "#DDDDFF"))))
   `(diff-removed ((,class ,diff-removed)))

   ;; SMerge
   `(smerge-refined-change ((,class (:background "#AAAAFF"))))

   ;; Ediff
   `(ediff-current-diff-A ((,class (:foreground "gray33" :background "#FFDDDD"))))
   `(ediff-current-diff-B ((,class (:foreground "gray33" :background "#DDFFDD"))))
   `(ediff-current-diff-C ((,class (:foreground "black" :background "cyan"))))
   `(ediff-even-diff-A ((,class (:foreground "black" :background "light grey"))))
   `(ediff-even-diff-B ((,class (:foreground "black" :background "light grey"))))
   `(ediff-fine-diff-A ((,class (:foreground "#A60000" :background "#FFAAAA"))))
   `(ediff-fine-diff-B ((,class (:foreground "#008000" :background "#55FF55"))))
   `(ediff-odd-diff-A ((,class (:foreground "black" :background "light grey"))))
   `(ediff-odd-diff-B ((,class (:foreground "black" :background "light grey"))))

   ;; Flyspell
   `(flyspell-duplicate ((,class (:underline "#008000" :inherit nil))))
   `(flyspell-incorrect ((,class (:underline "red" :inherit nil))))

   `(Info-title-1-face ((,class ,ol1)))
   `(Info-title-2-face ((,class ,ol2)))
   `(Info-title-3-face ((,class ,ol3)))
   `(Info-title-4-face ((,class ,ol4)))
   `(ace-jump-face-foreground ((,class (:foreground "black" :background "#FBE448"))))
   `(bbdb-company ((,class (:slant italic :foreground "steel blue"))))
   `(bbdb-field-name ((,class (:weight bold :foreground "steel blue"))))
   `(bbdb-field-value ((,class (:foreground "steel blue"))))
   `(bbdb-name ((,class (:underline t :foreground "#FF6633"))))
   `(browse-kill-ring-separator-face ((,class (:weight bold :foreground "slate gray"))))
   `(calendar-today ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFABE"))))
   `(cfw:face-annotation ((,class (:foreground "green" :background "red"))))
   `(cfw:face-day-title ((,class (:foreground "#C9C9C9"))))
   `(cfw:face-default-content ((,class (:foreground "#2952A3"))))
   `(cfw:face-default-day ((,class (:weight bold))))
   `(cfw:face-disable ((,class (:foreground "DarkGray"))))
   `(cfw:face-grid ((,class (:foreground "#DDDDDD"))))
   `(cfw:face-header ((,class (:foreground "#1662AF" :background "white" :weight bold))))
   `(cfw:face-holiday ((,class (:foreground "#777777" :background "#E4EBFE"))))
   `(cfw:face-periods ((,class (:foreground "white" :background "#668CD9" :slant italic))))
   `(cfw:face-saturday ((,class (:foreground "#4E4E4E" :background "white" :weight bold))))
   `(cfw:face-select ((,class (:foreground "#4A95EB" :background "#EDF1FA"))))
   `(cfw:face-sunday ((,class (:foreground "#4E4E4E" :background "white" :weight bold))))
   `(cfw:face-title ((,class (:foreground "#676767" :weight bold :inherit variable-pitch))))
   `(cfw:face-today ((,class (:foreground "#4F4A3D" :background "#FFFABE"))))
   `(cfw:face-today-title ((,class (:foreground "#4A95EB" :background "#FFFABE"))))
   `(cfw:face-toolbar ((,class (:background "white"))))
   `(cfw:face-toolbar-button-off ((,class (:foreground "#CFCFCF" :background "white"))))
   `(cfw:face-toolbar-button-on ((,class (:foreground "#5E5E5E" :background "#F6F6F6"))))
   `(change-log-date-face ((,class (:foreground "purple"))))
   `(change-log-file ((,class (:weight bold :foreground "#4183C4"))))
   `(circe-highlight-all-nicks-face ((,class (:foreground "blue" :background "#F0F0F0")))) ; other nick names
   `(circe-highlight-nick-face ((,class (:foreground "#009300" :background "#F0F0F0")))) ; messages with my nick cited
   `(circe-my-message-face ((,class (:foreground "#8B8B8B" :background "#F0F0F0"))))
   `(circe-originator-face ((,class (:foreground "blue"))))
   `(circe-prompt-face ((,class (:foreground "red"))))
   `(circe-server-face ((,class (:foreground "#99CAE5"))))
   `(comint-highlight-input ((,class (:weight bold :foreground "#0000FF"))))
   `(comint-highlight-prompt ((,class (:weight bold :foreground "black" :background "gold"))))
   `(compare-windows ((,class (:background "#FFFF00"))))
   `(compilation-error ((,class (:weight bold :foreground "red"))))
   `(compilation-info ((,class (:weight bold :foreground "#2A489E")))) ; used for grep
   `(compilation-line-number ((,class (:weight bold :foreground "#A535AE"))))
   `(compilation-warning ((,class (:weight bold :foreground "orange"))))
   `(css-property ((,class (:foreground "#00AA00"))))
   `(css-selector ((,class (:weight bold :foreground "blue"))))
   `(custom-button ((,class (:box (:line-width 2 :style released-button) :foreground "black" :background "lightgrey"))))
   `(custom-button-mouse ((,class (:box (:line-width 2 :style released-button) :foreground "black" :background "grey90"))))
   `(custom-button-pressed ((,class (:box (:line-width 2 :style pressed-button) :foreground "black" :background "light grey"))))
   `(custom-button-pressed-unraised ((,class (:underline t :foreground "magenta4"))))
   `(custom-button-unraised ((,class (:underline t))))
   `(custom-changed ((,class (:foreground "white" :background "blue"))))
   `(custom-comment ((,class (:background "gray85"))))
   `(custom-comment-tag ((,class (:foreground "blue4"))))
   `(custom-documentation ((,class (nil))))
   `(custom-face-tag ((,class (:weight bold))))
   `(custom-group-tag ((,class (:weight bold :foreground "blue1"))))
   `(custom-group-tag-1 ((,class (:weight bold :foreground "red1"))))
   `(custom-invalid ((,class (:foreground "yellow" :background "red"))))
   `(custom-link ((,class (:underline t :foreground "blue1"))))
   `(custom-modified ((,class (:foreground "white" :background "blue"))))
   `(custom-rogue ((,class (:foreground "pink" :background "black"))))
   `(custom-saved ((,class (:underline t))))
   `(custom-set ((,class (:foreground "blue" :background "white"))))
   `(custom-state ((,class (:foreground "green4"))))
   `(custom-themed ((,class (:foreground "white" :background "blue1"))))
   `(custom-variable-button ((,class (:weight bold :underline t))))
   `(custom-variable-tag ((,class (:weight bold :foreground "blue1"))))
   `(custom-visibility ((,class ,link)))
   `(diary-face ((,class (:foreground "#87C9FC"))))
   `(dircolors-face-asm ((,class (:foreground "black"))))
   `(dircolors-face-backup ((,class (:foreground "black"))))
   `(dircolors-face-compress ((,class (:foreground "red"))))
   `(dircolors-face-dir ((,class ,directory)))
   `(dircolors-face-doc ((,class (:foreground "black"))))
   `(dircolors-face-dos ((,class (:foreground "green3"))))
   `(dircolors-face-emacs ((,class (:foreground "black"))))
   `(dircolors-face-exec ((,class (:foreground "green3"))))
   `(dircolors-face-html ((,class (:foreground "black"))))
   `(dircolors-face-img ((,class (:foreground "black"))))
   `(dircolors-face-lang ((,class (:foreground "black"))))
   `(dircolors-face-lang-interface ((,class (:foreground "black"))))
   `(dircolors-face-make ((,class (:foreground "black"))))
   `(dircolors-face-objet ((,class (:foreground "black"))))
   `(dircolors-face-package ((,class (:foreground "red"))))
   `(dircolors-face-paddb ((,class (:foreground "black"))))
   `(dircolors-face-ps ((,class (:foreground "black"))))
   `(dircolors-face-sound ((,class (:foreground "black"))))
   `(dircolors-face-tar ((,class (:foreground "red"))))
   `(dircolors-face-text ((,class (:foreground "black"))))
   `(dircolors-face-yacc ((,class (:foreground "black"))))
   `(dired-directory ((,class ,directory)))
   `(dired-header ((,class ,directory)))
   `(dired-ignored ((,class (:strike-through t :foreground "red"))))
   `(dired-mark ((,class ,marked-line)))
   `(dired-marked ((,class ,marked-line)))
   `(dired-symlink ((,class ,symlink)))
   `(diredp-compressed-file-suffix ((,class (:foreground "red"))))
   `(diredp-date-time ((,class (:foreground "purple"))))
   `(diredp-dir-heading ((,class ,directory)))
   `(diredp-dir-priv ((,class ,directory)))
   `(diredp-exec-priv ((,class (:background "#03C03C"))))
   `(diredp-executable-tag ((,class (:foreground "green3" :background "white"))))
   `(diredp-file-name ((,class (:foreground "black"))))
   `(diredp-file-suffix ((,class (:foreground "#008000"))))
   `(diredp-flag-mark-line ((,class ,marked-line)))
   `(diredp-ignored-file-name ((,class ,shadow)))
   `(diredp-read-priv ((,class (:background "#0A99FF"))))
   `(diredp-write-priv ((,class (:foreground "white" :background "#FF4040"))))
   `(file-name-shadow ((,class ,shadow)))
   `(font-latex-bold-face ((,class (:weight bold :foreground "black"))))
   `(font-latex-italic-face ((,class (:slant italic :foreground "#1A1A1A"))))
   `(font-latex-math-face ((,class (:foreground "blue"))))
   `(font-latex-sectioning-1-face ((,class (:weight bold :foreground "cornflower blue"))))
   `(font-latex-sectioning-2-face ((,class ,ol1)))
   `(font-latex-sectioning-3-face ((,class ,ol2)))
   `(font-latex-sectioning-4-face ((,class ,ol3)))
   `(font-latex-sectioning-5-face ((,class ,ol4)))
   `(font-latex-sedate-face ((,class (:foreground "#FF5500"))))
   `(font-latex-string-face ((,class (:weight bold :foreground "#0066FF"))))
   `(font-latex-verbatim-face ((,class (:foreground "#000088" :background "#FFFFE0" :inherit nil))))
   `(git-commit-summary-face ((,class (:foreground "#000000"))))
   `(git-commit-comment-face ((,class (:slant italic :foreground "#696969"))))
   `(helm-action ((,class (:foreground "black"))))
   `(helm-bookmarks-su-face ((,class (:foreground "red"))))
   `(helm-buffer-process ((,class (:foreground "#008200"))))
   `(helm-candidate-number ((,class (:foreground "black" :background "#FFFF66"))))
   `(helm-dir-heading ((,class (:foreground "blue" :background "pink"))))
   `(helm-dir-priv ((,class (:foreground "dark red" :background "light grey"))))
   `(helm-ff-directory ((,class ,directory)))
   `(helm-ff-executable ((,class (:foreground "green3" :background "white"))))
   `(helm-ff-file ((,class (:foreground "black"))))
   `(helm-ff-invalid-symlink ((,class (:foreground "yellow" :background "red"))))
   `(helm-ff-symlink ((,class ,symlink)))
   `(helm-file-name ((,class (:foreground "blue"))))
   `(helm-gentoo-match-face ((,class (:foreground "red"))))
   `(helm-grep-running ((,class (:weight bold :foreground "white"))))
   `(helm-grep-lineno ((,class ,shadow)))
   `(helm-isearch-match ((,class (:background "#CCFFCC"))))
   `(helm-match ((,class ,match)))
   `(helm-moccur-buffer ((,class (:foreground "#0066CC"))))
   `(helm-selection ((,class ,highlight-line)))
   `(helm-selection-line ((,class ,volatile-highlight)))
   `(helm-source-header ((,class (:weight bold :foreground "white" :background "#2F69BF"))))
   `(helm-visible-mark ((,class ,marked-line)))
   `(helm-w3m-bookmarks-face ((,class (:underline t :foreground "cyan1"))))
   `(highlight-symbol-face ((,class (:background "#FFFFA0"))))
   `(hl-line ((,class ,highlight-line)))
   `(hl-tags-face ((,class (:background "#FEFCAE"))))
   `(holiday-face ((,class (:foreground "#777777" :background "#E4EBFE"))))
   `(html-helper-bold-face ((,class (:weight bold :foreground "black"))))
   `(html-helper-italic-face ((,class (:slant italic :foreground "black"))))
   `(html-helper-underline-face ((,class (:underline t :foreground "black"))))
   `(html-tag-face ((,class (:foreground "blue"))))
   `(ilog-non-change-face ((,class (:foreground "#6434A3"))))
   `(ilog-change-face ((,class (:foreground "#008200"))))
   `(ilog-echo-face ((,class (:foreground "#006FE0"))))
   `(ilog-load-face ((,class (:foreground "#BA36A5"))))
   `(ilog-message-face ((,class (:foreground "#808080"))))
   `(info-file ((,class (:weight bold :box (:line-width 1 :color "#0000CC") :foreground "cornflower blue" :background "LightSteelBlue1"))))
   `(info-header-node ((,class (:underline t :foreground "orange")))) ; nodes in header
   `(info-header-xref ((,class (:underline t :foreground "dodger blue")))) ; cross references in header
   `(info-menu-header ((,class ,ol2))) ; menu titles (headers) -- major topics
   `(info-menu-star ((,class (:foreground "black")))) ; every 3rd menu item
   `(info-node ((,class (:underline t :foreground "blue")))) ; node names
   `(info-quoted-name ((,class ,code-inline)))
   `(info-string ((,class ,string)))
   `(info-title-1 ((,class ,ol1)))
   `(info-xref ((,class (:underline t :foreground "#006DAF")))) ; unvisited cross-references
   `(info-xref-visited ((,class (:underline t :foreground "magenta4")))) ; previously visited cross-references
   `(light-symbol-face ((,class (:background "#FFFFA0"))))
   `(linum ((,class (:foreground "#AFAFAF" :background "white"))))
   `(log-view-file ((,class (:foreground "#0000CC" :background "#EAF2F5"))))
   `(lui-button-face ((,class ,link)))
   `(lui-highlight-face ((,class (:box '(:line-width 1 :color "#CC0000") :foreground "#CC0000" :background "#FFFF88")))) ; my nickname
   `(lui-time-stamp-face ((,class (:foreground "purple"))))
   `(magit-branch ((,class ,vc-branch)))
   `(magit-diff-add ((,class ,diff-added)))
   `(magit-diff-del ((,class ,diff-removed)))
   `(magit-diff-file-header ((,class (:weight bold :foreground "#4183C4"))))
   `(magit-diff-hunk-header ((,class ,diff-hunk-header)))
   `(magit-diff-none ((,class ,diff-none)))
   `(magit-header ((,class (:foreground "white" :background "#FF4040"))))
   `(magit-item-highlight ((,class (:background "#EAF2F5"))))
   `(magit-item-mark ((,class ,marked-line)))
   `(magit-log-head-label ((,class (:box (:line-width 1 :color "blue" :style nil)))))
   `(magit-log-tag-label ((,class (:box (:line-width 1 :color "#00CC00" :style nil)))))
   `(magit-section-title ((,class (:weight bold :foreground "cornflower blue"))))
   `(makefile-space-face ((,class (:background "hot pink"))))
   `(makefile-targets ((,class (:weight bold :foreground "blue"))))
   `(match ((,class ,match)))
   `(mm-uu-extract ((,class ,code-block)))
   `(moccur-current-line-face ((,class (:foreground "black" :background "#FFFFCC"))))
   `(moccur-face ((,class (:foreground "black" :background "#FFFF99"))))
   `(next-error ((,class ,volatile-highlight)))
   `(nobreak-space ((,class (:background "#CCE8F6"))))
   `(nxml-attribute-local-name-face ((,class (:foreground "magenta"))))
   `(nxml-attribute-value-delimiter-face ((,class (:foreground "green4"))))
   `(nxml-attribute-value-face ((,class (:foreground "green4"))))
   `(nxml-comment-content-face ((,class (:slant italic :foreground "red"))))
   `(nxml-comment-delimiter-face ((,class (:foreground "red"))))
   `(nxml-element-local-name ((,class (:box (:line-width 1 :color "#999999") :foreground "#000088" :background "#DEDEDE"))))
   `(nxml-element-local-name-face ((,class (:foreground "blue"))))
   `(nxml-processing-instruction-target-face ((,class (:foreground "purple1"))))
   `(nxml-tag-delimiter-face ((,class (:foreground "blue"))))
   `(nxml-tag-slash-face ((,class (:foreground "blue"))))
   `(org-agenda-block-count ((,class (:weight bold :foreground "blue"))))
   `(org-agenda-calendar-event ((,class (:weight bold :foreground "#3774CC" :background "#A8C5EF"))))
   `(org-agenda-calendar-sexp ((,class (:foreground "#777777" :background "#E4EBFE"))))
   `(org-agenda-clocking ((,class ,clock-line)))
   `(org-agenda-column-dateline ((,class ,column)))
   `(org-agenda-current-time ((,class (:underline t :foreground "#1662AF"))))
   `(org-agenda-date ((,class (:weight bold :foreground "#1662AF"))))
   `(org-agenda-date-today ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFABE"))))
   `(org-agenda-date-weekend ((,class (:weight bold :foreground "#4E4E4E"))))
   `(org-agenda-diary ((,class (:weight bold :foreground "green4" :background "light blue"))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground "gold2"))))
   `(org-agenda-done ((,class (:foreground "#555555" :background "#EEEEEE"))))
   `(org-agenda-filter-category ((,class (:weight bold :foreground "orange"))))
   `(org-agenda-filter-tags ((,class (:weight bold :foreground "orange"))))
   `(org-agenda-restriction-lock ((,class (:background "#E77D63"))))
   `(org-agenda-structure ((,class (:bold nil :foreground "#999999"))))
   `(org-archived ((,class (:foreground "gray70"))))
   `(org-beamer-tag ((,class (:box (:line-width 1 :color "#FABC18") :foreground "#2C2C2C" :background "#FFF8D0"))))
   `(org-block ((,class ,code-block)))
   `(org-block-background ((,class (:background "#FFFFE0"))))
   `(org-block-begin-line ((,class (:underline "#A7A6AA" :foreground "#555555" :background "#E2E1D5"))))
   `(org-block-end-line ((,class (:overline "#A7A6AA" :foreground "#555555" :background "#E2E1D5"))))
   `(org-checkbox ((,class (:weight bold :box (:line-width 1 :style pressed-button) :foreground "white" :background "#777777"))))
   `(org-clock-overlay ((,class (:foreground "white" :background "SkyBlue4"))))
   `(org-code ((,class ,code-inline)))
   `(org-column ((,class ,column)))
   `(org-column-title ((,class ,column)))
   `(org-date ((,class (:underline t :foreground "#00459E"))))
   `(org-default ((,class (:foreground "#333333"))))
   `(org-dim ((,class (:foreground "#AAAAAA"))))
   `(org-document-info ((,class (:foreground "#484848"))))
   `(org-document-info-keyword ((,class (:foreground "#008ED1" :background "#EAEAFF"))))
   `(org-document-title ((,class (:weight bold :foreground "black"))))
   `(org-done ((,class (:weight bold :box (:line-width 1 :color "#BBBBBB") :foreground "#BBBBBB" :background "#F0F0F0"))))
   `(org-drawer ((,class (:foreground "light sky blue"))))
   `(org-ellipsis ((,class (:underline nil :box (:line-width 1 :color "#999999") :foreground "#999999" :background "#FFF8C0")))) ; #FFEE62
   `(org-example ((,class (:foreground "blue" :background "#EAFFEA"))))
   `(org-footnote ((,class (:underline t :foreground "#008ED1"))))
   `(org-formula ((,class (:foreground "chocolate1"))))
   `(org-headline-done ((,class (:weight normal :foreground "#999999"))))
   `(org-hide ((,class (:foreground "#E2E2E2"))))
   `(org-inlinetask ((,class (:box (:line-width 1 :color "#EBEBEB") :foreground "#777777" :background "#FFFFD6"))))
   `(org-latex-and-related ((,class ,code-block)))
   `(org-level-1 ((,class ,ol1)))
   `(org-level-2 ((,class ,ol2)))
   `(org-level-3 ((,class ,ol3)))
   `(org-level-4 ((,class ,ol4)))
   `(org-level-5 ((,class ,ol5)))
   `(org-level-6 ((,class ,ol6)))
   `(org-level-7 ((,class ,ol7)))
   `(org-level-8 ((,class ,ol8)))
   `(org-link ((,class ,link)))
   `(org-list-dt ((,class (:weight bold :foreground "#335EA8"))))
   `(org-meta-line ((,class (:slant normal :foreground "#008ED1" :background "#EAEAFF"))))
   `(org-mode-line-clock ((,class ,clock-line)))
   `(org-mode-line-clock-overrun ((,class (:weight bold :box (:line-width 1 :color "#335EA8") :foreground "white" :background "#FF4040"))))
   `(org-number-of-items ((,class (:weight bold :foreground "white" :background "#79BA79"))))
   `(org-property-value ((,class (:foreground "#00A000"))))
   `(org-quote ((,class (:slant italic :foreground "dim gray" :background "#FFFFE0"))))
   `(org-scheduled ((,class (:foreground "#333333"))))
   `(org-scheduled-previously ((,class (:weight bold :foreground "#373737"))))
   `(org-scheduled-today ((,class (:foreground "#4F4A3D" :background "#FFFABE"))))
   `(org-sexp-date ((,class (:foreground "#3774CC"))))
   `(org-special-keyword ((,class (:weight bold :foreground "#00BB00" :background "#EAFFEA"))))
   `(org-table ((,class (:foreground "dark green" :background "#EAFFEA"))))
   `(org-tag ((,class (:weight normal :slant italic :foreground "#9A9FA4" :background "#F3F3F3"))))
   `(org-target ((,class ,link)))
   `(org-time-grid ((,class (:foreground "#CFCFCF"))))
   `(org-todo ((,class (:weight bold :box (:line-width 1 :color "#D8ABA7") :foreground "#000000" :background "#FFBBBB"))))
   `(org-upcoming-deadline ((,class (:foreground "#FF5555"))))
   `(org-verbatim ((,class (:foreground "#0066CC"))))
   `(org-verse ((,class (:slant italic :foreground "dim gray" :background "#EEEEEE"))))
   `(org-warning ((,class (:weight bold :foreground "black" :background "#CCE7FF"))))
   `(outline-1 ((,class ,ol1)))
   `(outline-2 ((,class ,ol2)))
   `(outline-3 ((,class ,ol3)))
   `(outline-4 ((,class ,ol4)))
   `(outline-5 ((,class ,ol5)))
   `(outline-6 ((,class ,ol6)))
   `(outline-7 ((,class ,ol7)))
   `(outline-8 ((,class ,ol8)))
   `(pabbrev-debug-display-label-face ((,class (:background "chartreuse"))))
   `(pabbrev-suggestions-face ((,class (:weight bold :foreground "white" :background "red"))))
   `(pabbrev-suggestions-label-face ((,class (:weight bold :foreground "white" :background "purple"))))
   `(paren-face-match ((,class (:foreground "#E2464C" :background "#FFE182"))))
   `(paren-face-mismatch ((,class (:weight bold :foreground "white" :background "#FF3F3F"))))
   `(paren-face-no-match ((,class (:weight bold :foreground "white" :background "#FF3F3F"))))
   `(racket-selfeval-face ((,class (:foreground "#D0372D"))))
   `(recover-this-file ((,class (:weight bold :background "#FF3F3F"))))
   `(rng-error ((,class (:weight bold :foreground "red" :background "#FBE3E4"))))
   `(sh-heredoc ((,class (:foreground "blue" :background "#EEF5FE"))))
   `(sh-quoted-exec ((,class (:foreground "#FF1493"))))
   `(shadow ((,class ,shadow)))
   `(shell-option-face ((,class (:foreground "forest green"))))
   `(shell-output-2-face ((,class (:foreground "blue"))))
   `(shell-output-3-face ((,class (:foreground "purple"))))
   `(shell-output-face ((,class (:foreground "black"))))
   `(shell-prompt-face ((,class (:weight bold :foreground "yellow"))))
   `(show-paren-match ((,class (:foreground "#E2464C" :background "#FFFF00"))))
   `(show-paren-mismatch ((,class (:weight bold :foreground "white" :background "#FF3F3F"))))
   `(sml-modeline-end-face ((,class (:background "#6BADF6")))) ; #335EA8
   `(sml-modeline-vis-face ((,class (:background "#1979CA"))))
   `(speedbar-button-face ((,class (:foreground "green4"))))
   `(speedbar-directory-face ((,class (:foreground "blue4"))))
   `(speedbar-file-face ((,class (:foreground "cyan4"))))
   `(speedbar-highlight-face ((,class ,volatile-highlight)))
   `(speedbar-selected-face ((,class (:underline t :foreground "red"))))
   `(speedbar-tag-face ((,class (:foreground "brown"))))
   `(svn-status-directory-face ((,class ,directory)))
   `(svn-status-filename-face ((,class (:weight bold :foreground "#4183C4"))))
   `(svn-status-locked-face ((,class (:weight bold :foreground "red"))))
   `(svn-status-marked-face ((,class ,marked-line)))
   `(svn-status-marked-popup-face ((,class (:weight bold :foreground "green3"))))
   `(svn-status-switched-face ((,class (:slant italic :foreground "gray55"))))
   `(svn-status-symlink-face ((,class ,symlink)))
   `(svn-status-update-available-face ((,class (:foreground "orange"))))
   `(tex-verbatim ((,class (:foreground "blue"))))
   `(tool-bar ((,class (:box (:line-width 1 :style released-button) :foreground "black" :background "gray75"))))
   `(tooltip ((,class (:foreground "black" :background "light yellow"))))
   `(trailing-whitespace ((,class (:background "#F6EBFE"))))
   `(traverse-match-face ((,class (:weight bold :foreground "blue violet"))))
   `(vc-annotate-face-3F3FFF ((,class (:foreground "#3F3FFF" :background "black"))))
   `(vc-annotate-face-3F6CFF ((,class (:foreground "#3F3FFF" :background "black"))))
   `(vc-annotate-face-3F99FF ((,class (:foreground "#3F99FF" :background "black"))))
   `(vc-annotate-face-3FC6FF ((,class (:foreground "#3F99FF" :background "black"))))
   `(vc-annotate-face-3FF3FF ((,class (:foreground "#3FF3FF" :background "black"))))
   `(vc-annotate-face-3FFF56 ((,class (:foreground "#4BFF4B" :background "black"))))
   `(vc-annotate-face-3FFF83 ((,class (:foreground "#3FFFB0" :background "black"))))
   `(vc-annotate-face-3FFFB0 ((,class (:foreground "#3FFFB0" :background "black"))))
   `(vc-annotate-face-3FFFDD ((,class (:foreground "#3FF3FF" :background "black"))))
   `(vc-annotate-face-56FF3F ((,class (:foreground "#4BFF4B" :background "black"))))
   `(vc-annotate-face-83FF3F ((,class (:foreground "#B0FF3F" :background "black"))))
   `(vc-annotate-face-B0FF3F ((,class (:foreground "#B0FF3F" :background "black"))))
   `(vc-annotate-face-DDFF3F ((,class (:foreground "#FFF33F" :background "black"))))
   `(vc-annotate-face-FF3F3F ((,class (:foreground "#FF3F3F" :background "black"))))
   `(vc-annotate-face-FF6C3F ((,class (:foreground "#FF3F3F" :background "black"))))
   `(vc-annotate-face-FF993F ((,class (:foreground "#FF993F" :background "black"))))
   `(vc-annotate-face-FFC63F ((,class (:foreground "#FF993F" :background "black"))))
   `(vc-annotate-face-FFF33F ((,class (:foreground "#FFF33F" :background "black"))))
   `(which-func ((,class (:weight bold :foreground "white"))))
   `(widget-button ((,class ,link)))
   `(widget-button-pressed ((,class (:foreground "red"))))
   `(widget-documentation ((,class (:foreground "green4"))))
   `(widget-field ((,class (:background "gray85"))))
   `(widget-inactive ((,class (:foreground "dim gray"))))
   `(widget-single-line-field ((,class (:background "gray85"))))
   `(yas/field-debug-face ((,class (:background "ivory2"))))
   `(yas/field-highlight-face ((,class (:background "DarkSeaGreen1"))))
   )

  (custom-theme-set-variables
   'leuven
   '(ansi-color-names-vector ["#242424" "#E5786D" "#95E454" "#CAE682"
                              "#8AC6F2" "#333366" "#CCAA8F" "#F6F3E8"]))
  )

;;;###autoload
(provide-theme 'leuven)

;;; leuven-theme.el ends here
