;;; whole-line-or-region.el --- operate on current line if region undefined

;; This file is not part of Emacs

;; Copyright (C) 2001 by Joseph L. Casadonte Jr.
;; Author:          Joe Casadonte <emacs@northbound-train.com>
;; Maintainer:      Steve Purcell <steve@sanityinc.com>
;; Created:         July 1, 2001
;; Keywords:        kill yank cut copy paste whole lines
;; Version: 20110901.930
;; X-Original-Version:         1.3.1
;; Latest Version:  https://github.com/purcell/whole-line-or-region

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
0;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  This minor mode allows functions to operate on the current line if
;;  they would normally operate on a region and region is currently
;;  undefined.
;;
;;  The primary use for this is to kill (cut) the current line if no
;;  region is defined, and kill-region is invoked.  It basically saves
;;  you the effort of going to the begining of the line, selecting the
;;  text up to the end of the line, and killing.  Similarly, when
;;  yanking, it's smart enough to know that the string to be yanked
;;  was killed as a whole line, and it should be yanked as one, too.
;;  So you don't need to position yourself at the start of the line
;;  before yanking.  If region *is* defined, though, all functions act
;;  as normal.
;;
;;  The inspiration for this came from an old editor I used to use
;;  (brief maybe?), that did this exact thing for you.  It was a handy
;;  feature to have, and I definitely wanted it when I moved to Emacs.
;;  I've extended the concept slightly, to let you copy N whole lines,
;;  using the standard prefix method.
;;
;;  NOTE: This package will behave unexpectedly (and indeed is nearly
;;  useless) if `transient-mark-mode' is off, as there is then always
;;  a region defined.
;;
;;  NOTE: I haven't gotten this to work under XEmacs (though I
;;  honestly haven't tried real hard).

;;; Usage:
;;
;;  M-x `whole-line-or-region-mode'

;;  Toggles whole-line-or-region-mode on & off.  Optional arg turns
;;  whole-line-or-region-mode on iff arg is a positive integer.  Then
;;  just call functions `copy-region-as-kill', `kill-region',
;;  `kill-ring-save' and `yank' as you normally would.
;;
;;  To turn the mode on automatically whenever Emacs starts, customize
;;  `whole-line-or-region-mode' (which see).

;;; Extending Package:
;;
;;  I've tried to make the base functions as generic as possible so
;;  that this same concept can be used for other region-based
;;  functions.  The only function I've thought of to date to extend in
;;  this manner is `comment-dwim'.  Examples using `comment-dwim'
;;  follow.
;;
;;  In order to extend this package for additional region-based
;;  functions, you must understand how those functions work, and write
;;  a new stub function that will be used to replace it.  One of two
;;  whole-line-or-region functions must be called from within that
;;  stub; which one to use depends on whether or not the original
;;  function wants region passed into it, or assumes region is defined
;;  before being called.
;;
;;  Using `kill-region' as an example, looking at its definition we
;;  see that it takes two arguments, BEG and END.  Looking at it
;;  another way, it's interactive declaration is "r", which says to
;;  pass in the current region.  Because of this, the stub function
;;  for it should call `whole-line-or-region-call-with-region':
;;
;;     (defun whole-line-or-region-kill-region (prefix)
;;       "Kill region or PREFIX whole lines."
;;       (interactive "*p")
;;       (whole-line-or-region-call-with-region 'kill-region prefix t))
;;
;;  The first argument to `whole-line-or-region-call-with-region' is
;;  the function being replaced.  The second is the value for prefix,
;;  so that the stub can operate on more than just one line (e.g. C-u
;;  12 M-w would copy 12 whole lines).  Other arguments are explained
;;  in the function documentation.
;;
;;  The function `comment-dwim', on the other hand, expects region to
;;  be defined coming in, so its stub should call into the other
;;  whole-line stub, `whole-line-or-region-call-with-prefix'.  There are
;;  things to consider, though.  The original `comment-dwim' wants a
;;  raw prefix value, but it doesn't use it to work over a variable
;;  number of lines; rather it uses it to signal what DWIM really
;;  does.  Sort of defeats the purpose of a DWIM command, if you ask
;;  me -- it should be simple enough to determine from the current
;;  context what DWIMs should do.  I digress, however.....
;;
;;  The "proper" way to write a whole-line version of `comment-dwim'
;;  would be like the following:
;;
;;    (defun whole-line-or-region-comment-dwim (raw-prefix)
;;      "Call `comment-dwim' on current region or current line."
;;      (interactive "*P")
;;      (whole-line-or-region-call-with-prefix 'comment-dwim 1 nil t raw-prefix))
;;
;;  The arguments for `whole-line-or-region-call-with-prefix' are
;;  basically the same as for `whole-line-or-region-call-with-region',
;;  but how each of them call the original function differs.  The
;;  first one calls it with two arguments (i.e. region's BEG & END)
;;  and the second one sets mark (i.e. defines region) and passes in
;;  prefix (raw or processed, depending).
;;
;;  So the above example for `comment-dwim' would call the original
;;  function with the current region (if defined) or the current line
;;  (the second argument, the number of lines to operate on, being
;;  hard-coded to 1), also passing in the raw prefix, for use within
;;  the original function.  It retains its original semantics and just
;;  saves you from having to mark the current line.
;;
;;  It could instead be defined like so:
;;
;;    (defun whole-line-or-region-comment-dwim-2 (prefix)
;;      "Call `comment-dwim' on region or PREFIX whole lines."
;;      (interactive "*p")
;;      (whole-line-or-region-call-with-prefix 'comment-dwim prefix nil t))
;;
;;  What this version does is override the normal behavior of the
;;  prefix arg to `comment-dwim', and instead uses it to indicate how
;;  many lines the whole-line version will comment out -- no prefix
;;  value is passed to the original function in this case.  This is
;;  the version that I use, as it's just more intuitive for me.
;;
;;  After defining the new stub, however you do it, the package needs
;;  to know about it so that it can toggle its use on and off as the
;;  mode toggles on and off.  For that you need to customize the
;;  variable `whole-line-or-region-extensions-alist', telling it the
;;  original function name (`comment-dwim') and the new one
;;  (`whole-line-or-region-comment-dwim-2').  If you want to limit the
;;  redefinition to a specific keymap then specify that as well;
;;  otherwise, the rebinding will occur in the global keymap.
;;  Rebinding occurs via `substitute-key-definition' (which see).

;;; To Do:
;;
;;  o Nothing, at the moment.

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of whole-line-or-region was developed and tested with NTEmacs
;;  22.2.1 under Windows XP Pro.  Please, let me know if it works with
;;  other OS and versions of Emacs.

;;; Change Log:
;;
;;  see http://www.northbound-train.com/emacs/whole-line-or-region.log

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; Code:

(eval-when-compile
  ;; silence the old byte-compiler
  (defvar byte-compile-dynamic nil)
  (set (make-local-variable 'byte-compile-dynamic) t))

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst whole-line-or-region-version
  "$Revision: 1.3 $"
  "Version number for 'whole-line-or-region' package.")

;; ---------------------------------------------------------------------------
(defun whole-line-or-region-version-number ()
  "Return 'whole-line-or-region' version number."
  (string-match "[0123456789.]+" whole-line-or-region-version)
  (match-string 0 whole-line-or-region-version))

;; ---------------------------------------------------------------------------
(defun whole-line-or-region-display-version ()
  "Display 'whole-line-or-region' version."
  (interactive)
  (message "whole-line-or-region version <%s>." (whole-line-or-region-version-number)))

;;; **************************************************************************
;;; ***** customization
;;; **************************************************************************
(defgroup whole-line-or-region nil
  "Customization group for whole-line-or-region minor mode."
  :group 'editing-basics
  :group 'convenience)

;; ---------------------------------------------------------------------------
(defun whole-line-or-region-customize ()
  "Customization of the group 'whole-line-or-region'."
  (interactive)
  (customize-group "whole-line-or-region"))

;; ---------------------------------------------------------------------------
(defcustom whole-line-or-region-mode nil
  "Non-nil if whole-line-or-region minor mode is enabled.

Setting this variable directly does not take effect; use either
\\[customize] or the function `whole-line-or-region-mode'."
  :set (lambda (symbol value)
		 (whole-line-or-region-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'whole-line-or-region)

;; ---------------------------------------------------------------------------
(defcustom whole-line-or-region-extensions-alist '(
			   (copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
			   (kill-region whole-line-or-region-kill-region nil)
			   (kill-ring-save whole-line-or-region-kill-ring-save nil)
			   (yank whole-line-or-region-yank nil)
			   )
  "List of functions for whole-line-or-region to swap.

When whole-line-or-region is activated, all original functions will be
replaced with their whole-line counterparts in the global keymap,
unless the optional keymap is specified (in which case it will be
replace in that map only).  Similarly, when whole-line-or-region is
de-activated, the functions will be swapped back.

The default is to map the following:

  o `copy-region-as-kill'  ->  `whole-line-or-region-copy-region-as-kill'
  o `kill-region'          ->  `whole-line-or-region-kill-region'
  o `kill-ring-save'       ->  `whole-line-or-region-kill-ring-save'
  o `yank'                 ->  `whole-line-or-region-yank'

In addition, the following functions are provided by the package for
your convenience:

  o `whole-line-or-region-delete'
  o `whole-line-or-region-comment-dwim'
  o `whole-line-or-region-comment-dwim-2'

See the individual functions for more information on what they do and
suggested mappings."
  :type '(repeat
		  (list :tag "Function Mappings:"
				(function :tag "Original Function")
				(function :tag "Whole-line Version")
				(variable :tag "Keymap (optional)")
				))
  :group 'whole-line-or-region
  :set (lambda (symbol newval)
		 (set symbol newval)
		 (when whole-line-or-region-mode
		   (whole-line-or-region-bind-keys)))
  )

;; ---------------------------------------------------------------------------
(defcustom whole-line-or-region-mode-line-string " WLR"
  "String to display in mode-line when 'whole-line-or-region' is active.

Must start with a space.  Changes will take effect next time emacs is
started."
  :type 'string
  :group 'whole-line-or-region)

;; ---------------------------------------------------------------------------
(defcustom whole-line-or-region-load-hook nil
  "Hook to run when package is loaded."
  :type 'hook
  :group 'whole-line-or-region)

;; ---------------------------------------------------------------------------
(defcustom whole-line-or-region-on-hook nil
  "Hook called when 'whole-line-or-region' mode is turned on."
  :type 'hook
  :group 'whole-line-or-region)

;; ---------------------------------------------------------------------------
(defcustom whole-line-or-region-off-hook nil
  "Hook called when 'whole-line-or-region' mode is turned off."
  :type 'hook
  :group 'whole-line-or-region)

;;; **************************************************************************
;;; ***** minor mode functions
;;; **************************************************************************
; (defvar whole-line-or-region-mode nil )

;;; --------------------------------------------------------------------------
;;;###autoload
(defun whole-line-or-region-mode (&optional arg)
  "Toggle use of whole-line-or-region minor mode.

This minor mode allows functions to operate on the current line if
they would normally operate on a region and region is currently
undefined.

Optional ARG turns mode on iff ARG is a positive integer."
  (interactive "P")

  ;; toggle on and off
  (let ((old-mode whole-line-or-region-mode))
	(setq whole-line-or-region-mode
		  (if arg (or (listp arg)
					  (> (prefix-numeric-value arg) 0))
			(not whole-line-or-region-mode)))

	(when (not (equal old-mode whole-line-or-region-mode))
	  ;; enable/disable advice
	  (if whole-line-or-region-mode
		  (whole-line-or-region-bind-keys)
		(whole-line-or-region-restore-keys))

	  (run-hooks (if whole-line-or-region-mode
					 'whole-line-or-region-on-hook
				   'whole-line-or-region-off-hook))
	  )))

;; ---------------------------------------------------------------------------
;; add to minor-mode-alist if not there already
(or
 (assq 'whole-line-or-region-mode minor-mode-alist)
 (setq minor-mode-alist
	   (cons
		(list 'whole-line-or-region-mode whole-line-or-region-mode-line-string)
		minor-mode-alist)))

;;; **************************************************************************
;;; ***** interactive functions (used by default)
;;; **************************************************************************
;;;###autoload
(defun whole-line-or-region-copy-region-as-kill (prefix)
  "Copy region or PREFIX whole lines."
  (interactive "p")
  (whole-line-or-region-call-with-region 'copy-region-as-kill prefix t))

;;; --------------------------------------------------------------------------
;; (defalias 'whole-line-or-region-copy 'whole-line-or-region-copy-region-as-kill)

;;; **************************************************************************
;;;###autoload
(defun whole-line-or-region-kill-region (prefix)
  "Kill (cut) region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-call-with-region 'kill-region prefix t))

;;; **************************************************************************
;;;###autoload
(defun whole-line-or-region-kill-ring-save (prefix)
  "Copy region or PREFIX whole lines."
  (interactive "p")
  (whole-line-or-region-call-with-region 'kill-ring-save prefix t))

;;; **************************************************************************
;;;###autoload
(defun whole-line-or-region-yank (raw-prefix &optional string-in)
  "Yank (paste) previously killed text.

If the text to be yanked was killed with a whole-line-or-region
function *as* a whole-line, then paste it as a whole line (i.e. do not
break up the current line, and do not force the user to move point).

RAW-PREFIX is used to determine which string to yank, just as `yank'
would normally use it.

Optionally, pass in string to be \"yanked\" via STRING-IN."
  (interactive "*P")

  ;; figure out what yank would do normally
  (let ((string-to-yank (or string-in (current-kill
									   (cond ((listp raw-prefix) 0)
											 ((eq raw-prefix '-) -1)
											 (t (1- raw-prefix))) t)))
		(saved-column (current-column)))

	;; check for whole-line prop in yanked text
	(if (get-text-property 0 'whole-line-or-region string-to-yank)
		(let ((beg (line-beginning-position)))
		  ;; goto beg of line and yank
		  (beginning-of-line)
		  (if string-in
			  ;; insert "manually"
			  (insert string-in)
			;; just yank as normal
			(yank raw-prefix))

		  ;; a whole-line killed from end of file may not have a
		  ;; trailing newline -- add one, in these cases
		  (when (not (string-match "\n$" string-to-yank))
			(insert "\n")
			(previous-line 1))

		  ;; restore state of being....
		  (move-to-column saved-column)
		  (remove-text-properties beg (+ beg 1) '(whole-line-or-region nil)))

	  ;; no whole-line-or-region mark
	  (if string-in
		  ;; insert "manually"
		  (progn
			(when (and delete-selection-mode
					   mark-active)
			  (delete-active-region))
			(insert string-in))
	  ;; just yank as normal
	  (yank raw-prefix)))
	))

;;; --------------------------------------------------------------------------
;; in case delete-selection-mode (delsel.el) is being used
(if (string-match "Emacs 21" (emacs-version))
	(put 'whole-line-or-region-yank 'delete-selection 'yank)
  (put 'whole-line-or-region-yank 'delete-selection t))

;;; **************************************************************************
;;; alternate interactive functions
;;; **************************************************************************
;;;###autoload
(defun whole-line-or-region-delete (prefix)
  "Delete region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-call-with-region 'delete-region prefix))

;;; **************************************************************************
;;;###autoload
(defun whole-line-or-region-comment-dwim (raw-prefix)
  "Call `comment-dwim' on current region or current line.

See `comment-dwim' for details of RAW-PREFIX usage."
  (interactive "*P")
  (whole-line-or-region-call-with-prefix 'comment-dwim 1 nil t raw-prefix))

;;; **************************************************************************
;;;###autoload
(defun whole-line-or-region-comment-dwim-2 (prefix)
  "Call `comment-dwim' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-call-with-prefix 'comment-dwim prefix nil t))

;;; **************************************************************************
;;; ***** internal functions
;;; **************************************************************************
(defun whole-line-or-region-call-with-region (fn &optional cnt mark-as-whole send-prefix prefix)
  "Calls FN on region or CNT whole lines.

If region is defined simply call FN, passing in the start and end of
the current region.

If region is not currently defined, then define it temporarily as the
current line.  Additionally, if CNT is set, expand region to cover the
next CNT whole lines (or previous CNT whole lines, if CNT is
negative).  Before FN is called, mark the temporary region with a
special property if MARK-AS-WHOLE is non-nil (this is useful if the
text could be worked on with some future whole-line-or-region
function, and it makes sense to understand the context in which FN was
originally called, e.g. killing and yanking text; see
`whole-line-or-region-yank' for an example).

In either case, if SEND-PREFIX is non-nil, then PREFIX is passed into
FN as a third argument."
  (whole-line-or-region-base-call fn fn t nil nil cnt mark-as-whole send-prefix prefix))

;;; **************************************************************************
(defun whole-line-or-region-call-with-prefix (fn &optional cnt mark-as-whole send-prefix prefix)
  "Calls FN on region or CNT whole lines.

If region is defined simply call FN.

If region is not currently defined, then define it temporarily as the
current line.  Additionally, if CNT is set, expand region to cover the
next CNT whole lines (or previous CNT whole lines, if CNT is
negative).  Before FN is called, mark the temporary region with a
special property if MARK-AS-WHOLE is non-nil (this is useful if the
text could be worked on with some future whole-line-or-region
function, and it makes sense to understand the context in which FN was
originally called, e.g. killing and yanking text; see
`whole-line-or-region-yank' for an example).

In either case, if SEND-PREFIX is non-nil, then PREFIX is passed into
FN as the sole argument."
  (whole-line-or-region-base-call fn fn nil nil nil cnt mark-as-whole send-prefix prefix))

;;; **************************************************************************
(defun whole-line-or-region-base-call (norm-fn wlr-fn
									   &optional beg-end pre-args post-args
									   cnt mark-as-whole send-prefix prefix)
  "Calls FN on region or CNT whole lines.

If region is defined simply call NORM-FN.

If region is not currently defined, then define it temporarily as the
current line.  Additionally, if CNT is set, expand region to cover the
next CNT whole lines (or previous CNT whole lines, if CNT is
negative).  Before WLR-FN is called, mark the temporary region with a
special property if MARK-AS-WHOLE is non-nil (this is useful if the
text could be worked on with some future whole-line-or-region
function, and it makes sense to understand the context in which WLR-FN was
originally called, e.g. killing and yanking text; see
`whole-line-or-region-yank' for an example).

In either case, if BEG-END is non-nil, then pass into FN the start and
end of the current region.  PRE-ARGS and POST-ARGS are lists of
arguments to be passed into FN before \(PRE-ARGS) and/or after
\(POST-ARGS) the start and end of the current region (but only if
BEG-END is non-nil).  Finally, if SEND-PREFIX is non-nil, then PREFIX
is passed into FN before POST-ARGS."

  ;; region is defined, so just do what should normally be done
  (if (and mark-active
		   (/= (point) (mark)))
	  ;; just call it, but make sure to pass all of the arguments....
	  (let (args)
		(when pre-args
		  (whole-line-or-region-append-to-list 'args pre-args))

		(when beg-end
		  (whole-line-or-region-append-to-list 'args (point))
		  (whole-line-or-region-append-to-list 'args (mark)))

		(when send-prefix
		  (whole-line-or-region-append-to-list 'args (list prefix)))

		(when post-args
		  (whole-line-or-region-append-to-list 'args post-args))

		(apply 'funcall norm-fn args))

	;; no region defined, act on whole line
	(let ((saved-column (current-column))
		  (current-mod-state (buffer-modified-p))
		  beg end)
	  (save-excursion
		(setq beg (line-beginning-position))
		(set-mark beg)

		;; add whole-line property, sometimes
		(when mark-as-whole
		  (let ((inhibit-read-only t))
   		    (put-text-property beg (min (point-max) (+ beg 1)) 'whole-line-or-region t)
			(set-buffer-modified-p current-mod-state)))

		(setq end (line-beginning-position (+ (or cnt 1) 1)))
		(goto-char end)

		(let (args)
		  (when pre-args
			(whole-line-or-region-append-to-list 'args pre-args))

		  (when beg-end
			(whole-line-or-region-append-to-list 'args beg)
			(whole-line-or-region-append-to-list 'args end))

		  (when send-prefix
			(whole-line-or-region-append-to-list 'args (list prefix)))

		  (when post-args
			(whole-line-or-region-append-to-list 'args post-args))

		  (apply 'funcall wlr-fn args))

		;; remove whole-line property, sometimes
		(when mark-as-whole
		  (let ((inhibit-read-only t)
				(current-mod-state (buffer-modified-p)))
		    (remove-text-properties beg (min (point-max) (+ beg 1)) '(whole-line-or-region nil))
		    (set-buffer-modified-p current-mod-state)))
		)

	  (move-to-column saved-column))
	))

;;; **************************************************************************
(defun whole-line-or-region-bind-keys (&optional switch)
  "Bind keys according to `whole-line-or-region-extensions-alist'.

With optional SWITCH, restore keys instead."
  (let ((gmap (current-global-map))
		(ext-alist whole-line-or-region-extensions-alist)
		elem orig wlr map)
	(while ext-alist
	  (setq elem (car ext-alist))
	  (setq ext-alist (cdr ext-alist))

	  (setq orig (nth 0 elem))
	  (setq wlr (nth 1 elem))
	  (setq map (nth 2 elem))

	  (if switch
		  (substitute-key-definition wlr orig (or map gmap))
		(substitute-key-definition orig wlr (or map gmap)))
	  )))

;;; **************************************************************************
(defun whole-line-or-region-restore-keys ()
  "Restore keys according to `whole-line-or-region-extensions-alist'."
  (whole-line-or-region-bind-keys t))

;;; **************************************************************************
(defun whole-line-or-region-append-to-list (list-var element)
	"Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.

The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

If you want to use `whole-line-or-region-append-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `whole-line-or-region-append-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
	(set list-var
		 (append (symbol-value list-var) (if (listp element) element (list element)))
		 ))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'whole-line-or-region)
(run-hooks 'whole-line-or-region-load-hook)

;;; whole-line-or-region.el ends here
