This package adds more customizable highlighting for files in dired
listings.  The group `dired-faces' provides only nine faces and
isn't very fine-grained.

The definitions are added using a macro `dired-rainbow-define'.
You can display its documentation by calling

M-x describe-function RET dired-rainbow-define RET

Here are some example uses:

(defconst my-dired-media-files-extensions
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
  "Media files.")

(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
(dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)

boring regexp due to lack of imagination
(dired-rainbow-define log (:inherit default
                           :italic t) ".*\\.log")

See https://github.com/Fuco1/dired-hacks for the entire collection.
