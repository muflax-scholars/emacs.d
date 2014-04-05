Minor mode for org-mode to sync org-mode and trello

1) Add the following to your emacs init file
(require 'org-trello)

Automatically
2) Once - Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards (C-c o i)
M-x org-trello/install-key-and-token

3) Once per org-mode file/board you want to connect to (C-c o I)
M-x org-trello/install-board-and-lists-ids

*Beware* you must setup your trello board with the name you use as keywords (TODO, DONE e.g) on your org-mode file.

4) You can also create a board directly from a org-mode buffer (C-c o b)
M-x org-trello/create-board
