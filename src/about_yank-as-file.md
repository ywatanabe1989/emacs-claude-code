<!-- ---
!-- Timestamp: 2025-07-01 03:49:28
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/about_yank-as-file.md
!-- --- -->


User request:
(defun --ecc-select-remote)
This is select remote host from the current ssh configs (~/.ssh/{config,conf.d/}

(defun ecc-yank-as-file)
This should first call --ecc-select-remote to determine target file
Handle remote case by using scp
This is useful when claude is working remotely but they are restricted to read the machine they are running (this is usual case)

## Why yank-as-file?
as yank long sentences will make the claude terminal dirty and raise problem of scrolling.

<!-- EOF -->