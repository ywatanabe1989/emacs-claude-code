;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; File: ecc-monitoring-tab.el

(defgroup ecc-monitoring nil
  "Claude Code monitoring dashboard settings."
  :group 'ecc
  :prefix "ecc-monitoring-")

(defcustom ecc-monitoring-status-file
  "~/proj/todo/scitex/status.org"
  "Path to the status org file for Claude monitoring."
  :type 'file
  :group 'ecc-monitoring)

(defcustom ecc-monitoring-repo-paths
  '(("cloud" . "~/proj/scitex-cloud")
    ("ui" . "~/proj/scitex-ui")
    ("dotfiles-skills" . "~/.dotfiles/src/.claude/to_claude/skills")
    ("dotfiles-hooks" . "~/.dotfiles/src/.claude/to_claude/hooks"))
  "Alist of (NAME . PATH) for repo-monitor directories."
  :type '(alist :key-type string :value-type directory)
  :group 'ecc-monitoring)

(defcustom ecc-monitoring-tab-name
  "Claude"
  "Name for the Claude monitoring tab."
  :type 'string
  :group 'ecc-monitoring)

(defun ecc--find-own-vterm ()
  "Find the vterm buffer running this Claude instance.
First try name match, then fall back to probe-based detection."
  (or
   ;; Fast path: match by name
   (car (seq-filter
         (lambda (name) (string-match-p "claude-instance" name))
         (mapcar #'buffer-name
                 (seq-filter (lambda (b)
                               (with-current-buffer b
                                 (derived-mode-p 'vterm-mode)))
                             (buffer-list)))))
   ;; Slow path: probe all vterms with unique marker
   ;; (called from Claude via MCP: ecc--probe-vterm-buffers)
   nil))

(defun ecc--probe-vterm-buffers (probe-string)
  "Search all vterm buffers for PROBE-STRING, return matching buffer name.
Usage from Claude: echo a UUID, then call this to find which vterm has it."
  (let ((vterm-bufs (seq-filter (lambda (b)
                                  (with-current-buffer b
                                    (derived-mode-p 'vterm-mode)))
                                (buffer-list))))
    (car (seq-filter
          (lambda (name)
            (with-current-buffer name
              (save-excursion
                (goto-char (point-max))
                (search-backward probe-string
				 (max (point-min) (- (point-max) 5000))
				 t))))
          (mapcar #'buffer-name vterm-bufs)))))

(defun ecc-create-monitoring-tab ()
  "Create Claude monitoring tab: status.org | repo-monitor | own vterm."
  (interactive)
  (require 'repo-monitor nil t)
  (let ((tab-name (etm-new ecc-monitoring-tab-name))
        (vterm-buf (ecc--find-own-vterm)))
    (delete-other-windows)
    (find-file (expand-file-name ecc-monitoring-status-file))
    (when (fboundp 'etm-buffer-set) (etm-buffer-set "home"))
    (split-window-right)
    (split-window-right)
    (other-window 1)
    (if (fboundp 'repo-monitor-add)
        (progn
          (repo-monitor-stop)
          (dolist (pair ecc-monitoring-repo-paths)
            (repo-monitor-add (expand-file-name (cdr pair)) (car pair)))
          (switch-to-buffer "*Repo Monitor*")
          (when (fboundp 'etm-buffer-set)
            (etm-buffer-set "semi-home")))
      (switch-to-buffer "*scratch*"))
    (other-window 1)
    (when vterm-buf (switch-to-buffer vterm-buf))
    (when (fboundp 'etm-buffer-set) (etm-buffer-set "results"))
    (balance-windows)
    (select-window (car (window-list)))
    (message "Claude monitoring tab created: %s" tab-name)))

(provide 'ecc-monitoring-tab)
