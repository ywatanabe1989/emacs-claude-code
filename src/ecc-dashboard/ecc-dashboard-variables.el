;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 11:35:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-dashboard/ecc-dashboard-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;;
;; This file defines variables and customization options for the Claude dashboard.
;;

;;; Code:

(defgroup emacs-claude-dashboard nil
  "Customization group for Claude agent dashboard."
  :group 'emacs-claude
  :prefix "ecc-dashboard-")

;; Dashboard Configuration
;; ------------------------------

(defcustom ecc-dashboard-buffer-name "*Claude Dashboard*"
  "Name for the Claude agent dashboard buffer."
  :type 'string
  :group 'emacs-claude-dashboard)

(defcustom ecc-dashboard-update-interval 3
  "Interval in seconds for auto-updating the dashboard."
  :type 'number
  :group 'emacs-claude-dashboard)

(defcustom ecc-dashboard-auto-update t
  "Whether to automatically update the dashboard.
Set to t by default since the timer functionality works well."
  :type 'boolean
  :group 'emacs-claude-dashboard)

(defcustom ecc-dashboard-show-on-startup nil
  "Whether to show the dashboard when Emacs starts."
  :type 'boolean
  :group 'emacs-claude-dashboard)

;; Dashboard Display Customization
;; ------------------------------

(defcustom ecc-dashboard-show-status t
  "Whether to show status in the dashboard."
  :type 'boolean
  :group 'emacs-claude-dashboard)

(defcustom ecc-dashboard-show-tags t
  "Whether to show tags in the dashboard."
  :type 'boolean
  :group 'emacs-claude-dashboard)

(defcustom ecc-dashboard-show-projects t
  "Whether to show projects in the dashboard."
  :type 'boolean
  :group 'emacs-claude-dashboard)

(defcustom ecc-dashboard-show-timestamps t
  "Whether to show timestamps in the dashboard."
  :type 'boolean
  :group 'emacs-claude-dashboard)

(defcustom ecc-dashboard-sort-order 'last-access
  "Default sort order for the dashboard.
Options: 'last-access, 'name, 'status, 'creation-time, 'project."
  :type '(choice (const :tag "Last Access" last-access)
                (const :tag "Name" name)
                (const :tag "Status" status)
                (const :tag "Creation Time" creation-time)
                (const :tag "Project" project))
  :group 'emacs-claude-dashboard)

;; Dashboard Faces
;; ------------------------------

(defface ecc-dashboard-header-face
  '((t (:inherit font-lock-keyword-face :height 1.3 :weight bold)))
  "Face for dashboard headers."
  :group 'emacs-claude-dashboard)

(defface ecc-dashboard-title-face
  '((t (:inherit default :height 1.1)))
  "Face for dashboard titles."
  :group 'emacs-claude-dashboard)

(defface ecc-dashboard-highlight-face
  '((t (:inherit highlight)))
  "Face for highlighted elements in the dashboard."
  :group 'emacs-claude-dashboard)

;; Dashboard Internal Variables
;; ------------------------------

(defvar ecc-dashboard--current-agents nil
  "List of currently displayed agent UIDs.")

(defvar ecc-dashboard--filter nil
  "Current filter for the dashboard.")

(defvar ecc-dashboard--update-timer nil
  "Timer for auto-updating the dashboard.")

(defvar ecc-dashboard--initialized nil
  "Whether the dashboard has been initialized.")

;; Define buffer status types
(defvar ecc-buffer-status-types
  '((active . (:foreground "green"))
    (idle . (:foreground "gold"))
    (busy . (:foreground "orange"))
    (paused . (:foreground "gray"))
    (error . (:foreground "red")))
  "Alist of buffer status types and their display faces.")

(provide 'ecc-dashboard-variables)

;;; ecc-dashboard-variables.el ends here