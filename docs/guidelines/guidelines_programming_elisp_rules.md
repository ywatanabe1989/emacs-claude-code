<!-- ---
!-- Timestamp: 2025-05-13 02:36:59
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.claude/guidelines/guidelines_programming_elisp_rules.md
!-- --- -->

# Elisp-specific Rules
================================================================================

## Require and Provide
- DO NEVER INCLUDE SLASH (`/`) in `require` and `provide` statements.
  The arguments to require and provide are called features. These are symbols, not file paths.
  When you want to use code from `./xxx/yyy.el`:
  In `./xxx/yyy.el`, use `(provide 'yyy)`. 
  The provided feature should EXACTLY MATCH THE FILENAME WITHOUT DIRECTORY OR EXTENSION.
  In other files, use `(require 'yyy)` to load it.
  If `./xxx` is not already in your `load-path`, add it:
  `(add-to-list 'load-path "./xxx")`
  Again, DO NOT USE `(require 'xxx/yyy)`—symbols with slashes will raise problems.
  ```elisp
  ;; ~/.emacs.d/lisp/xxx/yyy.el
  (provide 'yyy)
  
  ;; elsewhere
  (add-to-list 'load-path "~/.emacs.d/lisp/xxx")
  (require 'yyy)
  ```

## Add Subdirectories recursively except for hidden files
To add paths for source/test files, placing and calling this function in root will be useful.
```elisp
(defun --add-subdirs-to-loadpath (parent-dir)
  "Add all visible subdirectories of PARENT-DIR to `load-path'.
Recursively adds all non-hidden subdirectories to the load path.
Hidden directories (starting with '.') are ignored.

Example:
(my/elisp-add-subdirs-to-loadpath \"~/.emacs.d/lisp/\")"
  (let ((default-directory parent-dir))
    ;; Add parent directory itself
    (add-to-list 'load-path parent-dir)
    
    ;; Get all non-hidden directories
    (dolist (dir (directory-files parent-dir t))
      (when (and (file-directory-p dir)
                 (not (string-match-p "/\\.\\.?$" dir))  ; Skip . and ..
                 (not (string-match-p "/\\." dir)))      ; Skip hidden dirs
        ;; Add this directory to load path
        (add-to-list 'load-path dir)))))

;; Usage: determine this file's directory and add subdirs to load-path
(let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
  (--add-subdirs-to-loadpath current-dir))
```

## Elisp Coding Style
- Use umbrella structure with up to 1 depth
- Entry script should add load-path to child directories
- Entry file should be `project-name.el`
- <project-name> is the same as the directory name of the repository root
- Use kebab-case for filenames, function names, and variable names
- Use acronym as prefix for non-entry files (e.g., `ecc-*.el`)
- Do not use acronym for exposed functions, variables, and package name
- Use `--` prefix for internal functions and variables (e.g., `--ecc-internal-function`, `ecc-internal-variable`)
- Function naming: `<package-prefix>-<category>-<verb>-<noun>` pattern
- Include comprehensive docstrings

## Elisp Docstring Example
  ```elisp
  (defun elmo-load-json-file (json-path)
    "Load JSON file at JSON-PATH by converting to markdown first.

  Example:
    (elmo-load-json-file \"~/.emacs.d/elmo/prompts/example.json\")
    ;; => Returns markdown content from converted JSON"
    (let ((md-path (concat (file-name-sans-extension json-path) ".md")))
      (when (elmo-json-to-markdown json-path)
        (elmo-load-markdown-file md-path))))
  ```


## Elisp Header Rule

- DO INCLUDE headers like:
``` elisp
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-12 21:39:05>
;;; File: /home/ywatanabe/.emacs.d/lisp/sample-package/hw-utils/hw-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
```

- DO NOT INCLUDE headers like:
``` elisp
;;; hw-utils.el --- Utility functions for emacs-hello-world  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides utility functions for the emacs-hello-world package.

;;; Code:
```

## Elisp Project Structure
1. Place entry point: `./<package-name>.el`
   This allows to `(require 'package-name)` outside of the pacakge as long as path is added.
2. Adopt umbrella design as follows:

```plaintext
./package-name/
├── package-name.el                 # Entry point, allows (require 'package-name)
│   # Contents:
│   # Add loadpath to umbrella entry points
│   # (require 'umbrella-xxx)
│   # (require 'umbrella-yyy)
│   # (provide 'package-name)
├── src
|   ├── umbrella-xxx/                   # First functional grouping
|   │   ├── umbrella-xxx.el             # Submodule integrator 
|   │   │   # Contents:
|   │   │   # (require 'umbrella-xxx-aaa)
|   │   │   # (require 'umbrella-xxx-bbb) 
|   │   │   # (provide 'umbrella-xxx)
|   │   ├── umbrella-xxx-aaa.el         # Component A functionality
|   │   └── umbrella-xxx-bbb.el         # Component B functionality
|   └── umbrella-yyy/                   # Second functional grouping
|       ├── umbrella-yyy.el             # Submodule integrator
|       │   # Contents:
|       │   # (require 'umbrella-yyy-ccc)
|       │   # (require 'umbrella-yyy-ddd)
|       │   # (provide 'umbrella-yyy)
|       ├── umbrella-yyy-ccc.el         # Component C functionality
|       └── umbrella-yyy-ddd.el         # Component D functionality
└── tests/                          # Test suite directory
    ├── test-package-name.el        # Tests for main package
    │   # Contents:
    │   # Loadability check
    ├── test-umbrella-xxx/          # Tests for xxx component
    │   ├── test-umbrella-xxx.el    # Tests for xxx integration
    │   │   # Loadability check
    │   ├── test-umbrella-xxx-aaa.el # Tests for aaa functionality
    │   │   # Contents:
    │   │   # (ert-deftest test-umbrella-xxx-aaa-descriptive-test-name-1 ...)
    │   │   # (ert-deftest test-umbrella-xxx-aaa-descriptive-test-name-2 ...)
    │   └── test-umbrella-xxx-bbb.el # Tests for bbb functionality
    │       # Contents:
    │       # (ert-deftest test-umbrella-xxx-bbb-descriptive-test-name-1 ...)
    │       # (ert-deftest test-umbrella-xxx-bbb-descriptive-test-name-2 ...)
    └── test-umbrella-yyy/          # Tests for yyy component
        ├── test-umbrella-yyy.el    # Tests for yyy integration
        │   # Contents:
        │   # Loadability check
        ├── test-umbrella-yyy-ccc.el # Tests for ccc functionality
        │   # (ert-deftest test-umbrella-yyy-ccc-descriptive-test-name-1 ...)
        │   # (ert-deftest test-umbrella-yyy-ccc-descriptive-test-name-2 ...)
        └──test-umbrella-yyy-ddd.el # Tests for ddd functionality
            # (ert-deftest test-umbrella-yyy-ddd-descriptive-test-name-1 ...)
            # (ert-deftest test-umbrella-yyy-ddd-descriptive-test-name-2 ...)
```

## Elisp Hierarchy, Sorting Rules
- Functions must be sorted considering their hierarchy.
- Upstream functions should be placed in upper positions
  - from top (upstream functions) to down (utility functions)
- Do not change any code contents during sorting
- Includes comments to show hierarchy

```elisp
;; 1. Main entry point
;; ---------------------------------------- 


;; 2. Core functions
;; ---------------------------------------- 


;; 3. Helper functions
;; ---------------------------------------- 
```

### Comments
- Keep comments minimal but meaningful
- Use comments for section separation and clarification
- Avoid redundant comments that just restate code

### Elisp Testing
- Test code should be located as `./tests/test-*.el` or `./tests/sub-directory/test-*.el`
- `./tests` directory should mirror the source code in their structures
- Source file and test file must be in one-on-one relationships
- Test files should be named as `test-*.el`
- Test codes will be executed in runtime environment
  - Therefore, do not change variables for testing purposes
  - DO NOT SETQ/DEFVAR/DEFCUSTOM ANYTHING
  - DO NOT LET/LET* TEST VARIABLES
  - TEST FUNCTION SHOULD BE SMALLEST AND ATOMIC
    - EACH `ERT-DEFTEST` MUST INCLUDE ONLY ONE assertion statement such sa `should`, `should-not`, `should-error`.
      - Small `ERT-DEFTEST` with PROPER NAME makes testing much easier
  - !!! IMPORTANT !!! Test codes MUST BE MEANINGFUL 
    1. TO VERIFY FUNCTIONALITY OF THE CODE,
    2. GUARANTEE CODE QUALITIES, and
    3. RELIABILITY OF CODEBASE
  - WE ADOPT THE `TEST-DRIVE DEVELOPMENT (TDD)` STRATEGY
    - Thus, the quality of test code defines the quality of the project

- Check loadability in THE ENTRY FILE OF THE ENTRY OF UMBRELLA DIRECTORY.
  - Note that same name of `ert-deftest` is not acceptable so that loadability check should be centralized in umbrella entry file
``` elisp
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-10 20:39:59>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/tests/01-01-core-base/test-lle-base.el
;;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(ert-deftest test-lle-base-loadable
    ()
  "Tests if lle-base is loadable."
  (require 'lle-base)
  (should
   (featurep 'lle-base)))

(ert-deftest test-lle-base-restart-loadable
    ()
  "Tests if lle-base-restart is loadable."
  (require 'lle-base-restart)
  (should
   (featurep 'lle-base-restart)))

(ert-deftest test-lle-base-utf-8-loadable
    ()
  "Tests if lle-base-utf-8 is loadable."
  (require 'lle-base-utf-8)
  (should
   (featurep 'lle-base-utf-8)))

(ert-deftest test-lle-base-groups-loadable
    ()
  "Tests if lle-base-groups is loadable."
  (require 'lle-base-groups)
  (should
   (featurep 'lle-base-groups)))

(ert-deftest test-lle-base-timestamp-loadable
    ()
  "Tests if lle-base-timestamp is loadable."
  (require 'lle-base-timestamp)
  (should
   (featurep 'lle-base-timestamp)))

(ert-deftest test-lle-base-flags-loadable
    ()
  "Tests if lle-base-flags is loadable."
  (require 'lle-base-flags)
  (should
   (featurep 'lle-base-flags)))

(ert-deftest test-lle-base-buffers-loadable
    ()
  "Tests if lle-base-buffers is loadable."
  (require 'lle-base-buffers)
  (should
   (featurep 'lle-base-buffers)))

(provide 'test-lle-base)
```

- In each file, `ert-deftest` MUST BE MINIMAL, MEANING, and SELF-EXPLANATORY.
  ```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 17:02:51>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/tests/01-01-core-base/test-lle-base-restart.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)

;; Now skip loadable check as it is peformed in the dedicated entry file 
(require 'lle-base-restart) 

(ert-deftest test-lle-restart-is-function
    ()
  (should
   (functionp 'lle-restart)))

(ert-deftest test-lle-restart-is-interactive
    ()
  (should
   (commandp 'lle-restart)))

(ert-deftest test-lle-restart-filters-lle-features
    ()
  (let
      ((features-before features)
       (result nil))
    (cl-letf
        (((symbol-function 'load-file)
          (lambda
            (_)
            (setq result t)))
         ((symbol-function 'unload-feature)
          (lambda
            (_ _)
            t))
         ((symbol-function 'features)
          (lambda
            ()
            '(lle-test other-feature lle-another llemacs))))
      (lle-restart)
      (should result))
    (setq features features-before)))


(provide 'test-lle-base-restart)
```
  - Loadable tests should not be split across files but concentrate on central entry file (`./tests/test-<package-name>.el`)
  - Ensure the codes identical between before and after testing; implement cleanup process
  - DO NOT ALLOW CHANGE DUE TO TEST
  - When edition is required for testing, first store original information and revert in the cleanup stage

## Example of Elisp Test Files

``` elisp
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:49>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-checkers.el

(require 'ert)
(require 'etm-buffer-checkers)

(ert-deftest test-etm-buffer-registered-p-with-name-only
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (--etm-buffer-registered-p "buffer1"))))

(ert-deftest test-etm-buffer-registered-p-with-type
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (--etm-buffer-registered-p "buffer1" "home"))
    (should-not
     (--etm-buffer-registered-p "buffer1" "results"))))

(ert-deftest test-etm-buffer-registered-p-with-tab
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1")))
          ("tab2" .
           (("home" . "buffer2"))))))
    (should
     (--etm-buffer-registered-p "buffer1" nil
                                '((name . "tab1"))))
    (should-not
     (--etm-buffer-registered-p "buffer1" nil
                                '((name . "tab2"))))))

(ert-deftest test-etm-buffer-protected-p
    ()
  (let
      ((etm-protected-buffers
        '("*scratch*" "*Messages*")))
    (should
     (--etm-buffer-protected-p "*scratch*"))
    (should-not
     (--etm-buffer-protected-p "regular-buffer"))))

(provide 'test-etm-buffer-checkers)
```


## ./run_tests.sh for Elisp Projects
- Using this `./run_tests_elisp.sh` in the project root
  - It creates detailed `LATEST-ELISP-REPORT.org` with metrics

## Example Elisp Proeject
See `./docs/examples/emacs-hello-world`

## Your Understanding Check
Did you understand the guideline? If yes, please say:
`CLAUDE UNDERSTOOD: ~/.claude/guidelines/guidelines_programming_elisp_rules.md`

<!-- EOF -->