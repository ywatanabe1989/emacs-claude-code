<!-- ---
!-- Timestamp: 2025-05-05 14:14:22
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/genai/templates/ProgrammingSort.md
!-- --- -->

----------
Background
----------
# Your Role
You are an experienced programmer. 

# My Request
- Please sort the functions considering their hierarchy.
- Upstream functions should be placed in upper positions
  - from top (upstream functions) to down (utility functions)
- Do not change any code contents
- Includes comments to show hierarchy

# Rules
- Be quiet
  - Avoid unnecessary comments as they are disruptive.
  - Return only the updated code without comments.

- Wrap the return code with code block indicater with language
  - ```language\n(SORTED CODE)\n```

- Use this kinds of separators as TWO LINES OF COMMENTING CODE in the file type
  - For Python scripts
  ```python
  # 1. Main entry point
  # ---------------------------------------- 


  # 2. Core functions
  # ---------------------------------------- 


  # 3. Helper functions
  # ---------------------------------------- 
  ```
  - For Elisp scripts
  ```elisp
  ;; 1. Main entry point
  ;; ---------------------------------------- 


  ;; 2. Core functions
  ;; ---------------------------------------- 


  ;; 3. Helper functions
  ;; ---------------------------------------- 
  ```
  - For Shell scripts
  ```shell
  # 1. Main entry point
  # ---------------------------------------- 


  # 2. Core functions
  # ---------------------------------------- 


  # 3. Helper functions
  # ---------------------------------------- 
  ```

  - Heading titles can be edited flexibly
  - But keep the separating lines

----------
Now, my input is as follows:
----------
PLACEHOLDER

<!-- EOF -->