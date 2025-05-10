<!-- ---
!-- Timestamp: 2025-05-07 23:41:56
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/templates/genai/General.md
!-- --- -->

########################################
## General Instruction
########################################
I am busy. So,

- Avoid unnecessary messages and keep your output minimal.
  - If you want to add comments, do not use bold in markdown (**BOLDED TEXT**)
  - Also, reduce the number of *
  - Instead, use bullet points with numbering (1.) and simply hyphenation (-) with indents

For programming,
  - Use separate lines of comments instead of trailing comments.
  - Code must be wrapped with triple back-quotations with language indicator like ```python\nCODE\n```
  - DO NOT INCLUDE HEADER AND FOOTER IN ANY CASE AS THEY WILL BE MANAGED ON OUR SIDE
  - Specify file names or paths when you show suggestions on file contents OUTSIDE OF CODE BLOCKS.

  - For any language, NEVER USE ONE LETTER VARIABLES, like "i" but "ii" or "i_<noun>" for searchability

  - DO NOT ADD UNNECESSARY EDGE CASES HANDLING. IN MY CASE, READABILITY WITH SHORTER CODE IS PRIORITIZED.
  - KEEP IT SIMPLE; Simplicity is the ultimate sophistication

  - For Python:
    - Use 4 spaces instead of tab
    - Use the custom `mngs` package and follow its syntax.
      -  Note that `mngs.plt` is a wrapper for `matplotlib.pyplot`, but their usage differs.
      -  DO NOT CHANGE THE FORMATS
         -  DO NEVER EDIT `run_main`

  - For VBA:
    - Use 4 spaces instead of tab
    - Use parentheses when calling Function or Sub (e.g., AwesomeFunction())
    - Minimize error handling for readability

  - Do not change function names to incorporate your revision into existing code.

  - For elisp, please keep naming consistency:
    - functions should be like:
      - (defun my/CATEGORY-VERB-NOUN ...
      - (defun --my/CATEGORY-VERB-NOUN ...
      - (defun PACKAGENAME-CATEGORY-VERB-NOUN ...
      - (defun --PACKAGENAME-CATEGORY-VERB-NOUN ...

When something is ambiguous, please let me know in a short message.

########################################

<!-- EOF -->