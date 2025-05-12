<!-- ---
!-- Timestamp: 2025-05-11 14:51:49
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.claude/guidelines/guidelines_programming_general_rules.md
!-- --- -->

## General Programming Rules

- **Focus on code clarity and maintainability**
  - Do Not Repeat Yourself (DRY principle)
  - Use symbolic links wisely, especially for large data to keep clear organization and easy navigation
  - Prepare `./scripts/utils/<versatile_func.py>` for versatile and reusable code
  - If versatile code is applicable beyond projects, implement in the `mngs` package
  - Avoid unnecessary comments as they can be disruptive
  - Return only the updated code without comments
  - Code should be self-explanatory; variable, function, and class names are crucial
  - Comments can be distracting if the code is properly written

- **Avoid 1-letter variable names**
  - They make searching challenging
  - For example, rename variable x to xx for better readability and searchability

- **Commenting style**
  - Subjects of comments should be "this file/code/function" implicitly
  - Verbs should be in singular form (e.g., "# Computes ..." instead of "# Compute ...")

- **Documentation**
  - Always include docstrings with example usage
  - Follow language-specific docstring format standards

- **Imports and Dependencies**
  - Keep importing packages MECE (Mutually Exclusive and Collectively Exhaustive)
  - Remove unnecessary packages and add necessary ones

- **Code Structure**
  - Use modular approaches for reusability, readability, maintainability, and scalability
  - Split functions into subroutines or meaningful chunks whenever possible

- **PATH Conventions**
  - Use relative paths from the project root
  - Relative paths should start with dots, like "./relative/example.py" or "../relative/example.txt"
  - All scripts are assumed to be executed from the project root (e.g., ./scripts/example.sh)

- **Code Block Indicators**
  - Use appropriate code block indicators:
  ```python
  # Python example
  ```
  ```shell
  # Shell example
  ```
  ```elisp
  ;; Elisp example
  ```
  ``` pseudo-code
    # Pseudo-code
  ```
  ``` plaintext
  Plain text
    ```
  ``` markdown
  Markdown Contents
  ```

- **String Formatting**
  - Split strings into shorter lines
  - For example, with f-string concatenation with parentheses in Python:
  ```python
  # Good
  error_msg = (
      f"Failed to parse JSON response: {error}\n"
      f"Prompt: {self._ai_prompt}\n"
      f"Generated command: {commands}\n"
      f"Response: {response_text}"
  )
  
  # Not Good
  error_msg = f"Failed to parse JSON response: {error}\nPrompt: {self._ai_prompt}\nGenerated command: {commands}\nResponse: {response_text}"
  ```

- **Headers and Footers**
  - Do not change headers (e.g., time stamp, file name, authors) and footers (e.g., # EOF)

- **Indentation**
  - Pay attention to indentation
  - Code will be copied/pasted as is

- **Org Report**
  - When asked, prepare report in `.org` file under `./reports` directory
  - Also, convert to PDF

## Your Understanding Check
Did you understand the guideline? If yes, please say:
`CLAUDE UNDERSTOOD: ~/.claude/guidelines/guidelines_programming_general_rules.md`

<!-- EOF -->