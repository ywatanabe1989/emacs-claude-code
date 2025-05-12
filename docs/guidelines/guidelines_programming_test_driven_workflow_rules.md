<!-- ---
!-- Timestamp: 2025-05-11 14:52:31
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.claude/guidelines/guidelines_programming_test_driven_workflow_rules.md
!-- --- -->

## !!! IMPORTANT !!! Test-Driven Development (TDD) Workflow !!! IMPORTANT !!!
The most important guideline in this document is that we adopt test-driven development workflow explained below.

1. **Start with tests**
   - **PRIORITIZE TEST OVER SOURCE**
     - The quality of test is the quality of the project
   - We strictly follow **TEST-DRIVEN DEVELOPMENT**
   - Write MEANINGFUL tests BEFORE implementing source code
   - Write MEANINGFUL tests BASED ON EXPECTED INPUT/OUTPUT PAIRS
   - AVOID MOCK IMPLEMENTATIONS
   - In this stage, TESTS SHOULD TARGET FUNCTIONALITY THAT DOESN'T EXIST YET
   - Use `./run_tests.sh --debug` for running tests
   - Test code should have expected directory structure based on project goals and conventions in the used language

2. **Verify test failures**
   - Run the tests to confirm they fail first
     - Our aim is now clear; all we need is to solve the failed tests
   - Not to write implementation code yet

3. **Git commit test files**
   - Review the tests for completeness to satisfy the project goals and requirements
     - Not determine the qualities of test files based on source files
       - Prioritize test code over source code
       - Thus, test code MUST BE SOLID
   - Commit the tests when satisfied

4. **Implement functionality**
   - If the above steps 1-3 completed, now you are allowed to implement source code that passes the tests
   - !!! IMPORTANT !!! NOT TO MODIFY THE TEST FILES IN THIS STEP
   - Iterate until all tests pass

5. **Verify implementation quality**
   - Use independent subagents to check if implementation overfits to tests
   - Ensure solution meets broader requirements beyond tests

6. **Summarize the current iteration by listing:**
   - What were verified
   - What are not verified yet
     - Reasons why they are not verified if not expected

7. **Commit implementation**
   - Commit the source code once satisfied

## Testing Script
1. Execute `./run_tests.sh --debug` at the repository root
2. Log file (`./.run_tests.sh.log`) may catch errors
3. Read the report `./*REPORT*.org`, especially, contents below this header: `* Failed Tests`
3. THINK next steps based on test results and project progress
4. ~~Known issues are often related to path settings in `./src` and `./test`~~

## About `./run_tests.sh`
- `./run_tests.sh` MUST BE VERSATILE AMONG PROJECTS.
THUS, NEVER USE PROJECT-SPECIFIC INFORMATION, SUCH AS DIRECTORY STRUCTURE, IN THE SCRIPT.
However, we assume test codes are located under `./tests`.
So, in `./run_tests.sh` script, add paths to source and test code recursively for testing purposes.

## Quality Check of Test Code
In any step, ensure qualities of test codes:
    - Are they split into small test functions?
    - Are meaningful to check required functionalities?
    - Are there no obsolete test codes?
    - Are the test code structure mirroring source code?


## Your Understanding Check
Did you understand the guideline? If yes, please say:
`CLAUDE UNDERSTOOD: ~/.claude/guidelines/guidelines_programming_test_driven_workflow_rules.md`

<!-- EOF -->