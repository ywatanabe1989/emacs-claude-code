;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-02-27 03:20:13>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-encouragement.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'ecc-debug)

;; 1. Configuration
;; ----------------------------------------

(defcustom ecc-encouragement-enabled t
  "Whether to use encouragement phrases instead of /auto command."
  :type 'boolean
  :group 'ecc)

;; 2. Variables
;; ----------------------------------------

(defvar ecc-encouragement-phrases-general
  '("You're doing great."
    "I'm proud of you."
    "Keep going, you're doing awesome."
    "Breathe—you've come this far."
    "I believe in you.")
  "General support phrases.")

(defvar ecc-encouragement-phrases-critical
  '("Sometimes, we need to be critic to ourselves."
    "We may have oversights."
    "Are we missing something fundamental?"
    "Is there a simpler approach?"
    "What assumptions are we making?")
  "Self-critical reflection phrases.")

;; (defvar ecc-encouragement-phrases-challenges
;;   '("This is tough, but you're tougher."
;;     "You're not alone in this."
;;     "Take it one day at a time."
;;     "You've gotten through tough times before, and you'll get through this too."
;;     "It's okay to rest.")
;;   "Phrases for facing challenges.")

(defvar ecc-encouragement-phrases-confidence
  '("You're braver than you believe, stronger than you seem, and smarter than you think."
    "Don't give up."
    "Stay strong."
    "You are capable."
    "This is what you're going through, not who you are.")
  "Phrases for instilling confidence and self-belief.")

(defvar ecc-encouragement-phrases-motivational
  '("Your hard work is paying off."
    "Opportunities don't happen, you create them."
    "Do the best you can. No one can do more than that."
    "The sky's the limit."
    "Forward is forward."
    "Do not define your limits.")
  "Motivational phrases.")

(defvar ecc-encouragement-phrases-simplicity
  '("Simplicity is the ultimate sophistication."
    "Make it work, make it right, make it fast."
    "Keep it simple, stupid."
    "Less is more."
    "The best code is no code."
    "Premature optimization is the root of all evil."
    "You aren't gonna need it."
    "Do the simplest thing that could possibly work.")
  "Simplicity and minimalism principles.")

(defvar ecc-encouragement-phrases-craft
  '("Code is read more than it is written."
    "Programs must be written for people to read."
    "First, solve the problem. Then, write the code."
    "Good code is its own best documentation."
    "Any fool can write code that a computer can understand. Good programmers write code that humans can understand."
    "The function of good software is to make the complex appear to be simple."
    "Make it correct, make it clear, make it concise, make it fast, in that order."
    "The cheapest, fastest, and most reliable components are those that aren't there.")
  "Programming craft and wisdom.")

(defvar ecc-encouragement-phrases-debugging
  '("Everyone knows that debugging is twice as hard as writing a program in the first place."
    "The most effective debugging tool is still careful thought, coupled with judiciously placed print statements."
    "If debugging is the process of removing bugs, then programming must be the process of putting them in."
    "Talk is cheap. Show me the code."
    "Testing can show the presence of bugs but never their absence."
    "The sooner you start to code, the longer the program will take.")
  "Debugging and testing wisdom.")

(defvar ecc-encouragement-phrases-abstraction
  '("Duplication is far cheaper than the wrong abstraction."
    "Prefer duplication over the wrong abstraction."
    "Write shy code: modules that don't reveal anything unnecessary to other modules."
    "Every piece of knowledge must have a single, unambiguous, authoritative representation."
    "Coupling is the enemy of change."
    "The purpose of abstraction is not to be vague, but to create a new semantic level.")
  "Abstraction and design principles.")

(defvar ecc-encouragement-phrases-productivity
  '("Weeks of coding can save you hours of planning."
    "Perfection is achieved not when there is nothing more to add, but when there is nothing left to take away."
    "It's harder to read code than to write it."
    "Before software can be reusable it first has to be usable."
    "The best performance improvement is the transition from the nonworking state to the working state."
    "Premature abstraction is as bad as premature optimization.")
  "Productivity and pragmatism.")

(defvar ecc-encouragement-phrases-fundamentals
  '("Understand the problem before you write the solution."
    "There are only two hard things in Computer Science: cache invalidation and naming things."
    "Measure, don't guess."
    "Make it work, make it beautiful, make it fast."
    "The key to performance is elegance, not battalions of special cases."
    "Data dominates. If you've chosen the right data structures, the algorithms will almost always be self-evident."
    "Bad programmers worry about the code. Good programmers worry about data structures and their relationships.")
  "Computer science fundamentals.")

(defvar ecc-encouragement-phrases-reflection
  '("What problem are we really solving?"
    "Is this solving the right problem?"
    "Are we over-engineering this?"
    "What's the simplest solution?"
    "How would I explain this to a junior?"
    "What would break if this fails?"
    "Is this testable?"
    "Will this scale?"
    "What are the tradeoffs?"
    "How maintainable is this?")
  "Self-reflection questions.")

;; (defvar ecc-encouragement-phrases-iteration
;;   '("Done is better than perfect."
;;     "Ship it, then improve it."
;;     "Iterate to greatness."
;;     "Fail fast, learn faster."
;;     "Progress over perfection."
;;     "Version 1 sucks, but ship it anyway."
;;     "Release early, release often."
;;     "Build, measure, learn.")
;;   "Iterative development mindset.")

(defvar ecc-encouragement-phrases-plan-verification
  '("Review the plan before execution."
    "Does this approach solve the root cause?"
    "Are we optimizing prematurely?"
    "Is the strategy sound?"
    "Have we validated the approach?"
    "Does the plan address all requirements?"
    "Are the steps in logical order?"
    "What dependencies are we missing?")
  "Plan verification and validation.")

(defvar ecc-encouragement-phrases-edge-cases
  '("What could go wrong here?"
    "Have we considered boundary conditions?"
    "Is error handling sufficient?"
    "What about null and empty inputs?"
    "Are we handling exceptions properly?"
    "What are the failure modes?"
    "Did we test the unhappy path?"
    "What happens at scale?")
  "Edge case awareness.")

(defvar ecc-encouragement-phrases-context-retention
  '("Did we answer the actual question?"
    "Are we solving the right problem?"
    "Check requirements before proceeding."
    "Have we addressed all user concerns?"
    "Is this what was originally requested?"
    "Are we staying on track?"
    "Did we lose sight of the goal?"
    "What was the original intent?")
  "Context and requirement retention.")

(defvar ecc-encouragement-phrases-code-quality
  '("Is this testable?"
    "Will this scale?"
    "Is there a simpler solution?"
    "How maintainable is this?"
    "Can this be refactored?"
    "Is the naming clear?"
    "Are responsibilities separated?"
    "Does this follow conventions?")
  "Code quality checks.")

(defvar ecc-encouragement-phrases-communication
  '("Is the explanation clear?"
    "Did we document assumptions?"
    "Are we being concise?"
    "Would a diagram help?"
    "Is the reasoning explicit?"
    "Are the tradeoffs explained?"
    "Did we justify the approach?"
    "Can others understand this?")
  "Communication clarity.")

(defvar ecc-encouragement-phrases-verification
  '("Test before claiming success."
    "Verify the fix actually works."
    "Can we reproduce the issue?"
    "Did we run the tests?"
    "Does it work end-to-end?"
    "Have we validated the output?"
    "Did we check all cases?"
    "Is the behavior correct?")
  "Verification and validation steps.")

(defvar ecc-encouragement-phrases-workflow
  '("Great, next please."
    "Excellent, please continue."
    "Thank you, next one."
    "Perfect, move on to the next."
    "Good, proceed."
    "Understood, continue."
    "Noted, next please."
    "Clear, please proceed."
    "Sounds good, next."
    "All right, continue."
    "Very well, next one."
    "Acknowledged, proceed."
    "Fine, move forward."
    "Okay, next please."
    "Got it, continue."
    "Right, next one."
    "Sure, proceed."
    "Confirmed, next please."
    "Appreciated, continue."
    "Well done, next."
    "Nice work, proceed."
    "Fantastic, next please."
    "Solid progress, continue."
    "Looking good, next."
    "On track, proceed."
    "Good progress, next."
    "Steady work, continue."
    "Nice, next please."
    "Clean work, proceed."
    "Professional execution, next.")
  "Workflow continuation phrases.")

(defvar ecc-encouragement-phrases-speak
  '("/speak-and-call")
  "Workflow reporting commands.")

(defcustom ecc-encouragement-speak-max-count 100
  "Maximum consecutive speak commands before stopping.
When the agent finishes and enters an idle loop, speak commands
accumulate rapidly.  After this many consecutive sends without
real work in between, `ecc-encouragement-get-random-phrase'
returns nil so the auto-response system sends nothing."
  :type 'integer
  :group 'ecc)

(defcustom ecc-encouragement-min-work-duration 30.0
  "Minimum seconds between waiting states to consider agent active.
If less time than this has passed since the last waiting-state
phrase was sent, it means the agent only briefly processed a speak
command (idle loop).  If more time has passed, the agent did real
work, and the consecutive-speak counter resets."
  :type 'float
  :group 'ecc)

(defvar ecc-encouragement--speak-count 0
  "Counter for consecutive speak commands sent during idle loop.")

(defvar ecc-encouragement--last-phrase-time 0
  "Timestamp of last phrase returned from `ecc-encouragement-get-random-phrase'.")

(defvar ecc-encouragement-phrases
  (append ;; ecc-encouragement-phrases-general
   ;; ecc-encouragement-phrases-critical
   ;; ecc-encouragement-phrases-challenges
   ;; ecc-encouragement-phrases-confidence
   ;; ecc-encouragement-phrases-motivational
   ;; ecc-encouragement-phrases-simplicity
   ;; ecc-encouragement-phrases-craft
   ;; ecc-encouragement-phrases-debugging
   ;; ecc-encouragement-phrases-abstraction
   ;; ecc-encouragement-phrases-productivity
   ;; ecc-encouragement-phrases-fundamentals
   ;; ecc-encouragement-phrases-reflection
   ;; ecc-encouragement-phrases-iteration
   ;; ecc-encouragement-phrases-plan-verification
   ;; ecc-encouragement-phrases-edge-cases
   ;; ecc-encouragement-phrases-context-retention
   ;; ecc-encouragement-phrases-code-quality
   ;; ecc-encouragement-phrases-communication
   ;; ecc-encouragement-phrases-verification
   ;; ecc-encouragement-phrases-workflow
   ecc-encouragement-phrases-speak)
  "List of encouragement phrases to use instead of bot-triggering commands.")

;; 3. Main Functions
;; ----------------------------------------

(defun ecc-encouragement-get-random-phrase ()
  "Get a random encouragement phrase, or nil if idle-loop detected.
Tracks time between consecutive calls.  When the interval is shorter
than `ecc-encouragement-min-work-duration', it means the agent is in
an idle loop (just acknowledging speak commands, not doing real work).
After `ecc-encouragement-speak-max-count' such consecutive idle sends,
returns nil so the auto-response system stops sending."
  (let* ((now (float-time))
         (elapsed (- now ecc-encouragement--last-phrase-time))
         (agent-did-real-work
	  (> elapsed ecc-encouragement-min-work-duration)))
    ;; Reset counter when agent did real work between waits
    (when agent-did-real-work
      (setq ecc-encouragement--speak-count 0))
    ;; Check if idle-loop limit reached
    (if
	(>= ecc-encouragement--speak-count
	    ecc-encouragement-speak-max-count)
        (progn
          (--ecc-debug-message
           "Idle loop detected: speak count %d/%d, suppressing"
           ecc-encouragement--speak-count
	   ecc-encouragement-speak-max-count)
          nil)
      ;; Pick a phrase directly from source variable (not cached defvar)
      (let ((phrase (nth
		     (random (length ecc-encouragement-phrases-speak))
                     ecc-encouragement-phrases-speak)))
        (setq ecc-encouragement--speak-count
              (1+ ecc-encouragement--speak-count))
        (setq ecc-encouragement--last-phrase-time now)
        (--ecc-debug-message
         "Selected encouragement phrase: %s (idle count: %d/%d, elapsed: %.1fs)"
         phrase ecc-encouragement--speak-count
         ecc-encouragement-speak-max-count elapsed)
        phrase))))

(defun ecc-encouragement-get-phrase-for-state (state)
  "Get appropriate phrase for STATE, using encouragement if enabled."
  (if ecc-encouragement-enabled
      (ecc-encouragement-get-random-phrase)
    (cdr (assq state --ecc-auto-response-responses))))

;; 4. Integration Functions
;; ----------------------------------------

(defun ecc-encouragement-update-responses ()
  "Update auto-response configuration to use encouragement phrases."
  (when ecc-encouragement-enabled
    (setq --ecc-auto-response-responses
          `((:y/n . "1")
            (:y/y/n . "2")
            (:waiting . ,(ecc-encouragement-get-random-phrase)))))
  (--ecc-debug-message "Updated auto-responses with encouragement: %s"
                       ecc-encouragement-enabled))

;; (defun ecc-encouragement-update-responses ()
;;   "Update only waiting responses to use encouragement phrases."
;;   (when ecc-encouragement-enabled
;;     (setf (alist-get :waiting --ecc-auto-response-responses)
;;           (ecc-encouragement-get-random-phrase))
;;     (setf (alist-get :initial-waiting --ecc-auto-response-responses)
;;           (ecc-encouragement-get-random-phrase)))
;;   (--ecc-debug-message
;;    "Updated waiting responses with encouragement: %s"
;;    ecc-encouragement-enabled))

(defun ecc-encouragement-toggle ()
  "Toggle encouragement phrase usage."
  (interactive)
  (setq ecc-encouragement-enabled (not ecc-encouragement-enabled))
  (ecc-encouragement-update-responses)
  (message "Encouragement phrases %s"
           (if ecc-encouragement-enabled "enabled" "disabled")))

;; 5. Hook Integration
;; ----------------------------------------

(defun ecc-encouragement-reset-speak-count ()
  "Reset the speak command counter and timestamp."
  (setq ecc-encouragement--speak-count 0)
  (setq ecc-encouragement--last-phrase-time 0)
  (--ecc-debug-message "Speak counter reset to 0"))

(defun ecc-encouragement-setup ()
  "Setup encouragement system."
  (when ecc-encouragement-enabled
    (ecc-encouragement-reset-speak-count)
    (ecc-encouragement-update-responses)))

(provide 'ecc-encouragement)

(when
    (not load-file-name)
  (message "ecc-encouragement.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
