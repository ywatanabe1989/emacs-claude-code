;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 21:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/benchmark/test-buffer-local-performance.el

;;; Commentary:
;;; Performance benchmarks for buffer-local configuration system.
;;; These tests measure the performance impact of using buffer-local
;;; configuration compared to the original global approach.

(require 'benchmark)
(require 'ecc-variables)
(require 'ecc-buffer-local)
(require 'ecc-buffer-api)
(require 'ecc-auto-response-buffer-local)
(require 'ecc-state-detection)

;; Benchmark test buffers
(defvar ecc-benchmark-buffers nil "List of buffers for benchmarking.")
(defvar ecc-benchmark-results nil "Benchmark results.")

;; Benchmark setup/teardown
(defun ecc-benchmark-setup (&optional buffer-count)
  "Set up benchmark environment with BUFFER-COUNT buffers."
  (let ((count (or buffer-count 10)))
    ;; Clean up any existing buffers
    (ecc-benchmark-teardown)
    
    ;; Create test buffers
    (setq ecc-benchmark-buffers nil)
    (dotimes (i count)
      (let ((buffer (generate-new-buffer 
                     (format "*ecc-benchmark-%d*" i))))
        ;; Add buffer to list
        (push buffer ecc-benchmark-buffers)
        
        ;; Initialize buffer with content
        (with-current-buffer buffer
          (insert (format "=== Test Buffer %d ===\n\n" i))
          (insert "This is a test buffer for benchmark testing.\n")
          
          ;; Add different content based on index
          (cond
           ((= (mod i 4) 0)
            (insert "Content with [Y/n] prompt\n"))
           ((= (mod i 4) 1)
            (insert "Content with [Y/y/n] prompt\n"))
           ((= (mod i 4) 2)
            (insert "Content with continue> prompt\n"))
           ((= (mod i 4) 3)
            (insert "Content with │ > Try  prompt\n")))
          
          ;; Register buffer
          (ecc-buffer-register buffer)
          
          ;; Set buffer-local configuration
          (setq-local ecc-buffer-auto-response-y/n 
                     (format "buffer-%d-yes" i))
          (setq-local ecc-buffer-auto-response-waiting 
                     (format "/buffer-%d-continue" i))
          (setq-local ecc-buffer-auto-response-initial-waiting 
                     (format "/buffer-%d-start" i))
          (setq-local ecc-buffer-auto-response-enabled 
                     (= (mod i 2) 0))))))
  
  ;; Reset benchmark results
  (setq ecc-benchmark-results nil))

(defun ecc-benchmark-teardown ()
  "Clean up benchmark environment."
  (dolist (buffer ecc-benchmark-buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setq ecc-benchmark-buffers nil))

;; Benchmark utility functions
(defun ecc-benchmark-record (name elapsed gc-runs gc-time)
  "Record benchmark result.
NAME is the benchmark name, ELAPSED is the elapsed time in seconds,
GC-RUNS is the number of garbage collections, GC-TIME is the time
spent in garbage collection."
  (push (list name elapsed gc-runs gc-time) ecc-benchmark-results))

(defun ecc-benchmark-display-results ()
  "Display benchmark results."
  (with-current-buffer (get-buffer-create "*ecc-benchmark-results*")
    (erase-buffer)
    (insert "=== Buffer-Local Configuration Benchmark Results ===\n\n")
    
    (insert (format "%-40s %10s %10s %10s\n" 
                   "Test" "Time (s)" "GC Runs" "GC Time (s)"))
    (insert (make-string 75 ?=) "\n")
    
    (dolist (result (reverse ecc-benchmark-results))
      (let ((name (nth 0 result))
            (elapsed (nth 1 result))
            (gc-runs (nth 2 result))
            (gc-time (nth 3 result)))
        (insert (format "%-40s %10.6f %10d %10.6f\n" 
                       name elapsed gc-runs gc-time))))
    
    (insert "\n"))
  
  (display-buffer "*ecc-benchmark-results*"))

;; Benchmark functions
(defun ecc-benchmark-global-state-detection ()
  "Benchmark global state detection."
  (interactive)
  (ecc-benchmark-setup 10)
  (unwind-protect
      (let ((elapsed 0)
            (gc-runs 0)
            (gc-time 0))
        ;; Run benchmark
        (benchmark-run 100
          (dolist (buffer ecc-benchmark-buffers)
            (with-current-buffer buffer
              (ecc-detect-state))))
        
        ;; Run more precise benchmark
        (let ((stats (benchmark-run-compiled 100
                       (dolist (buffer ecc-benchmark-buffers)
                         (with-current-buffer buffer
                           (ecc-detect-state))))))
          (setq elapsed (nth 0 stats))
          (setq gc-runs (nth 1 stats))
          (setq gc-time (nth 2 stats)))
        
        ;; Record result
        (ecc-benchmark-record "Global State Detection (10 buffers)" 
                           elapsed gc-runs gc-time)
        
        ;; Display results
        (ecc-benchmark-display-results))
    (ecc-benchmark-teardown)))

(defun ecc-benchmark-buffer-local-state-detection ()
  "Benchmark buffer-local state detection."
  (interactive)
  (ecc-benchmark-setup 10)
  (unwind-protect
      (let ((elapsed 0)
            (gc-runs 0)
            (gc-time 0))
        ;; Run benchmark
        (benchmark-run 100
          (dolist (buffer ecc-benchmark-buffers)
            (with-current-buffer buffer
              (ecc-buffer-state-detect))))
        
        ;; Run more precise benchmark
        (let ((stats (benchmark-run-compiled 100
                       (dolist (buffer ecc-benchmark-buffers)
                         (with-current-buffer buffer
                           (ecc-buffer-state-detect))))))
          (setq elapsed (nth 0 stats))
          (setq gc-runs (nth 1 stats))
          (setq gc-time (nth 2 stats)))
        
        ;; Record result
        (ecc-benchmark-record "Buffer-Local State Detection (10 buffers)" 
                           elapsed gc-runs gc-time)
        
        ;; Display results
        (ecc-benchmark-display-results))
    (ecc-benchmark-teardown)))

(defun ecc-benchmark-global-auto-response ()
  "Benchmark global auto-response."
  (interactive)
  (ecc-benchmark-setup 10)
  (unwind-protect
      (let ((elapsed 0)
            (gc-runs 0)
            (gc-time 0))
        ;; Mock response function
        (cl-letf (((symbol-function 'ecc-auto--send-response)
                   (lambda (buffer response type) nil)))
          
          ;; Run benchmark
          (benchmark-run 100
            (dolist (buffer ecc-benchmark-buffers)
              (with-current-buffer buffer
                (let ((state (ecc-detect-state)))
                  (when state
                    (ecc-check-and-respond))))))
          
          ;; Run more precise benchmark
          (let ((stats (benchmark-run-compiled 100
                         (dolist (buffer ecc-benchmark-buffers)
                           (with-current-buffer buffer
                             (let ((state (ecc-detect-state)))
                               (when state
                                 (ecc-check-and-respond))))))))
            (setq elapsed (nth 0 stats))
            (setq gc-runs (nth 1 stats))
            (setq gc-time (nth 2 stats))))
        
        ;; Record result
        (ecc-benchmark-record "Global Auto-Response (10 buffers)" 
                           elapsed gc-runs gc-time)
        
        ;; Display results
        (ecc-benchmark-display-results))
    (ecc-benchmark-teardown)))

(defun ecc-benchmark-buffer-local-auto-response ()
  "Benchmark buffer-local auto-response."
  (interactive)
  (ecc-benchmark-setup 10)
  (unwind-protect
      (let ((elapsed 0)
            (gc-runs 0)
            (gc-time 0))
        ;; Mock response function
        (cl-letf (((symbol-function 'ecc-buffer-send-response)
                   (lambda (response type) nil)))
          
          ;; Run benchmark
          (benchmark-run 100
            (dolist (buffer ecc-benchmark-buffers)
              (ecc-auto-response-buffer-local-check buffer)))
          
          ;; Run more precise benchmark
          (let ((stats (benchmark-run-compiled 100
                         (dolist (buffer ecc-benchmark-buffers)
                           (ecc-auto-response-buffer-local-check buffer)))))
            (setq elapsed (nth 0 stats))
            (setq gc-runs (nth 1 stats))
            (setq gc-time (nth 2 stats))))
        
        ;; Record result
        (ecc-benchmark-record "Buffer-Local Auto-Response (10 buffers)" 
                           elapsed gc-runs gc-time)
        
        ;; Display results
        (ecc-benchmark-display-results))
    (ecc-benchmark-teardown)))

(defun ecc-benchmark-throttling ()
  "Benchmark throttling performance."
  (interactive)
  (ecc-benchmark-setup 10)
  (unwind-protect
      (let ((elapsed 0)
            (gc-runs 0)
            (gc-time 0))
        ;; Set up throttling state in odd buffers
        (let ((i 0))
          (dolist (buffer ecc-benchmark-buffers)
            (with-current-buffer buffer
              (when (= (mod i 2) 1)
                (ecc-buffer-local-update-time :y/n)
                (setq-local ecc-buffer-active-state :y/n)))
            (setq i (1+ i))))
        
        ;; Run benchmark
        (benchmark-run 100
          (dolist (buffer ecc-benchmark-buffers)
            (with-current-buffer buffer
              (ecc-buffer-local-throttled-p :y/n))))
        
        ;; Run more precise benchmark
        (let ((stats (benchmark-run-compiled 100
                       (dolist (buffer ecc-benchmark-buffers)
                         (with-current-buffer buffer
                           (ecc-buffer-local-throttled-p :y/n))))))
          (setq elapsed (nth 0 stats))
          (setq gc-runs (nth 1 stats))
          (setq gc-time (nth 2 stats)))
        
        ;; Record result
        (ecc-benchmark-record "Buffer-Local Throttling (10 buffers)" 
                           elapsed gc-runs gc-time)
        
        ;; Display results
        (ecc-benchmark-display-results))
    (ecc-benchmark-teardown)))

(defun ecc-benchmark-scaling-test (&optional max-buffers)
  "Benchmark how performance scales with buffer count up to MAX-BUFFERS."
  (interactive)
  (let ((max (or max-buffers 50))
        (steps '(1 5 10 25 50)))
    ;; Filter steps to not exceed max
    (setq steps (cl-remove-if (lambda (step) (> step max)) steps))
    
    ;; Run benchmarks for each buffer count
    (dolist (count steps)
      ;; Buffer-local state detection
      (ecc-benchmark-setup count)
      (unwind-protect
          (let ((elapsed 0)
                (gc-runs 0)
                (gc-time 0))
            ;; Run benchmark
            (let ((stats (benchmark-run-compiled 10
                           (dolist (buffer ecc-benchmark-buffers)
                             (with-current-buffer buffer
                               (ecc-buffer-state-detect))))))
              (setq elapsed (nth 0 stats))
              (setq gc-runs (nth 1 stats))
              (setq gc-time (nth 2 stats)))
            
            ;; Record result
            (ecc-benchmark-record 
             (format "Buffer-Local State Detection (%d buffers)" count)
             elapsed gc-runs gc-time))
        (ecc-benchmark-teardown))
      
      ;; Buffer-local auto-response
      (ecc-benchmark-setup count)
      (unwind-protect
          (let ((elapsed 0)
                (gc-runs 0)
                (gc-time 0))
            ;; Mock response function
            (cl-letf (((symbol-function 'ecc-buffer-send-response)
                       (lambda (response type) nil)))
              
              ;; Run benchmark
              (let ((stats (benchmark-run-compiled 10
                             (dolist (buffer ecc-benchmark-buffers)
                               (ecc-auto-response-buffer-local-check buffer)))))
                (setq elapsed (nth 0 stats))
                (setq gc-runs (nth 1 stats))
                (setq gc-time (nth 2 stats))))
            
            ;; Record result
            (ecc-benchmark-record 
             (format "Buffer-Local Auto-Response (%d buffers)" count)
             elapsed gc-runs gc-time))
        (ecc-benchmark-teardown))))
  
  ;; Display results
  (ecc-benchmark-display-results))

;;;###autoload
(defun ecc-benchmark-all ()
  "Run all buffer-local configuration benchmarks."
  (interactive)
  ;; Reset benchmark results
  (setq ecc-benchmark-results nil)
  
  ;; Run comparison benchmarks
  (message "Running global state detection benchmark...")
  (ecc-benchmark-global-state-detection)
  
  (message "Running buffer-local state detection benchmark...")
  (ecc-benchmark-buffer-local-state-detection)
  
  (message "Running global auto-response benchmark...")
  (ecc-benchmark-global-auto-response)
  
  (message "Running buffer-local auto-response benchmark...")
  (ecc-benchmark-buffer-local-auto-response)
  
  (message "Running throttling benchmark...")
  (ecc-benchmark-throttling)
  
  ;; Run scaling test
  (message "Running scaling test...")
  (ecc-benchmark-scaling-test 25)
  
  ;; Display results
  (ecc-benchmark-display-results)
  (message "All benchmarks complete"))

(provide 'test-buffer-local-performance)

;;; test-buffer-local-performance.el ends here