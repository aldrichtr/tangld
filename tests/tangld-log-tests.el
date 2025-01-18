;; -*- lexical-binding: t; -*-

(require 'tangld)

;; helper functions
(defun get-tangld-log-message ()
  "Return the text of the log buffer"
  (with-current-buffer (get-buffer tangld-log-buffer-name)
    (goto-char (point-min))
    (thing-at-point 'line t)))

(defun kill-tangld-log-buffer ()
  (let ((buffer (get-buffer tangld-log-buffer-name)))
    (when buffer (kill-buffer buffer))))


(describe
    "tangld logging"
  (describe
      "Default values"
    (it "Log level should be informational only (2)"
      (expect tangld-log-level :to-be 2))
    (it "Logging should go to the *tangld* buffer"
      (expect tangld-log-buffer-name :to-match "\*tangld\*"))
    (it "Do not write to file by default"
      (expect tangld-log-to-file :to-be nil))

    (it "When log-to-file is enabled, log dir should be in the data dir"
      (cond ((eq system-type 'windows-nt)
             (expect tangld-log-dir :to-match "\\\\AppData\\\\Local/tangld/log"))
            (t
             (expect tangld-log-dir :to-match "/\.local/share/tangld/log"))))

    (it "When log-to-file is enabled, log file is tangld.log"
      (expect tangld-log-file-name :to-match "tangld\.log"))

    (it "By default checksums should be recorded in the data directory"
      (cond ((eq system-type 'windows-nt)
             (expect tangld-file-tracking-db
                     :to-match "\\\\AppData\\\\Local/tangld/\.tangld-db"))
            (t
             (expect tangld-file-tracking-db
                     :to-match "/\.local/share/tangld/\.tangld-db")))))

  (describe
      "Log messages have the proper text"
    (after-each (kill-tangld-log-buffer))

    (describe
        "The proper label is applied"
      (before-all
        ;; make sure logging is set to max level so that messages get published
        (setq tangld-log-level 4
              test-message "output"))
      (after-all
        (if (boundp tangld-log-level)
            (makunbound tangld-log-level))
        (if (boundp test-message)
            (makunbound test-message)))

      (it "Log level 1 is warn"
        (tangld-log-message 1 "This is a warning message")
        (expect (get-tangld-log-message) :to-match "^WARN: "))
      (it "Log level 2 is info"
        (tangld-log-message 2 "This is an info message")
        (expect (get-tangld-log-message) :to-match "^INFO: "))
      (it "Log level 3 is debug"
        (tangld-log-message 3 "This is a debug message")
        (expect (get-tangld-log-message) :to-match "^DEBUG: "))
      (it "Log level 4 is trace"
        (tangld-log-message 4 "This is a trace message")
        (expect (get-tangld-log-message) :to-match "^TRACE: ")))
    (describe
        "Format is applied to log messages"
      (it "Formats warning messages"
        (tangld-log-message 1 "should have %s" test-message)
        (expect (get-tangld-log-message) :to-match "should have output\n"))
      (it "Formats info messages"
        (tangld-log-message 2 "should have %s" test-message)
        (expect (get-tangld-log-message) :to-match "should have output\n"))
      (it "Formats debug messages"
        (tangld-log-message 3 "should have %s" test-message)
        (expect (get-tangld-log-message) :to-match "should have output\n"))
      (it "Formats trace messages"
        (tangld-log-message 4 "should have %s" test-message)
        (expect (get-tangld-log-message) :to-match "should have output\n"))
      )
    )
  (describe
      "Logging is skipped when logging level is set"
    (describe
        "To TRACE"
      (before-all (setq tangld-log-level 4))
      (after-each (kill-tangld-log-buffer))
      (it "Does log TRACE messages"
        (tangld-log-message 4 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      (it "Does log DEBUG messages"
        (tangld-log-message 3 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      (it "Does log INFO messages"
        (tangld-log-message 2 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      (it "Does log WARN messages"
        (tangld-log-message 1 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      )
    (describe
        "To DEBUG"
      (before-all (setq tangld-log-level 3))
      (after-each (kill-tangld-log-buffer))
      (it "Does not log TRACE messages"
        (tangld-log-message 4 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil))
      (it "Does log DEBUG messages"
        (tangld-log-message 3 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      (it "Does log INFO messages"
        (tangld-log-message 2 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      (it "Does log WARN messages"
        (tangld-log-message 1 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      )
    (describe
        "To INFO"
      (before-all (setq tangld-log-level 2))
      (after-each (kill-tangld-log-buffer))
      (it "Does not log TRACE messages"
        (tangld-log-message 4 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil))
      (it "Does not log DEBUG messages"
        (tangld-log-message 3 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name)  :to-be nil))
      (it "Does log INFO messages"
        (tangld-log-message 2 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      (it "Does log WARN messages"
        (tangld-log-message 1 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      )
    (describe
        "To WARN"
      (before-all (setq tangld-log-level 1))
      (after-each (kill-tangld-log-buffer))
      (it "Does not log TRACE messages"
        (tangld-log-message 4 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil))
      (it "Does not log DEBUG messages"
        (tangld-log-message 3 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil))
      (it "Does not log INFO messages"
        (tangld-log-message 2 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil))
      (it "Does log WARN messages"
        (tangld-log-message 1 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :not :to-be nil))
      )
    (describe
        "To SILENT"
      (before-all (setq tangld-log-level 0))
      (after-each (kill-tangld-log-buffer))
      (it "Does not log TRACE messages"
        (tangld-log-message 4 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil))
      (it "Does not log DEBUG messages"
        (tangld-log-message 3 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil))
      (it "Does not log INFO messages"
        (tangld-log-message 2 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil))
      (it "Does not log WARN messages"
        (tangld-log-message 1 "this would be logged")
        (expect (get-buffer tangld-log-buffer-name) :to-be nil)))))
