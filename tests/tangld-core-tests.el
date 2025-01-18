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
 "tangld core tests:"
 (describe
  "Default values for paths"
  (describe
   "The base paths"
   (it "The user-data-dir should be set via env"
       (cond ((eq system-type 'windows-nt)
              (expect user-data-dir :to-match "\\\\AppData\\\\Local"))
             (t
              (expect user-data-dir :to-match "/\.local/share"))))

   (it "The user-config-dir should be set via env"
       (cond ((eq system-type 'windows-nt)
              (expect user-config-dir :to-match "\\\\AppData\\\\Roaming"))
             (t
              (expect user-config-dir :to-match "/\.config")))))
  (describe
   "The tangld directories"
   (it "The tangld-data-dir should be in the user-data-dir"
       (cond ((eq system-type 'windows-nt)
              (expect tangld-data-dir :to-match "\\\\AppData\\\\Local/tangld"))
             (t
              (expect tangld-data-dir :to-match "/\.local/share/tangld"))))

   (it "The tangld-config-dir should be in the user-config-dir"
       (cond ((eq system-type 'windows-nt)
              (expect tangld-config-dir :to-match
                      "\\\\AppData\\\\Roaming/tangld"))
             (t
              (expect tangld-config-dir :to-match "/\.config/tangld")))))
  (describe
   "Flags"
   (it "Do not ask for confirmation before eval"
       (expect tangld-confirm-on-eval :to-be nil))
   (it "Add links to source code blocks"
       (expect tangld-add-src-return-link-comments :to-be t)))))
