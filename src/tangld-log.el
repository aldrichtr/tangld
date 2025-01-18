;;; tangld-log.el --- Logging for the tangld system

;;; License:

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; tangld sends logging information to a buffer.  Optionally, the information
;; can also be written to a file.

;;; Code:

;; -- Customizable options {{{

(defcustom tangld-log-level 2
  "Logging level provided by tangld functions.
0 = silent no output
1 = warnings only
2 = informational
3 = verbose
4 = debugging"
  :group 'tangld
  :type 'integer)

(defcustom tangld-log-buffer-name "*tangld*"
  "Default buffer name where tangld logs messages."
  :group 'tangld
  :type 'string)

(defcustom tangld-log-to-file nil
  "Write the log information to a file."
  :group 'tangld
  :type '(choice (const :tag "Write log to file" t)
                 (const :tag "Log to buffer only" nil)))

(defcustom tangld-log-dir (file-name-concat tangld-data-dir "log")
  "The directory where log files are written.
Option has no effect if `tangld-log-to-file' is nil")

(defcustom tangld-log-file-name "tangld.log"
  "The name of the file to write the log to.  Any date tokens will be
converted according to `format-time-string'"
  :group 'tangld
  :type 'string)

;; -- }}}

(defconst tangld--log-levels '(( 0 . "SILENT")
                               ( 1 . "WARN")
                               ( 2 . "INFO")
                               ( 3 . "DEBUG")
                               ( 4 . "TRACE")))


(defun tangld-log--normalize-log-file-name ()
  "Create a valid file name from the `tangld-log-file-name' value."
  ;; TODO: get the date portions and convert them, then make sure it is a valid filename.
  )

(defun tangld-log-message (lvl msg &rest args)
  "Log messages to the tangld log buffer.
Argument LVL Log Level.
Argument MSG The message to write to the log.
Optional argument ARGS Arguments that are used to replace format strings in MSG."
  (let* ((level-name (or (cdr (assoc lvl tangld--log-levels)) "NONE"))
         (log-msg (concat level-name ": " (apply #'format msg args) "\n")))
    (if (<= lvl tangld-log-level)
        (with-current-buffer (get-buffer-create tangld-log-buffer-name)
          (goto-char (point-max))
          (insert log-msg )))))

(provide 'tangld-log)

;;; tangld-log.el ends here
