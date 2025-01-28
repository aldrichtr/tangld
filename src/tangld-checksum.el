;;; tangld-checksum.el -- allow tangld to test files for changes -*- lexical-binding: t -*-

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

;; In order to avoid tangling files that have not changed since the last time,
;; tangld tracks each file based on the computed checksum (the SHA1 hash of the
;; contents of the file).
;;
;; When `tangld-build' starts, it initializes the checksum database, and loads

;;; Code:

;; TODO: Update hash functions to ht.el

(defvar tangld-data-dir) ; Set in tangld-core

(declare-function tangld-log-message "tangld-log")

(defcustom tangld-file-tracking-db
  (file-name-concat tangld-data-dir ".tangld-db")
  "File to store database of files and checksums at last tangle."
  :group 'tangld
  :type 'file)

;; Hashtable of full-path -> checksum (SHA1 hash of file content)
(defvar tangld-checksums '()
  "Variable to store current file tracking database.")

;; -----------------------------------------------------------------------------
;; tangld--file* functions perform operations on individual files.
;; compute the checksum, add it to the database, and retrieve it from database.

(defun tangld--file-checksum-compute (path)
  "Compute the checksum of the file at PATH."
  (save-mark-and-excursion
    (let ((chksum (secure-hash 'sha1 (find-file-existing path))))
      (tangld-log-message 4 "the sha1 is %s for %s" chksum path)
      chksum)))

(defun tangld--file-checksum-get (path)
  "Get a checksum from the database.
PATH is the file to lookup in the database."
  (if (or (null tangld-checksums) (not (hash-table-p tangld-checksums)))
      (tangld--checksums-init))
  (gethash path tangld-checksums))

(defun tangld--file-checksum-put (path &optional chksum)
  "Add or update a checksum to the database.
Argument PATH Add the checksum and path to the database.
Optional argument CHKSUM Checksum of PATH."
  (if (or (null tangld-checksums) (not (hash-table-p tangld-checksums)))
      (tangld--checksums-init))
  (let ((chksum (or chksum (tangld--file-checksum-compute path))))
    (puthash path chksum tangld-checksums)))

;; -----------------------------------------------------------------------------
;; tangld--checksums* functions perform operations on the checksum database
;; load and store the database to disk

;; Elisp does not have a convenient way to serialize/deserialize a Hashtable the
;; way that it can an alist
;; TODO: consider serializing to json?
(defun tangld--checksum-to-alist (hash)
  "Turn the HASH into an alist for writing to a file"
  (let (newlist item)
    (maphash
     (lambda (key value)
       (if (setq item (assoc value newlist))
           (setcdr item (cons key (cdr item)))
         (push (list value key) newlist)))
     hash) newlist))

(defun tangld--checksum-to-hash (list)
  "Turn an alist LIST back into a hashtable"
  (let ((newtable (make-hash-table
                   :test 'equal
                   :weakness nil
                   :size (apply '+ (mapcar 'length list))))
        f)
    (mapc (lambda (item)
            (setq f (car item))
            (mapc (lambda (i) (puthash i f newtable)) (cdr item)))
          list)
    newtable))

(defun tangld--checksums-load (&optional path)
  "Load the database of checksums from PATH.
Read in from `tangld-file-tracking-db' to the variable `tangld-checksums'."
  (let ((path (or path tangld-file-tracking-db)))
    (save-mark-and-excursion
      (with-temp-buffer
        (insert-file-contents path)
        (setq contents (read (current-buffer)))
        (setq tangld-checksums (tangld--checksum-to-hash contents))))))

(defun tangld--checksums-save (&optional path)
  "Store the data to PATH or `tangld-file-checksums-db'."
  (let* ((path (or path tangld-file-tracking-db))
         (dir (file-name-directory path)))
    (save-mark-and-excursion
      (if (not (file-exists-p dir))
          (progn
            (tangld-log-message 4 "Creating directory %s" dir)
            (make-directory dir t)))
      (condition-case nil
          (with-temp-file path
            (let ((print-level nil)
                  (print-length nil))
              (print (tangld--checksum-to-alist tangld-checksums) (current-buffer))))
        (error
         (message
          "tangld: could not write checksums to %s" path))))))


(defun tangld--checksums-init (&optional force)
  "Initialize the tangld tracking database in memory.
If the table already exists, it will not wipe it unless FORCE is non-nil"
  (if (or (null tangld-checksums) force)
      (setq tangld-checksums
            (make-hash-table
             :test 'equal
             :weakness nil
             :size 30))))


;; -----------------------------------------------------------------------------

(defun tangld-file-changed-p (path)
  "Return non-nil if the file has changed since the checksum was last computed.
Argument PATH Path to the file to be tangled."
  (interactive)
  (let ((new-chksum (tangld--file-checksum-compute path))
        (old-chksum (tangld--file-checksum-get path)))
    ;; if either the old and new checksum are different, or there is
    ;; no old checksum, then the file has changed since last time.
    (or (not (string-equal new-chksum old-chksum))
        (equal nil old-chksum))))


(provide 'tangld-checksum)

;;; tangld-checksum.el ends here
