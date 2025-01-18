;; -*- lexical-binding: t; -*-

(require 'tangld)

;; helper functions

(defun get-tangld-db-entry ()
  (with-temp-buffer
    (insert-file-contents tangld-file-tracking-db)
    (goto-char (point-min))
    (thing-at-point 'line t)))

(defun delete-tangld-db ()
  (when (file-exists-p tangld-file-tracking-db)
    (delete-file tangld-file-tracking-db)))

;; TODO: make this programmatic
(defconst tangld-testing-dir "~/projects/tangld/tmp")

(defun create-tangld-test-file (fname content)
  "Create FNAME in the testing-dir with CONTENT"
  (let ((path (file-name-concat tangld-testing-dir fname)))
    (if (file-exists-p path)
        (delete-file path))
    (with-temp-file path
      (insert content))))


(describe
 "tangld checksums"
 (describe
  "Default values"
  (it "Tracking database is in tangld-data-dir"
      (expect tangld-file-tracking-db :to-match "\\\\AppData\\\\Local/tangld/\.tangld-db")))
 (describe
  "Create valid checksums"
  (it "Stores the checksum to the `tangld-checksums' table"
      (let* ((test-file (file-name-concat tangld-testing-dir "tangld.el"))
             (checksum (tangld--file-checksum-compute test-file)))
        (tangld--file-checksum-put test-file checksum)
        (expect tangld-checksums :not :to-be nil))))
 (describe
  "Read and write checksums to file"
  (before-each (delete-tangld-db))
  (it "Writes the checksums to the file"
      (let ((test-file (file-name-concat tangld-testing-dir "tangld.el")))
        (tangld--file-checksum-put test-file)
        (tangld--checksums-save)
        (expect (file-exists-p tangld-file-tracking-db))))
  (it "Reads the checksums from the file"
      (expect (file-exists-p tangld-file-tracking-db) :to-be nil)
      (let ((test-file (file-name-concat tangld-testing-dir "tangld.el")))
        (tangld--file-checksum-put test-file))
      (tangld--checksums-save)
      (tangld--checksums-init t) ; This will "reset" the in memory table
      (tangld--checksums-load)
      (expect tangld-checksums :not :to-be nil)
      (expect (hash-table-p tangld-checksums) :to-be t)
      (expect (hash-table-empty-p tangld-checksums) :to-be nil))
  )
 )
