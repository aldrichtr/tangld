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
(defconst tangld-testing-dir (getenv "TEMP"))

(defun create-tangld-test-file (fname content)
  "Create FNAME in the testing-dir with CONTENT"
  (let ((path (file-name-concat tangld-testing-dir fname)))
    (if (file-exists-p path)
        (delete-file path))
    (with-temp-file path
      (print content))))


(describe
 "tangld checksums"
 (describe
  "Default values"
  (before-all
   (let* ((fname "test.el"))
     (setq tangld-test-file (file-name-concat tangld-testing-dir fname))
     (create-tangld-test-file
      fname
      "The quick brown fox jumped over the lazy dog")))
  (it "Tracking database is in tangld-data-dir"
    (cond ((eq system-type 'windows-nt)
           (expect tangld-file-tracking-db
                   :to-match "\\\\AppData\\\\Local/tangld/\.tangld-db"))
          (t
           (expect tangld-file-tracking-db
                   :to-match ".*/\.local/share/tangld/\.tangld-db")))))

 (describe
  "Create valid checksums"
  (it "Stores the checksum to the `tangld-checksums' table"
      (let* ((checksum (tangld--file-checksum-compute tangld-test-file)))
        (tangld--file-checksum-put tangld-test-file checksum)
        (expect tangld-checksums :not :to-be nil))))

 (describe
  "Read and write checksums to file"
  (before-each (delete-tangld-db))
  (it "Writes the checksums to the file"
      (tangld--file-checksum-put tangld-test-file)
      (tangld--checksums-save)
      (expect (file-exists-p tangld-file-tracking-db)))
  (it "Reads the checksums from the file"
      (tangld--file-checksum-put tangld-test-file)
      (tangld--checksums-save)   ; store the checksums to disk
      (tangld--checksums-init t) ; This will "reset" the in memory table
      (tangld--checksums-load)   ; re-read from disk
      (expect tangld-checksums :not :to-be nil)
      (expect (hash-table-p tangld-checksums) :to-be t)
      (expect (hash-table-empty-p tangld-checksums) :to-be nil))
  )
 )
