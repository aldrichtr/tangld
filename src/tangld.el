;;; tangld.el --- literate config development environment -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Timothy Aldrich

;; Author: Timothy Aldrich <timothy.r.aldrich@gmail.com>
;; Version: 0.0.1
;; Keywords: tools processes
;; URL: https://github.com/aldrichtr/tangld

;;; Commentary:
;; A Literate Programming Environment for configuration files and scripts

;; tangld is an Emacs package that provides 'dotfiles management' features
;; using Literate Programming paradigms.  Using org-mode files with source
;; blocks and the tangle functionality, Emacs can be used as an IDE to
;; document, build, and install configuration files, scripts and other
;; files on a system.  More details are available in the README.org file.

;;; Code:

; The actual org-babel-tangle library
(require 'ob-tangle)


;;;; Constants




;;;; Global settings

(defgroup tangld nil
  "Literate Config Manager"
  :prefix "tangld-")

(defcustom tangld-log-buffer-name "*tangld*"
  "Default buffer name where tangld logs messages"
  :group 'tangld
  :type 'string)

; There's probably a much better way to do this but,
; I'm wrapping the `tangld-log-message' calls with a test of
; `tangld-log-level' which is a value from 0 to 4
(defcustom tangld-log-level 4
  "Logging level provided by tangld functions.
   0 = silent no output
   1 = warnings only
   2 = informational
   3 = verbose
   4 = debugging"
  :group 'tangld
  :type 'integer)

(defcustom tangld-project-plist '()
  "A list of projects used when calling `tangld-build'"
  :group 'tangld
  :type '(plist :value-type (group string )))

(defcustom tangld-confirm-on-eval nil
  "If non-nil, emacs will ask before evaluating code in source blocks"
  :group 'tangld
  :type '(choice
          (const :tag "Don't ask" nil)
          (const :tag "Ask" t)))

(defcustom tangld-add-src-return-link-comments t
  "Add a link to the source code block in the output"
  :group 'tangld
  :type 'boolean)

(defcustom tangld-file-tracking-file "~/.tangld/.tangld-db"
  "File to store database of files and checksums at last tangle"
  :group 'tangld
  :type 'file)

(defvar tangld-file-tracking-db '()
  "Variable to store current file tracking database")


;;;; Logging

(defun tangld-log-message (lvl msg &rest args)
  "Log messages to the tangld log buffer"
  (let ((log-msg (apply #'format msg args)))
    (if (<= lvl tangld-log-level)
        (print log-msg (get-buffer-create tangld-log-buffer-name)))))

;;;; Checksums


(defun tangld-file-changed-p (path)
  "return non-nil if the file has changed since the checksum was last computed"
  (let ((new-chksum (tangld-compute-file-hash path))
        (old-chksum (tangld-file-last-checksum-get path)))
                                        ; if either the old and new checksum are different, or there is no old checksum,
                                        ; then the file has changed since last time.
    (or (not (string-equal new-chksum old-chksum))
        (equal nil old-chksum))))

(defun tangld-compute-file-hash (path)
  "compute the checksum of the file at PATH"
  (interactive)
  (let ((old-point (point))
        (old-buff (current-buffer))
        (chksum (secure-hash 'sha1 (find-file-existing path))))
    (tangld-log-message 4 "the sha1 is %s for %s" chksum path)
    (unless (eq (current-buffer) old-buff)
      (switch-to-buffer old-buff))
    (goto-char old-point)
    chksum))

(defun tangld-file-checksums-load ()
  "Read the data from `tangld-file-checksums-file'"
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents tangld-file-tracking-file)
          (setq tangld-file-tracking-db (read (current-buffer))))
      (error
       (message "tangld: Could not read file checksums from %s. Setting list to nil" tangld-file-tracking-file)))))

(defun tangld-file-checksums-save ()
  "Store the data to `tangld-file-checksums-file'"
  (with-temp-file tangld-file-tracking-file
    (let ((print-length nil)
          (print-level nil))
      (print tangld-file-tracking-db (current-buffer)))))

(defun tangld-file-last-checksum-get (path)
  "Get a checksum from the database"
  (gethash path tangld-file-tracking-db))

(defun tangld-file-last-checksum-put (path &optional chksum)
  "Add or update a checksum to the database"
  (let ((checksum (or chksum (tangld-compute-file-hash path))))
    (puthash path checksum tangld-file-tracking-db)))

(defun tangld-file-tracking-db-init ()
  (let ((db  (make-hash-table
              :test 'equal
              :weakness nil
              :size 30)))
    (setq tangld-file-tracking-db db)))



;;;; Projects

(defun tangld-project-generate (name doc src inc cfg)
  "Add a project definition to the list `tangld-projects-plist'.
NAME is a unique name given to the project.  This is used in other functions like 
`tangld-build-project' <name>
DOC is a short docstring description of the project
SRC is a list of files to be tangled (each of these will be processed by `org-babel-tangle'
INC is a list of files to be ingested into the library of babel prior to tangling `org-babel-lob-ingest'"
  (interactive)
      (tangld-log-message 2 "generate: %s %s" name doc)
      (tangld-log-message 2 "          - sources %s" src)
      ;; TODO right now I don't need to do anything but add the files in src and lib to a list
      ;; and provide some status.  Later, additional processing can be added here first, prior to
      ;; adding it to the projects list.
  (let ((sources (-map (lambda (f)
                             (tangld-log-message 3 "      '%s' is in list" f)
                         f)
                       src))
        (includes (-map (lambda (f)
                              (tangld-log-message 3 "      '%s' is in list" f)
                          f)
                        inc))
        ;; Also, the config settings don't need to be processed here right now, so I'm
        ;; just leaving this "place-holder" for future config processing prior to generating
        ;; the project 
        (config cfg))
        (tangld-log-message 2 "adding %s to projects-plist" name)
    (setq tangld-projects-plist (plist-put tangld-projects-plist name (list sources includes config)))))


(defmacro tangld-project-define (name &optional doc &rest project-plist)
  "define a tangld project.  NAME is a unique name for this project.  DOC is a short description
and PROJECT-PLIST is a plist of files or search criteria.  The properties of the plist are:

:source either a list of files like 

:source (\"fileA.org\" \"fileB.org\") 

or a path description:

:source (:path \"~/.tangld/src\"        ; root directory to traverse
                     :recurse t             ; look in subdirectories
                     :files \"\\.org$\"       ; files to add to source
                     :exclude \"^\\.\"        ; unless they match this
                     :exclude-dir \"^\\.\"    ; don't look in these subdirs
                     :include \".*default.*\" ; cancel the exclusion for these
                     )
:include same options as :source

:config add any customization options here, like:

:config (progn
    (setq tangld-clear-library-before-build-p t))"
  (declare (indent defun))
  (tangld-log-message 4 "define: project %s" name)
  (let ((source (plist-get project-plist :source))
        (include (plist-get project-plist :include))
        (config (plist-get project-plist :config))
        (source-files nil)
        (include-files nil))
    (cond ((listp source)
           (tangld-log-message 4 "        - source is a list %s" source)
           (if (plist-get source :path)
               (progn
                 (tangld-log-message 4 "          - A path spec is included. Pass them to tangld-build-find-files")
                 (setq source-files (tangld-build-find-files (plist-get source :path)
                                                             :recurse (plist-get source :recurse)
                                                             :files   (plist-get source :files)
                                                             :exclude (plist-get source :exclude)
                                                             :exclude-dir (plist-get source :exclude-dir)
                                                             :include (plist-get source :include)))
                 (tangld-log-message 4 "         - now source files are:")
                 (tangld-log-message 4 (string-join source-files "\n")))
             (progn
               (tangld-log-message 4 "           - source is a list of files %s" source)
               (tangld-log-message 4 "           - add them to the list of source-files : %s" source-files )
               (setq source-files source))))
          ((stringp source)
           (tangld-log-message 4 "           - sources is a string: '%s' if its a dir we'd expand" source))
          (t
           (tangld-log-message 4 "sources is something else '%s'" source))
          )
    (cond ((listp include)
           (tangld-log-message 4 "        - include is a list %s" include)
           (if (plist-get include :path)
               (progn
                 (tangld-log-message 4 "          - A path spec is included. Pass them to tangld-build-find-files")
                 (setq include-files (tangld-build-find-files (plist-get include :path)
                                                              :recurse (plist-get include :recurse)
                                                              :files   (plist-get include :files)
                                                              :exclude (plist-get include :exclude)
                                                              :exclude-dir (plist-get include :exclude-dir)
                                                              :include (plist-get include :include)))
                 (tangld-log-message 4 "         - now include files are:")
                 (tangld-log-message 4 (string-join include-files "\n")))
             (progn
               (tangld-log-message 4 "           - include is a list of files %s" include)
               (tangld-log-message 4 "           - add them to the list of include-files : %s" include-files )
               (setq include-files include))))
          ((stringp include)
           (tangld-log-message 4 "           - includes is a string: '%s' if its a dir we'd expand" include))
          (t
           (tangld-log-message 4 "includes is something else '%s'" include))
          )
    (tangld-log-message 4 "      calling generate now:")
    (tangld-log-message 4 (string-join source-files "\n"))
    `(tangld-project-generate ,name ,doc (quote ,source-files) (quote ,include-files) (quote ,config))))

;;; tangld.el ends here

