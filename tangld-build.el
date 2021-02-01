;;; tangld-build.el --- literate config development environment -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Timothy Aldrich

;; Author: Timothy Aldrich <timothy.r.aldrich@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1")(org "9.1")(cl-lib "0.5"))
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

(defcustom tangld-pre-build-hook nil
  "Hook run prior to building org-source files.  This hook runs prior to loading
the library (or cache), and before the list of sources is built. Also see:
 - `org-babel-pre-tangle-hook'  :: before each file is tangled
 - `org-babel-post-tangle-hook' :: after each file is tangled
 - `org-babel-tangle-body-hook' :: over the body of each source block"
  :group 'tangld
  :type 'hook)

(defcustom tangld-post-build-hook nil
  "Hook run after all org-source files have been tangled."
  :group 'tangld
  :type 'hook)

(defcustom tangld-build-file-glob-pattern "\\.org"
  "Regular expression used to gather files to be tangled.  Combine regular
expressions in `tangld-build-file-glob-pattern' ,
`tangld-build-file-include-regex', and `tangld-build-file-exclude-regex' to
provide tangld-build fine-grained control over which files are tangld"
  :group 'tangld
  :type 'string)

(defcustom tangld-build-file-include-regex "\\.org$"
  "Regular expression used to match org-source file names that should be
tangled.  By default any file that ends in '.org' is included"
  :group 'tangld
  :type 'string)

(defcustom tangld-build-file-exclude-regex "^-"
  "Regular expression used to match  org-source file names that should be
skipped.  By default any file that starts with a '-' is skipped"
  :group 'tangld
  :type 'string)

(defcustom tangld-build-source-dir-exclude-regex "^-"
  "Regular expression used to match org-source directory names that should be
skipped.  By default any directory that starts with a '-' is skipped"
  :group 'tangld
  :type 'string)



(defun tangld-build ()
  "Tangle org-mode files in the source dir.  By default, build will only tangle
   files that have changed since last run."
  (interactive)
  ;; - read the config for options pertaining to this build
  ;; - run the pre-build hooks if any
  (run-hooks 'tangld-pre-build-hook)
  ;; - load the library-of-babel.
  ;;   - if the user says the cache can be used and there is one
  ;;     - load the cache file.
  ;;   - otherwise
  ;;     - load the library with our org-lib files
  ;; - if caching is enabled, store our library now
  ;; - if there is a db of file mod dates
  ;;   - load it now
  ;; - for each file in the src directory
  (let-alist tangld-project-dirs
    (cl-loop for source-file
             in (tangld-build-files (f-join .root .source))
             do
             (progn
               (message "tangling file : %s" source-file)
               (org-babel-tangle-file source-file))))
  ;;   - if the mod date matches the db entry
  ;;     and 'force' is not set
  ;;     -skip
  ;;   - otherwise
  ;;     - consult tangld-install-type setting
  ;;       - stage :: write to build-root.
  ;;         tangld-install will ignore 'stage' but will honor anything else
  ;;       - nil :: write to install-root.
  ;;       - link :: write to install-root.  link system dir/file to it.
  ;;       - stow :: write to install-root.  Call stow with the appropriate options
  ;;       - direct :: write to the system dir/file specified (destructive?)
  ;;   - record the mod date in the db
  ;; run the post-build hooks if any
  )

(defun tangld-build-file-filter (source-file)
  "Filtering function applied to files found in source directories
SOURCE-FILE is a filename and extension"
  (message "passing %s to the filter" source-file)
  (let-alist tangld-project-dirs
    ;; remove the source-directory part of the path so that we are 'rooted'
    ;; in source
    (setq src-path (replace-regexp-in-string (f-join .root .source) "" source-file))
    ;; grab the filename portion
    (setq src-file (f-filename source-file))
    (message "the source root is : %s" src-path)
    (message "the filename is : %s" src-file)
    (and
     (not (string-match tangld-build-source-dir-exclude-regex src-path))
     (not (string-match tangld-build-file-exclude-regex src-file))
     (string-match tangld-build-file-include-regex src-file)
    )))

(defun tangld-build-files (&optional build-dir)
  "Collect all of the org-source files in BUILD-DIR, applying the filters:
 - `tangld-build-file-glob-pattern'  : files matched are added to the list
 - `tangld-build-file-include-regex' : filenames matched are left in the list
 - `tangld-build-file-exclude-regex' : filenames matched are removed"
  (let-alist tangld-project-dirs
    (or build-dir (setq build-dir (f-join .root .build)))
    (seq-filter
     #'tangld-build-file-filter
     (directory-files-recursively build-dir tangld-build-file-glob-pattern ))))

;;; tangld-build.el ends here
