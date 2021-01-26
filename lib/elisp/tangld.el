;;; tangld.el --- literate config development environment -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Timothy Aldrich

;; Author: Timothy Aldrich <timothy.r.aldrich@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((org) (notifications) (f) (s))
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


(require 'f)


(defgroup tangld nil "Literate Config manager")

(defcustom tangld-project-dirs
  '((root    . "~/.tangld")
    (lib     . "lib")
    (build   . "build")
    (source  . "src")
    (install . "dotfiles")
    (system  . "~"))
  "A list of default project directory names"
  :group 'tangld
  :type '(alist :value-type (group string )))

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

(defcustom tangld-inhibit-init-if-exists t
  "Do not overwrite and existing project with tangld-init"
  :group 'tangld
  :type 'boolean)

(defcustom tangld-init-vc-on-init t
   "Initialize a version control system when initializing a new tangld project"
   :group 'tangld
   :type 'boolean)

(defun tangld-init ()
  "Setup a new tangld project"
  (interactive)
    (let-alist tangld-project-dirs
      (catch 'do-not-overwrite
        ;; It should be really hard to overwrite an existing project
        ;; so, check the inhibit variable AND ask for conformation
        (if (f-exists? ( format "%s" .root))
            (if tangld-inhibit-init-if-exists
                (if (y-or-n-p
                     (format
                      "WARNING: this will overwrite your project in %s continue?"
                      .root))
                    (f-delete .root t)) ;; they said its ok, delete it
              (throw 'do-not-overwrite (format "Aborted init in %s" .root))))) ;; bail on init

      ;; either it's a new directory or the old one was deleted
      (message "creating directories in %s" .root)
      (mapc 'f-mkdir (list
      .root
      (f-join .root .lib)
      (f-join .root .source)
      (f-join .root .build)
      (f-join .root .install)))

      ;; initialize the version control (git init)
      (if tangld-init-vc-on-init
          (tangld-init--init-vc .root))
      (message "initialized new tangld project in %s" .root)))



(defun tangld-init--init-vc (&optional vc-root-dir)
  "uses magit to initialize the project"
  (let-alist tangld-project-dirs
    (or vc-root-dir (setq vc-root-dir .root))
    (message "initializing git repo in %s" vc-root-dir)
    (if (featurep 'magit)
        (magit-init vc-root-dir)
      (message "Magit package not found"))))

(defun tangld-config (&optional type)
  "Configure tangld.  Set the source and target directories, Gather system
   information, and store for the build step to use.  If TYPE is specified
   store as options for a specific build type i.e. OS specific, shell options
   alternate install directory, etc. "
  (interactive)
  ;; The user sets options such as cache use, source and target dirs, etc.
  ;; write those to a project specific configuration location so that build
  ;; can use them as it's config when run.
  )

(defun tangld-build ()
  "Tangle org-mode files in the source dir.  By default, build will only tangle
   files that have changed since last run."
  (interactive)
  ;; - read the config for options pertaining to this build
  ;; - run the pre-build hooks if any
  ;; - load the library-of-babel.
  ;;   - if the user says the cache can be used and there is one
  ;;     - load the cache file.
  ;;   - otherwise
  ;;     - load the library with our org-lib files
  ;; - if caching is enabled, store our library now
  ;; - if there is a db of file mod dates
  ;;   - load it now
  ;; - for each file in the src directory
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

(defun tangld-install ()
  "Organize target directories, files and libraries on this system.  The build
   step tangles org files into their source, the install step moves them to their
   target location."
  (interactive)
  ;; read the config for options pertaining to the install
  ;; for each file in the build directory
  ;;   identify the install type for this file
  ;;   if
  )

(defun tangld-clean ()
  "Remove any files or settings created by the build phase"
  (interactive)
  )

(defun tangld-check ()
  "Run tests"
  (interactive)
  )

;;; tangld.el ends here
