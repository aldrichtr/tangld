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

(require 'ob-tangle)
(require 'ob-extended-tangle)
(require 'ob-load-namespaced-libraries)
(require 'ob-text-var-expansion)
(require 'ob-var-table)

;;;; Constants

(defconst tangld--load-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that yasnippet was loaded from.")

(defconst tangld--installed-lib-dir
  (expand-file-name "lib" tangld--load-dir))

(defconst tangld--default-user-lib-dir
  (expand-file-name "tangld-lib" user-emacs-directory))

;;;; Global settings

(defgroup tangld nil
  "Literate Config Manager"
  :prefix "tangld-")

(defcustom tangld-babel-library-dirs (list tangld--default-user-lib-dir)
  "List of top-level tangld library directories.

Each element, a string or a symbol whose value is a string,
designates a top-level directory where org-mode files can be found.

Elements appearing earlier in the list override later elements"
  :group 'tangld
  :type '(choice (directory :tag "Single directory")
                 (repeat :tag "List of directories"
                         (choice (directory) (variable)))))

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

(defcustom tangld-add-project-lib-dir-on-init t
  "Add the lib directory of the project to the tangld-babel-library-dirs
during init"
  :group 'tangld
  :type 'boolean)

(defcustom tangld-init-vc-on-init t
  "Initialize a version control system when initializing a new tangld project"
  :group 'tangld
  :type 'boolean)

(defcustom tangld-cache-p nil
  "Non-nil if tangled should cache."
  :group 'tangld
  :type 'boolean)

(defcustom tangld-cache-org-babel-library-p t
  "Non-nil if should cache."
  :group 'tangld
  :type boolean)

(defcustom tangld-use-cached-library-p t
  :group tangld
  :type 'boolean)

(defcustom tangld-config-file "config.org"
  "File where config details are written."
  :group 'tangld
  :type 'string)

(defcustom tangld-prebuild-hook nil
  "Hook run before `tangld-build' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-postbuild-hook nil
  "Hook run after `tangld-build' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-preinstall-hook nil
  :group 'tangld
  :type 'hook
  "Hook run before `tangld-install' is called.")

(defcustom tangld-postinstall-hook nil
  :group 'tangld
  :type 'hook
  "Hook run after `tangld-install' is called.")

(defcustom tangld-verbose-p nil
  :group 'tangld
  :type 'boolean
  "Whether tangled should display many messages.")

;;;; Helpers

;; These helpers are to help me develop this project quickly.

(defmacro tangld--ignore (&rest _)
  "Do nothing."
  nil)

(defun tangld--message (format-string &rest args)
  "Display message if `tangld-verbose-p' is non-nil."
  (when tangld-verbose-p (message message args)))

(defun tangld--tangle-file (file)
  "Tangle FILE without any messages or confirmation."
  (let ((org-confirm-babel-evaluate nil)
	(message-log-max nil)
	(inhibit-message t))
    (org-babel-tangle-file (expand-file-name file .source))))

;;;; Initialization - tangld-init

(defun tangld-init ()
  "Setup a new tangld project"
  (interactive)
  (let-alist tangld-project-dirs
    (catch 'do-not-overwrite
      ;; It should be really hard to overwrite an existing project
      ;; so, check the inhibit variable AND ask for conformation
      (when (f-exists? (format "%s" .root))
        (if tangld-inhibit-init-if-exists
            (if (y-or-n-p
                 (format
                  "WARNING: this will overwrite your project in %s continue?"
                  .root))
                (f-delete .root t)) ;; they said its ok, delete it
          (throw 'do-not-overwrite (format "Aborted init in %s" .root)))))

    ;; either it's a new directory or the old one was deleted
    (tangld--message "creating directories in %s" .root)
    (mapc 'f-mkdir (list
                    .root
                    (f-join .root .lib)
                    (f-join .root .source)
                    (f-join .root .build)
                    (f-join .root .install)))

    ;; add the lib directory to the list of babel libraries
    (when tangld-add-project-lib-dir-on-init
      (add-to-list 'tangld-babel-library-dirs (f-join .root .lib)))

    ;; initialize the version control (git init)
    (when tangld-init-vc-on-init
      (tangld-init--init-vc .root))
    (tangld--message "initialized new tangld project in %s" .root)))

(defun tangld-init--init-vc (&optional vc-root-dir)
  "Initialize the project using magit."
  (let-alist tangld-project-dirs
    (or vc-root-dir (setq vc-root-dir .root))
    (tangld--message "initializing git repo in %s" vc-root-dir)
    (if (featurep 'magit)
	(magit-call-git "init" (magit-convert-filename-for-git (expand-file-name vc-root-dir)))
      (message "Magit package not found"))))

;;;; Configuration - tangld-config

(defun tangld-config (&optional type)
  "Configure tangld. 

Set the source and target directories, Gather system information, and store for
the build step to use. If TYPE is specified store as options for a specific
build type i.e. OS specific, shell options alternate install directory, etc."
  (interactive)
  ;; The user sets options such as cache use, source and target dirs, etc.
  ;; write those to a project specific configuration location so that build
  ;; can use them as it's config when run.

  ;; environmental details
  ;; (list system-type system-name user-full-name)
  (tangld--message "environment details: %s %s %s" system-type system-name user-full-name)

  ;; tangld options
  (tangld--message "tangled-options: %s %s %s" "uh" "don't" "know?")
  ;; language, tangle options, exclusions, overrides,

  ;; build-options
  ;; staging area, copy, link, move, symlink manager
  (tangld--message "staging area: %s, " "don't know")

  ;; write-config
  (let-alist tangld-project-dirs
    (let ((config-file (f-join .root .lib tangld-config-file)))
      (tangld--ignore (f-write config-file))
      (tangld--message "Build options saved to '%s'" config-file))))

;;;; Build - tangld-build

(defun tangld-build (&optional force)
  "Tangle org-mode files in the source dir.

By default, build will only tangle files that have changed since last run."
  (interactive "P")
  ;; - read the config for options pertaining to this build
  (tangld--message "Read config options...")
  ;; config options
  ;; - run the pre-build hooks if any
  (tangld--message "Run `tangld-prebuild-hooks'.")
  (run-hooks 'tangld-prebuild-hooks)
  ;; - load the library-of-babel.
  (tangld--message "Load library of babel...")
  ;; Note this is not org babel, these are tangled source blocks used for
  ;; side-effect and other things.
  ;;   - if the user says the cache can be used and there is one
  ;;     - load the cache file.
  ;;   - otherwise
  ;;     - load the library with our org-lib files

  ;; - if caching is enabled, store our library now
  ;; store our library
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
  (let-alist tangld-project-dirs
    (unless (cddr (directory-files (f-join .root .source)))
      (tangld--message "No files in source dir."))

    (dolist (file (directory-files-recursively (f-join .root .source) "."))
      (let ((mod-date (file-attribute-modification-time))
	    (db-entry 'not-implemented))
	(tangld--message "Date file modified: %S" mod-date)
	(when (or force t)
	  (cl-case tangled-install-type
	    (stage
	     (tangld--message "stage - write to build-root."))
	    (link
	     (tangld--message "link - write %s to install-root..." file)
	     (tangld--message "link - make a symlink from %s to %s" from to)
	     (tangld--ignore (f-symlink file to)))
	    (stow
	     (tangld--message "stow - write %s to install-root..." file)
	     (tangld--message "stow - make symlink from %s to %s with stow" from to)
	     (tangld--ignore (f-symlink file to)))
	    (direct
	     (tangld--message "direct - write to system dir/file specified")
	     (tangld--ignore (tangld--tangle-file file)))
	    (nil
	     (tangld--message "write to install-root"))
	    (t
	     (error "Unknown link type '%S'" type))))
	(tangld-message "Record mod date %S in the db"))))
  
  ;; run the post-build hooks if any
  (run-hooks 'tangld-postbuild-hooks))

;;;; Install - tangld-install

(defun tangld-install ()
  "Organize target directories, files and libraries on this system. 

The build step tangles org files into their source, the install step moves them
to their target location."
  (interactive)
  (run-hooks 'tangld-pre-install-hook)
  ;; read the config for options pertaining to the install
  ;; for each file in the build directory
  ;;   identify the install type for this file
  ;;   if
  (let (install-type)
    (dolist (file (cddr (directory-files)))
      (org-babel-tangle-file file)))
  (run-hooks 'tangld-post-install-hook))

;;;; Clean - tangld-clean

(defun tangld-clean ()
  "Remove any files or settings created by the build phase."
  (interactive)
  (tangld--message "Remove the cache")
  (tangld--message "Remove the db"))

;;;; Check - tangld-check

(defun tangld-check ()
  "Run tests"
  (interactive))

;;; tangld.el ends here
