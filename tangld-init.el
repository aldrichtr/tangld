;;; tangld-init.el --- literate config development environment -*- lexical-binding: t; -*-
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

;;;; tangld-init settings
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

    ;; add the lib directory to the list of babel libraries
    (if tangld-add-project-lib-dir-on-init
        (add-to-list 'tangld-babel-library-dirs (f.join .root .lib)))

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

;;; tangld-init.el ends here
