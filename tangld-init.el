;;; tangld-init.el --- literate config development environment -*- lexical-binding: t; -*-
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

(defcustom tangld-inhibit-init-if-exists t
  "Do not overwrite and existing project with tangld-init"
  :group 'tangld
  :type 'boolean)

(defcustom tangld-init-vc-on-init-p t
  "Non-nil means a version control system is initialized when starting a new tangld project."
  :group 'tangld
  :type 'boolean)

(defun tangld--init-vc (&optional vc-root-dir)
  "Initialize the project using magit."
  (tangld-with-dirs
   (or vc-root-dir (setq vc-root-dir .root))
   (tangld--message "initializing git repo in %s" vc-root-dir)
   (if (featurep 'magit)
       (magit-call-git "init" (magit-convert-filename-for-git (expand-file-name vc-root-dir)))
     (message "Magit package not found"))))

;;;###autoload
(defun tangld-init ()
  "Setup a new tangld project"
  (interactive)

  (tangld-with-dirs
   (cond ((not (f-exists-p .root)) nil)
	 (tangld-inhibit-init-if-exists
	  (error (format "Aborted init in %s" .root)))
	 ((y-or-n-p (format "WARNING: this will overwrite your project in %s continue?" .root))
	  (f-delete .root t))
	 (t
	  (tangld--message "Overwriting %S" .root)))

   ;; either it's a new directory or the old one was deleted
   (tangld-message "Creating directories in %s" .root)
   (let ((dirs (mapcar #'tangld-dir (mapcar #'car tangld-project-dirs))))
     (dolist (dir dirs)
       (mkdir dir t)))

   ;; initialize the version control (git init)
   (when tangld-init-vc-on-init (tangld-init-vc .root))
   (tangld-message "Initialized new tangld project in %s" .root)))

(provide 'tangld-init)
