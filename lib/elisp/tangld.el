;;; tangld.el --- literate config development environment -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Timothy Aldrich
;; Author: Timothy Aldrich <timothy.r.aldrich@gmail.com>
;; URL: https://github.com/aldrichtr/tangld
;; Version: 0.0.1
;; Keywords: org-mode, IDE, Tools
;; Package-Requires: ((org))

;;; Commentary:
;; A Literate Programming Environment for configuration files and scripts

;; tangld is an Emacs package that provides 'dotfiles management' features
;; using Literate Programming paradigms.  Using org-mode files with source
;; blocks and the tangle functionality, Emacs can be used as an IDE to
;; document, build, and install configuration files, scripts and other
;; files on a system.  More details are available in the README.org file.

;;; Code:

(defgroup tangld nil
  "Literate Config manager"
  )
(defcustom tangld-confirm-on-eval nil
  "If non-nil, emacs will ask before evaluating code in source blocks"
  :group 'tangld
  :type '(choice
          (const :tag "Not" nil)
          (const :tag "Globally" t)))

(defun tangld-init ()
  "Setup a new tangld project"
  )

(defun tangld-config ()
  "Configure tangld.  Set the source and target directories, Gather system
   information, and store for the build step to use."
  )

(defun tangld-build ()
  "For each changed source file, tangle it to the build directory"
  )

(defun tangld-install ()
  "Organize target directories, files and libraries on this system.  The build
   step tangles org files into their source, the install step moves them to their
   target location."
  )

(defun tangld-clean ()
  "Remove any files or settings created by the build phase"
  )

(defun tangld-check ()
  "Run tests"
  )
;;; tangld.el ends here
