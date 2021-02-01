;;; tangld-install.el --- literate config development environment -*- lexical-binding: t; -*-
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

;;; tangld-install.el ends here
