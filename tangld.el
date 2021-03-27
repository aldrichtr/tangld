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

(require 'f)

(require 'tangld-utils)
(require 'tangld-init)
(require 'tangld-install)
(require 'tangld-build)
(require 'tangld-clean)
(require 'tangld-link)

;;;; Global settings

(defgroup tangld nil
  "Literate Config Manager"
  :prefix "tangld-")

(defcustom tangld-project-dirs
  '((root    . ".tangld")
    (lib     . "lib")
    (build   . "build")
    (source  . "src")
    (install . "dotfiles")
    (system  . "~"))
  "A list of default project directory names"
  :group 'tangld
  :type '(alist :value-type (group string)))

(defcustom tangld-verbose-p t
  "Whether tangled should display many messages."
  :group 'tangld
  :type 'boolean)

(defcustom tangld-process-type 'basic
  "How tangld should build and install org files from the source directory."
  :group 'tangld
  :type 'symbol)

(provide 'tangld)
