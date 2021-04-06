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
(require 'async)

(require 'ob-tangle)
(require 'ob-extended-tangle)
(require 'ob-load-namespaced-libraries)
(require 'ob-text-var-expansion)
(require 'ob-var-table)

(require 'tangld-utils)
(require 'tangld-init)
(require 'tangld-install)
(require 'tangld-build)
(require 'tangld-clean)

;;;; Global settings

(defgroup tangld nil
  "Literate Config Manager"
  :prefix "tangld-")

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

(defcustom tangld-inhibit-init-if-exists t
  "Do not overwrite and existing project with tangld-init"
  :group 'tangld
  :type 'boolean)

(defcustom tangld-init-vc-on-init-p t
  "Non-nil means a version control system is initialized when starting a new tangld project."
  :group 'tangld
  :type 'boolean)

(defcustom tangld-verbose-p nil
  "Whether tangled should display many messages."
  :group 'tangld
  :type 'boolean)

(defcustom tangld-install-type 'link
  "How to."
  :group 'tangld
  :type 'symbol)

(defcustom tangld-lazy-tangle-p t
  "Only tangle when necessary.
That is, when the target file either does not exist or is older than the source file."
  :group 'tangld
  :type 'boolean)

(provide 'tangld)
