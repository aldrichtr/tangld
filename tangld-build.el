;;; tangld-build.el --- literate config development environment -*- lexical-binding: t; -*-
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

(defcustom tangld-pre-build-hook nil
  "Hook run before `tangld-build' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-post-build-hook nil
  "Hook run after `tangld-build' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-build-fn #'tangld-build-fn
  "Function that specifies how a file will be built."
  :group 'tangld
  :type 'symbol)

(defun tangld-tangle-files (source-dir &optional target-dir)
  "Tangle files from SOURCE-DIR to TARGET-DIR.
If TARGET-DIR is omitted, default to the system dir specified by `tangld-project-dirs'."
  (tangld-with-tangle-reroute (or target-dir (tangld-dir 'system))
    (let ((files (directory-files-recursively source-dir "[^z-a]+\\.org")))
      (dolist (file files)
	(tangld-message "Building %s..." (f-abbrev file))
	(org-babel-tangle-file file)))))

(defun tangld-build-fn ()
  "Build source files based on `tangld-process-type'."
  (funcall (intern (format "tangld-build-fn-%s" tangld-process-type))))

(defun tangld-build-fn-direct ()
  "Tangle files into from the source dir to the build dir."
  (tangld-with-dirs (tangld-tangle-files .source .build)))

(defun tangld-build-fn-link ()
  "Tangle files from the source dir to the install dir."
  (tangld-with-dirs (tangld-tangle-files .source .install)))

(defun tangld-build-fn-basic ()
  "Do nothing."
  (tangld-message "Does nothing."))

;;;###autoload
(defun tangld-build (&optional force)
  "Tangle org-mode files from the source dir to the dotfiles dir.

By default, build will only tangle files that have changed since last run."
  (interactive "P")
  (run-hooks 'tangld-pre-build-hook)
  (funcall tangld-build-fn)
  (run-hooks 'tangld-post-build-hook))

(provide 'tangld-build)
