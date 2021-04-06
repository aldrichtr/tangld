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

(defcustom tangld-file-target-fn #'tangld--file-target-fn
  "Function that returns where a built file should go.")

(defcustom tangld-build-fn #'tangld--default-build-fn
  "Function that specifies how a file will be built.
The function takes two arguments, file and the path of the file to be built."
  :group 'tangld
  :type 'symbol)

(defun tangld--file-target-fn (file)
  "Map file to build target."
  (let ((source-dir (alist-get 'source tangld-project-dirs))
	(build-dir (alist-get 'build tagnld-project-dirs)))
    (f-expand (f-relative file source-dir) build-dir)))

(defun tangld--default-build-fn (file target)
  "Build FILE into the build directory."
  (cond ((file-ext-p file "org")
	 (tangld--tangle file target tangld-lazy-tangle-p))
	(t
	 (f-symlink file target))))

;;;###autoload
(defun tangld-build (&optional force)
  "Tangle org-mode files from the source dir to the dotfiles dir.

By default, build will only tangle files that have changed since last run."
  (interactive "P")
  (run-hooks 'tangld-pre-build-hook)
  (let ((tangld-lazy-tangle-p force)
	(source-dir (alist-get 'source tangld-project-dirs))
	(files (directory-files-recursively source-dir ".")))
    (dolist (file files)
      (funcall tangld-build-fn file (funcall tangld-file-target-fn file))))
  (run-hooks 'tangld-post-build-hook))

(provide 'tangld-build)
