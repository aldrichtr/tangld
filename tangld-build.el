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

(defcustom tangld-build-fn #'tangld--default-build-fn
  "Function that specifies how a file will be built."
  :group 'tangld)

(defun tangld-default-build-fn (file source-dir target-dir)
  "Build FILE from SOURCE to TARGET."
  (let-alist nil
    (cond ((file-ext-p file "org")
	   (tangld--tangle file target tangld--lazy-tangle-p))
	  (t
	   (f-symlink file target)))))

(defun tangld--link-type-build (file)
  "Apply appropriate build action based on `tangld-install-type'."
  (let* ((tangld-install-type (or tangld-install-type 'default))
	 (build-fn (intern (format "tangld--link-type-%s-build" tangld-install-type))))
    (funcall build-fn file)))

(defun tangld--link-type-direct-build (file)
  "Tangle file to the build directory."
  (funcall tangld-build-fn file 'source 'build))

(defun tangld--link-type-link-build (file)
  "Tangle file to install-root-dir."
  (funcall tangld--build-fn file 'source 'install))

(defun tangld--link-type-stow-build (file)
  "Tangle file to build directory."
  (funcall tangld-build-fn file 'source 'build))

(defalias 'tangld--link-type-default-build 'tangld--link-type-link-build)

;;;###autoload
(defun tangld-build (&optional force)
  "Tangle org-mode files from the source dir to the dotfiles dir.

By default, build will only tangle files that have changed since last run."
  (interactive "P")
  (run-hooks 'tangld-pre-build-hooks)
  (let ((tangld--lazy-tangle force)
	(source-dir (alist-get 'source tangld-project-dirs))
	(files (directory-files-recursively source-dir ".")))
    (mapc #'tangld--link-type-build files))
  (run-hooks 'tangld-post-build-hooks))

(provide 'tangld-build)
