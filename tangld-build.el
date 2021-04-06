;;; tangld-build.el
;;
;; Copyright (C) 2021 Timothy Aldrich

;; Author: Timothy Aldrich <timothy.r.aldrich@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((org) (notifications) (f) (s) (async))
;; Keywords: tools processes
;; URL: https://github.com/aldrichtr/tangld

(require 'tangld-helpers)
(require 'tangld)

(defcustom tangld-prebuild-hook nil
  "Hook run before `tangld-build' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-postbuild-hook nil
  "Hook run after `tangld-build' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-tangld-build-fn #'tangld--default-build-fn
  "Function that specifies how a file will be built."
  :group 'tangld)

;; Internal Functions

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

;;; Main Function

;;;###autoload
(defun tangld-build (&optional force)
  "Tangle org-mode files from the source dir to the dotfiles dir.

By default, build will only tangle files that have changed since last run."
  (interactive "P")
  (run-hooks 'tangld-prebuild-hooks)
  (tangld--let* ((tangld--lazy-tangle force))
    (mapc #'tangld--link-type-build (directory-files-recursively .source "."))))
