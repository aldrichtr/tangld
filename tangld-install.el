;;; tangld-install.el --- literate config development environment -*- lexical-binding: t; -*-
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

(defcustom tangld-pre-install-hook nil
  "Hook run before `tangld-install' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-post-install-hook nil
  "Hook run after `tangld-install' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-install-fn #'tangld-install-fn
  "Function used to install file to."
  :group 'tangld
  :type 'symbol)

;; TODO: not sure how to enforce this yet.
;; When installing files, we could easily run into the problem where a file already exists.
(defcustom tangld-install-overwrite-p nil
  "Whether to overwrite existing files in places to install.")

(defun tangld-install-delete-existing-maybe ()
  "Retur"
  (cond ((and tangld-installed-overwrite-p (f-exists-p path))
	 (tangld-message "file already at %s, deleting...")
	 (f-delete path))
	((and (f-exists-p ))
	 (tangld-message "file exists at %s, skipping...")
	 (continue))))

(defun tangld-install-fn ()
  "Call appropriate install function based on `tangld-process-type'."
  (funcall (intern (format "tangld-install-fn-%s" tangld-process-type))))

(defun tangld-install-fn-direct ()
  "Move files in the build-dir to their corresponding place in the system-dir."
  (tangld-let* ((path nil)
		(files (directory-files-recursively .build ".*")))
    (dolist (file files)
      (setq path (expand-file-name (f-relative file .build) .system))
      (when (tangld-install-delete-existing-maybe file)
	(tangld-message "Move %s to %s.")
	(f-move file path)))
    (tangld-message "Done.")))

(defun tangld-install-fn-link ()
  "Create symlinks from the files installed in the install-dir to the system-dir."
  (tangld-let* ((path nil)
		(files (directory-files-recursively .install ".*")))
    (dolist (file files)
      (setq path (f-expand (f-relative file .install) .system))
      (when (tangld-install-delete-existing-maybe file)
	(tangld-msg "Symlink %s to %s.")
	(f-symlink file path)))
    (tangld-message "Done.")))

(defun tangld-install-fn-basic ()
  "Just tangle files to their specified locations."
  (tangld-with-dirs (tangld-tangle-files .source)))

;;;###autoload
(defun tangld-install ()
  "Symlink files in dotfiles directory to system directory."
  (interactive)
  (run-hooks 'tangld-pre-install-hook)
  (funcall tangld-install-fn)
  (run-hooks 'tangld-post-install-hook))

(provide 'tangld-install)
