;;; tangld-clean.el --- literate config development environment -*- lexical-binding: t; -*-
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

;; We want to be able to remove the files we created via `tangld-install'.
;; However, for `tangld-process-type' there are no record of which files were
;; tangled. We could cache this information. Problem is, caching comes with its
;; own host of problems. It is difficult (impossible?) to avoid possible syncing
;; problems where the cache.

;; TODO: right now, this function seems to switch the buffer.

;; I go through the trouble of creating these variables. While it may seem
;; overkill, I think this is the precise and safe way to do this.

(defun tangld-clean--files-to-clean ()
  "Return a list of the files written to by building."
  (let* ((files (gensym "files-"))
	 (fname (gensym "fname-"))
	 (collect-filename-fn `(lambda (_ _ ,fname &rest _) (push ,fname ,files))))
    (set files nil)
    (unwind-protect (progn
		      (advice-add #'write-region :override collect-filename-fn)
		      (funcall tangld-build-fn))
      (advice-remove #'write-region collect-filename-fn))
    (prog1 (symbol-value files) (unintern files))))

(defun tangld-clean-fn ()
  "Apply appropriate clean action to FILE based on `tangld-install-type'."
  (let* ((tangld-process-type (or tangld-process-type 'default))
	 (clean-fn (intern (format "tangld-clean-%s" tangld-process-type))))
    (funcall clean-fn file)))

(defun tangld-clean-direct ()
  "Remove FILE created by direct."
  (dolist (file (tangld-clean--files-to-clean))
    (tangld-message "Deleting %s..." file)
    (file-delete file)))

(defun tangld-clean-link ()
  "Remove symlink created by clean link type."
  (dolist (file (tangld-clean--files-to-clean))
    (when (f-symlink-p target)
      (tangld-message "Deleting %s..." file)
      (f-delete target))))

(defun tangld-clean-basic (args)
  "Remove files created via `tangle-install-basic'."
  (dolist (file (tangld-clean--files-to-clean))
    (tangld-message "Deleting %s..." file)
    (file-delete file)))

(defun tangld--clean-default ()
  (tangld-message "Does nothing."))

;;;###autoload
(defun tangld-clean ()
  "Remove any symlinks corresponding to files in dotfiles-dir."
  (interactive)
  (funcall (tangld-clean-fn)))

(provide 'tangld-clean)
