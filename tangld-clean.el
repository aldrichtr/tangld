;;; tangld-install.el
;;
;; Copyright (C) 2021 Timothy Aldrich

;; Author: Timothy Aldrich <timothy.r.aldrich@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((org) (notifications) (f) (s) (async))
;; Keywords: tools processes
;; URL: https://github.com/aldrichtr/tangld

(defun tangld--link-type-clean (file)
  "Apply appropriate clean action to FILE based on `tangld-install-type'."
  (let* ((tangld-install-type (or tangld-install-type 'default))
	 (clean-fn (intern (format "tangld--link-type-%s-clean" tangld-install-type))))
    (funcall clean-fn file)))

(defun tangld--link-type-direct-clean (file)
  "Remove FILE created by direct."
  (f-delete file (tangld--target-file file 'direct)))

(defun tangld--link-type-link-clean (file)
  "Remove symlink created by clean link type."
  (let ((target (tangld--target-file file 'link)))
    (when (f-symlink-p target)
      (f-delete target))))

(defun tangld--link-type-stow-clean (file)
  "Remove FILE created by direct link type."
  (tangld--message "Not yet implemented."))

(defun tangld--link-type-default-clean (_)
  (tangld--message "Does nothing."))

;;;###autoload
(defun tangld-clean ()
  "Remove any symlinks corresponding to files in dotfiles-dir."
  (interactive)
  (mapc #'tangld--link-type-clean
	(directory-files-recursively (alist-get 'source tangld-project-dirs) ".")))

