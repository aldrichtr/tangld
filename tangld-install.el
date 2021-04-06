;;;; Install - tangld-install

(defcustom tangld-preinstall-hook nil
  "Hook run before `tangld-install' is called."
  :group 'tangld
  :type 'hook)

(defcustom tangld-postinstall-hook nil
  "Hook run after `tangld-install' is called."
  :group 'tangld
  :type 'hook)

(defun tangld--link-type-install (file)
  "Apply appropriate install action based on `tangld-install-type'."
  (let* ((tangld-install-type (or tangld-install-type 'default))
	 (install-fn (intern (format "tangld--link-type-%s-install" tangld-install-type))))
    (funcall install-fn file)))

(defun tangld--link-type-direct-install (file)
  "Move FILE from build-dir to system-dir."
  (let ((target (tangld--target-file file 'direct)))
    (unless (f-exists-p (f-parent target))
      (mkdir (f-parent target) t))
    (f-move file target)
    (tangld--message "move %s -> %s" (f-abbrev file) (f-abbrev target))))

(defun tangld--link-type-link-install (file)
  "Symlink FILE to system-dir."
  (let ((target (tangld--target-file file 'link)))
    (unless (f-symlink-p file)
      (f-symlink file target)
      (tangld--message "symlink %s -> %s" (f-abbrev file) (f-abbrev target)))))

(defun tangld--link-type-stow-install (file)
  "Use stow to symlink file."
  (tangld--message "Not yet implemented."))

(defun tangld--link-type-default-install (_) nil)

(defun tangld-install ()
  "Symlink files in dotfiles directory to system directory."
  (interactive)
  (run-hooks 'tangld-pre-install-hook)
  (mapc #'tangld--link-type-install (directory-files-recursively (alist-get 'install tangld-project-dirs) "."))
  (run-hooks 'tangld-post-install-hook))
