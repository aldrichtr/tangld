(defun tangld-init ()
  "Setup a new tangld project"
  (interactive)

  (let-alist (tangld--expanded-project-dir-paths tangld-project-dirs)
    (cond ((not (f-exists-p .root)) nil)
	  (tangld-inhibit-init-if-exists
	   (error (format "Aborted init in %s" .root)))
	  ((y-or-n-p (format "WARNING: this will overwrite your project in %s continue?" .root))
	   (f-delete .root t))
	  (t
	   (tangld--message "overwriting %S" .root)))

    ;; either it's a new directory or the old one was deleted
    (tangld--message "creating directories in %s" .root)
    (mapc #'f-mkdir (mapcar #'cdr (tangld--expanded-project-dir-paths tangld-project-dirs)))

    ;; add the lib directory to the list of babel libraries
    (when tangld-add-project-lib-dir-on-init
      (add-to-list 'tangld-babel-library-dirs .lib))

    ;; initialize the version control (git init)
    (when tangld-init-vc-on-init
      (tangld-init--init-vc .root))
    (tangld--message "initialized new tangld project in %s" .root)))

(defun tangld-init--init-vc (&optional vc-root-dir)
  "Initialize the project using magit."
  (tangld--with-project-dirs
   (or vc-root-dir (setq vc-root-dir .root))
   (tangld--message "initializing git repo in %s" vc-root-dir)
   (if (featurep 'magit)
       (magit-call-git "init" (magit-convert-filename-for-git (expand-file-name vc-root-dir)))
     (message "Magit package not found"))))
