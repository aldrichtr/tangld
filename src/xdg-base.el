;;; xdg-base.el --- Cross platform xdg directories

;;; Commentary:
;;

;;; Code:

(defun xdg-base-data-home (&optional os)
  "If `$XDG_DATA_HOME' is set, return that.  Otherwise,
output the directory according to OS or the `system-type' of the current
system"
  (interactive)
  (let ((os (or os system-type))
        (dir (or (getenv "XDG_DATA_HOME")
                 (cond ((eq os 'windows-nt)
                        (getenv "LOCALAPPDATA"))
                       ((eq os 'darwin)
                        (expand-file-name "~/Library"))
                       (t
                        (expand-file-name "~/.local/share"))
                       ))))
    dir))

(defun xdg-base-config-home (&optional os)
  "If `$XDG_CONFIG_HOME' is set, return that.  Otherwise,
output the directory according to OS or the `system-type' of the current
system"
  (interactive)
  (let ((os (or os system-type))
        (dir (or (getenv "XDG_CONFIG_HOME")
                 (cond ((eq os 'windows-nt)
                        (getenv "APPDATA"))
                       ((eq os 'darwin)
                        (expand-file-name "~/Library/Preferences"))
                       (t
                        (expand-file-name "~/.config"))
                       ))))
    dir))


(defun xdg-base-cache-home (&optional os)
  "If `$XDG_CACHE_HOME' is set, return that.  Otherwise,
output the directory according to OS or the `system-type' of the current
system"
  (interactive)
  (let ((os (or os system-type))
        (dir (or (getenv "XDG_CACHE_HOME")
                 (cond ((eq os 'windows-nt)
                        (expand-file-name
                         (file-name-concat (getenv "LOCALAPPDATA") "cache")))
                       ((eq os 'darwin)
                        (expand-file-name "~/Library/Preferences"))
                       (t
                        (expand-file-name "~/.config"))
                       ))))
    dir))

(provide 'xdg-base)

;;; xdg-base.el ends here
