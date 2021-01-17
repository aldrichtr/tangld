;;; ob-extended-tangle.el --- extend ob header args -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Tyler Ware
;;
;; Author: Tyler Ware
;; Created: August 16, 2020
;; Modified: August 16, 2020
;; Version: 0.0.1
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Adds extra header-args to org-babel.
;;  - `:tangle-relative' gives the ability to tangle files relative to either the
;;    `:dir' or some other specified path.
;;  - `:root-dir' gives the ability to specifiy a root directory. This can be used
;;    in tangling files relative to this root or allowing for `dir' values that are
;;    relative to the root
;;
;;; Code:

(defun ob-tangle-files (files)
  "Tangle multiple files."
  (cl-loop for file
           in files
           do
           (org-babel-tangle-file file)))

(defun ob-extended-tangle-args-a (fn &optional light datum)
  "Add :tangle-relative & :root-dir property to org babel header args.

The :tangle-relative property will make the :tangle files relative to
the :dir or to the value of :tangle-relative.

If :tangle-relative is
- equal to 'dir, then it uses :dir
- a string it uses the value passed

The :root-dir property can be used in conjunction with :tangle-relative.
if specified, then:
- if :dir has no value, the file is tangled relative to :root-dir
- if :dir has a value, but is an absolute path (unix), then :root-dir is ignored and the :dir is used for tangling
- if :dir has a value, but is not an absolute path (unix), then :root-dir and :dir are combined and the file is tangled to that path"
  (let ((info (funcall fn light datum)))
    (unless light
      (let* ((prop-alist (nth 2 info))
             (dir (substitute-env-vars (or (alist-get :dir prop-alist) "")))
             (root-dir (substitute-env-vars (or (alist-get :root-dir prop-alist) "")))
             (tangle (alist-get :tangle prop-alist))
             (tangle-relative (alist-get :tangle-relative prop-alist)))
        (when (and (stringp tangle)
                   (not (equal tangle "yes"))
                   (not (equal tangle "no"))
                   (not (string-prefix-p "/" tangle))
                   tangle-relative)
          (setf (alist-get :tangle prop-alist)
                (let ((directory (if (eq tangle-relative 'dir) dir "")))
                  (unless (null root-dir)
                    (setq directory (cond
                                     ((null directory) root-dir)
                                     ((string-prefix-p "/" directory) directory)
                                     (t (concat (file-name-as-directory root-dir) directory)))))

                  (let ((tangle-path (concat
                                      (file-name-as-directory directory)
                                      tangle)))
                    tangle-path))))))
    info))

(advice-add #'org-babel-get-src-block-info :around #'ob-extended-tangle-args-a)

(provide 'ob-extended-tangle)
;;; ob-extended-tangle.el ends here
