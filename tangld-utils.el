;;; tangld-utils.el --- literate config development environment -*- lexical-binding: t; -*-
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

(defcustom tangld-tangle-fn #'tangld--tangle
  "Function that returns a function that tangles a file to a target.
The function should accept two arguments, the file and the target. The function
should return (OUTCOME FILE TARGET), where OUTCOME is non-nil when tangling has
been successful.")

(defcustom tangld-tangle-result-fn #'tangld--tangle-result
  "Function that outputs a result based on tangling.")

(defcustom tangld-lazy-tangle-p t
  "Only tangle when necessary.
That is, when the target file either does not exist or is older than the source file."
  :group 'tangld
  :type 'boolean)

(defmacro tangld--with-dirs (&rest body)
  `(let-alist (tangld--expanded-project-dir-paths)
     ,@body))

(defmacro tangld--let* (vars &rest body)
  `(tangld--with-dirs (let* ,vars ,@body)))

(defun tangld-tangle-fn (file target)
  "Return a lambda tha tangles FILE to TARGET."
  `(lambda ()
     (require 'org)
     (require 'ob-tangle)
     (let ((org-babel-confirm-evaluate nil)
	   (gc-cons-threshold most-positive-fixnum)
	   (org-babel-default-header-args '((:tangle . "yes") (:results . "silent"))))
       (list (ignore-errors (org-babel-tangle-file ,file ,target)) ,file ,target))))

(defun tangld--async-tangle-file (file target)
  "Asynchronously tangle FILE to TARGET."
  (async-start (funcall tangld-tangle-fn file target) #'tangld--tangle-outcome))

(defun tangld--tangle (file target &optional force)
  "Tangle FILE into PROJECT-DIR.

Only tangles if target file either does not exist or is older than FILE. If
FORCE is enabled, tangle no matter what."
  (when (or force (not (f-exists-p target)) (file-newer-than-file-p file target))
    (tangld--message "tangling %s -> %s")
    (tangld--async-tangle-file file target)))

(defun tangld--tangle-result (result)
  "Output the result of tangling in the messages buffer."
  (cl-destructuring-bind (outcome file target) result
    (tangld--message "%s in tangling %s to %s" (if outcome "Succeded" "Failed") file target)))

(defun tangld--target-file (file source-dir target-dir)
  "Return the tangle target of link-type based on FILE."
  (f-expand (f-relative file source-dir) target-dir))

(defun tangld--message (format-string &rest args)
  "Display message if `tangld-verbose-p' is non-nil."
  (when tangld-verbose-p (message "[tangld] %s" (format format-string args))))

(defun tangld--expanded-project-dir-paths ()
  "Return `tangld-project-dirs' with values all expanded."
  (let ((expanded nil)
	(root (alist-get 'root tangld-project-dirs)))
    (dolist (it tangld-project-dirs)
      (let ((name (car it)) (val (cdr it)))
	(push (cons name (expand-file-name val root)) expanded)))
    expanded))

(provide 'tangld-utils)
