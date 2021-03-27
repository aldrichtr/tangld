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

(defun tangld-message (format-string &rest args)
  "Display message if `tangld-verbose-p' is non-nil."
  (when tangld-verbose-p
    (message "[tangld] %s" (apply #'format format-string args))))

(defun tangld-dir (dirname)
  "Return the full path specified of DIRNAME.
DIRNAME is a key in `tangld-project-dirs'."
  (let-alist tangld-project-dirs
    (cl-case dirname
      (system (f-full .system))
      (root (f-expand (alist-get dirname tangld-project-dirs) .system))
      (t (f-expand (alist-get dirname tangld-project-dirs) (f-join .system .root))))))

(defmacro tangld-with-dirs (&rest body)
  "Let-bind dotted symbols to the full path of their CDR in `tangld-project-dirs'."
  (let* ((keys (mapcar #'car tangld-project-dirs))
	 (var-and-vals (mapcar (lambda (key) (list (intern (format ".%s" key)) (tangld-dir key))) keys)))
    `(let ,var-and-vals
       ,@body)))

(defmacro tangld-let (vars &rest body)
  "Wrapper around `let'."
  (declare (indent defun))
  `(tangld-with-dirs (let ,vars ,@body)))

(defmacro tangld-let* (vars &rest body)
  "Wrapper around `let*'."
  (declare (indent defun))
  `(tangld-with-dirs (let* ,vars ,@body)))

(defun tangld-relative-lambda (target)
  "Return a function that reroutes a system path to target."
  (tangld-let ((path (gensym "path")))
    `(lambda (,path)
       (aprog1 (f-expand (f-relative ,path ,.system) ,target)
	 (message "[tangld] rerouting %s to %s" ,path it)))))

(defun tangld-org-src-info-around-advice-lambda (fn)
  "Return a function suitable for advising `org-babel-get-src-block-info'."
  (let ((orig-fn (gensym "orig-fn-"))
	(args (gensym "args-")))
    `(lambda (,orig-fn &rest ,args)
       (let* ((light (car ,args))
	      (datum (cadr ,args))
	      (info (funcall ,orig-fn light datum)))
	 (unless light
	   (let* ((prop-alist (nth 2 info))
		  (dir (substitute-env-vars (or (alist-get :dir prop-alist) "")))
		  (root-dir (substitute-env-vars (or (alist-get :root-dir prop-alist) "")))
		  (tangle (alist-get :tangle prop-alist)))
	     (setf (alist-get :mkdirp prop-alist) "yes")
	     (message "tangle: %s" tangle)
	     (cond ((or (not (stringp tangle)) (string= tangle "no")))
		   ((string= tangle "yes"))
		   ((string-prefix-p "/" tangle))
		   (t
		    (setf (alist-get :tangle prop-alist) (funcall ,fn tangle))))
	     (setf (seq-elt info 2) prop-alist)
	     info))
	 info))))

(defmacro tangld-with-tangle-reroute (dir &rest body)
  "Reroute location files in body are tangled relative to DIR."
  (declare (indent defun))
  (let ((advice (gensym "tangld-with-tangle-reroute-advice-")))
    `(unwind-protect
	 (progn
	   (fset ',advice (tangld-org-src-info-around-advice-lambda (tangld-relative-lambda ,dir)))
	   (advice-add #'org-babel-get-src-block-info :around #',advice)
	   (progn ,@body))
       (advice-remove 'org-babel-get-src-block-info #',advice)
       (fmakunbound #',advice))))
