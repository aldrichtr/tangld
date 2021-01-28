;;; ob-load-namespaced-libraries.el --- Allows importing of namespaced babel libraries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Tyler Ware
;;
;; Created: August 18, 2020
;; Modified: August 18, 2020
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'ob-lob)
(require 'subr-x)

(defmacro with-ob-lib (library &rest body)
  "TODO"
  `(let ((org-babel-library-of-babel ,library))
     ,@body))

(defmacro with-ob-global-lib (&rest body)
  "TODO"
  `(with-ob-lib org-babel-library-of-babel
                ,@body))

(defmacro ob-lib-delta (&rest body)
  "TODO"
  `(with-ob-global-lib
    (let ((previous-lib org-babel-library-of-babel))
      ,@body
      (seq-difference org-babel-library-of-babel previous-lib))))

(defun namespace-ob-lib (namespace library)
  "TODO"
  (mapcar
   (lambda (entry)
     (let* ((old-name (symbol-name (car entry)))
            (new-name (concat namespace "/" old-name)))
       (setf (car entry) (intern new-name)
             (nth 5 entry) new-name)
       entry))
   library))

(defun ob-make-lib-from-file (file &optional load namespace)
  "TODO"
  (let ((library (ob-lib-delta (org-babel-lob-ingest file))))
    (when namespace
      (setq library (namespace-ob-lib namespace library)))
    (when load
      (setq org-babel-library-of-babel (append
                                        library
                                        org-babel-library-of-babel)))
    library))

(defun ob-make-lib-from-files (files &optional load namespace-fn)
  "TODO"
  ;; If there are no cycles in the dependencies (fileA -needs-> fileB -needs-> fileC -needs-> fileA)
  ;; then this is enough tries to ensure we load all the files. Eventually, we could get more sophisticated
  ;; in how we load src blocks as cyclic file dependency does not imply a cyclic src block dependency.
  ;; This is good enough for now
  (let* ((max-tries (length files))
         (retry-table (make-hash-table :test #'equal
                                       :size (length files)))
         (library
          (ob-lib-delta
           (while files
             (let* ((file (pop files))
                    (try-count (or (gethash file retry-table) 0)))
               (puthash file (1+ try-count) retry-table)
               (condition-case err
                   (progn
                     (ob-make-lib-from-file file t (funcall (or namespace-fn #'identity) file))
                     (message "Successfully loaded library from file: %s" file))
                 (t (message "Error when loading (%s): %s" file err)
                    (if (< try-count max-tries)
                        ;; Put in back on the end and hope what it needs can be loaded
                        (setq files (append files (list file)))
                      (user-error "Unable to resolve library references for %s after %s tries" file max-tries))))))
           (message "Finished loading %s libraries" (length files)))))

    (when load
      (setq org-babel-library-of-babel
            (append library org-babel-library-of-babel)))
    library))


(provide 'ob-load-namespaced-libraries)
;;; ob-load-namespaced-libraries.el ends here
