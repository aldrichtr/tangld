;;; ob-var-table.el --- Declare babel variables in a table -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Created: August 18, 2020
;; Modified: August 18, 2020
;; Version: 0.0.1
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This package enables a new org babel header arg `:var-table' which
;; takes 3 args, a `table-ref', `var-name' column and a `var-value' column.
;; This is useful to easily define a large number of variables
;;
;;; Code:

(defun ob-var-table-a (fn &optional light datum)
  "TODO"
  (let ((info (funcall fn light datum)))
    (unless light
      (when-let* ((prop-alist (nth 2 info))
                  (var-table (alist-get :var-table prop-alist))
                  (var-table (split-string var-table " "))
                  (table-ref (car var-table))
                  (resolved-table (org-babel-ref-resolve table-ref))
                  (names-column (or (ignore-error
                                        (string-to-number (nth 1 var-table)))
                                    0))
                  (values-column (or (ignore-error
                                         (string-to-number (nth 2 var-table)))
                                     1))
                  (var-params
                   (seq-filter
                    (lambda (x) (eq :var (car x)))
                    (org-babel-process-params
                     (mapcar (lambda (row)
                               (cons :var (format "%s=%s"
                                                  (nth names-column row)
                                                  (nth values-column row))))
                             resolved-table)))))
        (setf (nth 2 info) (append prop-alist var-params))))
    info))

(advice-add #'org-babel-get-src-block-info :around #'ob-var-table-a)

(provide 'ob-var-table)
;;; ob-var-table.el ends here
