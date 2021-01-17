;;; ob-text-var-expansion.el --- expands environment style variables in plain text blocks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Tyler Ware
;;
;; Created: August 18, 2020
;; Modified: August 18, 2020
;; Version: 0.0.1
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Add ability to expand variables in plain text style SRC blocks like: json, conf, yaml, etc.
;;
;; Specifically, this will expand a `:var' inside a SRC block with the syntax `${name}'
;;
;;; Code:

(defun ob-text-var-expansion (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (if (null vars) (concat body "\n")
      (cl-loop for var in vars
               do
               (setq body (replace-regexp-in-string
                           (rx-to-string (list ': "${" (prin1-to-string (car var)) "}"))
                           (format "%s" (cdr var))
                           body)))
      body)))

(defalias 'org-babel-expand-body:conf 'ob-text-var-expansion)
(defalias 'org-babel-expand-body:json 'ob-text-var-expansion)
(defalias 'org-babel-expand-body:yaml 'ob-text-var-expansion)
(defalias 'org-babel-expand-body:text 'ob-text-var-expansion)

(provide 'ob-text-var-expansion)
;;; ob-text-var-expansion.el ends here
