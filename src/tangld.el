;;; tangld.el --- literate config development environment -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Timothy Aldrich

;; Author: Timothy Aldrich <timothy.r.aldrich@gmail.com>
;; Version: 0.0.1
;; Keywords: tools processes
;; URL: https://github.com/aldrichtr/tangld

;;; Commentary:
;; A Literate Programming Environment for configuration files and scripts

;; tangld is an Emacs package that provides 'dotfiles management' features
;; using Literate Programming paradigms.  Using org-mode files with source
;; blocks and the tangle functionality, Emacs can be used as an IDE to
;; document, build, and install configuration files, scripts and other
;; files on a system.  More details are available in the README.org file.

;;; Code:

; The actual org-babel-tangle library
(require 'ob-tangle)


;;;; Constants




;;;; Global settings

(defgroup tangld nil
  "Literate Config Manager"
  :prefix "tangld-")

(defcustom tangld-log-buffer-name "*tangld*"
  "Default buffer name where tangld logs messages"
  :group 'tangld
  :type 'string)

; There's probably a much better way to do this but,
; I'm wrapping the `tangld-log-message' calls with a test of
; `tangld-log-level' which is a value from 0 to 4
(defcustom tangld-log-level 4
  "Logging level provided by tangld functions.
   0 = silent no output
   1 = warnings only
   2 = informational
   3 = verbose
   4 = debugging"
  :group 'tangld
  :type 'integer)

(defcustom tangld-project-plist '()
  "A list of projects used when calling `tangld-build'"
  :group 'tangld
  :type '(plist :value-type (group string )))

(defcustom tangld-confirm-on-eval nil
  "If non-nil, emacs will ask before evaluating code in source blocks"
  :group 'tangld
  :type '(choice
          (const :tag "Don't ask" nil)
          (const :tag "Ask" t)))

(defcustom tangld-add-src-return-link-comments t
  "Add a link to the source code block in the output"
  :group 'tangld
  :type 'boolean)

;;;; Logging

(defun tangld-log-message (lvl msg &rest args)
  "Log messages to the tangld log buffer"
  (let ((log-msg (apply #'format msg args)))
    (if (<= lvl tangld-log-level)
        (print log-msg (get-buffer-create tangld-log-buffer-name)))))


;;;; Projects

(defun tangld-project-generate (name doc src inc)
  "Add a project definition to the list `tangld-projects-plist'.
NAME is a unique name given to the project.  This is used in other functions like 
`tangld-build-project' <name>
DOC is a short docstring description of the project
SRC is a list of files to be tangled (each of these will be processed by `org-babel-tangle'
INC is a list of files to be ingested into the library of babel prior to tangling `org-babel-lob-ingest'"
  (interactive)
      (tangld-log-message 2 "generate: %s %s" name doc)
      (tangld-log-message 2 "          - sources %s" src)
      ;; TODO right now I don't need to do anything but add the files in src and lib to a list
      ;; and provide some status.  Later, additional processing can be added here first, prior to
      ;; adding it to the projects list.
  (let ((sources (-map (lambda (f)
                             (tangld-log-message 3 "      '%s' is in list" f)
                         f)
                       src))
        (includes (-map (lambda (f)
                              (tangld-log-message 3 "      '%s' is in list" f)
                          f)
                        inc)))
        (tangld-log-message 2 "adding %s to projects-plist" name)
    (setq tangld-projects-plist (plist-put tangld-projects-plist name (list sources includes)))))


(defmacro tangld-project-define (name &optional doc &rest project-plist)
  "define a tangld project.  NAME is a unique name for this project.  DOC is a short description
and PROJECT-PLIST is a plist of files or search criteria.  The properties of the plist are:

:source either a list of files like 

:source (\"fileA.org\" \"fileB.org\") 

or a path description:

:source (:path \"~/.tangld/src\"        ; root directory to traverse
                     :recurse t             ; look in subdirectories
                     :files \"\\.org$\"       ; files to add to source
                     :exclude \"^\\.\"        ; unless they match this
                     :exclude-dir \"^\\.\"    ; don't look in these subdirs
                     :include \".*default.*\" ; cancel the exclusion for these
                     )
:include same options as :source"
  (declare (indent defun))
  (tangld-log-message 4 "define: project %s" name)
  (let ((source (plist-get project-plist :source))
        (include (plist-get project-plist :include))
        (source-files nil)
        (include-files nil))
    (cond ((listp source)
           (tangld-log-message 4 "        - source is a list %s" source)
           (if (plist-get source :path)
               (progn
                 (tangld-log-message 4 "          - A path spec is included. Pass them to tangld-build-find-files")
                 (setq source-files (tangld-build-find-files (plist-get source :path)
                                                             :recurse (plist-get source :recurse)
                                                             :files   (plist-get source :files)
                                                             :exclude (plist-get source :exclude)
                                                             :exclude-dir (plist-get source :exclude-dir)
                                                             :include (plist-get source :include)))
                 (tangld-log-message 4 "         - now source files are:")
                 (tangld-log-message 4 (string-join source-files "\n")))
             (progn
               (tangld-log-message 4 "           - source is a list of files %s" source)
               (tangld-log-message 4 "           - add them to the list of source-files : %s" source-files )
               (setq source-files source))))
          ((stringp source)
           (tangld-log-message 4 "           - sources is a string: '%s' if its a dir we'd expand" source))
          (t
           (tangld-log-message 4 "sources is something else '%s'" source))
          )
    (cond ((listp include)
           (tangld-log-message 4 "        - include is a list %s" include)
           (if (plist-get include :path)
               (progn
                 (tangld-log-message 4 "          - A path spec is included. Pass them to tangld-build-find-files")
                 (setq include-files (tangld-build-find-files (plist-get include :path)
                                                              :recurse (plist-get include :recurse)
                                                              :files   (plist-get include :files)
                                                              :exclude (plist-get include :exclude)
                                                              :exclude-dir (plist-get include :exclude-dir)
                                                              :include (plist-get include :include)))
                 (tangld-log-message 4 "         - now include files are:")
                 (tangld-log-message 4 (string-join include-files "\n")))
             (progn
               (tangld-log-message 4 "           - include is a list of files %s" include)
               (tangld-log-message 4 "           - add them to the list of include-files : %s" include-files )
               (setq include-files include))))
          ((stringp include)
           (tangld-log-message 4 "           - includes is a string: '%s' if its a dir we'd expand" include))
          (t
           (tangld-log-message 4 "includes is something else '%s'" include))
          )
    (tangld-log-message 4 "      calling generate now:")
    (tangld-log-message 4 (string-join source-files "\n"))
    `(tangld-project-generate ,name ,doc (quote ,source-files) (quote ,include-files))))

;;; tangld.el ends here

