;; tangld-project.el -- Define a tangld project -*- lexical-binding: t -*-

;;; License:

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A tangld project defines all the information necessary to tangle files.
;;
;; A tangld project is a hashtable that looks like:
;; ('id "0kgm12tady5l"
;;  'name "My first project"
;;  'desc "A project with some config files"
;;  'source '("~/src/config1.org" "~/src/config2.org")
;;  'include '("~/inc/lib.org")
;;  )
;;; Code:
(require 'f) ; file and directory functions
(require 'ht) ; hashtable functions
(require 'org-id)

(declare-function org-id-time-to-b36 "org-id")

(declare-function tangld-build--find-files "tangld-build")

;; An HashTable of projects where id is the key, and the value is the properties
;; of the project
(defvar tangld-projects "A list of registered tangld projects")

(defun tangld-project--generate-id (&optional type)
  "Generate an ID to uniquely identify a project"
  (org-id-time-to-b36))

(defun tangld-projects-init ()
  "Initialize the `tangld-projects' table."
  (setq tangld-projects (ht-create)))

(defun tangld-project-exists-p (id)
  "return t if there is a project defined with the given ID."
  (ht-contains-p tangld-projects id))

(defun tangld-project-clear-table ()
  "Clear the list of projects."
  (ht-clear! tangld-projects))

(defun tangld-project-validate (project)
  "Ensure that a project has the required information."
  ;; as long as it has an id and at least one source
  (and (ht-contains-p project 'id)
       (ht-contains-p project 'source)
       (>= (safe-length (ht-get project 'source)) 1)))

(defun tangld-project-add (project)
  "Add a tangld PROJECT to the `tangld-projects' table."
  (unless (ht-p project)
    (error "Not a valid project"))
  (let* ((id (ht-get project 'id)))
    (ht-set! tangld-projects id project)))

(defun tangld-project-remove (id-or-project)
  "Remove a tangld project from the table given its ID-OR-PROJECT."
  (let ((id nil))
  (cond ((ht-p id-or-project)
         (setq id (ht-get id-or-project 'id)))
        ((stringp id-or-project)
         (setq id id-or-project)))
  (ht-remove tangld-projects id)))


(defun tangld-project (name &optional doc &rest project-info)
  "Define a project for use by tangld."
  (let ((source  (plist-get project-info :source))
        (include (plist-get project-info :include))
        (config  (plist-get project-info :config))
        (source-files nil)
        (include-files nil))
    (if (member name tangld-projects)
        (error "Redefinition of tangld project %s" name))

    (cond ((null source) (error "No source files where given for tangld project %s" name))
          ((listp source) ; source is either a source definition (plist) or a list of files
           (if (plist-get source :path) ; source definition
               (setq source-files (tangld-build--find-files source))
             (source-files source)))
          ((stringp source)
           (if f-directory-p source
             (setq source-files (tangld-build--find-files source))
             (setq source-files source)))
          (t
           (tangld-log-message "there was an error in setting the source for %s"
                               name) ))

    (cond ((listp include) ; include is either a include definition (plist) or a list of files
           (if (plist-get include :path) ; include definition
               (setq include-files (tangld-build--find-files include))
             (include-files include)))  ; a list of files
          ((stringp include)
           (if f-directory-p include
             (setq include-files (tangld-build--find-files include))
             (setq include-files include)))
          (t
           (tangld-log-message 3 "No library files where given for tangld project %s" name)))
    (plist-put tangld-project-list name (list source))
    ))


(defun tangld--project-generate (name doc src inc cfg)
  "Add a project definition to the list `tangld-projects-plist'.  NAME is a
unique name given to the project.  This is used in other functions like
`tangld-build-project' <name> DOC is a short docstring description of the
project SRC is a list of files to be tangled (each of these will be processed
by `org-babel-tangle' INC is a list of files to be ingested into the library
of babel prior to tangling `org-babel-lob-ingest'"
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
                        inc))
        ;; Also, the config settings don't need to be processed here right now, so I'm
        ;; just leaving this "place-holder" for future config processing prior to generating
        ;; the project
        (config cfg))
    (tangld-log-message 2 "adding %s to projects-plist" name)
    (setq tangld-projects-plist (plist-put tangld-projects-plist name (list sources includes config)))))


(defmacro tangld-project-define (name &optional doc &rest project-plist)
  "Define a tangld project.
NAME is a unique name for this project.  DOC is a short description
and PROJECT-PLIST is a plist of files or search criteria.  The
properties of the plist are:

:source either a list of files like

:source (\"fileA.org\" \"fileB.org\")

or a path description:

:source (:path \"~/.tangld-src\"        ; root directory to traverse
                     :recurse t             ; look in subdirectories
                     :files \"\\.org$\"       ; files to add to source
                     :exclude \"^\\.\"        ; unless they match this
                     :exclude-dir \"^\\.\"    ; don't look in these subdirs
                     :include \".*default.*\" ; cancel the exclusion for these
                     )
:include same options as :source

:config add any customization options here, like:

:config (progn
    (setq tangld-clear-library-before-build-p t))"
  (declare (indent defun))
  (tangld-log-message 4 "define: project %s" name)
  (let ((source (plist-get project-plist :source))
        (include (plist-get project-plist :include))
        (config (plist-get project-plist :config))
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
                 (setq include-files
                       (tangld-build-find-files (plist-get include :path)
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
    `(tangld--project-generate ,name ,doc (quote ,source-files) (quote ,include-files) (quote ,config))))

(provide 'tangld-project)
;; tangld-project.el ends here.
