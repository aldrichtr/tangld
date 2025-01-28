;;; tangld-build.el --- literate config development environment -*- lexical-binding: t; -*-
;;

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
;; A Literate Programming Environment for configuration files and scripts

;; tangld is an Emacs package that provides 'dotfiles management' features
;; using Literate Programming paradigms.  Using org-mode files with source
;; blocks and the tangle functionality, Emacs can be used as an IDE to
;; document, build, and install configuration files, scripts and other
;; files on a system.  More details are available in the README.org file.


;;; Code:

;;;; Customization settings

(defcustom tangld-pre-build-hook nil
  "Hook run prior to building org-source files.  This hook runs prior to loading
the library (or cache), and before the list of sources is built. Also see:
 - `org-babel-pre-tangle-hook'  :: before each file is tangled
 - `org-babel-tangle-body-hook' :: over the body of each source block"
  :group 'tangld
  :type 'hook)

(defcustom tangld-post-build-hook nil
  "Hook run after all org-source files have been tangled. Also see:
 - `org-babel-post-tangle-hook' :: after each file is tangled"
  :group 'tangld
  :type 'hook)

(defcustom tangld-pre-load-library nil
  "Hook run before loading the library of babel."
  :group 'tangld
  :type 'hook)

(defcustom tangld-post-load-library nil
  "Hook run after loading the library of babel."
  :group 'tangld
  :type 'hook)

(defcustom tangld-pre-load-library-file nil
  "Hook run before loading each file into the library of babel."
  :group 'tangld
  :type 'hook)

(defcustom tangld-post-load-library-file nil
  "Hook run after loading each file into the library of babel."
  :group 'tangld
  :type 'hook)

(defcustom tangld-clear-library-before-build-p t
  "Clear the library-of-babel prior to build."
  :group 'tangld
  :type 'string)

(defcustom tangld-build-file-regex "\\.org$"
  "Regular expression used to gather files to be used in a project.  Combine
regular expressions in `tangld-build-file-regex' ,
`tangld-build-file-include-regex', and `tangld-build-file-exclude-regex' to
provide tangld-build fine-grained control over which files are ingested
(library) and tangled (source)"
  :group 'tangld
  :type 'string)

(defcustom tangld-build-file-include-regex "\\+"
  "Regular expression used to match org-source file names that should be
tangled.  By default any file that starts with '+' is included even if the
`tangld-build-file-exclude-regex' would match"
  :group 'tangld
  :type 'string)

(defcustom tangld-build-file-exclude-regex "^_"
  "Regular expression used to match org-source file names that should be
skipped.  By default any file that starts with a '_' is skipped"
  :group 'tangld
  :type 'string)

(defcustom tangld-build-dir-exclude-regex "^\\."
  "Regular expression used to match org-source directory names that should be
skipped.  By default any directory that starts with a '.' is skipped"
  :group 'tangld
  :type 'string)

(defun tangld-build-project (name &rest args)
  "Build a project listed in `tangld-projects-plist' with the given NAME"
  (interactive)
  (let* ((project (plist-get tangld-projects-plist name))
         (sources (or (pop project) '()))
         (includes (or (pop project) '()))
         (config   (or (project)) '()))
    (tangld-build sources includes config)))

(defun tangld-build (src &optional inc cfg)
  "Call org-babel-tangle on each file in SRC.
If INC is present, call 'org-babel-lob-ingest' on each file prior to tangle.
If CFG is non-nil, eval prior to build."
  (interactive)
  ;; 1. Run any pre-build hooks
  (run-hooks 'tangld-pre-build-hook)
  ;; 2. Set options that control tangling
  (setq org-confirm-babel-evaluate tangld-confirm-on-eval)
  (if (cfg) (funcall cfg))
  ;; 3. Load the library
  (if (and (tangld-load-library-on-build-p
            inc))
      (tangld-build-load-library inc))
  ;; 4. Tangle the source files
  (mapcar (lambda (f)
            (if (tangld-file-changed-p f)
                ((tangld-log-message 3 "Tangling '%s'" f)
                 (org-babel-tangle-file f)
                 (tangld-file-last-checksum-put f))
              (tangld-log-message 3 "'%s' no changes since last tangle" f)))
          src)
  ;; 5. Run any post-build hooks
  (run-hooks 'tangld-post-build-hook))

(defvar org-babel-library-of-babel) ;; from ob-lob.el
(defun tangld--clear-library ()
  "Remove any src blocks from the library-of-babel"
  (setq org-babel-library-of-babel nil)
  )

(defun tangld-build-load-library (lib-files &optional clear)
  "Ingest `org-mode' files into a library of babel for use by other source
blocks. if LIB-FILES is a list of one or more files, use them for the
`library-of-babel' if CLEAR is non-nil, clear prior to loading. Honors
`tangld-clear-library-before-build-p'."
  (interactive)
  (setq org-confirm-babel-evaluate tangld-confirm-on-eval)
  (let ((clear-p (or (clear tangld-clear-library-before-build-p))))
    (if (clear-p (tangld--clear-library)))
    (mapcar (lambda (f)
              ((tangld-log-message 3 "Loading library '%s'" f)
               (org-babel-lob-ingest f)))
            lib-files)))

(defun tangld--file-filter (file root &rest args)
  "Compare FILE to the regexen in ARGS.  ROOT is the root of the tangld project.
ARGS is a plist where each property is a regular expression where:
`:files' Matches against the file name (with extension).
`:exclude' Matches are excluded even if they match `:files'.
`:exclude-dir' don't look in these subdirectories.
`:include' Matches override the `:exclude'."
  (let* ((file-regex    (or (plist-get args :files) tangld-build-file-regex))
         (exclude-regex (or (plist-get args :exclude)
                            tangld-build-file-exclude-regex))
         (include-regex (or (plist-get args :include)
                            tangld-build-file-include-regex))
         (exl-dir-regex (or (plist-get args :exclude-dir)
                            tangld-build-dir-exclude-regex))
         (root-dir      (or root (f-dirname file)))
         (file-path-dirs  (f-split (f-relative (f-dirname file) root-dir)))
         (file-name (f-filename file)))
        ;;;; debugging ;;;;
    (tangld-log-message 4 "filter: args %s" args)
    (tangld-log-message 4 "filter: the files property is %s"
                        (plist-get args :files))
    (tangld-log-message 4 "filter: applying filters to %s in %s" file root)
    (if (f-file? file)
        (tangld-log-message 4 "        - PASS: %s is a file" file)
      (tangld-log-message 4 "        - FAIL: %s is not a file" file))
    (if (-none? #'(lambda (dir)
                    (tangld-log-message 4 "        -      testing dir %s" dir)
                    (s-matches? exl-dir-regex dir))
                file-path-dirs)
        (tangld-log-message 4 "        - PASS: No directories match exclusion"
                            exl-dir-regex)
      (tangld-log-message 4 "        - FAIL: A directory matches exclusion"
                          exl-dir-regex))

    (tangld-log-message 4 "         +----------------------")
    (if (not (s-matches? exclude-regex file-name))
        (tangld-log-message
         4
         "        |  - PASS: %s does not match exclusion %s"
         file exclude-regex)
      (tangld-log-message
       4
       "        |  - FAIL: %s matches exclusion %s"
       file exclude-regex))

    (if (s-matches? include-regex file-name)
        (tangld-log-message
         4
         "        |  - PASS: %s matches inclusion %s"
         file include-regex)
      (tangld-log-message
       4
       "        |  - FAIL: %s does not match inclusion %s"
       file include-regex))

    (if (or (not (s-matches? exclude-regex file-name))
            (s-matches? include-regex file-name))
        (tangld-log-message
         4
         "        - PASS: %s matches ex/inclusion test"
         file)
      (tangld-log-message
       4
       "        - FAIL: %s does not match ex/inclusion test"
       file))
    ;; multiple tests on the file, all need to be true to return true
    (and
     ;; it is a file
     (f-file? file)
     ;; none of the directories below the root match the directory exclusion
     (-none? #'(lambda (dir)
                 (s-matches? exl-dir-regex dir))
             file-path-dirs)
     ;; the file name does not match the filename exclusion
     (or (not (s-matches? exclude-regex file-name))
         ;; it matches the file inclusion (canceling the exclusion)
         (s-matches? include-regex file-name)))))

(defun tangld--find-files (dir &rest args)
  "Collect all files in DIR.
The files are passed to `tangld-build-file-filter'."
  (let ((recurse-p (plist-get args :recurse)))
    (f-entries dir (lambda (file)
                     (tangld-build-file-filter file dir args) recurse-p))))

(provide 'tangld-build)

;;; tangld-build.el ends here
