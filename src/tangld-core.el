;;; tangld-core.el --- literate config development environment -*- lexical-binding: t; -*-
;;

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
;; A Literate Programming Environment for configuration files and scripts

;; tangld is an Emacs package that provides "dotfiles management" features
;; using Literate Programming paradigms.  Using org-mode files with source
;; blocks and the tangle functionality, Emacs can be used as an IDE to
;; document, build, and install configuration files, scripts and other
;; files on a system.  More details are available in the README.org file.

;;; Code:


;; Constants
(defconst user-data-dir (or (getenv "XDG_DATA_HOME")
                            (getenv "LOCALAPPDATA")
                            (file-name-concat (getenv "HOME") ".local" "share"))
  "Base directory where user level application data is stored")

(defconst user-config-dir (or (getenv "XDG_CONFIG_HOME")
                              (getenv "APPDATA")
                              (file-name-concat (getenv "HOME") ".config"))
  "Base directory where user level application configurations are stored")

;; Customizable options

(defgroup tangld nil
  "Literate Config Manager."
  :group 'development
  :prefix "tangld-")

(defcustom tangld-data-dir (if user-data-dir
                               (file-name-concat user-data-dir "tangld")
                             (file-name-concat (locate-user-emacs-file "tangld")
                                               "data"))
  "Directory where user's tangld data files are stored."
  :type 'string
  :group 'tangld)

(defcustom tangld-config-dir (if user-config-dir
                                 (file-name-concat user-config-dir "tangld")
                               (file-name-concat
                                (locate-user-emacs-file "tangld") "config"))
  "Directory where user's tangld data files are stored."
  :type 'string
  :group 'tangld)


(defcustom tangld-confirm-on-eval nil
  "If non-nil, Emacs will ask before evaluating code in source blocks."
  :group 'tangld
  :type '(choice
          (const :tag "Don't ask" nil)
          (const :tag "Ask" t)))

(defcustom tangld-add-src-return-link-comments t
  "Add a link to the source code block in the output."
  :group 'tangld
  :type 'boolean)



;; -----------------------------------------------------------------------------

;;;; Projects

(provide 'tangld-core)

;;; tangld-core.el ends here
