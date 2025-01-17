;; -*- mode: emacs-lisp -*-

;;; metadata

(package "tangld"
         "0.1.0"
         "A Literate Programming Environment for configuration files.

tangld is an Emacs package that provides \"dotfiles management\"
features using Literate Programming paradigms.  Using org-mode files
with source code blocks and the amazing tangle functionality, Emacs
can be used as an IDE to document, build, and install configuration
files and scripts on a system.  More details are available in the
README.org file"
)

(website-url "https://github.com/aldrichtr/tangld")
(author "Timothy Aldrich" "timothy.r.aldrich@gmail.com")
(keywords "hypermedia" "org" "noweb")

;;; contents


(files '("src/tangld.el"
         "src/tangld-build.el"
         "src/tangld-checksum.el"
         "src/tangld-log.el"))

;;; tests

(script "test" "echo \"Error: no test specified\" && exit 1")

;;; dependencies

(source "gnu")
(source "melpa")


(depends-on "emacs" "25.1")
(depends-on "org" "8")
(depends-on "f" "0.20.0")
(depends-on "s" "1.12.0")

(development
 (depends-on "buttercup" "1.36"))
