(require 'ert)

;; Utils

(ert-deftest tangld-dir ()
  (should (string= (f-abbrev (tangld-dir 'system)) "~/"))
  (should (string= (f-abbrev (tangld-dir 'root)) "~/.tangld"))
  (should (string= (f-abbrev (tangld-dir 'source)) "~/.tangld/src"))
  (should (string= (f-abbrev (tangld-dir 'install)) "~/.tangld/dotfiles"))
  (should (string= (f-abbrev (tangld-dir 'build)) "~/.tangld/build")))

(ert-deftest tangld-with-dirs ()
  (should-not (cl-set-difference (tangld-with-dirs (list .system .root .source .install .build))
				 (mapcar #'tangld-dir (mapcar #'car tangld-project-dirs))
				 :test #'string=)))

(ert-deftest tangld-with-let ()
  (should (equal (tangld-let ((one 1)) (list .root one))
		 (list (f-abbrev (tangld-dir 'root)) 1))))

(ert-deftest tangld-with-let* ()
  (should (equal (tangld-let ((one 1) (sum (+ one 2))) (list .root one sum))
		 (list (tangld-dir 'root) 1 3))))

(ert-deftest tangld-relative-lambda ()
  (should (tangld-with-dirs (f-abbrev (funcall (tangld-relative-lambda .build) (f-expand "hello.txt" .system))))
	  "~/.tangld/build/hello.txt"))

(ert-deftest tangld-tangle-reroute ()
  (should (tangld-with-tangle-reroute root (org-babel-tangle-file))))

(ert-deftest tangld-message ()
  (should-not (let (tangld-verbose-p) (tangld-message "hello")))
  (should (string= (let (tangld-verbose-p t) (tangld-message "hello")) "[tangld] hello")))

;; Init

(ert-deftest tangld-init ()
  (should (tangld-init))
  (should (tang)))

;; Building
