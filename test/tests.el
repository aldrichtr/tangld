(require 'ert)
(require 'loopy "./loopy.el")

(ert-deftest tangld--expanded-project-dir-paths ()
  (should (tangld::expanded-project-dir-paths ))
  )
