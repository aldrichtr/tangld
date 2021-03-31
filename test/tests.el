;; The tangld interactive functions we want to test are full of side-effects
;; like creating files, and directories. It easier and safer to test these
;; commands if they'd only return values instead of creating/deleting files and
;; directories. Thankfully, we can use advice.

(ert-deftest tangld--expanded-project-dir-paths ()
  (should (tangld::expanded-project-dir-paths ))
  )
