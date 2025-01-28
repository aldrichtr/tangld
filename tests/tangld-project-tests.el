;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'ht)
(require 'tangld)

(describe
 "tangld Project tests"
 (it "Creates a new, empty, tangld projects table"
     (tangld-projects-init)
     (expect (ht-p tangld-projects) :to-be t)
     (expect (ht-empty-p tangld-projects) :to-be-truthy))
 (it "A known-good project is valid"
     (let* ((id (tangld-project--generate-id))
            (sources (list "~/projects/init.org"))
            (project (ht-create)))
       (ht-set! project 'id id)
       (ht-set! project 'source sources)
       (expect (tangld-project-validate project) :to-be-truthy))))
