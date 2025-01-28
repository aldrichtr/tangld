;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'projectile)

(defvar project-root (projectile-project-root))
(defvar tests-dir (file-name-concat project-root "tests"))
(defvar test-data-dir (file-name-concat tests-dir "data"))


(describe
    "Default values are set"
  (it "Pre-build hook is empty"
    (expect tangld-pre-build-hook :to-be nil)))
