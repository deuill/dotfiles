;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Install Kubernetes client support.
(package! kubel :pin "5d5639cac5d98bca74cad44b5a1e128df77050bd0a4ddb41698bb8aa02b18e07ac509")
(package! kubel-evil :pin "5d5639cac5d")
