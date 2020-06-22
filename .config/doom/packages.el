;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Install OpenAPI support.
(package! openapi-yaml-mode
  :recipe (:host github :repo "magoyette/openapi-yaml-mode")
  :pin "f3d4b5b")

;; Install Kubernetes client support.
;; (package! kubernetes :pin "cc33d8")
;; (package! kubernetes-evil :pin "cc33d8")
