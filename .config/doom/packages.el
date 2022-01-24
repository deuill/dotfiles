;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
(package! kubernetes :pin "de4e176d9cc3b2ed37bc047496594a30295d6420")
(package! kubernetes-evil :pin "de4e176d9cc3b2ed37bc047496594a30295d6420")
