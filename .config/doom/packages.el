;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
(package! kubernetes :pin "ea2a1ab91347740d3006c9591064d0fc77180ab7")
(package! kubernetes-evil :pin "ea2a1ab91347740d3006c9591064d0fc77180ab7")
