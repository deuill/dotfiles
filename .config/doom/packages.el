;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
(package! kubernetes :pin "7a5ec79c51698123098af58dea2d10bf15c5a82f")
(package! kubernetes-evil :pin "7a5ec79c51698123098af58dea2d10bf15c5a82f")
