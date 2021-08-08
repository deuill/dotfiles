;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
(package! kubernetes :pin "ffdae05d4d0e83be5c6884326b69a8ca83f2ae2b")
(package! kubernetes-evil :pin "ffdae05d4d0e83be5c6884326b69a8ca83f2ae2b")
