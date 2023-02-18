;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Core package overrides.
(package! eshell-did-you-mean :disable t)
(package! transient :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440")
(package! with-editor :pin "391e76a256aeec6b9e4cbd733088f30c677d965b")

;; Add-ons.
(package! shr-tag-pre-highlight :pin "931c447bc0d6c134ddc9657c664eeee33afbc54d")

;; Major modes.
(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
