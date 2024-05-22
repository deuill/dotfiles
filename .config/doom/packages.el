;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Add-ons.
(package! shr-tag-pre-highlight)
(package! flycheck-golangci-lint :pin "91c59b128aa6f719069cfb3e5df77588691a3e14")

;; Major modes.
(package! systemd)
(package! capnp-mode)
