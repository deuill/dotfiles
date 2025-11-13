;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Add-ons.
(package! drag-stuff)
(package! shr-tag-pre-highlight)
(package! sqlite-mode-extras)

;; Major modes.
(package! capnp-mode)
(package! consult-gh)
(package! devdocs-browser)
(package! go-playground)
(package! pr-review)
(package! protobuf-mode)
(package! rfc-mode)
(package! systemd)

;; Disabled packages.
(package! code-review :disable t)
