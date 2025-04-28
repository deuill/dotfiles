;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Add-ons.
(package! dape)
(package! drag-stuff)
(package! shr-tag-pre-highlight)
(package! sqlite-mode-extras)

;; Major modes.
(package! capnp-mode)
(package! code-review :pin "fba8fe3")
(package! devdocs-browser)
(package! pr-review)
(package! protobuf-mode)
(package! rfc-mode)
(package! systemd)

;; Minor modes.
(package! flycheck-golangci-lint :pin "14bf143")

;; Disabled packages.
(package! code-review :disable t)
