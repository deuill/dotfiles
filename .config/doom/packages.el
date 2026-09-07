;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Add-ons.
(package! drag-stuff            :pin "d49fe376d24f0f8ac5ade67b6d7fccc2487c81db")
(package! shr-tag-pre-highlight :pin "f2b390a6297a9bf6a4527f79527f582a1ecced66")
(package! sqlite-mode-extras    :pin "83881ac1298eb15aaded2d579b59d7d9d25e403b")

;; Major modes.
(package! capnp-mode      :pin "0ce58429c34916249536c8d61602822844466bdf")
(package! devdocs-browser :pin "a49adc1f20b745338db34b729a7c94394c1c1689")
(package! go-playground   :pin "5726251414d3d7cc05fd54566ee9149808501574")
(package! pr-review       :pin "938db766007f3444a2899b2457d9e2f4b4ffbebf")
(package! protobuf-mode   :pin "138451296bf4101f992faa215a1899f3b9ec29e7")
(package! rfc-mode        :pin "0ab3e0b5eca45e7baaea748063b8590d08b55789")
(package! systemd         :pin "8742607120fbc440821acbc351fda1e8e68a8806")

;; Disabled packages.
(package! code-review :disable t)
