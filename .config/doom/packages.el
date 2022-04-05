;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
(package! kubernetes :pin "d52ad7dacf17b659060e52d5e3318cafd7946616")
(package! kubernetes-evil :pin "d52ad7dacf17b659060e52d5e3318cafd7946616")
(package! eshell-did-you-mean :disable t)
