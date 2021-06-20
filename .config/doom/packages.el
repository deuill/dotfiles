;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
(package! kubernetes :pin "85afc58c19ca813b797eaf7c4ab7629f4e4657d9")
(package! kubernetes-evil :pin "85afc58c19ca813b797eaf7c4ab7629f4e4657d9")
