;;; cli.el -*- lexical-binding: t; -*-

(defcligroup! "Custom"
  "Custom, user-defined commands."
  (defcli! standalone (mode-or-file &rest args)
    "Spawn an emacsclient window with a standalone buffer for the given mode or file."
    (throw 'exit (list "emacsclient" "--create-frame" "--eval" (concat "(custom--with-standalone-buffer" (apply #'concat (mapcar (lambda (c) (concat "\"" c "\"")) (cons mode-or-file args))) ")")))))
