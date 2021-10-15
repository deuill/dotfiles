;;; cli.el -*- lexical-binding: t; -*-

(defcligroup! "Custom"
  "Custom, user-defined commands."
  (defcli! standalone (mode-or-file)
    "Spawn an emacsclient window with a standalone buffer for the given mode or file."
    (throw 'exit (list "emacsclient" "--create-frame" "--eval" (concat "(custom--with-standalone-buffer \"" mode-or-file "\")")))))
