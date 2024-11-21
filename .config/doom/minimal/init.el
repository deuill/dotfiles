;;; .doom.d/init.minimal.el -*- lexical-binding: t; -*-

(doom! :completion
       corfu
       vertico

       :ui
       doom
       (modeline +light)
       nav-flash
       ophints
       (window-select +numbers +switch-window)
       zen

       :editor
       (evil +everywhere)
       fold
       multiple-cursors
       rotate-text
       word-wrap

       :emacs
       electric
       undo

       :term
       eshell

       :tools
       magit

       :lang
       markdown

       :os
       (tty +osc)

       :config
       (default +bindings))

(add-hook! 'emacs-startup-hook
  (setq confirm-kill-emacs nil)
  (add-hook! '(kill-buffer-hook quit-window-hook) :local
    (save-buffers-kill-terminal t)))
