;;; .doom.d/init.minimal.el -*- lexical-binding: t; -*-

(doom! :completion
       corfu
       vertico

       :checkers
       syntax
       spell

       :ui
       doom
       (modeline +light)
       nav-flash
       ophints
       (popup +all +defaults)
       (window-select +numbers +switch-window)
       zen

       :editor
       (evil +everywhere)
       fold
       multiple-cursors
       rotate-text
       word-wrap

       :emacs
       (dired +icons)
       electric
       undo

       :term
       eshell

       :lang
       markdown

       :os
       (tty +osc)

       :config
       (default +bindings +smartparens))

(add-hook! 'emacs-startup-hook
  (setq confirm-kill-emacs nil)
  (add-hook! '(kill-buffer-hook quit-window-hook) :local
    (save-buffers-kill-terminal t)))
