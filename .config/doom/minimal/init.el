;;; .doom.d/init.minimal.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       vertico

       :checkers
       syntax
       (spell +hunspell)

       :ui
       doom
       hydra
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
       electric
       ibuffer
       undo

       :term
       eshell

       :tools
       (lookup +dictionary +docsets +offline)

       :lang
       markdown

       :os
       (tty +osc)

       :config
       (default +bindings +smartparens))
