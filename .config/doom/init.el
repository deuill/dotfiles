;;; .doom.d/init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       (ivy +prescient +fuzzy +icons)

       :checkers
       syntax
       grammar

       :ui
       doom
       fill-column
       hl-todo
       hydra
       modeline
       nav-flash
       neotree
       ophints
       (popup +all +defaults)
       vc-gutter
       (window-select +numbers +switch-window)
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       multiple-cursors
       rotate-text
       snippets
       word-wrap

       :emacs
       electric
       ibuffer
       vc

       :term
       eshell

       :tools
       debugger
       editorconfig
       (eval +overlay)
       (lookup +docsets)
       (lsp +peek)
       magit
       rgb

       :lang
       cc
       data
       emacs-lisp
       (go +lsp)
       javascript
       markdown
       (org +dragndrop +present)
       php
       python
       rest
       sh
       web

       :config
       (default +bindings +smartparens))