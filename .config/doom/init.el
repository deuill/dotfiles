;;; .doom.d/init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       (ivy +prescient +fuzzy +icons)

       :checkers
       (syntax +childframe)
       spell

       :ui
       doom
       fill-column
       hl-todo
       hydra
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       treemacs
       vc-gutter
       (window-select +numbers +switch-window)
       workspaces
       zen

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
       (undo +tree)
       vc

       :term
       vterm

       :tools
       (debugger +lsp)
       docker
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (lsp +peek)
       magit
       rgb

       :lang
       cc
       data
       emacs-lisp
       (go +lsp)
       javascript
       lua
       markdown
       (org +dragndrop +present)
       (php +lsp)
       python
       rest
       sh
       web
       yaml

       :config
       (default +bindings +smartparens))
