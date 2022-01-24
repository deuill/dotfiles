;;; .doom.d/init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       (vertico +icons)

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
       (dired +ranger +icons)
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
       pdf
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

(when noninteractive
  ;; Allow SSH-related environment variables to be set in persistent configuration.
  (add-to-list 'doom-env-whitelist "^SSH_"))
