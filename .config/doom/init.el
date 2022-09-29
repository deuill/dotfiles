;;; .doom.d/init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +childframe)
       (vertico +icons)

       :checkers
       (syntax +childframe)
       spell

       :ui
       doom
       hl-todo
       hydra
       ligatures
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       (treemacs +lsp)
       vc-gutter
       (window-select +numbers +switch-window)
       workspaces
       zen

       :editor
       (evil +everywhere)
       (format +onsave)
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
       eshell

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
       (lua +lsp)
       markdown
       (org +dragndrop +present)
       (php +lsp)
       (python +lsp)
       rest
       (rust +lsp)
       sh
       web
       yaml

       :config
       (default +bindings +smartparens))

(after! doom-cli-env
  ;; Allow SSH-related environment variables to be set in persistent configuration.
  (add-to-list 'doom-env-allow "^SSH_"))
