;;; .doom.d/init.el -*- lexical-binding: t; -*-

(doom! :completion
       (corfu +orderless +icons)
       (vertico +icons)

       :checkers
       (syntax +childframe)
       spell

       :ui
       dashboard
       doom
       (emoji +github)
       hl-todo
       ligatures
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       treemacs
       (vc-gutter +diff-hl +pretty)
       (window-select +numbers +switch-window)
       workspaces
       zen

       :editor
       (evil +everywhere)
       (format +onsave +lsp)
       file-templates
       fold
       multiple-cursors
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired +icons)
       electric
       (ibuffer +icons)
       undo
       vc

       :term
       vterm

       :tools
       debugger
       docker
       editorconfig
       (eval +overlay)
       (lookup +dictionary +offline)
       (lsp +eglot)
       (magit +forge)
       pdf
       (terraform +lsp)

       :lang
       cc
       data
       emacs-lisp
       (go +lsp)
       javascript
       (lua +lsp)
       markdown
       (org +pretty)
       (php +lsp)
       (python +lsp)
       rest
       (rust +lsp)
       (scheme +guile)
       (sh +fish +lsp)
       web
       yaml

       :os
       (tty +osc)

       :config
       (default +bindings +gnupg +smartparens))

(after! doom-cli-env
  ;; Allow SSH-related environment variables to be set in persistent configuration.
  (add-to-list 'doom-env-allow "^SSH_"))
