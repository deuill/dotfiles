;;; .doom.d/init.el -*- lexical-binding: t; -*-

(doom! :completion
       (corfu +icons)
       (vertico +icons)

       :checkers
       (syntax +childframe)
       spell

       :ui
       doom
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
       (dired +icons +ranger)
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
       (lookup +dictionary +docsets +offline)
       (lsp +eglot)
       (magit +forge)
       pdf
       tree-sitter

       :lang
       (cc +tree-sitter)
       data
       emacs-lisp
       (go +lsp +tree-sitter)
       (javascript +tree-sitter)
       (lua +lsp +tree-sitter)
       markdown
       (org +pretty)
       (php +lsp +tree-sitter)
       (python +lsp +tree-sitter)
       rest
       (rust +lsp +tree-sitter)
       (sh +fish +lsp +tree-sitter)
       (web +tree-sitter)
       (yaml +tree-sitter)

       :os
       (tty +osc)

       :config
       (default +bindings +gnupg +smartparens))

(after! doom-cli-env
  ;; Allow SSH-related environment variables to be set in persistent configuration.
  (add-to-list 'doom-env-allow "^SSH_"))
