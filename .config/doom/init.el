;;; .doom.d/init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +icons)
       (vertico +icons)

       :checkers
       (syntax +childframe)
       spell

       :ui
       doom
       hl-todo
       hydra
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       (treemacs +lsp)
       (vc-gutter +diff-hl +pretty)
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
       (dired +icons)
       electric
       ibuffer
       undo
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
       (magit +forge)
       pdf
       rgb
       tree-sitter

       :lang
       (cc +tree-sitter)
       data
       emacs-lisp
       (go +lsp +tree-sitter)
       (javascript +tree-sitter)
       (lua +lsp +tree-sitter)
       markdown
       (org +dragndrop +present)
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
       (default +bindings +smartparens))

(after! doom-cli-env
  ;; Allow SSH-related environment variables to be set in persistent configuration.
  (add-to-list 'doom-env-allow "^SSH_"))
