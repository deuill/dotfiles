;;; config.el -*- lexical-binding: t; -*-

;;;
;;; Includes and required libraries.
;;;

(load! "custom/custom")

;;;
;;; UI configuration.
;;;

;; Disable blinking cursor.
(blink-cursor-mode -1)

;; Show indentation whitespace characters.
(setq whitespace-style '(face trailing))
(global-whitespace-mode 1)

;; Hide fringes.
(set-fringe-style nil)

;; Set default values for UI parameters.
(setq-default
  ;; Default theme.
  doom-theme 'doom-monokai-pro

  ;; Font definitions.
  doom-font                (font-spec :family "Iosevka"        :size 24 :weight 'light)
  doom-big-font            (font-spec :family "Iosevka"        :size 28 :weight 'light)
  doom-variable-pitch-font (font-spec :family "IBM Plex Sans"  :size 22 :weight 'light)
  doom-serif-font          (font-spec :family "IBM Plex Serif" :size 22 :weight 'light)
  doom-unicode-font        (font-spec :family "Iosevka"        :size 24 :weight 'light)

  ;; Column used as limit for various modes.
  fill-column 100

  ;; Have which-key behave more sanely.
  which-key-idle-delay 0.2
  which-key-max-description-length 35
  which-key-prefix-prefix ""

  ;; Have Page Up/Down move to start/end of buffer when possible.
  scroll-error-top-bottom t

  ;; Stretch cursor to fill width of character underneath.
  x-stretch-cursor t

  ;; Disable line numbers.
  display-line-numbers-type nil

  ;; Set up mode-line.
  doom-modeline-persp-name t
  doom-modeline-vcs-max-length 30

  ;; Add horizontal margin to windows.
  left-margin-width 1
  right-margin-width 1)

(custom-set-faces!
  ;; Set colors consistent with Base16-Eighties theme.
  '(default              :background "#2d2d2d")
  '(hl-line              :background "#323232")
  '(mode-line            :background "#282828")
  '(vertical-border      :background "#282828" :foreground "#282828")
  '(solaire-default-face :background "#282828")
  '(solaire-hl-line-face :background "#323232")

  ;; Have whitespace blend into background until highlighted.
  '(whitespace-space :background "#2d2d2d" :foreground "#2d2d2d")
  '(whitespace-tab   :background "#2d2d2d" :foreground "#2d2d2d")

  ;; Set heading sizes for HTML documents.
  '(shr-h1 :height 1.9 :weight bold)
  '(shr-h2 :height 1.6 :weight bold)
  '(shr-h3 :height 1.4 :weight bold)
  '(shr-h4 :height 1.2 :weight bold)
  '(shr-h5 :height 1.1 :weight bold)
  '(shr-h6 :height 1.1 :weight normal))

;;;
;;; Package-specific configuration.
;;;

(setq-default shell-file-name "/usr/bin/fish")

(after! dash-docs
  (setq dash-docs-docsets-path "~/.local/share/docsets"
        dash-docs-browser-func #'eww))

(after! docker-tramp
  (setq docker-tramp-use-names t))

(after! evil
  ;; Transpose lines with J/K when in visual mode.
  (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

(after! eshell
  (setq eshell-banner-message "")
  (set-popup-rule! "^\\*eshell\\*" :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil))

(after! eww
  (setq shr-use-fonts t
        shr-discard-aria-hidden t
        shr-max-width fill-column
        shr-hr-line ?━
        shr-bullet "• "
        eww-search-prefix "https://lite.duckduckgo.com/lite?q=")
  (defun shr-add-font (start end type) (+custom/shr-add-font start end type))
  (set-popup-rule! "^\\*eww\\*" :side 'right :select t :quit 'nil :slot 0 :width (+ fill-column 4)))

(after! info
  (set-popup-rule! "^\\*info\\*" :side 'right :select t :quit 'current :slot 0 :width (+ fill-column 4)))

(after! (json-mode evil)
  (evil-define-key 'normal json-mode-map (kbd "<tab>") 'evil-toggle-fold))

(after! kubernetes-overview
  (setq kubernetes-kubectl-executable "kubectl-socks"
        kubernetes-poll-frequency 15
        kubernetes-redraw-frequency 15)
  (load-library "kubernetes-evil")
  (set-popup-rule! "^\\*kubernetes" :ignore t))

(after! lsp-mode
  (setq lsp-auto-guess-root t
        lsp-enable-file-watchers nil
        lsp-log-max nil))

(after! lsp-php
  (setq lsp-serenata-server-path "/usr/bin/serenata")
  (lsp-register-custom-settings '(("intelephense.environment.includePaths" ["._lsp-include"]))))

(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-border "#757575"
        lsp-ui-doc-position 'top)
  (set-face-attribute
   'lsp-ui-doc-background nil :background "#2d2d2d"))

(after! lsp-lua
  (setq lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server"
        lsp-clients-lua-language-server-main-location "/usr/lib/lua-language-server/bin/main.lua"))

(after! magit
   (setq magit-diff-refine-hunk t
         magit-display-buffer-function #'magit-display-buffer-traditional))

(after! (magit evil)
  (evil-define-key* 'normal magit-status-mode-map (kbd "<escape>") #'magit-mode-bury-buffer))

(after! markdown
  (setq markdown-asymmetric-header t
        markdown-enable-wiki-links t
        markdown-fontify-code-blocks-natively t))

(after! org
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil)
  (remove-hook! org-mode #'org-superstar-mode))

(after! org-roam
  (setq org-roam-directory "~/Documents/Notes"))

(after! persp-mode
  (setq persp-autokill-buffer-on-remove t
        persp-autokill-persp-when-removed-last-buffer t))

(after! projectile
  (setq projectile-globally-ignored-directories (append (default-value 'projectile-globally-ignored-directories) '("vendor"))))

(after! ranger
  (setq ranger-cleanup-on-disable t
        ranger-return-to-ranger t
        ranger-hide-cursor t))

(after! shr
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))

(after! sh-script
  (setq sh-basic-offset 4))

(after! sql
  (load! "custom/sql")
  (setq sql-postgres-login-params (append (default-value 'sql-postgres-login-params) '(port :default 5432)))
  (map! :localleader
        :map sql-mode-map
        :desc "Set SQL product"   "p" #'+sql/set-product
        :desc "Start SQL session" ";" #'+sql/start
        (:prefix ("s" . "send")
          :desc "Send buffer"     "b" #'sql-send-buffer
          :desc "Send region"     "r" #'sql-send-region
          :desc "Send string"     "s" #'sql-send-string
          :desc "Send paragraph"  "p" #'sql-send-paragraph))
  (advice-add 'sql-add-product :after #'+sql--populate-product-list)
  (advice-add 'sql-del-product :after #'+sql--populate-product-list)
  (+sql--populate-product-list))

(after! (sql lsp-sqls)
  (setq lsp-sqls-workspace-config-path nil))

(after! transient
  ;; Close transient windows with 'Escape' key.
  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq))

(after! vterm
  (set-popup-rule! "^\\*vterm\\*" :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil))

(after! (:or man woman)
  (set-popup-rule! "^\\*\\(?:Wo\\)?Man " :side 'right :select t :quit 'current :slot 0 :width (+ fill-column 4)))

(after! writeroom-mode
  (setq writeroom-width 100
        writeroom-fullscreen-effect 'maximized
        writeroom-bottom-divider-width 0
        writeroom-maximize-window nil)
  (when (modulep! :ui zen)
    (setq +zen-text-scale 0)))

(after! (yaml-mode evil)
  (evil-define-key 'normal yaml-mode-map (kbd "<tab>") 'evil-toggle-fold))

;;;
;;; Hooks and mode-specific configuration.
;;;

(add-hook! artist-mode
  (evil-emacs-state +1))

(add-hook! (doc-mode org-mode markdown-mode)
  (setq indent-tabs-mode nil)
  (flycheck-mode t)
  (writeroom-mode t)
  (visual-line-mode t)
  (display-fill-column-indicator-mode 0))

(add-hook! eww-mode
  (writeroom-mode t)
  (display-fill-column-indicator-mode 0))

(add-hook! 'evil-visual-state-entry-hook
  (setq-local whitespace-style '(face tabs tab-mark spaces space-mark trailing))
  (whitespace-turn-off)
  (whitespace-turn-on-if-enabled))

(add-hook! 'evil-visual-state-exit-hook
  (kill-local-variable 'whitespace-style)
  (whitespace-turn-off)
  (whitespace-turn-on-if-enabled))

(add-hook! git-commit-mode
  (setq indent-tabs-mode nil))

(add-hook! 'go-mode-lsp-hook
  (flycheck-add-next-checker 'lsp 'golangci-lint))

(add-hook! Info-mode
  (writeroom-mode t))

(add-hook! json-mode
  (hs-minor-mode))

(add-hook! 'kill-buffer-hook
  '+custom--add-buffer-to-killed-list-h)

(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))

(add-hook! markdown-mode
  (auto-fill-mode t))

(add-hook! pdf-view-mode
  (setq mode-line-format nil))

(add-hook! php-mode
  (php-enable-psr2-coding-style)
  (setq fill-column 100))

(add-hook! prog-mode
  (setq fill-column 100
        show-trailing-whitespace t)
  (display-fill-column-indicator-mode))

(add-hook! sql-mode
  (after! lsp-sqls (lsp-deferred)))

;;;
;;; Keybindings.
;;;

;;;
;;; These keybindings override default keybindings used for Doom. The
;;; following principles apply:
;;;
;;; For base keys, mnemonics are important. Use `b' for 'Buffer', `w'
;;; for 'Window', etc.
;;;
;;; For nested keys, the same key as the parent should do the most
;;; obvious thing.
;;;
;;; Keep nested keys as close to their parents as possible, or use
;;; home row for other hand if no other option exists.
;;;
;;; Do not expose the same functionality twice; if the function is
;;; extremely common or important, promote to root. Otherwise, nest
;;; as needed.
;;;
;;; Unsafe commands, i.e. commands that change the state of buffers
;;; or session in ways that are difficult to revert, should be made
;;; less accessible, e.g. by using capital keys.
;;;

(when (modulep! :editor evil)
  (map! :desc "Jump back in position history"    "<mouse-8>" #'better-jumper-jump-backward
        :desc "Jump forward in position history" "<mouse-9>" #'better-jumper-jump-forward)

  (map! :desc "Go to previous buffer" :n "<S-left>"  #'evil-prev-buffer
        :desc "Go to next buffer"     :n "<S-right>" #'evil-next-buffer)

  (map! :leader
        :desc "Open shell here"       ";"   (cond ((modulep! :term vterm)  #'vterm)
                                                  ((modulep! :term eshell) #'eshell))

        :desc "Switch to last buffer" "TAB" (cond ((modulep! :ui workspaces) #'+custom/alternate-buffer-in-workspace)
                                                  (t                         #'evil-switch-to-windows-last-buffer))

        :desc "Resume last search"    "SPC" (cond ((modulep! :completion vertico) #'vertico-repeat)
                                                  ((modulep! :completion ivy)     #'ivy-resume)
                                                  ((modulep! :completion helm)    #'helm-resume))

        "."   nil
        ","   nil
        "<"   nil
        "`"   nil
        "'"   nil
        "RET" nil

        :desc "Switch to 1st window"  "1"   #'winum-select-window-1
        :desc "Switch to 2nd window"  "2"   #'winum-select-window-2
        :desc "Switch to 3rd window"  "3"   #'winum-select-window-3
        :desc "Switch to 4th window"  "4"   #'winum-select-window-4
        :desc "Switch to 5th window"  "5"   #'winum-select-window-5
        :desc "Switch to 6th window"  "6"   #'winum-select-window-6
        :desc "Switch to 7th window"  "7"   #'winum-select-window-7
        :desc "Switch to 8th window"  "8"   #'winum-select-window-8
        :desc "Switch to 9th window"  "9"   #'winum-select-window-9
        :desc "Switch to side window" "0"   (cond ((modulep! :ui neotree)  #'+neotree/expand-or-open)
                                                  ((modulep! :ui treemacs) #'treemacs-select-window))

        "x" nil
        "X" nil

        (:prefix "b"
          :desc "Kill all buffers"             "D"   #'doom/kill-all-buffers
                                               "k"   nil
                                               "K"   nil
                                               "l"   nil
          :desc "Paste and replace buffer"     "P"   #'+custom/paste-buffer
          :desc "Rename buffer"                "r"   #'rename-buffer
          :desc "Revert buffer"                "R"   #'+custom/safe-revert-buffer
          :desc "Reopen killed buffer"         "u"   #'+custom/reopen-killed-buffer
          (:when (modulep! :emacs undo +tree)
            :desc "Open undo tree"             "U"   #'undo-tree-visualize)
          :desc "Pop up scratch buffer"        "x"   #'doom/open-scratch-buffer
          :desc "Switch to scratch buffer"     "X"   #'doom/switch-to-scratch-buffer)

        (:prefix "f"
                                               "c"   nil
          :desc "Find file as root"            "e"   #'doom/sudo-find-file
          :desc "Open current file as root"    "E"   #'doom/sudo-this-file
                                               "l"   nil
                                               "p"   nil
                                               "P"   nil
                                               "u"   nil
                                               "U"   nil)

        (:prefix "g"
          (:when (modulep! :ui vc-gutter)
                                               "]"   nil
                                               "["   nil
                                               "."   nil
            :desc "Jump to next hunk"          "n"   #'git-gutter:next-hunk
            :desc "Jump to previous hunk"      "p"   #'git-gutter:previous-hunk
            :desc "Revert hunk"                "r"   #'git-gutter:revert-hunk
            :desc "Git stage hunk"             "s"   #'git-gutter:stage-hunk
            :desc "Git time machine"           "t"   #'git-timemachine-toggle)
          (:when (modulep! :tools magit)
                                               "/"   nil
                                               "'"   nil
                                               "D"   nil
            :desc "Git fetch"                  "f"   #'magit-fetch
            :desc "Git pull"                   "F"   #'magit-pull
                                               "f"   nil))

        "i" nil
        "n" nil
        "o" nil

        (:prefix "p"
          (:when (modulep! :ui workspaces)
            :desc "Switch to last project"     "TAB" #'+workspace/other
            :desc "Switch to next project"     "]"   #'+workspace/switch-right
            :desc "Switch to previous project" "["   #'+workspace/switch-left
            :desc "Switch to 1st project"      "1"   #'+workspace/switch-to-0
            :desc "Switch to 2nd project"      "2"   #'+workspace/switch-to-1
            :desc "Switch to 3rd project"      "3"   #'+workspace/switch-to-2
            :desc "Switch to 4th project"      "4"   #'+workspace/switch-to-3
            :desc "Switch to 5th project"      "5"   #'+workspace/switch-to-4
            :desc "Switch to 6th project"      "6"   #'+workspace/switch-to-5
            :desc "Switch to 7th project"      "7"   #'+workspace/switch-to-6
            :desc "Switch to 8th project"      "8"   #'+workspace/switch-to-7
            :desc "Switch to 9th project"      "9"   #'+workspace/switch-to-8)
                                               "."   nil
                                               ">"   nil
                                               "!"   nil
          :desc "Open shell in project"        ";"   (cond ((modulep! :term vterm) #'+vterm/toggle)
                                                           ((modulep! :term eshell) #'+eshell/toggle))
                                               "b"   nil
                                               "c"   nil
                                               "C"   nil
                                               "d"   nil
          :desc "Delete project workspace"     "D"   #'+workspace/delete
                                               "e"   nil
          :desc "Find file in project"         "f"   #'projectile-find-file
                                               "F"   nil
                                               "g"   nil
                                               "i"   nil
                                               "k"   nil
          :desc "List project workspaces"      "l"   #'+workspace/display
          :desc "New project workspace"        "n"   #'+workspace/new
                                               "o"   nil
          :desc "Rename project workspace"     "R"   #'+workspace/rename
                                               "s"   nil
          :desc "Save project files"           "S"   #'projectile-save-project-buffers
          :desc "Toggle file tree"             "t"   (cond ((modulep! :ui neotree)  #'+neotree/open)
                                                           ((modulep! :ui treemacs) #'+treemacs/toggle))
                                               "T"   nil
          :desc "Switch project workspace"     "w"   #'+workspace/switch-to
          :desc "Remove project"               "x"   nil
                                               "X"   #'projectile-remove-known-project)

        (:prefix "s"
                                               "f"   nil
                                               "p"   nil
                                               "P"   nil
          :desc "Replace in buffer"            "r"   #'+custom/query-replace-buffer
          :desc "Replace in project"           "R"   #'projectile-replace)

        "r" nil

        (:prefix "t"
                                               "f"   nil
                                               "F"   nil
                                               "g"   nil
                                               "p"   nil
                                               "r"   nil
                                               "s"   nil
          :desc "Zoom"                         "z"   #'+hydra/text-zoom/body)))
