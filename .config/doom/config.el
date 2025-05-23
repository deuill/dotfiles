;;; config.el -*- lexical-binding: t; -*-

;;;
;;; Includes and required libraries.
;;;

(load! "custom/custom" "~/.config/doom")

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
 doom-big-font            (font-spec :family "Iosevka"        :size 32 :weight 'light)
 doom-variable-pitch-font (font-spec :family "IBM Plex Sans"  :size 24 :weight 'light)
 doom-serif-font          (font-spec :family "IBM Plex Serif" :size 24 :weight 'light)
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
 doom-modeline-vcs-max-length 30)

(custom-set-faces!
  ;; Set colors consistent with Base16-Eighties theme.
  '(default                           :background "#2d2d2d")
  '(hl-line                           :background "#323232")
  '(mode-line                         :background "#282828")
  '(vertical-border                   :background "#282828" :foreground "#282828")
  '(solaire-default-face              :background "#282828")
  '(solaire-hl-line-face              :background "#323232")
  '(treemacs-window-background-face   :background "#323232")
  `(tree-sitter-hl-face:function.call :foreground ,(doom-color 'functions))
  '(eglot-highlight-symbol-face       :inherit region)

  ;; Have whitespace blend into background until highlighted.
  '(whitespace-space :background "#2d2d2d" :foreground "#2d2d2d")
  '(whitespace-tab   :background "#2d2d2d" :foreground "#2d2d2d")

  ;; Set heading sizes for HTML documents.
  '(shr-h1 :height 1.6 :weight bold)
  '(shr-h2 :height 1.3 :weight bold)
  '(shr-h3 :height 1.2 :weight bold)
  '(shr-h4 :height 1.15 :weight bold)
  '(shr-h5 :height 1.1 :weight bold)
  '(shr-h6 :height 1.1 :weight normal))

;;;
;;; Package-specific configuration.
;;;

(set-popup-rule! "^\\*doom:scratch" :side 'right :select t :quit 'other :slot 0 :width (+ fill-column 4))

(setq-default auth-sources '("secrets:login")
              browse-url-browser-function 'eww-browse-url
              doom-scratch-initial-major-mode 'text-mode
              shell-file-name "/bin/bash")

(after! code-review
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer
        code-review-auth-login-marker 'forge)
  (set-popup-rule! "^\\*Code Review" :side 'right :select t :quit 'other))

(after! dape
  (setq dape-cwd-fn 'projectile-project-root))

(after! deft
  (setq deft-directory "~/Documents/Notes"
        deft-default-extension "md")
  (defun deft () (interactive)(+custom/deft-popup))
  (set-popup-rule! "^\\*Deft\\*" :side 'right :select t :quit 'other :slot 0 :width (+ fill-column 4)))

(after! devdocs-browser
  (set-popup-rule! "^\\*devdocs-" :side 'right :select t :quit 'other :slot 0 :width (+ fill-column 4)))

(after! eglot
  (set-popup-rule! "^\\*eglot-help" :side 'bottom :select t :quit 'current :slot 0 :height 0.5))

(after! (eglot evil)
  (evil-collection-define-key 'normal 'eglot-mode-map
    "gD" 'xref-find-references))

(after! evil
  ;; Transpose lines with J/K when in visual mode.
  (define-key evil-visual-state-map "J" #'drag-stuff-down)
  (define-key evil-visual-state-map "K" #'drag-stuff-up))

(after! eshell
  (setq eshell-banner-message "")
  (set-popup-rule! "^\\*\\(?:doom:\\)eshell" :vslot -5 :select t :modeline nil :quit nil :ttl nil :height 0.25))

(after! eww
  (setq shr-use-fonts t
        shr-discard-aria-hidden nil
        shr-max-width fill-column
        shr-hr-line ?━
        shr-bullet "• "
        url-user-agent "Mozilla/5.0 (Android 14; Mobile; rv:109.0) Gecko/121.0 Firefox/121.0"
        eww-search-prefix "https://lite.duckduckgo.com/lite?q=")
  (evil-define-key* 'normal eww-mode-map (kbd "r") 'eww-reload)
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
        lsp-log-max nil)
  (delete 'lsp-terraform lsp-client-packages))

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
        magit-display-buffer-function #'magit-display-buffer-traditional
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (require 'pr-review))

(after! (magit evil)
  (evil-define-key* 'normal magit-status-mode-map (kbd "<escape>") #'magit-mode-bury-buffer))

(after! markdown
  (setq markdown-asymmetric-header t
        markdown-enable-wiki-links t
        markdown-fontify-code-blocks-natively t))

(after! org-roam
  (setq org-roam-directory "~/Documents/Notes"))

(after! persp-mode
  (setq persp-autokill-buffer-on-remove t
        persp-autokill-persp-when-removed-last-buffer t))

(after! projectile
  (setq projectile-globally-ignored-directories (append (default-value 'projectile-globally-ignored-directories) '("vendor"))))

(after! pr-review
  (setq pr-review-ghub-auth-name 'forge)
  (transient-append-suffix 'magit-merge "d"
    '("y" "Review pull request" +custom/start-pr-review))
  (after! forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +custom/start-pr-review))))

(after! ranger
  (setq ranger-cleanup-on-disable t
        ranger-return-to-ranger t
        ranger-hide-cursor t))

(after! restclient
  (set-popup-rule! "^\\*HTTP Response\\*" :side 'right :select t :quit 'current :slot 0 :width 0.5))

(after! rfc-mode
  (setq rfc-mode-directory (concat doom-data-dir "rfc/"))
  (set-popup-rule! "^\\*rfc" :side 'right :select t :quit 'other :slot 0 :width (+ fill-column 4)))

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

(after! sqlite-mode
  (require 'sqlite-mode-extras)
  (set-popup-rule! "^\\*SQLite " :ignore t)
  (map! :map sqlite-mode-map
        :nvi "h" #'sqlite-mode-extras-backtab-dwim
        :nvi "j" #'next-line
        :nvi "k" #'previous-line
        :nvi "l" #'sqlite-mode-extras-tab-dwim
        :nvi "a" #'sqlite-mode-extras-add-row
        :nvi "D" #'sqlite-mode-extras-delete-row-dwim
        :nvi "E" #'sqlite-mode-extras-execute
        :nvi "C" #'sqlite-mode-extras-compose-and-execute
        :nvi "S" #'sqlite-mode-extras-execute-and-display-select-query
        :nvi "<backtab>" #'sqlite-mode-extras-backtab-dwim
        :nvi "<tab>" #'sqlite-mode-extras-tab-dwim
        :nvi "RET" #'sqlite-mode-extras-ret-dwim
        :nvi "DEL" #'sqlite-mode-extras-delete-row-dwim))

(after! transient
  ;; Close transient windows with 'Escape' key.
  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq))

(after! vterm
  (setq vterm-shell "/usr/bin/fish")
  (set-popup-rule! "^\\*\\(?:doom:\\)vterm" :vslot -5 :select t :modeline nil :quit nil :ttl nil :height 0.25))

(after! vundo
  (setq vundo-glyph-alist vundo-ascii-symbols))

(after! (:or man woman)
  (set-popup-rule! "^\\*\\(?:Wo\\)?Man " :side 'right :select t :quit 'current :slot 0 :width (+ fill-column 4)))

(after! writeroom-mode
  (setq writeroom-width 100
        writeroom-fullscreen-effect 'maximized
        writeroom-bottom-divider-width 0
        writeroom-maximize-window nil)
  (when (modulep! :ui zen)
    (setq +zen-text-scale 0)))

(after! yaml-mode
  (add-to-list 'auto-mode-alist '("\\.bu\\'" . yaml-mode)))

(after! (yaml-mode evil)
  (evil-define-key* 'normal yaml-mode-map (kbd "<tab>") #'evil-toggle-fold))

;;;
;;; Hooks and mode-specific configuration.
;;;

(add-hook! artist-mode
  (evil-emacs-state +1))

(add-hook! code-review-mode
  (persp-add-buffer (current-buffer)))

(add-hook! csv-mode
  (setq csv-align-padding 3)
  (csv-align-mode t))

(add-hook! (doc-mode org-mode markdown-mode)
  (setq indent-tabs-mode nil)
  (flycheck-mode t)
  (writeroom-mode t)
  (visual-line-mode t)
  (display-fill-column-indicator-mode 0))

(add-hook! 'doom-load-theme-hook :append
  (unless (display-graphic-p)
    (solaire-global-mode -1)))

(add-hook! 'doom-scratch-buffer-created-hook
  (evil-define-key* 'normal 'local (kbd "q") #'(lambda () (interactive) (+popup/close nil t))))

(add-hook! eww-mode
  (writeroom-mode t)
  (display-fill-column-indicator-mode 0))

(add-hook! 'evil-visual-state-entry-hook
  (when (funcall whitespace-enable-predicate)
    (setq-local whitespace-style '(face tabs tab-mark spaces space-mark trailing))
    (whitespace-turn-off)
    (whitespace-turn-on)))

(add-hook! 'evil-visual-state-exit-hook
  (when (funcall whitespace-enable-predicate)
    (kill-local-variable 'whitespace-style)
    (whitespace-turn-off)
    (whitespace-turn-on)))

(add-hook! git-commit-mode
  (setq indent-tabs-mode nil))

(add-hook! go-mode
  (setq
   ;; Enable support for additional Flycheck checkers, such as GolangCI-Lint.
   flycheck-eglot-exclusive nil))

(add-hook! Info-mode
  (writeroom-mode t))

(add-hook! json-mode
  (hs-minor-mode))

(add-hook! 'kill-buffer-hook
           '+custom--add-buffer-to-killed-list-h)

(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))

(add-hook! markdown-mode
  (setq markdown-fontify-code-blocks-natively t)
  (auto-fill-mode t))

(add-hook! pdf-view-mode
  (setq mode-line-format nil))

(add-hook! php-mode
  (php-enable-psr2-coding-style)
  (setq fill-column 100))

(add-hook! (php-mode eglot)
  (let ((key (with-temp-buffer
               (progn (insert-file-contents "~/.config/intelephense/license.txt")
                      (buffer-string)))))
    (add-to-list 'eglot-server-programs `(((web-mode :language-id "php") (php-mode :language-id "php")) .
                                          ("intelephense" "--stdio" :initializationOptions (:licenseKey ,key))))))

(add-hook! prog-mode
  (setq fill-column 100
        show-trailing-whitespace t)
  (display-fill-column-indicator-mode))

(add-hook! rfc-mode
  (writeroom-mode t))

(add-hook! scheme-mode
  (after! geiser-mode
    (setq geiser-mode-start-repl-p t)))

(add-hook! sql-mode
  (after! lsp-sqls (lsp-deferred)))

(add-hook! sqlite-mode-hook
  (sqlite-extras-minor-mode))

(add-hook! yaml-mode
  (indent-tabs-mode -1))

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

        (:prefix "c"
         :desc "Open debugger" "X"  #'+debugger/start)

        (:prefix "f"
         "c"   nil
         :desc "Copy this file"               "C"   #'+custom/copy-this-file
         :desc "Find file as root"            "e"   #'doom/sudo-find-file
         :desc "Open current file as root"    "E"   #'doom/sudo-this-file
         "l"   nil
         "p"   nil
         "P"   nil
         "u"   nil
         "U"   nil)

        (:prefix "g"
                 (:when (modulep! :tools magit)
                   "D"   nil
                   :desc "Git fetch"                  "f"   #'magit-fetch
                   :desc "Git pull"                   "F"   #'magit-pull))

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
                 :desc "Delete project workspace"     "D"   #'+workspace/kill
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
                 :desc "Pop up scratch buffer"        "x"   #'doom/open-project-scratch-buffer
                 :desc "Remove project"               "X"   #'projectile-remove-known-project)

        (:prefix "s"
         "f"   nil
         :desc "Look up in docset"       "k" #'devdocs-browser-open
         :desc "Look up in other docset" "K" #'devdocs-browser-open-in
         "p"   nil
         "P"   nil
         :desc "Replace in buffer"       "r" #'+custom/query-replace-buffer
         :desc "Replace in project"      "R" #'projectile-replace)

        "r" nil

        (:prefix "t"
                 "f"   nil
                 "F"   nil
                 "g"   nil
                 "p"   nil
                 "r"   nil
                 "s"   nil
                 "z"   nil)))
