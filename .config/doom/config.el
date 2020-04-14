;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;
;;; UI configuration.
;;;

;; Disable blinking cursor.
(blink-cursor-mode -1)

;; Set default values for UI parameters.
(setq-default
  ;; Default theme.
  doom-theme 'doom-monokai-pro

  ;; Font definitions.
  doom-font                (font-spec :family "Iosevka Term SS02" :size 11.50 :weight 'light)
  doom-big-font            (font-spec :family "Iosevka Term SS02" :size 12.50 :weight 'light)
  doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :weight 'light)
  doom-serif-font          (font-spec :family "IBM Plex Serif" :weight 'light)

  ;; Column used as limit for various modes.
  fill-column 100

  ;; Show which-key popup faster.
  which-key-idle-delay 0.2

  ;; Have Page Up/Down move to start/end of buffer when possible.
  scroll-error-top-bottom t

  ;; Stretch cursor to fill width of character underneath.
  x-stretch-cursor t

  ;; Disable line numbers.
  display-line-numbers-type nil

  ;; Set up mode-line.
  doom-modeline-persp-name t
  doom-modeline-vcs-max-length 30)

;; Set colors consistent with Base16-Eighties theme.
(add-hook! 'doom-load-theme-hook
  (custom-set-faces
   '(default              ((t (:background "#2d2d2d"))))
   '(hl-line              ((t (:background "#323232"))))
   '(mode-line            ((t (:background "#282828"))))
   '(solaire-default-face ((t (:background "#2d2d2d"))))
   '(solaire-hl-line-face ((t (:background "#323232"))))))

;;;
;;; Package-specific configuration.
;;;

(after! evil
  ;; Transpose lines with J/K when in visual mode.
  (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

(after! magit
   (setq magit-diff-refine-hunk t
         magit-revision-show-gravatars nil))

(after! markdown
  (setq markdown-asymmetric-header t
        markdown-enable-wiki-links t
        markdown-fontify-code-blocks-natively t))

(after! lsp
  (setq lsp-auto-guess-root t
        lsp-enable-file-watchers nil
        lsp-log-max nil))

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-border "#757575"
        lsp-ui-doc-max-width 300
        lsp-ui-doc-max-height 50
        lsp-ui-doc-position 'top)
  (set-face-attribute
   'lsp-ui-doc-background nil :background "#2d2d2d"))

(after! persp-mode
  (setq persp-autokill-buffer-on-remove t
        persp-autokill-persp-when-removed-last-buffer t))

(after! projectile
  (setq projectile-globally-ignored-directories (append (default-value 'projectile-globally-ignored-directories) '("vendor"))))

(after! sql-mode
  (setq sql-postgres-login-params (append (default-value 'sql-postgres-login-params) '(port :default 5432))))

(after! transient
  ;; Close transient windows with 'Escape' key.
  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq))

(after! writeroom-mode
  (setq writeroom-width 100
        writeroom-fullscreen-effect 'maximized
        writeroom-bottom-divider-width 0
        writeroom-maximize-window nil)
  (when (featurep! :ui zen)
    (setq +zen-text-scale 0)))

;;;
;;; Mode-specific configuration.
;;;

(add-hook! (doc-mode markdown-mode)
  (setq indent-tabs-mode nil)
  (flycheck-mode t)
  (writeroom-mode t)
  (visual-line-mode t))

(add-hook! git-commit-mode
  (setq indent-tabs-mode nil))

(add-hook! go-mode
  (after! flycheck-golangci-lint (flycheck-select-checker 'golangci-lint))
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook! php-mode
  (php-enable-psr2-coding-style))

(add-hook! prog-mode
  (setq fill-column 100
        show-trailing-whitespace t))

(add-hook! markdown-mode
  (auto-fill-mode t))

;;;
;;; Custom functions.
;;;

(defun +custom/alternate-buffer-in-persp (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (cl-destructuring-bind (buf start pos)
    (let ((buffer-list (persp-buffer-list))
          (my-buffer (window-buffer window)))
      (seq-find (lambda (it)
                  (and (not (eq (car it) my-buffer))
                        (member (car it) buffer-list)))
                (window-prev-buffers)
                (list nil nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

(defvar +custom--killed-buffer-list nil
  "List of recently killed buffers.")

(defun +custom--add-buffer-to-killed-list-h ()
  "If buffer is associated with a file name, add that file
to the `killed-buffer-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name +custom--killed-buffer-list)))

(defun +custom/reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when +custom--killed-buffer-list
    (find-file (pop +custom--killed-buffer-list))))

(defun +custom/yank-buffer ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun +custom/paste-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun +custom/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

;;;
;;; Hooks
;;;

;; Store references to killed buffers.
(add-hook 'kill-buffer-hook #'+custom--add-buffer-to-killed-list-h)

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

(map! :leader
      :desc "M-x"             ":"     #'execute-extended-command
      :desc "Open shell here" ";"
      (cond ((featurep! :term eshell) #'+eshell/toggle)
            ((featurep! :term shell)  #'+shell/toggle)
            ((featurep! :term term)   #'+term/toggle)
            (else                     nil))

      (:when (featurep! :ui popup)
        :desc "Toggle last popup" "~" #'+popup/toggle)

      :desc "Switch to last buffer" "TAB"
      (cond ((featurep! :ui workspaces) #'+custom/alternate-buffer-in-persp)
            (else                       #'evil-switch-to-windows-last-buffer))

      :desc "Resume last search" "SPC"
      (cond ((featurep! :completion ivy)  #'ivy-resume)
            ((featurep! :completion helm) #'helm-resume)
            (else                         nil))

      :desc "Search in project"            "/" #'+default/search-project
      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point

      "."   nil
      ","   nil
      "<"   nil
      "`"   nil
      "'"   nil
      "RET" nil

      :desc "Switch to 1st window"   "1"  #'winum-select-window-1
      :desc "Switch to 2nd window"   "2"  #'winum-select-window-2
      :desc "Switch to 3rd window"   "3"  #'winum-select-window-3
      :desc "Switch to 4th window"   "4"  #'winum-select-window-4
      :desc "Switch to 5th window"   "5"  #'winum-select-window-5
      :desc "Switch to 6th window"   "6"  #'winum-select-window-6
      :desc "Switch to 7th window"   "7"  #'winum-select-window-7
      :desc "Switch to 8th window"   "8"  #'winum-select-window-8
      :desc "Switch to 9th window"   "9"  #'winum-select-window-9
      :desc "Switch to 10th window"  "0"  #'winum-select-window-0

      "x" nil
      "X" nil

      :desc "Universal argument"    "u"    #'universal-argument
      :desc "window"                "w"    evil-window-map
      :desc "help"                  "h"    help-map

      (:prefix-map ("b" . "buffer")
        :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
        :desc "Previous buffer"             "["   #'previous-buffer
        :desc "Next buffer"                 "]"   #'next-buffer
        (:when (featurep! :ui workspaces)
          :desc "Switch workspace buffer"   "b" #'persp-switch-to-buffer
          :desc "Switch buffer"             "B" #'switch-to-buffer)
        (:unless (featurep! :ui workspaces)
          :desc "Switch buffer"             "b" #'switch-to-buffer)
        :desc "Kill buffer"                 "d"   #'kill-current-buffer
        :desc "Kill all buffers"            "D"   #'doom/kill-all-buffers
        :desc "List buffers"                "i"   #'ibuffer
                                            "k"   nil
                                            "K"   nil
                                            "l"   nil
        :desc "Set bookmark"                "m"   #'bookmark-set
        :desc "Delete bookmark"             "M"   #'bookmark-delete
        :desc "Next buffer"                 "n"   #'next-buffer
                                            "N"   nil
        :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
        :desc "Previous buffer"             "p"   #'previous-buffer
        :desc "Paste and replace buffer"    "P"   #'+custom/paste-buffer
                                            "r"   nil
        :desc "Revert buffer"               "R"   #'+custom/safe-revert-buffer
        :desc "Save buffer"                 "s"   #'basic-save-buffer
        :desc "Save all buffers"            "S"   #'evil-write-all
        :desc "Reopen killed buffer"        "u"   #'+custom/reopen-killed-buffer
                                            "U"   nil
        :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
        :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
        :desc "Yank buffer"                 "Y"   #'+custom/yank-buffer
        :desc "Bury buffer"                 "z"   #'bury-buffer
        :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)

      "c" nil

      (:prefix-map ("f" . "file")
                                            "c"   nil
        :desc "Copy this file"              "C"   #'doom/copy-this-file
        :desc "Delete this file"            "D"   #'doom/delete-this-file
        :desc "Find file as root"           "e"   #'doom/sudo-find-file
        :desc "Open current file as root"   "E"   #'doom/sudo-this-file
        :desc "Find file"                   "f"   #'find-file
        :desc "Find file from here"         "F"   #'+default/find-file-under-here
                                            "l"   nil
                                            "p"   nil
                                            "P"   nil
        :desc "Recent files"                "r"   #'recentf-open-files
        :desc "Rename/move file"            "R"   #'doom/move-this-file
        :desc "Save file"                   "s"   #'save-buffer
        :desc "Save file as..."             "S"   #'write-file
                                            "u"   nil
                                            "U"   nil
        :desc "Yank filename"               "y"   #'+default/yank-buffer-filename)

      (:prefix-map ("g" . "git")
        :desc "Revert file"                 "R"   #'vc-revert
        :desc "Copy link to remote"         "y"   #'browse-at-remote-kill
        :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
        (:when (featurep! :ui hydra)
          :desc "Merge"                     "m"   #'+vc/smerge-hydra/body)
        (:when (featurep! :ui vc-gutter)
                                            "]"   nil
                                            "["   nil
                                            "."   nil
          :desc "Jump to next hunk"         "n"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"     "p"   #'git-gutter:previous-hunk
          :desc "Revert hunk"               "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"          "t"   #'git-timemachine-toggle)
        (:when (featurep! :tools magit)
                                            "/"   nil
                                            "'"   nil
          :desc "Git switch branch"         "b"   #'magit-branch-checkout
          :desc "Git status"                "g"   #'magit-status
                                            "D"   nil
          :desc "Git blame"                 "B"   #'magit-blame-addition
          :desc "Git clone"                 "C"   #'magit-clone
          :desc "Git fetch"                 "f"   #'magit-fetch
          :desc "Git pull"                  "F"   #'magit-pull
          :desc "Git buffer log"            "L"   #'magit-log
          :desc "Git stage file"            "S"   #'magit-stage-file
          :desc "Git unstage file"          "U"   #'magit-unstage-file
                                            "f"   nil
          (:prefix ("o" . "open in browser")
            :desc "Browse file or region"     "o"   #'browse-at-remote
            :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
            :desc "Browse remote"             "r"   #'forge-browse-remote
            :desc "Browse commit"             "c"   #'forge-browse-commit
            :desc "Browse an issue"           "i"   #'forge-browse-issue
            :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
            :desc "Browse issues"             "I"   #'forge-browse-issues
            :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
          (:prefix ("l" . "list")
            (:when (featurep! :tools gist)
              :desc "List gists"              "g"   #'+gist:list)
            :desc "List repositories"         "r"   #'magit-list-repositories
            :desc "List submodules"           "s"   #'magit-list-submodules
            :desc "List issues"               "i"   #'forge-list-issues
            :desc "List pull requests"        "p"   #'forge-list-pullreqs
            :desc "List notifications"        "n"   #'forge-list-notifications)
          (:prefix ("c" . "create")
            :desc "Initialize repo"           "r"   #'magit-init
                                              "R"   nil
            :desc "Commit"                    "c"   #'magit-commit-create
            :desc "Fixup"                     "f"   #'magit-commit-fixup
            :desc "Branch"                    "b"   #'magit-branch-and-checkout
            :desc "Issue"                     "i"   #'forge-create-issue
            :desc "Pull request"              "p"   #'forge-create-pullreq)))

      "i" nil
      "n" nil
      "o" nil

      (:prefix-map ("p" . "project")
        (:when (featurep! :ui workspaces)
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
                                             "." nil
                                             ">" nil
                                             "!" nil
                                             ";" nil
        :desc "Add new project"              "a" #'projectile-add-known-project
                                             "b" nil
                                             "c" nil
                                             "C" nil
                                             "d" nil
        :desc "Delete project workspace"     "D" #'+workspace/delete
                                             "e" nil
        :desc "Find file in project"         "f" #'projectile-find-file
                                             "F" nil
                                             "g" nil
                                             "i" nil
                                             "k" nil
        :desc "List project workspaces"      "l" #'+workspace/display
        :desc "New project workspace"        "n" #'+workspace/new
                                             "o" nil
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Find recent project files"    "r" #'projectile-recentf
        :desc "Rename project workspace"     "R" #'+workspace/rename
                                             "s" nil
        :desc "Save project files"           "S" #'projectile-save-project-buffers
        :desc "Toggle file tree"             "t"
        (cond ((featurep! :ui neotree)           #'+neotree/open)
              ((featurep! :ui treemacs)          #'+treemacs/toggle)
              (else                              nil))
                                             "T" nil
        :desc "Switch project workspace"     "w" #'+workspace/switch-to
        :desc "Remove project"               "x" #'projectile-remove-known-project
                                             "X" nil)

      (:prefix-map ("q" . "quit/session")
        :desc "Restart emacs server"         "d" #'+default/restart-server
        :desc "Delete frame"                 "f" #'delete-frame
        :desc "Clear current frame"          "F" #'doom/kill-all-buffers
        :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
        :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
        :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
        :desc "Quick save current session"   "s" #'doom/quicksave-session
        :desc "Restore last session"         "l" #'doom/quickload-session
        :desc "Save session to file"         "S" #'doom/save-session
        :desc "Restore session from file"    "L" #'doom/load-session
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart)

      (:prefix-map ("s" . "search")
        :desc "Jump to bookmark"                 "b" #'bookmark-jump
        :desc "Search current directory"         "d" #'+default/search-cwd
        :desc "Search other directory"           "D" #'+default/search-other-cwd
                                                 "f" nil
        :desc "Jump to symbol"                   "i" #'imenu
        :desc "Jump to visible link"             "l" #'link-hint-open-link
        :desc "Jump to link"                     "L" #'ffap-menu
        :desc "Jump list"                        "j" #'evil-show-jumps
        :desc "Jump to mark"                     "m" #'evil-show-marks
        :desc "Look up online"                   "o" #'+lookup/online
        :desc "Look up online (w/ prompt)"       "O" #'+lookup/online-select
                                                 "k" nil
                                                 "K" nil
                                                 "p" nil
                                                 "P" nil
                                                 "r" nil
        :desc "Search buffer"                    "s" #'swiper-isearch
        :desc "Search buffer for thing at point" "S" #'swiper-isearch-thing-at-point
        :desc "Dictionary"                       "t" #'+lookup/dictionary-definition
        :desc "Thesaurus"                        "T" #'+lookup/synonyms)

      "r" nil

      (:prefix-map ("t" . "toggle")
                                             "b" nil
                                             "f" nil
                                             "F" nil
                                             "g" nil
        (:when (featurep! :ui indent-guides)
          :desc "Indent guides"              "i" #'highlight-indent-guides-mode)
        :desc "Indent style"                 "I" #'doom/toggle-indent-style
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
                                             "p" nil
                                             "r" nil
                                             "s" nil
                                             "t" nil
                                             "w" nil
        :desc "Soft line wrapping"           "w" #'visual-line-mode
        (:when (featurep! :editor word-wrap)
          :desc "Soft line wrapping"         "w" #'+word-wrap-mode)
        :desc "Zoom"                         "z" #'+hydra/text-zoom/body))
