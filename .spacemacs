;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(;; Languages
     c-c++
     csv
     emacs-lisp
     go
     html
     javascript
     lua
     markdown
     org
     php
     scheme
     sql
     yaml
     ;; Checking and linting
     spell-checking
     syntax-checking
     ;; Tagging and completion
     auto-completion
     ;; UI
     neotree
     ibuffer
     ;; Tools
     dash
     deft
     docker
     gtags
     helm
     lsp
     restclient
     shell
     xclipboard
     ;; Source control
     git
     version-control)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(dtrt-indent
     color-identifiers-mode
     smart-tabs-mode
     writeroom-mode)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(vi-tilde-fringe
     org-bullets)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5) (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai spacemacs-dark spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator slant :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka Term" :weight light :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil

   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t

   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 5

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.25

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%a - Spacemacs"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq
   ;; Use custom file for additional settings.
   custom-file "~/.emacs.d/.cache/custom-settings.el"

   ;; Do not use variable-width fonts for headers.
   monokai-use-variable-pitch nil

   ;; Set colors consistent with Base16-Eighties theme.
   monokai-background     "#2d2d2d"
   monokai-highlight-line "#323232")

  ;; Load additional settings.
  (load "~/.emacs.d/.cache/custom-settings.el"))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq-default
    ;; Set indentation to tabs.
    indent-tabs-mode t
    standard-indent 4
    c-basic-offset 4
    go-tab-width 4
    tab-width 4

    ;; Have fill commands break at 100 column widths.
    fill-column 100

    ;; Reduce performance impact of long lines.
    bidi-display-reordering nil

    ;; Have Page Up/Down move to start/end of buffer when possible.
    scroll-error-top-bottom t

    ;; Autocompleted docstrings appear in tooltips.
    auto-completion-enable-help-tooltip t

    ;; Use dumb-jump as default jump handler.
    spacemacs-default-jump-handlers '(dumb-jump-go evil-goto-definition)

    ;; Tie buffer and perspective removal.
    persp-autokill-buffer-on-remove t
    persp-autokill-persp-when-removed-last-buffer t

    ;; Defaults for iBuffer mode.
    ibuffer-group-buffers-by 'projects

    ;; Use simple NeoTree theme.
    neo-theme 'icons
    neo-banner-message nil
    neo-mode-line-type 'none

    ;; Set defaults for Org mode.
    org-enable-github-support t
    org-ellipsis " …"
    org-bullets-bullet-list '("•" "◦")
    org-projectile-file "TODO.org"

    ;; Set defaults for Markdown mode.
    markdown-hide-markup t
    markdown-fontify-code-blocks-natively t

    ;; Set user defaults for Deft.
    deft-recursive t
    deft-extensions '("org" "md")
    deft-directory "~/Documents/Notes"

    ;; Set default shell.
    shell-default-shell 'ansi-term

    ;; Projectile defaults.
    projectile-globally-ignored-directories '("vendor")

    ;; Use common cache path for docsets.
    helm-dash-docset-newpath "~/.cache/docsets"
    helm-dash-browser-func 'eww-split

    ;; Defaults for Magit.
    magit-diff-refine-hunk t
    magit-revision-show-gravatars nil
    git-commit-mode-hook '(set-git-commit-defaults)

    ;; Make indentation detection with dtrt more conservative.
    dtrt-indent-min-quality 90.0
    dtrt-indent-active-mode-line-info ""

    ;; Defaults for Go.
    go-backend 'lsp
    go-use-golangci-lint t
    go-format-before-save t
    godoc-at-point-function 'godoc-gogetdoc

    ;; Defaults for SQL.
    sql-auto-indent nil
    sql-capitalize-keywords t

    ;; Enable clang support for C/C++ layers.
    c-c++-enable-clang-support t

    ;; Have PHP-CS check against the PSR2 standard.
    flycheck-phpcs-standard "PSR2"

    ;; Set color for column marker.
    fci-rule-color "#484848"

    ;; Set highlighter for diff markers in margin.
    version-control-diff-tool 'git-gutter+
    git-gutter-fr+-side 'left-fringe
    left-fringe-width 20

    ;; Set defaults for writeroom mode.
    writeroom-width 100
    writeroom-restore-window-config t
    writeroom-fullscreen-effect 'maximized
    writeroom-bottom-divider-width 0

    ;; Defaults for LSP UI.
    ;; lsp-eldoc-hook '(lsp-document-highlight)
    lsp-ui-doc-border "#484848"
    lsp-ui-doc-max-width 100
    lsp-ui-doc-position 'bottom
    lsp-eldoc-render-all nil

    ;; Configuration for Ediff.
    ediff-window-setup-function 'ediff-setup-windows-plain
    ediff-split-window-function 'split-window-horizontally
    ediff-diff-options "-w"

    ;; Show human-readable names instead of container IDs for Docker.
    docker-tramp-use-names t

    ;; Do not show project name in buffer name segment.
    ggtags-mode-line-project-name nil

    ;; Defaults for HTML rendering, don't apply contrasting backgrounds.
    shr-use-fonts nil
    shr-color-visible-luminance-min 75)

  ;; Global keybindings.
  (global-set-key (kbd "M-<up>") 'next-buffer)
  (global-set-key (kbd "M-<down>") 'previous-buffer)
  (spacemacs/set-leader-keys
    "TAB" 'alternate-buffers)

  ;; Mode-specific hooks and configuration.
  (add-hook 'prog-mode-hook 'set-prog-mode-defaults)
  (add-hook 'css-mode-hook 'set-prog-mode-defaults)

  (add-hook 'markdown-mode-hook 'set-doc-mode-defaults)
  (add-hook 'org-mode-hook 'set-doc-mode-defaults)
  (add-hook 'Info-mode-hook 'set-doc-mode-defaults)

  (add-hook 'php-mode-hook 'set-php-mode-defaults)
  (add-hook 'sh-mode-hook 'set-sh-mode-defaults)
  (add-hook 'emacs-lisp-mode-hook 'set-lisp-mode-defaults)
  (add-hook 'sql-mode-hook 'set-sql-mode-defaults)
  (add-hook 'deft-mode-hook 'set-deft-mode-defaults)
  (add-hook 'git-commit-mode-hook 'set-git-commit-defaults)

  (add-to-list 'spacemacs-indent-sensitive-modes 'sql-mode)

  ;; Generic sane defaults for all modes.
  (spacemacs/toggle-mode-line-minor-modes-off))

;; Generic sane defaults for programming language modes.
(defun set-prog-mode-defaults ()
  ;; Enable column marker.
  (fci-mode 1)

  ;; Attempt to automatically determine indentation settings from buffer.
  (dtrt-indent-mode 1))

;; Documentation file-specific defaults.
(defun set-doc-mode-defaults ()
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :modes (text-mode markdown-mode org-mode)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end)))

  (add-to-list 'flycheck-checkers 'proselint)

  ;; Enable `flycheck' with specific linters.
  (flycheck-mode 1)

  (setq
   ;; Disable indentation with tabs.
   indent-tabs-mode nil)

  ;; Enable distraction-free editing mode.
  (writeroom-mode 1)

  ;; Enable soft word-wrapping.
  (visual-line-mode 1))

;; PHP-specific defaults.
(defun set-php-mode-defaults ()
  ;; Set PSR2 coding style as default.
  (php-enable-psr2-coding-style)

  ;; Inherit prog-mode defaults.
  (set-prog-mode-defaults))

(defun set-sh-mode-defaults ()
  (add-to-list 'flycheck-checkers 'sh-shellcheck)

  ;; Enable `flycheck' with specific linters.
  (flycheck-mode 1))

;; Lisp-specific defaults.
(defun set-lisp-mode-defaults ()
  ;; Disable indentation with tabs.
  (setq indent-tabs-mode nil))

;; SQL-specific defaults.
(defun set-sql-mode-defaults ()
  ;; Disable indentation with tabs.
  (setq indent-tabs-mode nil))

;; Deft-specific defaults.
(defun set-deft-mode-defaults ()
  ;; Enable distraction-free editing mode.
  (writeroom-mode 1))

;; Defaults for Git commit messages.
(defun set-git-commit-defaults ()
  ;; Disable tab indentation.
  (setq-local indent-tabs-mode nil))

(defun eww-split (url)
  "Loads eww content in split window"
  (interactive)
  (select-window (split-window-right))
  (eww url))

(defvar fci-mode-suppressed nil)

(defun fci-enabled-p ()
  "Checks if fci-mode is enabled for this session"
  (and (boundp 'fci-mode) fci-mode))

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (fci-enabled-p)))
    (when fci-enabled
      (set (make-local-variable 'fci-mode-suppressed) fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and fci-mode-suppressed
             (null popup-instances))
    (setq fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(defun alternate-buffers ()
  "Switch between buffers in the current perspective"
  (interactive)
  (with-persp-buffer-list () (switch-to-buffer nil)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
