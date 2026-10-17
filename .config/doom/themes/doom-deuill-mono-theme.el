;;; doom-deuill-mono-theme.el --- Simple monochromatic theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: deuill <https://deuill.org>
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-deuill-mono-theme nil
  "Options for the `doom-deuill-mono' theme."
  :group 'doom-themes)

(defcustom doom-deuill-mono-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-deuill-mono-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-deuill-mono
    "A simple, monochromatic color scheme"
  :family 'doom-base16
  :background-mode 'dark

  ;; name        gui       256       16
  (
   (base0      '("#f7f3ef" "#f7f3ef" "white"      ))
   (base1      '("#e0dcd7" "#e0dcd7" "brightblack"))
   (base2      '("#c4c0bb" "#c4c0bb" "brightblack"))
   (base3      '("#a9a5a0" "#a9a5a0" "brightblack"))
   (base4      '("#928d88" "#928d88" "brightblack"))
   (base5      '("#706c68" "#706c68" "brightblack"))
   (base6      '("#4e4b47" "#4e4b47" "black"      ))
   (base7      '("#2c2b23" "#2c2b23" "black"      ))
   (base8      '("#1d1b19" "black"   "black"      ))
   (bg         base1)
   (bg-alt     base0)
   (fg         base8)
   (fg-alt     base7)

   (grey       base6)
   (red        '("#ab4642" "#ab4642" "red"))
   (orange     base6)
   (yellow     base7)
   (green      base6)
   (cyan       base7)
   (dark-cyan  base6)
   (blue       base7)
   (dark-blue  base6)
   (violet     base7)
   (magenta    base6)
   (teal       base6)

   ;; face categories
   (highlight      red)
   (vertical-bar   base4)
   (selection      base2)
   (builtin        base7)
   (comments       base5)
   (doc-comments   base8)
   (constants      base8)
   (functions      base7)
   (keywords       base8)
   (methods        base7)
   (operators      base7)
   (type           base7)
   (strings        base8)
   (variables      base7)
   (numbers        base8)
   (region         base7)
   (error          red)
   (warning        base7)
   (success        base8)
   (vc-added       base8)
   (vc-modified    base7)
   (vc-deleted     base5)

   ;; custom categories
   (modeline-bg     fg)
   (modeline-bg-alt fg-alt)
   (modeline-fg     bg-alt)
   (modeline-fg-alt bg)
   (-modeline-pad
    (when doom-deuill-mono-padded-modeline
      (if (integerp doom-deuill-mono-padded-modeline)
          doom-deuill-mono-padded-modeline
        4))))

  ;; --- faces ------------------------------
  (
   ;; base overrides
   ((font-lock-function-name-face &override) :weight 'bold :inverse-video t)
   ((font-lock-type-face &override)          :weight 'bold :underline t)
   ((font-lock-comment-face &override)       :slant 'italic)
   ((font-lock-builtin-face &override)       :weight 'bold)
   ((font-lock-string-face &override)        :slant 'italic)
   ((font-lock-number-face &override)        :slant 'italic)
   ((font-lock-keyword-face &override)       :weight 'bold)
   ((help-key-binding &override)             :background nil    :foreground red   :box nil)
   ((region &override)                       :background bg-alt :foreground base6 :inverse-video t)
   (match                                    :background base7  :foreground base0)

   ;; I-search and I-edit
   (isearch-fail     :foreground red)
   (lazy-highlight   :background base6 :foreground base1)
   (iedit-occurrence :background red   :foreground base0)

   ;; current line
   (hl-line :background selection :foreground fg)

   ;; fill column indicator
   (fill-column-indicator :foreground base3)

   ;; transient
   (transient-key-stay :foreground red)
   (transient-key-exit :background red :foreground base0)

   ;; diff
   (diff-changed        :background base1 :foreground base8)
   (diff-added          :background base1 :foreground base8 :weight 'bold)
   (diff-removed        :background base1 :foreground base5)
   (diff-refine-changed :background base2 :foreground base8)
   (diff-refine-added   :background base6 :foreground base0 :weight 'bold)
   (diff-refine-removed :background base4 :foreground base6)

   ;; ediff
   (ediff-odd-diff-A     :background base2 :foreground base5 :extend t)
   (ediff-odd-diff-B     :background base2 :foreground base8 :extend t)
   (ediff-even-diff-A    :inherit 'ediff-odd-diff-A)
   (ediff-even-diff-B    :inherit 'ediff-odd-diff-B)
   (ediff-current-diff-A :background base3 :foreground base5 :extend t)
   (ediff-current-diff-B :background base5 :foreground base8 :extend t)

   ;; magit
   (magit-diff-base                   :background base1  :foreground base8 :extend t)
   (magit-diff-base-highlight         :background base2  :foreground base8 :extend t)
   (magit-diff-added                  :background base1  :foreground base8 :extend t)
   (magit-diff-added-highlight        :background base5  :foreground base1 :extend t)
   (magit-diff-removed                :background base1  :foreground base5 :extend t)
   (magit-diff-removed-highlight      :background base3  :foreground base5 :extend t)
   (magit-diff-context                :background bg-alt :foreground fg    :extend t)
   (magit-diff-context-highlight      :background bg     :foreground fg    :extend t)
   (magit-diff-file-heading           :foreground red :weight 'bold :extend t)
   (magit-diff-file-heading-selection :background base6 :foreground bg :weight 'bold :extend t)
   (magit-diff-hunk-heading           :background base6 :foreground bg-alt :extend t)
   (magit-diff-hunk-heading-highlight :background base7 :foreground bg :weight 'bold :extend t)
   (magit-dimmed                      :foreground base5)

   ;; completions
   (completions-common-part :foreground base8 :weight 'bold :underline t)

   ;; orderless
   (orderless-match-face-0 :foreground bg :background region :weight 'bold)
   (orderless-match-face-1 :inherit 'orderless-match-face-0)
   (orderless-match-face-2 :inherit 'orderless-match-face-0)
   (orderless-match-face-3 :inherit 'orderless-match-face-0)

   ;; doom-modeline
   (doom-modeline-bar               :background base0)
   (doom-modeline-info              :foreground base2)
   (doom-modeline-buffer-path       :foreground base2 :weight 'bold)
   (doom-modeline-project-dir       :foreground base0 :weight 'bold)
   (doom-modeline-persp-name        :foreground base2)
   (doom-modeline-buffer-major-mode :foreground base2)

   ;; line numbers
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base3 :bold bold)

   ;; eglot
   (eglot-highlight-symbol-face :background base2)

   ;; vertico
   (vertico-current :background highlight :foreground bg)

   ;; modeline
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;; markdown
   (markdown-header-face :foreground base8 :weight 'bold)
   (markdown-code-face   :background base2 :extend t)

   ;; treemacs
   (treemacs-file-face :foreground fg-alt)

   ;; tooltip
   (tooltip :background base2 :foreground fg-alt)))

(provide 'doom-deuill-mono-theme)
;;; doom-deuill-mono-theme.el ends here
