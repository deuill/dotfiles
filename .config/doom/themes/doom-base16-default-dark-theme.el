;;; doom-base16-default-dark-theme.el --- Port of Chris Kempson's Base16 theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: deuill <https://deuill.org>
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-base16-default-dark-theme nil
  "Options for the `doom-base16-default-dark' theme."
  :group 'doom-themes)

(defcustom doom-base16-default-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-base16-default-dark-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-base16-default-dark
    "A port of Base16 Eighties"
  :family 'doom-base16
  :background-mode 'dark

  ;; name        gui       256       16
  ((bg         '("#2d2d2d" nil       nil          ))
   (bg-alt     '("#282828" nil       nil          ))
   (base0      '("#080808" "black"   "black"      ))
   (base1      '("#181818" "#181818"              ))
   (base2      '("#282828" "#282828"              ))
   (base3      '("#383838" "#383838" "brightblack"))
   (base4      '("#585858" "#585858" "brightblack"))
   (base5      '("#787878" "#787878" "brightblack"))
   (base6      '("#b8b8b8" "#b8b8b8" "brightblack"))
   (base7      '("#d8d8d8" "#d8d8d8" "brightblack"))
   (base8      '("#f8f8f8" "#f8f8f8" "white"      ))
   (fg         '("#fefefe" "#fefefe" "white"))
   (fg-alt     '("#787878" "#787878" "brightblack"))

   (grey       '("#787878" "#787878" "brightblack"))
   (red        '("#ab4642" "#ab4642" "red"))
   (orange     '("#dc9656" "#dc9656" "orange"))
   (yellow     '("#f6ca88" "#f6ca88" "yellow"))
   (green      '("#a1b56c" "#a1b56c" "green"))
   (cyan       '("#86c1b9" "#86c1b9" "blue"))
   (dark-cyan  (doom-darken cyan 0.15))
   (blue       '("#7cafc2" "#7cafc2" "blue"))
   (dark-blue  (doom-darken blue 0.15))
   (violet     '("#ba8baf" "#ba8baf" "violet"))
   (magenta    (doom-darken violet 0.15))
   (teal       (doom-darken cyan 0.25))

   ;; face categories
   (highlight      base8)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      base3)
   (builtin        base8)
   (comments       grey)
   (doc-comments   yellow)
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      violet)
   (type           yellow)
   (strings        green)
   (variables      base8)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    violet)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     bg-alt)
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     fg-alt)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when doom-base16-default-dark-padded-modeline
      (if (integerp doom-base16-default-dark-padded-modeline)
          doom-base16-default-dark-padded-modeline
        4))))

  ;; --- faces ------------------------------
  (
   ;; I-search
   (match          :foreground fg :background base3)
   (isearch        :inherit 'match :box `(:line-width 2 :color ,yellow))
   (lazy-highlight :inherit 'match)
   (isearch-fail   :foreground red)

   ;; deadgrep
   (deadgrep-match-face :inherit 'match :box `(:line-width 2 :color ,yellow))

   ;; ediff.
   `(ediff-current-diff-A :background ,(doom-blend vc-deleted bg 0.1))
   `(ediff-current-diff-B :background ,(doom-blend vc-added bg 0.1))
   `(ediff-current-diff-C :background ,(doom-blend vc-modified bg 0.1))
   `(ediff-fine-diff-A    :background ,(doom-blend vc-deleted bg 0.3) :weight bold)
   `(ediff-fine-diff-B    :background ,(doom-blend vc-added bg 0.3) :weight bold)
   `(ediff-fine-diff-C    :background ,(doom-blend vc-modified bg 0.3) :weight bold)

   ;; highlight-line-changes.
   `(highlight-changes        :background ,(doom-blend vc-added bg 0.3))
   `(highlight-changes-delete :background ,(doom-blend vc-deleted bg 0.3))

   ;; current line
   (hl-line :background (doom-darken selection 0.1))

   ;; fill column indicator
   (fill-column-indicator :foreground base4)

   ;;;; swiper
   (swiper-background-match-face-1               :inherit 'match :bold bold)
   (swiper-background-match-face-2               :inherit 'match)
   (swiper-background-match-face-3               :inherit 'match :foreground green)
   (swiper-background-match-face-4               :inherit 'match :bold bold :foreground green)
   (swiper-match-face-1                          :inherit 'isearch :bold bold)
   (swiper-match-face-2                          :inherit 'isearch)
   (swiper-match-face-3                          :inherit 'isearch :foreground green)
   (swiper-match-face-4                          :inherit 'isearch :bold bold :foreground green)
   (swiper-line-face                             :inherit 'hl-line)

   ;;;; centaur-tabs
   (centaur-tabs-selected :foreground yellow :background bg)
   (centaur-tabs-unselected :foreground fg-alt :background bg-alt)
   (centaur-tabs-selected-modified :foreground yellow :background bg)
   (centaur-tabs-unselected-modified :foreground fg-alt :background bg-alt)
   (centaur-tabs-active-bar-face :background yellow)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected :foreground base8)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground base8)

   ;;;; doom-modeline
   (doom-modeline-bar :background yellow)
   (doom-modeline-buffer-path       :foreground blue :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground yellow :bold bold)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)

   ;; modeline
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;;;; treemacs
   (treemacs-file-face :foreground fg-alt)

   ;; tooltip
   (tooltip :background base2 :foreground fg-alt)))

(provide 'doom-base16-default-dark-theme)
;;; doom-base16-default-dark-theme.el ends here


