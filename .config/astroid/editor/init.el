;; Astroid embedded editor configuration.

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-user-dir
      (concat (file-name-directory load-file-name) "packages"))

(package-initialize)

;; Install package dependencies if needed.

(setq package-list '(evil notmuch))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Enable global modes.

(evil-mode)

(require 'notmuch)
(setq-default major-mode 'notmuch-message-mode)

;; Appearance.

(blink-cursor-mode 0)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq-default mode-line-format nil)

(custom-set-faces
 '(default ((t (:background "white" :foreground "#222"))))
 '(fringe ((t (:background "white")))))

;; Misc. Configuration

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default confirm-kill-emacs 'y-or-n-p)
(setq-default use-dialog-box nil)
