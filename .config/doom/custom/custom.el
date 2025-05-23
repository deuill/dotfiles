;;; custom.el -*- lexical-binding: t; -*-

(defvar +custom--killed-buffer-list nil
  "List of recently killed buffers.")

;;;###autoload
(defun +custom/alternate-buffer-in-workspace (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (let ((buffer-list (+workspace-buffer-list))
            (buf (window-buffer window)))
        (seq-find (lambda (it)
                    (and (not (eq (car it) buf))
                         (member (car it) buffer-list)))
                  (window-prev-buffers)
                  (list nil nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

;;;###autoload
(defun +custom--add-buffer-to-killed-list-h ()
  "If buffer is associated with a file name, add that file
to the `killed-buffer-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name +custom--killed-buffer-list)))

;;;###autoload
(defun +custom/reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when +custom--killed-buffer-list
    (find-file (pop +custom--killed-buffer-list))))

;;;###autoload
(defun +custom/paste-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;###autoload
(defun +custom/safe-revert-buffer ()
  "Prompt before reverting buffer."
  (interactive)
  (revert-buffer nil nil))

;;;###autoload
(defun +custom/query-replace-buffer ()
  "Search and replace literal string in buffer."
  (interactive)
  (let ((orig-point (point)))
    (save-excursion
      (goto-char (point-min))
      (call-interactively 'query-replace))
    (goto-char orig-point)))

;;;###autoload
(defun +custom/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH, switching to the file immediately."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (doom/copy-this-file new-path force-p)
  (find-file new-path))

;;;###autoload
(defun +custom/shr-add-font (start end type)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (bolp)
        (skip-chars-forward " "))
      (add-face-text-property (point) (min (line-end-position) end) type)
      (if (< (line-end-position) end)
          (forward-line 1)
        (goto-char end)))))

;;;###autoload
(defun +custom/deft-popup ()
  "Pop *Deft* buffer and load files."
  (pop-to-buffer deft-buffer)
  (if (not (eq major-mode 'deft-mode))
      (deft-mode)))

(defconst +custom--project-notes-filename "notes.org")

;;;###autoload
(defun +custom--pr-review-at-point ()
  (interactive)
  (or (forge-pullreq-at-point)
      (and (forge-pullreq-p forge-buffer-topic)
           forge-buffer-topic)
      (user-error "No current pull-request"))
  (let ((pullreq (or (forge-pullreq-at-point) (forge-current-topic))))
    (if (not (forge-pullreq-p pullreq))
        (user-error "We can only review PRs at the moment. You tried on something else."))
    (let* ((repo (forge-get-repository pullreq))
           (owner (oref repo owner))
           (name (oref repo name))
           (number (oref pullreq number)))
      (message "Opening pull request %s..." (forge-get-url pullreq))
      (pr-review-open owner name number))))

;;;###autoload
(defun +custom/start-pr-review (arg)
  "Start PR review for the pull-request under current point."
  (interactive "P")
  (call-interactively
   (if (or arg (not (featurep 'forge)))
       #'pr-review
     #'+custom--pr-review-at-point)))

;;;###autoload
(defun +custom/open-project-notes ()
  "Creates or opens project-local notes file."
  (interactive)
  (find-file (expand-file-name +custom--project-notes-filename (projectile-project-root)))
  (auto-save-mode t)
  (add-hook! 'kill-buffer-hook :local 'save-buffer))

;;;###autoload
(defun +custom/sqlite-view-file ()
  "Runs `sqlite-mode-open-file' on the file name visited by the current buffer, killing it."
  (let ((file-name buffer-file-name)
        (directory default-directory))
    (kill-current-buffer)
    (switch-to-buffer (get-buffer-create (format "*SQLite %s*" (file-name-nondirectory file-name))))
    (sqlite-mode-open-file file-name)
    (setq default-directory directory)))

(add-to-list 'magic-mode-alist '("SQLite format 3\x00" . +custom/sqlite-view-file))

;;;###autoload
(define-derived-mode rich-view-mode fundamental-mode "rich-view-mode"
  "Major mode for viewing rich text (e.g. RTF, DOC) files."
  (delete-region (point-min) (point-max))
  (rename-buffer (concat "*" (buffer-file-name) "*"))
  (call-process "pandoc" nil t t "--to" "html" (buffer-file-name))
  (shr-render-region (point-min) (point-max))
  (set-buffer-modified-p nil)
  (read-only-mode)
  (goto-char (point-min)))

(add-to-list 'auto-mode-alist '("\\.rtf\\'" . rich-view-mode))
(add-to-list 'auto-mode-alist '("\\.docx\\'" . rich-view-mode))
(add-to-list 'auto-mode-alist '("\\.odt\\'" . rich-view-mode))

;;;###autoload
(defvar-keymap aerc-compose-mode-map
  "C-c C-c" #'(lambda () (interactive) (save-buffers-kill-terminal t))
  "C-c C-k" #'kill-emacs
  "C-c q"   #'kill-emacs)

;;;###autoload
(define-derived-mode aerc-compose-mode mail-mode "Aerc Compose"
  "Major mode for Aerc email composition"
  (setq-default mode-line-format nil)
  (add-hook! 'doom-init-ui-hook :append
    (setq mode-line-format nil))
  (evil-insert-state))

(add-to-list 'auto-mode-alist '("aerc-compose-[0-9]+\\.eml\\'" . aerc-compose-mode))
