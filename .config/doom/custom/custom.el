;;; custom.el -*- lexical-binding: t; -*-

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
  "Copy entire buffer to the kill ring"
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

(defun +custom/query-replace-buffer ()
  "Search and replace literal string in buffer."
  (interactive)
  (let ((orig-point (point)))
    (save-excursion
      (goto-char (point-min))
      (call-interactively 'query-replace))
    (goto-char orig-point)))

(defun +custom/font-scale (size)
  "Scale font of given SIZE by the default font scale in the environment."
  (let ((scaled (round (* size (string-to-number (getenv "GDK_DPI_SCALE"))))))
    (if (floatp size) (float scaled) scaled)))

(defun +custom/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH, switching to the file immediately."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (doom/copy-this-file new-path force-p)
  (find-file new-path))