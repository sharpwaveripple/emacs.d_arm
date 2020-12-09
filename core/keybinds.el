(use-package general)

(defun kill-buffer-and-window-if-repl ()
  "Kill current buffer, and also window if repl."
  (interactive)
  (if (equal major-mode 'inferior-python-mode)
      (kill-buffer-and-window)
    (kill-buffer)))

(provide 'keybinds)
;;; keybinds.el ends here
