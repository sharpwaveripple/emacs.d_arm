(use-package csv-mode
  :hook (csv-mode . csv-align-mode))

(global-so-long-mode 1)

(use-package auto-sudoedit
  :config (auto-sudoedit-mode t))

(use-package magit
  :defer 1
  :straight (:pin "2fb3bf782ccf5652b98f8de989f014749473eacf"))

(use-package restart-emacs)

;; https://stackoverflow.com/questions/21756052/how-to-send-c-left-into-emacs-term
(defun vterm-send-Cbackspace () (interactive) (vterm-send-key (kbd "C-w")))
(defun term-send-Cright () (interactive) (term-send-raw-string "\e[1;5C"))
(defun term-send-Cleft  () (interactive) (term-send-raw-string "\e[1;5D"))

(use-package vterm
  :bind (:map vterm-mode-map
              ("<C-backspace>" . vterm-send-Cbackspace)
              ("C-<right>" . term-send-Cright)
              ("C-<left>" . term-send-Cleft))
  :init
  (setq vterm-always-compile-module t
        vterm-kill-buffer-on-exit t
        vterm-max-scrollback 10000)
  :hook (vterm-mode . (lambda () (read-only-mode -1))))

(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :config
  ;; Check only when saving or opening files. Newline & idle checks are a mote
  ;; excessive and can catch code in an incomplete state, producing false
  ;; positives, so we removed them.
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)
  (setq flycheck-indication-mode 'right-fringe)
  )

(use-package flycheck-pos-tip)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; (defun yas-insert-snippet-evil-insert ()
;;   (interactive)
;;   (yas-insert-snippet)
;;   (evil-insert))

(use-package yasnippet)

(use-package yasnippet-snippets
  :after yasnippets)

(setq yas-triggers-in-field t)

(provide 'tools)
