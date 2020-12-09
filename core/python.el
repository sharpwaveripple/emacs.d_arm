(defun ipython-open-repl ()
  "Open an inferior ipython process, killing already existing ones."
  (interactive)
  (when (get-buffer "*Python*")
    (kill-buffer "*Python*"))
  (split-window-right)
  (run-python)
  (select-window (previous-window)))

;; (defun ipython-open-repl ()
;;   (interactive)
;;   (evil-window-vsplit 80)
;;   (run-python)
;;   (shrink-window-if-larger-than-buffer)
;;   (select-window (previous-window)))

(defun ipython-shell-send-defun-advance ()
  (interactive)
  (ipython-shell-send-defun)
  (python-nav-end-of-defun))

(defun elpy-send-region-and-step ()
  (interactive)
  (elpy-shell-send-region-or-buffer-and-step)
  (evil-exit-visual-state))

(defun hs-toggle ()
  (interactive)
  (hs-toggle-hiding)
  (evil-beginning-of-line))

(use-package python
  :hook (python-mode . hs-minor-mode
         python-mode . ipython-open-repl)
  :config
  (setq python-shell-interpreter "ipython"
	    python-shell-interpreter-args "--simple-prompt -i"
	    python-indent-offset 4
	    python-indent-guess-indent-offset nil)
  (general-define-key
   :keymaps 'python-mode-map
   :major-modes t
   "<C-return>" 'elpy-shell-send-group-and-step))

(use-package sphinx-doc
  :after python
  :hook (python-mode . sphinx-doc-mode))

(use-package conda
  :after python)

(use-package py-isort
  :after python
  :hook (before-save . py-isort-before-save))

(use-package pyimport
  :after python)

(use-package elpy
  :after python
  :hook
  (python-mode . elpy-enable)
  :config
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))))

;; need to install black and jedi; consider bootstrapping
;; '/home/jon/.emacs.d/elpy/rpc-venv/bin/python -m pip install --upgrade pip' command.

(provide 'python)
