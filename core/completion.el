(use-package ivy
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500
        ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ivy-use-virtual-buffers t
        ;; ...but if that ever changes, show their full path
        enable-recursive-minibuffers t
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t
        ivy-virtual-abbreviate 'abbreviate
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-ignore-buffers '("\\` " "\\`\\*"))
  (ivy-mode))

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-path-style 'abbreviate
        ivy-rich-parse-remote-buffer nil)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (when-let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
              (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
  (ivy-rich-mode +1)))

(use-package all-the-icons-ivy-rich
  :after all-the-icons ivy-rich
  :config (all-the-icons-ivy-rich-mode 1))

(use-package counsel
  :after ivy
  :custom
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (ivy-initial-inputs-alist nil)
  :config
  (counsel-mode))


(use-package ivy-posframe
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width 10
        ivy-posframe-parameters
        `((min-width . 90)
          (min-height . ,ivy-height)))
  ;; posframe doesn't work well with async sources (the posframe will
  ;; occasionally stop responding/redrawing), and causes violent resizing of the
  ;; posframe.
  (dolist (fn '(swiper counsel-rg counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-posframe-display-functions-alist)
          #'ivy-display-function-fallback)))

(use-package smex)

(use-package swiper)

(use-package projectile
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        ;; projectile-switch-project-action #'projectile-dired
        projectile-sort-order 'recentf)
  (projectile-global-mode +1))

(use-package counsel-projectile
  :after counsel projectile
  :config
  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))

(use-package company
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay 0.25
        company-tooltip-idle-delay 0.25
        company-minimum-prefix-length 1
        company-tooltip-maximum-width 50
        company-tooltip-minimum-width 20
        company-tooltip-width-grow-only t
        company-tooltip-limit 20
        company-tooltip-align-annotations t
        company-require-match 'never
        company-selection-wrap-around t
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '(company-capf)
        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-scrollbar 'inherit
        company-box-doc t
        company-box-doc-delay 0.25
        company-box-max-candidates 50
        company-box-frame-behavior 'point))

(provide 'completion)
