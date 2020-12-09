;;
;;; General UX

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)


;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 1
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling


;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;
;;; Windows/frames

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; These are disabled directly through their frame parameters, to avoid the
;; extra work their minor modes do, but we have to unset these variables
;; ourselves, otherwise users will have to cycle them twice to re-enable them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(setq x-gtk-use-system-tooltips nil)


;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.01)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Typing yes/no is obnoxious when y/n will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(use-package hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;;;###package whitespace
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [? ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;###package image
(setq image-animate-loop t)

;;;###package rainbow-delimiters
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp.
(setq rainbow-delimiters-max-face-count 3)


;;
;;; Theme & font

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)


(use-package doom-modeline
  :config
  (doom-modeline-def-modeline
    'gs
    ;; Left mode line segments
    '(bar
      "  "
      matches
      buffer-info
      buffer-position
      selection-info)
    ;; Right mode line segments
    '(major-mode
      battery
      vcs))
  (doom-modeline-set-modeline 'gs t)
  :hook (after-init . doom-modeline-init))

(setq display-time-day-and-date t
      display-time-format "%I:%M %p %e %b %Y"
      display-time-default-load-average nil)
(display-time-mode t)
(display-battery-mode)
(column-number-mode)

;; (setq doom-font (font-spec :family "Deja Vu Sans Mono" :size 18))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-city-lights t))

(defvar +vc-gutter-in-margin nil
  "If non-nil, use the margin for diffs instead of the fringe.")

(defvar +vc-gutter-in-remote-files nil
  "If non-nil, enable the vc gutter in remote files (e.g. open through TRAMP).")

(defvar +vc-gutter-diff-unsaved-buffer nil
  "If non-nil, `diff-hl-flydiff-mode' will be activated. This allows on-the-fly
diffing, even for unsaved buffers.")

(defvar +vc-gutter-default-style t
  "If non-nil, enable the default look of the vc gutter.
This means subtle thin bitmaps on the left, an arrow bitmap for flycheck, and
flycheck indicators moved to the right fringe.")


(use-package git-gutter
  :init
  (add-hook 'find-file-hook
            (defun +vc-gutter-init-maybe-h ()
              "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
              (let ((file-name (buffer-file-name (buffer-base-buffer))))
                (when (or +vc-gutter-in-remote-files
                          (not (file-remote-p (or file-name default-directory))))
                  (if (null file-name)
                      (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local)
                    (when (and (vc-backend file-name)
                               (progn
                                 (require 'git-gutter)
                                 (not (memq major-mode git-gutter:disabled-modes))))
                      (if (and (display-graphic-p)
                               (require 'git-gutter-fringe nil t))
                          (setq-local git-gutter:init-function      #'git-gutter-fr:init
                                      git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                                      git-gutter:clear-function     #'git-gutter-fr:clear
                                      git-gutter:window-width -1)
                        (setq-local git-gutter:init-function      'nil
                                    git-gutter:view-diff-function #'git-gutter:view-diff-infos
                                    git-gutter:clear-function     #'git-gutter:clear-diff-infos
                                    git-gutter:window-width 1))
                      (git-gutter-mode +1)
                      (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local)))))))
  ;; Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;; syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;; global minor modes gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I don't know why
  ;; this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  ;; Only enable the backends that are available, so it doesn't have to check
  ;; when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows))

(use-package git-gutter-fringe
  :config
  (when +vc-gutter-default-style
    ;; standardize default fringe width
    (if (fboundp 'fringe-mode) (fringe-mode '4))

    ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)))

;; (use-package highlight-indent-guides
;;   :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
;;   :init
;;   (setq highlight-indent-guides-method 'character))

;; Define your custom doom-modeline
;;(doom-modeline-def-modeline 'my-simple-line
;;  '(bar matches buffer-info remote-host buffer-position parrot selection-info)
;;  '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

;; Add to `doom-modeline-mode-hook` or other hooks
;;(defun setup-custom-doom-modeline ()
;;   (doom-modeline-set-modeline 'my-simple-line 'default))
;;(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

(provide 'ui)
;;; ui.el ends here
