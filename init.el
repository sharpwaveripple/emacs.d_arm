;;; init.el -*- lexical-binding: t; -*-
;;
;; Author:  Jonathan Tay <jonathantay193@gmail.com>
;;
;;; License: MIT

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  ;; Ensure Doom is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Load the heart of Emacs
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-agenda-files '("~/work/donders/anil/paper/main.org" "~/life/work.org"))
 '(warning-suppress-types '((comp) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
