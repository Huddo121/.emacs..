;;
;; Variable Definitions
;;

(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;;
;; Packages and Plugins
;;

;; Set up package management
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Highlight Indent Guides
(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;; Powerline
(use-package powerline)
(powerline-default-theme)

;; Projectile
(use-package projectile)
(add-hook 'prog-mode-hook 'projectile-mode)
(use-package flx-ido) ;; Better fuzzy finder

;; Neotree
(use-package neotree)
(global-set-key [f8] 'neotree-toggle)

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Solarized, dark, because I'm allergic to sunlight
(use-package solarized-theme)

;; Install Programming Languages
(use-package elixir-mode) ;; Elixir Packages
(use-package alchemist)

;; PureScript
(use-package purescript-mode)
(use-package psc-ide)
(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)
    (psc-ide-server-start)))

(customize-set-variable 'psc-ide-rebuild-on-save t)

;; Pick up PureScript projects
(projectile-register-project-type 'pulp '(".pulp-cache")
                  :compile "pulp build"
                  :test "pulp test"
                  :run "pulp run")

;;
;; Modes
;;
(electric-indent-mode 0)

;;
;; Cusom Behaviour
;;

(global-linum-mode t) ;; Line numbers, please
(setq linum-format "%4d \u2502 ")

;; Pg Up, Pg dn, M-v, C-v, bigger overlap between screens
(setq next-screen-context-lines 8)

;; Set company-mode to start after initialisation
(add-hook 'after-init-hook 'global-company-mode)

;; Save file on loss of focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; I normally press right-alt + Enter (M-RET) for complete + import
(global-set-key (kbd "M-RET") 'company-complete)

;; Switch open windows with C-c <Arrow>
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Use S-/ to comment or uncomment the current line or selected region
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; TODO: iTerm2 has Cmd + / bound to some sort of 'find-cursor' function
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages (quote (projectile use-package)))
 '(psc-ide-rebuild-on-save t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (load-theme 'solarized-dark)

