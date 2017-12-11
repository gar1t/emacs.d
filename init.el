;;===================================================================
;; Package support
;;===================================================================

(require 'package)

(setq package-list
      '(
	beacon
	browse-kill-ring
	color-theme-sanityinc-tomorrow
	crux
        edit-indirect
	git-timemachine
	magit
	markdown-mode
	move-text
	savehist
	smart-mode-line
	undo-tree
        web-mode
        yaml-mode
	))

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))

;;===================================================================
;; General config
;;===================================================================

;; UI
(menu-bar-mode -1)
(set-default 'truncate-lines t)
(setq column-number-mode t)

;; Smart mode line
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; Beacon
(beacon-mode 1)
(setq beacon-color "#ffff00")

;; Tabs to spaces
(setq-default indent-tabs-mode nil)

;; Brose kill ring
(browse-kill-ring-default-keybindings)

;; Undo tree
(global-undo-tree-mode)

;; Disable backups
(setq make-backup-files nil)

;; Indents
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq js-indent-level 2)

;; Compilation window
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)

;; History
(savehist-mode 1)

;; Crux
(require 'crux)

;; Close compile on success
(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;===================================================================
;; File associations
;;===================================================================

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))
(add-to-list 'auto-mode-alist '("MODEL\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("MODELS\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("PACKAGE\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . diff-mode))

;;===================================================================
;; Key bindings
;;===================================================================

(global-set-key (kbd "C-x RET") 'compile)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "C-x l") 'ibuffer)
(global-set-key (kbd "C-x f") 'find-file-at-point)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "ESC <up>") 'move-text-up)
(global-set-key (kbd "ESC <down>") 'move-text-down)
(global-set-key (kbd "C-k") #'crux-smart-kill-line)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c k") #'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)

;;===================================================================
;; Customizations
;;===================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules")))
 '(magit-dispatch-arguments nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "color-16" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(compilation-error ((t (:inherit error :foreground "red"))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face :foreground "color-245")))))
