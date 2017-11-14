;;===================================================================
;; Theme customizations
;;===================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#050505" :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))

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
	easy-kill
	git-timemachine
	magit
	markdown-mode
	move-text
	savehist
	smart-mode-line
	undo-tree
	volatile-highlights
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

(beacon-mode 1)
(setq beacon-color "#ffff00")
(browse-kill-ring-default-keybindings)
(crux-reopen-as-root-mode)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-undo-tree-mode)
(menu-bar-mode -1)
(savehist-mode 1)
(set-default 'truncate-lines t)
(smart-mode-line-enable)
(volatile-highlights-mode t)

;;===================================================================
;; Close on successful compile
;;===================================================================

(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;;===================================================================
;; File name mode associations
;;===================================================================

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
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
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "M-g t") 'git-timemachine)
(global-set-key (kbd "ESC <up>") 'move-text-up)
(global-set-key (kbd "ESC <down>") 'move-text-down)
(global-set-key (kbd "C-k") 'crux-smart-kill-line)
(global-set-key (kbd "C-c d") 'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c r") 'crux-rename-file-and-buffer)

;;===================================================================
;; Other
;;===================================================================

(add-hook 'before-save-hook 'delete-trailing-whitespace)
