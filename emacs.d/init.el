;; Mermelade
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Use tabs
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; White space mode
(global-whitespace-mode)
(setq whitespace-style '(face trailing))

;; Look & Feel fixes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'solarized-dark t)
(nyan-mode)
(setq column-number-mode t
      size-indication-mode t)

(defun list-buffers (&optional arg)
  (interactive "P")
  (display-buffer (list-buffers-noselect arg))
  (other-window 1))

;; Idle mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(require 'ido-ubiquitous)
(ido-vertical-mode)
(ido-at-point-mode)

;; Projectile
(projectile-global-mode)

;; comment and uncomment
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
