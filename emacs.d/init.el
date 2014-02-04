;; Mermelade
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq column-number-mode t
      size-indication-mode t)



(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'solarized-dark t)
(nyan-mode)
;; (require 'pretty-mode)
;; (global-pretty-mode 1)
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)


(defun list-buffers (&optional arg)
  (interactive "P")
  (display-buffer (list-buffers-noselect arg))
  (other-window 1))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(require 'ido-ubiquitous)
(ido-at-point-mode)

;; comment and uncomment
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(projectile-global-mode)
(ido-vertical-mode)
