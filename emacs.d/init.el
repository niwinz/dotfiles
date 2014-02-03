;; Mermelade
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq column-number-mode t
      size-indication-mode t)

(projectile-global-mode)

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
