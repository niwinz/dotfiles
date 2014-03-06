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

;; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(require 'ido-ubiquitous)
(ido-vertical-mode)
(ido-at-point-mode)

;; Projectile
;;(projectile-global-mode)

;; comment and uncomment
(defun comment-or-uncomment-block ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-block)


(defun quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

(global-set-key (kbd "<f9>") 'quick-copy-line)


;; original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
  If no region is selected and current line is not blank and we are not at the end of the line,
  then comment current line.
  Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'comment-dwim-line)
(global-font-lock-mode 1)

(require 'groovy-mode)
(require 'groovy-electric)

;; ;; turn on syntax highlighting

;; ;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
;; (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;; (add-to-list 'auto-mode-alist '("\\.groovy" . groovy-mode))
;; (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; ;;; make Groovy mode electric by default.
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))


(add-to-list 'auto-mode-alist '("\\.jinja\\'" . html-mode))
