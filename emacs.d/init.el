;; Package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Look & Feel fixes

(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-font-lock-mode 1)
(global-whitespace-mode)
(setq whitespace-style '(face trailing))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'solarized-light t)
(nyan-mode)
(setq column-number-mode t
      size-indication-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(defun list-buffers (&optional arg)
  (interactive "P")
  (display-buffer (list-buffers-noselect arg))
  (other-window 1))

(add-to-list 'auto-mode-alist '("/requirements\\.txt\\'" . conf-mode))

;; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(require 'ido-ubiquitous)
(ido-vertical-mode)
(ido-at-point-mode)

(add-hook 'python-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (setq
              venv-location "~/.virtualenvs")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addtional functions     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(global-set-key (kbd "<f10>") 'kill-whole-line)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-block)
(global-set-key (kbd "C-z C-z") 'comment-or-uncomment-block)

;; original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
;; (defun comment-dwim-line (&optional arg)
;;   "Replacement for the comment-dwim command.
;;   If no region is selected and current line is not blank and we are not at the end of the line,
;;   then comment current line.
;;   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
;;   (interactive "*P")
;;   (comment-normalize-vars)
;;   (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
;;       (comment-or-uncomment-region (line-beginning-position) (line-end-position))
;;     (comment-dwim arg)))

;;(global-set-key "\M-;" 'comment-dwim-line)

;; TODO: should be moved on a hook
(require 'groovy-mode)
(require 'groovy-electric)

;; Hooks

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))

(add-to-list 'auto-mode-alist '("\\.jinja\\'" . html-mode))

;; Abbrevs
(setq-default abbrev-mode t)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
