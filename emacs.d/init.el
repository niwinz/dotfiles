;; Package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Look & Feel fixes
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default visible-cursor nil)
(setq dabbrev-case-fold-search nil)
(setq visible-cursor nil)
(setq x-stretch-cursor 1)

(prefer-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'misterioso t)
(nyan-mode)
(setq column-number-mode t
      size-indication-mode t)

(global-whitespace-mode)
(global-font-lock-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(setq whitespace-style '(face trailing))

;; Backup temporal directories
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(require 'ido-ubiquitous)
(ido-vertical-mode)
(ido-at-point-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'jade-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (setq sws-tab-width 4)))

(add-hook 'coffee-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (show-paren-mode)
             (define-clojure-indent
               (defroutes 'defun)
               (GET 2)
               (POST 2)
               (PUT 2)
               (DELETE 2)
               (HEAD 2)
               (ANY 2)
               (context 2))
             (define-clojure-indent
               ;; built-ins
               (maybe-let 1)
               (mlet 1))))

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

;; (global-set-key (kbd "<f9>") 'quick-copy-line)
;; (global-set-key (kbd "<f10>") 'kill-whole-line)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-l"))

(global-set-key (kbd "C-z C-z") 'comment-or-uncomment-block)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-l") 'quick-copy-line)
(global-set-key (kbd "C-c C-k") 'kill-line)

;; Hooks
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))
(add-to-list 'auto-mode-alist '("/requirements\\.txt\\'" . conf-mode))

;; Abbrevs
(setq-default abbrev-mode t)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
