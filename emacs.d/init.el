;; Package management
(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/") t)


(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;; Look & Feel fixes
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default visible-cursor nil)
(setq make-backup-files nil)
(setq dabbrev-case-fold-search nil)
(setq visible-cursor nil)
(setq x-stretch-cursor 1)
(setq backup-directory-alist `(("." . "~/.saves")))

(electric-indent-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; Theme
;; (load-theme 'misterioso t)
(load-theme 'whiteboard t)

;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Whitespace mode
(global-whitespace-mode)
(global-font-lock-mode t)
(setq whitespace-style '(face trailing))

;; Encoding
(prefer-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

;; Buffer name style
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Nyan Cat
(require 'nyan-mode)
(nyan-mode)
(setq column-number-mode t
      size-indication-mode t)

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
             (setq sws-tab-width 2)))

(add-hook 'coffee-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (show-paren-mode)
             ;; (setq clojure-defun-style-default-indent t)
             (electric-indent-mode -1)
             (define-clojure-indent
               (defroutes 'defun)
               (it 'defun)
               (describe 'defun)
               (GET 2)
               (POST 2)
               (PUT 2)
               (DELETE 2)
               (HEAD 2)
               (ANY 2)
               (context 2))
             (define-clojure-indent
               (it 1)
               (errlet 1)
               (maybe-let 1)
               (atomic 'defun)
               (mlet 1))))

(add-hook 'sass-mode-hook
          '(lambda ()
             (setq sass-indent-offset 4)
             (setq haml-indent-offset 4)
             (electric-indent-mode -1)))


(add-hook 'python-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (electric-indent-mode -1)
             (show-paren-mode)
             (setq venv-location "~/.virtualenvs")))

(add-hook 'typescript-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (electric-indent-mode -1)
             (show-paren-mode)))

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

(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)

;; Hooks
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 2)))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))
(add-to-list 'auto-mode-alist '("/requirements\\.txt\\'" . conf-mode))

;; Abbrevs
(setq-default abbrev-mode t)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
