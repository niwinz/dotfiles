;; Package management
(require 'package)

(add-to-list 'load-path "~/.emacs.d/local/")
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))


;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)


(setq package-selected-packages
      '(bind-key
        expand-region
        charmap
        clojure-mode
        cmake-modea
        csv-mode
        dockerfile-mode
        elixir-modeg
        erlang
        graphql-mode
        ido-at-point
        ;;ido-ubiquitous
        ido-completing-read+
        ido-vertical-mode
        ipython
        jade-mode
        jinja2-mode
        js2-mode
        json-mode
        jsx-mode
        less-css-mode
        leuven-theme
        markdown-mode+
        multiple-cursors
        mustache-mode
        nyan-mode
        protobuf-mode
        rainbow-delimiters
        scala-mode
        scss-mode
        stylus-mode
        toml-mode
        typescript
        undo-tree
        vline
        yaml-mode))

;; Look & Feel fixes
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default visible-cursor nil)
(setq package-check-signature nil)
(setq make-backup-files nil)
(setq dabbrev-case-fold-search nil)
(setq visible-cursor nil)
(setq x-stretch-cursor 1)

(setq create-lockfiles nil) 
;; (setq backup-directory-alist `(("." . "~/.saves")))
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

(setq confirm-kill-emacs #'y-or-n-p)
(setq inhibit-splash-screen t)
(setq fci-rule-column 90)
(setq fill-column 80)

(electric-indent-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(pending-delete-mode t)

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

;; Cider
;; (setq cider-repl-tab-command #'indent-for-tab-command)
;; (setq cider-auto-mode nil)
;; (setq cider-repl-result-prefix ";; => ")
;; (setq cider-interactive-eval-result-prefix ";; => ")

;; String formating
;; (require 'string-inflection)
;; (global-set-key (kbd "C-c i") 'string-inflection-cycle)
;; (global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
;; (global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
;; (global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle)

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
;;(require 'ido-ubiquitous)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(ido-vertical-mode)
(ido-at-point-mode)

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 4)
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")
        ("jinja"    . "\\.jinja\\'")))

(setq web-mode-content-types-alist
  '(("jsx"  . ".*\\.js[x]?\\'")))

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
(global-set-key (kbd "C-;") 'recenter-top-bottom)
(global-set-key (kbd "C-c C-k") 'kill-line)

(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)

(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)

;; (require 'change-inner)
;; (global-set-key (kbd "C-'") 'er/expand-region)

;; saltar marca C-x C-x  (anterior)
;; C-u C-space (saltar entre todas marcas anteriores)
;; C-space dos veces deja una marca

;; Usar bind-key en vez de global-set-key

;; C-M-k Borrar sexpr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'js-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (show-paren-mode)
             (setq js-indent-level 2)))

(add-hook 'js2-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (show-paren-mode)
             (setq js2-basic-offset 4)))

(add-hook 'jade-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (setq sws-tab-width 2)))

;; (require 'clojure-mode)
(add-hook 'clojure-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (show-paren-mode)
             ;; (setq clojure-defun-style-default-indent t)
             (electric-indent-mode -1)

             (define-clojure-indent
               (defroutes 'defun)
               (simple-benchmark 'defun)
               (it 'defun)
               (describe 'defun)
               (context 2))

             (define-clojure-indent
               (it 1)
               (async 'defun)
               (fdef 'defun)
               (fnc 'defun)
               (errlet 1)
               (maybe-let 1)
               (atomic 'defun)
               (alet 1)
               (mlet 1))))

(add-hook 'scss-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (setq css-indent-offset 2)
             (electric-indent-mode -1)))

(add-hook 'python-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (electric-indent-mode -1)
             (show-paren-mode)
             (setq venv-location "~/.virtualenvs")))

(add-hook 'typescript-mode-hook
          '(lambda ()
             (setq typescript-indent-level 2)
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)
             (electric-indent-mode -1)
             (show-paren-mode)))

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 2)))

(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("/requirements\\.txt\\'" . conf-mode))

;; Abbrevs
(setq-default abbrev-mode t)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (vue-mode ido-completing-read+ graphql-mode yaml-mode vline undo-tree typescript toml-mode stylus-mode scss-mode scala-mode rainbow-delimiters protobuf-mode nyan-mode mustache-mode multiple-cursors markdown-mode+ leuven-theme less-css-mode jsx-mode json-mode js2-mode jinja2-mode jade-mode ipython ido-vertical-mode ido-at-point erlang elixir-mode dockerfile-mode csv-mode cmake-mode clojure-mode charmap bind-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
