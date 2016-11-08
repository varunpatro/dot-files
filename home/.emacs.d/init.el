;;; init.el --- Emacs init file

;;; Commentary:

;; Varun's Emacs init file

;;; Code:

;; Enabling Package Archive
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

;; User Details
(setq user-full-name "Varun Kumar Patro"
      user-mail-address "varun.kumar.patro@gmail.com")

;; UI Cruft
;; (tooltip-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(scroll-bar-mode -1)
;; (setq inhibit-splash-screen t)
;; (setq inhibit-startup-message t

;; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (setq use-package-always-ensure t))


;; Easier y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom Files
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Backup Files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Delete Old Files
;; (let ((week (* 60 60 24 7))
;;       (current (float-time (current-time))))
;;   (dolist (file (directory-files temporary-file-directory t))
;;     (when (and (backup-file-name-p file)
;;                (> (- current (float-time (fifth (file-attributes file))))
;;                   week))
;;       (message "%s" file)
;;       (delete-file file))))

;; Line Numbers
(global-linum-mode 1)

;; Default Font
(add-to-list 'default-frame-alist
             '(font . "Fira Code-16"))

;; Tabs vs Spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Theming
;; (load-theme 'gruvbox t)

;; WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Ace-Window
(use-package ace-window
  :bind (("M-q" . ace-window)))

;; Avy (char based decision tree)
(use-package avy
  :bind* (("C-'" . avy-goto-char)
          ("C-," . avy-goto-char-2)))

;; imenu (Interactive menu)
(bind-key* "M-i" 'imenu)

(use-package counsel)
(use-package swiper
  :bind*
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-char)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c l" . counsel-locate))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
;;    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ;; fuzzy find
    (define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
    (ivy-set-actions
     'counsel-find-file
     '(("d" (lambda (x) (delete-file (expand-file-name x)))
        "delete"
        )))
    (ivy-set-actions
     'ivy-switch-buffer
     '(("k"
        (lambda (x)
          (kill-buffer x)
          (ivy--reset-state ivy-last))
        "kill")
       ("j"
        ivy--switch-buffer-other-window-action
        "other window")))))

;; Browse Kill Ring (clipboard history)
(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;; Expand Region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; Zap to Char
(use-package zzz-to-char
  :bind (("M-z" . zzz-up-to-char)))

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Templating
(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :defer 5
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

;; Autocompletion
(use-package company
  :defer 5
  :diminish company-mode
  :init (progn
          (add-hook 'after-init-hook 'global-company-mode)
          (setq company-dabbrev-ignore-case nil
                company-dabbrev-code-ignore-case nil
                company-dabbrev-downcase nil
                company-idle-delay 0
                company-begin-commands '(self-insert-command)
                company-transformers '(company-sort-by-occurrence))
          (use-package company-quickhelp
            :config (company-quickhelp-mode 1))))

;; Find File in Project
(use-package find-file-in-project
  :bind (("s-f" . find-file-in-project)
         ("s-F". find-file-in-current-directory)
         ("M-s-f" . find-file-in-project-by-selected)))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-blame))
  :init (setq magit-auto-revert-mode nil)
  :config (add-hook 'magit-mode-hook 'hl-line-mode))

;; Which Key
(use-package which-key
  :diminish which-key-mode
  :config (add-hook 'after-init-hook 'which-key-mode))

;; Volatile Highlights
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; Git Gutter
(use-package git-gutter+
  :init (global-git-gutter+-mode)
  :diminish git-gutter+-mode
  :defer 5
  :config (progn
            (setq git-gutter+-modified-sign "==")
            (setq git-gutter+-added-sign "++")
            (setq git-gutter+-deleted-sign "--")))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Golden Ratio
;; (use-package golden-ratio
;;   :diminish golden-ratio-mode
;;   :config (progn
;;             (add-to-list 'golden-ratio-extra-commands 'ace-window)
;;             (golden-ratio-mode 1)))

;;; init.el ends here
