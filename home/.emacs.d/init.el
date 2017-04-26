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
(if (display-graphic-p)
    (progn
      ;; (tooltip-mode -1)
      (tool-bar-mode -1)
      ;; (menu-bar-mode -1)
      (scroll-bar-mode -1)
      ;; (setq inhibit-splash-screen t)
      ;; (setq inhibit-startup-message t)
      ))

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

;; Easier Scrolling
(use-package smooth-scrolling)
(smooth-scrolling-mode 1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; Custom Files
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Backup Files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

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
             '(font . "Fira Code-14"))

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
          ("M-g f" . avy-goto-line)
          ("M-g w" . avy-goto-word-1)))

(avy-setup-default)

;; imenu (Interactive menu)
(bind-key* "M-i" 'imenu)

;; Helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(helm-mode 1)

;; Helm Swoop
(use-package helm-swoop)

;; Deft
(use-package deft)
(setq deft-recursive t)
(setq deft-extensions '("org" "md" "txt"))
(setq deft-default-extension "org")
(setq deft-directory "~/Dropbox/notes")
(setq deft-auto-save-interval 0)
(global-set-key (kbd "C-c d") 'deft)
(global-set-key (kbd "C-x C-g") 'deft-find-file)

;; Expand Region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

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
            :config (company-quickhelp-mode 1))
          ))

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

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; init.el ends here
