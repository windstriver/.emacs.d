;; UI
(setq inhibit-startup-message t)
(put 'inhibit-startup-echo-area-message 'saved-value
     (setq inhibit-startup-echo-area-message (user-login-name)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(tooltip-mode -1)
(set-fringe-mode 30)

;; full screen
(add-to-list 'default-frame-alist '(undecorated . t))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; set up the visible bell
(setq visible-bell t)

;; font
(set-face-attribute 'default nil :font "Iosevka" :height 130)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; org-mode
(setq org-directory "~/OneDrive - RWE/Documents/GTD-Org")
(setq org-agenda-files (list "gtd.org"))
(setq org-clock-mode-line-total 'today)

;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package default with straight
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; nerd-icons
(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-icon nil)
  :init (doom-modeline-mode 1))

;; doom-one theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

;; evil
(use-package evil
  :ensure t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-C-i-jump nil)
  (evil-mode 1))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("READ\\.md\\'" . gfm-mode))
