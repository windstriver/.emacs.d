;; UI
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

;; hide init messages
(setq inhibit-startup-message t)
(put 'inhibit-startup-echo-area-message 'saved-value
     (setq inhibit-startup-echo-area-message (user-login-name)))

;; font
(set-face-attribute 'default nil :font "Iosevka" :height 120)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; org-mode
(setq org-directory "~/OneDrive - RWE/Documents/GTD-Org/")
(setq org-agenda-files (list "gtd.org" "notes.org" "journal.org"))

(setq org-clock-mode-line-total 'today)

; org-capture
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "notes.org" "INBOX")
	 "* %?\nEntered on %U\n%i\n%a")
	("j" "Journal" entry (file+datetree "journal.org")
	 "* %?\nEntered on %U\n%i\n%a")))

; org-babel
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							 (python . t)))
;; calendar/diary
(setq diary-file (concat org-directory "diary"))

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

;; straight: integration with use-package 
; use-package will use straight.el to automatically install missing packages if you provide :straight t
; Specifying :straight t is unnecessary if you set straight-use-package-by-default to a non-nil value.
; variable use-package-always-ensure is associated with package.el, should not be used with straight.el
(setq straight-use-package-version 'straight)
(setq straight-use-package-by-default t)

;; vertico
(use-package vertico
  :config
  (vertico-mode))

;; which-key
(use-package which-key
  :init
  (setq which-key-idle-delay 3)
  :diminish which-key-mode
  :config
  (which-key-mode))

;; nerd-icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; doom-modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  :config
  (doom-modeline-mode 1))

;; doom-one theme
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-one t))

;; evil
;; (use-package evil
;;  :init
;;  (setq evil-want-C-i-jump nil)
;;  :config
;;  (evil-mode 1))

;; markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))
