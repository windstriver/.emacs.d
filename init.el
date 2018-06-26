;;; Package repository
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-instal 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;; GUI
;;; Disable the menu bar
;(menu-bar-mode -1)
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable the scroll bar
(scroll-bar-mode -1)
;; Disable the startup screen
(setq inhibit-startup-screen t)
;; Minibuffer
(setq resize-mini-windows nil)
(setq max-mini-window-height nil)
;; Highlight current line
(global-hl-line-mode +1)

;;; Themes
; (load-theme 'leuven t)
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  )

;;; Disable backup of files
(setq make-backup-files nil)

;;; Turn on Auto Fill mode automatically in Text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;;; Spell checking
(setq ispell-program-name "/usr/local/bin/aspell")

;;; Org mode
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c b" . org-iswitchb)
	 ("C-c C-w" . org-refile))
  
  :config
  (progn
    (setq org-directory "~/Dropbox/Org")
    (setq org-agenda-files (list "gtd.org"))
    (setq org-default-notes-file "notes.org")
    ;; org capture templates
    (setq org-capture-templates
	  '(("t" "Todo" entry (file+headline "gtd.org" "INS")
             "* TODO %?\n%i\n%a")
            ("j" "Journal" entry (file+olp+datetree "journal.org")
             "* %?\nEntered on %U\n%i\n%a")))
    (setq org-todo-keywords
      '((sequence "TODO" "WAIT" "PROJ" "|" "DONE" "CACL")))
    ;; Structure completion elements
    ;; The value gets inserted after typing '<' followed by the key
    ;; and then  the completion key, usually 'TAB'
    (add-to-list 'org-structure-template-alist
		 (list "m" (concat "#+TITLE: ?\n"
				   "#+AUTHOR: Wang, Yong\n"
				   "#+STARTUP: content\n"
				   "#+STARTUP: indent\n")))
    )
  )

;; org bullets
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook
	    (lambda () (org-bullets-mode +1)))
  )
