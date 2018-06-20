;;; Package repository
(require 'package)
;;  MELPA
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Disable the menu bar
;(menu-bar-mode -1)

;;; Disable the toolbar
(tool-bar-mode -1)

;;; Disable the scroll bar
(scroll-bar-mode -1)

;;; Disable the startup screen
(setq inhibit-startup-screen t)

;;; Minibuffer
(setq resize-mini-windows nil)
(setq max-mini-window-height nil)

;;; Highlight current line
(global-hl-line-mode +1)

;;; Set color theme
;(load-theme 'leuven t)
(load-theme 'zenburn t)

;;; Disable backup of files
(setq make-backup-files nil)

;;; Turn on Auto Fill mode automatically in Text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;;; Org mode
(require 'org)
(setq org-directory "~/Dropbox/Org")
(setq org-default-notes-file "notes.org")
(setq org-agenda-files (list "gtd.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "gtd.org" "Ins")
         "* TODO %?\n%i\n%a")
        ("j" "Journal" entry (file+olp+datetree "journal.org")
         "* %?\nEntered on %U\n%i\n%a")))
(add-to-list 'org-structure-template-alist
	     (list "P" (concat "#+TITLE: ?\n"
			       "#+AUTHOR: Wang, Yong\n"
			       "#+STARTUP: content\n"
			       "#+STARTUP: indent\n")))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

