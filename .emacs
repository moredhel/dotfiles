(setq inhibit-startup-screen t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; non-specific functionality
(setq vc-follow-symlinks t)
(load-theme 'whiteboard)

;; custom functions & variables
(let ((mu4epath
       (concat
        (file-name-directory
         (file-truename
          (executable-find "mu")))
        "/../share/emacs/site-lisp/mu4e")))
  (when (and
         (string-prefix-p "/nix/store/" mu4epath)
         (file-directory-p mu4epath))
    (add-to-list 'load-path mu4epath)))

(defun my-org-mobile-push ()
  (interactive)
  (org-mobile-push)
  (org-icalendar-combine-agenda-files))

(defun org-mobile-pull-switch ()
  (interactive)
  (org-mobile-pull)
  (switch-to-buffer "from-mobile.org"))

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; this may need to be moved into the helm use-package section.
(defun helm-save-and-paste ()
  (interactive)
  (kill-new (x-get-clipboard))
  (helm-show-kill-ring))

;; move this somewhere more useful...
(global-set-key (kbd "M-p") 'helm-save-and-paste)

(use-package mu4e
  :no-require t
  :config
  (setq user-full-name "Hamish Hutchings"
        user-mail-address "hamish@aoeu.me"
        message-kill-buffer-on-exit t
        mu4e-change-filenames-when-moving t
        mu4e-compose-format-flowed t
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-view-show-addresses 't
        mu4e-maildir "~/mail"
        mu4e-user-mail-address-list '("hamish@aoeu.me"
                                      "moredhel@aoeu.me"
                                      "hamhut@hamhut1066.com"
                                      "hamish.hutchings@container-solutions.com")
        mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder  "/Deferred"
        mu4e-refile-folder "/Archive"
        mu4e-headers-fields
        '( (:date          .  25)    ;; alternatively, use :human-date
           (:flags         .   6)
           (:from          .  22)
           (:subject       .  nil)) ;; alternatively, use :thread-subject
        mu4e-get-mail-command "mbsync -aq"
        message-kill-buffer-on-exit t
        mu4e-html2text-command "html2text -width 72 -nobs")
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t))

;; (use-package solarized-theme
;;   :ensure t
;;   :no-require t
;;   :config
;;   (load-theme 'solarized-dark t))

(use-package magit
  :ensure t)

(use-package better-defaults :ensure t)
(use-package better-defaults)

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "q"  'delete-frame
    "bd"  'evil-delete-buffer
    "w"  'save-buffer
    "pb"  'helm-projectile-switch-to-buffer
    "pf" 'projectile-find-file
    "bb"  'helm-buffers-list
    "bs"  'switch-to-scratch
    "oo"  'other-frame
    "kb" 'kill-buffer
    "gs" 'magit-status
    "/"  'helm-projectile-ag

    ;; org
    "mp" 'my-org-mobile-push
    "mg" 'org-mobile-pull-switch
    "r"  'org-capture
    "cj" 'org-clock-goto
    "cr" 'org-resolve-clocks

    ;; mail
    "mc" 'mu4e-compose-new
    "mm" 'mu4e
    "mU" 'mu4e-update-mail-and-index))
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))

(use-package find-file-in-project)
(use-package idle-highlight-mode :ensure t)


(use-package org-mu4e
  :config
  (setq org-mu4e-link-query-in-headers-mode nil)
  (setq org-capture-templates
        '(
          ;; default todo
          ("t" "todo" entry (file+headline "~/org/notes.org" "Tasks")
           "* TODO %?\n%a\n")
          ;; custom todo for emails
          ("m" "mail" entry (file+headline "~/org/notes.org" "Tasks")
           "* TODO %? :mail:
            SCHEDULED: %(org-insert-time-stamp(org-read-date nil t \"+0d\"))
            %a"))))

(use-package org-journal
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-format "%Y%m%d.org"))

(use-package org
  :config
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)

  ;; org-icalendar
  (setq org-icalendar-combined-agenda-file "~/Dropbox/Apps/MobileOrg/org.ics"
        org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
        org-icalendar-with-timestamps t
        org-icalendar-alarm-time 30)

  ;; org-mode generic
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday 0) ;; change start day to Sunday
  (setq org-agenda-files '("~/org/notes.org"
                           "~/org/life.org"
                           "~/org/life-media.org"
                           "~/org/CS"
                           "~/org/journal/"))

  (setq org-refile-use-outline-path 'full-file-path)
  (setq org-outline-complete-in-steps nil)
  (setq org-completion-use-ido nil)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-use-speed-commands t)

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 9)))

  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE" "DELEGATED" "ABANDONED")))

  (setq org-stuck-projects
        (quote
         ("+project/-DONE-ABANDONED"
          ("DOING")
          ("unstick"
           "onhold"
           "actionable")
          "")))

  (setq org-tags-exclude-from-inheritance '("project"))

  ;; custom agenda filters
  (setq org-agenda-custom-commands
        '(("ph" tags-todo "+PRIORITY={A}"
           ((org-agenda-overriding-header "High Priority")))
          ("pp" tags-todo "+project"
           ((org-agenda-overriding-header "Projects")))
          ;; Show all tags that need to be done
          ;; of a higher level
          ("wH" tags-todo "LEVEL<3+work"
           ((org-agenda-overriding-header "Overview")))
          ("tw" tags-todo "+work"
           ((org-agenda-overriding-header "Work")))
          ("tW" tags-todo "-work"
           ((org-agenda-overriding-header "Non-Work")))))

  ;; setup org-mobile stuffs!
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  (add-hook 'after-init-hook 'org-mobile-pull)
  (add-hook 'kill-emacs-hook 'org-mobile-push))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package scpaste :ensure t)
(use-package swiper :ensure t)

;; setup mu4e with nix.

(use-package helm
  :config
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq racer-rust-src-path ""
        racer-cmd "racer"))

;; start the server
;; (server-start)

;; language specific configs
(use-package go-mode
  :no-require t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package nix-mode
  :no-require t)

(use-package dockerfile-mode
  :no-require t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(org-agenda-files
   (quote
    ("~/org/journal/20170919.org" "~/org/notes.org" "~/org/life.org" "~/org/life-media.org" "/home/moredhel/org/CS/general.org" "/home/moredhel/org/journal/20170912.org" "/home/moredhel/org/journal/20170913.org" "/home/moredhel/org/journal/20170914.org")))
 '(package-selected-packages
   (quote
    (org-journal company racer nix-sandbox auto-complete haskell-mode helm-ag helm-projectile projectile go-mode go yaml-mode use-package swiper solarized-theme smex scpaste rust-mode paredit org-mobile-sync org-evil org-bullets notmuch nix-mode markdown-mode magit ido-ubiquitous idle-highlight-mode helm find-file-in-project evil-leader enh-ruby-mode dockerfile-mode better-defaults)))
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
