(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(defvar my-packages '(better-defaults
                      evil-leader
                      evil
                      find-file-in-project
                      idle-highlight-mode
                      ido-ubiquitous
                      magit
                      markdown-mode
                      org
                      paredit
                      smex
                      solarized-theme
                      scpaste
                      swiper))

(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; load theme
(load-theme 'solarized-dark t)

;; ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)

;; package customisation
(require 'better-defaults)

(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "w"  'save-buffer
  "b"  'ido-switch-buffer
  "gs" 'magit-status
  "mp" 'org-mobile-push
  "mg" 'org-mobile-pull)

(require 'evil)
(evil-mode 1)

(require 'find-file-in-project)
(require 'idle-highlight-mode)

(require 'org)
(global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cb" 'org-iswitchb)

;; org-mode generic
(setq org-default-notes-file "~/org/notes.org")
(setq org-agenda-files '("notes.org"
                         "life.org"
                         "container-solutions.org"
                         "container-solutions-adidas.org"))
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE" "DELEGATED")))

;; setup org-mobile stuffs!
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)

(require 'paredit)
    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'scpaste)
(require 'swiper)

;; setup mu4e with nix.
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

(require 'mu4e)



;; Auto stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (swiper evil-leader markdown-mode org-mobile-sync org solarized-theme evil scpaste magit find-file-in-project ido-ubiquitous idle-highlight-mode paredit smex better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
