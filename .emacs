;;; .emacs  --- Hamish's Emacs Config
;;; Commentary:


;;; Code:
;;; Setting PATH
(defun set-exec-path-from-shell-PATH ()
  "Set the PATH environment variable."
    (let ((path-from-shell (shell-command-to-string "$SHELL -c 'echo $PATH'")))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))


(setenv "PYTHONPATH" (concat "/usr/local/lib/python2.7/site-packages/"))

(add-to-list 'load-path "~/.emacs.d/custom/")
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")

(load "~/.emacs.d/my-loadpackages.el")

;; Requirements
(require 'fic-mode)
(require 'evil)
(require 'smart-mode-line)
(require 'zone)
;; (require 'company)
(require 'flycheck)
(require 'rainbow-delimiters)
(require 'monokai-theme)
(require 'yasnippet)
;; (require 'go-flymake)
;; (require 'go-mode-autoloads)

(load "ruby-config.el")
(load "python-config.el")
(load "html-config.el")
;; (load "go-config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "0eebf69ceadbbcdd747713f2f3f839fe0d4a45bd0d4d9f46145e40878fc9b098" default)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(fic-background-color "#66D9EF")
 '(fic-foreground-color "#FD971F")
 '(fic-highlighted-words (cons "HACK" fic-highlighted-words))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(org-agenda-files nil))

;; Generic Settings
(setq make-backup-files nil) ;; disable backup files
(setq vc-follow-symlinks t) ;; disable backup files
(scroll-bar-mode -1) ;; Disable Scroll Bar
(tool-bar-mode -1)
(define-coding-system-alias 'UTF-8 'utf-8) ;; Add utf encoding support
(setq-default tab-width 2 indent-tabs-mode nil)
(setq inhibit-startup-message t) ;; hide splash screen
(setq gdb-many-windows t)
(global-set-key (kbd "s-f") 'toggle-frame-fullscreen) ;; Fullscreen shortcut
(set-language-environment 'utf-8) ;; UTF-8 Stuffs
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)
(desktop-save-mode 1)
;; (server-start)


;;; File Hooks
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; Evil-mode
(evil-mode 1)
(setq evil-want-fine-undo t)
(setq evil-emacs-state-cursor '("#A6E22E" box))
(setq evil-normal-state-cursor '("#F92672" box))
(setq evil-visual-state-cursor '("#66D9EF" box))
(setq evil-insert-state-cursor '("#A6E22E" bar))
(setq evil-replace-state-cursor '("#A6E22E" bar))
(setq evil-operator-state-cursor '("#A6E22E" hollow))

;;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;;; YaSnippet
(yas-global-mode 1)

;;; Autocomplete
(require 'auto-complete-config)
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(global-auto-complete-mode t)

(setq ac-ignore-case nil)

;; Allow for looking up Documentation easily
(define-key evil-normal-state-map "K" 'dash-at-point)

;; electric-pair-mode
(electric-pair-mode t)

;;; Modeline
(display-time-mode 1)
(display-battery-mode 1)
(sml/setup)
(sml/apply-theme 'respectful)


;; zone - 5 mins
(zone-when-idle 300)

;; company
;; (add-hook 'after-init-hook 'global-company-mode)

;; Syntastic
;; flycheck
(global-flycheck-mode t)
(setq flycheck-ruby-rubocop-executable "/usr/local/bin/rubocop")
(add-hook 'enh-ruby-mode-hook (lambda ()
                                (flycheck-select-checker 'ruby-rubocop)
                                'hs-minor-mode
                                ))
(add-hook 'js-mode-hook (lambda ()
                                (flycheck-select-checker 'javascript-jshint)
                                'hs-minor-mode
                                ))

(add-hook 'python-mode-hook (lambda ()
                                'hs-minor-mode
                                ))
;; TODO: Set these as only when flycheck is enabled
(global-set-key (kbd "C-c n") 'flycheck-next-error)
(global-set-key (kbd "C-c p") 'flycheck-previous-error)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; fic
(add-hook 'prog-mode-hook 'turn-on-fic-mode)

;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


(setq org-agenda-files '("~/Org/agenda"))




;; Eldoc:
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; Haskell-mode
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))


(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
