;;; package  --- Ruby Config Code
;;; Commentary:
;; Configuration for Ruby Related Editor Preferences

;;; Code:

;; (setq enh-ruby-program "/home/moredhel/.rbenv/versions/1.9.3-p448/bin/ruby")
(setq enh-ruby-program "ruby")
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
 
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; Remove Colours
(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
 
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)
 
(provide 'ruby-config.el)
;;; ruby-config.el ends here
