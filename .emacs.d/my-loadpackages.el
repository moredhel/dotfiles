;;; package --- Summary
;;; Commentary:
                                        ; ~/.emacs.d/my-loadpackages.el
                                        ; loading package

;;; Code:
(load "~/.emacs.d/my-packages.el")

(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)

(provide 'my-loadpackages)
;;; my-loadpackages.el ends here
