;;; package --- Python Configuration

;;; Commentary:


;;; Code:

;;; Custom function to evaluate the current python file in a buffer
(defun eval-py ()
  "Evaluate/Run current Python file"
  (interactive)
(python-shell-send-file (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; (global-set-key (kbd "C-c C-n") eval-py)
;;(eval-after-load 'python-mode
  ;;(lambda () (local-set-key (kbd "C-c C-n") eval-py)))

;; Add cwd to python path
;; (setq python-remove-cwd-from-path nil)
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c C-n") 'eval-py))


(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.envs")


;; (jedi:install-server)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . jedi-mode))

(provide 'python-config)
;;; python-config.el ends here
