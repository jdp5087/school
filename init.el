(setq mybuf1 "")
(setq mybuf2 "")

(require 'xcscope)

(setq c-default-style "linux")
(defun my-c-mode-hook ()
  (setq indent-tabs-mode t)
  (setq tab-width 8))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(define-key global-map [(ctrl \,)] (lambda ()
				     (interactive)
				     (if (string= mybuf1 "")
					 (setq mybuf1 (read-string "Buffer name to set:"))
				       (switch-to-buffer mybuf1))))
(define-key global-map [(C \.)] (lambda ()
				  (interactive)
				  (if (string= mybuf2 "")
				      (setq mybuf2 (read-string "Buffer name to set:"))
				    (switch-to-buffer mybuf2))))





(define-key global-map [(ctrl /)] (lambda () (interactive) (switch-to-buffer "*shell*")))
(define-key global-map [(ctrl f3)] 'cscope-set-initial-directory)

(define-key global-map [(ctrl f4)] 'cscope-unset-initial-directory)

(define-key global-map [(ctrl f5)] 'cscope-find-this-symbol)

(define-key global-map [(ctrl f6)] 'cscope-find-global-definition)

(define-key global-map [(ctrl f7)]

'cscope-find-global-definition-no-prompting)

(define-key global-map [(ctrl f8)] 'cscope-pop-mark)

(define-key global-map [(ctrl f9)] 'cscope-next-symbol)

(define-key global-map [(ctrl f10)] 'cscope-next-file)

(define-key global-map [(ctrl f11)] 'cscope-prev-symbol)

(define-key global-map [(ctrl f12)] 'cscope-prev-file)

(define-key global-map [(meta f9)] 'cscope-display-buffer)

(define-key global-map [(meta f10)] 'cscope-show-entry-other-window)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-light))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
