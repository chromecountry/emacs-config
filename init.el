;;; init.el -- Summary

;;; Commentary:

;;; Code:

(load-file "~/.emacs.d/config/config.el") 	; load configuration 
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-separator '(0.2))
 '(warning-suppress-log-types
   '(((copilot copilot-no-mode-indent))
     ((copilot copilot-no-mode-indent))))
 '(warning-suppress-types
   '(((copilot copilot-no-mode-indent))
     ((copilot copilot-no-mode-indent)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
