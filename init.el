;;; init.el --- Summary
;;; Commentary:
;;; Code:

;; ----------------------------------------------------------------------
;; Copilot: hard guards BEFORE anything else loads
;; ----------------------------------------------------------------------

;; Pin node path early so copilot sees it even during init churn.
(setq copilot-node-executable "/usr/bin/node")

;; Never let copilot start during init, in elisp/org/scratch, or in straight build files.
;; Enforced via copilot's enable predicates.
(setq copilot-enable-predicates
      (list
       (lambda ()
         (and
          ;; Don't start before Emacs has finished init.
          (boundp 'after-init-time)

          ;; Only in real files
          (buffer-file-name)

          ;; Not in elisp/org/scratch
          (not (memq major-mode
                     '(emacs-lisp-mode lisp-interaction-mode org-mode)))

          ;; Not anywhere under your straight build tree
          (not (string-match-p
                "/\\.emacs\\.d/\\.packages/straight/build/"
                (buffer-file-name)))))))

;; Extra safety: make copilot-mode a NO-OP during init only.
(defvar my/starting-up t)
(advice-add 'copilot-mode :around
            (lambda (orig &rest args)
              (unless my/starting-up
                (apply orig args))))
(add-hook 'emacs-startup-hook
          (lambda () (setq my/starting-up nil)))

;; ----------------------------------------------------------------------
;; Straight.el bootstrap (package.el disabled via early-init is ideal)
;; ----------------------------------------------------------------------

;; If you also add (setq package-enable-at-startup nil) in early-init.el,
;; package.el won't initialize and you won't see the warning.
;; This line here doesn't prevent early init, but it's harmless as a fallback:
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(require 'use-package)

;; ----------------------------------------------------------------------
;; Load the rest of your configuration
;; ----------------------------------------------------------------------

(load-file "~/.emacs.d/config/config.el")  ; load configuration

;; ----------------------------------------------------------------------
;; User customizations (kept as-is)
;; ----------------------------------------------------------------------

(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-separator '(0.2))
 '(warning-suppress-log-types
   '(((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-no-mode-indent))
     ((copilot copilot-no-mode-indent))))
 '(warning-suppress-types
   '(((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-exceeds-max-char))
     ((copilot copilot-no-mode-indent))
     ((copilot copilot-no-mode-indent)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
