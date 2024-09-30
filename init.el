
(setq inhibit-startup-message t)

(scroll-bar-mode -1)	; Disable visible scrollbar
(tool-bar-mode -1)	; Disable the toolbar
(tooltip-mode -1)	; Disable tooltips
(set-fringe-mode 10)	; Give some breathing room

(menu-bar-mode -1)	; Disable the menu bar

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

;; Initialize ivy with custom key bindings
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Enrich Ivy interface
(use-package ivy-rich
     :init
     (ivy-rich-mode 1))

(global-set-key (kbd "C-M-s") 'counsel-switch-buffer)

;; You will need to run M-x all-the-icons-install-fonts 
(use-package all-the-icons)

;; Initialize counsel with custom key bindings
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-× b" . counsel-ibuffer)
	 ("C-× C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
:config
(setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-monokai-machine t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-varibale] . counsel-describe-variable)
  ([remap describe-command] . counsel-describe-command)
  ([remap describe-key] . helpful-key))

(use-package general)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline counsel ivy yasnippet-snippets volatile-highlights use-package latex-extra company command-log-mode auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
