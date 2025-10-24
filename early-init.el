;; Disable package.el so Straight is the only package manager
(setq package-enable-at-startup nil
      package-quickstart nil)

;; (optional) Small startup niceties
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 134217728  ; 128MB
                  gc-cons-percentage 0.1)))
