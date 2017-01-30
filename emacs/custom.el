(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("28130127bbf3072c1bbc7652fca7245f186bb417b3b385a5e4da57b895ffe9d8" default)))
 '(projectile-mode-line
   (quote
    (:eval
     (if
	 (file-remote-p default-directory)
	 " Proj"
       (format " Proj[%s]"
	       (projectile-project-name))))))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit default))))
 '(show-paren-match ((t (:weight normal))))
 '(sp-show-pair-match-face ((t (:background "#1B1E1C" :foreground "#87D700" :weight normal))))
 '(trailing-whitespace ((t (:background "#602020")))))
