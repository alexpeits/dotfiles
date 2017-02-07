(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "28130127bbf3072c1bbc7652fca7245f186bb417b3b385a5e4da57b895ffe9d8" default)))
 '(fci-rule-color "#5E5E5E")
 '(hl-sexp-background-color "#1c1f26")
 '(imenu-list-minor-mode nil)
 '(linum-format " %7i ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/oeworks/timesheets/new/2017_feb.org")))
 '(package-selected-packages
   (quote
    (rainbow-delimiters arduino-mode irony-eldoc company-tern theme-changer afternoon-theme flatland-theme zenburn-theme which-key web-mode wc-mode use-package sublime-themes spacemacs-theme spaceline smartparens pyenv-mode popwin persp-projectile ox-twbs org-clock-csv org-bullets org ob-ipython neotree monokai-theme molokai-theme moe-theme material-theme magit js2-mode jedi intero imenu-list highlight-numbers highlight-escape-sequences helm-projectile helm-fuzzy-find helm-ag hc-zenburn-theme gruvbox-theme git-gutter-fringe git-gutter-fringe+ fuzzy-match flycheck-irony flx-ido evil-terminal-cursor-changer evil-surround evil-nerd-commenter evil-leader esup company-quickhelp company-irony-c-headers company-irony company-ghci company-ghc company-c-headers company-anaconda color-theme-sanityinc-tomorrow c-eldoc auto-complete-clang auctex atom-one-dark-theme ac-anaconda)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (file-remote-p default-directory)
         " Proj"
       (format " Proj[%s]"
               (projectile-project-name))))))
 '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("ON_HOLD" . "cyan")
      (\,
       ("TESTING" . "magenta"))))))
 '(vc-annotate-background "#202020")
 '(vc-annotate-color-map
   (quote
    ((20 . "#C99090")
     (40 . "#D9A0A0")
     (60 . "#ECBC9C")
     (80 . "#DDCC9C")
     (100 . "#EDDCAC")
     (120 . "#FDECBC")
     (140 . "#6C8C6C")
     (160 . "#8CAC8C")
     (180 . "#9CBF9C")
     (200 . "#ACD2AC")
     (220 . "#BCE5BC")
     (240 . "#CCF8CC")
     (260 . "#A0EDF0")
     (280 . "#79ADB0")
     (300 . "#89C5C8")
     (320 . "#99DDE0")
     (340 . "#9CC7FB")
     (360 . "#E090C7"))))
 '(vc-annotate-very-old-color "#E090C7"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit default))))
 '(show-paren-match ((t (:weight normal))))
 '(sp-show-pair-match-face ((t (:background "#1B1E1C" :foreground "#87D700" :weight normal))))
 '(trailing-whitespace ((t (:background "#602020")))))
