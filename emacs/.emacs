(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom-themes/")
(setenv "PATH" (concat (getenv "PATH") ":/home/alex/.cabal/bin"))
(setq exec-path (append exec-path '("/home/alex/.cabal/bin")))
(setq exec-path (append exec-path '("/home/alex/.nvm/versions/node/v6.9.5/bin")))
(setq user-full-name "Alex Peitsinis"
      user-mail-address "alexpeitsinis@gmail.com")

;; ----------------
;; Package management
;; ----------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("MELPA Stable" . "https://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
;; activate installed packages
(package-initialize)
(setq package-enable-at-startup nil)

(require 'use-package)

(require 'myfuncs)
(require 'myengines)


;; ----------------
;; various
;; ----------------

;; remember last position
(if (<= emacs-major-version 24)
    (use-package saveplace
       :ensure t
       :config
       (setq-default save-place t)
    )
  (save-place-mode 1))

;; undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode "")
  )

;; highlight numbers
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  )

(use-package neotree
  :ensure t
  )

(setq show-paren-delay 0.3)

;; show column in modeline
(setq column-number-mode t)

;; use column width 80 to fill (e.g. with gq)
(setq-default fill-column 80)

;; store all backup and autosave files in
;; one dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; only with this set to nil can org-mode export & open too
(setq process-connection-type nil)

;; i love this
(defalias 'yes-or-no-p #'y-or-n-p)

;; always scroll to the end of compilation buffers
(setq compilation-scroll-output t)

;; some keymaps
(global-set-key (kbd "M-o") 'other-window)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-p") 'find-function-at-point)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; flyspell on pure text buffers
(dolist (hook '(text-mode-hook change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; what it says
(defun my/revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
          (message "Refreshed open files"))

;; ----------------
;; UI & themes
;; ----------------


(setq-default indent-tabs-mode nil)
(when window-system
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  ;; (setq solarized-high-contrast-mode-line t)
  ;; (load-theme 'solarized-dark t)
  )

;; disable annoying stuff
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; linum
(use-package linum
  :config
  ;; (global-linum-mode t)
  (add-hook 'prog-mode-hook (lambda () (linum-mode t)))
  ;; (setq linum-format "%4d ")
  (setq linum-format 'dynamic)
  )

;; always open helm buffers at bottom
(use-package popwin
  :ensure t
  :config
  (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
  (add-hook 'helm-after-initialize-hook (lambda ()
                                            (popwin:display-buffer helm-buffer t)
                                            (popwin-mode -1)))
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
  )

; font size
(setq text-scale-mode-step 1.05)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


;; highlight trailing whitespace
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(use-package smartparens
  :ensure t
  :defer t
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :init
  (progn
    ;; settings
    (setq sp-show-pair-delay 0.2
          ;; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)
    (add-hook 'prog-mode-hook 'smartparens-mode)
    (add-hook 'comint-mode-hook 'smartparens-mode))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  ;; don't create a pair with single quote in minibuffer
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-pair "(" nil :post-handlers
	   '(:add (my/smartparens-pair-newline-and-indent "RET")))
  (sp-pair "{" nil :post-handlers
	   '(:add (my/smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
	   '(:add (my/smartparens-pair-newline-and-indent "RET")))
  (diminish 'smartparens-mode "")
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (diminish 'which-key-mode "")
  )

(use-package imenu-list
  :ensure t
  :config
  (global-set-key (kbd "C-\\") #'imenu-list-minor-mode)
  (setq imenu-list-size 30)
  )

;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (require 'spaceline-config)
;;   (setq powerline-height 15)
;;   ;; (setq powerline-default-separator "slant")
;;   (setq powerline-default-separator 'utf-8)
;;   (spaceline-spacemacs-theme)
;;   )

;; ----------------
;; term
;; ----------------

;; no line numbers in terminal
(add-hook 'term-mode-hook (lambda () (linum-mode 0)))

;; automatically close term buffers on EOF
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)


;; ----------------
;; VCS
;; ----------------

(use-package magit
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

;; (if (display-graphic-p)
;;     (progn
;;       (require 'git-gutter-fringe)
;;       (global-git-gutter-mode +1)
;;       )
;;   (progn
;;     (global-git-gutter-mode +1)
;;     ;; (set-face-background 'git-gutter:modified "purple")
;;     ;; (set-face-foreground 'git-gutter:modified "green")
;;     ;; (set-face-background 'git-gutter:added "purple")
;;     ;; (set-face-foreground 'git-gutter:added "green")
;;     ;; (set-face-background 'git-gutter:deleted "purple")
;;     ;; (set-face-foreground 'git-gutter:deleted "green")
;;     ))
;; (diminish 'git-gutter-mode "")
(if (display-graphic-p)
    (progn
      (use-package diff-hl
        :ensure t
        :config
        ;; (add-hook 'prog-mode-hook 'diff-hl-mode)
        ;; (add-hook 'vc-dir-mode-hook 'diff-hl-mode)
        (global-diff-hl-mode)
        (diff-hl-flydiff-mode)
        )))


;; ----------------
;; evil
;; ----------------
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  )

(use-package evil
  :ensure t
  :config
  (setq evil-want-C-i-jump nil)
  (evil-mode 1)

  ;; emacs mode is default in some modes
  (delete 'term-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (delete 'inferior-python-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)

  ;; magit
  (evil-define-key 'normal magit-blame-mode-map (kbd "q") 'magit-blame-quit)

  ;; neotree
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

  ;; move state to beginning of modeline
  (setq evil-mode-line-format '(before . mode-line-front-space))
  ;; change state colors
  ;; (setq evil-normal-state-tag   (propertize " <N> " 'face '((:foreground "#268bd2" :weight extra-bold)))
  ;;       evil-emacs-state-tag    (propertize " <E> " 'face '((:foreground "#dc752f" :weight extra-bold)))
  ;;       evil-insert-state-tag   (propertize " <I> " 'face '((:foreground "#2aa198" :weight extra-bold)))
  ;;       evil-replace-state-tag  (propertize " <R> " 'face '((:foreground "#df005f" :weight extra-bold)))
  ;;       evil-motion-state-tag   (propertize " <M> " 'face '((:foreground "#df005f" :weight extra-bold)))
  ;;       evil-visual-state-tag   (propertize " <V> " 'face '((:foreground "#d75fd7" :weight extra-bold)))
  ;;       evil-operator-state-tag (propertize " <O> " 'face '((:foreground "#df005f" :weight extra-bold))))

  ;; this is needed to be able to use C-h
  (global-set-key (kbd "C-h") 'undefined)
  (define-key evil-insert-state-map (kbd "C-k") nil)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-visual-state-map (kbd ";") 'evil-ex)
  (evil-ex-define-cmd "sv" 'split-window-below)

  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

  (define-key evil-visual-state-map (kbd "<") '(lambda ()
                 (interactive)
                 (progn
                     (call-interactively 'evil-shift-left)
                     (execute-kbd-macro "gv"))))

  (define-key evil-visual-state-map (kbd ">") '(lambda ()
                 (interactive)
                 (progn
                     (call-interactively 'evil-shift-right)
                     (execute-kbd-macro "gv"))))

  ;; evilnc toggles instead of commenting/uncommenting
  (setq evilnc-invert-comment-line-by-line t)

  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "]"  'find-tag-other-window
    ";"  'evilnc-comment-or-uncomment-lines
    ")"  'my/fix-theme
    "h"  'help

    "s"  'shell-command

    "um" 'menu-bar-mode
    "up" 'rainbow-delimiters-mode
    "uh" 'rainbow-mode

    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bb" 'helm-buffers-list
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit

    "tb" 'my/toggle-bg
    "tg" 'global-diff-hl-mode
    "th" 'global-hl-line-mode
    "tl"  'linum-mode
    "ts" 'flycheck-mode
    "tw" 'my/toggle-scrolling

    "j"  'my/jump-to-definition

    "pp" 'projectile-persp-switch-project
    "pl" 'persp-next
    "ph" 'persp-prev
    "pr" 'persp-rename
    "pq" 'persp-kill

    "Ts" 'helm-themes
    "ff" 'helm-find
    "fa" 'helm-ag

    "ft" 'neotree-toggle)
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  )


;; ----------------
;; python
;; ----------------
(use-package pyvenv) ;; this has to be downloaded
(setq python-shell-prompt-detect-failure-warning nil)
(my|define-jump-handlers python-mode)
(my|define-jump-handlers cython-mode anaconda-mode-goto)
(add-hook 'python-mode-hook (lambda ()
                              (anaconda-mode)
                              (diminish 'anaconda-mode " An")
                              (anaconda-eldoc-mode)
                              (diminish 'anaconda-eldoc-mode "")
                              (add-to-list 'my-jump-handlers-python-mode
					   '(anaconda-mode-find-definitions :async t))))
(diminish 'eldoc-mode "")


;; ----------------
;; c/c++
;; ----------------
(use-package irony
  :ensure t
  :defer t
  :init
  ;; (add-hook 'c++-mode-hook 'irony-mode)
  ;; (add-hook 'c-mode-hook 'irony-mode)
  ;; (add-hook 'objc-mode-hook 'irony-mode)
  ;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  ;; (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
  (use-package ggtags :ensure t)
  (dolist (hook '(c++-mode-hook
                  c-mode-hook
                  objc-mode-hook))
    (add-hook hook #'(lambda()
                       (irony-mode)
                       (ggtags-mode 1)
                       (c-turn-on-eldoc-mode)
                       ;; (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
                       ;; (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
                       ;; (turn-on-eldoc-mode)
                       )))
  (defvar c-eldoc-includes "-I/usr/include -I/usr/include/python3.5m -I./ -I../")
  :config
  (defun my-irony-mode-hook ()
    (defun irony-snippet-available-p () -1)
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (use-package company-irony-c-headers :ensure t :defer t)
  )

(my|define-jump-handlers c-mode)
(my|define-jump-handlers c++-mode)
(setq c-default-style "linux"
      c-basic-offset 4)


;; ----------------
;; haskell
;; ----------------

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;; ----------------
;; js
;; ----------------
(use-package nvm :ensure t)
(defun nvm-use-ver (version)
  (interactive "sVersion: ")
  (nvm-use version))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(my|define-jump-handlers js2-mode)
(my|define-jump-handlers web-mode)
(add-hook 'js2-mode-hook (function (lambda ()
                                     (nvm-use "v6.9.5")
                                     (setq evil-shift-width 2)
                                     (tern-mode)
                                     (add-to-list 'my-jump-handlers-js2-mode
                                                  'tern-find-definition))))
(add-hook 'web-mode-hook (function (lambda ()
                                     (nvm-use "v6.9.5")
                                     (setq evil-shift-width 2)
                                     (tern-mode)
                                     (add-to-list 'my-jump-handlers-web-mode
                                                  'tern-find-definition))))
(setq
 ;; js2-mode
 js2-basic-offset 2
 js-indent-level 2
 ;; web-mode
 css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2)

;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(defun my/toggle-jsmodes ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((mode major-mode))
      (cond
       ((string= mode "js2-mode") (web-mode))
       ((string= mode "web-mode") (js2-mode))
       ((string= mode "js-mode") (js2-mode))))))

(evil-leader/set-key "tj" 'my/toggle-jsmodes)

;; ----------------
;; lisps
;; ----------------
;; Common LISP
(use-package slime
  :ensure t
  :defer t
  :init
  ;; set up slime according to this link
  ;; http://www.jonathanfischer.net/modern-common-lisp-on-linux/
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  (use-package slime-company :ensure t :defer t)
  (slime-setup '(slime-fancy slime-company))
  (put 'if 'common-lisp-indent-function 2)
  )

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                common-lisp-mode-hook
                lisp-interaction-mode-hook
                clojure-mode-hook))
  (add-hook hook #'(lambda() (smartparens-mode 0))))

;; expand macros in another window
(global-set-key (kbd "C-c C-m") '(lambda () (interactive) (macrostep-expand t)))

;; ----------------
;; LaTeX
;; ----------------
(defun my/latex-setup ()
  (defun my/texcount ()
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" "-nc" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count))))
  (define-key LaTeX-mode-map "\C-cw" 'my/texcount))

(add-hook 'LaTeX-mode-hook 'my/latex-setup t)

;; ----------------
;; json, yaml, markdown, rst
;; ----------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  )

(use-package yaml-mode :ensure t)


;; ----------------
;; company & completions
;; ----------------
(use-package company
  :ensure t
  :defer t
  :init
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.3)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (company-quickhelp-mode 1)
  (diminish 'company-mode " Com")
  (eval-after-load "company"
    '(progn
       (add-to-list 'company-backends 'company-anaconda)
       (add-to-list 'company-backends '(company-irony-c-headers company-c-headers company-irony))
       (add-to-list 'company-backends 'company-ghc)
       (add-to-list 'company-backends 'company-tern)
       (define-key company-active-map (kbd "C-k") 'company-select-previous)
       (define-key company-active-map (kbd "C-j") 'company-select-next)
       (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "C-l") 'company-complete-selection)
       (define-key company-active-map (kbd "C-f") 'company-show-location)
       (setq company-minimum-prefix-length 3)
       ))
  )


;; ----------------
;; syntax checking
;; ----------------

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flymake-yaml :ensure t)
  (eval-after-load 'flycheck
    '(progn
       (set-face-background 'flycheck-warning "unspecified-bg")
       (set-face-foreground 'flycheck-warning "unspecified-fg")
       (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
      ))
  (define-key global-map (kbd "C-c ! t") 'flycheck-mode)
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.33)))

  (evil-leader/set-key
    "el" 'my/toggle-flycheck-error-list)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq-default flycheck-temp-prefix ".flycheck")
  )


;; ----------------
;; ido mode
;; ----------------
(use-package flx-ido
  :ensure  t
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  )


;; ----------------
;; hydra
;; ----------------
;; (use-package hydra
;;   :ensure t
;;   :config
;;   (defhydra hydra-common (global-map "<f5>")
;;     "common functions"
;;     ("g" text-scale-increase "in")
;;     ("l" text-scale-decrease "out"))
;;   )

;; ----------------
;; helm
;; ----------------
(use-package helm-config)
(use-package helm-themes)
(use-package helm-mode :config (helm-mode 1))
(use-package helm-adaptive :config (helm-adaptive-mode 1))
(use-package helm-utils :config (helm-popup-tip-mode 1))
(use-package helm-sys :config (helm-top-poll-mode 1))
(use-package helm-fuzzy-find :ensure t)
(use-package helm-ag :ensure t)

(setq helm-M-x-fuzzy-match t)
(setq helm-locate-fuzzy-match t)

;; Global-map
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-s")                          'helm-occur)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
(define-key global-map (kbd "M-g i")                 'helm-gid)
(define-key global-map (kbd "C-x r p") 'helm-projects-history)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-l") (kbd "RET"))

;; splitting
(define-key helm-map (kbd "C-c s") 'my/helm-file-switch-other-window-horizontally)
(define-key helm-map (kbd "C-c v") 'my/helm-file-switch-other-window-vertically)
(define-key helm-buffer-map (kbd "C-c s") 'my/helm-buffer-switch-other-window-horizontally)
(define-key helm-buffer-map (kbd "C-c v") 'my/helm-buffer-switch-other-window-vertically)

(with-eval-after-load 'helm-files
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-find-files-map
    (kbd "S-<tab>") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map
    (kbd "<backtab>") 'helm-find-files-up-one-level)
  ;; For terminal.
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-find-files-map
    (kbd "S-TAB") 'helm-find-files-up-one-level)
  (define-key helm-map (kbd "C-z") 'helm-select-action))

(when linum-mode
  (add-hook 'helm-after-initialize-hook (lambda ()
                                          (with-helm-buffer
                                            (linum-mode 0)))))

(global-set-key (kbd "C-x C-f") 'my/helm-find-files)
(diminish 'helm-mode "")

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  )

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  )

(use-package perspective :config (persp-mode))
(use-package persp-projectile :ensure t)


;; ----------------
;; org mode
;; ----------------
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(setq org-confirm-babel-evaluate nil)
(setq org-clock-into-drawer nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-directory (expand-file-name "~/org/"))
(setq org-default-notes-file (concat org-directory "capture.org"))
;; (setq org-src-window-setup 'current-window)
;; format string used when creating CLOCKSUM lines and when generating a
;; time duration (avoid showing days)
(setq org-time-clocksum-format
    '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(add-hook 'org-mode-hook (lambda ()
			   (define-key org-mode-map (kbd "TAB") 'org-cycle)
			   (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
                           (add-to-list 'org-structure-template-alist '("pf" "#+BEGIN_SRC ipython :session :file %file :exports both\n?\n#+END_SRC"))
                           (add-to-list 'org-structure-template-alist '("po" "#+BEGIN_SRC ipython :session :exports both\n?\n#+END_SRC"))
			   (org-bullets-mode 1)
			   (org-babel-do-load-languages
			    'org-babel-load-languages
			    '((python . t)
                              (ipython . t)
			      ;; other languages..
			      ))
			   (use-package ox-twbs :ensure t)
			   (evil-leader/set-key
			     "oc" 'org-table-delete-column
			     "or" 'org-table-kill-row)))


;; ----------------------------------------------------------------------------------------------

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Inconsolata-11" nil t)
;; (set-frame-font "Fira Mono-10" nil t)
;; (set-frame-font "Ubuntu Mono-12" nil t)
;; (set-frame-font "Liberation Mono-10" nil t)
;; (set-frame-font "DejaVu Sans Mono-10" nil t)
;; (set-frame-font "Consolas-10.5" nil t)
;; (setq spacemacs-theme-org-height nil)
(if (display-graphic-p)
    (progn
      ;; (load-theme 'tango t)
      (load-theme 'solarized-dark t)
      (add-hook 'org-mode-hook (lambda ()
                                 (set-face-attribute 'org-block-background nil :background "#04303B")
                                 (set-face-attribute 'org-block nil :background "#04303B")
                                 (set-face-attribute 'org-block-begin-line nil :background "#073642")
                                 (set-face-attribute 'org-block-end-line nil :background "#073642")))
      ;; (use-package theme-changer
	;; :ensure t
	;; :config
	;; (setq calendar-latitude 37.98)
	;; (setq calendar-longitude 23.72)
	;; (change-theme 'solarized-light 'solarized-dark)
        ;; )
      ;; (load-theme 'sanityinc-tomorrow-night t)
      ;(set-face-attribute 'cursor nil :background "gray")
      )
  (progn
    (load-theme 'spacemacs-dark t)
    ;; (load-theme 'monokai)
    ;; (set-face-attribute 'mode-line nil :background "#404040")
    ;; (set-face-attribute 'mode-line-inactive nil :background "#282828")
    ))
(setq linum-format 'dynamic)

(set-face-attribute 'show-paren-match nil :weight 'normal)
(set-face-attribute 'trailing-whitespace nil :background "#602020")
