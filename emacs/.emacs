;; http://blog.aaronbieber.com/2016/01/23/living-in-evil.html

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom"))
(setenv "PATH" (concat (getenv "PATH") ":/home/alex/.cabal/bin"))
(setq exec-path (append exec-path '("/home/alex/.cabal/bin")))

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

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed
  'evil-leader
  'evil
  'ghc
  'cl-lib
  'company
  'anaconda-mode
  'company-anaconda
  'company-ghc
  'flycheck
  'ido
  'which-key
  'helm
  'projectile
  'helm-projectile
  'persp-projectile
  'perspective
  'popwin
  'web-mode
  'js2-mode
  'ob-ipython
  'helm-core
  'smartparens
  )

(require 'use-package)

(require 'myfuncs)
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom-themes/")

;; ----------------
;; various
;; ----------------
;; remember last place in file
;; (use-package saveplace)
;; (setq-default save-place t)
;; (save-place-mode 1)
(global-undo-tree-mode)
(diminish 'undo-tree-mode "")
(setq show-paren-delay 0.3)

(if (<= emacs-major-version 24)
    (use-package saveplace
       :ensure t
       :config
       (setq-default save-place t)
    )
  (save-place-mode 1))

;(show-paren-mode 1)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; show column
(setq column-number-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package neotree
  :ensure t
  )

;; only with this set to nil can org-mode export & open too
(setq process-connection-type nil)

;; --------------
;; custom functions
;; --------------




;; ----------------
;; UI & themes
;; ----------------
; disable annoying stuff
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(global-linum-mode t)
(setq linum-format "%4d ")
;(defun linum-format-func (line)
  ;(let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     ;(propertize (format (format "%%%dd" w) line) 'face 'linum)))
;(setq linum-format 'linum-format-func)

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


(setq text-scale-mode-step 1.05)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


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
(if (display-graphic-p)
    (progn
      (require 'git-gutter-fringe)
      (global-git-gutter-mode +1)
      )
  (progn
    (global-git-gutter-mode +1)
    ;; (set-face-background 'git-gutter:modified "purple")
    ;; (set-face-foreground 'git-gutter:modified "green")
    ;; (set-face-background 'git-gutter:added "purple")
    ;; (set-face-foreground 'git-gutter:added "green")
    ;; (set-face-background 'git-gutter:deleted "purple")
    ;; (set-face-foreground 'git-gutter:deleted "green")
    ))
(diminish 'git-gutter-mode "")


;; ----------------
;; evil
;; ----------------
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  )
(setq evil-want-C-i-jump nil)
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  ;; emacs mode is default in some modes
  (delete 'term-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode)

  ;; neotree
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

  ;; move state to beginning of modeline
  (setq evil-mode-line-format '(before . mode-line-front-space))
  ;; change state colors
  (setq evil-normal-state-tag   (propertize " <N> " 'face '((:foreground "#268bd2" :weight extra-bold)))
        evil-emacs-state-tag    (propertize " <E> " 'face '((:foreground "#dc752f" :weight extra-bold)))
        evil-insert-state-tag   (propertize " <I> " 'face '((:foreground "#2aa198" :weight extra-bold)))
        evil-replace-state-tag  (propertize " <R> " 'face '((:foreground "#df005f" :weight extra-bold)))
        evil-motion-state-tag   (propertize " <M> " 'face '((:foreground "#df005f" :weight extra-bold)))
        evil-visual-state-tag   (propertize " <V> " 'face '((:foreground "#d75fd7" :weight extra-bold)))
        evil-operator-state-tag (propertize " <O> " 'face '((:foreground "#df005f" :weight extra-bold))))

  ;; this is needed to be able to use C-h
  (global-set-key (kbd "C-h") 'undefined)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;(define-key evil-insert-state-map (kbd "C-h") 'backward-char)
  ;(define-key evil-insert-state-map (kbd "C-j") 'next-line)
  ;(define-key evil-insert-state-map (kbd "C-k") 'previous-line)
  ;(define-key evil-insert-state-map (kbd "C-l") 'forward-char)

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

  ;; evilnv toggles instead of commenting/uncommenting
  (setq evilnc-invert-comment-line-by-line t)

  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "]"  'find-tag-other-window
    ";"  'evilnc-comment-or-uncomment-lines
    ")"  'my/fix-theme

    "s"  'shell-command

    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bb" 'helm-buffers-list
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit

    "ts" 'flycheck-mode
    "tb" 'my/toggle-bg
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
;(with-eval-after-load "evil"
  ;(load "evilhacks"))


;; ----------------
;; magit
;; ----------------
(evil-define-key 'normal magit-blame-mode-map (kbd "q") 'magit-blame-quit)


;; ----------------
;; python
;; ----------------
(use-package pyvenv)
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
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
  (defvar c-eldoc-includes "-I/usr/include -I/usr/include/python3.5 -I./ -I../ ")
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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default
 ;; js2-mode
 js2-basic-offset 2
 js-indent-level 2
 ;; web-mode
 css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2)

(add-hook 'js2-mode-hook (lambda ()
         (setq evil-shift-width 2)
         (setq-default indent-tabs-mode nil)
         (setq jsmd "js")))

(add-hook 'web-mode-hook (lambda ()
           (setq jsmd "web")))

(evil-leader/set-key
  "tj" 'my/toggle-jsmodes)

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
;; company & completions
;; ----------------
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  )

(eval-after-load "company"
  '(progn
    (add-to-list 'company-backends 'company-anaconda)
    (add-to-list 'company-backends '(company-irony-c-headers company-c-headers company-irony))
    (add-to-list 'company-backends 'company-ghc)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    ;(define-key company-active-map (kbd "TAB") 'company-select-next)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "C-l") 'company-complete-selection)))
;(ac-config-default)
(company-quickhelp-mode 1)
(diminish 'company-mode " Com")

;(use-package smartparens-config)


(use-package smartparens
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
    (add-hook 'comint-mode-hook 'smartparens-mode)
  :config
  (progn
    (require 'smartparens-config)
    ;; (spacemacs//adaptive-smartparent-pair-overlay-face)
    ;; (add-hook 'spacemacs-post-theme-change-hook
    ;;           'spacemacs//adaptive-smartparent-pair-overlay-face)
    (show-smartparens-global-mode +1)
    ;; don't create a pair with single quote in minibuffer
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-pair "(" nil :post-handlers
             '(:add (my/smartparens-pair-newline-and-indent "RET")))
    (sp-pair "{" nil :post-handlers
             '(:add (my/smartparens-pair-newline-and-indent "RET")))
    (sp-pair "[" nil :post-handlers
             '(:add (my/smartparens-pair-newline-and-indent "RET")))
    )))
(diminish 'smartparens-mode "")


;; ----------------
;; syntax checking
;; ----------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (eval-after-load 'flycheck
      '(progn
        (set-face-background 'flycheck-warning "unspecified-bg")
        (set-face-foreground 'flycheck-warning "unspecified-fg")))

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
  )
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;; ----------------
;; ido mode
;; ----------------
;; (use-package ido
;;   :ensure t
;;   :config
;;   (setq ido-enable-prefix t)
;;   (ido-mode t)
;;
;;   (defun bind-ido-keys ()
;;     "Keybindings for ido mode."
;;     (define-key ido-completion-map (kbd "C-h") 'ido-up-directory))
;;
;;   (add-hook 'ido-setup-hook #'bind-ido-keys)
;;   )
(use-package flx-ido
  :ensure  t
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  )



;; ----------------
;; misc
;; ----------------
(which-key-mode)
(diminish 'which-key-mode "")



;; ----------------
;; helm
;; ----------------
(use-package helm-config)
(use-package helm-themes)

(use-package helm-mode
    :config (helm-mode 1))

(use-package helm-adaptive
    :config (helm-adaptive-mode 1))

(use-package helm-ring
    :config (helm-push-mark-mode 1))

(use-package helm-utils
    ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
    :config (helm-popup-tip-mode 1))

(use-package helm-sys
    :config (helm-top-poll-mode 1))

(use-package helm-fuzzy-find
  :ensure t
  )

(setq helm-M-x-fuzzy-match t)
(setq helm-locate-fuzzy-match t)

;; Global-map
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
;; (global-set-key (kbd "M-y")                          'helm-show-kill-ring)
;(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
;; (global-set-key (kbd "C-h d")                        'helm-info-at-point)
;; (global-set-key (kbd "C-h i")                        'helm-info)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
;; (global-set-key (kbd "C-h C-f")                      'helm-apropos)
;; (global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
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
;(evil-add-hjkl-bindings helm-map 'emacs (kbd "C-j") 'helm-next-line)
;(evil-add-hjkl-bindings helm-map 'emacs (kbd "C-k") 'helm-previous-line)
;(define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
;(evil-add-hjkl-bindings helm-map (kbd "C-l") (kbd "RET"))

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

;(with-eval-after-load 'helm-files
;  (dolist (keymap (list helm-find-files-map helm-read-file-map))
;    (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
;    (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
;    ;; rebind `describe-key' for convenience
;    (define-key keymap (kbd "C-S-h") 'describe-key)))
(when linum-mode
  (add-hook 'helm-after-initialize-hook (lambda ()
                                          (with-helm-buffer
                                            (linum-mode 0)))))

(global-set-key (kbd "C-x C-f") 'my/helm-find-files)
(diminish 'helm-mode "")

(projectile-global-mode)
(use-package helm-projectile)
(helm-projectile-on)

(persp-mode)
(use-package persp-projectile)



;; ----------------
;; org mode
;; ----------------
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
;; format string used when creating CLOCKSUM lines and when generating a
;; time duration (avoid showing days)
(setq org-time-clocksum-format
    '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(add-hook 'org-mode-hook (lambda ()
         (define-key org-mode-map (kbd "TAB") 'org-cycle)
         (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
			   (org-bullets-mode 1)
			   (org-babel-do-load-languages
			    'org-babel-load-languages
			    '((ipython . t)
			      ;; other languages..
			      ))
			   (use-package ox-twbs
			     :ensure t
			     )
			   (evil-leader/set-key
			     "oc" 'org-table-delete-column
			     "or" 'org-table-kill-row)))

;; ----------------------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "28130127bbf3072c1bbc7652fca7245f186bb417b3b385a5e4da57b895ffe9d8" default)))
 '(fci-rule-color "#20240E")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(org-agenda-files (quote ("~/oeworks/oeworks.org" "~/org/vermantia.org")))
 '(package-selected-packages
   (quote
    (auto-complete-clang neotree company-irony-c-headers imenu-list helm-ag fuzzy-match wc-mode gruvbox-theme powerline sublime-themes org helm-fuzzy-find magit git-gutter-fringe fringe-helper git-gutter-fringe+ org-clock-csv flx-ido company-c-headers ox-twbs solarized-theme auctex org-bullets evil-surround zenburn-theme which-key web-mode use-package spacemacs-theme smartparens pyenv-mode popwin persp-projectile ob-ipython monokai-theme molokai-theme moe-theme material-theme js2-mode jedi intero highlight-numbers highlight-escape-sequences helm-projectile hc-zenburn-theme git-gutter git-gutter+ evil-terminal-cursor-changer evil-nerd-commenter evil-leader esup company-quickhelp company-ghci company-ghc company-anaconda color-theme-sanityinc-tomorrow atom-one-dark-theme ac-anaconda)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
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
 '(sml/mode-width
   (if
       (eq
	(powerline-current-separator)
	(quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir)))
		   (quote powerline-active1)
		   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir)))
		   (quote powerline-active1)
		   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir)))
		   (quote sml/global)
		   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir)))
		   (quote powerline-active2)
		   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

;(set-frame-font "Source Code Pro-10" nil t)
;(set-frame-font "Ubuntu Mono-13" nil t)
(set-frame-font "Menlo for Powerline-11" nil t)
;(set-frame-font "DejaVu Sans Mono-10" nil t)
;(require 'init-powerline)
;(load-theme 'spacemacs-dark)
(if (display-graphic-p)
    (progn
      (load-theme 'spolsky t)
      ;(setq linum-format "%4d ")
      (setq linum-format 'dynamic)
      (set-face-attribute 'vertical-border nil :foreground "dim gray")
      )
  (progn
    (load-theme 'monokai)
    (setq linum-format 'dynamic)
    ;; (my/fix-theme)
    (set-face-attribute 'mode-line nil :background "#404040")
    (set-face-attribute 'mode-line-inactive nil :background "#282828")
    (set-face-attribute 'sp-show-pair-match-face nil :inverse-video nil)
    ))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit default)))))
