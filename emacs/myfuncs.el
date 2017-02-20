;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
(setq themebg "default")

;; (defun my/fix-theme ()
;;   (interactive)
;;   (set-face-attribute 'mode-line nil :background "#404040")
;;   (set-face-attribute 'mode-line-inactive nil :background "#282828")
;;   (set-face-attribute 'sp-show-pair-match-face nil :inverse-video nil)
;;   (set-face-attribute 'vertical-border nil :foreground "dim gray")
;;   )

(defun my/fix-theme ()
  (interactive)
  (setq linum-format 'dynamic)
  (set-face-attribute 'vertical-border nil :foreground "dim gray")
  )

(defun my/dark-bg ()
  (custom-set-faces
   '(default ((t (:inherit default
                           :background "#1D1F21" ))))
   '(font-lock-comment-face ((t (:inherit font-lock-comment-face
                                          :background "#1D1F21" ))))
   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-delimiter-face
                                                    :background "#1D1F21" )))))
  (setq themebg "dark")
  (message "Toggled background color: dark")
  )

(defun my/default-bg ()
  (custom-set-faces
   '(default ((t (:inherit default))))
   '(font-lock-comment-face ((t (:inherit font-lock-comment-face))))
   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-delimiter-face)))))
  (setq themebg "default")
  (message "Toggled background color: default")
  )

(defun my/toggle-bg ()
  (interactive)
  (if (equal themebg "default")
      (my/dark-bg)
    (my/default-bg))
  )

(setq scroll-conservatively 101)

(defun my/toggle-scrolling ()
  (interactive)
  (if (equal scroll-conservatively 0)
      (setq scroll-conservatively 101)
    (setq scroll-conservatively 0))
  )

(defun my/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun my/smartparens-pair-newline-and-indent (id action context)
  (my/smartparens-pair-newline id action context)
  (indent-according-to-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump to definition

(defvar my-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local my-jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro my|define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.
This defines a variable `my-jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `my-jump-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my//init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "my-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`my-default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq my-jump-handlers
               (append ,handlers-list
                       my-default-jump-handlers)))
       (add-hook ',mode-hook ',func)
       )))

(defun my/jump-to-definition ()
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler my-jump-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or async
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major modes

;; JS
(setq jsmd "none")

(defun my/toggle-jsmodes ()
  (interactive)
  (if (equal jsmd "js")
      (web-mode)
    (js2-mode))
  )

;; org-mode
(defun my/timesheet-block ()
  (interactive)
  (insert-file-contents "~/.emacs.d/timesheet.org"))

;; helm-projectile-persp-switch-project!
(defun my/switch-project ()
  (interactive)
  (persp-switch (let ((temp-charset "1234567890abcdefghijklmnopqrstuvwxyz")
                      (random-string ""))
                  (dotimes (i 6 random-string)
                    (setq random-string
                          (concat
                           random-string
                           (char-to-string (elt temp-charset (random (length temp-charset)))))
                          ))
                  ))
  (helm-projectile-switch-project)
  (persp-rename (projectile-project-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error checking

(defun my/toggle-flycheck-error-list ()
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm

(defun my/helm-find-files (arg)
  "Custom spacemacs implementation for calling helm-find-files-1.
Removes the automatic guessing of the initial value based on thing at point. "
  (interactive "P")
  (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))

(provide 'myfuncs)


(defun my/shell-command-to-string ()
  (interactive)
  (let ((cmd (read-string "Command: ")))
  (insert (shell-command-to-string cmd)))
  )
