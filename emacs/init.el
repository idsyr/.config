(require 'package)
(setq package-archives '(("GNU ELPA"    . "https://elpa.gnu.org/packages/")
                         ("MELPA"       . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
                         ("NonGNU ELPA" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)



(mapc (lambda (mode) (funcall mode -1))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(global-display-line-numbers-mode 1)

(global-visual-line-mode 1)

(setq indent-tabs-mode nil)

(setq backup-directory-alist '((".*" . "~/.emacs_backup")))

(set-frame-parameter (selected-frame) 'buffer-predicate
		     (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))

(global-auto-revert-mode t)

(setq dired-kill-when-opening-new-dired-buffer t)
(eval-after-load 'dired
  '(define-key dired-mode-map [?\r] 'dired-find-alternate-file))
(put 'dired-find-alternate-file 'disabled nil)

; ------------------------------------------
;  KEYBINDS
; ------------------------------------------

(use-package evil
  :config
  (evil-mode t)
  )

(use-package general
  :config
  (general-evil-setup)
  (global-set-key [escape] 'keyboard-escape-quit)
  (general-create-definer idsyr/leader-keys
    :states '(normal insert visual)
    :prefix "SPC"
    :global-prefix "M-SPC"
    )
  (idsyr/leader-keys
    "f" 'find-file
    )
  (idsyr/leader-keys
    "b n" 'next-buffer
    "b p" 'previous-buffer
    "b k" 'kill-current-buffer
    "b l" 'switch-to-buffer
    "b r" 'rename-buffer
    )
  (idsyr/leader-keys
    "t n" 'display-line-numbers-mode
    "t l" 'visual-line-mode
    )
  (idsyr/leader-keys
    "w n" 'evil-window-next
    "w k" 'evil-window-delete
    )
  )


; ------------------------------------------
;  THEME
; ------------------------------------------

(add-to-list 'default-frame-alist '(alpha-background . 80))

(use-package adwaita-dark-theme
  :config
  (load-theme 'adwaita-dark t)
  )

(add-to-list 'default-frame-alist '(font . "BlexMonoNerdFontMono 11"))

(setq inhibit-startup-screen t)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-startup-banner '"~/.config/emacs/icon.png")
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)
(setq dashboard-startupify-list
      '(dashboard-insert-newline
        dashboard-insert-newline
        dashboard-insert-newline
        dashboard-insert-banner
        dashboard-insert-newline
        dashboard-insert-init-info
        dashboard-insert-items))
(setq dashboard-items '((recents . 10)))

; ------------------------------------------
;  AUTOFORMAT
; ------------------------------------------

(use-package clang-format+
  :hook ((c-mode   . clang-format+-mode)
	 (c++-mode . clang-format+-mode))
  )

(setq clang-format+-context 'definition)
(setq clang-format-style "file") 

(defun clang-format-on-insert ()
  (when (member (this-command-keys) '(";" "}"))
    (clang-format+-apply-to-modifications)))

(defun setup-clang-format ()
  (unless (memq #'clang-format-on-insert post-self-insert-hook)
    (add-hook 'post-self-insert-hook #'clang-format-on-insert nil t)))

(add-hook 'c-mode-hook #'setup-clang-format)
(add-hook 'c++-mode-hook #'setup-clang-format)


; ------------------------------------------
;  SYNTAX CHECK
; ------------------------------------------

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  )
 '(eglot-inlay-hint-face ((t (:height 1.0 :family "MononokiNerdFontMono" :slant italic :inherit shadow))))
(setq eglot-ignored-server-capabilites '(:inlayHintProvider))


; ------------------------------------------
;  AUTOCOMPLETE
; ------------------------------------------

(use-package company
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "<space>") 'company-abort)
  )


; ------------------------------------------
;  LANG SUPPORT
; ------------------------------------------

(use-package cmake-mode )
(use-package lua-mode)
