(require 'package)
(setq package-archives '(("GNU ELPA"    . "https://elpa.gnu.org/packages/")
			 ("MELPA"       . "https://melpa.org/packages/")
			 ("NonGNU ELPA" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)



(use-package evil
  :ensure t
  :config
  (evil-mode t)
  )

(use-package general
  :ensure t
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
    "w k" 'evil-window-kill
    )
  )



(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'latte)
  (load-theme 'catppuccin t)
  )

(add-to-list 'default-frame-alist '(font . "MononokiNerdFontMono 11"))

(mapc (lambda (mode) (funcall mode -1))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode)
      )

(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)



(setq backup-directory-alist '((".*" . "~/.emacs_backup")))

(global-auto-revert-mode t)

(set-frame-parameter (selected-frame) 'buffer-predicate
		     (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))

(use-package el-fetch
  :ensure t
  :config
  (setq inhibit-startup-screen t)
  (add-hook 'emacs-startup-hook 'el-fetch)
  )



(use-package clang-format+
  :ensure t
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

