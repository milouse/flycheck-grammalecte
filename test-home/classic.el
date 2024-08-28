(message "flycheck-grammalecte demo: Using CLASSIC test context.")

(setopt org-startup-folded 'showeverything)

(add-hook 'org-mode-hook #'flycheck-mode)
(add-hook 'latex-mode-hook #'flycheck-mode)

(load (expand-file-name "../grammalecte-loaddefs" user-emacs-directory))
(setq grammalecte--debug-mode t)
(flycheck-grammalecte-setup)
