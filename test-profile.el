(setq make-backup-files nil
      auto-save-default nil
      visible-bell t
      warning-minimum-level :debug
      warning-minimum-log-level :debug
      org-startup-folded "showeverything"
      grammalecte--debug-mode t)

(with-eval-after-load 'flycheck
  (flycheck-grammalecte-setup))

(require 'flycheck)
(add-hook 'org-mode-hook #'flycheck-mode)
