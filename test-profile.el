(setq make-backup-files nil
      auto-save-default nil
      visible-bell t
      warning-minimum-level :debug
      warning-minimum-log-level :debug
      org-startup-folded "showeverything"
      flycheck-grammalecte--debug-mode t)

(flycheck-grammalecte-setup)

(add-hook 'org-mode-hook #'flycheck-mode)
