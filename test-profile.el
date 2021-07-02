(setq make-backup-files nil
      auto-save-default nil
      visible-bell t
      warning-minimum-level :debug
      warning-minimum-log-level :debug
      debug-on-error t
      org-startup-folded "showeverything"
      grammalecte-settings-file (expand-file-name
                                 "tmp-grammalecte-cache.el"
                                 (file-name-directory load-file-name))
      grammalecte--debug-mode t)

(with-eval-after-load 'flycheck
  (flycheck-grammalecte-setup))

(require 'flycheck)
(add-hook 'org-mode-hook #'flycheck-mode)
