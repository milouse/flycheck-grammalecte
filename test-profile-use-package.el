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

(require 'use-package)

(use-package flycheck-grammalecte
             :hook (org-mode . flycheck-mode)
             :init
             (setq flycheck-grammalecte-report-apos nil
                   flycheck-grammalecte-report-esp nil
                   flycheck-grammalecte-report-nbsp nil)
             :config
             (grammalecte-download-grammalecte)
             (flycheck-grammalecte-setup))
