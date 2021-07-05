(add-to-list 'load-path "use-package-master")
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
