(message "flycheck-grammalecte demo: Using USE-PACKAGE test context.")

(use-package org :defer t
  :custom (org-startup-folded 'showeverything))

(use-package flycheck :defer t
  :hook (org-mode latex-mode))

(defconst fg/install-dir (expand-file-name ".." user-emacs-directory)
  "Path to the current flycheck-grammalect code.")

(use-package flycheck-grammalecte :defer t :after flycheck
  :load-path fg/install-dir
  :hook ((org-mode latex-mode) . flycheck-grammalecte-setup)
  :custom
  (flycheck-grammalecte-report-apos nil)
  (flycheck-grammalecte-report-esp nil)
  (flycheck-grammalecte-report-nbsp nil)
  :config
  (setq grammalecte--debug-mode t))
