(with-eval-after-load 'flycheck
  (flycheck-grammalecte-setup))

(require 'flycheck)
(add-hook 'org-mode-hook #'flycheck-mode)
