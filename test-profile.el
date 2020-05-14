(add-to-list 'load-path "dash.el-master")

(add-to-list 'load-path "epl-master")
(add-to-list 'load-path "pkg-info-master")
(require 'pkg-info)

(add-to-list 'load-path "flycheck-master")

(setq make-backup-files nil
      visible-bell t
      org-startup-folded "showeverything"
      flycheck-grammalecte--debug-mode t)

(load-file "flycheck-grammalecte.el")
(flycheck-grammalecte-setup)

(add-hook 'org-mode-hook #'flycheck-mode)
