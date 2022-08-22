(setq make-backup-files nil
      auto-save-default nil
      inhibit-startup-screen t
      visible-bell t
      warning-minimum-level :debug
      warning-minimum-log-level :debug
      debug-on-error t
      org-startup-folded "showeverything"
      grammalecte--debug-mode t)

(dolist (path '("dash.el" "flycheck" "epl" "pkg-info"))
  (add-to-list 'load-path (format "%s-master" path)))

(load (expand-file-name "grammalecte-loaddefs"))
