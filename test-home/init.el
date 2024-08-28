(setopt auto-save-default nil
        debug-on-error t
        inhibit-startup-screen t
        make-backup-files nil
        visible-bell t
        warning-minimum-level :debug
        warning-minimum-log-level :debug)

(dolist (path '("dash.el" "flycheck" "epl"))
  (add-to-list
   'load-path
   (expand-file-name (format "../%s-master" path) user-emacs-directory)))

(autoload 'flycheck-mode "flycheck")
