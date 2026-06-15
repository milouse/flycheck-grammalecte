(setopt auto-save-default nil
        debug-on-error t
        inhibit-startup-screen t
        make-backup-files nil
        visible-bell t
        warning-minimum-level :debug
        warning-minimum-log-level :debug)

(dolist (path '("dash" "flycheck"))
  (add-to-list
   'load-path
   (expand-file-name (format "../vendor/%s" path) user-emacs-directory)))

(autoload 'flycheck-mode "flycheck")
