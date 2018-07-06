;;; flycheck-grammalecte.el --- Support grammalecte in flycheck

;; Copyright (C) 2018 Étienne Deparis
;; Copyright (C) 2017 Guilhem Doulcier

;; Maintener: Étienne Deparis <etienne@depar.is>
;; Author: Guilhem Doulcier <guilhem.doulcier@espci.fr>
;;   Étienne Deparis <etienne@depar.is>
;; Created: 21 February 2017
;; Version: 0.1
;; Package-Requires: ((flycheck "0.18"))
;; Keywords: i18n, wp
;; Homepage: https://git.deparis.io/flycheck-grammalecte/

;;; Commentary:

;; This package adds support for Grammalecte to flycheck.

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'flycheck)

;;;; Configuration options:

(defgroup flycheck-grammalecte nil
  "Flycheck Grammalecte options"
  :group 'flycheck-options
  :group 'i18n)

(defcustom flycheck-grammalecte-directory
  (if load-file-name (file-name-directory load-file-name) default-directory)
  "Location of the flycheck-grammalecte package.

This variable must point to the directory where the emacs-lisp and
python files named `flycheck-grammalecte.el' and
`flycheck-grammalecte.el' are kept.
The default value is automatically computed from the included file."
  :type 'string
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-spellcheck t
  "Report spellcheck errors if non nil.

Default is t."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-grammar t
  "Report grammar errors if non nil.

Default is t."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-apos t
  "Report apostrophe errors if non nil.

Default is t."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-nbsp t
  "Report non-breakable spaces errors if non nil.

Default is t."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-enabled-modes
  '(org-mode text-mode mail-mode latex-mode)
  "Major modes for which `flycheck-grammalecte' should be enabled.

Default modes are `org-mode', `text-mode', `mail-mode' and
`latex-mode'."
  :type '(repeat (symbol :tag "Mode"))
  :group 'flycheck-grammalecte)

(defconst flycheck-grammalecte-grammalecte-version "0.6.4")


;;; Helper methods:

(defun flycheck-grammalecte-download-grammalecte ()
  "Download and extract grammalecte python program."
  (interactive)
  (let* ((fg-gm-dist-name
          (concat "Grammalecte-fr-v"
                  flycheck-grammalecte-grammalecte-version))
         (fg-gm-zip-name (concat fg-gm-dist-name ".zip"))
         (fg-gm-dl-url
          (concat
           "http://www.dicollecte.org/grammalecte/zip/"
           fg-gm-zip-name))
         (fg-gm-local-file
          (expand-file-name
           fg-gm-dist-name flycheck-grammalecte-directory))
         (fg-gm-local-zip-file
          (expand-file-name
           (concat fg-gm-dist-name ".zip")
           flycheck-grammalecte-directory))
         (fg-gm-source-folder
          (expand-file-name "grammalecte" fg-gm-local-file))
         (fg-gm-target-folder
          (expand-file-name
           "grammalecte" flycheck-grammalecte-directory)))
    (unless (file-exists-p fg-gm-local-zip-file)
      (url-copy-file fg-gm-dl-url fg-gm-local-zip-file))
    (call-process "unzip" nil nil nil
                  fg-gm-local-zip-file (concat "-d" fg-gm-local-file))
    (when (file-exists-p fg-gm-source-folder)
      (rename-file fg-gm-source-folder fg-gm-target-folder)
      (delete-directory fg-gm-local-file t)
      (delete-file fg-gm-local-zip-file))
    (message "Grammalecte dowloaded and extracted in %s"
             fg-gm-target-folder)))

(defun flycheck-grammalecte-download-grammalecte-if-needed ()
  "Download grammalecte if not there."
  (unless (file-exists-p
           (expand-file-name "grammalecte/grammar_checker.py"
                             flycheck-grammalecte-directory))
    (if (yes-or-no-p
         "[flycheck-grammalecte] Grammalecte data not found. Download it NOW?")
        (flycheck-grammalecte-download-grammalecte)
      (display-warning "flycheck-grammalecte"
                       "Grammalecte will fail if used.
Please run the command `flycheck-grammalecte-download-grammalecte'
as soon as possible."))))
(add-hook 'after-init-hook
          'flycheck-grammalecte-download-grammalecte-if-needed)

;;;; Flycheck methods:

;; Maybe change it for the complete path to the python file?
(flycheck-def-executable-var 'français-grammalecte "python3")

;; We do not use the `flycheck-define-checker' helper because we use a
;; quoted variable to store modes list
(flycheck-define-command-checker 'francais-grammalecte
  "Grammalecte syntax checker for french language
`http://www.dicollecte.org/grammalecte/'."
  :command '("python3"
             (eval
              (expand-file-name
               "flycheck-grammalecte.py"
               flycheck-grammalecte-directory))
             (eval (unless flycheck-grammalecte-report-spellcheck "-S"))
             (eval (unless flycheck-grammalecte-report-grammar "-G"))
             (eval (unless flycheck-grammalecte-report-apos "-A"))
             (eval (unless flycheck-grammalecte-report-nbsp "-N"))
             source)
  :error-patterns
  '((warning line-start "grammaire|" line "|" column "|" (message) line-end)
    (info line-start "orthographe|" line "|" column "|" (message) line-end))
  :modes flycheck-grammalecte-enabled-modes)

(add-to-list 'flycheck-checkers 'francais-grammalecte)

(provide 'flycheck-grammalecte)
;;; flycheck-grammalecte.el ends here
