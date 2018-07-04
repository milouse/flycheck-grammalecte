;;; flycheck-grammalecte.el --- Support grammalecte in flycheck

;; Copyright (C) 2018 Étienne Deparis
;; Copyright (C) 2017 Guilhem Doulcier

;; Maintener: Étienne Deparis <etienne@depar.is>
;; Author: Guilhem Doulcier <guilhem.doulcier@espci.fr>
;;   Étienne Deparis <etienne@depar.is>
;; Created: 21 February 2017
;; Version: 0.1
;; Package-Requires: ((flycheck "0.18"))
;; Keywords: grammar check, flycheck, spellcheck, grammalecte
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
`flycheck-grammalecte.el' are kept. The default value is automatically
computed from the included file."
  :type 'string
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-spellcheck t
  "Report spellcheck errors if non `nil'.

Default is `t'."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-grammar t
  "Report grammar errors if non `nil'.

Default is `t'."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-apos t
  "Report apostrophe errors if non `nil'.

Default is `t'."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-nbsp t
  "Report non-breakable spaces errors if non `nil'.

Default is `t'."
  :type 'boolean
  :group 'flycheck-grammalecte)


;;;; Flycheck methods:

(flycheck-define-checker francais-grammalecte
  "Grammalecte syntax checker for french language `http://www.dicollecte.org/grammalecte/'."
  :command ("python3"
            (eval
             (expand-file-name
              "./flycheck-grammalecte.py"
              flycheck-grammalecte-directory))
            (eval (unless flycheck-grammalecte-report-spellcheck "-S"))
            (eval (unless flycheck-grammalecte-report-grammar "-G"))
            (eval (unless flycheck-grammalecte-report-apos "-A"))
            (eval (unless flycheck-grammalecte-report-nbsp "-N"))
            source)
  :error-patterns
  ((warning line-start "grammaire|" line "|" column "|" (message) line-end)
   (info line-start "orthographe|" line "|" column "|" (message) line-end))
  :modes (org-mode text-mode mail-mode))

(add-to-list 'flycheck-checkers 'francais-grammalecte)

(provide 'flycheck-grammalecte)
;;; flycheck-grammalecte.el ends here
