;;; flycheck-pyflakes.el --- Support grammalecte in flycheck

;; Copyright (C) 2017 Guilhem Doulcier <guilhem.doulcier@espci.fr>
;;
;; Author: Guilhem Doulcier <guilhem.doulcier@espci.fr>
;; Created: 21 February 2017
;; Version: 0.1
;; Package-Requires: ((flycheck "0.18"))

;;; Commentary:

;; This package adds support for grammalecte to flycheck.

;; It requires the python modules flycheck-grammalecte.py and
;; grammalecte to be in the same folder than this script.

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

; Get the location of the script or the current directory if the script is being evaluated in a buffer.
(defconst grammalecte-directory (if load-file-name (file-name-directory load-file-name) default-directory))


(flycheck-define-checker francais-grammalecte
  "Grammalecte syntax checker for french language `http://www.dicollecte.org/grammalecte/'."
  :command ("python3" (eval (expand-file-name "./flycheck-grammalecte.py" grammalecte-directory)))
  :standard-input t
  :error-patterns
  ((warning line-start "grammaire|" line "|" column "|" (message) line-end)
   (info line-start "orthographe|" line "|" column "|" (message) line-end))
  :modes (org-mode))

(add-to-list 'flycheck-checkers 'francais-grammalecte)

(provide 'flycheck-grammalecte)
;;; flycheck-grammalecte.el ends here
