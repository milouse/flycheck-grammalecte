;;; flycheck-grammalecte.el --- Integrate Grammalecte with Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Étienne Deparis
;; Copyright (C) 2017 Guilhem Doulcier

;; Maintener: Étienne Deparis <etienne@depar.is>
;; Author: Guilhem Doulcier <guilhem.doulcier@espci.fr>
;;         Étienne Deparis <etienne@depar.is>
;; Created: 21 February 2017
;; Version: 0.7
;; Package-Requires: ((emacs "24.3") (flycheck "26"))
;; Keywords: i18n, text
;; Homepage: https://git.deparis.io/flycheck-grammalecte/

;;; Commentary:

;; Adds support for Grammalecte (a french grammar checker) to flycheck.

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

(defcustom flycheck-grammalecte-report-esp t
  "Report useless spaces and tabs errors if non nil.
Default is t."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-enabled-modes
  '(org-mode text-mode mail-mode latex-mode)
  "Major modes for which `flycheck-grammalecte' should be enabled.

Default modes are `org-mode', `text-mode', `mail-mode' and
`latex-mode'."
  :type '(repeat (function :tag "Mode"))
  :group 'flycheck-grammalecte)

(defvar flycheck-grammalecte-directory
  (if load-file-name (file-name-directory load-file-name) default-directory)
  "Location of the flycheck-grammalecte package.
This variable must point to the directory where the emacs-lisp and
python files named `flycheck-grammalecte.el' and
`flycheck-grammalecte.py' are kept.
The default value is automatically computed from the included file.")

(defconst flycheck-grammalecte-grammalecte-version "0.6.5")

;;;; Helper methods:

(defun flycheck-grammalecte--download-zip ()
  "Download Grammalecte CLI zip file."
  (let* ((fgm-zip-name
          (concat "Grammalecte-fr-v"
                  flycheck-grammalecte-grammalecte-version
                  ".zip"))
         (fgm-dl-url
          (concat "https://grammalecte.net/grammalecte/zip/"
                  fgm-zip-name))
         (fgm-zip-file (expand-file-name
                        fgm-zip-name
                        flycheck-grammalecte-directory)))
    ;; Do not download it twice if it's still there for some reason…
    (unless (file-exists-p fgm-zip-file)
      (url-copy-file fgm-dl-url fgm-zip-file))
    (message "Grammalecte downloaded to %s" fgm-zip-file)
    fgm-zip-file))

(defun flycheck-grammalecte--extract-zip (fgm-zip-file)
  "Extract FGM-ZIP-FILE."
  (let ((fgm-extracted-folder (file-name-sans-extension fgm-zip-file)))
    ;; Unzip file given in parameters in `fgm-extracted-folder'.
    (call-process "unzip" nil nil nil
                  fgm-zip-file (concat "-d" fgm-extracted-folder))
    ;; Remove the zip file
    (delete-file fgm-zip-file)
    (message "Grammalecte extracted to %s" fgm-extracted-folder)
    fgm-extracted-folder))

(defun flycheck-grammalecte--install-py-files (fgm-extracted-folder)
  "Install the interesting files from FGM-EXTRACTED-FOLDER.
Move the `grammalecte' subfolder, containing the necessary python files
from FGM-EXTRACTED-FOLDER to their destination, alongside the other
package files."
  (let ((fgm-source-folder
         (expand-file-name "grammalecte" fgm-extracted-folder))
        (fgm-target-folder
         (expand-file-name "grammalecte"
                           flycheck-grammalecte-directory)))
    ;; Always do a clean update. Begin by removing old folder if it's
    ;; present.
    (when (file-directory-p fgm-target-folder)
      (delete-directory fgm-target-folder t))
    ;; Extract the `grammalecte' subfolder from the extracted directory.
    (when (file-exists-p fgm-source-folder)
      (rename-file fgm-source-folder fgm-target-folder)
      ;; Do some cleanup
      (delete-directory fgm-extracted-folder t))
    (message "Grammalecte installed in %s" fgm-target-folder)
    fgm-target-folder))

(defun flycheck-grammalecte--download-grammalecte-if-needed ()
  "Install Grammalecte python package if it's not there."
  ;; This function only works for `flycheck-grammalecte-enabled-modes'.
  ;; No need to bother the user in other modes.
  (when (memq major-mode flycheck-grammalecte-enabled-modes)
    (unless (file-exists-p
             (expand-file-name "grammalecte/grammar_checker.py"
                               flycheck-grammalecte-directory))
      (if (yes-or-no-p
           "[flycheck-grammalecte] Grammalecte data not found. Download it NOW?")
          (flycheck-grammalecte-download-grammalecte)
        (display-warning "flycheck-grammalecte"
                         "Grammalecte will fail if used.
Please run the command `flycheck-grammalecte-download-grammalecte'
as soon as possible.")))))

(defun flycheck-grammalecte--fetch-crisco-words (word type)
  "Fetch TYPE words from the CRISCO dictionary for the given WORD.
TYPE may be `synonymes' or `antonymes'."
  (shell-command-to-string
   (concat "curl -s http://crisco.unicaen.fr/des/synonymes/" word
           " | sed -n '/<i class=[^>]*>[0-9]* " type
           "/{n;s|\\s*<a href=\"/des/synonymes/[^\"]*\">\\([^<]*\\)</a>,\\?|- \\1\\n|g;p;/<!--Fin liste des "
           type "-->/q}' | sed '$ d'")))

(defun flycheck-grammalecte-download-grammalecte ()
  "Download, extract and install Grammalecte python program."
  (interactive)
  (flycheck-grammalecte--install-py-files
   (flycheck-grammalecte--extract-zip
    (flycheck-grammalecte--download-zip))))

(add-hook 'flycheck-mode-hook
          #'flycheck-grammalecte--download-grammalecte-if-needed)

(defun flycheck-grammalecte-find-synomyms-at-point ()
  "Find synonyms for the word at point.
This function will call a subprocess to fetch data from the CRISCO¹
thesaurus through curl and sed.  The found words are then displayed in
a new buffer in another window.  This function will not work with
Windows OS.
¹ http://crisco.unicaen.fr/des/synonymes/"
  (interactive)
  (if (get-buffer "*Flycheck Grammalecte Synomyms*")
      (kill-buffer "*Flycheck Grammalecte Synomyms*"))
  (let ((word (thing-at-point 'word 'no-properties))
        (buffer (get-buffer-create "*Flycheck Grammalecte Synomyms*")))
    (with-current-buffer buffer
      (insert "* Synomymes\n\n")
      (insert (flycheck-grammalecte--fetch-crisco-words word "synonymes"))
      (insert "\n* Antonymes\n\n")
      (insert (flycheck-grammalecte--fetch-crisco-words word "antonymes"))
      (org-mode)
      (read-only-mode))
    (switch-to-buffer-other-window buffer)
    (goto-char (point-min))))


;;;; Checker definition:

(flycheck-def-executable-var 'français-grammalecte "python3")

;; We do not use the `flycheck-define-checker' helper because we use a
;; quoted variable to store modes list
(flycheck-define-command-checker 'francais-grammalecte
  "Grammalecte syntax checker for french language
`https://grammalecte.net/'."
  :command '("python3"
             (eval
              (expand-file-name
               "flycheck-grammalecte.py"
               flycheck-grammalecte-directory))
             (eval (unless flycheck-grammalecte-report-spellcheck "-S"))
             (eval (unless flycheck-grammalecte-report-grammar "-G"))
             (eval (unless flycheck-grammalecte-report-apos "-A"))
             (eval (unless flycheck-grammalecte-report-nbsp "-N"))
             (eval (unless flycheck-grammalecte-report-esp "-W"))
             source)
  :error-patterns
  '((warning line-start "grammaire|" line "|" column "|" (message) line-end)
    (info line-start "orthographe|" line "|" column "|" (message) line-end))
  :modes flycheck-grammalecte-enabled-modes)

(add-to-list 'flycheck-checkers 'francais-grammalecte)

(provide 'flycheck-grammalecte)
;;; flycheck-grammalecte.el ends here
