;;; flycheck-grammalecte.el --- Integrate Grammalecte with Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Étienne Deparis
;; Copyright (C) 2017 Guilhem Doulcier

;; Maintener: Étienne Deparis <etienne@depar.is>
;; Author: Guilhem Doulcier <guilhem.doulcier@espci.fr>
;;         Étienne Deparis <etienne@depar.is>
;; Created: 21 February 2017
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (flycheck "26"))
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

(defcustom flycheck-grammalecte-report-spellcheck nil
  "Report spellcheck errors if non-nil.
Default is nil.  You should use `flyspell' instead."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-grammar t
  "Report grammar errors if non-nil.
Default is t."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-apos t
  "Report apostrophe errors if non-nil.
Default is t."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-nbsp t
  "Report non-breakable spaces errors if non-nil.
Default is t."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-esp t
  "Report useless spaces and tabs errors if non-nil.
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

(defcustom flycheck-grammalecte-download-without-asking nil
  "Download grammalecte upstream package without asking if non-nil.

Otherwise, it will ask for a yes-or-no confirmation."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-filters '("\\\\(\\w+)(?:\\[([^]]+)\\])?(?:{([^}]*)})?")
  "List patterns for which errors in matching texts must be ignored.

These patterns must be python Regular Expressions¹.
Escape character `\\' must be doubled twice: one time for Emacs
and one time for python.  For example, to exclude LaTeX math
formulas, one can use :

    (setq flycheck-grammalecte-filters
          '(\"\\$.*?\\$\"
            \"\\\\begin{equation}.*?\\\\end{equation}\"))

Filters are applied sequentially.  In practice all characters of
the matching pattern are replaced by `&', which are ignored by
grammalecte.

¹ See URL `https://docs.python.org/3.5/library/re.html#regular-expression-syntax'."
  :type '(repeat string)
  :group 'flycheck-grammalecte)

(defvar flycheck-grammalecte--directory
  (if load-file-name (file-name-directory load-file-name) default-directory)
  "Location of the flycheck-grammalecte package.

This variable must point to the directory where the emacs-lisp and
python files named `flycheck-grammalecte.el' and
`flycheck-grammalecte.py' are kept.

The default value is automatically computed from the included file.")

(defvar flycheck-grammalecte--debug-mode nil
  "Display some debug messages when non-nil.")



;;;; Helper methods:

(defun flycheck-grammalecte--grammalecte-version ()
  "Return the upstream version of the flycheck-grammalecte package.
Signal a `file-error' error if something wrong happen while retrieving
the Grammalecte home page or if no version string is found in the page."
  (let* ((url "https://grammalecte.net/index.html")
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (if (re-search-forward
           "^ +<p id=\"version_num\">\\([0-9.]+\\)</p>$"
           nil t) ;; Parse all downloaded data and avoid error
          (match-string 1)
        (signal 'file-error
                (list url "No version number found on grammalecte website"))))))

(defun flycheck-grammalecte--download-zip ()
  "Download Grammalecte CLI zip file."
  (let* ((fgm-zip-name
          (format "Grammalecte-fr-v%s.zip"
                  (flycheck-grammalecte--grammalecte-version)))
         (fgm-dl-url
          (format "https://grammalecte.net/grammalecte/zip/%s"
                  fgm-zip-name))
         (fgm-zip-file (expand-file-name
                        fgm-zip-name
                        flycheck-grammalecte--directory)))
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
                           flycheck-grammalecte--directory)))
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

(defun flycheck-grammalecte--download-grammalecte-if-needed (&optional force)
  "Install Grammalecte python package if it's required.
This method checks if the python package is already installed and
if the current buffer major mode is present in the
`flycheck-grammalecte-enabled-modes' list.
If optional argument FORCE is non-nil, verification will occurs even
when current buffer major mode is not in `flycheck-grammalecte-enabled-modes'."
  (when (or force (memq major-mode flycheck-grammalecte-enabled-modes))
    (unless (file-exists-p
             (expand-file-name "grammalecte/grammar_checker.py"
                               flycheck-grammalecte--directory))
      (if (or flycheck-grammalecte-download-without-asking
              (yes-or-no-p
               "[flycheck-grammalecte] Grammalecte data not found.  Download it NOW?"))
          (flycheck-grammalecte-download-grammalecte)
        (display-warning "flycheck-grammalecte"
                         "Grammalecte will fail if used.
Please run the command `flycheck-grammalecte-download-grammalecte'
as soon as possible.")))))

(defun flycheck-grammalecte--kill-ring-save-at-point (&optional pos replace)
  "Copy the word at point or POS and paste it when REPLACE is non-nil.

The word is taken from the synonyms result buffer at point or POS when
POS is non-nil.

When REPLACE is non-nil, it will replace the word at point in the
other buffer by the copied word."
  (unless pos (setq pos (point)))
  (goto-char pos)
  (when (string= "-" (string (char-after (line-beginning-position))))
    (let ((beg (+ 2 (line-beginning-position))) ;; ignore the leading -
          (end (line-end-position)))
      (kill-ring-save beg end)
      (if (not replace)
          (message
           "%s sauvé dans le kill-ring.  Utilisez `C-y' n'importe où pour l'utiliser."
           (buffer-substring-no-properties beg end))
        (quit-window t)
        (flycheck-grammalecte--delete-word-at-point)
        (yank)))))

(defun flycheck-grammalecte--delete-word-at-point ()
  "Delete the word around point, or region if one is active."
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'word))))
    (when bounds
      (delete-region (car bounds) (cdr bounds)))))



;;;; Synonyms and antonyms helper methods:

(defun flycheck-grammalecte--extract-crisco-words (type)
  "Extract all words for TYPE from the current buffer."
  (save-excursion
    (save-restriction
      (let ((results '()) content start end)
        (goto-char (point-min))
        (if (re-search-forward
             (format "<i class=[^>]*>[[:digit:]]* %s?" type)
             nil t)
            (setq start (match-beginning 0))
          (when flycheck-grammalecte--debug-mode
            (display-warning
             "flycheck-grammalecte"
             (format "Début de liste des %s non trouvée." type))))
        (if (re-search-forward
             (format "<!-- ?Fin liste des %s ?-->" type)
             nil t)
            (setq end (match-beginning 0))
          (when flycheck-grammalecte--debug-mode
            (display-warning
             "flycheck-grammalecte"
             (format "Fin de liste des %s non trouvée." type))))
        (when (and start end)
          (narrow-to-region start end)
          (setq content (decode-coding-string (buffer-string) 'utf-8-unix))
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            (while (re-search-forward "[[:blank:]]*<a href=\"/des/synonymes/[^\"]*\">\\([^<]*\\)</a>,?" nil t)
              (push (match-string 1) results))))
        results))))

(defun flycheck-grammalecte--fetch-crisco-words (word)
  "Fetch synonymes and antonymes for the given WORD from the CRISCO."
  (let ((url (format "https://crisco2.unicaen.fr/des/synonymes/%s" word))
        found-words)
    (with-current-buffer (url-retrieve-synchronously url t t)
      (let ((synonymes (flycheck-grammalecte--extract-crisco-words "synonymes"))
            (antonymes (flycheck-grammalecte--extract-crisco-words "antonymes")))
        (setq found-words (list :synonymes synonymes
                                :antonymes antonymes))
        (if (and flycheck-grammalecte--debug-mode
                 (seq-empty-p synonymes) (seq-empty-p antonymes))
            (pop-to-buffer (current-buffer))
          (kill-buffer (current-buffer)))))
    found-words))

(defun flycheck-grammalecte--propertize-crisco-words (words)
  "Insert WORDS at point, after having propertized them."
  (if (seq-empty-p words)
      (insert "Aucun résultat")
    (insert
     (mapconcat
      #'(lambda (w)
          (concat "- "
                  (propertize w 'mouse-face 'highlight
                                'help-echo "mouse-1: Remplacer par…")))
      words "\n"))))



;;;; Special buffer major mode methods

(defun flycheck-grammalecte--set-buffer-title (title)
  "Decorate the current buffer `header-line-format', prefixed by TITLE.
It adds information on how to close it."
  (setq-local
   header-line-format
   (format-message
    "%s. Quitter `q' ou `k', Copier avec `w'. Remplacer avec `mouse-1' ou `RET'."
    title)))

(defvar flycheck-grammalecte-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k"
      #'(lambda () (interactive) (quit-window t)))
    (define-key map "o" #'other-window)
    (define-key map "q" #'quit-window)
    (define-key map "w"
      #'(lambda () (interactive)
          (flycheck-grammalecte--kill-ring-save-at-point)))
    (define-key map (kbd "<mouse-1>")
      #'(lambda (event)
          (interactive "e")
          (flycheck-grammalecte--kill-ring-save-at-point
           (posn-point (event-end event)) t)))
    (define-key map (kbd "<RET>")
      #'(lambda () (interactive)
          (flycheck-grammalecte--kill-ring-save-at-point (point) t)))
    map)
  "Keymap for `flycheck-grammalecte-mode'.")

(define-derived-mode flycheck-grammalecte-mode special-mode
  "Flycheck Grammalecte mode"
  "Major mode used to display results of a synonym research or
conjugation table."
  (buffer-disable-undo)
  (setq buffer-read-only t
        show-trailing-whitespace nil)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))
  (goto-char (point-min)))



;;;; Synonyms and antonyms public methods:

;;;###autoload
(defun flycheck-grammalecte-find-synonyms (word)
  "Find french synonyms and antonyms for the given WORD.
This function will fetch data from the CRISCO¹ thesaurus.

The found words are then displayed in a new buffer in another window.

¹ See URL `https://crisco2.unicaen.fr/des/synonymes/'"
  (interactive "sMot: ")
  (let* ((buffer-name (format "*Synonymes de %s*" word))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create buffer-name))
      (let ((found-words (flycheck-grammalecte--fetch-crisco-words word)))
        (with-current-buffer buffer
          (insert (propertize (format "* Synonymes de %s" word)
                              'face 'org-level-1) "\n\n")
          (flycheck-grammalecte--propertize-crisco-words
           (plist-get found-words :synonymes))
          (insert "\n\n" (propertize (format "* Antonymes de %s" word)
                                     'face 'org-level-1) "\n\n")
          (flycheck-grammalecte--propertize-crisco-words
           (plist-get found-words :antonymes))
          (insert "\n") ;; Avoid ugly last button
          (flycheck-grammalecte-mode)
          (flycheck-grammalecte--set-buffer-title
           "Sélection de synonymes ou d'antonymes."))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun flycheck-grammalecte-find-synonyms-at-point ()
  "Find french synonyms and antonyms for the word at point."
  (interactive)
  (let ((word (thing-at-point 'word 'no-properties)))
    (if word
        (flycheck-grammalecte-find-synonyms word)
      (call-interactively 'flycheck-grammalecte-find-synonyms))))



;;;; Public methods:

;;;###autoload
(defun flycheck-grammalecte-conjugate-verb (verb)
  "Display the conjugation table for the given VERB."
  (interactive "sVerbe: ")
  (flycheck-grammalecte--download-grammalecte-if-needed t)
  (if (get-buffer "*Conjugaison*")
      (kill-buffer "*Conjugaison*"))
  (let ((buffer (get-buffer-create "*Conjugaison*")))
    (with-current-buffer buffer
      (insert
       (shell-command-to-string
        (format "python3 %s %s"
                (expand-file-name "conjugueur.py" flycheck-grammalecte--directory)
                verb)))
      (goto-char (point-min))
      (while (re-search-forward "^\\* [^\n]+$" nil t)
        (replace-match (propertize (match-string 0) 'face 'org-level-1)))
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\* [^\n]+$" nil t)
        (replace-match (propertize (match-string 0) 'face 'org-level-2)))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\(?:avoir\\|être\\)\\*" nil t)
        (replace-match (propertize (match-string 0) 'face 'bold)))
      (goto-char (point-min))
      (while (re-search-forward "^\\- \\([^ \n]+\\)$" nil t)
        (replace-match
         (propertize (match-string 1) 'mouse-face 'highlight
                     'help-echo "mouse-1: Copier le mot")
         t t nil 1))
      (flycheck-grammalecte-mode)
      (flycheck-grammalecte--set-buffer-title
       (format "Conjugaison de %s." verb)))
    (pop-to-buffer buffer)))


(defun flycheck-grammalecte-download-grammalecte ()
  "Download, extract and install Grammalecte python program."
  (interactive)
  (flycheck-grammalecte--install-py-files
   (flycheck-grammalecte--extract-zip
    (flycheck-grammalecte--download-zip))))

(add-hook 'flycheck-mode-hook
          #'flycheck-grammalecte--download-grammalecte-if-needed)



;;;; Checker definition:

;;;###autoload
(defun flycheck-grammalecte-setup ()
  "Build the flycheck checker, matching your taste."
  (flycheck-def-executable-var 'grammalecte "python3")
  (let ((cmdline '(source))
        (filters (mapcan #'(lambda (filter) (list "-f" filter))
                         flycheck-grammalecte-filters))
        (grammalecte-bin (expand-file-name
                          "flycheck-grammalecte.py"
                          flycheck-grammalecte--directory)))
    (unless flycheck-grammalecte-report-spellcheck (push "-S" cmdline))
    (unless flycheck-grammalecte-report-grammar (push "-G" cmdline))
    (unless flycheck-grammalecte-report-apos (push "-A" cmdline))
    (unless flycheck-grammalecte-report-nbsp (push "-N" cmdline))
    (unless flycheck-grammalecte-report-esp (push "-W" cmdline))
    (setq cmdline (nconc (list "python3" grammalecte-bin) filters cmdline))

    (when flycheck-grammalecte--debug-mode
      (let ((checker-path (expand-file-name
                           "grammalecte/grammar_checker.py"
                           flycheck-grammalecte--directory)))
        (if (file-exists-p checker-path)
            (message "[Flycheck Grammalecte] Found in %s" checker-path)
          (message "[Flycheck Grammalecte] NOT FOUND")))
      (message "[Flycheck Grammalecte] checker command: %s"
               (mapconcat
                #'(lambda (item)
                    (cond ((symbolp item)
                           (format "'%s" (symbol-name item)))
                          ((stringp item)
                           item)))
                cmdline
                " ")))

    ;; Now that we have all our variables, we can create the custom
    ;; checker.
    (flycheck-define-command-checker 'grammalecte
      "Grammalecte syntax checker for french language
See URL `https://grammalecte.net/'."
      :command cmdline
      :error-patterns
      '((warning line-start "grammaire|" line "|" column "|" (message) line-end)
        (info line-start "orthographe|" line "|" column "|" (message) line-end))
      :modes flycheck-grammalecte-enabled-modes)
    (add-to-list 'flycheck-checkers 'grammalecte)))

(provide 'flycheck-grammalecte)
;;; flycheck-grammalecte.el ends here
