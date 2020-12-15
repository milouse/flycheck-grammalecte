;;; flycheck-grammalecte.el --- Integrate Grammalecte with Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Étienne Deparis
;; Copyright (C) 2017 Guilhem Doulcier

;; Maintener: Étienne Deparis <etienne@depar.is>
;; Author: Guilhem Doulcier <guilhem.doulcier@espci.fr>
;;         Étienne Deparis <etienne@depar.is>
;; Created: 21 February 2017
;; Version: 1.3
;; Package-Requires: ((emacs "26.1") (flycheck "26"))
;; Keywords: i18n, text
;; Homepage: https://git.umaneti.net/flycheck-grammalecte/

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
(require 'nxml-mode)

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
  '(latex-mode
    mail-mode
    markdown-mode
    message-mode
    mu4e-compose-mode
    org-mode
    text-mode)
  "Major modes for which `flycheck-grammalecte' should be enabled.

Sadly, flycheck does not use `derived-mode-p' to check if it must
be enabled or not in the current buffer.  Thus, be sure to set up
a comprehensive mode list for your own usage.

Default modes are `latex-mode', `mail-mode', `markdown-mode',
`message-mode', `mu4e-compose-mode', `org-mode' and `text-mode'."
  :type '(repeat (function :tag "Mode"))
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-download-without-asking nil
  "Download Grammalecte upstream package without asking if non-nil.

Otherwise, it will ask for a yes-or-no confirmation."
  :type 'boolean
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-filters
  '("(?m)^# ?-*-.+$")
  "Patterns for which errors in matching texts are ignored.

As these patterns will be used by the underlying python script,
they must be python Regular Expressions (See URL
`https://docs.python.org/3.5/library/re.html#regular-expression-syntax').

Escape character `\\' must be doubled twice: one time for Emacs
and one time for python.  For example, to exclude LaTeX math
formulas, one can use :

    (setq flycheck-grammalecte-filters
          '(\"\\$.*?\\$\"
            \"(?s)\\\\begin{equation}.*?\\\\end{equation}\"))

For simple use case, you can try to use the function
`flycheck-grammalecte--convert-elisp-rx-to-python'.

Filters are applied sequentially.  In practice all characters of
the matching pattern are replaced by `█', which are ignored by
grammalecte.

This patterns are always sent to Grammalecte.  See the variable
`flycheck-grammalecte-filters-by-mode' for mode-related patterns."
  :type '(repeat string)
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-filters-by-mode
  '((latex-mode "\\\\(?:title|(?:sub)*section){([^}]+)}"
                "\\\\\\w+(?:\\[[^]]+\\])?(?:{[^}]*})?")
    (org-mode "(?ims)^[ \t]*#\\+begin_src.+#\\+end_src"
              "(?im)^[ \t]*#\\+begin[_:].+$"
              "(?im)^[ \t]*#\\+end[_:].+$"
              "(?m)^[ \t]*(?:DEADLINE|SCHEDULED):.+$"
              "(?m)^\\*+ .*[ \t]*(:[\\w:@]+:)[ \t]*$"
              "(?im)^[ \t]*#\\+(?:caption|description|keywords|(?:sub)?title):"
              "(?im)^[ \t]*#\\+(?!caption|description|keywords|(?:sub)?title)\\w+:.*$"))
  "Filtering patterns by mode.

Each element has the form (MODE PATTERNS...), where MODE must be
a valid major mode and PATTERNS must be a list of regexp as
described in the variable `flycheck-grammalecte-filters'.

Contrary to flycheck, we will use `derived-mode-p' to check if a
filters list must be activated or not.  Thus you are not obliged
to list all possible modes, as soon as one is an ancestor of
another.

Patterns defined here will be added after the ones defined in
`flycheck-grammalecte-filters' when their associated mode matches
the current buffer major mode, or is an ancestor of it.  This
operation is only done once when the function
`flycheck-grammalecte-setup' is run."
  :type '(alist :key-type (function :tag "Mode")
                :value-type (repeat string))
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-borders-by-mode
  '((latex-mode . "^\\\\begin{document}$")
    (mail-mode . "^--text follows this line--")
    (message-mode . "^--text follows this line--"))
  "Line patterns before which proofing must not occur for given mode.

Each element is a cons-cell (MODE . PATTERN), where MODE must be
a valid major mode and PATTERN must be a regexp as described in
the variable `flycheck-grammalecte-filters'.

For the given MODE, the corresponding PATTERN should match a line in the
file being proofed.  All lines before this match will be ignored by the
process.  This variable is used for example to avoid proofing mail
headers or LaTeX documents header.

Contrary to flycheck, we will use `derived-mode-p' to check if a
border must be activated or not.  Thus you are not obliged to
list all possible modes, as soon as one is an ancestor of
another.  This activation is only done once when the function
`flycheck-grammalecte-setup' is run."
  :type '(repeat string)
  :group 'flycheck-grammalecte)

(defvar flycheck-grammalecte--directory
  (if load-file-name (file-name-directory load-file-name) default-directory)
  "Location of the flycheck-grammalecte package.

This variable must point to the directory where the emacs-lisp and
python files named `flycheck-grammalecte.el' and
`flycheck-grammalecte.py' are kept.  It must end with a / (see
`file-name-as-directory').

The default value is automatically computed from the included file.")

(defvar flycheck-grammalecte--grammalecte-directory
  (expand-file-name "grammalecte" flycheck-grammalecte--directory)
  "Location of the Grammalecte python package.

This variable may be changed if you already have Grammalecte installed
somewhere on your machine.

This variable value must not end with a / (see `directory-file-name').

The default value is a folder alongside this elisp package.")

(defvar flycheck-grammalecte--debug-mode nil
  "Display some debug messages when non-nil.")



;;;; Helper methods:

(defun flycheck-grammalecte--convert-elisp-rx-to-python (regexp)
  "Convert the given elisp REGEXP to a python 3 regular expression.

For example, given the following REGEXP

   \\\\(?:title\\|\\(?:sub\\)*section\\){\\([^}]+\\)}\\)

This function will return

    \\\\(?:title|(?:sub)*section){([^}]+)})

See URL
`https://docs.python.org/3.5/library/re.html#regular-expression-syntax'
and Info node `(elisp)Syntax of Regular Expressions'."
  (let ((convtable '(("\\\\(" . "(")
                     ("\\\\)" . ")")
                     ("\\\\|" . "|")
                     ("\\\\" . "\\\\")
                     ("\\[:alnum:\\]" . "\\w")
                     ("\\[:space:\\]" . "\\s")
                     ("\\[:blank:\\]" . "\\s")
                     ("\\[:digit:\\]" . "\\d")
                     ("\\[:word:\\]" . "\\w"))))
    (when (and (string-match "\\[:\\([a-z]+\\):\\]" regexp)
               (not (string= "digit" (match-string 1 regexp)))
               (not (string= "word" (match-string 1 regexp))))
      (signal 'invalid-regexp
              (list (format
                     "%s is not supported by python regular expressions"
                     (match-string 0 regexp)))))
    (dolist (convpattern convtable)
      (setq regexp
            (replace-regexp-in-string (car convpattern)
                                      (cdr convpattern)
                                      regexp t t)))
    regexp))

(defun flycheck-grammalecte--augment-pythonpath-if-needed ()
  "Augment PYTHONPATH with the install directory of grammalecte.
If the parent directory of `flycheck-grammalecte--grammalecte-directory'
is not this elisp package installation directory, then add the former in
the PYTHONPATH environment variable in order to make python scripts work
as expected."
  (let ((grammalecte-parent-path
         (file-name-directory
          (directory-file-name flycheck-grammalecte--grammalecte-directory)))
        (current-pythonpath (or (getenv "PYTHONPATH") "")))
    (unless (or (string-match-p grammalecte-parent-path current-pythonpath)
                (string= grammalecte-parent-path flycheck-grammalecte--directory))
      (setenv "PYTHONPATH"
              (if (string= current-pythonpath "")
                  grammalecte-parent-path
                (format "%s:%s" grammalecte-parent-path current-pythonpath))))))

(defun flycheck-grammalecte--grammalecte-version ()
  "Return the currently installed Grammalecte version."
  (flycheck-grammalecte--augment-pythonpath-if-needed)
  (let* ((python-script "from grammalecte.fr.gc_engine import __version__
print(__version__)")
         (fg-version
          (shell-command-to-string
           (format "python3 -c \"%s\"" python-script))))
    ;; Only return a version number if we got something which looks like a
    ;; version number (else it may be a python crash when Grammalecte is not
    ;; yet downloaded)
    (when (string-match "^[0-9.]+$" fg-version)
      (match-string 0 fg-version))))

(defun flycheck-grammalecte--grammalecte-upstream-version ()
  "Return the upstream version of Grammalecte.
Signal a `file-error' error if something wrong happen while retrieving
the Grammalecte home page or if no version string is found in the page."
  (let* ((url "https://grammalecte.net/index.html")
         (inhibit-message t) ;; Do not display url-retrieve messages
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (if (re-search-forward
           "<p id=\"version_num\">\\([0-9.]+\\)</p>"
           nil t) ;; Parse all downloaded data and avoid error
          (match-string 1)
        (signal 'file-error
                (list url "No version number found on grammalecte website"))))))

(defun flycheck-grammalecte--download-zip (&optional grammalecte-version)
  "Download Grammalecte CLI zip file.
If given, this function will try to download the GRAMMALECTE-VERSION
of the python package."
  (let* ((up-version (if (or (not grammalecte-version)
                             (string= grammalecte-version "last"))
                         (flycheck-grammalecte--grammalecte-upstream-version)
                       grammalecte-version))
         (zip-name (format "Grammalecte-fr-v%s.zip" up-version))
         (dl-url
          (format "https://grammalecte.net/grammalecte/zip/%s"
                  zip-name))
         (zip-file (expand-file-name
                    zip-name
                    flycheck-grammalecte--directory)))
    ;; Do not download it twice if it's still there for some reason...
    (unless (file-exists-p zip-file)
      (url-copy-file dl-url zip-file)
      (message "[Flycheck Grammalecte] Downloaded to %s" zip-file))
    zip-file))

(defun flycheck-grammalecte--extract-zip (zip-file)
  "Extract ZIP-FILE."
  (let ((extracted-folder (file-name-sans-extension zip-file)))
    ;; Unzip file given in parameters in `extracted-folder'.
    (call-process "unzip" nil nil nil
                  zip-file (concat "-d" extracted-folder))
    ;; Remove the zip file
    (delete-file zip-file)
    (message "[Flycheck Grammalecte] Extracted to %s" extracted-folder)
    extracted-folder))

(defun flycheck-grammalecte--install-py-files (extracted-folder)
  "Install the interesting files from EXTRACTED-FOLDER.
Move the `grammalecte' subfolder, containing the necessary python files
from EXTRACTED-FOLDER to their destination, alongside the other
package files."
  (let ((source-folder
         (expand-file-name "grammalecte" extracted-folder))
        (target-folder flycheck-grammalecte--grammalecte-directory))
    ;; Always do a clean update. Begin by removing old folder if it's
    ;; present.
    (when (file-directory-p target-folder)
      (delete-directory target-folder t))
    ;; Extract the `grammalecte' subfolder from the extracted directory.
    (when (file-exists-p source-folder)
      (rename-file source-folder target-folder)
      ;; Do some cleanup
      (delete-directory extracted-folder t))
    (message "[Flycheck Grammalecte] Installed in %s" target-folder)
    target-folder))

(defun flycheck-grammalecte--download-grammalecte-if-needed (&optional force)
  "Install Grammalecte python package if it's required.
This method checks if the python package is already installed and
if the current buffer major mode is present in the
`flycheck-grammalecte-enabled-modes' list.
If optional argument FORCE is non-nil, verification will occurs even
when current buffer major mode is not in `flycheck-grammalecte-enabled-modes'."
  (when (or force (memq major-mode flycheck-grammalecte-enabled-modes))
    (let ((local-version (flycheck-grammalecte--grammalecte-version))
          (upstream-version (flycheck-grammalecte--grammalecte-upstream-version)))
      (if (not (stringp local-version))
          ;; It seems there is no currently downloaded Grammalecte
          ;; package. Force install it, as nothing will work without it.
          (flycheck-grammalecte-download-grammalecte upstream-version)
        (when (and (string-version-lessp local-version upstream-version)
                   (or flycheck-grammalecte-download-without-asking
                       (yes-or-no-p
                        "[flycheck-grammalecte] Grammalecte is out of date.  Download it NOW?")))
          (flycheck-grammalecte-download-grammalecte upstream-version))))))

(defun flycheck-grammalecte--split-error-message (err)
  "Split ERR message between actual message and suggestions."
  (when err
    (let* ((err-msg (split-string (flycheck-error-message err) "⇨" t " "))
           (suggestions (split-string (or (cadr err-msg) "") "," t " ")))
      (cons (car err-msg) suggestions))))

(defun flycheck-grammalecte--fix-error (err repl &optional region)
  "Replace the wrong REGION of ERR by REPL."
  (when repl
    (unless region
      (setq region (flycheck-error-region-for-mode err major-mode)))
    (when region
      (delete-region (car region) (cdr region)))
    (insert repl)))

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

(defun flycheck-grammalecte--propertize-conjugation-buffer ()
  "Propertize some important words in the conjugation buffer."
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
                 'help-echo "mouse-1: Remplacer par…")
     t t nil 1)))



;;;; Definition helper methods:

(defun flycheck-grammalecte--extract-cnrtl-definition (start)
  "Extract a definition from the current XML buffer at START."
  (goto-char start)
  (delete-region (point-min) (point))
  (let ((inhibit-message t)) ;; Silences nxml-mode messages
    (nxml-mode)
    (nxml-forward-element)
    (delete-region (point) (point-max))
    (libxml-parse-html-region (point-min) (point-max))))

(defun flycheck-grammalecte--fetch-cnrtl-word (word)
  "Fetch WORD definition, according to TLFi, on CNRTL."
  (let ((url (format "https://www.cnrtl.fr/definition/%s" word))
        (definitions '()) count start)
    ;; Get initial definitions location, number of definitions and
    ;; initial definition.
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (if (search-forward "<div id=\"lexicontent\">" nil t)
          (setq start (match-beginning 0))
        (when flycheck-grammalecte--debug-mode
          (display-warning 'flycheck-grammalecte "Définition non trouvée.")))
      (if (re-search-backward "'/definition/[^/]+//\\([0-9]+\\)'" nil t)
          (setq count (string-to-number (match-string 1)))
        (when flycheck-grammalecte--debug-mode
          (display-warning 'flycheck-grammalecte "Nombre de définitions non trouvé.")))
      (when (and start count)
        (push (flycheck-grammalecte--extract-cnrtl-definition start) definitions)
        (kill-buffer (current-buffer))))
    ;; Collect additional definitions.
    (when (and start count)
      (dotimes (i count)
        (with-current-buffer
            (url-retrieve-synchronously (format "%s/%d" url (1+ i)) t t)
          (push (flycheck-grammalecte--extract-cnrtl-definition start) definitions)
          (kill-buffer (current-buffer)))))
    (reverse definitions)))



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
             'flycheck-grammalecte
             (format "Début de liste des %s non trouvée." type))))
        (if (re-search-forward
             (format "<!-- ?Fin liste des %s ?-->" type)
             nil t)
            (setq end (match-beginning 0))
          (when flycheck-grammalecte--debug-mode
            (display-warning
             'flycheck-grammalecte
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



;;;; Definition public methods:

;;;###autoload
(defun flycheck-grammalecte-define (word)
  "Find definition for the french WORD.
This function will fetch data from the CNRTL¹ website, in the TLFi².

The found words are then displayed in a new buffer in another window.

See URL `https://www.cnrtl.fr/definition/'.

¹ « Centre National de Ressources Textuelles et Lexicales »
² « Trésor de la Langue Française informatisé »."
  (interactive "sMot: ")
  (let* ((buffer-name (format "*Définition de %s*" word))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create buffer-name))
      ;; Display all definitions in a dedicated buffer.
      (with-current-buffer buffer
        (let ((definitions (flycheck-grammalecte--fetch-cnrtl-word word)))
          (if (seq-empty-p definitions)
              (insert (format "Aucun résultat pour %s." word))
            (dolist (d definitions)
              (shr-insert-document d)
              (insert "\n\n\n"))))
        (special-mode)
        (visual-line-mode)
        (local-set-key "k" #'(lambda () (interactive) (quit-window t)))
        (local-set-key "o" #'other-window)
        (setq-local header-line-format
                    (format-message "Définition de %s. Quitter `q' ou `k'."
                                    word))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))


;;;###autoload
(defun flycheck-grammalecte-define-at-point ()
  "Find definitions for the french word at point."
  (interactive)
  (let ((word (thing-at-point 'word 'no-properties)))
    (if word
        (flycheck-grammalecte-define word)
      (call-interactively 'flycheck-grammalecte-define))))



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
  (let* ((buffer-name (format "*Conjugaison de %s*" verb))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (flycheck-grammalecte--download-grammalecte-if-needed t)
      (flycheck-grammalecte--augment-pythonpath-if-needed)
      (setq buffer (get-buffer-create buffer-name))
      (with-current-buffer buffer
        (insert
         (shell-command-to-string
          (format "python3 %s %s"
                  (expand-file-name "conjugueur.py" flycheck-grammalecte--directory)
                  verb)))
        (flycheck-grammalecte--propertize-conjugation-buffer)
        (flycheck-grammalecte-mode)
        (flycheck-grammalecte--set-buffer-title
         (format "Conjugaison de %s." verb))))
    (pop-to-buffer buffer)))

(defun flycheck-grammalecte-correct-error-at-point (pos)
  "Correct the first error encountered at POS.

This method replace the word at POS by the first suggestion coming from
flycheck, if any."
  (interactive "d")
  (let ((first-err (car-safe (flycheck-overlay-errors-at pos))))
    (when first-err
      (flycheck-grammalecte--fix-error
       first-err
       (cadr (flycheck-grammalecte--split-error-message first-err))))))

(defun flycheck-grammalecte-correct-error-at-click (event)
  "Popup a menu to help correct error under mouse pos defined in EVENT."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (let ((first-err (car-safe (flycheck-overlay-errors-at (point)))))
      (when first-err
        (let* ((region (flycheck-error-region-for-mode first-err major-mode))
               (word (buffer-substring-no-properties (car region) (cdr region)))
               (splitted-err (flycheck-grammalecte--split-error-message first-err))
               (repl-menu (mapcar
                           #'(lambda (repl) (list repl repl))
                           (cdr splitted-err))))
          ;; Add a reminder of the error message
          (push (car splitted-err) repl-menu)
          (flycheck-grammalecte--fix-error
           first-err
           (car-safe
            (x-popup-menu
             event
             (list
              (format "Corrections pour %s" word)
              (cons "Suggestions de Grammalecte" repl-menu))))
           region))))))

;;;###autoload
(defun flycheck-grammalecte-download-grammalecte (grammalecte-version)
  "Download, extract and install Grammalecte python program.
This function will try to download the GRAMMALECTE-VERSION
of the python package.  If GRAMMALECTE-VERSION is \"last\", the last version
of the package will be downloaded.
This function can also be used at any time to upgrade the
Grammalecte python program."
  (interactive "sVersion: ")
  (flycheck-grammalecte--install-py-files
   (flycheck-grammalecte--extract-zip
    (flycheck-grammalecte--download-zip grammalecte-version))))

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
                          flycheck-grammalecte--directory))
        (flycheck-version-number (string-to-number (flycheck-version nil)))
        flycheck-grammalecte--error-patterns)

    (pcase-dolist (`(,mode . ,patterns) flycheck-grammalecte-filters-by-mode)
      (when (derived-mode-p mode)
        (dolist (filter patterns)
          (nconc filters (list "-f" filter)))))
    (pcase-dolist (`(,mode . ,border) flycheck-grammalecte-borders-by-mode)
      (when (derived-mode-p mode)
        (nconc filters (list "-b" border))))
    (unless flycheck-grammalecte-report-spellcheck (push "-S" cmdline))
    (unless flycheck-grammalecte-report-grammar (push "-G" cmdline))
    (unless flycheck-grammalecte-report-apos (push "-A" cmdline))
    (unless flycheck-grammalecte-report-nbsp (push "-N" cmdline))
    (unless flycheck-grammalecte-report-esp (push "-W" cmdline))
    (setq cmdline (nconc (list "python3" grammalecte-bin) filters cmdline))

    (setq flycheck-grammalecte--error-patterns
          (if (< flycheck-version-number 32)
              '((warning line-start "grammaire|" (message) "|" line "|"
                         (1+ digit) "|" column "|" (1+ digit) line-end)
                (info line-start "orthographe|" (message) "|" line "|"
                      (1+ digit) "|" column "|" (1+ digit) line-end))
            '((warning line-start "grammaire|" (message) "|" line "|" end-line
                       "|" column "|" end-column line-end)
              (info line-start "orthographe|" (message) "|" line "|" end-line
                    "|" column "|" end-column line-end))))

    (when flycheck-grammalecte--debug-mode
      (let ((checker-path (expand-file-name
                           "grammar_checker.py"
                           flycheck-grammalecte--grammalecte-directory)))
        (if (file-exists-p checker-path)
            (message "[Flycheck Grammalecte][DEBUG] Found in %s" checker-path)
          (message "[Flycheck Grammalecte][DEBUG] NOT FOUND")))
      (message "[Flycheck Grammalecte][DEBUG] Detected mode: %s" major-mode)
      (message "[Flycheck Grammalecte][DEBUG] Checker command: %s"
               (mapconcat
                #'(lambda (item)
                    (cond ((symbolp item) (format "'%s" (symbol-name item)))
                          ((stringp item) item)))
                cmdline " "))
      (message "[Flycheck Grammalecte][DEBUG] Flycheck error-patterns %s"
               flycheck-grammalecte--error-patterns))

    ;; Be sure grammalecte python module is accessible
    (flycheck-grammalecte--augment-pythonpath-if-needed)

    ;; Now that we have all our variables, we can create the custom
    ;; checker.
    (flycheck-define-command-checker 'grammalecte
      "Grammalecte syntax checker for french language
See URL `https://grammalecte.net/'."
      :command cmdline
      :error-patterns flycheck-grammalecte--error-patterns
      :modes flycheck-grammalecte-enabled-modes)
    (add-to-list 'flycheck-checkers 'grammalecte)

    (if (< flycheck-version-number 32)
        (let ((warn-user-about-flycheck
               #'(lambda (_arg)
                   (display-warning
                    'flycheck-grammalecte
                    (format "Le remplacement des erreurs ne fonctionne qu'avec flycheck >= 32 (vous utilisez la version %s)."
                            flycheck-version-number)))))
          ;; Desactivate corrections methods
          (advice-add 'flycheck-grammalecte-correct-error-at-click
                      :override
                      warn-user-about-flycheck)
          (advice-add 'flycheck-grammalecte-correct-error-at-point
                      :override
                      warn-user-about-flycheck))
      ;; Add our fixers to right click and C-c ! g
      (define-key flycheck-mode-map (kbd "<mouse-3>")
        #'flycheck-grammalecte-correct-error-at-click)
      (define-key flycheck-command-map "g"
        #'flycheck-grammalecte-correct-error-at-point))))

(add-hook 'after-change-major-mode-hook
          #'(lambda ()
              (when (memq major-mode flycheck-grammalecte-enabled-modes)
                (flycheck-grammalecte-setup))))

(provide 'flycheck-grammalecte)
;;; flycheck-grammalecte.el ends here
