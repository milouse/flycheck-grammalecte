;;; flycheck-grammalecte.el --- Integrate Grammalecte with Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Étienne Deparis
;; Copyright (C) 2017 Guilhem Doulcier

;; Maintainer: Étienne Deparis <etienne@depar.is>
;; Author: Guilhem Doulcier <guilhem.doulcier@espci.fr>
;;         Étienne Deparis <etienne@depar.is>
;; Created: 21 February 2017
;; Version: 2.4
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
(unless (fboundp 'pkg-info-version-info)
  (require 'pkg-info))

;; Version 2.0 introduced a major refactoring
(dolist
    (spec
     '((flycheck-grammalecte--debug-mode grammalecte--debug-mode)
       (flycheck-grammalecte--directory grammalecte--site-directory)
       (flycheck-grammalecte-grammalecte-directory grammalecte-python-package-directory)
       (flycheck-grammalecte-download-without-asking grammalecte-download-without-asking)
       (flycheck-grammalecte-mode-map grammalecte-mode-map)))
  (define-obsolete-variable-alias (car spec) (cadr spec) "2.0"))

(require 'grammalecte)

(dolist
    (spec
     '((flycheck-grammalecte--grammalecte-version grammalecte--version)
       (flycheck-grammalecte--grammalecte-upstream-version grammalecte--upstream-version)
       (flycheck-grammalecte-kill-ring-save grammalecte-kill-ring-save)
       (flycheck-grammalecte-save-and-replace grammalecte-save-and-replace)
       (flycheck-grammalecte-define grammalecte-define)
       (flycheck-grammalecte-define-at-point grammalecte-define-at-point)
       (flycheck-grammalecte-find-synonyms grammalecte-find-synonyms)
       (flycheck-grammalecte-find-synonyms-at-point grammalecte-find-synonyms-at-point)
       (flycheck-grammalecte-conjugate-verb grammalecte-conjugate-verb)
       (flycheck-grammalecte-download-grammalecte grammalecte-download-grammalecte)))
  (define-obsolete-function-alias (car spec) (cadr spec) "2.0"))


;; Make the compile happy about grammalecte lib
(declare-function grammalecte--version "grammalecte")
(declare-function grammalecte--augment-pythonpath-if-needed "grammalecte")
(eval-when-compile
  (defvar grammalecte--site-directory)
  (defvar grammalecte-python-package-directory))

;;;; Configuration options:

(defgroup flycheck-grammalecte nil
  "Flycheck Grammalecte options"
  :group 'flycheck-options
  :group 'grammalecte)

(defcustom flycheck-grammalecte-report-spellcheck nil
  "Report spellcheck errors if non-nil.
Default is nil.  You should use `flyspell' instead."
  :type 'boolean
  :package-version "0.2"
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-grammar t
  "Report grammar errors if non-nil.
Default is t."
  :type 'boolean
  :package-version "0.2"
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-apos t
  "Report apostrophe errors if non-nil.
Default is t."
  :type 'boolean
  :package-version "0.2"
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-nbsp t
  "Report non-breakable spaces errors if non-nil.
Default is t."
  :type 'boolean
  :package-version "0.2"
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-esp t
  "Report useless spaces and tabs errors if non-nil.
Default is t."
  :type 'boolean
  :package-version "0.6"
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-report-typo t
  "Report typographic signs errors if non-nil.
Default is t."
  :type 'boolean
  :package-version "2.4"
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
  :package-version "0.2"
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
  :package-version "1.1"
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-filters-by-mode
  '((latex-mode "\\\\(?:title|(?:sub)*section){([^}]+)}"
                "\\\\\\w+(?:\\[[^]]+\\])?(?:{[^}]*})?")
    (org-mode "(?ims)^[ \t]*#\\+begin_src.+?#\\+end_src"
              "(?im)^[ \t]*#\\+begin[_:].+$"
              "(?im)^[ \t]*#\\+end[_:].+$"
              "(?m)^[ \t]*(?:DEADLINE|SCHEDULED):.+$"
              "(?m)^\\*+ .*[ \t]*(:[\\w:@]+:)[ \t]*$"
              "(?im)^[ \t]*#\\+(?:caption|description|keywords|(?:sub)?title):"
              "(?im)^[ \t]*#\\+(?!caption|description|keywords|(?:sub)?title)\\w+:.*$")
    (message-mode "(?m)^[ \t]*(?:[\\w_.]+>|[]>|]).*"))
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
  :package-version "1.1"
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
  :type '(cons (function :tag "Mode") string)
  :package-version "1.1"
  :group 'flycheck-grammalecte)

(defcustom flycheck-grammalecte-predicate nil
  "A function to determine whether flycheck-grammalecte should be used.

This function is called without arguments and shall return non-nil if
flycheck-grammalecte shall be used to check the current buffer.  Otherwise it
shall return nil. This function is only called in matching major modes (see
`flycheck-grammalecte-enabled-modes'.

For example, if you only want to have flycheck-grammalecte in french
documents, you may want to use something like:

    (setq flycheck-grammalecte-predicate
          (lambda ()
            (or (and (derived-mode-p 'org-mode)
                     (equal \"fr\"
                            (or (cadar (org-collect-keywords '(\"LANGUAGE\")))
                                (bound-and-true-p
                                  org-export-default-language))))
                (and (boundp 'ispell-local-dictionary)
                     (member ispell-local-dictionary
                             '(\"fr\" \"francais7\" \"francais-tex\"))))))"
  :type 'function
  :package-version "2.0"
  :group 'flycheck-grammalecte)

(defconst flycheck-grammalecte--error-patterns
  (if (< (string-to-number (flycheck-version nil)) 32)
      '((warning line-start "grammaire|" (message) "|" line "|"
                 (1+ digit) "|" column "|" (1+ digit) line-end)
        (info line-start "orthographe|" (message) "|" line "|"
              (1+ digit) "|" column "|" (1+ digit) line-end))
    '((warning line-start "grammaire|" (message) "|" line "|" end-line
               "|" column "|" end-column line-end)
      (info line-start "orthographe|" (message) "|" line "|" end-line
            "|" column "|" end-column line-end)))
  "External python command output matcher for Flycheck.

It uses `rx' keywords, with some specific ones defined by Flycheck in
`flycheck-rx-to-string'.")


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
               (not (equal "digit" (match-string 1 regexp)))
               (not (equal "word" (match-string 1 regexp))))
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

(defun flycheck-grammalecte--patch-flycheck-mode-map ()
  "Add new commands to `flycheck-mode-map' if possible."
  (let ((flycheck-version-number
         (string-to-number (flycheck-version nil))))
    (if (< flycheck-version-number 32)
        (let ((warn-user-about-flycheck
               (lambda (_arg)
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

(defun flycheck-grammalecte--prepare-arg-list (arg items)
  "Build an arguments list for ARG from ITEMS elements.

This function may return nil if the current context does not allow any of the
ITEMS element to be used as argument."
  (let (arguments)
    (pcase-dolist (`(,mode . ,patterns) items)
      (when (derived-mode-p mode)
        (let (result)
          (if (listp patterns)
              ;; If items was a list of lists
              (when (dolist (elem patterns result)
                      (setq result (nconc result (list arg elem))))
                (setq arguments (nconc arguments result)))
            ;; In case items was a list of cons-cells
            (setq arguments (nconc arguments (list arg patterns)))))))
    arguments))

(defun flycheck-grammalecte--retry-setup (&optional _version)
  "Try to call again `flycheck-grammalecte-setup'.

This function is expected to be called as an advice to
`grammalecte-download-grammalecte'.

As soon as it is called, the advice is removed (as the setup function may
create it again if needed)."
  ;; Self-remove from advices if I was there.
  (when (advice-member-p #'flycheck-grammalecte--retry-setup
                         'grammalecte-download-grammalecte)
    (advice-remove 'grammalecte-download-grammalecte
                   #'flycheck-grammalecte--retry-setup))
  (flycheck-reset-enabled-checker 'grammalecte))

(defun flycheck-grammalecte--verify-setup (_)
  "Validate the Grammalecte setup.

This function is used internally by flycheck to determine wether
flycheck-grammalecte can be used or not."
  (let ((version (grammalecte--version)))
    (list (flycheck-verification-result-new
           :label "Grammalecte"
           :message (if version
                        (format "version %s found in %s"
                                version grammalecte-python-package-directory)
                      "Not found.  Please run `grammalecte-download-grammalecte' to install it.")
           :face (if version 'success '(bold error))))))



;;;; Public methods:

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
               repl-menu)
          (setq repl-menu
                (dolist (repl (cdr splitted-err) repl-menu)
                  (push (list repl repl) repl-menu)))
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



;;;; Checker definition:

;;;###autoload
(defun flycheck-grammalecte-setup ()
  "Build the flycheck checker, matching your taste."
  (let ((cmdline `("python3"
                   ,(expand-file-name "flycheck_grammalecte.py"
                                      grammalecte--site-directory)
                   ,(unless flycheck-grammalecte-report-spellcheck "-S")
                   ,(unless flycheck-grammalecte-report-grammar "-G")
                   ,(unless flycheck-grammalecte-report-apos "-A")
                   ,(unless flycheck-grammalecte-report-nbsp "-N")
                   ,(unless flycheck-grammalecte-report-esp "-W")
                   ,(unless flycheck-grammalecte-report-typo "-T")
                   (option-list "-f" flycheck-grammalecte-filters)
                   (eval (flycheck-grammalecte--prepare-arg-list
                          "-f" flycheck-grammalecte-filters-by-mode))
                   (eval (flycheck-grammalecte--prepare-arg-list
                          "-b" flycheck-grammalecte-borders-by-mode))
                   source)))

    ;; If grammalecte is not available, add a little advice to
    ;; `grammalecte-download-grammalecte'
    (unless (grammalecte--version)
      (advice-add 'grammalecte-download-grammalecte :after-while
                  #'flycheck-grammalecte--retry-setup))

    ;; Be sure grammalecte python module is accessible
    (grammalecte--augment-pythonpath-if-needed)

    ;; Now that we have all our variables, we can create the custom
    ;; checker.
    (flycheck-def-executable-var 'grammalecte "python3")
    (flycheck-define-command-checker 'grammalecte
      "Grammalecte syntax checker for french language
See URL `https://grammalecte.net/'."
      :command (seq-remove #'null cmdline)
      :error-patterns flycheck-grammalecte--error-patterns
      :modes flycheck-grammalecte-enabled-modes
      :predicate (lambda ()
                   (if (functionp flycheck-grammalecte-predicate)
                       (funcall flycheck-grammalecte-predicate)
                     t))
      :enabled #'grammalecte--version
      :verify #'flycheck-grammalecte--verify-setup)
    (add-to-list 'flycheck-checkers 'grammalecte)
    (flycheck-grammalecte--patch-flycheck-mode-map)))


(provide 'flycheck-grammalecte)
;;; flycheck-grammalecte.el ends here
