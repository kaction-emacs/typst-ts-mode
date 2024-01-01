;;; typst-ts-embedding-lang-settings.el --- Embedding Languages Settings  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains settings for tree sitter languages.
;; to test settings:
;; emacs --batch -l ./typst-ts-embedding-lang-settings.el --eval "(typst-ts-embedding-lang-settings-test)"

;;; Code:
(require 'treesit)

(defvar typst-ts-embedding-lang-settings
  '((python . (:feature
               python
               :font-lock python--treesit-settings
               :indentation nil
               :ts-feature-list
               '(( comment definition)
                 ( keyword string type)
                 ( assignment builtin constant decorator
                   escape-sequence number string-interpolation )
                 ( bracket delimiter function operator variable property ))))
    (rust . (:feature
             rust-ts-mode
             :font-lock rust-ts-mode--font-lock-settings
             :indentation rust-ts-mode--indent-rules
             :ts-feature-list
             '(( comment definition)
               ( keyword string)
               ( assignment attribute builtin constant escape-sequence
                 number type)
               ( bracket delimiter error function operator property variable)))))
  "Settings for raw block languages.")


;; from vimscript-ts-mode (https://github.com/nverno/vimscript-ts-mode)
(defun typst-ts-els--merge-features (a b)
  "Merge `treesit-font-lock-feature-list's A with B."
  (cl-loop for x in a
           for y in b
           collect (seq-uniq (append x y))))

;; hugely insprired by vimscript-ts-mode (https://github.com/nverno/vimscript-ts-mode)
(defun typst-ts-els-merge-settings (settings)
  "Merge SETTINGS."
  (let ((feature (plist-get settings :feature))
        (font-lock-settings (plist-get settings :font-lock))
        (indentation-rules (plist-get settings :indentation))
        (ts-feature-list (plist-get settings :ts-feature-list)))
    (require feature)
    (setq font-lock-settings (eval font-lock-settings))
    (setq indentation-rules (eval indentation-rules))
    (setq ts-feature-list (eval ts-feature-list))
    (setq-local
     treesit-font-lock-settings (append treesit-font-lock-settings
                                        font-lock-settings)
     treesit-simple-indent-rules (append treesit-simple-indent-rules
                                         indentation-rules)
     treesit-font-lock-feature-list (typst-ts-els--merge-features
                                     treesit-font-lock-feature-list
                                     ts-feature-list))))

(defun typst-ts-els-merge-lang (lang)
  "Merge embedding language LANG settings."
  (let ((settings (alist-get lang typst-ts-embedding-lang-settings)))
    (if settings
        (typst-ts-els-merge-settings settings)
      (signal 'lang-no-in-settings '(lang)))))

(defun typst-ts-els--try-get-variable--try-name (prefixes postfixes)
  (let (variable)
    (catch 'exit
      (cl-loop for prefix in prefixes
               for postfix in postfixes
               do
               (ignore-errors
                 (setq variable (intern (concat prefix postfix)))
                 (throw 'exit (eval variable)))))))

(defun typst-ts-els--try-get-fls--try-name (_lang prefixes)
  (typst-ts-els--try-get-variable--try-name
   prefixes
   '("--font-lock-settings" "-font-lock-settings")))

(defun typst-ts-els--try-get-ir--try-name (_lang prefixes)
  (typst-ts-els--try-get-variable--try-name
   prefixes
   '("--indent-rules" "-indent-rules")))

(defvar typst-ts-els--try-get-feature-name-list
  '("-ts-mode" "-mode" "")
  "Used by `typset-ts-els--try-require-feature'.")

(defvar typst-ts-els--try-variable-prefix-list
  '("-ts-mode" "-ts" "")
  "Used by `typset-ts-els--try-require-feature'.")

(defvar typst-ts-els--try-get-fls--func-list
  '(typst-ts-els--try-get-fls--try-name)
  "TODO documentation.")

(defvar typst-ts-els--try-get-ir--func-list
  '(typst-ts-els--try-get-ir--try-name)
  "TODO documentation.")

(defun typst-ts-els--try-require-feature (lang)
  "Try to require feature for LANG.
If success, then return the feature symbol, else nil."
  (let (feature)
    (catch 'exit
      (dolist (elem typst-ts-els--try-get-feature-name-list)
        (setq feature
              (intern (concat (symbol-name lang) elem)))
        (ignore-errors
          (require feature)
          (throw 'exit feature))))))

(defun typst-ts-els--try-get-variable-value (lang prefixes func-list)
  "Try to get value of a variable specified for LANG.
The variable is like font lock settings or indentation rules.  It is specified
by FUNC-LIST.
Return nil if cannot get.
PREFIXES: a list of variable name prefix.
FUNC-LIST: functions to be called to get the variable value."
  (let (value)
    (catch 'exit
      (dolist (func func-list)
        (ignore-errors
          (setq value (funcall func lang prefixes))
          (when value
            (throw 'exit value)))))))

(defun typst-ts-els--try-get-font-lock-settings (lang prefixes)
  (typst-ts-els--try-get-variable-value
   lang prefixes typst-ts-els--try-get-fls--func-list))

(defun typst-ts-els--try-get-indentation-rules (lang prefixes)
  (typst-ts-els--try-get-variable-value
   lang prefixes typst-ts-els--try-get-ir--func-list))

;; NOTE this operation is high cost
(defun typst-ts-els--try-get-ts-feature-list (mode)
  (with-temp-buffer
    (funcall mode)
    treesit-font-lock-feature-list))

(defvar-local typst-ts-els--include-languages
    '(typst)
  "DON'T MANUALLY CHANGE THIS VARIABLE!")

(defun typst-ts-els-include-dynamically (_ranges _parser)
  "Include language setting dynamically.
Use this function as one notifier of `treesit-parser-notifiers'."
  (let ((parsers (treesit-parser-list))
        lang
        feature prefixes font-lock-settings indentation-rules ts-feature-list)
    (unless (eq (length parsers) (length typst-ts-els--include-languages))
      (dolist (parser parsers)
        (setq lang (treesit-parser-language parser))
        (unless (member lang typst-ts-els--include-languages)
          (unwind-protect
              (condition-case _err
                  ;; first try loading settings from configuration
                  (typst-ts-els-merge-lang lang)
                (error
                 ;; if not found or encountering error during loading,
                 ;; then guess variables to load
                 (catch 'exit
                   (ignore-errors
                     ;; require feature
                     (setq feature (typst-ts-els--try-require-feature lang))
                     (unless feature
                       (throw 'exit "no feature found"))

                     (setq prefixes
                           (cl-loop for postfix in typst-ts-els--try-variable-prefix-list
                                    collect (concat (symbol-name lang) postfix)))

                     ;; merge font lock settings
                     (setq font-lock-settings
                           (typst-ts-els--try-get-font-lock-settings lang prefixes))
                     (setq treesit-font-lock-settings
                           (append treesit-font-lock-settings
                                   font-lock-settings))

                     ;; merge indent rules
                     (setq indentation-rules
                           (typst-ts-els--try-get-indentation-rules lang prefixes))
                     (setq treesit-simple-indent-rules
                           (append treesit-simple-indent-rules
                                   indentation-rules))
                     
                     ;; merge ts feature lists
                     (setq ts-feature-list
                           (typst-ts-els--try-get-ts-feature-list
                            (intern (concat (symbol-name lang) postfix))))
                     (setq treesit-font-lock-feature-list
                           (typst-ts-els--merge-features
                            treesit-font-lock-feature-list
                            ts-feature-list))
                     ))))
            (add-to-list typst-ts-els--include-languages lang))
          )))))

(defun typst-ts-embedding-lang-settings-test ()
  "Test typst-ts-embedding-lang-settings."
  (dolist (setting-entry typst-ts-embedding-lang-settings)
    (message "Testing %s ..." (car setting-entry))
    (typst-ts-els-merge-settings (cdr setting-entry)))
  (message "No problem found!"))


(provide 'typst-ts-embedding-lang-settings)

;;; typst-ts-embedding-lang-settings.el ends here
