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

;; Functionality to embed other languages in typst documentation.

;;; Code:
(require 'treesit)

(defcustom typst-ts-enable-predefined-settings t
  "Whether to use predefined embedding language settings.
Use predefined settings will speed up the process of merging tree sitter
language mode settings.  However, settings (especially feature list)
may vary with different versions of a language mode, so you may get wrong
settings.
If you enable this feature, we highly recommend you to customize it when
error occurs."
  :type 'boolean
  :group 'typst-ts)

(defcustom typst-ts-highlight-raw-block-langs-not-in-settings nil
  "Whether to highlight raw block of language that is not in settings.
The cost for setting up these languages is usually higher than those
languages in settings."
  :type 'boolean
  :group 'typst-ts)

;; to test settings:
;; emacs --batch -l ./typst-ts-embedding-lang-settings.el --eval "(typst-ts-embedding-lang-settings-test)"
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
               ( bracket delimiter error function operator property variable))))
    (bash . (:feature
             sh-script
             :font-lock sh-mode--treesit-settings
             :indentation nil
             :ts-feature-list
             '(( comment function)
               ( command declaration-command keyword string)
               ( builtin-variable constant heredoc number
                 string-interpolation variable)
               ( bracket delimiter misc-punctuation operator)))))
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

(defun typst-ts-els-merge-lang-settings (lang)
  "Merge embedding language LANG settings."
  (let ((settings (alist-get lang typst-ts-embedding-lang-settings)))
    (if settings
        (typst-ts-els-merge-settings settings)
      (error "Language %s not in settings" lang))))

(defun typst-ts-els--treesit-range-rules (lang)
  "Get the treesit range rules for LANG.
LANG: language symbol."
  (treesit-range-rules
   :embed lang
   :host 'typst
   :local t
   `((raw_blck
      lang: (_) @_lang
      (blob) @capture
      (:equal @_lang ,(symbol-name lang))))))

(defun typst-ts-els--add-treesit-range-rules (lang)
  "Add treesit range rule for LANG.
LANG: language symbol."
  (setq
   treesit-range-settings
   (nconc
    treesit-range-settings
    (typst-ts-els--treesit-range-rules lang))))

(defun typst-ts-els--try-get-ts-settings (mode)
  (with-temp-buffer
    (setq-local delay-mode-hooks t)  ; don't run hooks associated with MODE
    (funcall mode)
    (list
     :treesit-font-lock-settings
     treesit-font-lock-settings
     :treesit-simple-indent-rules
     treesit-simple-indent-rules
     :treesit-font-lock-feature-list
     treesit-font-lock-feature-list)))

(defvar-local typst-ts-els--include-languages
    '(typst)
  "DON'T MANUALLY CHANGE THIS VARIABLE!")

(defun typst-ts-els-include-dynamically (_ranges _parser)
  "Include language setting dynamically.
Use this function as one notifier of `treesit-parser-notifiers'."
  ;; `treesit-language-at-point-function' will ensure that the
  ;; languages in `treesit-parser-list' are valid (not just a random string)
  (let ((parser-langs
         (delete-dups
          (append
           ;; parsers created by `treesit-language-at-point-function'
           ;; i.e. parsers cannot be created by `treesit-range-settings'
           (mapcar #'treesit-parser-language (treesit-parser-list))
           ;; parsers created by `treesit-range-settings'
           (mapcar #'treesit-parser-language
                   (treesit-local-parsers-on (point-min) (point-max))))))
        lang-ts-mode settings)
    (dolist (lang parser-langs)
      (unless (member lang typst-ts-els--include-languages)
        (unwind-protect
            (condition-case _err
                ;; first try loading settings from configuration
                (progn
                  (unless typst-ts-enable-predefined-settings
                    (error "User don't allow to load predefined settings"))
                  ;; note: the `treesit-range-settings' for languages in
                  ;; predefined settings are already settled at mode start
                  (typst-ts-els-merge-lang-settings lang)
                  (message "Load %s language settings from configuration." lang))
              (error
               ;; if language not in setting or encounter error during loading,
               ;; then try your luck to load it
               (condition-case err
                   (progn
                     ;; add range rules
                     (typst-ts-els--add-treesit-range-rules lang)
                     ;; delete top level parsers, so range rules works (i.e. local parsers)
                     ;; so that highlighting will not exceed the desired range
                     (mapc #'treesit-parser-delete (treesit-parser-list nil lang))

                     ;; find and merge settings
                     (setq lang-ts-mode
                           (intern (concat (symbol-name lang) "-ts-mode")))
                     (setq settings
                           (typst-ts-els--try-get-ts-settings lang-ts-mode))

                     (setq treesit-font-lock-settings
                           (append treesit-font-lock-settings
                                   (plist-get settings :treesit-font-lock-settings)))

                     (setq treesit-simple-indent-rules
                           (append treesit-simple-indent-rules
                                   (plist-get settings :treesit-simple-indent-rules)))

                     (setq treesit-font-lock-feature-list
                           (typst-ts-els--merge-features
                            treesit-font-lock-feature-list
                            (plist-get settings :treesit-font-lock-feature-list)))
                     (message "Luckily merged %s language settings." lang))
                 (error
                  (message "Loading %s language settings without luck: \n%s"
                           lang
                           (error-message-string err))))))
          ;; whatever, we won't load that language again
          (add-to-list 'typst-ts-els--include-languages lang))
        ))))

(defun typst-ts-embedding-lang-settings-test ()
  "Test typst-ts-embedding-lang-settings."
  (dolist (setting-entry typst-ts-embedding-lang-settings)
    (message "Testing %s ..." (car setting-entry))
    (typst-ts-els-merge-settings (cdr setting-entry)))
  (message "No problem found!"))

(provide 'typst-ts-embedding-lang-settings)

;;; typst-ts-embedding-lang-settings.el ends here
