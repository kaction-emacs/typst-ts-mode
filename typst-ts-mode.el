;;; typst-ts-mode.el --- tree-sitter support for Typst  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Keywords: typst languages tree-sitter
;; URL: https://git.sr.ht/~meow_king/typst-ts-mode
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29.1"))

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

;;; Code:

(require 'treesit)

(defgroup typst-ts nil
  "Tree Sitter enabled Typst Writing."
  :prefix "typst-ts"
  :group 'text
  :group 'languages)

(defgroup typst-ts-markup nil
  "Typst tree sitter markup."
  :prefix "typst-ts-markup"
  :group 'typst-ts)

(defgroup typst-ts-faces nil
  "Typst tree sitter faces."
  :prefix "typst-ts-faces"
  :group 'typst-ts)

(defcustom typst-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `json-ts-mode'."
  :type 'integer
  :group 'typst-ts)

(defcustom typst-ts-markup-header-same-height nil
  "Whether to make header face in markup context share the same height."
  :type 'boolean
  :group 'typst-ts-faces)

(defcustom typst-ts-markup-header-scale
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "Header Scale."
  :type 'list
  :set (lambda (symbol value)
         (set-default symbol value)
         (when typst-ts-markup-header-same-height
           (set-default symbol (make-list (length value) 1.0))))
  :set-after typst-ts-markup-header-same-height
  :group 'typst-ts-faces)

;; Markup Faces =================================================================

(defface typst-ts-markup-header-face
  '((t :weight bold))
  "The basic face for Typst ts markup headers.")

(defface typst-ts-markup-header-face-1
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 0 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-2
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 1 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-3
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 2 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-4
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 3 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-5
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 4 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-6
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 5 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-shorthand-face
  '((t :inherit shadow))
  "Face for linebreak."
  :group 'typst-ts-faces)

(defface typst-ts-markup-url-face
  '((t :inherit link))
  "Face for url."
  :group 'typst-ts-faces)

(defface typst-ts-markup-emphasis-face
  '((t :inherit italic))
  "Face for emphasis."
  :group 'typst-ts-faces)

(defface typst-ts-markup-strong-face
  '((t :inherit bold))
  "Face for strong."
  :group 'typst-ts-faces)

(defface typst-ts-markup-item-face
  '((t :inherit shadow))
  "Face for item."
  :group 'typst-ts-faces)

;; NOTE: add a defcustom option to let user choose whether to only fontify the
;; the whole element
;; same as `typst-ts-markup-rawspan-face' and `typst-ts-markup-rawblock-face'
(defface typst-ts-markup-term-face
  nil
  "Face for term."
  :group 'typst-ts-faces)

(defface typst-ts-markup-term-indicator-face
  '((t :inherit shadow))
  "Face for term."
  :group 'typst-ts-faces)

(defface typst-ts-markup-term-term-face
  '((t :inherit bold))
  "Face for term."
  :group 'typst-ts-faces)

(defface typst-ts-markup-term-description-face
  '((t :inherit normal))
  "Face for term."
  :group 'typst-ts-faces)

(defface typst-ts-markup-quote-face ;; TODO better choice?
  '((t :inherit shadow))
  "Face for quote."
  :group 'typst-ts-faces)

(defface typst-ts-markup-linebreak-face
  '((t :inherit escape-glyph))
  "Face for linebreak."
  :group 'typst-ts-faces)

(defface typst-ts-markup-escape-face
  '((t :inherit escape-glyph))
  "Face for linebreak."
  :group 'typst-ts-faces)

(defface typst-ts-markup-raw-indicator-face
  '((t :inherit shadow))
  "Face for rawblock and rawspan indicator."
  :group 'typst-ts-faces)

(defface typst-ts-markup-raw-blob-face
  '((t :inherit variable-pitch))
  "Face for rawblock and rawspan blob."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawblock-face
  '((t :inherit normal))
  "Face for rawblock."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawblock-indicator-face
  '((t :inherit typst-ts-markup-raw-indicator-face))
  "Face for rawblock indicator."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawblock-blob-face
  '((t :inherit typst-ts-markup-raw-blob-face))
  "Face for rawblock blob."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawspan-face
  '((t :inherit normal))
  "Face for rawspan."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawspan-indicator-face
  '((t :inherit typst-ts-markup-raw-indicator-face))
  "Face for rawspan indicator."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawspan-lang-face
  '((t :inherit variable-pitch))
  "Face for rawspan ident."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawspan-blob-face
  '((t :inherit typst-ts-markup-raw-blob-face))
  "Face for rawspan blob."
  :group 'typst-ts-faces)

(defface typst-ts-markup-label-face
  '((t :inherit homoglyph))
  "Face for label."
  :group 'typst-ts-faces)

(defface typst-ts-markup-reference-face
  '((t :inherit homoglyph))
  "Face for reference."
  :group 'typst-ts-faces)

;; Code Faces ===================================================================

(defface typst-ts-code-indicator-face
  '((t :inherit shadow))
  "Face for code indicator #."
  :group 'typst-ts-faces)


;; Math Faces ===================================================================

(defface typst-ts-math-indicator-face
  '((t :inherit shadow))
  "Face for math indicator $."
  :group 'typst-ts-faces)


(defvar typst-ts-mode-font-lock-rules
  '(;; Typst font locking
    :language typst
    :feature comment
    ((comment) @font-lock-comment-face)

    :language typst
    :feature common
    ((shorthand) @typst-ts-shorthand-face)

    :language typst
    :feature markup
    ((heading) @typst-ts-markup-header-face
     (url) @typst-ts-markup-url-face
     (emph) @typst-ts-markup-emphasis-face
     (strong) @typst-ts-markup-strong-face
     (item "item" @typst-ts-markup-item-face)
     (term
      "item" @typst-ts-markup-term-indicator-face
      term: (text) @typst-ts-markup-term-term-face
      ":" @typst-ts-markup-term-indicator-face
      (text) @typst-ts-markup-term-description-face)
     (quote) @typst-ts-markup-quote-face
     (linebreak) @typst-ts-markup-linebreak-face
     (escape) @typst-ts-markup-escape-face
     (raw_span
      "`" @typst-ts-markup-rawspan-indicator-face
      (blob) @typst-ts-markup-rawspan-blob-face
      "`" @typst-ts-markup-rawspan-indicator-face)
     (raw_blck
      "```" @typst-ts-markup-rawblock-indicator-face
      (ident) :? @typst-ts-markup-rawspan-lang-face
      (blob) @typst-ts-markup-rawblock-blob-face ;; TODO use function to fontify region
      "```" @typst-ts-markup-rawblock-indicator-face)
     (label) @typst-ts-markup-label-face ;; TODO more precise highlight (upstream)
     (ref) @typst-ts-markup-reference-face)

    ;; please note that some feature there also in the math mode
    :language typst
    :feature code
    ("#" @typst-ts-code-indicator-face
     ;; "end" @typst-ts-code-indicator-face ;; "end" is nothing but only a indicator
     (number) @font-lock-number-face
     (string) @font-lock-string-face
     (content ["[" "]"] @font-lock-punctuation-face)
     (builtin) @font-lock-builtin-face
     (bool) @font-lock-constant-face
     (none) @font-lock-constant-face
     (auto) @font-lock-constant-face
     (ident) @font-lock-variable-use-face

     ["(" ")" "{" "}"] @font-lock-punctuation-face
     ["," ";" ".." ":" "sep"] @font-lock-punctuation-face
     "assign" @font-lock-punctuation-face
     (field "." @font-lock-punctuation-face)

     ;; operator
     (in ["in" "not"] @font-lock-keyword-face)
     (and "and" @font-lock-keyword-face)
     (or "or" @font-lock-keyword-face)
     (not "not" @font-lock-keyword-face)
     (sign ["+" "-"] @font-lock-operator-face)
     (add "+" @font-lock-operator-face)
     (sub "-" @font-lock-operator-face)
     (mul "*" @font-lock-operator-face)
     (div "/" @font-lock-operator-face)
     (cmp ["==" "<=" ">=" "!=" "<" ">"] @font-lock-operator-face)
     (wildcard) @font-lock-operator-face

     ;; control
     (let "let" @font-lock-keyword-face)
     (branch ["if" "else"] @font-lock-keyword-face)
     (while "while" @font-lock-keyword-face)
     (for ["for" "in"] @font-lock-keyword-face)
     (import "import" @font-lock-keyword-face)
     (as "as" @font-lock-keyword-face)
     (include "include" @font-lock-keyword-face)
     (show "show" @font-lock-keyword-face)
     (set "set" @font-lock-keyword-face)
     (return "return" @font-lock-keyword-face)
     (flow ["break" "continue"] @font-lock-keyword-face)

     ;; TODO lambda
     (call ;; function
      item: (ident) @font-lock-function-call-face)
     (call ;; method
      item: (field field: (ident) @font-lock-function-call-face))
     (tagged field: (ident) @font-lock-variable-name-face)
     (field field: (ident) @font-lock-constant-face))

    :language typst
    :feature math
    ((math "$" @typst-ts-math-indicator-face)
     
     (fraction "/" @font-lock-operator-face)
     (fac "!" @font-lock-operator-face)
     (attach ["^" "_"] @font-lock-operator-face)
     (align) @font-lock-operator-face
     
     (symbol) @font-lock-constant-face
     (letter) @font-lock-constant-face)))

(defun typst-ts-mode-comment-setup()
  "Setup comment related stuffs for typst-ts-mode."
  ;; stolen from `c-ts-common-comment-setup'
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (or (seq "/" (+ "/"))
                                         (seq "/" (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") "/"))))))


;;;###autoload
(define-derived-mode typst-ts-mode text-mode "Typst"
  "Major mode for editing Typst, powered by tree-sitter."
  :group 'typst
  ;; TODO syntax table
  ;; :syntax-table typst-ts-mode--syntax-table

  (unless (treesit-ready-p 'typst)
    (error "Tree-sitter for Typst isn't available"))
  (treesit-parser-create 'typst)

  ;; Comments.
  (typst-ts-mode-comment-setup)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}()[]" electric-indent-chars))

  ;; Font Lock TODO
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules typst-ts-mode-font-lock-rules))
  (setq-local treesit-font-lock-feature-list
              ;; TODO
              '((comment common markup code math)))

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))

(provide 'typst-ts-mode)
