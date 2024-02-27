;;; typst-ts-mode.el --- Tree Sitter support for Typst  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; Version: 0.10.0
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

;; Tree Sitter Support for Typst

;;; Code:

(require 'treesit)
(require 'compile)
(require 'outline)

(require 'typst-ts-embedding-lang-settings)

(defgroup typst-ts nil
  "Tree Sitter enabled Typst Writing."
  :prefix "typst-ts"
  :group 'text
  :group 'languages)

(defgroup typst-ts-faces nil
  "Typst tree sitter faces."
  :prefix "typst-ts-faces"
  :group 'typst-ts)

(defcustom typst-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `typst-ts-mode'."
  :type 'natnum
  :group 'typst-ts)


(defcustom typst-ts-mode-indent-offset-section 2
  "The indent offset for section.
i.e. The indentation offset after header."
  :type 'natnum
  :group 'typst-ts)

(defcustom typst-ts-mode-fontification-precision-level 'middle
  "Whether to use precise face fontification.
Note that precise face fontification will case performance degrading.
The performance degrading is mainly on the first load of the file.  Since
treesit incrementally fontifys regions (IMO), the later editing experience won't
be noticeably affected probably."
  :type '(choice (const :tag "Minimum level" min)
                 (const :tag "Middle level" middle)
                 (const :tag "Maximum level" max))
  :group 'typst-ts)

;; TODO currently set nil as default, since it still needs refinement
(defcustom typst-ts-mode-enable-raw-blocks-highlight nil
  "Whether to enable raw block highlighting."
  :type 'boolean
  :group 'typst-ts)

(defcustom typst-ts-mode-highlight-raw-blocks-at-startup nil
  "Whether to highlight raw blocks at *mode startup*.
Note: this may take some time for documents with lot of raw blocks."
  :type 'boolean
  :group 'typst-ts)

(defcustom typst-ts-mode-executable-location "typst"
  "The location or name(if in variable `exec-path') for Typst executable."
  :type 'string
  :group 'typst-ts)

(defcustom typst-ts-mode-compile-options ""
  "User defined compile options for `typst-ts-mode-compile'.
The compile options will be passed to the end of
`<typst-executable> compile <current-file>' command."
  :type 'string
  :group 'typst-ts)

(defcustom typst-ts-mode-before-compile-hook nil
  "Hook runs after compile."
  :type 'hook
  :group 'typst-ts)

(defcustom typst-ts-mode-after-compile-hook nil
  "Hook runs after compile.
Note the requirement of this hook is the same as `compilation-finish-functions'.
Also note that this hook runs with typst buffer(the buffer you are editing) as
the current buffer."
  :type 'hook
  :group 'typst-ts)

(defcustom typst-ts-mode-tab-function 'indent-for-tab-command
  "Default function for `typst-ts-mode-cycle' when conditions don't match."
  :type 'function
  :group 'typst-ts)

(defcustom typst-ts-mode-return-function 'newline
  "Default function for `typst-ts-mode-return' when conditions don't match."
  :type 'function
  :group 'typst-ts)

(defcustom typst-ts-mode-watch-options ""
  "User defined compile options for `typst-ts-mode-watch'.
The compile options will be passed to the
`<typst-executable> watch <current-file>' sub-command."
  :type 'string
  :group 'typst-ts)

(defcustom typst-ts-mode-watch-modeline-indicator-enable t
  "Whether to enable mode line indicator for typst watch."
  :type 'boolean
  :group 'typst-ts)

(defcustom typst-ts-mode-watch-modeline-indicator "[Watch]"
  "Modeline indicator for typst watch."
  :type 'string
  :group 'typst-ts)

(defcustom typst-ts-mode-watch-process-name "*Typst-Watch*"
  "Process name for `typst watch' sub-command."
  :type 'string
  :group 'typst-ts)

(defcustom typst-ts-mode-watch-process-buffer-name "*Typst-Watch*"
  "Process buffer name for `typst watch' sub-command."
  :type 'string
  :group 'typst-ts)

(defcustom typst-ts-mode-display-watch-process-bufer-automatically t
  "Whether the typst watch process buffer should be displayed automatically.
This means the buffer will be displayed when error occurs, hide when error
is eliminated."
  :type 'boolean
  :group 'typst-ts)

(defcustom typst-ts-mode-display-watch-process-buffer-parameters
  `(display-buffer-at-bottom
    (window-height . fit-window-to-buffer))
  "Display buffer parameters."
  :type 'symbol
  :group 'typst-ts)

(defvar typst-ts-mode-before-watch-hook nil
  "Hook runs after compile.")

(defvar typst-ts-mode-after-watch-hook nil
  "Hook runs after compile.")

(defcustom typst-ts-markup-header-same-height t
  "Whether to make header face in markup context share the same height.
Note it only works when user choose `max' level of fontification precision
level.  See `typst-ts-mode-fontification-precision-level'."
  :type 'boolean
  :group 'typst-ts-faces)

(defcustom typst-ts-markup-header-scale
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "Header Scale."
  :type '(list integer integer integer integer integer integer)
  :set (lambda (symbol value)
         (set-default symbol value)
         (when typst-ts-markup-header-same-height
           (set-default symbol (make-list (length value) 1.0))))
  :set-after '(typst-ts-markup-header-same-height)
  :group 'typst-ts-faces)

;; Face =========================================================================
(defface typst-ts-watch-modeline-indicator-face
  '((t :inherit (underline bold)))
  "Face for typst watch modeline indicator."
  :group 'typst-ts-faces)

;; Common Face ==================================================================

(defface typst-ts-shorthand-face
  '((t :inherit shadow))
  "Face for linebreak."
  :group 'typst-ts-faces)

(defface typst-ts-error-face
  '((t :inherit font-lock-warning-face))
  "Face for linebreak."
  :group 'typst-ts-faces)

;; Markup Faces =================================================================

(defface typst-ts-markup-header-indicator-face
  '((t :weight bold))
  "Face for Typst ts markup header indicator.")

(defface typst-ts-markup-header-face
  '((t :weight bold))
  "Face for Typst ts markup headers text.")

(defface typst-ts-markup-header-indicator-face-1
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 0 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-1
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 0 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-indicator-face-2
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 1 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-2
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 1 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-indicator-face-3
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 2 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-3
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 2 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-indicator-face-4
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 3 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-4
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 3 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-indicator-face-5
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 4 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-5
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 4 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-indicator-face-6
  `((t :inherit typst-ts-markup-header-indicator-face
       :height ,(nth 5 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-indicator-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-header-face-6
  `((t :inherit typst-ts-markup-header-face
       :height ,(nth 5 typst-ts-markup-header-scale)))
  "See `typst-ts-markup-header-face'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-url-face
  '((t :inherit link))
  "Face for url."
  :group 'typst-ts-faces)

(defface typst-ts-markup-emphasis-indicator-face
  '((t :inherit italic))
  "Face for emphasis."
  :group 'typst-ts-faces)

(defface typst-ts-markup-emphasis-face
  '((t :inherit italic))
  "Face for emphasis."
  :group 'typst-ts-faces)

(defface typst-ts-markup-strong-indicator-face
  '((t :inherit bold))
  "Face for strong."
  :group 'typst-ts-faces)

(defface typst-ts-markup-strong-face
  '((t :inherit bold))
  "Face for strong."
  :group 'typst-ts-faces)

(defface typst-ts-markup-item-face
  '((t :inherit shadow))
  "Face for whole term, use in min and middle fontification level.
See `typst-ts-mode-fontification-precision-level'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-item-indicator-face
  '((t :inherit shadow))
  "Face for item."
  :group 'typst-ts-faces)

(defface typst-ts-markup-item-text-face
  '((t :inherit default))
  "Face for item."
  :group 'typst-ts-faces)

(defface typst-ts-markup-term-face
  '((t :inherit shadow))
  "Face for whole term, use in min and middle fontification level.
See `typst-ts-mode-fontification-precision-level'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-term-indicator-face
  '((t :inherit shadow))
  "Face for term indicator."
  :group 'typst-ts-faces)

(defface typst-ts-markup-term-term-face
  '((t :inherit bold))
  "Face for term text."
  :group 'typst-ts-faces)

(defface typst-ts-markup-term-description-face
  '((t :inherit italic))
  "Face for term description."
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
  '((t :inherit shadow))
  "Face for rawblock and rawspan blob."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawblock-face
  '((t :inherit shadow))
  "Face for whole raw block, use in min and middle fontification level.
See `typst-ts-mode-fontification-precision-level'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawblock-indicator-face
  '((t :inherit typst-ts-markup-raw-indicator-face))
  "Face for rawblock indicator."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawblock-lang-face
  '((t :inherit font-lock-type-face))
  "Face for rawspan ident."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawblock-blob-face
  '((t :inherit typst-ts-markup-raw-blob-face))
  "Face for rawblock blob."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawspan-face
  '((t :inherit shadow))
  "Face for whole raw span, use in min and middle fontification level.
See `typst-ts-mode-fontification-precision-level'."
  :group 'typst-ts-faces)

(defface typst-ts-markup-rawspan-indicator-face
  '((t :inherit typst-ts-markup-raw-indicator-face))
  "Face for rawspan indicator."
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


;; ==============================================================================
;; TODO typst has three modes (namely 'markup', 'code' and 'math')
;; Currently only add common settings to syntax table
(defvar typst-ts-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; comment
    (modify-syntax-entry  ?/     ". 124b"  st)
    (modify-syntax-entry  ?*     ". 23"    st)
    (modify-syntax-entry  ?\n    "> b"     st)
    st))

(defvar typst-ts-mode-font-lock-rules
  nil
  "You can customize this variable to override the whole default font lock rules.
Like this:

(setq typst-ts-mode-font-lock-rules
        (append
         (typst-ts-mode-font-lock-rules)
         \='(
           :language typst
           :type custom
           ((el-psy-kongaroo) @el-psy-kongaroo))))

However, if you only want to modify specific part of the font lock rules, please
customize variables starts with `typst-ts-mode-font-lock-rules-'.  The trailing
part of the name is in the `typst-ts-mode-font-lock-feature-list'.

BTW, if you want to enable/disable specific font lock feature, please change
`treesit-font-lock-level' or modify `typst-ts-mode-font-lock-feature-list'.")

(defvar typst-ts-mode-font-lock-rules-comment nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-common nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-markup-basic nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-code-basic nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-math-basic nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-markup-standard nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-code-standard nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-math-standard nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-markup-extended nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-code-extended nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defvar typst-ts-mode-font-lock-rules-math-extended nil
  "See variable `typst-ts-mode-font-lock-rules'.")

(defun typst-ts-mode-highlight-block-fn (node _override _start _end)
  "A function used in `typst-ts-mode-font-lock-rules'.
This function assign `typst-ts-markup-rawblock-blob-face' to those raw block
whose language cannot be found or be loaded.
NODE."
  (let ((ns (treesit-node-start node))
        (ne (treesit-node-end node))
        (lang-node (treesit-node-prev-sibling node))
        lang)
    (if (not (equal (treesit-node-type lang-node) "ident"))
        (put-text-property ns ne 'face 'typst-ts-markup-rawblock-blob-face)
      (setq lang (gethash
                  (downcase (treesit-node-text lang-node))
                  typst-ts-els-tag-lang-map))
      (unless (and lang (treesit-ready-p lang t))
        (put-text-property ns ne 'face 'typst-ts-markup-rawblock-blob-face)))))

(defun typst-ts-mode-font-lock-rules ()
  ;; use function `typst-ts/util/setup-fontification-debug-environment' in
  ;; `side/utils.el' to setup test environment.
  "Generate font lock rules for `treesit-font-lock-rules'.
If you want to customize the rules, please customize the same name variable
`typst-ts-mode-font-lock-rules'."
  (let ((markup-basic
         (pcase typst-ts-mode-fontification-precision-level
           ('min
            `((heading) @typst-ts-markup-header-face
              (emph) @typst-ts-markup-emphasis-face
              (strong) @typst-ts-markup-strong-face
              (item) @typst-ts-markup-item-face
              (term) @typst-ts-markup-term-face
              (raw_span) @typst-ts-markup-rawspan-face
              ,@(if typst-ts-mode-enable-raw-blocks-highlight
                    '((raw_blck
                       "```" @typst-ts-markup-rawblock-indicator-face
                       (ident) :? @typst-ts-markup-rawblock-lang-face
                       (blob) @typst-ts-mode-highlight-block-fn
                       "```" @typst-ts-markup-rawblock-indicator-face))
                  '((raw_blck) @typst-ts-markup-rawblock-face))
              (label) @typst-ts-markup-label-face
              (ref) @typst-ts-markup-reference-face))
           ('middle
            `((heading) @typst-ts-markup-header-face
              (emph) @typst-ts-markup-emphasis-face
              (strong) @typst-ts-markup-strong-face
              (item
               "-" @typst-ts-markup-item-indicator-face
               (text) @typst-ts-markup-item-text-face)
              (term
               "/" @typst-ts-markup-term-indicator-face
               term: (text) @typst-ts-markup-term-term-face
               ":" @typst-ts-markup-term-indicator-face
               (text) @typst-ts-markup-term-description-face)
              (raw_span
               "`" @typst-ts-markup-rawspan-indicator-face
               (blob) @typst-ts-markup-rawspan-blob-face
               "`" @typst-ts-markup-rawspan-indicator-face)
              (raw_blck
               "```" @typst-ts-markup-rawblock-indicator-face
               (ident) :? @typst-ts-markup-rawblock-lang-face
               ;; NOTE let embedded language fontify blob
               ,@(if typst-ts-mode-enable-raw-blocks-highlight
                     '((blob) @typst-ts-mode-highlight-block-fn)
                   '((blob) @typst-ts-markup-rawblock-blob-face))
               "```" @typst-ts-markup-rawblock-indicator-face)
              (label) @typst-ts-markup-label-face
              (ref) @typst-ts-markup-reference-face))
           ('max
            `(,@(if typst-ts-markup-header-same-height
                    '((heading "=" @typst-ts-markup-header-indicator-face
                               (text) @typst-ts-markup-header-face)
                      (heading "==" @typst-ts-markup-header-indicator-face
                               (text) @typst-ts-markup-header-face)
                      (heading "===" @typst-ts-markup-header-indicator-face
                               (text) @typst-ts-markup-header-face)
                      (heading "====" @typst-ts-markup-header-indicator-face
                               (text) @typst-ts-markup-header-face)
                      (heading "=====" @typst-ts-markup-header-indicator-face
                               (text) @typst-ts-markup-header-face)
                      (heading "======" @typst-ts-markup-header-indicator-face
                               (text) @typst-ts-markup-header-face))
                  '((heading "=" @typst-ts-markup-header-indicator-face-1
                             (text) @typst-ts-markup-header-face-1)
                    (heading "==" @typst-ts-markup-header-indicator-face-2
                             (text) @typst-ts-markup-header-face-2)
                    (heading "===" @typst-ts-markup-header-indicator-face-3
                             (text) @typst-ts-markup-header-face-3)
                    (heading "====" @typst-ts-markup-header-indicator-face-4
                             (text) @typst-ts-markup-header-face-4)
                    (heading "=====" @typst-ts-markup-header-indicator-face-5
                             (text) @typst-ts-markup-header-face-5)
                    (heading "======" @typst-ts-markup-header-indicator-face-6
                             (text) @typst-ts-markup-header-face-6)))
              (emph
               "_" @typst-ts-markup-emphasis-indicator-face
               (text) @typst-ts-markup-emphasis-face
               "_" @typst-ts-markup-emphasis-indicator-face)
              (strong
               "*" @typst-ts-markup-strong-indicator-face
               (text) @typst-ts-markup-strong-face
               "*" @typst-ts-markup-strong-indicator-face)
              (item
               "-" @typst-ts-markup-item-indicator-face
               (text) @typst-ts-markup-item-text-face)
              (term
               "/" @typst-ts-markup-term-indicator-face
               term: (text) @typst-ts-markup-term-term-face
               ":" @typst-ts-markup-term-indicator-face
               (text) @typst-ts-markup-term-description-face)
              (escape) @typst-ts-markup-escape-face
              (raw_span
               "`" @typst-ts-markup-rawspan-indicator-face
               (blob) @typst-ts-markup-rawspan-blob-face
               "`" @typst-ts-markup-rawspan-indicator-face)
              (raw_blck
               "```" @typst-ts-markup-rawblock-indicator-face
               (ident) :? @typst-ts-markup-rawblock-lang-face
               ;; NOTE let embedded language fontify blob
               ,@(if typst-ts-mode-enable-raw-blocks-highlight
                     '((blob) @typst-ts-mode-highlight-block-fn)
                   '((blob) @typst-ts-markup-rawblock-blob-face))
               "```" @typst-ts-markup-rawblock-indicator-face)
              (label) @typst-ts-markup-label-face  ; TODO more precise highlight (upstream)
              (ref) @typst-ts-markup-reference-face)
            ))))
    `(;; Typst font locking
      :language typst
      :feature comment
      ,(if typst-ts-mode-font-lock-rules-comment
           typst-ts-mode-font-lock-rules-comment
         '((comment) @font-lock-comment-face))

      :language typst
      :feature common
      ,(if typst-ts-mode-font-lock-rules-common
           typst-ts-mode-font-lock-rules-common
         '((shorthand) @typst-ts-shorthand-face
           (ERROR) @typst-ts-error-face))

      :language typst
      :feature markup-basic
      ,(if typst-ts-mode-font-lock-rules-markup-basic
           typst-ts-mode-font-lock-rules-markup-basic
         markup-basic)

      :language typst
      :feature markup-standard
      ,(if typst-ts-mode-font-lock-rules-markup-standard
           typst-ts-mode-font-lock-rules-markup-standard
         '((linebreak) @typst-ts-markup-linebreak-face
           (quote) @typst-ts-markup-quote-face))

      :language typst
      :feature markup-extended
      ,(if typst-ts-mode-font-lock-rules-markup-extended
           typst-ts-mode-font-lock-rules-markup-extended
         '((url) @typst-ts-markup-url-face))

      ;; please note that some feature there also in the math mode
      :language typst
      :feature code-basic
      ,(if typst-ts-mode-font-lock-rules-code-basic
           typst-ts-mode-font-lock-rules-code-basic
         '("#" @typst-ts-code-indicator-face
           ;; "end" @typst-ts-code-indicator-face ;; "end" is nothing but only a indicator
           (string) @font-lock-string-face
           (bool) @font-lock-constant-face
           (none) @font-lock-constant-face
           (auto) @font-lock-constant-face

           (in ["in" "not"] @font-lock-keyword-face)
           (and "and" @font-lock-keyword-face)
           (or "or" @font-lock-keyword-face)
           (not "not" @font-lock-keyword-face)
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

           (call ;; function
            item: (ident) @font-lock-function-call-face)
           (call ;; method
            item: (field field: (ident) @font-lock-function-call-face))
           (tagged field: (ident) @font-lock-variable-name-face)
           (field field: (ident) @font-lock-constant-face)))

      :language typst
      :feature code-standard
      ,(if typst-ts-mode-font-lock-rules-code-standard
           typst-ts-mode-font-lock-rules-code-standard
         '((ident) @font-lock-variable-use-face
           (builtin) @font-lock-builtin-face))

      :language typst
      :feature code-extended
      ,(if typst-ts-mode-font-lock-rules-code-extended
           typst-ts-mode-font-lock-rules-code-extended
         ;; TODO lambda symbol
         '((number) @font-lock-number-face

           (content ["[" "]"] @font-lock-punctuation-face)
           (sign ["+" "-"] @font-lock-operator-face)
           (add "+" @font-lock-operator-face)
           (sub "-" @font-lock-operator-face)
           (mul "*" @font-lock-operator-face)
           (div "/" @font-lock-operator-face)
           (cmp ["==" "<=" ">=" "!=" "<" ">"] @font-lock-operator-face)
           (wildcard) @font-lock-operator-face

           ["(" ")" "{" "}"] @font-lock-punctuation-face
           ["," ";" ".." ":" "sep"] @font-lock-punctuation-face
           "assign" @font-lock-punctuation-face
           (field "." @font-lock-punctuation-face)))

      :language typst
      :feature math-basic
      ,(if typst-ts-mode-font-lock-rules-math-basic
           typst-ts-mode-font-lock-rules-math-basic
         '((math "$" @typst-ts-math-indicator-face)))

      :language typst
      :feature math-standard
      ,(if typst-ts-mode-font-lock-rules-math-standard
           typst-ts-mode-font-lock-rules-math-standard
         '((symbol) @font-lock-constant-face
           (letter) @font-lock-constant-face))

      :language typst
      :feature math-extended
      ,(if typst-ts-mode-font-lock-rules-math-extended
           typst-ts-mode-font-lock-rules-math-extended
         '((fraction "/" @font-lock-operator-face)
           (fac "!" @font-lock-operator-face)
           (attach ["^" "_"] @font-lock-operator-face)
           (align) @font-lock-operator-face)))))

(defconst typst-ts-mode-font-lock-feature-list
  '((comment common)
    (markup-basic code-basic math-basic)
    (markup-standard code-standard math-standard)
    (markup-extended code-extended math-extended)))

(defconst typst-ts-mode--container-node-types
  '("block" "content" "group" "math")
  "Bracket node types.")

(defun typst-ts-mode--node-inside-brackets (parent)
  "Return the bracket node if the PARENT of node is a bracket or inside bracket.
Return nil if the node is not inside brackets."
  (treesit-parent-until
   parent
   (lambda (parent)
     (member (treesit-node-type parent) typst-ts-mode--container-node-types))
   t))

(defun typst-ts-mode--get-node-bol (node)
  "Get the NODE's indentation offset (at node beginning)."
  (save-excursion
    (goto-char (treesit-node-start node))
    (back-to-indentation)
    (point)))

(defun typst-ts-mode--ancestor-in (types &optional return-bol)
  "Return a function to check whether one of the ancestors of a node is in TYPES.
The returned function suits `treesit-simple-indent-rules' Match.
If RETURN-BOL is non-nil, then return returns the beginning of line position of
the corresponding ancestor node that its type is in TYPES, else return the
corresponding ancestor node.  Return nil if ancestor not matching."
  (lambda (_node parent _bol)
    (let* ((query-node parent)
           (ancestor (treesit-parent-until
                      query-node
                      (lambda (parent)
                        (member (treesit-node-type parent) types))
                      t)))
      (if return-bol
          (when ancestor
            (typst-ts-mode--get-node-bol ancestor))
        ancestor))))

(defun typst-ts-mode--ancestor-bol (types)
  "See `typst-ts-mode--ancestor-in'.
TYPES."
  (typst-ts-mode--ancestor-in types t))

(defun typst-ts-mode--identation-item-linebreak (_node _parent bol)
  "Where the current line is underneath a item with linebreak as ending.
Ignore whitespaces.
BOL: beginning of the current line.
See `treesit-simple-indent-rules'."
  (when-let* ((prev-nonwhite-pos (save-excursion
                                   (goto-char bol)
                                   (skip-chars-backward "\s\r\n\t")
                                   (1- (point))))
              ((and (not (eq prev-nonwhite-pos 0))  ; first line
                    (not (eq  ; has previous sibling
                          (line-number-at-pos prev-nonwhite-pos)
                          (line-number-at-pos (point))))))
              (prev-nonwhite-line-node
               (treesit-node-at prev-nonwhite-pos))
              ((equal (treesit-node-type prev-nonwhite-line-node) "linebreak"))
              
              (prev-nonwhite-line-heading-node
               (save-excursion
                 (goto-char prev-nonwhite-pos)
                 (back-to-indentation)
                 (treesit-node-at (point))))
              ((equal (treesit-node-type prev-nonwhite-line-heading-node) "-"))
              
              (prev-nonwhite-line-top-node (treesit-node-parent
                                            prev-nonwhite-line-heading-node)))
    (equal (treesit-node-type prev-nonwhite-line-top-node) "item")))

(defun typst-ts-mode--indentation-item-linebreak-get-pos (_node _parent bol)
  "Get the previous item indentation position.
See `typst-ts-mode--identation-item-linebreak'.
BOL: beginning of the current line.
This function is used instead of `parent-bol' is to make sure in the situation
where current point is point-max with no newline character at ending can also
work well.  Example:
1. el \\$
    2. psy \\$
        | <- insert cursor should be here."
  (save-excursion
    (goto-char bol)
    (skip-chars-backward "\s\r\n\t")
    (back-to-indentation)
    (point)))

(defun typst-ts-mode--indentation-in-section-content (node parent bol)
  "Detect whether current node is inside section > content.
NODE, PARENT, BOL see info node `(elisp) Parser-based Indentation'.
If match, return the bol of the content node."
  (when-let ((container-node
              (funcall (typst-ts-mode--ancestor-in typst-ts-mode--container-node-types)
                       node parent bol)))
    (when (and
           (string= "content" (treesit-node-type container-node))
           (string= "section" (treesit-node-type (treesit-node-parent container-node))))
      (typst-ts-mode--get-node-bol container-node))))

(defvar typst-ts-mode--indent-rules
  ;; debug tips:
  ;; use `typst-ts/util/setup-indent-debug-environment' function in `side/utils.el'
  ;; it basically does these (with some extra trivial stuffs):
  ;; 1. `toggle-debug-on-error' to make sure you indentation code error report
  ;; 2. enable `treesit--indent-verbose' to see what indentation rule matches
  ;; 3. `treesit-inspect-mode' or `treesit-inspect-node-at-point'
  
  ;; `indentation-test.typ' file is used for testing indentation.

  ;; no-node situation: often in insert mode > hit return at the line ending
  ;; `typst-ts-mode-indent-line-function' is created for handling end of buffer
  ;;  edge cases
  `((typst
     ;; ((lambda (node parent bol)  ; NOTE
     ;;    (message "%s %s %s" node parent bol)
     ;;    nil) parent-bol 0)

     ((n-p-gp "section" "source_file" nil) column-0 0)  ; <2>
     
     ((and (node-is ")") (parent-is "group")) parent-bol 0)
     ((and (node-is "}") (parent-is "block")) parent-bol 0)
     ((and (node-is "]") (parent-is "content")) parent-bol 0)
     ;; math - the last "$" notation
     ((match "$" "math" nil 2 2) parent-bol 0)

     ;; code field, example:
     ;; "a b c"
     ;;   .split(" ")
     ((n-p-gp "." "field" nil) parent-bol typst-ts-mode-indent-offset)

     ;; math align, example:
     ;; sum_(k=0)^n k
     ;;   &= 1 + ... + n \
     ((node-is "align") parent-bol typst-ts-mode-indent-offset)

     ;; item - child item
     ((and (node-is "item") (parent-is "item")) parent-bol typst-ts-mode-indent-offset)

     ;; item - previous nonwhite line is item type and the ending is a linebreak
     (typst-ts-mode--identation-item-linebreak
      typst-ts-mode--indentation-item-linebreak-get-pos typst-ts-mode-indent-offset)

     ;; item - item should follow its previous line item's indentation level
     ((lambda (node parent &rest _)
        (save-excursion
          (forward-line -1)
          (back-to-indentation)
          (string= "item" (treesit-node-type
                           (treesit-node-parent
                            (treesit-node-at (point)))))))
      prev-line
      0)

     ;; raw block
     ;; (TODO add indent offset when `typst-ts-mode-enable-raw-blocks-highlight' is t)
     ;; the last "```" notation for raw block
     ((match "```" "raw_blck" nil 3 3) parent-bol 0)
     ((n-p-gp nil "blob" "raw_blck")
      no-indent
      ;; make sure the content indentation is at least as long as raw block header's
      (lambda (_node parent bol)
        (let* ((node-raw-blck (treesit-node-parent parent))
               (raw-block-column (typst-ts-mode-column-at-pos
                                  (typst-ts-mode--get-node-bol node-raw-blck)))
               (bol-column (typst-ts-mode-column-at-pos bol)))
          (if (< bol-column raw-block-column)
              (- raw-block-column bol-column)
            0))))

     ;; section > content > *> any, suitable for no-node
     ;; see <2>, which indents the top level headings
     (typst-ts-mode--indentation-in-section-content
      typst-ts-mode--indentation-in-section-content
      typst-ts-mode-indent-offset-section)

     ;; inside container
     (,(typst-ts-mode--ancestor-in typst-ts-mode--container-node-types)
      ,(typst-ts-mode--ancestor-bol typst-ts-mode--container-node-types)
      typst-ts-mode-indent-offset)

     ((and no-node (parent-is "source_file"))
      prev-line 0)

     ;; TODO to be examined
     (,(typst-ts-mode--ancestor-in '("ERROR")) no-indent 0)

     (no-node parent-bol 0)

     ;; example: (item (text) (text) (text)) when `(text)' is in different line
     (catch-all prev-line 0)))
  "Tree-sitter indent rules for `rust-ts-mode'.")

(defvar typst-ts-mode-indent-function nil
  "This variable shouldn't be customized by user.
It should hold the originally value of `treesit-indent-function'.")

(defun typst-ts-mode-indent (node parent bol)
  "Indent function for `treesit-indent-function'.
This function basically call `typst-ts-mode-indent-function' (i.e. the original
`treesit-indent-function' to indent), and then it checks whether the current
line has a local parser (i.e. raw block with highlight on).  If it has, we
add offset to the line to match the indentation of raw block label.
NODE, PARENT and BOL see `treesit-indent-function'."
  (unless typst-ts-mode-indent-function
    (error "Variable `typst-ts-mode-indent-function' shouldn't be null!"))
  (let ((res (funcall typst-ts-mode-indent-function node parent bol)))
    ;; if it is a highlighted raw block region (i.e. contains at least one local parser)
    (unless (car res) (setcar res (save-excursion (beginning-of-line) (point))))
    (when (treesit-local-parsers-at (treesit-node-start node))
      (let* ((blob_node (treesit-node-at bol 'typst))
             (raw_block_node (treesit-node-parent blob_node))
             (raw_block_bol_column (typst-ts-mode-column-at-pos
                                    (typst-ts-mode--get-node-bol raw_block_node)))
             (cur-line-bol (save-excursion (back-to-indentation) (point)))
             (cur-line-bol-column (typst-ts-mode-column-at-pos cur-line-bol))
             (offset (- raw_block_bol_column cur-line-bol-column)))
        (when (> offset 0)
          (setcdr res raw_block_bol_column))))
    res))

(defun typst-ts-mode-comment-setup()
  "Setup comment related stuffs for `typst-ts-mode'."
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

(defun typst-ts-mode--imenu-function-defintion-p (node)
  "Whether NODE is a function defintion node."
  (let* ((parent-node (treesit-node-parent node))
         (grandparent-node (treesit-node-parent parent-node)))
    (and (equal (treesit-node-type node) "ident")
         (equal (treesit-node-type parent-node) "call")
         (equal (treesit-node-type grandparent-node) "let"))))

(defun typst-ts-mode--imenu-name-function (node)
  "Generate name of NODE for displaying in Imenu."
  (treesit-node-text node))

(defun typst-ts-mode-compile--compilation-finish-function (cur-buffer)
  "For `typst-ts-mode-after-compile-hook' and `compilation-finish-functions'.
CUR-BUFFER: original typst buffer, in case user set
`display-buffer-alist' option for compilation buffer to switch to compilation
buffer before compilation."
  (lambda (compilation-buffer msg)
    (unwind-protect
        (with-current-buffer cur-buffer
          (run-hook-with-args 'typst-ts-mode-after-compile-hook compilation-buffer msg))
      (remove-hook 'compilation-finish-functions
                   (typst-ts-mode-compile--compilation-finish-function cur-buffer)))))

;; outline-minor-mode ================================================================================

(defconst typst-ts-mode-outline-regexp "^[[:space:]]*\\(=+\\) "
  "Regexp identifying Typst header.")

(defun typst-ts-mode-outline-level ()
  "Return the level of the heading at point."
  (save-excursion
    (end-of-line)
    (if (re-search-backward typst-ts-mode-outline-regexp nil t)
	      (- (match-end 1) (match-beginning 1))
      0)))

(defun typst-ts-mode-heading--at-point-p ()
  "Whether the current line is a heading.
Return the heading node when yes otherwise nil."
  (let ((node (treesit-node-parent
	             (treesit-node-at
                (save-excursion
                  (beginning-of-line-text)
                  (point))))))
    (if (string= (treesit-node-type node) "heading")
        node
      nil)))

;;;###autoload
(defun typst-ts-mode-heading-up ()
  "Switch the current heading with the heading above."
  (interactive)
  (typst-ts-mode-meta--dwim 'up))

;;;###autoload
(defun typst-ts-mode-heading-down ()
  "Switch the current heading with the heading below."
  (interactive)
  (typst-ts-mode-meta--dwim 'down))

;;;###autoload
(defun typst-ts-mode-heading-increase ()
  "Increase the heading level."
  (interactive)
  (typst-ts-mode-meta--dwim 'right))

;;;###autoload
(defun typst-ts-mode-heading-decrease ()
  "Decrease heading level."
  (interactive)
  (typst-ts-mode-meta--dwim 'left))

(defun typst-ts-mode-meta--dwim (direction)
  "Do something depending on the context with meta key + DIRECTION.
`left': `typst-ts-mode-heading-decrease',
`right': `typst-ts-mode-heading-increase',
`up': `typst-ts-mode-heading-up',
`down': `typst-ts-mode-heading-down'.
When there is no relevant action to do it will execute the relevant function in
the `GLOBAL-MAP' (example: `right-word')."
  (let ((heading (typst-ts-mode-heading--at-point-p))
	;; car function, cdr string of function for `substitute-command-keys'
	(call-me/string
	 (pcase direction
	   ('left
	    (cons #'outline-promote
		  "\\[typst-ts-mode-heading-decrease]"))
	   ('right
	    (cons #'outline-demote
		  "\\[typst-ts-mode-heading-decrease]"))
	   ('up
	    (cons #'outline-move-subtree-up
		  "\\[typst-ts-mode-heading-up]"))
	   ('down
	    (cons #'outline-move-subtree-down
		  "\\[typst-ts-mode-heading-down]"))
	   (_ (error "%s is not one of: `right' `left'" direction)))))
    (if heading
	(call-interactively (car call-me/string))
      (call-interactively
       (keymap-lookup global-map (substitute-command-keys (cdr call-me/string)))))))

(defun typst-ts-mode-compile ()
  "Compile current typst file."
  (interactive)
  (run-hooks typst-ts-mode-before-compile-hook)

  ;; The reason to take such a awkward solution is that `compilation-finish-functions'
  ;; should be a global variable and also its functions. It doesn't work if we
  ;; define them inside a let binding.
  (add-hook 'compilation-finish-functions
            (typst-ts-mode-compile--compilation-finish-function (current-buffer)))
  (compile
   (format "%s compile %s %s"
           typst-ts-mode-executable-location
           (file-name-nondirectory buffer-file-name)
           typst-ts-mode-compile-options)
   'typst-ts-compilation-mode))

;; RETURN ================================================================================

(defun typst-ts-mode--item-on-line-p ()
  "Does the current line have an item node?
Return the node when yes otherwise
return the node that is one character left from the end of line."
  (treesit-node-parent
   (treesit-node-at
    (save-excursion
      ;; starting from the beginning because line could be 1. wow.
      (beginning-of-line)
      (condition-case nil
          (progn
            (search-forward-regexp (rx (or "+" "-" "."))
                                   (pos-eol)
                                   nil
                                   nil)
            (left-char))
        (search-failed
         ;; need to go to the end of line and then one left because end of line is the next node
         (goto-char (1- (pos-eol)))))
      (point)))))

(defun typst-ts-mode-meta-return (&optional arg)
  "Depending on context, insert a heading or insert an item.
The new heading is created after the ending of current heading.
Using ARG argument will ignore the context and it will insert a heading instead."
  (interactive "P")
  (let ((node (typst-ts-mode--item-on-line-p)))
    (cond
     (arg (typst-ts-mode-insert--heading nil))
     ((string= (treesit-node-type node) "item")
      (typst-ts-mode-insert--item node))
     (t
      (typst-ts-mode-insert--heading node)))))

(defun typst-ts-mode-return (&optional arg)
  "Handle RET depends on condition.
When prefix ARG is non-nil, call `typst-ts-mode-return-function'."
  (interactive "P")
  (let (execute-result node)
    (setq
     execute-result
     (catch 'execute-result
       (when-let* ((cur-pos (point))
                   (cur-node (treesit-node-at cur-pos))
                   (cur-node-type (treesit-node-type cur-node))
                   (parent-node (treesit-node-parent cur-node))  ; could be nil
                   (parent-node-type (treesit-node-type parent-node)))
         (cond
          ;; if provided with a prefix-argument, then do `typst-ts-mode-return-function'
          (arg (throw 'execute-result 'default))
          ;; on item node end
          ((and (eolp)
                (setq node (typst-ts-mode--item-on-line-p))
                (string= (treesit-node-type node) "item"))
           (if (> (treesit-node-child-count node) 1)
               (typst-ts-mode-insert--item node)
             ;; no text means delete the item on current line
             (beginning-of-line)
             (kill-line)
             (indent-according-to-mode))
           (throw 'execute-result 'success))
          ))))
    ;; execute default action if not successful
    (unless (eq execute-result 'success)
      (if (commandp typst-ts-mode-return-function)
          (if (and current-prefix-arg
                   (yes-or-no-p
                    (format
                     "Execute function `%s' with the given prefix argument?"
                     typst-ts-mode-return-function)))
              (call-interactively typst-ts-mode-return-function)
            (let ((current-prefix-arg nil))
              (call-interactively typst-ts-mode-return-function)))
        (funcall typst-ts-mode-return-function)))))

(defun typst-ts-mode-insert--item (node)
  "Insert an item after NODE.
NODE must be an item node!
This function respects indentation."
  (let* (;; +, -, or <num>.
	       (item-type (treesit-node-text
	                   (treesit-node-child node 0)))
         (item-number (string-to-number item-type))
         (item-end (treesit-node-end node))
         (node-bol-column (typst-ts-mode-column-at-pos
                           (typst-ts-mode--get-node-bol node))))
    (goto-char item-end)
    (newline)
    (indent-line-to node-bol-column)
    (insert (if (= item-number 0)
                item-type
              (concat (number-to-string (1+ item-number)) "."))
            " ")))

(defun typst-ts-mode-insert--heading (node)
  "Insert a heading after the section that NODE is part of.
When there is no section it will insert a heading below point."
  (let* ((section
	        (treesit-parent-until
	         node
	         (lambda (node)
	           (string= (treesit-node-type node) "section"))
	         t))
	       ;; first child is heading
	       (heading (treesit-node-child section 0))
	       (heading-level (treesit-node-type (treesit-node-child heading 0))))
    (if section
        (goto-char (treesit-node-end section))
      ;; no headings so far
      (setq heading-level "=")
      (forward-line 1))
    ;; something can be in the next line/section, the heading needs be on its own line
    ;; this has to be done after `goto-char' because it will invalidate the node
    (newline)
    (forward-line -1)
    ;; insert the heading and indent
    (insert heading-level " ")
    (indent-according-to-mode)))

;;;###autoload
(defun typst-ts-mode-preview (file)
  "Open the result compile file.
FILE: file path for the result compile file."
  (interactive (list (concat (file-name-base buffer-file-name) ".pdf")))
  ;; don't use `browse-url-of-file', which cannot open non-english documents
  (browse-url file))

(defun typst-ts-mode-compile-and-preview--compilation-finish-function (cur-buffer)
  "For `typst-ts-mode-compile-and-preview' and `compilation-finish-functions'.
CUR-BUFFER: original typst buffer, in case user set
`display-buffer-alist' option for compilation buffer to switch to compilation
buffer before compilation."
  (lambda (_b _msg)
    (unwind-protect
        (with-current-buffer cur-buffer
          (call-interactively #'typst-ts-mode-preview))
      (remove-hook 'compilation-finish-functions
                   (typst-ts-mode-compile-and-preview--compilation-finish-function cur-buffer)))))

;;;###autoload
(defun typst-ts-mode-compile-and-preview ()
  "Compile & Preview.
Assuming the compile output file name is in default style."
  (interactive)
  ;; use a local variable version of `compilation-finish-functions' to shadow
  ;; global version doesn't work
  (add-hook 'compilation-finish-functions
            (typst-ts-mode-compile-and-preview--compilation-finish-function
             (current-buffer)))
  (typst-ts-mode-compile))

(defun typst-ts-mode--watch-process-filter (proc output)
  "Filter the `typst watch' process output.
Only error will be transported to the process buffer.
See `(info \"(elisp) Filter Functions\")'.
PROC: process; OUTPUT: new output from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (let ((window (get-buffer-window))
            (re (rx bol "error:" (+ not-newline) "\n" (+ blank) "┌─ "
                    (+ not-newline) ":"  ; file
                    (+ num) ":"  ; start-line
                    (+ num) "\n"  ; start-col
                    (+ (+ (or blank num)) "│" (* not-newline) "\n")))
            (next-match-start-pos 0)
            res-output)
        (while (string-match re output next-match-start-pos)
          (setq res-output (concat
                            res-output
                            (when res-output "\n")
                            (substring output (match-beginning 0) (match-end 0)))
                next-match-start-pos (match-end 0)))
        ;; Insert the Error text
        (if (not res-output)
            (when (and typst-ts-mode-display-watch-process-bufer-automatically window)
              (delete-window window))
          (insert res-output)
          (goto-char (point-min))
          (when typst-ts-mode-display-watch-process-bufer-automatically
            (typst-ts-mode-display-watch-buffer)))))))

;;;###autoload
(defun typst-ts-mode-display-watch-buffer ()
  "Display typst watch process buffer."
  (interactive)
  (if (not (buffer-live-p (get-buffer typst-ts-mode-watch-process-buffer-name)))
      (user-error "The typst watch process buffer %s is not alive!" typst-ts-mode-watch-process-buffer-name)
    (display-buffer
     typst-ts-mode-watch-process-buffer-name
     typst-ts-mode-display-watch-process-buffer-parameters)))

;;;###autoload
(defun typst-ts-mode-watch ()
  "Watch(hot compile) current typst file."
  (interactive)
  (run-hooks typst-ts-mode-before-watch-hook)
  (with-current-buffer (get-buffer-create typst-ts-mode-watch-process-buffer-name)
    (erase-buffer)
    (unless (eq major-mode 'typst-ts-compilation-mode)
      (typst-ts-compilation-mode)
      (read-only-mode -1)))
  (set-process-filter
   (start-process-shell-command
    typst-ts-mode-watch-process-name typst-ts-mode-watch-process-buffer-name
    (format "%s watch %s %s"
            typst-ts-mode-executable-location
            (file-name-nondirectory buffer-file-name)
            typst-ts-mode-watch-options))
   'typst-ts-mode--watch-process-filter)
  ;; add mode line indicator
  (when typst-ts-mode-watch-modeline-indicator-enable
    (push
     (propertize typst-ts-mode-watch-modeline-indicator 'face 'typst-ts-watch-modeline-indicator-face)
     global-mode-string))
  (message "Start Watch :3"))

;;;###autoload
(defun typst-ts-mode-watch-stop ()
  "Stop watch process."
  (interactive)
  (delete-process typst-ts-mode-watch-process-name)
  ;; delete associated watch process buffer and window
  (let ((window (get-buffer-window typst-ts-mode-watch-process-buffer-name)))
    (kill-buffer typst-ts-mode-watch-process-buffer-name)
    (when window
      (delete-window window)))
  (run-hooks typst-ts-mode-after-watch-hook)
  ;; remove mode line indicator
  (when typst-ts-mode-watch-modeline-indicator-enable
    (setq global-mode-string (remove typst-ts-mode-watch-modeline-indicator global-mode-string)))
  (message "Stop Watch :‑."))

;;;###autoload
(defun typst-ts-mode-watch-toggle ()
  "Toggle watch process."
  (interactive)
  (if (get-process typst-ts-mode-watch-process-name)
      (typst-ts-mode-watch-stop)
    (typst-ts-mode-watch)))

(defvar typst-ts-compilation-mode-error
  (cons (rx bol "error:" (+ not-newline) "\n" (+ blank) "┌─ "
            (group (+ not-newline)) ":" ;; file
            (group (+ num)) ":"         ;; start-line
            (group (+ num)) "\n")       ;; start-col
        '(1 2 3))
  "Regexp for Error in compilation buffer.")

;;;###autoload
(define-compilation-mode typst-ts-compilation-mode "Typst Compilation"
  "Customized major mode for typst watch compilation."
  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'typst-error typst-ts-compilation-mode-error))
  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'typst-error))

(defun typst-ts-mode-column-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

;;;###autoload
(defun typst-ts-mode-cycle (&optional _arg)
  "Cycle.
Customize `typst-ts-mode-tab-function' for default tab function when no
condition matches."
  (interactive "P")
  (let (execute-result)
    (setq
     execute-result
     ;; plz manually throw `\'success' to `execute-result'
     (catch 'execute-result
       (when-let* ((cur-pos (point))
                   (cur-node (treesit-node-at cur-pos))
                   (cur-node-type (treesit-node-type cur-node))
                   (parent-node (treesit-node-parent cur-node))  ; could be nil
                   (parent-node-type (treesit-node-type parent-node)))
         (cond
          ((equal parent-node-type "raw_blck")
           (insert-tab)
           (throw 'execute-result 'success))
          
          ((or (equal cur-node-type "parbreak")
               (equal parent-node-type "item")
               ;; please turn on whitespace-mode to test the following conditions
               (eobp)
               (eq (point) (1- (point-max))))
           (when-let* ((cur-line-bol
                        (save-excursion
                          (back-to-indentation)
                          (point)))
                       (prev-nonwhite-pos (save-excursion
                                            (goto-char cur-line-bol)
                                            (skip-chars-backward "\s\r\n\t")
                                            (1- (point))))
                       ((and (not (eq prev-nonwhite-pos 0))  ; first line
                             (not (eq  ; has previous sibling
                                   (line-number-at-pos prev-nonwhite-pos)
                                   (line-number-at-pos (point))))))
                       (prev-nonwhite-line-node
                        (treesit-node-at prev-nonwhite-pos))
                       (prev-nonwhite-line-bol
                        ;; TODO typst-ts-mode--get-node-bol
                        (save-excursion
                          (goto-char prev-nonwhite-pos)
                          (back-to-indentation)
                          (point)))
                       (prev-nonwhite-line-heading-node
                        (treesit-node-at prev-nonwhite-line-bol))
                       (prev-nonwhite-line-top-node (treesit-node-parent
                                                     prev-nonwhite-line-heading-node))
                       (cur-line-bol-column (typst-ts-mode-column-at-pos cur-line-bol))
                       (prev-nonwhite-line-bol-column
                        (typst-ts-mode-column-at-pos prev-nonwhite-line-bol)))
             (cond
              ;; 1. el
              ;; 2. psy| <- can toggle indent
              ((and
                (equal (treesit-node-type prev-nonwhite-line-top-node) "item")
                (equal (treesit-node-type prev-nonwhite-line-heading-node) "-")
                ;; previous nonwhite-line ending is not '\' character
                (not (equal (treesit-node-type prev-nonwhite-line-node) "linebreak")))
               ;; TODO cycle all its children
               (let (point)
                 (if (not (eq cur-line-bol-column prev-nonwhite-line-bol-column))
                     (progn
                       (setq point (point))
                       (indent-line-to prev-nonwhite-line-bol-column)
                       (goto-char (- point typst-ts-mode-indent-offset)))
                   (setq point (point))
                   (indent-line-to (+ typst-ts-mode-indent-offset
                                      prev-nonwhite-line-bol-column))
                   (goto-char (+ typst-ts-mode-indent-offset point)))
                 (throw 'execute-result 'success))))))
          (t nil)))))
    ;; execute default action if not successful
    (unless (eq execute-result 'success)
      (if (commandp typst-ts-mode-tab-function)
          (call-interactively typst-ts-mode-tab-function)
        (funcall typst-ts-mode-tab-function)))))

;;;###autoload
(defvar typst-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c c") #'typst-ts-mode-compile-and-preview)
    (define-key map (kbd "C-c C-c C") #'typst-ts-mode-compile)
    (define-key map (kbd "C-c C-c w") #'typst-ts-mode-watch-toggle)
    (define-key map (kbd "C-c C-c p") #'typst-ts-mode-preview)
    (define-key map (kbd "M-<left>") #'typst-ts-mode-heading-decrease)
    (define-key map (kbd "M-<right>") #'typst-ts-mode-heading-increase)
    (define-key map (kbd "M-<down>") #'typst-ts-mode-heading-down)
    (define-key map (kbd "M-<up>") #'typst-ts-mode-heading-up)
    (define-key map (kbd "M-<return>") #'typst-ts-mode-meta-return)
    (define-key map (kbd "<return>") #'typst-ts-mode-return)
    (define-key map (kbd "TAB") #'typst-ts-mode-cycle)
    map))

(defun typst-ts-mode--language-at-point (pos)
  "Get the treesit language should be used at POS.
See `treesit-language-at-point-function'."
  (let ((lang
         (when-let* ((cur-node (treesit-node-at pos 'typst))
                     ((equal (treesit-node-type cur-node) "blob"))
                     (parent-node (treesit-node-parent cur-node))
                     ((equal (treesit-node-type
                              (treesit-node-parent cur-node)) "raw_blck"))
                     (lang-node
                      (treesit-node-prev-sibling cur-node))
                     ((equal (treesit-node-type lang-node) "ident")))
           (gethash
            (downcase (treesit-node-text lang-node))
            typst-ts-els-tag-lang-map))))
    (if lang
        (if (treesit-ready-p lang t) lang nil)
      'typst)))

(defun typst-ts-mode--treesit-range-rules (langs)
  ;; from vimscript-ts-mode.el
  "Create range captures for LANGS."
  (cl-loop for lang in langs
           when (treesit-ready-p lang)
           nconc
           (condition-case err
               (typst-ts-els--treesit-range-rules lang)
             (error
              (message "%s" (error-message-string err))
              nil))))

(defun typst-ts-mode-indent-line-function ()
  "A simple wrapper of `treesit-indent' for handle indentation edge cases.
It is useful to handle end of buffer situation (please turn on `whitespace-mode'
to see that it's actually end of buffer).  Basically, if we are at the end of
buffer, the node, parent passed to our treesit indentation function will be nil,
source_file, which is not desired.
If we are before a '\n' character, then the node and its parent probably are
nil and parbreak."
  (when (eobp)
    (insert "\n")
    (backward-char))
  (treesit-indent))

;;;###autoload
(define-derived-mode typst-ts-mode text-mode "Typst"
  "Major mode for editing Typst, powered by tree-sitter."
  :group 'typst
  :syntax-table typst-ts-mode-syntax-table
  :after-hook
  ;; it seems like the following code only works in this place (after-hook)
  (when (and typst-ts-mode-enable-raw-blocks-highlight
             typst-ts-mode-highlight-raw-blocks-at-startup)
    ;; since currently local parsers haven't created, we cannot only load
    ;; those necessary parsers
    (cl-loop for setting in typst-ts-embedding-lang-settings
             for lang = (car setting)
             for config = (cdr setting)
             when (treesit-ready-p lang t)
             do
             (unwind-protect
                 (typst-ts-els-merge-settings config)
               ;; some feature like cmake-ts-mode will create a parser when
               ;; the feature is required, so we need to clean thease parsers
               (mapc #'treesit-parser-delete (treesit-parser-list nil lang))
               (add-to-list 'typst-ts-els--include-languages lang))))

  (unless (treesit-ready-p 'typst)
    (error "Tree-sitter for Typst isn't available"))

  (let ((parser (treesit-parser-create 'typst)))
    (when typst-ts-mode-enable-raw-blocks-highlight
      (treesit-parser-add-notifier
       parser
       'typst-ts-els-include-dynamically)))

  ;; Comments.
  (typst-ts-mode-comment-setup)

  ;; Electric
  (setq-local
   ;; =: heading and others
   ;; &: math align
   ;; .: code field
   electric-indent-chars (append "{}()[]$=&." electric-indent-chars)
   electric-pair-pairs '((?\" . ?\")
                         (?\{ . ?\})
                         (?\( . ?\))
                         (?\[ . ?\])
                         (?\$ . ?\$)))

  ;; Font Lock
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules (if typst-ts-mode-font-lock-rules
                                                   typst-ts-mode-font-lock-rules
                                                 (typst-ts-mode-font-lock-rules))))
  (setq-local treesit-font-lock-feature-list typst-ts-mode-font-lock-feature-list)

  ;; Indentation
  (setq-local treesit-simple-indent-rules typst-ts-mode--indent-rules)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              `(("Functions" typst-ts-mode--imenu-function-defintion-p nil
                 typst-ts-mode--imenu-name-function)
                ("Headings" "^heading$" nil typst-ts-mode--imenu-name-function)))

  ;; Compile Command
  (ignore-errors
    (format "%s compile %s %s"
            typst-ts-mode-executable-location
            (file-name-nondirectory buffer-file-name)
            typst-ts-mode-compile-options))

  (if (not typst-ts-mode-enable-raw-blocks-highlight)
      (setq-local treesit-range-settings
                  (typst-ts-mode--treesit-range-rules '(typst)))
    (setq-local treesit-language-at-point-function
                'typst-ts-mode--language-at-point)
    (setq-local treesit-range-settings
                (typst-ts-mode--treesit-range-rules
                 (append
                  (cl-loop for setting in typst-ts-embedding-lang-settings
                           when (treesit-ready-p (car setting) t)
                           collect (car setting))
                  '(typst)))))

  ;; Outline
  (if nil  ; (>= emacs-major-version 30)
      ;; FIXME maybe it's a upstream bug. Circle top-level section will cycle all the content below
      (setq treesit-outline-predicate (regexp-opt '("section" "source_file")))
    (setq-local outline-regexp typst-ts-mode-outline-regexp)
    (setq-local outline-level #'typst-ts-mode-outline-level))
  ;; Although without enabling `outline-minor-mode' also works, enabling it
  ;; provides outline ellipsis
  ;; TODO add it to after-hook
  (outline-minor-mode t)
  
  (setq-local typst-ts-mode-indent-function treesit-indent-function
              treesit-indent-function 'typst-ts-mode-indent)
  (treesit-major-mode-setup)

  (setq-local indent-line-function #'typst-ts-mode-indent-line-function))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))

(provide 'typst-ts-mode)

;;; typst-ts-mode.el ends here
