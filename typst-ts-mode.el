;;; typst-ts-mode.el --- Tree Sitter support for Typst  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; Version: 0.8.0
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
  "Number of spaces for each indentation step in `json-ts-mode'."
  :type 'integer
  :group 'typst-ts)

(defcustom typst-ts-mode-executable-location "typst"
  "The location or name(if in `exec-path') for Typst executable."
  :type 'string
  :group 'typst-ts)

(defcustom typst-ts-mode-compile-options ""
  "User defined compile options for `typst-ts-mode-compile'.
The compile options will be passed to the end of
`<typst-executable> compile <current-file>' command."
  :type 'string
  :group 'typst-ts)

(defvar typst-ts-mode-before-compile-hook nil
  "Hook runs after compile.")

(defvar typst-ts-mode-after-compile-hook nil
  "Hook runs after compile.
Note the requirement of this hook is the same as `compilation-finish-functions'.
Also note that this hook runs with typst buffer(the buffer you are editing) as
the current buffer.")

(defcustom typst-ts-mode-watch-options ""
  "User defined compile options for `typst-ts-mode-watch'.
The compile options will be passed to the
`<typst-executable> watch <current-file>' sub-command."
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
  :set-after '(typst-ts-markup-header-same-height)
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
  `(;; Typst font locking
    :language typst
    :feature comment
    ((comment) @font-lock-comment-face)

    :language typst
    :feature common
    ((shorthand) @typst-ts-shorthand-face)

    :language typst
    :feature markup-basic
    (,@(if typst-ts-markup-header-same-height
           '((heading (text)) @typst-ts-markup-header-face)
         '((heading "=" (text)) @typst-ts-markup-header-face-1
           (heading "==" (text)) @typst-ts-markup-header-face-2
           (heading "===" (text)) @typst-ts-markup-header-face-3
           (heading "====" (text)) @typst-ts-markup-header-face-4
           (heading "=====" (text)) @typst-ts-markup-header-face-5))
     (emph) @typst-ts-markup-emphasis-face
     (strong) @typst-ts-markup-strong-face
     (item "-" @typst-ts-markup-item-face)
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
      (ident) :? @typst-ts-markup-rawspan-lang-face
      (blob) @typst-ts-markup-rawblock-blob-face ;; TODO use function to fontify region
      "```" @typst-ts-markup-rawblock-indicator-face)
     (label) @typst-ts-markup-label-face ;; TODO more precise highlight (upstream)
     (ref) @typst-ts-markup-reference-face)

    :language typst
    :feature markup-standard
    ((linebreak) @typst-ts-markup-linebreak-face
     (quote) @typst-ts-markup-quote-face)

    :language typst
    :feature markup-extended
    ((url) @typst-ts-markup-url-face)


    ;; please note that some feature there also in the math mode
    :language typst
    :feature code-basic
    ("#" @typst-ts-code-indicator-face
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
     (field field: (ident) @font-lock-constant-face))

    :language typst
    :feature code-standard
    ((ident) @font-lock-variable-use-face
     (builtin) @font-lock-builtin-face)

    :language typst
    :feature code-extended ;; TODO lambda symbol
    ((number) @font-lock-number-face

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
     (field "." @font-lock-punctuation-face))

    :language typst
    :feature math-basic
    ((math "$" @typst-ts-math-indicator-face))

    :language typst
    :feature math-standard
    ((symbol) @font-lock-constant-face
     (letter) @font-lock-constant-face)

    :language typst
    :feature math-extended
    ((fraction "/" @font-lock-operator-face)
     (fac "!" @font-lock-operator-face)
     (attach ["^" "_"] @font-lock-operator-face)
     (align) @font-lock-operator-face)))

(defconst typst-ts-mode--bracket-node-types
  '("block" "content" "group")
  "Bracket node types.")

(defun typst-ts-mode--node-inside-brackets (parent)
  "Return the bracket node if the PARENT of node is a bracket or inside bracket.
Return nil if the node is not inside brackets."
  (treesit-parent-until
   parent
   (lambda (parent)
     (member (treesit-node-type parent) typst-ts-mode--bracket-node-types))
   t))

(defun typst-ts-mode--ancestor-in (types &optional return-bol)
  "Return a function which is suits `treesit-simple-indent-rules' Match.
An ancestor includes the query node itself.
If RETURN-BOL is non-nil, then return returns the beginning of line position of
the corresponding ancestor node that its type is in TYPES, else return the
corresponding ancestor node.  Return nil if ancestor not matching."
  (lambda (node parent _bol)
    (let* ((query-node (if node ;; considering node may be nil
                           node
                         parent))
           (ancestor (treesit-parent-until
                      query-node
                      (lambda (parent)
                        (member (treesit-node-type parent) types))
                      t)))
      (if return-bol
          (if ancestor
              (save-excursion
                (goto-char (treesit-node-start ancestor))
                (back-to-indentation)
                (point))
            nil)
        ancestor))))

(defun typst-ts-mode--ancestor-bol (types)
  "See `typst-ts-mode--ancestor-in'.
TYPES."
  (typst-ts-mode--ancestor-in types t))

(defconst typst-ts-mode--indent-rules
  ;; you can set `treesit--indent-verbose' variable to t to see which indentation
  ;; rule matches.
  (let ((offset typst-ts-mode-indent-offset))
    `((typst
       ;; ((lambda (node parent bol)
       ;;    (message "%s %s %s" (treesit-node-type node) (treesit-node-type parent) bol)
       ;;    nil) parent-bol 0)

       ((and (node-is ")") (parent-is "group")) parent-bol 0)
       ((and (node-is "}") (parent-is "block")) parent-bol 0)
       ((and (node-is "]") (parent-is "content")) parent-bol 0)

       ((and (node-is "item") (parent-is "item")) parent-bol ,offset)

       ((parent-is "block") parent-bol ,offset)
       ((parent-is "content") parent-bol ,offset)
       ((parent-is "group") parent-bol ,offset)

       ;; don't indent raw block
       ((and no-node ,(typst-ts-mode--ancestor-in (list "raw_blck")))
        no-indent 0)

       ((and no-node
             ,(typst-ts-mode--ancestor-in typst-ts-mode--bracket-node-types))
        ,(typst-ts-mode--ancestor-bol typst-ts-mode--bracket-node-types)
        ,offset)

       ((and no-node
             (not ,(typst-ts-mode--ancestor-in typst-ts-mode--bracket-node-types)))
        parent-bol 0))))
  "Tree-sitter indent rules for `rust-ts-mode'.")

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

;;;###autoload
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
            (re (rx bol "error:" (+ not-newline) "\n   ┌─ "
                    (+ not-newline) ":" ;; file
                    (+ num) ":"         ;; start-line
                    (+ num) "\n"
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
  (message "Stop Watch :‑."))

;;;###autoload
(defun typst-ts-mode-watch-toggle ()
  "Toggle watch process."
  (interactive)
  (if (get-process typst-ts-mode-watch-process-name)
      (typst-ts-mode-watch-stop)
    (typst-ts-mode-watch)))

(defvar typst-ts-compilation-mode-error
  (cons (rx bol "error:" (+ not-newline) "\n   ┌─ "
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

;;;###autoload
(defvar typst-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c c") #'typst-ts-mode-compile-and-preview)
    (define-key map (kbd "C-c C-c C") #'typst-ts-mode-compile)
    (define-key map (kbd "C-c C-c w") #'typst-ts-mode-watch-toggle)
    (define-key map (kbd "C-c C-c p") #'typst-ts-mode-preview)
    map))

;;;###autoload
(define-derived-mode typst-ts-mode text-mode "Typst"
  "Major mode for editing Typst, powered by tree-sitter."
  :group 'typst
  :syntax-table typst-ts-mode-syntax-table

  (unless (treesit-ready-p 'typst)
    (error "Tree-sitter for Typst isn't available"))
  (treesit-parser-create 'typst)

  ;; Comments.
  (typst-ts-mode-comment-setup)

  ;; Electric
  (setq-local
   electric-indent-chars (append "{}()[]$" electric-indent-chars)
   electric-pair-pairs '((?\" . ?\")
                         (?\{ . ?\})
                         (?\( . ?\))
                         (?\[ . ?\])
                         (?\$ . ?\$)))

  ;; Font Lock
  (setq-local treesit-font-lock-level 4)
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules typst-ts-mode-font-lock-rules))
  (setq-local treesit-font-lock-feature-list
              '((comment common)
                (markup-basic code-basic math-basic)
                (markup-standard code-standard math-standard)
                (markup-extended code-extended math-extended)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules typst-ts-mode--indent-rules)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              `(("Functions" typst-ts-mode--imenu-function-defintion-p nil
                 typst-ts-mode--imenu-name-function)
                ("Headings" "^heading$" nil typst-ts-mode--imenu-name-function)))

  ;; Compile Command
  (setq-local compile-command
              (format "%s compile %s %s"
                      typst-ts-mode-executable-location
                      (file-name-nondirectory buffer-file-name)
                      typst-ts-mode-compile-options))

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))

(provide 'typst-ts-mode)

;;; typst-ts-mode.el ends here
