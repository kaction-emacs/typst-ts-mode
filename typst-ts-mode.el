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

(defgroup typst nil
  "Typst Writing."
  :prefix "typst-"
  :group 'text
  :group 'languages)

(defcustom typst-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `json-ts-mode'."
  :type 'integer
  :group 'typst)


;;;###autoload
(define-derived-mode typst-ts-mode text-mode "Typst"
  "Major mode for editing Typst, powered by tree-sitter."
  :group 'typst
  ;; :syntax-table typst-ts-mode--syntax-table

  (unless (treesit-ready-p 'typst)
    (error "Tree-sitter for Typst isn't available"))

  (treesit-parser-create 'typst)
  
  (if (treesit-ready-p 'typst)
      ;; TODO what if typst-mode exist?
      (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode)))
  (treesit-major-mode-setup))

(provide 'typst-ts-mode)
