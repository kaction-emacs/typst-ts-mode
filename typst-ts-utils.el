;;; typst-ts-utils.el --- utility functions for typst-ts-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

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

;; Utility functions for typst-ts-mode

;;; Code:

(require 'treesit)

;; code is from treesit.el inside Emacs Source
(defun typst-ts-utils-local-parsers-at (&optional pos language with-host)
  "Return all the local parsers at POS.
It's a copy of Emacs 30's `treesit-local-parsers-at' function.
POS LANGUAGE WITH-HOST."
  (if (>= emacs-major-version 30)
      (funcall #'treesit-local-parsers-at pos language with-host)
    (let ((res nil))
      (dolist (ov (overlays-at (or pos (point))))
        (when-let ((parser (overlay-get ov 'treesit-parser))
                   (host-parser (overlay-get ov 'treesit-host-parser)))
          (when (or (null language)
                    (eq (treesit-parser-language parser)
                        language))
            (push (if with-host (cons parser host-parser) parser) res))))
      (nreverse res))))

;; code is from treesit.el inside Emacs Source
(defun typst-ts-utils-local-parsers-on (&optional beg end language with-host)
  "Return all the local parsers between BEG END.
It's a copy of Emacs 30's `treesit-local-parsers-on' function.
BEG END LANGUAGE WITH-HOST."
  (if (>= emacs-major-version 30)
      (funcall #'treesit-local-parsers-on beg end language with-host)
    (let ((res nil))
      (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
        (when-let ((parser (overlay-get ov 'treesit-parser))
                   (host-parser (overlay-get ov 'treesit-host-parser)))
          (when (or (null language)
                    (eq (treesit-parser-language parser)
                        language))
            (push (if with-host (cons parser host-parser) parser) res))))
      (nreverse res))))

(defun typst-ts-utils-node-get (node instructions)
  "Get things from NODE by INSTRUCTIONS.
It's a copy of Emacs 30's `treesit-node-get' function."
  (declare (indent 1))
  (if (>= emacs-major-version 30)
      (treesit-node-get node instructions)
    (while (and node instructions)
      (pcase (pop instructions)
        ('(field-name) (setq node (treesit-node-field-name node)))
        ('(type) (setq node (treesit-node-type node)))
        (`(child ,idx ,named) (setq node (treesit-node-child node idx named)))
        (`(parent ,n) (dotimes (_ n)
                        (setq node (treesit-node-parent node))))
        (`(text ,no-property) (setq node (treesit-node-text node no-property)))
        (`(children ,named) (setq node (treesit-node-children node named)))
        (`(sibling ,step ,named)
         (dotimes (_ (abs step))
           (setq node (if (> step 0)
                          (treesit-node-next-sibling node named)
                        (treesit-node-prev-sibling node named)))))))
    node))

(provide 'typst-ts-utils)

;;; typst-ts-utils.el ends here
