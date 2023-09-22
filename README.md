# typst-ts-mode

![Static Badge](https://img.shields.io/badge/Made_with-Emacs-purple)

[Project](https://git.sr.ht/~meow_king/typst-ts-mode)
[Public Inbox](https://lists.sr.ht/~meow_king/public-inbox): General Consults
[Sending a Patch](https://lists.sr.ht/~meow_king/dev)
[Discussion](https://lists.sr.ht/~meow_king/discussion): Questions and Feedback
[Tickets](https://todo.sr.ht/~meow_king/typst-ts-mode)

https://github.com/uben0/tree-sitter-typst

``` emacs-lisp
(setq
 consult-imenu-config (append consult-imenu-config
                              '((typst-ts-mode :topLevel "Headings" :types
                                               ((?h "Headings" typst-ts-markup-header-face)
                                                (?f "Functions" font-lock-function-name-face))))))
```
