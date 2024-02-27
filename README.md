# Typst Tree-Sitter Mode

Tree Sitter support for Typst. Minimum Emacs version requirement: 29.  

![Static Badge](https://img.shields.io/badge/Made_with-Emacs-purple)

[Discussion on Zulip](https://meow-place.zulipchat.com/)  
[Tickets](https://todo.sr.ht/~meow_king/typst-ts-mode): Issues, feature requests, etc.  
[Announce](https://lists.sr.ht/~meow_king/typst-ts-mode-announce) Announcements. Subscribe to it to receive the latest news for `typst-ts-mode`.  
[Send a Patch](https://lists.sr.ht/~meow_king/typst-ts-mode-dev)  

## Requirement

1. Emacs >= 29

2. Latest [Typst](https://github.com/typst/typst).  
`typst 0.10.0 (70ca0d25)`

3. Tree Sitter grammar for Typst: https://github.com/uben0/tree-sitter-typst  
   commit: `3a6c81bb56082604f5e63834cb2f946beeda1e76 - Feb 7, 2024`  
   To install the grammar, you can execute the following elisp code:  

   ``` emacs-lisp
   (add-to-list 'treesit-language-source-alist
                '(typst "https://github.com/uben0/tree-sitter-typst"))
   (treesit-install-language-grammar 'typst)
   ```

    Note there are also [tree sitter modules](https://github.com/casouri/tree-sitter-module), [treesit-auto](https://github.com/renzmann/treesit-auto) and [treesit-langs](https://github.com/kiennq/treesit-langs) for you to easily manage your tree sitter modes and grammars.

## Installation

For reference, this is my configuration.
``` emacs-lisp
(use-package typst-ts-mode
  :elpaca (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files ("*.el"))
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")
  
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))
```

## Keys

`C-c C-c c` : `typst-ts-mode-compile-and-preview`  
`C-c C-c C` : `typst-ts-mode-compile`  
`C-c C-c w` : `typst-ts-mode-watch-toggle`  
`C-c C-c p` : `typst-ts-mode-preview`  
`M-<left>`  : `typst-ts-mode-heading-decrease`  
`M-<right>` : `typst-ts-mode-heading-increase`  
`M-<up>`    : `typst-ts-mode-heading-up`  
`M-<down>`  : `typst-ts-mode-heading-down`  
`TAB`       : `typst-ts-mode-cycle`  
`M-<return>`: `typst-ts-mode-meta-return`  
`<return>`  : `typst-ts-mode-return`  

*NOTE*: `outline-minor-mode` is enabled by `typst-ts-mode`, so you can use comamnd 
defined by `outline-minor-mode` such as `outline-cycle`.

## Customization Options

For customizable options: `customize` -> `typst-ts`.  

Here are some options you may find useful:  
1. `typst-ts-mode-indent-offset` (default 4) and `typst-ts-mode-indent-offset-section` (default 2)  
   Set `typst-ts-mode-indent-offset-section` to 0 to make the structure flat.
2. `typst-ts-mode-executable-location`  
3. `typst-ts-mode-watch-options`.  
   Set this to `--open` so typst will open the compiled file for you.
4. `typst-ts-mode-compile-options`.  
   Note that setting `--open` has no use for this customization variable. 
   You can execute the shell command `typst compile <file> --open && sleep 1`
   to view what is happening. 
5. `typst-ts-mode-display-watch-process-bufer-automatically`. (default `t`)  
   so the `typst watch` process buffer appear when an error occurs, and 
   disappear when there is no error. You may find sometimes there is only one 
   error at a time, and it is because Typst itself do this style. 
   You may find `auto-save-visited-mode`,
   [auto-save](https://github.com/manateelazycat/auto-save) or 
   [super-save](https://github.com/bbatsov/super-save) useful (or annoying).
6. `typst-ts-mode-before-compile-hook` and `typst-ts-mode-after-compile-hook`  
7. `typst-ts-mode-return-autoincrement` autoincrement lists when pressing RETURN (default `t`).

### Fontification
1. `typst-ts-mode-fontification-precise-level` (default `'middle`)  
   Available values: `min`, `middle` and `max`. Different level affects the precision
   of the fontification. For example, `- item`, we may fontify the whole expression
   using one face, or two faces. Note it is related to the performance of fontification
   process, especially the first fontification process (when you open the file).  
2. `typst-ts-markup-header-same-height` and `typst-ts-markup-header-scale`  
   Control header height. Note that it only works when `typst-ts-mode-fontification-precise-level`
   is set to `max`.
3. Override default font lock rules  
   Please see the documentation of `typst-ts-mode-font-lock-rules`, you can find 
   how to override the whole font lock rules or only small part of the font lock
   rules.

### Raw block highlighting
_This is an experimental feature_  
Only support tree-sitter languages.  
For more detailed documentation about raw block highlighting see 
[this documentation](./doc/raw-block-highlighing.md)  
1. `typst-ts-mode-enable-raw-blocks-highlight` (default `nil`)  
2. `typst-ts-mode-highlight-raw-blocks-at-startup` (default `nil`)  
3. `typst-ts-highlight-raw-block-langs-not-in-predefined-settings` (default `t`)  

### Consult Imenu Integration
If you use `consult-iemnu`
command [consult](https://github.com/minad/consult), you way want this setting.
``` emacs-lisp
(setq
 consult-imenu-config
 (append consult-imenu-config
         '((typst-ts-mode :topLevel "Headings" :types
                          ((?h "Headings" typst-ts-markup-header-face)
                           (?f "Functions" font-lock-function-name-face))))))
```

## Contribute

1. please work on `develop` branch, which will be combined into `main` branch every one week or so.

## Co-Maintainer

[Huan Nguyen](https://sr.ht/~huan)

## Do you want to develop a new Tree Sitter Major Mode?

Here are some resources:
- [Let’s Write a Tree-Sitter Major Mode - Matering Emacs](https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode)
- [ts-query-highlight](https://sr.ht/~meow_king/ts-query-highlight/)  
  I wrote this package to highlight tree sitter queries. I use this package to help
developing `typst-ts-mode`.
- [combobulate](https://github.com/mickeynp/combobulate)
