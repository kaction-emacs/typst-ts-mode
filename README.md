# Typst Tree-Sitter Mode

Tree Sitter support for Typst. Minimum Emacs version requirement: 29.  

![Static Badge](https://img.shields.io/badge/Made_with-Emacs-purple)

[Project](https://sr.ht/~meow_king/typst-mode)  
[Home Package for this Package](https://git.sr.ht/~meow_king/typst-ts-mode)  
[Send a Patch](https://lists.sr.ht/~meow_king/dev)  
[Discussion](https://lists.sr.ht/~meow_king/discussion): Questions and Feedback  
[Tickets](https://todo.sr.ht/~meow_king/typst-ts-mode)  
[Public Inbox](https://lists.sr.ht/~meow_king/public-inbox): General Consults

## Requirement

1. Emacs >= 29

2. Latest [Typst](https://github.com/typst/typst).  
Why use the latest? Since Typst is still in its frequent development and the 
tree sitter parser for Typst will also be updated frequently, this package should 
follow their steps. As a result, one user (among many users) should use the 
latest Typst executable to match the tree sitter parser change and the change of this package.  
The version of this package is intentionally set to match the version of Typst release,
so keep an eye out when updating your packages.

3. Tree Sitter parser for Typst: https://github.com/uben0/tree-sitter-typst  

The following script to compile the parser is stolen from 
https://github.com/casouri/tree-sitter-module  

``` shell
git clone --depth=1 --single-branch -b master git@github.com:uben0/tree-sitter-typst.git

cd tree-sitter-typst/src
cc -fPIC -c -I. parser.c

# Compile scanner.c.
if test -f scanner.c
then
    cc -fPIC -c -I. scanner.c
fi

# Compile scanner.cc.
if test -f scanner.cc
then
    c++ -fPIC -I. -c scanner.cc
fi

# Link.
if test -f scanner.cc
then
    c++ -fPIC -shared *.o -o "libtree-sitter-typst.so"
else
    cc -fPIC -shared *.o -o "libtree-sitter-typst.so"
fi
```
Then move the resulting file `libtree-sitter-typst.so` to the directory you put
for tree sitter parsers. For more information, you should take a look at `(info "(elisp) Language Grammar")` (eval this expression in Emacs).

## Installation

For reference, this is my configuration.
``` emacs-lisp
(use-package typst-ts-mode
  :straight (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open"))
```

## Keys

`C-c C-c c` : `typst-ts-mode-compile-and-preview`  
`C-c C-c C` : `typst-ts-mode-compile`  
`C-c C-c w` : `typst-ts-mode-watch-toggle`  
`C-c C-c p` : `typst-ts-mode-preview`  

## Customization Options

`customize` -> `typst-ts`

For some options you may find useful:
1. `typst-ts-mode-indent-offset`
2. `typst-ts-mode-executable-location`
3. `typst-ts-mode-watch-options`. Set this to `--open` so typst will open the compiled
file for you.
4. `typst-ts-mode-compile-options`. Note that setting `--open` has no use for this
customization variable. What `--open` do in Typst process is to spawn a new thread 
to open the compiled file with default application corresponding to each platform. With its
main process finished,  all Typst's sub-process will be killed by Emacs (This is my assumption,
for more information, you may have to dive into [crate - open](https://crates.io/crates/open)).
You can try shell command `typst compile <file> --open && sleep 1` in Emacs to see what happened. 
5. `typst-ts-mode-display-watch-process-bufer-automatically`. This is set to `t` by
default, so the `typst watch` process buffer appear when an error occurs, and disappear
when there is no error. You may find there is only one error at a time, and it is
because Typst itself do this style. You may find [auto-save](https://github.com/manateelazycat/auto-save)
and [super-save](https://github.com/bbatsov/super-save) useful (or annoying).

### Consult Imenu Integration
If you use [consult](https://github.com/minad/consult) and use `consult-iemnu`
command, you way want this setting.
``` emacs-lisp
(setq
 consult-imenu-config
 (append consult-imenu-config
         '((typst-ts-mode :topLevel "Headings" :types
                          ((?h "Headings" typst-ts-markup-header-face)
                           (?f "Functions" font-lock-function-name-face))))))
```

## Do you want to develop a new Tree Sitter Major Mode?

Here are some resources:
- [Let’s Write a Tree-Sitter Major Mode - Matering Emacs](https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode)
- [ts-query-highlight](https://sr.ht/~meow_king/ts-query-highlight/)
- [combobulate](https://github.com/mickeynp/combobulate)
