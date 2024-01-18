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

;; NOTE:
;; Raw block 'typc' tag cannot work as expected in typst code mode, it works in
;; typst markup mode

;;; Code:
(require 'treesit)

(defcustom typst-ts-highlight-raw-block-langs-not-in-predefined-settings t
  "Whether to highlight raw block of language that is not in settings.
i.e. not in `typst-ts-embedding-lang-settings'.
The cost for setting up other languages is usually higher than those
languages in settings."
  :type 'boolean
  :group 'typst-ts)

(defvar typst-ts-els-lang-tags-map
  #s(hash-table
     size 182
     test equal
     data
     (
      .git (".git" )
      .mailmap (".mailmap" "mailmap" )
      actionscript ("as" "actionscript" )
      ada ("adb" "ads" "gpr" "ada" )
      adoc ("adoc" "ad" "asciidoc" )
      adp ("adp" )
      applescript ("applescript" "script editor" )
      asp ("asa" "asp" "asp" )
      attributes ("attributes" "gitattributes" ".gitattributes" )
      authorized_keys ("authorized_keys" "pub" "authorized_keys2" )
      awk ("awk" )
      bash ("sh" "bash" "zsh" "ash" ".bash_aliases" ".bash_completions" ".bash_functions" ".bash_login" ".bash_logout" ".bash_profile" ".bash_variables" ".bashrc" ".profile" ".textmate_init" ".zlogin" ".zlogout" ".zprofile" ".zshenv" ".zshrc" "pkgbuild" "ebuild" "eclass" )
      bat ("bat" "cmd" )
      bibtex ("bib" "bibtex" )
      build ("build" )
      c ("c" "h" )
      c-sharp ("cs" "csx" "c#" )
      cabal ("cabal" )
      camlp4 ("camlp4" )
      clojure ("clj" "cljc" "cljs" "edn" "clojure" )
      cmake ("cmakelists.txt" "cmake" )
      cmakecache ("cmakecache.txt" "cmakecache" )
      cmakecommands ("cmakecommands" )
      coffeescript ("coffee" "cakefile" "coffee.erb" "cson" "coffeescript" )
      commands-builtin-shell-bash ("commands-builtin-shell-bash" )
      commit_editmsg ("commit_editmsg" "merge_msg" "tag_editmsg" )
      commonlisp ("clisp" )
      cpp ("cpp" "cc" "cp" "cxx" "c++" "c" "h" "hh" "hpp" "hxx" "h++" "inl" "ipp" )
      cpuinfo ("cpuinfo" )
      crontab ("tab" "crontab" "cron.d" )
      crystal ("cr" "crystal" )
      css ("css" "css.erb" "css.liquid" )
      csv ("csv" "tsv" )
      d ("d" "di" )
      dart ("dart" )
      diff ("diff" "patch" )
      dockerfile ("dockerfile" "dockerfile" )
      dot ("dot" "dot" "gv" )
      dotenv (".env" ".env.dist" ".env.local" ".env.sample" ".env.example" ".env.template" ".env.test" ".env.test.local" ".env.testing" ".env.dev" ".env.development" ".env.development.local" ".env.prod" ".env.production" ".env.production.local" ".env.dusk.local" ".env.staging" ".env.default" ".env.defaults" ".envrc" ".flaskenv" "env" "env.example" "env.sample" "env.template" "dotenv" )
      elixir ("ex" "exs" "elixir" )
      elm ("elm" )
      email ("eml" "msg" "mbx" "mboxz" "email" )
      envvars ("envvars" "htaccess" "htaccess" "htgroups" "htgroups" "htpasswd" "htpasswd" ".htaccess" ".htaccess" ".htgroups" ".htgroups" ".htpasswd" ".htpasswd" )
      erbsql ("erbsql" "sql.erb" )
      erlang ("erl" "hrl" "emakefile" "emakefile" "escript" "erlang" )
      exclude ("exclude" "gitignore" ".gitignore" )
      f ("f" "f" "f77" "f77" "for" "for" "fpp" "fpp" )
      f90 ("f90" "f90" "f95" "f95" "f03" "f03" "f08" "f08" )
      fish ("fish" )
      fs ("fs" "fsi" "fsx" "f#" )
      fstab ("fstab" "crypttab" "mtab" )
      git-rebase-todo ("git-rebase-todo" )
      gitconfig ("gitconfig" ".gitconfig" ".gitmodules" )
      gitlog ("gitlog" )
      glsl ("vs" "fs" "gs" "vsh" "fsh" "gsh" "vshader" "fshader" "gshader" "vert" "frag" "geom" "tesc" "tese" "comp" "glsl" "mesh" "task" "rgen" "rint" "rahit" "rchit" "rmiss" "rcall" )
      gnuplot ("gp" "gpl" "gnuplot" "gnu" "plot" "plt" )
      go ("go" )
      graphql ("graphql" "graphqls" "gql" )
      groff/troff ("groff" "troff" "1" "2" "3" "4" "5" "6" "7" "8" "9" "groff/troff" )
      groovy ("groovy" "gvy" "gradle" "jenkinsfile" )
      group ("group" )
      h.in ("h.in" )
      haml ("haml" "sass" )
      haskell ("hs" "haskell" )
      hh.in ("hh.in" "hpp.in" "hxx.in" "h++.in" )
      hosts ("hosts" )
      htm.j2 ("htm.j2" "html.j2" "xhtml.j2" "xml.j2" )
      html ("html" "htm" "shtml" "xhtml" )
      html.eex ("html.eex" "html.leex" )
      http ("http" )
      ini ("ini" "ini" "inf" "inf" "reg" "reg" "lng" "cfg" "cfg" "desktop" "url" "url" ".editorconfig" ".coveragerc" ".pylintrc" ".gitlint" ".hgrc" "hgrc" )
      java ("java" "bsh" )
      javadoc ("javadoc" )
      javascript ("js" "htc" "javascript" )
      jinja2 ("j2" "jinja2" "jinja" )
      jq ("jq" )
      js.erb ("js.erb" )
      json ("json" "sublime-settings" "sublime-menu" "sublime-keymap" "sublime-mousemap" "sublime-theme" "sublime-build" "sublime-project" "sublime-completions" "sublime-commands" "sublime-macro" "sublime-color-scheme" "ipynb" "pipfile.lock" )
      jsonnet ("jsonnet" "libsonnet" "libjsonnet" )
      jsp ("jsp" )
      julia ("jl" "julia" )
      known_hosts ("known_hosts" "known_hosts.old" )
      kotlin ("kt" "kts" "kotlin" )
      latex ("tex" "ltx" "latex" )
      lean ("lean" )
      less ("less" "css.less" )
      lhs ("lhs" )
      lisp ("lisp" "cl" "l" "mud" "el" "scm" "ss" "lsp" "fasl" )
      llvm ("ll" "llvm" )
      log ("log" )
      lua ("lua" )
      makefile ("make" "gnumakefile" "makefile" "makefile" "makefile.am" "makefile.am" "makefile.in" "makefile.in" "ocamlmakefile" "mak" "mk" )
      manpage ("man" "manpage" )
      markdown ("md" "mdown" "markdown" "markdn" )
      matlab ("matlab" )
      mediawikerpanel ("mediawikerpanel" )
      mediawiki ("mediawiki" "wikipedia" "wiki" )
      meminfo ("meminfo" )
      multimarkdown ("multimarkdown" )
      namelist ("namelist" )
      nginx ("conf.erb" "conf" "nginx.conf" "mime.types" "fastcgi_params" "scgi_params" "uwsgi_params" "nginx" )
      nim ("nim" "nims" "nimble" )
      ninja ("ninja" )
      nix ("nix" )
      objective-c ("m" "h" "objective-c" )
      objective-c++ ("mm" "m" "h" "objective-c++" )
      ocaml ("ml" "mli" "ocaml" )
      ocamllex ("mll" "ocamllex" )
      ocamlyacc ("mly" "ocamlyacc" )
      orgmode ("org" "orgmode" )
      pascal ("pas" "p" "dpr" "pascal" )
      passwd ("passwd" )
      pb.txt ("pb.txt" "proto.text" "textpb" "pbtxt" "prototxt" )
      perl ("pl" "pc" "pm" "pmc" "pod" "t" "perl" )
      php ("php" "php3" "php4" "php5" "php7" "phps" "phpt" "phtml" )
      properties ("properties" )
      proto ("proto" "protodevel" )
      puppet ("pp" "epp" "puppet" )
      purescript ("purs" "purescript" )
      python ("py" "py3" "pyw" "pyi" "pyx" "pyx.in" "pxd" "pxd.in" "pxi" "pxi.in" "rpy" "cpy" "sconstruct" "sconstruct" "sconstruct" "sconscript" "gyp" "gypi" "snakefile" "vpy" "wscript" "bazel" "bzl" "python" )
      qml ("qml" "qmlproject" )
      r ("r" "r" "rprofile" )
      racket ("rkt" "racket" )
      rails ("rails" "rhtml" "erb" "html.erb" )
      rd ("rd" )
      re ("re" )
      rego ("rego" )
      requirements.txt ("requirements.txt" "requirements.in" "pip" )
      resolv ("resolv.conf" "resolv" )
      restructuredtext ("rst" "rest" "restructuredtext" )
      robot ("robot" "resource" )
      ruby ("rb" "appfile" "appraisals" "berksfile" "brewfile" "capfile" "cgi" "cheffile" "config.ru" "deliverfile" "fastfile" "fcgi" "gemfile" "gemspec" "guardfile" "irbrc" "jbuilder" "podfile" "podspec" "prawn" "rabl" "rake" "rakefile" "rantfile" "rbx" "rjs" "ruby.rail" "scanfile" "simplecov" "snapfile" "thor" "thorfile" "vagrantfile" "ruby" )
      rust ("rs" "rust" )
      rxml ("rxml" "builder" )
      scala ("scala" "sbt" "sc" )
      scss ("scss" )
      shell-unix-generic ("shell-unix-generic" )
      show-nonprintable ("show-nonprintable" )
      slim ("slim" "skim" )
      sml ("sml" "cm" "sig" )
      solidity ("sol" "solidity" )
      sql ("sql" "ddl" "dml" )
      ssh_config ("ssh_config" )
      sshd_config ("sshd_config" )
      strace ("strace" )
      stylus ("styl" "stylus" )
      svelte ("svlt" "svelte" )
      swift ("swift" )
      syslog ("syslog" )
      systemverilog ("sv" "v" "svh" "vh" "systemverilog" )
      tcl ("tcl" )
      terraform ("tf" "tfvars" "hcl" "terraform" )
      tex ("sty" "cls" "tex" )
      textile ("textile" )
      tfstate ("tfstate" )
      todo.txt ("todo.txt" "done.txt" )
      toml ("toml" "tml" "cargo.lock" "gopkg.lock" "pipfile" "pdm.lock" "poetry.lock" )
      twig ("twig" "html.twig" )
      txt ("txt" )
      typescript ("ts" "mts" "cts" "typescript" )
      typescriptreact ("tsx" "typescriptreact" )
      typst ("typ" "typc" "typst" )
      varlink ("varlink" )
      verilog ("v" "v" "verilog" )
      viml ("vim" "vimrc" "gvimrc" ".vimrc" ".gvimrc" "_vimrc" "_gvimrc" "viml" )
      vue ("vue" )
      vyper ("vy" "vyper" )
      xml ("xml" "xsd" "xslt" "tld" "dtml" "rng" "rss" "opml" "svg" "xaml" )
      yaml ("yaml" "yml" "sublime-syntax" )
      yasm ("yasm" "nasm" "asm" "inc" "mac" )
      yaws ("yaws" )
      zig ("zig" )

      ))
  "Tree sitter language -> Raw block tags map.
Associated map: `typst-ts-els-tag-lang-map'.
Please use function `typst-ts-els--add-treesit-range-rules' and
`typst-ts-els--lang-name-remap' to modify them.")

(defvar typst-ts-els-tag-lang-map
  #s(hash-table
     size 626
     test equal
     data
     (
      ".git" .git ".mailmap" .mailmap "mailmap" .mailmap
      "as" actionscript "actionscript" actionscript "adb" ada
      "ads" ada "gpr" ada "ada" ada
      "adoc" adoc "ad" adoc "asciidoc" adoc
      "adp" adp "applescript" applescript "script editor" applescript
      "asa" asp "asp" asp "asp" asp
      "attributes" attributes "gitattributes" attributes ".gitattributes" attributes
      "authorized_keys" authorized_keys "pub" authorized_keys "authorized_keys2" authorized_keys
      "awk" awk "sh" bash "bash" bash
      "zsh" bash "ash" bash ".bash_aliases" bash
      ".bash_completions" bash ".bash_functions" bash ".bash_login" bash
      ".bash_logout" bash ".bash_profile" bash ".bash_variables" bash
      ".bashrc" bash ".profile" bash ".textmate_init" bash
      ".zlogin" bash ".zlogout" bash ".zprofile" bash
      ".zshenv" bash ".zshrc" bash "pkgbuild" bash
      "ebuild" bash "eclass" bash "bat" bat
      "cmd" bat "bib" bibtex "bibtex" bibtex
      "build" build "c" c "h" c
      "cs" c-sharp "csx" c-sharp "c#" c-sharp
      "cabal" cabal "camlp4" camlp4 "clj" clojure
      "cljc" clojure "cljs" clojure "edn" clojure
      "clojure" clojure "cmakelists.txt" cmake "cmake" cmake
      "cmakecache.txt" cmakecache "cmakecache" cmakecache "cmakecommands" cmakecommands
      "coffee" coffeescript "cakefile" coffeescript "coffee.erb" coffeescript
      "cson" coffeescript "coffeescript" coffeescript "commands-builtin-shell-bash" commands-builtin-shell-bash
      "commit_editmsg" commit_editmsg "merge_msg" commit_editmsg "tag_editmsg" commit_editmsg
      "clisp" commonlisp "cpp" cpp "cc" cpp
      "cp" cpp "cxx" cpp "c++" cpp
      "c" cpp "h" cpp "hh" cpp
      "hpp" cpp "hxx" cpp "h++" cpp
      "inl" cpp "ipp" cpp "cpuinfo" cpuinfo
      "tab" crontab "crontab" crontab "cron.d" crontab
      "cr" crystal "crystal" crystal "css" css
      "css.erb" css "css.liquid" css "csv" csv
      "tsv" csv "d" d "di" d
      "dart" dart "diff" diff "patch" diff
      "dockerfile" dockerfile "dockerfile" dockerfile "dot" dot
      "dot" dot "gv" dot ".env" dotenv
      ".env.dist" dotenv ".env.local" dotenv ".env.sample" dotenv
      ".env.example" dotenv ".env.template" dotenv ".env.test" dotenv
      ".env.test.local" dotenv ".env.testing" dotenv ".env.dev" dotenv
      ".env.development" dotenv ".env.development.local" dotenv ".env.prod" dotenv
      ".env.production" dotenv ".env.production.local" dotenv ".env.dusk.local" dotenv
      ".env.staging" dotenv ".env.default" dotenv ".env.defaults" dotenv
      ".envrc" dotenv ".flaskenv" dotenv "env" dotenv
      "env.example" dotenv "env.sample" dotenv "env.template" dotenv
      "dotenv" dotenv "ex" elixir "exs" elixir
      "elixir" elixir "elm" elm "eml" email
      "msg" email "mbx" email "mboxz" email
      "email" email "envvars" envvars "htaccess" envvars
      "htaccess" envvars "htgroups" envvars "htgroups" envvars
      "htpasswd" envvars "htpasswd" envvars ".htaccess" envvars
      ".htaccess" envvars ".htgroups" envvars ".htgroups" envvars
      ".htpasswd" envvars ".htpasswd" envvars "erbsql" erbsql
      "sql.erb" erbsql "erl" erlang "hrl" erlang
      "emakefile" erlang "emakefile" erlang "escript" erlang
      "erlang" erlang "exclude" exclude "gitignore" exclude
      ".gitignore" exclude "f" f "f" f
      "f77" f "f77" f "for" f
      "for" f "fpp" f "fpp" f
      "f90" f90 "f90" f90 "f95" f90
      "f95" f90 "f03" f90 "f03" f90
      "f08" f90 "f08" f90 "fish" fish
      "fs" fs "fsi" fs "fsx" fs
      "f#" fs "fstab" fstab "crypttab" fstab
      "mtab" fstab "git-rebase-todo" git-rebase-todo "gitconfig" gitconfig
      ".gitconfig" gitconfig ".gitmodules" gitconfig "gitlog" gitlog
      "vs" glsl "fs" glsl "gs" glsl
      "vsh" glsl "fsh" glsl "gsh" glsl
      "vshader" glsl "fshader" glsl "gshader" glsl
      "vert" glsl "frag" glsl "geom" glsl
      "tesc" glsl "tese" glsl "comp" glsl
      "glsl" glsl "mesh" glsl "task" glsl
      "rgen" glsl "rint" glsl "rahit" glsl
      "rchit" glsl "rmiss" glsl "rcall" glsl
      "gp" gnuplot "gpl" gnuplot "gnuplot" gnuplot
      "gnu" gnuplot "plot" gnuplot "plt" gnuplot
      "go" go "graphql" graphql "graphqls" graphql
      "gql" graphql "groff" groff/troff "troff" groff/troff
      "1" groff/troff "2" groff/troff "3" groff/troff
      "4" groff/troff "5" groff/troff "6" groff/troff
      "7" groff/troff "8" groff/troff "9" groff/troff
      "groff/troff" groff/troff "groovy" groovy "gvy" groovy
      "gradle" groovy "jenkinsfile" groovy "group" group
      "h.in" h.in "haml" haml "sass" haml
      "hs" haskell "haskell" haskell "hh.in" hh.in
      "hpp.in" hh.in "hxx.in" hh.in "h++.in" hh.in
      "hosts" hosts "htm.j2" htm.j2 "html.j2" htm.j2
      "xhtml.j2" htm.j2 "xml.j2" htm.j2 "html" html
      "htm" html "shtml" html "xhtml" html
      "html.eex" html.eex "html.leex" html.eex "http" http
      "ini" ini "ini" ini "inf" ini
      "inf" ini "reg" ini "reg" ini
      "lng" ini "cfg" ini "cfg" ini
      "desktop" ini "url" ini "url" ini
      ".editorconfig" ini ".coveragerc" ini ".pylintrc" ini
      ".gitlint" ini ".hgrc" ini "hgrc" ini
      "java" java "bsh" java "javadoc" javadoc
      "js" javascript "htc" javascript "javascript" javascript
      "j2" jinja2 "jinja2" jinja2 "jinja" jinja2
      "jq" jq "js.erb" js.erb "json" json
      "sublime-settings" json "sublime-menu" json "sublime-keymap" json
      "sublime-mousemap" json "sublime-theme" json "sublime-build" json
      "sublime-project" json "sublime-completions" json "sublime-commands" json
      "sublime-macro" json "sublime-color-scheme" json "ipynb" json
      "pipfile.lock" json "jsonnet" jsonnet "libsonnet" jsonnet
      "libjsonnet" jsonnet "jsp" jsp "jl" julia
      "julia" julia "known_hosts" known_hosts "known_hosts.old" known_hosts
      "kt" kotlin "kts" kotlin "kotlin" kotlin
      "tex" latex "ltx" latex "latex" latex
      "lean" lean "less" less "css.less" less
      "lhs" lhs "lisp" lisp "cl" lisp
      "l" lisp "mud" lisp "el" lisp
      "scm" lisp "ss" lisp "lsp" lisp
      "fasl" lisp "ll" llvm "llvm" llvm
      "log" log "lua" lua "make" makefile
      "gnumakefile" makefile "makefile" makefile "makefile" makefile
      "makefile.am" makefile "makefile.am" makefile "makefile.in" makefile
      "makefile.in" makefile "ocamlmakefile" makefile "mak" makefile
      "mk" makefile "man" manpage "manpage" manpage
      "md" markdown "mdown" markdown "markdown" markdown
      "markdn" markdown "matlab" matlab "mediawikerpanel" mediawikerpanel
      "mediawiki" mediawiki "wikipedia" mediawiki "wiki" mediawiki
      "meminfo" meminfo "multimarkdown" multimarkdown "namelist" namelist
      "conf.erb" nginx "conf" nginx "nginx.conf" nginx
      "mime.types" nginx "fastcgi_params" nginx "scgi_params" nginx
      "uwsgi_params" nginx "nginx" nginx "nim" nim
      "nims" nim "nimble" nim "ninja" ninja
      "nix" nix "m" objective-c "h" objective-c
      "objective-c" objective-c "mm" objective-c++ "m" objective-c++
      "h" objective-c++ "objective-c++" objective-c++ "ml" ocaml
      "mli" ocaml "ocaml" ocaml "mll" ocamllex
      "ocamllex" ocamllex "mly" ocamlyacc "ocamlyacc" ocamlyacc
      "org" orgmode "orgmode" orgmode "pas" pascal
      "p" pascal "dpr" pascal "pascal" pascal
      "passwd" passwd "pb.txt" pb.txt "proto.text" pb.txt
      "textpb" pb.txt "pbtxt" pb.txt "prototxt" pb.txt
      "pl" perl "pc" perl "pm" perl
      "pmc" perl "pod" perl "t" perl
      "perl" perl "php" php "php3" php
      "php4" php "php5" php "php7" php
      "phps" php "phpt" php "phtml" php
      "properties" properties "proto" proto "protodevel" proto
      "pp" puppet "epp" puppet "puppet" puppet
      "purs" purescript "purescript" purescript "py" python
      "py3" python "pyw" python "pyi" python
      "pyx" python "pyx.in" python "pxd" python
      "pxd.in" python "pxi" python "pxi.in" python
      "rpy" python "cpy" python "sconstruct" python
      "sconstruct" python "sconstruct" python "sconscript" python
      "gyp" python "gypi" python "snakefile" python
      "vpy" python "wscript" python "bazel" python
      "bzl" python "python" python "qml" qml
      "qmlproject" qml "r" r "r" r
      "rprofile" r "rkt" racket "racket" racket
      "rails" rails "rhtml" rails "erb" rails
      "html.erb" rails "rd" rd "re" re
      "rego" rego "requirements.txt" requirements.txt "requirements.in" requirements.txt
      "pip" requirements.txt "resolv.conf" resolv "resolv" resolv
      "rst" restructuredtext "rest" restructuredtext "restructuredtext" restructuredtext
      "robot" robot "resource" robot "rb" ruby
      "appfile" ruby "appraisals" ruby "berksfile" ruby
      "brewfile" ruby "capfile" ruby "cgi" ruby
      "cheffile" ruby "config.ru" ruby "deliverfile" ruby
      "fastfile" ruby "fcgi" ruby "gemfile" ruby
      "gemspec" ruby "guardfile" ruby "irbrc" ruby
      "jbuilder" ruby "podfile" ruby "podspec" ruby
      "prawn" ruby "rabl" ruby "rake" ruby
      "rakefile" ruby "rantfile" ruby "rbx" ruby
      "rjs" ruby "ruby.rail" ruby "scanfile" ruby
      "simplecov" ruby "snapfile" ruby "thor" ruby
      "thorfile" ruby "vagrantfile" ruby "ruby" ruby
      "rs" rust "rust" rust "rxml" rxml
      "builder" rxml "scala" scala "sbt" scala
      "sc" scala "scss" scss "shell-unix-generic" shell-unix-generic
      "show-nonprintable" show-nonprintable "slim" slim "skim" slim
      "sml" sml "cm" sml "sig" sml
      "sol" solidity "solidity" solidity "sql" sql
      "ddl" sql "dml" sql "ssh_config" ssh_config
      "sshd_config" sshd_config "strace" strace "styl" stylus
      "stylus" stylus "svlt" svelte "svelte" svelte
      "swift" swift "syslog" syslog "sv" systemverilog
      "v" systemverilog "svh" systemverilog "vh" systemverilog
      "systemverilog" systemverilog "tcl" tcl "tf" terraform
      "tfvars" terraform "hcl" terraform "terraform" terraform
      "sty" tex "cls" tex "tex" tex
      "textile" textile "tfstate" tfstate "todo.txt" todo.txt
      "done.txt" todo.txt "toml" toml "tml" toml
      "cargo.lock" toml "gopkg.lock" toml "pipfile" toml
      "pdm.lock" toml "poetry.lock" toml "twig" twig
      "html.twig" twig "txt" txt "ts" typescript
      "mts" typescript "cts" typescript "typescript" typescript
      "tsx" typescriptreact "typescriptreact" typescriptreact "typ" typst
      "typc" typst "typst" typst "varlink" varlink
      "v" verilog "v" verilog "verilog" verilog
      "vim" viml "vimrc" viml "gvimrc" viml
      ".vimrc" viml ".gvimrc" viml "_vimrc" viml
      "_gvimrc" viml "viml" viml "vue" vue
      "vy" vyper "vyper" vyper "xml" xml
      "xsd" xml "xslt" xml "tld" xml
      "dtml" xml "rng" xml "rss" xml
      "opml" xml "svg" xml "xaml" xml
      "yaml" yaml "yml" yaml "sublime-syntax" yaml
      "yasm" yasm "nasm" yasm "asm" yasm
      "inc" yasm "mac" yasm "yaws" yaws
      "zig" zig
      ))
  "Raw block tag -> tree sitter language map.
Associated map: `typst-ts-els-lang-tags-map'.
Please use function `typst-ts-els--add-treesit-range-rules' and
`typst-ts-els--lang-name-remap' to modify them.")

;; to test settings:
;; emacs --batch -l ./typst-ts-embedding-lang-settings.el --eval "(typst-ts-embedding-lang-settings-test)"
(defvar typst-ts-embedding-lang-settings
  '((bash . (:feature
             sh-script
             :font-lock sh-mode--treesit-settings
             :indentation nil
             :ts-feature-list
             '(( comment function)
               ( command declaration-command keyword string)
               ( builtin-variable constant heredoc number
                 string-interpolation variable)
               ( bracket delimiter misc-punctuation operator))))
    (c . (:feature
          c-ts-mode
          :font-lock (c-ts-mode--font-lock-settings 'c)
          :indentation (c-ts-mode--get-indent-style 'c)
          :ts-feature-list c-ts-mode--feature-list))
    (c-sharp . (:feature
                csharp-mode
                :font-lock csharp-ts-mode--font-lock-settings
                :indentation csharp-ts-mode--indent-rules
                :ts-feature-list
                '(( comment definition)
                  ( keyword string type directives)
                  ( constant escape-sequence expression literal property)
                  ( function bracket delimiter error))))
    (cmake . (:feature
              cmake-ts-mode
              :font-lock cmake-ts-mode--font-lock-settings
              :indentation cmake-ts-mode--indent-rules
              :ts-feature-list
              '((comment)
                (keyword string)
                (builtin constant escape-sequence function number variable)
                (bracket error misc-punctuation))))
    (cpp . (:feature
            c-ts-mode
            :font-lock (c-ts-mode--font-lock-settings 'cpp)
            :indentation (c-ts-mode--get-indent-style 'cpp)
            :ts-feature-list c-ts-mode--feature-list))
    (css . (:feature
            css-mode
            :font-lock css--treesit-settings
            :indentation css--treesit-indent-rules
            :ts-feature-list
            '((selector comment query keyword)
              (property constant string)
              (error variable function operator bracket))))
    (dockerfile . (:feature
                   dockerfile-ts-mode
                   :font-lock dockerfile-ts-mode--font-lock-settings
                   :indentation dockerfile-ts-mode--indent-rules
                   :ts-feature-list
                   '((comment)
                     (keyword string)
                     (image-spec number)
                     (bracket delimiter error operator))))
    (elixir . (:feature
               elixir-ts-mode
               :font-lock elixir-ts--font-lock-settings
               :indentation elixir-ts--indent-rules
               :ts-feature-list
               '(( elixir-comment elixir-doc elixir-function-name)
                 ( elixir-string elixir-keyword elixir-data-type)
                 ( elixir-sigil elixir-variable elixir-builtin
                   elixir-string-escape)
                 ( elixir-function-call elixir-operator elixir-number ))))
    (go . (:feature
           go-ts-mode
           :font-lock go-ts-mode--font-lock-settings
           :indentation go-ts-mode--indent-rules
           :ts-feature-list
           '(( comment definition)
             ( keyword string type)
             ( constant escape-sequence label number)
             ( bracket delimiter error function operator property variable))))
    (gomod . (:feature
              go-ts-mode
              :font-lock go-mod-ts-mode--font-lock-settings
              :indentation go-mod-ts-mode--indent-rules
              :ts-feature-list
              '((comment)
                (keyword)
                (number)
                (bracket error operator))))
    (heex . (:feature
             heex-ts-mode
             :font-lock heex-ts--font-lock-settings
             :indentation heex-ts--indent-rules
             :ts-feature-list
             '(( heex-comment heex-keyword heex-doctype )
               ( heex-component heex-tag heex-attribute heex-string )
               () ())))
    (html . (:feature
             html-ts-mode
             :font-lock html-ts-mode--font-lock-settings
             :indentation html-ts-mode--indent-rules
             :ts-feature-list
             '((comment keyword definition)
               (property string)
               () ())))
    (java . (:feature
             java-ts-mode
             :font-lock java-ts-mode--font-lock-settings
             :indentation java-ts-mode--indent-rules
             :ts-feature-list java-ts-mode--feature-list))
    (javascript . (:feature
                   js
                   :font-lock js--treesit-font-lock-settings
                   :indentation js--treesit-indent-rules
                   :ts-feature-list
                   '(( comment definition)
                     ( keyword string)
                     ( assignment constant escape-sequence jsx number
                       pattern string-interpolation)
                     ( bracket delimiter function operator property))))
    (json . (:feature
             json-ts-mode
             :font-lock json-ts-mode--font-lock-settings
             :indentation json-ts--indent-rules
             :ts-feature-list
             '((comment constant number pair string)
               (escape-sequence)
               (bracket delimiter error))))
    (kotlin . (:feature
               kotlin-ts-mode
               :font-lock kotlin-ts-mode--treesit-settings
               :indentation kotlin-ts-mode--treesit-indent-rules
               :ts-feature-list
               '((comment number string definition)
                 (keyword builtin type constant variable)
                 (escape-sequence function property))))
    (lua . (:feature
            lua-ts-mode
            :font-lock lua-ts--font-lock-settings
            :indentation lua-ts--simple-indent-rules
            :ts-feature-list
            '((comment definition)
              (keyword string)
              (assignment builtin constant number)
              (bracket delimiter escape function
                       operator property punctuation variable))))
    (python . (:feature
               python
               :font-lock python--treesit-settings
               :indentation nil
               :ts-feature-list
               '(( comment definition)
                 ( keyword string type)
                 ( assignment builtin constant decorator
                   escape-sequence number string-interpolation )
                 ( bracket delimiter function operator variable property ))))
    (ruby . (:feature
             ruby-ts-mode
             :font-lock (ruby-ts--font-lock-settings 'ruby)
             :indentation (ruby-ts--indent-rules)
             :ts-feature-list
             '(( comment method-definition parameter-definition)
               ( keyword regexp string type)
               ( builtin-variable builtin-constant builtin-function
                 delimiter escape-sequence
                 constant global instance
                 interpolation literal symbol assignment)
               ( bracket error function operator punctuation))))
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
    (toml . (:feature
             toml-ts-mode
             :font-lock toml-ts-mode--font-lock-settings
             :indentation toml-ts-mode--indent-rules
             :ts-feature-list
             '((comment)
               (constant number pair string)
               (escape-sequence)
               (delimiter error))))
    (tsx . (:feature
            typescript-ts-mode
            :font-lock (typescript-ts-mode--font-lock-settings 'tsx)
            :indentation (typescript-ts-mode--indent-rules 'tsx)
            :ts-feature-list
            '((comment declaration)
              (keyword string escape-sequence)
              (constant expression identifier jsx number pattern property)
              (function bracket delimiter))))
    (typescript . (:feature
                   typescript-ts-mode
                   :font-lock (typescript-ts-mode--font-lock-settings 'typescript)
                   :indentation (typescript-ts-mode--indent-rules 'typescript)
                   :ts-feature-list
                   '((comment declaration)
                     (keyword string escape-sequence)
                     (constant expression identifier number pattern property)
                     (operator function bracket delimiter))))
    (yaml . (:feature
             yaml-ts-mode
             :font-lock yaml-ts-mode--font-lock-settings
             :indentation nil
             :ts-feature-list
             '((comment)
               (string type)
               (constant escape-sequence number property)
               (bracket delimiter error misc-punctuation))))
    (mermaid . (:feature
                mermaid-ts-mode
                :font-lock mermaid-ts--treesit-font-lock-rules
                :indentation mermaid-ts--indent-rules
                :ts-feature-list
                '((comments)
                  (constants keywords text links)
                  (nodes)))))
  "Predefined settings for raw block languages.
Format: Language name -> settings.
Use function `typst-ts-embedding-lang-settings-test' to test your settings.")


(defun typst-ts-els--merge-features (a b)
  "Merge `treesit-font-lock-feature-list' A with B."
  (when (not (and a b))
    (error "One of the treesit font lock feature list is nil when merge!"))
  (cl-loop for i to 3  ; [0, 3]
           collect
           (seq-uniq (append (nth i a) (nth i b)))))

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

(defun typst-ts-els--ignore-regexp-case (re)
  "Make a regular expression ignore case.
RE: regular expression.
NOTE: only suitable for `(regexp-opt string)' output.
This function is specially made since :match predicate in treesit query is
case-sensitive."
  (let (inside-squre)
    (cl-loop for c across re
             concat
             (if (or (and (>= c ?a)
                          (<= c ?z))
                     (and (>= c ?A)
                          (<= c ?Z)))
                 (if inside-squre
                     (string (upcase c) (downcase c))
                   (concat "[" (string (upcase c) (downcase c)) "]"))
               (if (= c ?\[)
                   (setq inside-squre t)
                 (when (= c ?\])
                   (setq inside-squre nil)))
               (string c)))))

(defun typst-ts-els--get-lang-tags-regexp-ignore-case (lang)
  "Get the regexp matching for LANG tags."
  (let ((tags (gethash lang typst-ts-els-lang-tags-map)))
    (unless tags
      (error "%s not in `typst-ts-els-lang-tags-map'" lang))
    (concat "^"
            (typst-ts-els--ignore-regexp-case (regexp-opt tags))
            "$")))

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
      (:match ,(typst-ts-els--get-lang-tags-regexp-ignore-case lang) @_lang)
      ))))

(defun typst-ts-els--add-treesit-range-rules (lang)
  "Add treesit range rule for LANG.
LANG: language symbol."
  (setq
   treesit-range-settings
   (nconc
    treesit-range-settings
    (typst-ts-els--treesit-range-rules lang))))

(defun typst-ts-els--try-get-ts-settings (mode)
  "Try your luck to get related settings for specific tree sitter mode.
MODE: tree sitter mode.
This function basically create a temp buffer, then get the tree sitter settings
from related local variables."
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
Basically, this function will first try to merge settings from predefined
settings `typst-ts-embedding-lang-settings'.  If a language is not in the
predefined settings, then it will try to guess the corresponding tree sitter
major mode name from the language, try to load it using
`typst-ts-els--try-get-ts-settings'.
Use this function as one notifier of `treesit-parser-notifiers'."
  ;; `treesit-language-at-point-function' will ensure that the
  ;; languages in `treesit-parser-list' are valid (not just a random string)
  (let ((parser-langs
         (delete-dups
          (append
           ;; parsers created by `treesit-language-at-point-function' (
           ;; `typst-ts-mode--language-at-point'.)
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
                  ;; note: the `treesit-range-settings' for languages in
                  ;; predefined settings are already settled at mode start
                  (typst-ts-els-merge-lang-settings lang)
                  ;; some feature like cmake-ts-mode will create a parser when
                  ;; the feature is required, so we need to clean thease parsers
                  (mapc #'treesit-parser-delete (treesit-parser-list nil lang))
                  (message "Load %s language settings from configuration." lang))
              (error
               ;; if language not in setting or encounter error during loading,
               ;; then try your luck to load it
               (when typst-ts-highlight-raw-block-langs-not-in-predefined-settings
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
                             (error-message-string err)))))
               ))
          ;; whatever, we won't load that language again
          (add-to-list 'typst-ts-els--include-languages lang))
        ))))

;;; Utilities functions for changing language tag relationship (change two maps
;;; synchronizely) =============================================================

(defun typst-ts-els--get-lang-input (lang)
  (if (symbolp lang)
      (intern (downcase (symbol-name lang)))
    (if (stringp lang)
        (intern (downcase lang))
      (error "LANG should be either symbol or string"))))

(defun typst-ts-els--get-tags-input (tags)
  (if (stringp tags)
      (list (downcase tags))
    (if (and (listp tags)
             (stringp (nth 0 tags)))
        (mapcar #'downcase tags)
      (error "TAGS should be either a string or a list of strings"))))

;;;###autoload
(defun typst-ts-els--add-lang-tags-relationship (lang tags)
  "Add or modify language tags relationship.
This function will make changes to `typst-ts-els-lang-tags-map' and
`typst-ts-els-tag-lang-map'.
LANG: either a symbol or string.
TAGS: either a string or a list of strings."
  (let ((lang (typst-ts-els--get-lang-input lang))
        (tags (typst-ts-els--get-tags-input tags))
        (original-tags (gethash lang typst-ts-els-lang-tags-map))
        temp-lang)
    (dolist (tag tags)
      (setq temp-lang (gethash tag typst-ts-els-tag-lang-map))
      (when (and temp-lang (not (eq temp-lang lang)))
        (puthash temp-lang
                 (remove tag (gethash temp-lang typst-ts-els-lang-tags-map))
                 typst-ts-els-lang-tags-map))
      
      (puthash tag lang typst-ts-els-tag-lang-map))
    (puthash lang (seq-uniq (append tags original-tags)) typst-ts-els-lang-tags-map)))

;;;###autoload
(defun typst-ts-els--lang-name-remap (lang newlang)
  "Remap language name to a new language name.
This function will remap lang to newlang for `typst-ts-els-lang-tags-map' and
`typst-ts-els-tag-lang-map'.
LANG and NEWLANG: either a symbol or string."
  (let ((lang (typst-ts-els--get-lang-input lang))
        (newlang (typst-ts-els--get-lang-input newlang))
        lang-tags newlang-tags)
    (unless (eq lang newlang)
      (setq lang-tags (gethash lang typst-ts-els-lang-tags-map))
      (setq newlang-tags (gethash newlang typst-ts-els-lang-tags-map))
      
      (dolist (tag lang-tags)
        (puthash tag newlang typst-ts-els-tag-lang-map))
      
      (puthash newlang (append newlang-tags lang-tags) typst-ts-els-lang-tags-map)
      (remhash lang typst-ts-els-lang-tags-map))))

;;; Test Utilities =============================================================

(defun typst-ts-embedding-lang-settings-test ()
  "Test `typst-ts-embedding-lang-settings'."
  (with-temp-buffer
    (setq-local treesit-font-lock-feature-list
                '((comment common)
                  (markup-basic code-basic math-basic)
                  (markup-standard code-standard math-standard)
                  (markup-extended code-extended math-extended)))
    (let (missing-dylibs err-msgs)
      (dolist (setting-entry typst-ts-embedding-lang-settings)
        (let ((language (car setting-entry))
              (config (cdr setting-entry)))
          (message "Testing %s ..."  language)
          (unless (treesit-ready-p language t)
            (setq missing-dylibs (list (symbol-name language))))
          (condition-case err
              (typst-ts-els-merge-settings config)
            (error
             (setq err-msgs (list (error-message-string err)))))))
      (message "---------  Missing Tree Sitter Dynamic libraries -------------")
      (message " (This also could be an error of entry key name in settings) ")
      (message "%s" (string-join missing-dylibs " "))
      (message "---------  Error Messages ------------------------------------")
      (message "%s" (string-join err-msgs "\n")))))

(provide 'typst-ts-embedding-lang-settings)

;;; typst-ts-embedding-lang-settings.el ends here
