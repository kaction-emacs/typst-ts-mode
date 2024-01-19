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
      txt ("txt" )
      asp ("asa" "asp" )
      actionscript ("actionscript" "as" )
      applescript ("applescript" "script editor" )
      bat ("bat" "cmd" )
      build ("build" )
      c-sharp ("cs" "c#" "csx" )
      cpp ("cc" "cp" "cxx" "c++" "hpp" "hxx" "cpp" "inl" "ipp" "h++" "hh" )
      c ("c" )
      css ("css.erb" "css" "css.liquid" )
      clojure ("cljs" "edn" "clj" "cljc" "clojure" )
      d ("di" "d" )
      diff ("diff" "patch" )
      erlang ("erlang" "emakefile" "escript" "erl" "hrl" )
      yaws ("yaws" )
      attributes ("attributes" ".gitattributes" "gitattributes" )
      commit_editmsg ("tag_editmsg" "merge_msg" "commit_editmsg" )
      gitconfig (".gitconfig" ".gitmodules" "gitconfig" )
      exclude ("exclude" "gitignore" ".gitignore" )
      .git (".git" )
      gitlog ("gitlog" )
      .mailmap (".mailmap" "mailmap" )
      git-rebase-todo ("git-rebase-todo" )
      go ("go" )
      dot ("dot" "gv" )
      groovy ("groovy" "gradle" "gvy" "jenkinsfile" )
      html ("htm" "shtml" "html" "xhtml" )
      haskell ("hs" "haskell" )
      lhs ("lhs" )
      json ("ipynb" "sublime-build" "sublime-theme" "sublime-keymap" "sublime-macro" "sublime-color-scheme" "sublime-completions" "sublime-project" "pipfile.lock" "sublime-settings" "json" "sublime-mousemap" "sublime-commands" "sublime-menu" )
      jsp ("jsp" )
      java ("java" "bsh" )
      javadoc ("javadoc" )
      properties ("properties" )
      javascript ("js" "htc" "javascript" )
      bibtex ("bib" "bibtex" )
      latex ("ltx" "latex" )
      tex ("cls" "sty" "tex" )
      commonlisp ("clisp" )
      lisp ("lsp" "lisp" "ss" "l" "fasl" "el" "mud" "cl" "scm" )
      lua ("lua" )
      makefile ("makefile.am" "ocamlmakefile" "make" "makefile" "mk" "gnumakefile" "mak" "makefile.in" )
      markdown ("markdn" "markdown" "mdown" "md" )
      multimarkdown ("multimarkdown" )
      matlab ("matlab" )
      ocaml ("mli" "ocaml" "ml" )
      ocamllex ("mll" "ocamllex" )
      ocamlyacc ("mly" "ocamlyacc" )
      camlp4 ("camlp4" )
      objective-c++ ("objective-c++" "mm" )
      objective-c ("m" "h" "objective-c" )
      php ("phtml" "php" "php5" "php7" "phpt" "php4" "php3" "phps" )
      pascal ("dpr" "p" "pas" "pascal" )
      perl ("pc" "pl" "perl" "pmc" "t" "pm" "pod" )
      python ("pxi" "bazel" "rpy" "gyp" "python" "py" "py3" "bzl" "pyw" "pxd.in" "pyx.in" "snakefile" "sconscript" "pyi" "sconstruct" "wscript" "pyx" "pxd" "pxi.in" "gypi" "cpy" "vpy" )
      r ("r" "rprofile" )
      rd ("rd" )
      rails ("html.erb" "rails" "erb" "rhtml" )
      js.erb ("js.erb" )
      haml ("sass" "haml" )
      rxml ("builder" "rxml" )
      erbsql ("erbsql" "sql.erb" )
      re ("re" )
      restructuredtext ("restructuredtext" "rest" "rst" )
      ruby ("brewfile" "fcgi" "jbuilder" "appfile" "rakefile" "ruby" "cheffile" "rabl" "podfile" "rbx" "config.ru" "irbrc" "cgi" "deliverfile" "guardfile" "gemfile" "appraisals" "ruby.rail" "gemspec" "vagrantfile" "berksfile" "fastfile" "prawn" "capfile" "rake" "rb" "podspec" "scanfile" "snapfile" "thor" "thorfile" "rjs" "rantfile" "simplecov" )
      rust ("rust" "rs" )
      sql ("ddl" "dml" "sql" )
      scala ("scala" "sbt" "sc" )
      bash (".bash_profile" "bash" ".zprofile" "ebuild" "sh" ".bash_functions" ".bash_aliases" "zsh" ".bash_completions" ".bash_logout" ".bash_variables" ".zshrc" "eclass" "ash" ".bash_login" ".zshenv" ".bashrc" ".textmate_init" ".zlogout" "pkgbuild" ".zlogin" ".profile" )
      shell-unix-generic ("shell-unix-generic" )
      commands-builtin-shell-bash ("commands-builtin-shell-bash" )
      adp ("adp" )
      tcl ("tcl" )
      textile ("textile" )
      xml ("xsd" "rng" "dtml" "xslt" "xml" "opml" "xaml" "svg" "rss" "tld" )
      yaml ("yml" "yaml" "sublime-syntax" )
      awk ("awk" )
      ada ("ads" "gpr" "ada" "adb" )
      envvars ("htaccess" ".htgroups" "htgroups" "htpasswd" ".htpasswd" "envvars" ".htaccess" )
      adoc ("asciidoc" "adoc" "ad" )
      yasm ("yasm" "nasm" "inc" "mac" "asm" )
      h.in ("h.in" )
      hh.in ("hh.in" "h++.in" "hpp.in" "hxx.in" )
      cmake ("cmake" "cmakelists.txt" )
      cmakecache ("cmakecache.txt" "cmakecache" )
      cmakecommands ("cmakecommands" )
      csv ("csv" "tsv" )
      cabal ("cabal" )
      coffeescript ("cakefile" "coffee.erb" "coffee" "cson" "coffeescript" )
      cpuinfo ("cpuinfo" )
      crontab ("tab" "crontab" "cron.d" )
      crystal ("cr" "crystal" )
      dart ("dart" )
      dockerfile ("dockerfile" )
      dotenv (".env.example" ".env.staging" ".env.template" ".env.production.local" "dotenv" ".env.test" ".env.dev" ".env.default" ".env.development" ".env" ".env.sample" ".env.testing" ".env.defaults" ".env.test.local" ".env.prod" ".env.production" ".env.dusk.local" ".envrc" ".flaskenv" ".env.dist" ".env.local" "env" "env.example" "env.sample" ".env.development.local" "env.template" )
      elixir ("elixir" "exs" "ex" )
      html.eex ("html.leex" "html.eex" )
      elm ("elm" )
      email ("msg" "email" "mbx" "mboxz" "eml" )
      fs ("f#" "fsx" "fsi" )
      fish ("fish" )
      f ("fpp" "f77" "f" "for" )
      f90 ("f95" "f90" "f03" "f08" )
      namelist ("namelist" )
      fstab ("fstab" "crypttab" "mtab" )
      glsl ("vs" "gsh" "comp" "mesh" "task" "rgen" "vshader" "glsl" "rmiss" "rahit" "fs" "rint" "vert" "vsh" "gs" "fsh" "frag" "geom" "tesc" "rchit" "rcall" "fshader" "gshader" "tese" )
      graphql ("graphqls" "gql" "graphql" )
      groff/troff ("groff" "8" "groff/troff" "9" "1" "5" "4" "3" "6" "7" "2" "troff" )
      group ("group" )
      twig ("html.twig" "twig" )
      hosts ("hosts" )
      ini ("lng" ".editorconfig" ".hgrc" "reg" "desktop" "hgrc" "inf" "cfg" "url" ".gitlint" ".pylintrc" "ini" ".coveragerc" )
      htm.j2 ("html.j2" "htm.j2" "xhtml.j2" "xml.j2" )
      jinja2 ("jinja2" "j2" "jinja" )
      jsonnet ("libsonnet" "libjsonnet" "jsonnet" )
      julia ("jl" "julia" )
      kotlin ("kt" "kotlin" "kts" )
      less ("less" "css.less" )
      llvm ("ll" "llvm" )
      lean ("lean" )
      manpage ("manpage" "man" )
      mediawikerpanel ("mediawikerpanel" )
      mediawiki ("wikipedia" "mediawiki" "wiki" )
      meminfo ("meminfo" )
      nginx ("fastcgi_params" "scgi_params" "conf" "nginx.conf" "uwsgi_params" "conf.erb" "mime.types" "nginx" )
      nim ("nims" "nimble" "nim" )
      ninja ("ninja" )
      nix ("nix" )
      orgmode ("org" "orgmode" )
      passwd ("passwd" )
      proto ("proto" "protodevel" )
      pb.txt ("proto.text" "pbtxt" "pb.txt" "textpb" "prototxt" )
      puppet ("puppet" "pp" "epp" )
      purescript ("purs" "purescript" )
      qml ("qml" "qmlproject" )
      racket ("racket" "rkt" )
      rego ("rego" )
      requirements.txt ("requirements.txt" "requirements.in" "pip" )
      resolv ("resolv.conf" "resolv" )
      robot ("robot" "resource" )
      scss ("scss" )
      sml ("cm" "sml" "sig" )
      slim ("slim" "skim" )
      strace ("strace" )
      stylus ("stylus" "styl" )
      solidity ("sol" "solidity" )
      vyper ("vy" "vyper" )
      jq ("jq" )
      svelte ("svelte" "svlt" )
      swift ("swift" )
      systemverilog ("sv" "systemverilog" "svh" "vh" )
      toml ("cargo.lock" "poetry.lock" "pipfile" "pdm.lock" "tml" "toml" "gopkg.lock" )
      tfstate ("tfstate" )
      terraform ("hcl" "terraform" "tf" "tfvars" )
      todo.txt ("todo.txt" "done.txt" )
      typescript ("mts" "ts" "cts" "typescript" )
      typescriptreact ("tsx" "typescriptreact" )
      verilog ("verilog" "v" )
      viml ("_gvimrc" "gvimrc" ".vimrc" "viml" "vimrc" "_vimrc" ".gvimrc" "vim" )
      vue ("vue" )
      zig ("zig" )
      gnuplot ("plt" "gnuplot" "gpl" "gnu" "gp" "plot" )
      http ("http" )
      log ("log" )
      show-nonprintable ("show-nonprintable" )
      authorized_keys ("authorized_keys" "pub" "authorized_keys2" )
      known_hosts ("known_hosts.old" "known_hosts" )
      ssh_config ("ssh_config" )
      sshd_config ("sshd_config" )
      syslog ("syslog" )
      varlink ("varlink" )
      typst ("typc" "typst" "typ" )

      ))
  "Tree sitter language -> Raw block tags map.
Associated map: `typst-ts-els-tag-lang-map'.
Please use function `typst-ts-els--add-treesit-range-rules' and
`typst-ts-els--lang-name-remap' to modify them.")

(defvar typst-ts-els-tag-lang-map
  #s(hash-table
     size 588
     test equal
     data
     (
      "txt" txt "asp" asp "asa" asp
      "as" actionscript "actionscript" actionscript "applescript" applescript
      "script editor" applescript "bat" bat "cmd" bat
      "build" build "c#" c-sharp "csx" c-sharp
      "cs" c-sharp "c++" cpp "h++" cpp
      "hxx" cpp "cpp" cpp "hpp" cpp
      "ipp" cpp "hh" cpp "inl" cpp
      "cxx" cpp "cc" cpp "cp" cpp
      "c" c "css.liquid" css "css" css
      "css.erb" css "cljs" clojure "clj" clojure
      "cljc" clojure "edn" clojure "clojure" clojure
      "di" d "d" d "patch" diff
      "diff" diff "escript" erlang "erlang" erlang
      "erl" erlang "hrl" erlang "emakefile" erlang
      "yaws" yaws "gitattributes" attributes ".gitattributes" attributes
      "attributes" attributes "tag_editmsg" commit_editmsg "commit_editmsg" commit_editmsg
      "merge_msg" commit_editmsg ".gitconfig" gitconfig ".gitmodules" gitconfig
      "gitconfig" gitconfig "gitignore" exclude ".gitignore" exclude
      "exclude" exclude ".git" .git "gitlog" gitlog
      "mailmap" .mailmap ".mailmap" .mailmap "git-rebase-todo" git-rebase-todo
      "go" go "gv" dot "dot" dot
      "gvy" groovy "gradle" groovy "groovy" groovy
      "jenkinsfile" groovy "html" html "htm" html
      "shtml" html "xhtml" html "hs" haskell
      "haskell" haskell "lhs" lhs "ipynb" json
      "sublime-macro" json "sublime-project" json "sublime-completions" json
      "sublime-keymap" json "sublime-settings" json "sublime-mousemap" json
      "sublime-commands" json "sublime-color-scheme" json "pipfile.lock" json
      "sublime-menu" json "sublime-build" json "json" json
      "sublime-theme" json "jsp" jsp "java" java
      "bsh" java "javadoc" javadoc "properties" properties
      "htc" javascript "js" javascript "javascript" javascript
      "bib" bibtex "bibtex" bibtex "ltx" latex
      "latex" latex "sty" tex "cls" tex
      "tex" tex "clisp" commonlisp "fasl" lisp
      "el" lisp "lisp" lisp "l" lisp
      "mud" lisp "lsp" lisp "cl" lisp
      "scm" lisp "ss" lisp "lua" lua
      "makefile.am" makefile "mk" makefile "makefile.in" makefile
      "make" makefile "makefile" makefile "gnumakefile" makefile
      "mak" makefile "ocamlmakefile" makefile "mdown" markdown
      "markdn" markdown "md" markdown "markdown" markdown
      "multimarkdown" multimarkdown "matlab" matlab "ml" ocaml
      "ocaml" ocaml "mli" ocaml "mll" ocamllex
      "ocamllex" ocamllex "mly" ocamlyacc "ocamlyacc" ocamlyacc
      "camlp4" camlp4 "objective-c++" objective-c++ "mm" objective-c++
      "objective-c" objective-c "h" objective-c "m" objective-c
      "php" php "phps" php "php3" php
      "phpt" php "phtml" php "php7" php
      "php5" php "php4" php "pascal" pascal
      "dpr" pascal "p" pascal "pas" pascal
      "pm" perl "pmc" perl "perl" perl
      "pl" perl "pc" perl "pod" perl
      "t" perl "py" python "pxi.in" python
      "pxi" python "rpy" python "sconstruct" python
      "pyi" python "gypi" python "pyw" python
      "bazel" python "python" python "pyx.in" python
      "wscript" python "pxd" python "pyx" python
      "sconscript" python "pxd.in" python "snakefile" python
      "py3" python "gyp" python "vpy" python
      "bzl" python "cpy" python "r" r
      "rprofile" r "rd" rd "rails" rails
      "rhtml" rails "erb" rails "html.erb" rails
      "js.erb" js.erb "haml" haml "sass" haml
      "builder" rxml "rxml" rxml "erbsql" erbsql
      "sql.erb" erbsql "re" re "rest" restructuredtext
      "restructuredtext" restructuredtext "rst" restructuredtext "capfile" ruby
      "simplecov" ruby "thor" ruby "berksfile" ruby
      "thorfile" ruby "cheffile" ruby "podspec" ruby
      "config.ru" ruby "rjs" ruby "vagrantfile" ruby
      "ruby" ruby "deliverfile" ruby "rb" ruby
      "guardfile" ruby "ruby.rail" ruby "irbrc" ruby
      "fastfile" ruby "fcgi" ruby "rabl" ruby
      "rantfile" ruby "prawn" ruby "appfile" ruby
      "appraisals" ruby "scanfile" ruby "brewfile" ruby
      "podfile" ruby "gemfile" ruby "gemspec" ruby
      "jbuilder" ruby "rake" ruby "rbx" ruby
      "snapfile" ruby "rakefile" ruby "cgi" ruby
      "rust" rust "rs" rust "ddl" sql
      "dml" sql "sql" sql "scala" scala
      "sbt" scala "sc" scala "pkgbuild" bash
      "zsh" bash "bash" bash "ash" bash
      ".zprofile" bash "sh" bash "eclass" bash
      ".bash_functions" bash ".bash_logout" bash ".bash_completions" bash
      ".bash_aliases" bash "ebuild" bash ".bash_profile" bash
      ".zshrc" bash ".textmate_init" bash ".zlogin" bash
      ".zshenv" bash ".profile" bash ".bash_variables" bash
      ".bash_login" bash ".zlogout" bash ".bashrc" bash
      "shell-unix-generic" shell-unix-generic "commands-builtin-shell-bash" commands-builtin-shell-bash "adp" adp
      "tcl" tcl "textile" textile "xsd" xml
      "rss" xml "xslt" xml "dtml" xml
      "opml" xml "tld" xml "rng" xml
      "xml" xml "svg" xml "xaml" xml
      "yaml" yaml "yml" yaml "sublime-syntax" yaml
      "awk" awk "ads" ada "adb" ada
      "gpr" ada "ada" ada ".htpasswd" envvars
      "htpasswd" envvars ".htaccess" envvars "htaccess" envvars
      "htgroups" envvars ".htgroups" envvars "envvars" envvars
      "ad" adoc "asciidoc" adoc "adoc" adoc
      "nasm" yasm "yasm" yasm "asm" yasm
      "mac" yasm "inc" yasm "h.in" h.in
      "hxx.in" hh.in "h++.in" hh.in "hh.in" hh.in
      "hpp.in" hh.in "cmake" cmake "cmakelists.txt" cmake
      "cmakecache" cmakecache "cmakecache.txt" cmakecache "cmakecommands" cmakecommands
      "tsv" csv "csv" csv "cabal" cabal
      "cson" coffeescript "coffeescript" coffeescript "coffee" coffeescript
      "coffee.erb" coffeescript "cakefile" coffeescript "cpuinfo" cpuinfo
      "tab" crontab "cron.d" crontab "crontab" crontab
      "crystal" crystal "cr" crystal "dart" dart
      "dockerfile" dockerfile "env.template" dotenv ".env.production" dotenv
      ".flaskenv" dotenv ".envrc" dotenv ".env.testing" dotenv
      ".env.dusk.local" dotenv "env.example" dotenv "env.sample" dotenv
      ".env.development" dotenv ".env.defaults" dotenv ".env.test" dotenv
      ".env" dotenv ".env.local" dotenv ".env.prod" dotenv
      ".env.dist" dotenv ".env.test.local" dotenv ".env.dev" dotenv
      ".env.production.local" dotenv "env" dotenv "dotenv" dotenv
      ".env.sample" dotenv ".env.default" dotenv ".env.example" dotenv
      ".env.development.local" dotenv ".env.template" dotenv ".env.staging" dotenv
      "exs" elixir "ex" elixir "elixir" elixir
      "html.leex" html.eex "html.eex" html.eex "elm" elm
      "eml" email "msg" email "mboxz" email
      "email" email "mbx" email "fsi" fs
      "f#" fs "fsx" fs "fish" fish
      "f77" f "fpp" f "f" f
      "for" f "f08" f90 "f90" f90
      "f95" f90 "f03" f90 "namelist" namelist
      "fstab" fstab "crypttab" fstab "mtab" fstab
      "rahit" glsl "vshader" glsl "tesc" glsl
      "vert" glsl "rcall" glsl "vsh" glsl
      "frag" glsl "comp" glsl "task" glsl
      "rgen" glsl "tese" glsl "gsh" glsl
      "fs" glsl "rchit" glsl "fshader" glsl
      "vs" glsl "gshader" glsl "gs" glsl
      "rmiss" glsl "mesh" glsl "fsh" glsl
      "geom" glsl "glsl" glsl "rint" glsl
      "gql" graphql "graphqls" graphql "graphql" graphql
      "9" groff/troff "3" groff/troff "groff" groff/troff
      "4" groff/troff "6" groff/troff "8" groff/troff
      "groff/troff" groff/troff "2" groff/troff "troff" groff/troff
      "1" groff/troff "7" groff/troff "5" groff/troff
      "group" group "twig" twig "html.twig" twig
      "hosts" hosts "inf" ini "reg" ini
      "desktop" ini "url" ini ".hgrc" ini
      "cfg" ini ".editorconfig" ini ".gitlint" ini
      "hgrc" ini "ini" ini "lng" ini
      ".pylintrc" ini ".coveragerc" ini "htm.j2" htm.j2
      "xhtml.j2" htm.j2 "html.j2" htm.j2 "xml.j2" htm.j2
      "jinja" jinja2 "j2" jinja2 "jinja2" jinja2
      "libsonnet" jsonnet "libjsonnet" jsonnet "jsonnet" jsonnet
      "julia" julia "jl" julia "kts" kotlin
      "kotlin" kotlin "kt" kotlin "less" less
      "css.less" less "llvm" llvm "ll" llvm
      "lean" lean "man" manpage "manpage" manpage
      "mediawikerpanel" mediawikerpanel "wikipedia" mediawiki "wiki" mediawiki
      "mediawiki" mediawiki "meminfo" meminfo "mime.types" nginx
      "conf.erb" nginx "nginx.conf" nginx "scgi_params" nginx
      "conf" nginx "fastcgi_params" nginx "nginx" nginx
      "uwsgi_params" nginx "nimble" nim "nims" nim
      "nim" nim "ninja" ninja "nix" nix
      "orgmode" orgmode "org" orgmode "passwd" passwd
      "proto" proto "protodevel" proto "pbtxt" pb.txt
      "proto.text" pb.txt "prototxt" pb.txt "pb.txt" pb.txt
      "textpb" pb.txt "pp" puppet "puppet" puppet
      "epp" puppet "purescript" purescript "purs" purescript
      "qml" qml "qmlproject" qml "rkt" racket
      "racket" racket "rego" rego "requirements.txt" requirements.txt
      "requirements.in" requirements.txt "pip" requirements.txt "resolv.conf" resolv
      "resolv" resolv "resource" robot "robot" robot
      "scss" scss "cm" sml "sig" sml
      "sml" sml "skim" slim "slim" slim
      "strace" strace "styl" stylus "stylus" stylus
      "sol" solidity "solidity" solidity "vy" vyper
      "vyper" vyper "jq" jq "svlt" svelte
      "svelte" svelte "swift" swift "svh" systemverilog
      "systemverilog" systemverilog "sv" systemverilog "vh" systemverilog
      "pipfile" toml "tml" toml "cargo.lock" toml
      "toml" toml "pdm.lock" toml "gopkg.lock" toml
      "poetry.lock" toml "tfstate" tfstate "terraform" terraform
      "tf" terraform "tfvars" terraform "hcl" terraform
      "done.txt" todo.txt "todo.txt" todo.txt "typescript" typescript
      "ts" typescript "mts" typescript "cts" typescript
      "typescriptreact" typescriptreact "tsx" typescriptreact "verilog" verilog
      "v" verilog ".gvimrc" viml "_gvimrc" viml
      "viml" viml "gvimrc" viml "vim" viml
      ".vimrc" viml "vimrc" viml "_vimrc" viml
      "vue" vue "zig" zig "gp" gnuplot
      "plt" gnuplot "gnu" gnuplot "gnuplot" gnuplot
      "gpl" gnuplot "plot" gnuplot "http" http
      "log" log "show-nonprintable" show-nonprintable "pub" authorized_keys
      "authorized_keys" authorized_keys "authorized_keys2" authorized_keys "known_hosts" known_hosts
      "known_hosts.old" known_hosts "ssh_config" ssh_config "sshd_config" sshd_config
      "syslog" syslog "varlink" varlink "typc" typst
      "typ" typst "typst" typst
      ))
  "Raw block tag -> tree sitter language map.
Associated map: `typst-ts-els-lang-tags-map'.
Please use function `typst-ts-els--add-treesit-range-rules' and
`typst-ts-els--lang-name-remap' to modify them.")

;; to test settings:
;; emacs --batch -l ./typst-ts-embedding-lang-settings.el --eval "(typst-ts-embedding-lang-settings-test)"
;; TODO some language includes other languages
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
