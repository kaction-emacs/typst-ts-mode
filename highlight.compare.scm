(call
  item: (ident) @function)
(call
  item: (field field: (ident) @function.method))
(tagged field: (ident) @tag)
(field field: (ident) @tag)
(comment) @comment

; CONTROL
(let "let" @keyword.storage.type)
(branch ["if" "else"] @keyword.control.conditional)
(while "while" @keyword.control.repeat)
(for ["for" "in"] @keyword.control.repeat)
(import "import" @keyword.control.import)
(as "as" @keyword.operator)
(include "include" @keyword.control.import)
(show "show" @keyword.control)
(set "set" @keyword.control)
(return "return" @keyword.control)
(flow ["break" "continue"] @keyword.control)

; OPERATOR
(in ["in" "not"] @keyword.operator)           ;; DONE code
(and "and" @keyword.operator)                 ;; DONE code
(or "or" @keyword.operator)                   ;; DONE code
(not "not" @keyword.operator)                 ;; DONE code
(sign ["+" "-"] @operator)                    ;; DONE code
(add "+" @operator)                           ;; DONE code
(sub "-" @operator)                           ;; DONE code
(mul "*" @operator)                           ;; DONE code
(div "/" @operator)                           ;; DONE code
(cmp ["==" "<=" ">=" "!=" "<" ">"] @operator) ;; DONE code
(fraction "/" @operator)                      ;; TODO math
(fac "!" @operator)                           ;; TODO math
(attach ["^" "_"] @operator)                  ;; TODO math
(wildcard) @operator                          ;; DONE code

; VALUE
(raw_blck "```" @operator) @markup.raw.block ;; DONE markup 
(raw_span "`" @operator) @markup.raw.block   ;; DONE markup 
(raw_blck lang: (ident) @tag)                ;; DONE markup
(label) @tag                                 ;; DONE markup
(ref) @tag                                   ;; DONE markup
(number) @constant.numeric                   ;; DONE code
(string) @string                             ;; DONE code
(content ["[" "]"] @operator)                ;; DONE code
(bool) @constant.builtin.boolean             ;; DONE code
(builtin) @constant.builtin code             ;; DONE code
(none) @constant.builtin code                ;; DONE code
(auto) @constant.builtin                     ;; DONE code
(ident) @variable                            ;; DONE code
(call
  item: (builtin) @function.builtin)

; MARKUP
(item "item" @operator)             ;; DONE markup
(term ["item" ":"] @operator)       ;; DONE markup
(heading) @markup.heading           ;; DONE markup
(url) @tag                          ;; DONE markup 
(emph "_" @operator) @markup.italic ;; DONE markup
(strong "*" @operator) @markup.bold ;; DONE markup 
(item) @markup.list                 ;; DONE markup
(term) @markup.list                 ;; DONE markup
(symbol) @constant.character        ;; TODO math
(shorthand) @constant.builtin       ;; DONE common
(quote) @markup.quote               ;; DONE markup
(align) @operator                   ;; TODO math
(letter) @constant.character        ;; TODO math
(linebreak) @constant.builtin       ;; DONE markup

(math "$" @operator)
"#" @operator
"end" @operator

(escape) @constant.character.escape
["(" ")" "{" "}"] @ponctuation.bracket
["," ";" ".." ":" "sep"] @ponctuation.delimiter
"assign" @ponctuation
(field "." @ponctuation)
