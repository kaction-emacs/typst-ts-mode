// This file only serves for testing highlight, and it is not a syntax completion test. 

// comment

-? $ -> $ // shorthand

// header face (current cannot cope with tailing comment)
= headline
https://www.google.com // url
_emph_ // emphasis
*strong* // strong
// item (current cannot cope with tailing comment)
- item 
/ term1: value
"El Psy Kongraoo" // quote
hello \ // line break
`El Psy Kongraoo` // raw span
```bash // raw block
sudo rm -rf *
```
<label> // label
@reference // reference

#(4.2) // number
#"El Psy Kongaroo" // string
#[El Psy Kongraoo] // content
#true #false  // boolean
#sym.bar.h  // builtin
#set text(a: 0) // call & builtin
#none // none
#auto // auto
#(a + b) // ident

#(0 in "0" not in a) // in
#(a and b or not c) // and, or, not
#(2 + - 1) #(2 - -1) // sign
#(1 + 1) // add
#(1 - 1) // sub
#(1 * 1) // mul
#(1 / 1) // div
#if 2 > 1 [] // cmp
#import "a": * // wildcard

#let a = b 
