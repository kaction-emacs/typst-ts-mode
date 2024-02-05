// This file only serves for testing highlight, and it is not a syntax completion test.

// comment

-? $ -> $ // shorthand

// header face
= headline
  https://www.google.com // url
  _emph_ // emphasis
  *strong* // strong
  - item // item
  / term1: value
  "El Psy Kongraoo" // quote
  hello \ // line break
  `El Psy Kongraoo` // raw span
  // raw block
  ```bash
sudo rm -rf *
  ```
  <label> // label
  @reference // reference

  Hello\nWorld // escape


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

  #let a = b  // let
  #if b {} else {} // branch
  #while n < 10 { // while
      (n,)
  }
  #for i in a {} // for
  #import "a": a, b // import
  #import "a.lib" as b // as
  #include "a" // include
  #show: columns.with(2) // show
  #set text(a: 0) // set
  #let a() = { // return
      return 2
  }
  #for letter in "abc nope" { // flow
      if letter == " " {
          break
      } else if letter == "a" {
          continue
      }
      letter
  }

  #a()() // function
  #range().map // builtin function
  #l.zip(r).map( // method
      ((a,b)) => a + b // TODO lambda
  )
  #(a, c: b) // tagged
  #a.b // field

  $ a $ // math
  $ 1 + 1 = 2 $
  $ E = m * c^2 $
  $ eq.not(0) $
  $ cal(A) := { x in RR | x "is natural" } $
