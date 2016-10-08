# PL/O Ocaml
`plo_ocaml` implements Niklaus Wirth's PL/O programming language in Ocaml. PL/0 is well documented at [wikipedia](https://en.wikipedia.org/wiki/PL/0)

#Implementation Details 
**Lexer** - Lexer is implemented via `ocamllex`.
**Parser** - Parser is a hand-crafted LL(1) parser.
**Backend** - LLVM and NASM assembly(WIP - Work in Progress).
