# AIGER file format syntax

The AIGER file format's syntax is given below using EBNF (Extended Backus-Naur Format) notation.

```
Aiger  ::= ("aag" | "aig") S Header (EOL Body)?
Header ::= Unsigned S Unsigned S Unsigned S Unsigned S Unsigned

Body     ::= Literals? (EOL Latches)? (EOL Literals)? (EOL Gates)?
Literals ::= Unsigned (EOL Unsigned)*
Latches  ::= Latch (EOL Latch)*
Latch    ::= Unsigned S Unsigned
Gates    ::= Gate (EOL Gate)*
Gate     ::= Unsigned S Unsigned S Unsigned

S        ::= " "
NL       ::= "\n"
CR       ::= "\r"
EOL      ::= CR? NL
Unsigned ::= [0-9]+
```
