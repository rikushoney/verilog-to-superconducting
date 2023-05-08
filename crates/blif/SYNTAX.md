# Syntax rules for BLIF file format

The BLIF file format's syntax is described below using Extended Backus-Naur Form (EBNF) notation.

```
Circuit           ::= Model (EOL Model)* EOL?
Model             ::= ".model" (S+ Ident)? EOL ModelFields EOL Commands (EOL ".end")?

ModelFields       ::= ModelField (EOL ModelField)*
Commands          ::= Command (EOL Command)*

ModelField        ::= (InputsList | OutputsList | ClockList)
InputsList        ::= ".inputs" S+ SignalList
OutputsList       ::= ".outputs" S+ SignalList
ClockList         ::= ".clock" S+ SignalList
SignalList        ::= Ident (S+ Ident)*

Command           ::= LogicGate        |
                      GenericLatch     |
                      LibraryGate      |
                      ModelReference   |
                      SubfileReference |
                      FsmDescription   |
                      ClockConstraint  |
                      DelayConstraint

LogicGate         ::= (".exdc" EOL)? ".names" S+ SignalList EOL SingleOutputCover
SingleOutputCover ::= InputPlane+ OutputPlane (EOL InputPlane+ OutputPlane)*
InputPlane        ::= [01] | DontCare
DontCare          ::= [xX] | "-"
OutputPlane       ::= [01]

GenericLatch      ::= ".latch" S+ Ident S+ Ident S+ LatchKind S+ LatchControl (S+ LogicValue)?
LatchKind         ::= "fe" | "re" | "ah" | "al" | "as"
LatchControl      ::= Ident | "NIL"
LogicValue        ::= [0-3] | DontCare

LibraryGate       ::= GateTechnology | LatchTechnology
GateTechnology    ::= ".gate" S+ Ident S+ FormalActualList
LatchTechnology   ::= ".mlatch" S+ Ident S+ FormalActualList S+ LatchControl (S+ LogicValue)?
FormalActualList  ::= Ident "=" Ident (S+ Ident "=" Ident)*

ModelReference    ::= ".subckt" S+ Ident S+ FormalActualList

SubfileReference  ::= ".search" S+ Ident

FsmDescription    ::= UNIMPLEMENTED
ClockConstraint   ::= UNIMPLEMENTED
DelayConstraint   ::= UNIMPLEMENTED

Ident             ::= ([^#=] - S)+
Comment           ::= '#' [^\n]* \n
S                 ::= [ \t] | "\\\n"
EOL               ::= S* (S* (\n | Comment) S*)+
```
