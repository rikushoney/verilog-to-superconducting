# Syntax rules for the BLIF file format

The BLIF file format's syntax is described below using Extended Backus-Naur Form (EBNF) notation.

```
Circuit             ::= Model (EOL Model)* EOL?
Model               ::= ".model" (S+ Ident)? EOL ModelFields EOL Commands (EOL ".end")?

ModelFields         ::= ModelField (EOL ModelField)*
Commands            ::= Command (EOL Command)*

ModelField          ::= InputsList | OutputsList | ClockList
InputsList          ::= ".inputs" S+ SignalList
OutputsList         ::= ".outputs" S+ SignalList
ClockList           ::= ".clock" S+ SignalList
SignalList          ::= Ident (S+ Ident)*

Command             ::= LogicGate        |
                        GenericLatch     |
                        LibraryGate      |
                        ModelReference   |
                        SubfileReference |
                        FsmDescription   |
                        ClockConstraint  |
                        DelayConstraint

LogicGate           ::= (".exdc" EOL)? ".names" S+ SignalList EOL SingleOutputCover
SingleOutputCover   ::= SingleOutput (EOL SingleOutput)*
SingleOutput        ::= InputPlane+ S+ OutputPlane
InputPlane          ::= [01] | "-"
OutputPlane         ::= [01]

GenericLatch        ::= ".latch" S+ Ident S+ Ident S+ LatchKind S+ LatchControl (S+ LogicValue)?
LatchKind           ::= "fe" | "re" | "ah" | "al" | "as"
LatchControl        ::= Ident | "NIL"
LogicValue          ::= [0-3]

LibraryGate         ::= GateTechnology | LatchTechnology
GateTechnology      ::= ".gate" S+ Ident S+ FormalActualList
LatchTechnology     ::= ".mlatch" S+ Ident S+ FormalActualList S+ LatchControl (S+ LogicValue)?
FormalActualList    ::= Ident "=" Ident (S+ Ident "=" Ident)*

ModelReference      ::= ".subckt" S+ Ident S+ FormalActualList

SubfileReference    ::= ".search" S+ Filename
Filename            ::= ("\"" [^"] "\"") | Ident

FsmDescription      ::= ".start_kiss" EOL FsmFields EOL StateMapping EOL ".end_kiss" FsmEnd
FsmFields           ::= NumInputs EOL NumOutputs (EOL NumTerms)? (EOL NumStates)? (EOL ResetState)?
NumInputs           ::= ".i" S+ PosInt
NumOutputs          ::= ".o" S+ PosInt
NumTerms            ::= ".p" S+ PosInt
NumStates           ::= ".s" S+ PosInt
ResetState          ::= ".r" S+ Ident
StateMapping        ::= StateTransition (EOL StateTransition)*
StateTransition     ::= ([01] | DontCare) S+ Ident S+ Ident S+ ([01] | DontCare)
FsmEnd              ::= (EOL LatchOrder)? (EOL CodeMapping)?
LatchOrder          ::= ".latch_order" S+ LatchOrderList
LatchOrderList      ::= Ident (S+ Ident)*
CodeMapping         ::= CodeMap (EOL CodeMap)*
CodeMap             ::= ".code" S+ Ident S+ Ident

ClockConstraint     ::= ".cycle" S+ Number EOL ClockEvents EOL
ClockEvents         ::= ClockEvent (EOL ClockEvent)*
ClockEvent          ::= ".clock_event" S+ Number S+ EventsList
EventsList          ::= Event (S+ Event)*
Event               ::= [rf] "'" Ident (S+ Number S+ Number)?

DelayConstraint     ::= DelayConstraintKind (EOF DelayConstraintKind)*
DelayConstraintKind ::= Area              |
                        Delay             |
                        WireLoadSlope     |
                        Wire              |
                        InputArrival      |
                        DefInputArrival   |
                        OutputRequired    |
                        DefOutputRequired |
                        InputDrive        |
                        DefInputDrive     |
                        MaxInputLoad      |
                        DefMaxInputLoad   |
                        OutputLoad        |
                        DefOutputLoad

Area                ::= ".area" S+ Number
Delay               ::= ".delay" S+ Ident S+ DelayFields
DelayFields         ::= Phase S+ Number S+ Number S+ Number S+ Number S+ Number S+ Number
Phase               ::= "INV" | "NONINV" | "UNKNOWN"
WireLoadSlope       ::= ".wire_load_slope" S+ Number
Wire                ::= ".wire" S+ WireLoadList
WireLoadList        ::= Number (S+ Number)*
InputArrival        ::= ".input_arrival" S+ Ident S+ Number S+ Number (S+ RelativeEvent)?
RelativeEvent       ::= [ba] S+ [rf] "'" Ident
DefInputArrival     ::= ".default_input_arrival" S+ Number S+ Number
OutputRequired      ::= ".output_required" S+ Ident S+ Number S+ Number (S+ RelativeEvent)?
DefOutputRequired   ::= ".default_output_required" S+ Number S+ Number
InputDrive          ::= ".input_drive" S+ Ident S+ Number S+ Number
DefInputDrive       ::= ".default_input_drive" S+ Number S+ Number
MaxInputLoad        ::= ".max_input_load" S+ Ident S+ Number
DefMaxInputLoad     ::= ".default_max_input_load" S+ Number
OutputLoad          ::= ".output_load" S+ Ident S+ Number
DefOutputLoad       ::= ".default_output_load" S+ Number

Number              ::= [0-9]+ ("." [0-9]+)? ([eE] [+-] [0-9]+)?
PosInt              ::= [0-9]+ - "0"
Ident               ::= ([^#=] - S)+
Comment             ::= '#' [^\n]* "\n"
S                   ::= [ \t] | "\\\n"
EOL                 ::= S* (S* ("\n" | Comment) S*)+
```