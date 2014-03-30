from pyparsing import *;

# AssignmentChar ::= ":="
AssignmentChar = Literal(":=");

# Underscore ::= "_"
Underscore = Literal("_");

# Colon ::= ":"
Colon = Literal(":");

# SemiColon ::= ";"
SemiColon = Literal(";");

# Minus ::= "-"
Minus = Literal("-")

# Dot ::= "."
Dot = Literal(".")

# NonZeroNum ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
# NOTE: I set the first "1" explicity as Literal, so that the | operator
#       coherces the following numbers.
NonZeroNum = Literal("1") | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

# Num ::= "0" | NonZeroNum
Num = "0" | NonZeroNum

# DecimalNumber ::= { Num }+
DecimalNumber = Combine( OneOrMore( Num ) );

# Number ::= ( NonZeroNum { Num } ) | "0"
Number = Combine( NonZeroNum + ZeroOrMore( Num ) | "0" );

# Hex ::= Num | "A" | "B" | "C" | "D" | "E" | "F" | "a" | "b" | "c" | "d" | "e" | "f"
Hex = Num | "A" | "B" | "C" | "D" | "E" | "F" | "a" | "b" | "c" | "d" | "e" | "f";

# Hstring ::= "'" { Hex } "'" "H"
Hstring = Combine("'" + ZeroOrMore( Hex ) + "'" + "H");

# Oct ::= Hex Hex
Oct = Hex + Hex

# Ostring ::= "'" { Oct } "'" "O"
Ostring = Combine("'" + OneOrMore( Oct ) + "'" + "O");

# Bin ::= "0" | "1"
Bin = Literal("0") | Literal("1");

# Bstring ::= "'" { Bin } "'" "B"
Bstring = Combine("'" + ZeroOrMore( Bin ) + "'" + "B");

# LowerAlpha ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
LowerAlpha = Literal("a") | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z";

# UpperAlpha ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
UpperAlpha = Literal("A") | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z";

# Alpha ::= UpperAlpha | LowerAlpha
Alpha = UpperAlpha | LowerAlpha;

# AlphaNum ::= Alpha | Num
AlphaNum = Alpha | Num;

# Identifier ::= Alpha { AlphaNum | Underscore }
Identifier = Combine( Alpha + ZeroOrMore( AlphaNum | Underscore ) );

# QualifiedIdentifier ::= { Identifier Dot } Identifier
QualifiedIdentifier = Combine( ZeroOrMore( Identifier + Dot ) + Identifier );

# QualifiedIdentifierList   ::=    QualifiedIdentifier { "," QualifiedIdentifier }
QualifiedIdentifierList = delimitedList( QualifiedIdentifier );

# IdentifierList   ::=    Identifier { "," Identifier }
IdentifierList = delimitedList( Identifier );

# ExtendedIdentifier     ::=     [ Identifier Dot ] Identifier
ExtendedIdentifier = Combine( Optional( Identifier + Dot ) + Identifier );

# CaseKeyword ::= "case"
CaseKeyword = Keyword("case");

# SelectCase ::= CaseKeyword ( "(" InLineTemplate { "," InLineTemplate } ")" | ElseKeyword ) StatementBlock
InLineTemplate = Forward();
ElseKeyword = Forward();
StatementBlock = Forward();
SelectCase = CaseKeyword + ( "(" + delimitedList( InLineTemplate ) + ")" | ElseKeyword ) + StatementBlock;

# SelectCaseBody ::= "{" { SelectCase }+ "}"
SelectCaseBody = "{" + Group( OneOrMore( Group( SelectCase ) ) ) + "}";

# SelectKeyword ::= "select"
SelectKeyword = Keyword("select") ;

# SelectCaseConstruct ::= SelectKeyword "(" SingleExpression ")" SelectCaseBody
SingleExpression = Forward();
SelectCaseConstruct = SelectKeyword + "(" + SingleExpression + ")" + SelectCaseBody;


# ElseKeyword ::= "else"
ElseKeyword << Keyword("else");

# ElseClause ::= ElseKeyword StatementBlock
ElseClause = ElseKeyword + StatementBlock;

# ElseIfClause ::= ElseKeyword IfKeyword "(" BooleanExpression ")" StatementBlock
IfKeyword = Forward();
BooleanExpression = Forward();
ElseIfClause = ElseKeyword + IfKeyword + "(" + BooleanExpression + ")" + StatementBlock;

# IfKeyword ::= "if"
IfKeyword << Keyword("if");

# ConditionalConstruct ::= IfKeyword "(" BooleanExpression ")" StatementBlock { ElseIfClause } [ ElseClause ]
ConditionalConstruct = Group( IfKeyword + "(" + BooleanExpression + ")" + StatementBlock ) + ZeroOrMore( Group( ElseIfClause ) ) + Optional( Group( ElseClause ) );

# DoKeyword ::= "do"
DoKeyword = Keyword("do");

# DoWhileStatement ::= DoKeyword StatementBlock WhileKeyword "(" BooleanExpression ")"
WhileKeyword = Forward();
DoWhileStatement = DoKeyword + StatementBlock + WhileKeyword + "(" + BooleanExpression + ")";

# WhileKeyword ::= "while"
WhileKeyword << Keyword("while")

# WhileStatement ::= WhileKeyword "(" BooleanExpression ")" StatementBlock
# Initial ::= VarInstance | Assignment
# ForKeyword ::= "for"
# ForStatement ::= ForKeyword "(" Initial SemiColon BooleanExpression SemiColon Assignment ")" StatementBlock
# LoopConstruct ::= ForStatement | WhileStatement | DoWhileStatement
# LogItem ::= FreeText | InLineTemplate
#TMP
InLineTemplate << ( Literal("42") | Identifier ); # TEMP
ElseKeyword << Keyword("else");
StatementBlock << ( Literal("{") + Literal("}") ); # TEMP
SingleExpression << Literal("expression"); 
BooleanExpression << Literal("isExpression"); 
# LogKeyword ::= "log"
# LogStatement ::= LogKeyword "(" LogItem { "," LogItem } ")"
# ShiftOp ::= "<<" | ">>" | "<@" | "@>"
# StringOp ::= "&"
# EqualOp ::= "==" | "!="
# RelOp ::= "<" | ">" | ">=" | "<="
# UnaryOp ::= "+" | "-"
# MultiplyOp ::= "*" | "/" | "mod" | "rem"
# AddOp ::= "+" | "-" | StringOp
# OpCall ::= ConfigurationOps | GetLocalVerdict | TimerOps | TestcaseInstance | ( FunctionInstance [ ExtendedFieldReference ] ) | ( TemplateOps [ ExtendedFieldReference ] ) | ActivateOp
# ExtendedFieldReference ::= { ( Dot ( Identifier | PredefinedType ) ) | ArrayOrBitRef | ( "[" Minus "]" ) }+
# Primary ::= OpCall | Value | "(" SingleExpression ")"
# UnaryExpression ::= [ UnaryOp ] Primary
# MulExpression ::= UnaryExpression { MultiplyOp UnaryExpression } | CompoundExpression
# AddExpression ::= MulExpression { AddOp MulExpression }
# BitNotExpression ::= [ "not4b" ] AddExpression
# BitAndExpression ::= BitNotExpression { "and4b" BitNotExpression }
# BitXorExpression ::= BitAndExpression { "xor4b" BitAndExpression }
# BitOrExpression ::= BitXorExpression { "or4b" BitXorExpression }
# ShiftExpression ::= BitOrExpression { ShiftOp BitOrExpression }
# RelExpression ::= ShiftExpression [ RelOp ShiftExpression ] | CompoundExpression
# EqualExpression ::= RelExpression { EqualOp RelExpression }
# NotExpression ::= [ "not" ] EqualExpression
# AndExpression ::= NotExpression { "and" NotExpression }
# XorExpression ::= AndExpression { "xor" AndExpression }
# SingleExpression ::= XorExpression { "or" XorExpression }
# Assignment ::= VariableRef AssignmentChar ( Expression | TemplateBody )
# ArrayElementConstExpressionList ::= ConstantExpression { "," ConstantExpression }
# ArrayConstExpression ::= "{" [ ArrayElementConstExpressionList ] "}"
# FieldConstExpressionSpec ::= FieldReference AssignmentChar ConstantExpression
# FieldConstExpressionList ::= "{" FieldConstExpressionSpec { "," FieldConstExpressionSpec } "}"
# CompoundConstExpression ::= FieldConstExpressionList | ArrayConstExpression
# BooleanExpression ::= SingleExpression
# ConstantExpression ::= SingleExpression | CompoundConstExpression
# NotUsedOrExpression ::= Expression | Minus
# ArrayElementExpressionList ::= NotUsedOrExpression { "," NotUsedOrExpression }
# ArrayExpression ::= "{" [ ArrayElementExpressionList ] "}"
# FieldExpressionSpec ::= FieldReference AssignmentChar NotUsedOrExpression
# FieldExpressionList ::= "{" FieldExpressionSpec { "," FieldExpressionSpec } "}"
# CompoundExpression ::= FieldExpressionList | ArrayExpression
# Expression ::= SingleExpression | CompoundExpression
# BasicStatements ::= Assignment | LogStatement | LoopConstruct | ConditionalConstruct | SelectCaseConstruct | StatementBlock
# ContinueStatement ::= "continue"
# BreakStatement ::= "break"
# DeactivateKeyword ::= "deactivate"
# DeactivateStatement ::= DeactivateKeyword [ "(" ComponentOrDefaultReference ")" ]
# ActivateKeyword ::= "activate"
# ActivateOp ::= ActivateKeyword "(" AltstepInstance ")"
# RepeatStatement ::= "repeat"
# GotoKeyword ::= "goto"
# GotoStatement ::= GotoKeyword Identifier
# LabelKeyword ::= "label"
# LabelStatement ::= LabelKeyword Identifier
# InterleavedGuard ::= "[" "]" GuardOp
# InterleavedGuardElement ::= InterleavedGuard StatementBlock
# InterleavedGuardList ::= { InterleavedGuardElement [ SemiColon ] }+
# InterleavedKeyword ::= "interleave"
# InterleavedConstruct ::= InterleavedKeyword "{" InterleavedGuardList "}"
# GuardOp ::= TimeoutStatement | ReceiveStatement | TriggerStatement | GetCallStatement | CatchStatement | CheckStatement | GetReplyStatement | DoneStatement | KilledStatement
# AltGuardChar ::= "[" [ BooleanExpression ] "]"
# ElseStatement ::= "[" ElseKeyword "]" StatementBlock
# GuardStatement ::= AltGuardChar ( AltstepInstance [ StatementBlock ] | GuardOp StatementBlock )
# AltGuardList ::= { GuardStatement | ElseStatement [ SemiColon ] }
# AltKeyword ::= "alt"
# AltConstruct ::= AltKeyword "{" AltGuardList "}"
# ReturnStatement ::= ReturnKeyword [ Expression | InLineTemplate ]
# ActionText ::= FreeText | Expression
# ActionKeyword ::= "action"
# SUTStatements ::= ActionKeyword "(" ActionText { StringOp ActionText } ")"
# GetLocalVerdict ::= "getverdict"
# SetVerdictKeyword ::= "setverdict"
# SetLocalVerdict ::= SetVerdictKeyword "(" SingleExpression { "," LogItem } ")"
# BehaviourStatements ::= TestcaseInstance | FunctionInstance | ReturnStatement | AltConstruct | InterleavedConstruct | LabelStatement | GotoStatement | RepeatStatement | DeactivateStatement | AltstepInstance | ActivateOp | BreakStatement | ContinueStatement
# AllRef ::= ( GroupKeyword AllKeyword [ ExceptKeyword "{" QualifiedIdentifierList "}" ] ) | ( ( TypeDefKeyword | TemplateKeyword | ConstKeyword | AltstepKeyword | TestcaseKeyword | FunctionKeyword | SignatureKeyword | ModuleParKeyword ) AllKeyword [ ExceptKeyword "{" IdentifierList "}" ] )
# DefOrFieldRef ::= QualifiedIdentifier | ( ( FieldReference | "[" Minus "]" ) [ ExtendedFieldReference ] ) | AllRef
# DefOrFieldRefList ::= DefOrFieldRef { "," DefOrFieldRef }
# AttribQualifier ::= "(" DefOrFieldRefList ")"
# OverrideKeyword ::= "override"
# ExtensionKeyword ::= "extension"
# DisplayKeyword ::= "display"
# VariantKeyword ::= "variant"
# EncodeKeyword ::= "encode"
# AttribKeyword ::= EncodeKeyword | VariantKeyword | DisplayKeyword | ExtensionKeyword | OptionalKeyword
# SingleWithAttrib ::= AttribKeyword [ OverrideKeyword ] [ AttribQualifier ] FreeText
# MultiWithAttrib ::= { SingleWithAttrib [ SemiColon ] }
# WithAttribList ::= "{" MultiWithAttrib "}"
# WithKeyword ::= "with"
# WithStatement ::= WithKeyword WithAttribList
# TemplateRestriction ::= "(" ( OmitKeyword | ValueKeyword | PresentKeyword ) ")"
# RestrictedTemplate ::= OmitKeyword | ( TemplateKeyword TemplateRestriction )
# FormalTemplatePar ::= [ ( InParKeyword | OutParKeyword | InOutParKeyword ) ] ( TemplateKeyword | RestrictedTemplate ) Type Identifier [ ":=" ( InLineTemplate | Minus ) ]
# FormalTimerPar ::= [ InOutParKeyword ] TimerKeyword Identifier
# FormalPortPar ::= [ InOutParKeyword ] Identifier Identifier
# FormalValuePar ::= [ ( InParKeyword | InOutParKeyword | OutParKeyword ) ] Type Identifier [ ":=" ( Expression | Minus ) ]
# InOutParKeyword ::= "inout"
# OutParKeyword ::= "out"
# InParKeyword ::= "in"
# OmitKeyword ::= "omit"
# AddressValue ::= "null"
# FreeText ::= """ { ExtendedAlphaNum } """
# ExtendedAlphaNum ::= 
# Char ::= 
# Cstring ::= """ { Char } """
# ReferencedValue ::= ExtendedIdentifier [ ExtendedFieldReference ]
# Exponential ::= "E"
# FloatENotation ::= Number [ Dot DecimalNumber ] Exponential [ Minus ] Number
# FloatDotNotation ::= Number Dot DecimalNumber
# NaNKeyword ::= "not_a_number"
# FloatValue ::= FloatDotNotation | FloatENotation | NaNKeyword
# CharKeyword ::= "char"
# Quadruple ::= CharKeyword "(" Number "," Number "," Number "," Number ")"
# CharStringValue ::= Cstring | Quadruple
# VerdictTypeValue ::= "pass" | "fail" | "inconc" | "none" | "error"
# BooleanValue ::= "true" | "false"
# PredefinedValue ::= Bstring | BooleanValue | CharStringValue | Number | Ostring | Hstring | VerdictTypeValue | Identifier | FloatValue | AddressValue | OmitKeyword
# Value ::= PredefinedValue | ReferencedValue
# ArrayDef ::= { "[" SingleExpression [ ".." SingleExpression ] "]" }+
# TypeReference ::= Identifier
# ReferencedType ::= ExtendedIdentifier [ ExtendedFieldReference ]
# UniversalKeyword ::= "universal"
# UniversalCharString ::= UniversalKeyword CharStringKeyword
# CharStringKeyword ::= "charstring"
# AnyTypeKeyword ::= "anytype"
# DefaultKeyword ::= "default"
# AddressKeyword ::= "address"
# FloatKeyword ::= "float"
# VerdictTypeKeyword ::= "verdicttype"
# HexStringKeyword ::= "hexstring"
# OctetStringKeyword ::= "octetstring"
# IntegerKeyword ::= "integer"
# BooleanKeyword ::= "boolean"
# BitStringKeyword ::= "bitstring"
# PredefinedType ::= BitStringKeyword | BooleanKeyword | CharStringKeyword | UniversalCharString | IntegerKeyword | OctetStringKeyword | HexStringKeyword | VerdictTypeKeyword | FloatKeyword | AddressKeyword | DefaultKeyword | AnyTypeKeyword
# Type ::= PredefinedType | ReferencedType
# TestcaseOperation ::= TestcaseKeyword "." StopKeyword [ "(" { ( FreeText | InLineTemplate ) [ "," ] } ")" ]
# TimeoutKeyword ::= "timeout"
# TimerRefOrAny ::= ArrayIdentifierRef | ( AnyKeyword TimerKeyword )
# TimeoutStatement ::= TimerRefOrAny Dot TimeoutKeyword
# RunningTimerOp ::= TimerRefOrAny Dot RunningKeyword
# ReadKeyword ::= "read"
# ReadTimerOp ::= ArrayIdentifierRef Dot ReadKeyword
# TimerRefOrAll ::= ArrayIdentifierRef | AllKeyword TimerKeyword
# StopTimerStatement ::= TimerRefOrAll Dot StopKeyword
# StartTimerStatement ::= ArrayIdentifierRef Dot StartKeyword [ "(" Expression ")" ]
# TimerOps ::= ReadTimerOp | RunningTimerOp
# TimerStatements ::= StartTimerStatement | StopTimerStatement | TimeoutStatement
# CheckStateKeyword ::= "checkstate"
# PortOrAllAny ::= PortOrAll | AnyKeyword PortKeyword
# CheckStateStatement ::= PortOrAllAny Dot CheckStateKeyword "(" SingleExpression ")"
# AnyKeyword ::= "any"
# HaltKeyword ::= "halt"
# HaltStatement ::= PortOrAll Dot HaltKeyword
# StopKeyword ::= "stop"
# StopStatement ::= PortOrAll Dot StopKeyword
# StartStatement ::= PortOrAll Dot StartKeyword
# ClearOpKeyword ::= "clear"
# PortOrAll ::= ArrayIdentifierRef | AllKeyword PortKeyword
# ClearStatement ::= PortOrAll Dot ClearOpKeyword
# CatchOpParameter ::= Signature "," InLineTemplate | TimeoutKeyword
# CatchOpKeyword ::= "catch"
# PortCatchOp ::= CatchOpKeyword [ "(" CatchOpParameter ")" ] [ FromClause ] [ PortRedirect ]
# CatchStatement ::= PortOrAny Dot PortCatchOp
# CheckPortOpsPresent ::= PortReceiveOp | PortGetCallOp | PortGetReplyOp | PortCatchOp
# RedirectPresent ::= PortRedirectSymbol SenderSpec
# FromClausePresent ::= FromClause [ PortRedirectSymbol SenderSpec ]
# CheckParameter ::= CheckPortOpsPresent | FromClausePresent | RedirectPresent
# CheckOpKeyword ::= "check"
# PortCheckOp ::= CheckOpKeyword [ "(" CheckParameter ")" ]
# CheckStatement ::= PortOrAny Dot PortCheckOp
# ValueMatchSpec ::= ValueKeyword InLineTemplate
# GetReplyOpKeyword ::= "getreply"
# RedirectWithValueAndParamSpec ::= ValueSpec [ ParamSpec ] [ SenderSpec ] | RedirectWithParamSpec
# PortRedirectWithValueAndParam ::= PortRedirectSymbol RedirectWithValueAndParamSpec
# PortGetReplyOp ::= GetReplyOpKeyword [ "(" InLineTemplate [ ValueMatchSpec ] ")" ] [ FromClause ] [ PortRedirectWithValueAndParam ]
# GetReplyStatement ::= PortOrAny Dot PortGetReplyOp
# VariableEntry ::= VariableRef | Minus
# VariableList ::= VariableEntry { "," VariableEntry }
# VariableAssignment ::= VariableRef AssignmentChar Identifier
# AssignmentList ::= VariableAssignment { "," VariableAssignment }
# ParamAssignmentList ::= "(" ( AssignmentList | VariableList ) ")"
# ParamKeyword ::= "param"
# ParamSpec ::= ParamKeyword ParamAssignmentList
# RedirectWithParamSpec ::= ParamSpec [ SenderSpec ] | SenderSpec
# PortRedirectWithParam ::= PortRedirectSymbol RedirectWithParamSpec
# GetCallOpKeyword ::= "getcall"
# PortGetCallOp ::= GetCallOpKeyword [ "(" InLineTemplate ")" ] [ FromClause ] [ PortRedirectWithParam ]
# GetCallStatement ::= PortOrAny Dot PortGetCallOp
# TriggerOpKeyword ::= "trigger"
# PortTriggerOp ::= TriggerOpKeyword [ "(" InLineTemplate ")" ] [ FromClause ] [ PortRedirect ]
# TriggerStatement ::= PortOrAny Dot PortTriggerOp
# SenderKeyword ::= "sender"
# SenderSpec ::= SenderKeyword VariableRef
# ValueKeyword ::= "value"
# SingleValueSpec ::= VariableRef [ AssignmentChar FieldReference ExtendedFieldReference ]
# ValueSpec ::= ValueKeyword ( VariableRef | ( "(" SingleValueSpec { "," SingleValueSpec } ")" ) )
# PortRedirectSymbol ::= "->"
# PortRedirect ::= PortRedirectSymbol ( ValueSpec [ SenderSpec ] | SenderSpec )
# FromKeyword ::= "from"
# FromClause ::= FromKeyword ( InLineTemplate | AddressRefList | AnyKeyword ComponentKeyword )
# ReceiveOpKeyword ::= "receive"
# PortReceiveOp ::= ReceiveOpKeyword [ "(" InLineTemplate ")" ] [ FromClause ] [ PortRedirect ]
# PortOrAny ::= ArrayIdentifierRef | AnyKeyword PortKeyword
# ReceiveStatement ::= PortOrAny Dot PortReceiveOp
# RaiseKeyword ::= "raise"
# PortRaiseOp ::= RaiseKeyword "(" Signature "," InLineTemplate ")" [ ToClause ]
# RaiseStatement ::= ArrayIdentifierRef Dot PortRaiseOp
# ReplyValue ::= ValueKeyword Expression
# ReplyKeyword ::= "reply"
# PortReplyOp ::= ReplyKeyword "(" InLineTemplate [ ReplyValue ] ")" [ ToClause ]
# ReplyStatement ::= ArrayIdentifierRef Dot PortReplyOp
# CallBodyOps ::= GetReplyStatement | CatchStatement
# CallBodyGuard ::= AltGuardChar CallBodyOps
# CallBodyStatement ::= CallBodyGuard StatementBlock
# CallBodyStatementList ::= { CallBodyStatement [ SemiColon ] }+
# PortCallBody ::= "{" CallBodyStatementList "}"
# NowaitKeyword ::= "nowait"
# CallTimerValue ::= Expression | NowaitKeyword
# CallParameters ::= InLineTemplate [ "," CallTimerValue ]
# CallOpKeyword ::= "call"
# PortCallOp ::= CallOpKeyword "(" CallParameters ")" [ ToClause ]
# CallStatement ::= ArrayIdentifierRef Dot PortCallOp [ PortCallBody ]
# ToKeyword ::= "to"
# AddressRefList ::= "(" InLineTemplate { "," InLineTemplate } ")"
# ToClause ::= ToKeyword ( InLineTemplate | AddressRefList | AllKeyword ComponentKeyword )
# SendOpKeyword ::= "send"
# PortSendOp ::= SendOpKeyword "(" InLineTemplate ")" [ ToClause ]
# SendStatement ::= ArrayIdentifierRef Dot PortSendOp
# CommunicationStatements ::= SendStatement | CallStatement | ReplyStatement | RaiseStatement | ReceiveStatement | TriggerStatement | GetCallStatement | GetReplyStatement | CatchStatement | CheckStatement | ClearStatement | StartStatement | StopStatement | HaltStatement | CheckStateStatement
# KillKeyword ::= "kill"
# ComponentOrDefaultReference ::= VariableRef | FunctionInstance
# KillTCStatement ::= KillKeyword | ( ( ComponentReferenceOrLiteral | AllKeyword ComponentKeyword ) Dot KillKeyword )
# ComponentReferenceOrLiteral ::= ComponentOrDefaultReference | MTCKeyword | SelfOp
# StopTCStatement ::= StopKeyword | ( ComponentReferenceOrLiteral | AllKeyword ComponentKeyword ) Dot StopKeyword
# StartKeyword ::= "start"
# StartTCStatement ::= ComponentOrDefaultReference Dot StartKeyword "(" FunctionInstance ")"
# UnmapKeyword ::= "unmap"
# UnmapStatement ::= UnmapKeyword [ SingleConnectionSpec [ ParamClause ] | AllConnectionsSpec [ ParamClause ] | AllPortsSpec | AllCompsAllPortsSpec ]
# MapKeyword ::= "map"
# ParamClause ::= ParamKeyword FunctionActualParList
# MapStatement ::= MapKeyword SingleConnectionSpec [ ParamClause ]
# DisconnectKeyword ::= "disconnect"
# AllCompsAllPortsSpec ::= "(" AllKeyword ComponentKeyword ":" AllKeyword PortKeyword ")"
# AllPortsSpec ::= "(" ComponentRef ":" AllKeyword PortKeyword ")"
# AllConnectionsSpec ::= "(" PortRef ")"
# DisconnectStatement ::= DisconnectKeyword [ SingleConnectionSpec | AllConnectionsSpec | AllPortsSpec | AllCompsAllPortsSpec ]
# ComponentRefAssignment ::= Identifier ":=" ComponentRef
# ComponentRef ::= ComponentOrDefaultReference | SystemKeyword | SelfOp | MTCKeyword
# PortRef ::= ComponentRef Colon ArrayIdentifierRef
# SingleConnectionSpec ::= "(" PortRef "," PortRef ")"
# ConnectKeyword ::= "connect"
# ConnectStatement ::= ConnectKeyword SingleConnectionSpec
# AliveKeyword ::= "alive"
# CreateKeyword ::= "create"
# AliveOp ::= ComponentId Dot AliveKeyword
# RunningKeyword ::= "running"
# RunningOp ::= ComponentId Dot RunningKeyword
# KilledKeyword ::= "killed"
# DoneKeyword ::= "done"
# ComponentId ::= ComponentOrDefaultReference | ( AnyKeyword | AllKeyword ) ComponentKeyword
# KilledStatement ::= ComponentId Dot KilledKeyword
# DoneStatement ::= ComponentId Dot DoneKeyword
# SelfOp ::= "self"
# CreateOp ::= ComponentType Dot CreateKeyword [ "(" ( SingleExpression | Minus ) [ "," SingleExpression ] ")" ] [ AliveKeyword ]
# ConfigurationOps ::= CreateOp | SelfOp | SystemKeyword | MTCKeyword | RunningOp | AliveOp
# ConfigurationStatements ::= ConnectStatement | MapStatement | DisconnectStatement | UnmapStatement | DoneStatement | KilledStatement | StartTCStatement | StopTCStatement | KillTCStatement
# ArrayIdentifierRef ::= Identifier { ArrayOrBitRef }
# TimerKeyword ::= "timer"
# TimerInstance ::= TimerKeyword VarList
# VariableRef ::= Identifier [ ExtendedFieldReference ]
# SingleTempVarInstance ::= Identifier [ ArrayDef ] [ AssignmentChar TemplateBody ]
# TempVarList ::= SingleTempVarInstance { "," SingleTempVarInstance }
# VarKeyword ::= "var"
# SingleVarInstance ::= Identifier [ ArrayDef ] [ AssignmentChar Expression ]
# VarList ::= SingleVarInstance { "," SingleVarInstance }
# VarInstance ::= VarKeyword ( ( Type VarList ) | ( ( TemplateKeyword | RestrictedTemplate ) Type TempVarList ) )
# ControlStatement ::= TimerStatements | BasicStatements | BehaviourStatements | SUTStatements | StopKeyword
# ControlStatementOrDef ::= ( FunctionLocalDef | FunctionLocalInst ) [ WithStatement ] | ControlStatement
# ControlStatementOrDefList ::= { ControlStatementOrDef [ SemiColon ] }+
# ModuleControlBody ::= [ ControlStatementOrDefList ]
# ControlKeyword ::= "control"
# ModuleControlPart ::= ControlKeyword "{" ModuleControlBody "}" [ WithStatement ] [ SemiColon ]
# FriendModuleDef ::= "friend" "module" IdentifierList [ SemiColon ]
# ModuleParList ::= Identifier [ AssignmentChar ConstantExpression ] { "," Identifier [ AssignmentChar ConstantExpression ] }
# ModulePar ::= Type ModuleParList
# MultitypedModuleParList ::= { ModulePar [ SemiColon ] }
# ModuleParKeyword ::= "modulepar"
# ModuleParDef ::= ModuleParKeyword ( ModulePar | ( "{" MultitypedModuleParList "}" ) )
# ExtConstDef ::= ExtKeyword ConstKeyword Type IdentifierList
# ExtKeyword ::= "external"
# ExtFunctionDef ::= ExtKeyword FunctionKeyword Identifier "(" [ FunctionFormalParList ] ")" [ ReturnType ]
# GroupKeyword ::= "group"
# GroupDef ::= GroupKeyword Identifier "{" [ ModuleDefinitionsList ] "}"
# ImportImportSpec ::= ImportKeyword AllKeyword
# ImportModuleParSpec ::= ModuleParKeyword IdentifierListOrAllWithExcept
# ImportSignatureSpec ::= SignatureKeyword IdentifierListOrAllWithExcept
# ImportFunctionSpec ::= FunctionKeyword IdentifierListOrAllWithExcept
# ImportTestcaseSpec ::= TestcaseKeyword IdentifierListOrAllWithExcept
# ImportAltstepSpec ::= AltstepKeyword IdentifierListOrAllWithExcept
# ImportConstSpec ::= ConstKeyword IdentifierListOrAllWithExcept
# ImportTemplateSpec ::= TemplateKeyword IdentifierListOrAllWithExcept
# AllWithExcept ::= AllKeyword [ ExceptKeyword IdentifierList ]
# ImportTypeDefSpec ::= TypeDefKeyword IdentifierListOrAllWithExcept
# IdentifierListOrAllWithExcept ::= IdentifierList | AllWithExcept
# QualifiedIdentifierWithExcept ::= QualifiedIdentifier [ ExceptsDef ]
# AllGroupsWithExcept ::= AllKeyword [ ExceptKeyword QualifiedIdentifierList ]
# GroupRefListWithExcept ::= QualifiedIdentifierWithExcept { "," QualifiedIdentifierWithExcept }
# ImportGroupSpec ::= GroupKeyword ( GroupRefListWithExcept | AllGroupsWithExcept )
# RecursiveKeyword ::= "recursive"
# ImportFromSpec ::= FromKeyword ModuleId [ RecursiveKeyword ]
# ImportElement ::= ImportGroupSpec | ImportTypeDefSpec | ImportTemplateSpec | ImportConstSpec | ImportTestcaseSpec | ImportAltstepSpec | ImportFunctionSpec | ImportSignatureSpec | ImportModuleParSpec | ImportImportSpec
# ImportSpec ::= { ImportElement [ SemiColon ] }
# ExceptModuleParSpec ::= ModuleParKeyword IdentifierListOrAll
# ExceptSignatureSpec ::= SignatureKeyword IdentifierListOrAll
# ExceptFunctionSpec ::= FunctionKeyword IdentifierListOrAll
# ExceptAltstepSpec ::= AltstepKeyword IdentifierListOrAll
# ExceptTestcaseSpec ::= TestcaseKeyword IdentifierListOrAll
# ExceptConstSpec ::= ConstKeyword IdentifierListOrAll
# ExceptTemplateSpec ::= TemplateKeyword IdentifierListOrAll
# ExceptTypeDefSpec ::= TypeDefKeyword IdentifierListOrAll
# IdentifierListOrAll ::= IdentifierList | AllKeyword
# ExceptGroupSpec ::= GroupKeyword ( QualifiedIdentifierList | AllKeyword )
# ExceptElement ::= ExceptGroupSpec | ExceptTypeDefSpec | ExceptTemplateSpec | ExceptConstSpec | ExceptTestcaseSpec | ExceptAltstepSpec | ExceptFunctionSpec | ExceptSignatureSpec | ExceptModuleParSpec
# ExceptSpec ::= { ExceptElement [ SemiColon ] }
# ExceptKeyword ::= "except"
# ExceptsDef ::= ExceptKeyword "{" ExceptSpec "}"
# AllWithExcepts ::= AllKeyword [ ExceptsDef ]
# ImportKeyword ::= "import"
# ImportDef ::= ImportKeyword ImportFromSpec ( AllWithExcepts | ( "{" ImportSpec "}" ) )
# AltstepInstance ::= ExtendedIdentifier "(" [ FunctionActualParList ] ")"
# AltstepLocalDef ::= VarInstance | TimerInstance | ConstDef | TemplateDef
# AltstepLocalDefList ::= { AltstepLocalDef [ WithStatement ] [ SemiColon ] }
# AltstepKeyword ::= "altstep"
# AltstepDef ::= AltstepKeyword Identifier "(" [ FunctionFormalParList ] ")" [ RunsOnSpec ] "{" AltstepLocalDefList AltGuardList "}"
# TestcaseActualParList ::= ( TemplateInstanceActualPar { "," TemplateInstanceActualPar } ) | ( TemplateInstanceAssignment { "," TemplateInstanceAssignment } )
# ExecuteKeyword ::= "execute"
# TestcaseInstance ::= ExecuteKeyword "(" ExtendedIdentifier "(" [ TestcaseActualParList ] ")" [ "," ( Expression | Minus ) [ "," SingleExpression ] ] ")"
# SystemKeyword ::= "system"
# SystemSpec ::= SystemKeyword ComponentType
# ConfigSpec ::= RunsOnSpec [ SystemSpec ]
# TestcaseKeyword ::= "testcase"
# TestcaseDef ::= TestcaseKeyword Identifier "(" [ TemplateOrValueFormalParList ] ")" ConfigSpec StatementBlock
# NoBlockKeyword ::= "noblock"
# Signature ::= ExtendedIdentifier
# ExceptionKeyword ::= "exception"
# ExceptionSpec ::= ExceptionKeyword "(" TypeList ")"
# SignatureFormalParList ::= FormalValuePar { "," FormalValuePar }
# SignatureKeyword ::= "signature"
# SignatureDef ::= SignatureKeyword Identifier "(" [ SignatureFormalParList ] ")" [ ReturnType | NoBlockKeyword ] [ ExceptionSpec ]
# ArrayIdentifierRefAssignment ::= Identifier ":=" ArrayIdentifierRef
# FunctionActualParAssignment ::= TemplateInstanceAssignment | ComponentRefAssignment | ArrayIdentifierRefAssignment
# FunctionActualPar ::= ArrayIdentifierRef | InLineTemplate | ComponentRef | Minus
# FunctionActualParList ::= ( FunctionActualPar { "," FunctionActualPar } ) | ( FunctionActualParAssignment { "," FunctionActualParAssignment } )
# PreDefFunctionIdentifier ::= Identifier
# FunctionRef ::= [ Identifier Dot ] ( Identifier | PreDefFunctionIdentifier )
# FunctionInstance ::= FunctionRef "(" [ FunctionActualParList ] ")"
# FunctionStatement ::= ConfigurationStatements | TimerStatements | CommunicationStatements | BasicStatements | BehaviourStatements | SetLocalVerdict | SUTStatements | TestcaseOperation
# FunctionLocalDef ::= ConstDef | TemplateDef
# FunctionLocalInst ::= VarInstance | TimerInstance
# FunctionStatementList ::= { FunctionStatement [ SemiColon ] }+
# FunctionDefList ::= { ( FunctionLocalDef | FunctionLocalInst ) [ WithStatement ] [ SemiColon ] }+
# StatementBlock ::= "{" [ FunctionDefList ] [ FunctionStatementList ] "}"
# MTCKeyword ::= "mtc"
# OnKeyword ::= "on"
# RunsKeyword ::= "runs"
# RunsOnSpec ::= RunsKeyword OnKeyword ComponentType
# ReturnKeyword ::= "return"
# ReturnType ::= ReturnKeyword [ TemplateKeyword | RestrictedTemplate ] Type
# FunctionFormalPar ::= FormalValuePar | FormalTimerPar | FormalTemplatePar | FormalPortPar
# FunctionFormalParList ::= FunctionFormalPar { "," FunctionFormalPar }
# FunctionKeyword ::= "function"
# FunctionDef ::= FunctionKeyword Identifier "(" [ FunctionFormalParList ] ")" [ RunsOnSpec ] [ ReturnType ] StatementBlock
# ValueofKeyword ::= "valueof"
# ValueofOp ::= ValueofKeyword "(" InLineTemplate ")"
# MatchKeyword ::= "match"
# MatchOp ::= MatchKeyword "(" Expression "," InLineTemplate ")"
# TemplateOps ::= MatchOp | ValueofOp
# TemplateInstanceActualPar ::= InLineTemplate | Minus
# TemplateActualParList ::= "(" [ ( TemplateInstanceActualPar { "," TemplateInstanceActualPar } ) | ( TemplateInstanceAssignment { "," TemplateInstanceAssignment } ) ] ")"
# DerivedRefWithParList ::= ModifiesKeyword TemplateRefWithParList
# InLineTemplate ::= [ ( Type | Signature ) Colon ] [ DerivedRefWithParList AssignmentChar ] TemplateBody
# TemplateRefWithParList ::= ExtendedIdentifier [ TemplateActualParList ]
# TemplateInstanceAssignment ::= Identifier ":=" InLineTemplate
# InfinityKeyword ::= "infinity"
# Bound ::= ( [ "!" ] SingleExpression ) | ( [ Minus ] InfinityKeyword )
# Range ::= "(" Bound ".." Bound ")"
# PresentKeyword ::= "present"
# IfPresentKeyword ::= "ifpresent"
# WildcardLengthMatch ::= LengthKeyword "(" SingleExpression ")"
# TemplateList ::= "(" TemplateBody { "," TemplateBody }+ ")"
# AnyOrOmit ::= "*"
# AnyValue ::= "?"
# PermutationKeyword ::= "permutation"
# PermutationMatch ::= PermutationKeyword ListOfTemplates
# SupersetKeyword ::= "superset"
# SupersetMatch ::= SupersetKeyword ListOfTemplates
# SubsetKeyword ::= "subset"
# SubsetMatch ::= SubsetKeyword ListOfTemplates
# AllElementsFrom ::= AllKeyword FromKeyword TemplateBody
# TemplateListItem ::= TemplateBody | AllElementsFrom
# ListOfTemplates ::= "(" TemplateListItem { "," TemplateListItem } ")"
# ComplementKeyword ::= "complement"
# Complement ::= ComplementKeyword ListOfTemplates
# PatternQuadruple ::= "\" "q" "(" Number "," Number "," Number "," Number ")"
# EscapedPatternClassChar ::= "[" | "-" | "^" | "]"
# NonSpecialPatternClassChar ::= Char
# PatternClassChar ::= NonSpecialPatternClassChar | PatternQuadruple | "\" EscapedPatternClassChar
# NonSpecialPatternChar ::= Char
# PatternChar ::= NonSpecialPatternChar | PatternQuadruple
# PatternElement ::= ( ( "\" ( "?" | "*" | "\" | "[" | "]" | "{" | "}" | """ | "|" | "(" | ")" | "#" | "+" | "d" | "w" | "t" | "n" | "r" | "s" | "b" ) ) | ( "?" | "*" | "\" | "|" | "+" ) | ( "[" [ "^" ] [ { PatternClassChar [ "-" PatternClassChar ] } ] "]" ) | ( "{" [ "\" ] ReferencedValue "}" ) | ( "\" "N" "{" ( ReferencedValue | Type ) "}" ) | ( """ """ ) | ( "(" PatternElement ")" ) | ( "#" ( Num | ( "(" Num "," [ Num ] ")" ) | ( "(" "," Num ")" ) ) ) | PatternChar )
# Pattern ::= """ { PatternElement } """
# PatternKeyword ::= "pattern"
# PatternParticle ::= Pattern | ReferencedValue
# CharStringMatch ::= PatternKeyword PatternParticle { "&" PatternParticle }
# OctOrMatch ::= Oct | AnyValue | AnyOrOmit
# OctetStringMatch ::= "'" { OctOrMatch } "'" "O"
# HexOrMatch ::= Hex | AnyValue | AnyOrOmit
# HexStringMatch ::= "'" { HexOrMatch } "'" "H"
# BinOrMatch ::= Bin | AnyValue | AnyOrOmit
# BitStringMatch ::= "'" { BinOrMatch } "'" "B"
# ExtraMatchingAttributes ::= StringLength | IfPresentKeyword | ( StringLength IfPresentKeyword )
# MatchingSymbol ::= Complement | ( AnyValue [ WildcardLengthMatch ] ) | ( AnyOrOmit [ WildcardLengthMatch ] ) | ListOfTemplates | Range | BitStringMatch | HexStringMatch | OctetStringMatch | CharStringMatch | SubsetMatch | SupersetMatch
# ArrayElementSpec ::= Minus | PermutationMatch | TemplateBody
# ArrayElementSpecList ::= ArrayElementSpec { "," ArrayElementSpec }
# ArrayValueOrAttrib ::= "{" [ ArrayElementSpecList ] "}"
# FieldOrBitNumber ::= SingleExpression
# ArrayOrBitRef ::= "[" FieldOrBitNumber "]"
# ParRef ::= Identifier
# StructFieldRef ::= Identifier | PredefinedType | TypeReference
# FieldReference ::= StructFieldRef | ArrayOrBitRef | ParRef
# FieldSpec ::= FieldReference AssignmentChar ( TemplateBody | Minus )
# FieldSpecList ::= "{" FieldSpec { "," FieldSpec } "}"
# SingleTemplateExpression ::= MatchingSymbol | ( TemplateRefWithParList [ ExtendedFieldReference ] )
# SimpleTemplateSpec ::= SingleTemplateExpression [ "&" SimpleSpec ]
# SimpleSpec ::= ( SingleExpression [ "&" SimpleTemplateSpec ] ) | SimpleTemplateSpec
# TemplateBody ::= ( SimpleSpec | FieldSpecList | ArrayValueOrAttrib ) [ ExtraMatchingAttributes ]
# TemplateOrValueFormalPar ::= FormalValuePar | FormalTemplatePar
# TemplateOrValueFormalParList ::= TemplateOrValueFormalPar { "," TemplateOrValueFormalPar }
# ModifiesKeyword ::= "modifies"
# DerivedDef ::= ModifiesKeyword ExtendedIdentifier
# TemplateKeyword ::= "template"
# BaseTemplate ::= ( Type | Signature ) Identifier [ "(" TemplateOrValueFormalParList ")" ]
# TemplateDef ::= TemplateKeyword [ TemplateRestriction ] BaseTemplate [ DerivedDef ] AssignmentChar TemplateBody
# ConstKeyword ::= "const"
# SingleConstDef ::= Identifier [ ArrayDef ] AssignmentChar ConstantExpression
# ConstList ::= SingleConstDef { "," SingleConstDef }
# ConstDef ::= ConstKeyword Type ConstList
# PortElement ::= Identifier [ ArrayDef ]
# PortInstance ::= PortKeyword ExtendedIdentifier PortElement { "," PortElement }
# ComponentElementDef ::= PortInstance | VarInstance | TimerInstance | ConstDef
# ComponentDefList ::= { ComponentElementDef [ WithStatement ] [ SemiColon ] }
# ComponentType ::= ExtendedIdentifier
# ExtendsKeyword ::= "extends"
# ComponentKeyword ::= "component"
# ComponentDef ::= ComponentKeyword Identifier [ ExtendsKeyword ComponentType { "," ComponentType } ] "{" [ ComponentDefList ] "}"
# ProcOrType ::= Signature | Type
# ProcOrTypeList ::= AllKeyword | ( ProcOrType { "," ProcOrType } )
# MixedList ::= Direction ProcOrTypeList
# MixedKeyword ::= "mixed"
# MixedAttribs ::= MixedKeyword "{" { ( AddressDecl | MixedList | ConfigParamDef ) [ SemiColon ] }+ "}"
# SignatureList ::= Signature { "," Signature }
# AllOrSignatureList ::= AllKeyword | SignatureList
# ProcedureList ::= Direction AllOrSignatureList
# ProcedureKeyword ::= "procedure"
# ProcedureAttribs ::= ProcedureKeyword "{" { ( AddressDecl | ProcedureList | ConfigParamDef ) [ SemiColon ] }+ "}"
# TypeList ::= Type { "," Type }
# AllKeyword ::= "all"
# AllOrTypeList ::= AllKeyword | TypeList
# MessageKeyword ::= "message"
# Direction ::= InParKeyword | OutParKeyword | InOutParKeyword
# MessageList ::= Direction AllOrTypeList
# AddressDecl ::= AddressKeyword Type
# UnmapParamDef ::= UnmapKeyword ParamKeyword "(" FormalValuePar { "," FormalValuePar } ")"
# MapParamDef ::= MapKeyword ParamKeyword "(" FormalValuePar { "," FormalValuePar } ")"
# ConfigParamDef ::= MapParamDef | UnmapParamDef
# MessageAttribs ::= MessageKeyword "{" { ( AddressDecl | MessageList | ConfigParamDef ) [ SemiColon ] }+ "}"
# PortDefAttribs ::= MessageAttribs | ProcedureAttribs | MixedAttribs
# PortKeyword ::= "port"
# PortDefBody ::= Identifier PortDefAttribs
# PortDef ::= PortKeyword PortDefBody
# LengthKeyword ::= "length"
# StringLength ::= LengthKeyword "(" SingleExpression [ ".." Bound ] ")"
# RangeDef ::= Bound ".." Bound
# TemplateOrRange ::= RangeDef | TemplateBody | Type
# AllowedValuesSpec ::= "(" ( ( TemplateOrRange { "," TemplateOrRange } ) | CharStringMatch ) ")"
# SubTypeSpec ::= AllowedValuesSpec [ StringLength ] | StringLength
# SubTypeDef ::= Type ( Identifier | AddressKeyword ) [ ArrayDef ] [ SubTypeSpec ]
# Enumeration ::= Identifier [ "(" [ Minus ] Number ")" ]
# EnumerationList ::= Enumeration { "," Enumeration }
# EnumKeyword ::= "enumerated"
# EnumDef ::= EnumKeyword ( Identifier | AddressKeyword ) "{" EnumerationList "}"
# SetOfDef ::= SetKeyword [ StringLength ] OfKeyword StructOfDefBody
# StructOfDefBody ::= ( Type | NestedTypeDef ) ( Identifier | AddressKeyword ) [ SubTypeSpec ]
# OfKeyword ::= "of"
# RecordOfDef ::= RecordKeyword [ StringLength ] OfKeyword StructOfDefBody
# SetKeyword ::= "set"
# SetDef ::= SetKeyword StructDefBody
# UnionFieldDef ::= ( Type | NestedTypeDef ) Identifier [ ArrayDef ] [ SubTypeSpec ]
# UnionDefBody ::= ( Identifier | AddressKeyword ) "{" UnionFieldDef { "," UnionFieldDef } "}"
# UnionKeyword ::= "union"
# UnionDef ::= UnionKeyword UnionDefBody
# OptionalKeyword ::= "optional"
# NestedEnumDef ::= EnumKeyword "{" EnumerationList "}"
# NestedSetOfDef ::= SetKeyword [ StringLength ] OfKeyword ( Type | NestedTypeDef )
# NestedRecordOfDef ::= RecordKeyword [ StringLength ] OfKeyword ( Type | NestedTypeDef )
# NestedSetDef ::= SetKeyword "{" [ StructFieldDef { "," StructFieldDef } ] "}"
# NestedUnionDef ::= UnionKeyword "{" UnionFieldDef { "," UnionFieldDef } "}"
# NestedRecordDef ::= RecordKeyword "{" [ StructFieldDef { "," StructFieldDef } ] "}"
#
#   # NestedTypeDef ::= NestedRecordDef | NestedUnionDef | NestedSetDef | NestedRecordOfDef | NestedSetOfDef | NestedEnumDef
#   NestedTypeDef = NestedRecordDef | NestedUnionDef | NestedSetDef | NestedRecordOfDef | NestedSetOfDef | NestedEnumDef;
#
#   # StructFieldDef ::= ( Type | NestedTypeDef ) Identifier [ ArrayDef ] [ SubTypeSpec ] [ OptionalKeyword ]
#   StructFieldDef = ( Type | NestedTypeDef ) & Identifier & ( ArrayDef )[:1] & ( SubTypeSpec )[:1] & ( OptionalKeyword )[:1];
#
#   # StructDefBody ::= ( Identifier | AddressKeyword ) "{" [ StructFieldDef { "," StructFieldDef } ] "}"
#   StructDefBody = ( Identifier | AddressKeyword ) & Literal("{") & ( StructFieldDef & ( Literal(",") & StructFieldDef )[:] )[:1] & Literal("}");
#
#   # RecordKeyword ::= "record"
#   RecordKeyword = Literal("record");
#
#   # RecordDef ::= RecordKeyword StructDefBody
#   RecordDef = RecordKeyword StructDefBody;
#   
#   # StructuredTypeDef ::= RecordDef | UnionDef | SetDef | RecordOfDef | SetOfDef | EnumDef | PortDef | ComponentDef
#   StructuredTypeDef = RecordDef | UnionDef | SetDef | RecordOfDef | SetOfDef | EnumDef | PortDef | ComponentDef;
#   
#   # TypeDefKeyword ::= "type"
#   TypeDefKeyword = Literal("type");
#   
#   # TypeDefBody ::= StructuredTypeDef | SubTypeDef
#   TypeDefBody = StructuredTypeDef | SubTypeDef;
#   
#   # TypeDef ::= TypeDefKeyword TypeDefBody
#   TypeDef = TypeDefKeyword & TypeDefBody;
#   
#   # Visibility ::= "public" | "friend" | "private"
#   Visibility = Literal("public") | Literal("friend") | Literal("private");
#   
#   # ModuleDefinition ::= ( ( [ Visibility ] ( TypeDef | ConstDef | TemplateDef | ModuleParDef | FunctionDef | SignatureDef | TestcaseDef | AltstepDef | ImportDef | ExtFunctionDef | ExtConstDef ) ) | ( [ "public" ] GroupDef ) | ( [ "private" ] FriendModuleDef ) ) [ WithStatement ]
#   ModuleDefinition = ( ( ( Visibility )[:1] & ( TypeDef | ConstDef | TemplateDef | ModuleParDef | FunctionDef | SignatureDef | TestcaseDef | AltstepDef | ImportDef | ExtFunctionDef | ExtConstDef ) ) | ( ( Literal("public") )[:1] & GroupDef ) | ( ( Literal("private") )[:1] & FriendModuleDef ) ) & ( WithStatement )[:1];
#   
#   # ModuleDefinitionsList ::= { ModuleDefinition [ SemiColon ] }+
#   ModuleDefinitionsList = ( ModuleDefinition & ( SemiColon )[:1] )[1:];
#
#   # LanguageKeyword ::= "language"
#   LanguageKeyword = Literal("language");
#
#   # LanguageSpec ::= LanguageKeyword FreeText { "," FreeText }
#   LanguageSpec = LanguageKeyword & FreeText & ( Literal(",") & FreeText )[:];
#
#   # ModuleId ::= Identifier [ LanguageSpec ]
#   ModuleId = Identifier & ( LanguageSpec )[:1];
#
#   # TTCN3ModuleKeyword ::= "module"
#   TTCN3ModuleKeyword = Literal("module");
#
#   # TTCN3Module ::= TTCN3ModuleKeyword ModuleId "{" [ ModuleDefinitionsList ] [ ModuleControlPart ] "}" [ WithStatement ] [ SemiColon ]
#   TTCN3Module = TTCN3ModuleKeyword & ModuleId & Literal("{") & ( ModuleDefinitionsList )[:1] & ( ModuleControlPart )[:1] & Literal("}") & ( WithStatement )[:1] & ( SemiColon )[:1]

def parse(text):
   print "**>>", (TTCN3Module & Eof()).parse(text);
