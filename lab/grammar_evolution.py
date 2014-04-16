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
WhileKeyword << Keyword("while");

# WhileStatement ::= WhileKeyword "(" BooleanExpression ")" StatementBlock
WhileStatement = WhileKeyword + "(" + BooleanExpression + ")" + StatementBlock;

# Initial ::= VarInstance | Assignment
VarInstance = Forward();
Assignment = Forward();
Initial = VarInstance | Assignment;

# ForKeyword ::= "for"
ForKeyword = Keyword("for");

# ForStatement ::= ForKeyword "(" Initial SemiColon BooleanExpression SemiColon Assignment ")" StatementBlock
ForStatement = ForKeyword + "(" + \
                  Group( Group( Initial ).setName("Initial") + \
                  Suppress(SemiColon) + \
                  Group( BooleanExpression ).setName("Condition") + \
                  Suppress(SemiColon) + \
                  Group( Assignment ).setName("Assignment") ) + \
               ")" + StatementBlock;

## LoopConstruct ::= ForStatement | WhileStatement | DoWhileStatement
LoopConstruct = ForStatement | WhileStatement | DoWhileStatement;

# LogItem ::= FreeText | InLineTemplate
FreeText = Forward();
LogItem = FreeText | InLineTemplate;

# LogKeyword ::= "log"
LogKeyword = Keyword("log");

# LogStatement ::= LogKeyword "(" LogItem { "," LogItem } ")"
LogStatement = LogKeyword + "(" + delimitedList( LogItem ) + ")";

# ShiftOp ::= "<<" | ">>" | "<@" | "@>"
ShiftOp = Literal("<<") | Literal(">>") | Literal("<@") | Literal("@>");

# StringOp ::= "&"
StringOp = Literal("&");

# EqualOp ::= "==" | "!="
EqualOp = Literal("==") | Literal("!=");

# RelOp ::= "<" | ">" | ">=" | "<="
RelOp = Literal("<") | Literal(">") | Literal(">=") | Literal("<=");

# UnaryOp ::= "+" | "-"
UnaryOp = Literal("+") | Literal("-");

# MultiplyOp ::= "*" | "/" | "mod" | "rem"
MultiplyOp = Literal("*") | Literal("/") | Keyword("mod") | Keyword("rem");

# AddOp ::= "+" | "-" | StringOp
AddOp = Literal("+") | Literal("-") | StringOp;

# OpCall ::= ConfigurationOps | GetLocalVerdict | TimerOps | TestcaseInstance | ( FunctionInstance [ ExtendedFieldReference ] ) | ( TemplateOps [ ExtendedFieldReference ] ) | ActivateOp
ConfigurationOps = Forward();
GetLocalVerdict = Forward();
TimerOps = Forward();
TestcaseInstance = Forward();
FunctionInstance = Forward();
ExtendedFieldReference = Forward();
TemplateOps = Forward();
ActivateOp = Forward();
OpCall = ConfigurationOps | GetLocalVerdict | TimerOps | TestcaseInstance | ( FunctionInstance + Optional( ExtendedFieldReference ) ) | ( TemplateOps + Optional( ExtendedFieldReference ) ) | ActivateOp;

# ExtendedFieldReference ::= { ( Dot ( Identifier | PredefinedType ) ) | ArrayOrBitRef | ( "[" Minus "]" ) }+
PredefinedType = Forward();
ArrayOrBitRef = Forward();
ExtendedFieldReference << ( OneOrMore( ( Dot + ( Identifier | PredefinedType ) ) | ArrayOrBitRef | ( "[" + Minus + "]" ) ) );

# Primary ::= OpCall | Value | "(" SingleExpression ")"
Value = Forward();
Primary = OpCall | Value | "(" + SingleExpression + ")";

# UnaryExpression ::= [ UnaryOp ] Primary
UnaryExpression = Optional( UnaryOp ) + Primary;

# MulExpression ::= UnaryExpression { MultiplyOp UnaryExpression } | CompoundExpression
CompoundExpression = Forward();
MulExpression = UnaryExpression + ZeroOrMore( MultiplyOp + UnaryExpression ) | CompoundExpression;

# AddExpression ::= MulExpression { AddOp MulExpression }
AddExpression = MulExpression + ZeroOrMore( AddOp + MulExpression );

# BitNotExpression ::= [ "not4b" ] AddExpression
BitNotExpression = Optional( Keyword("not4b") ) + AddExpression;

# BitAndExpression ::= BitNotExpression { "and4b" BitNotExpression }
BitAndExpression = BitNotExpression + ZeroOrMore( Keyword("and4b") + BitNotExpression );

# BitXorExpression ::= BitAndExpression { "xor4b" BitAndExpression }
BitXorExpression = BitAndExpression + ZeroOrMore( Keyword("xor4b") + BitAndExpression );

# BitOrExpression ::= BitXorExpression { "or4b" BitXorExpression }
BitOrExpression = BitXorExpression + ZeroOrMore( Keyword("or4b") + BitXorExpression );

# ShiftExpression ::= BitOrExpression { ShiftOp BitOrExpression }
ShiftExpression = BitOrExpression + ZeroOrMore( ShiftOp + BitOrExpression );

# RelExpression ::= ShiftExpression [ RelOp ShiftExpression ] | CompoundExpression
RelExpression = ShiftExpression + Optional( RelOp + ShiftExpression ) | CompoundExpression;

# EqualExpression ::= RelExpression { EqualOp RelExpression }
EqualExpression = RelExpression + ZeroOrMore( EqualOp + RelExpression );

# NotExpression ::= [ "not" ] EqualExpression
NotExpression = Optional( Keyword("not") ) + EqualExpression;

# AndExpression ::= NotExpression { "and" NotExpression }
AndExpression = NotExpression + ZeroOrMore( Keyword("and") + NotExpression );

# XorExpression ::= AndExpression { "xor" AndExpression }
XorExpression = AndExpression + ZeroOrMore( Keyword("xor") + AndExpression );

# SingleExpression ::= XorExpression { "or" XorExpression }
SingleExpression << ( XorExpression + ZeroOrMore( Keyword("or") + XorExpression ) );

# Assignment ::= VariableRef AssignmentChar ( Expression | TemplateBody )
VariableRef = Forward();
Expression = Forward();
TemplateBody = Forward();
Assignment << ( VariableRef.setName("lhs") + \
                AssignmentChar + \
                ( Expression | TemplateBody ).setName("rhs") );

# ArrayElementConstExpressionList ::= ConstantExpression { "," ConstantExpression }
ConstantExpression = Forward();
ArrayElementConstExpressionList = delimitedList( ConstantExpression );

# ArrayConstExpression ::= "{" [ ArrayElementConstExpressionList ] "}"
ArrayConstExpression = "{" + Optional( ArrayElementConstExpressionList ) + "}";

# FieldConstExpressionSpec ::= FieldReference AssignmentChar ConstantExpression
FieldReference = Forward();
FieldConstExpressionSpec = FieldReference + AssignmentChar + ConstantExpression;

# FieldConstExpressionList ::= "{" FieldConstExpressionSpec { "," FieldConstExpressionSpec } "}"
FieldConstExpressionList = "{" + delimitedList( FieldConstExpressionSpec ) + "}";

# CompoundConstExpression ::= FieldConstExpressionList | ArrayConstExpression
CompoundConstExpression = FieldConstExpressionList | ArrayConstExpression;

# BooleanExpression ::= SingleExpression
BooleanExpression << SingleExpression;

# ConstantExpression ::= SingleExpression | CompoundConstExpression
ConstantExpression << ( SingleExpression | CompoundConstExpression );

# NotUsedOrExpression ::= Expression | Minus
NotUsedOrExpression = Expression | Minus;

# ArrayElementExpressionList ::= NotUsedOrExpression { "," NotUsedOrExpression }
ArrayElementExpressionList = delimitedList( NotUsedOrExpression );

# ArrayExpression ::= "{" [ ArrayElementExpressionList ] "}"
ArrayExpression = "{" + Optional( ArrayElementExpressionList ) + "}";

# FieldExpressionSpec ::= FieldReference AssignmentChar NotUsedOrExpression
FieldExpressionSpec = FieldReference + AssignmentChar + NotUsedOrExpression;

# FieldExpressionList ::= "{" FieldExpressionSpec { "," FieldExpressionSpec } "}"
FieldExpressionList = "{" + delimitedList( FieldExpressionSpec ) + "}";

# CompoundExpression ::= FieldExpressionList | ArrayExpression
CompoundExpression << ( FieldExpressionList | ArrayExpression );

# Expression ::= SingleExpression | CompoundExpression
Expression << ( SingleExpression | CompoundExpression );

# BasicStatements ::= Assignment | LogStatement | LoopConstruct | ConditionalConstruct | SelectCaseConstruct | StatementBlock
BasicStatements = Assignment | LogStatement | LoopConstruct | ConditionalConstruct | SelectCaseConstruct | StatementBlock;

# ContinueStatement ::= "continue"
ContinueStatement = Keyword("continue");

# BreakStatement ::= "break"
BreakStatement = Keyword("break");

# DeactivateKeyword ::= "deactivate"
DeactivateKeyword = Keyword("deactivate");

# DeactivateStatement ::= DeactivateKeyword [ "(" ComponentOrDefaultReference ")" ]
ComponentOrDefaultReference = Forward();
DeactivateStatement = DeactivateKeyword + Optional( "(" + ComponentOrDefaultReference + ")" );

# ActivateKeyword ::= "activate"
ActivateKeyword = Keyword("activate");

# ActivateOp ::= ActivateKeyword "(" AltstepInstance ")"
AltstepInstance = Forward();
ActivateOp << ActivateKeyword + "(" + AltstepInstance + ")";

# RepeatStatement ::= "repeat"
RepeatStatement = Keyword("repeat");

# GotoKeyword ::= "goto"
GotoKeyword = Keyword("goto");

# GotoStatement ::= GotoKeyword Identifier
GotoStatement = GotoKeyword + Identifier;

# LabelKeyword ::= "label"
LabelKeyword = Keyword("label");

# LabelStatement ::= LabelKeyword Identifier
LabelStatement = LabelKeyword + Identifier;

# InterleavedGuard ::= "[" "]" GuardOp
GuardOp = Forward();
InterleavedGuard = "[" + "]" + GuardOp

# InterleavedGuardElement ::= InterleavedGuard StatementBlock
InterleavedGuardElement = InterleavedGuard + StatementBlock;

# InterleavedGuardList ::= { InterleavedGuardElement [ SemiColon ] }+
InterleavedGuardList = OneOrMore( InterleavedGuardElement + Optional( SemiColon ) );

# InterleavedKeyword ::= "interleave"
InterleavedKeyword = Keyword("interleave");

# InterleavedConstruct ::= InterleavedKeyword "{" InterleavedGuardList "}"
InterleavedConstruct = InterleavedKeyword + "{" + InterleavedGuardList + "}"

# GuardOp ::= TimeoutStatement | ReceiveStatement | TriggerStatement | GetCallStatement | CatchStatement | CheckStatement | GetReplyStatement | DoneStatement | KilledStatement
TimeoutStatement = Forward();
ReceiveStatement = Forward();
TriggerStatement = Forward();
GetCallStatement = Forward();
CatchStatement = Forward();
CheckStatement = Forward();
GetReplyStatement = Forward();
DoneStatement = Forward();
KilledStatement = Forward();
GuardOp << ( TimeoutStatement | ReceiveStatement | TriggerStatement | GetCallStatement | CatchStatement | CheckStatement | GetReplyStatement | DoneStatement | KilledStatement );

# AltGuardChar ::= "[" [ BooleanExpression ] "]"
AltGuardChar = "[" + Optional( BooleanExpression ) + "]";

# ElseStatement ::= "[" ElseKeyword "]" StatementBlock
ElseStatement = "[" + ElseKeyword + "]" + StatementBlock;

# GuardStatement ::= AltGuardChar ( AltstepInstance [ StatementBlock ] | GuardOp StatementBlock )
GuardStatement = AltGuardChar + ( AltstepInstance + Optional( StatementBlock ) | GuardOp + StatementBlock );

# AltGuardList ::= { GuardStatement | ElseStatement [ SemiColon ] }
AltGuardList = ZeroOrMore( GuardStatement | ElseStatement + Optional( SemiColon ) );

# AltKeyword ::= "alt"
AltKeyword = Keyword("alt");

# AltConstruct ::= AltKeyword "{" AltGuardList "}"
AltConstruct = AltKeyword + "{" + AltGuardList + "}";

# ReturnStatement ::= ReturnKeyword [ Expression | InLineTemplate ]
ReturnKeyword = Forward();
ReturnStatement = ReturnKeyword + Optional( Expression | InLineTemplate );

# ActionText ::= FreeText | Expression
ActionText = FreeText | Expression;

# ActionKeyword ::= "action"
ActionKeyword = Keyword("action");

# SUTStatements ::= ActionKeyword "(" ActionText { StringOp ActionText } ")"
SUTStatements = ActionKeyword + "(" + ActionText + ZeroOrMore( StringOp + ActionText ) + ")";

# GetLocalVerdict ::= "getverdict"
GetLocalVerdict = Keyword("getverdict");

# SetVerdictKeyword ::= "setverdict"
SetVerdictKeyword = Keyword("setverdict");

# SetLocalVerdict ::= SetVerdictKeyword "(" SingleExpression { "," LogItem } ")"
SetLocalVerdict = SetVerdictKeyword + "(" + SingleExpression + ZeroOrMore( "," + LogItem ) + ")";

# BehaviourStatements ::= TestcaseInstance | FunctionInstance | ReturnStatement | AltConstruct | InterleavedConstruct | LabelStatement | GotoStatement | RepeatStatement | DeactivateStatement | AltstepInstance | ActivateOp | BreakStatement | ContinueStatement
BehaviourStatements = TestcaseInstance | FunctionInstance | ReturnStatement | AltConstruct | InterleavedConstruct | LabelStatement | GotoStatement | RepeatStatement | DeactivateStatement | AltstepInstance | ActivateOp | BreakStatement | ContinueStatement;

# AllRef ::= ( GroupKeyword AllKeyword [ ExceptKeyword "{" QualifiedIdentifierList "}" ] ) | ( ( TypeDefKeyword | TemplateKeyword | ConstKeyword | AltstepKeyword | TestcaseKeyword | FunctionKeyword | SignatureKeyword | ModuleParKeyword ) AllKeyword [ ExceptKeyword "{" IdentifierList "}" ] )
GroupKeyword = Forward();
AllKeyword = Forward();
ExceptKeyword = Forward();
TypeDefKeyword = Forward();
TemplateKeyword = Forward();
ConstKeyword = Forward();
AltstepKeyword = Forward();
TestcaseKeyword = Forward();
FunctionKeyword = Forward();
SignatureKeyword = Forward();
ModuleParKeyword = Forward();
AllRef = ( GroupKeyword + AllKeyword + Optional( ExceptKeyword + "{" + QualifiedIdentifierList + "}" ) ) | ( ( TypeDefKeyword | TemplateKeyword | ConstKeyword | AltstepKeyword | TestcaseKeyword | FunctionKeyword | SignatureKeyword | ModuleParKeyword ) + AllKeyword + Optional( ExceptKeyword + "{" + IdentifierList + "}" ) );

# DefOrFieldRef ::= QualifiedIdentifier | ( ( FieldReference | "[" Minus "]" ) [ ExtendedFieldReference ] ) | AllRef
DefOrFieldRef = QualifiedIdentifier | ( ( FieldReference | "[" + Minus + "]" ) + Optional( ExtendedFieldReference ) ) | AllRef;

# DefOrFieldRefList ::= DefOrFieldRef { "," DefOrFieldRef }
DefOrFieldRefList = delimitedList( DefOrFieldRef );

# AttribQualifier ::= "(" DefOrFieldRefList ")"
AttribQualifier = "(" + DefOrFieldRefList + ")";

# OverrideKeyword ::= "override"
OverrideKeyword = Keyword("override");

# ExtensionKeyword ::= "extension"
ExtensionKeyword = Keyword("extension");

# DisplayKeyword ::= "display"
DisplayKeyword = Keyword("display");

# VariantKeyword ::= "variant"
VariantKeyword = Keyword("variant");

# EncodeKeyword ::= "encode"
EncodeKeyword = Keyword("encode");

# AttribKeyword ::= EncodeKeyword | VariantKeyword | DisplayKeyword | ExtensionKeyword | OptionalKeyword
OptionalKeyword = Forward();
AttribKeyword = EncodeKeyword | VariantKeyword | DisplayKeyword | ExtensionKeyword | OptionalKeyword;

# SingleWithAttrib ::= AttribKeyword [ OverrideKeyword ] [ AttribQualifier ] FreeText
SingleWithAttrib = AttribKeyword + Optional( OverrideKeyword ) + Optional( AttribQualifier ) + FreeText;

# MultiWithAttrib ::= { SingleWithAttrib [ SemiColon ] }
MultiWithAttrib = ZeroOrMore( SingleWithAttrib + Optional( SemiColon ) );

# WithAttribList ::= "{" MultiWithAttrib "}"
WithAttribList = "{" + MultiWithAttrib + "}"

# WithKeyword ::= "with"
WithKeyword = Keyword("with");

# WithStatement ::= WithKeyword WithAttribList
WithStatement = WithKeyword + WithAttribList;

# TemplateRestriction ::= "(" ( OmitKeyword | ValueKeyword | PresentKeyword ) ")"
OmitKeyword = Forward();
ValueKeyword = Forward();
PresentKeyword = Forward();
TemplateRestriction = "(" + ( OmitKeyword | ValueKeyword | PresentKeyword ) + ")";

# RestrictedTemplate ::= OmitKeyword | ( TemplateKeyword TemplateRestriction )
RestrictedTemplate = OmitKeyword | ( TemplateKeyword + TemplateRestriction );

# FormalTemplatePar ::= [ ( InParKeyword | OutParKeyword | InOutParKeyword ) ] ( TemplateKeyword | RestrictedTemplate ) Type Identifier [ ":=" ( InLineTemplate | Minus ) ]
InParKeyword = Forward();
OutParKeyword = Forward();
InOutParKeyword = Forward();
Type = Forward();
FormalTemplatePar = Optional( ( InParKeyword | OutParKeyword | InOutParKeyword ) ) + ( TemplateKeyword | RestrictedTemplate ) + Type + Identifier + Optional( ":=" + ( InLineTemplate | Minus ) );

# FormalTimerPar ::= [ InOutParKeyword ] TimerKeyword Identifier
TimerKeyword = Forward();
FormalTimerPar = Optional( InOutParKeyword ) + TimerKeyword + Identifier;

# FormalPortPar ::= [ InOutParKeyword ] Identifier Identifier
FormalPortPar = Optional( InOutParKeyword ) + Identifier + Identifier;

# FormalValuePar ::= [ ( InParKeyword | InOutParKeyword | OutParKeyword ) ] Type Identifier [ ":=" ( Expression | Minus ) ]
FormalValuePar = Optional( ( InParKeyword | InOutParKeyword | OutParKeyword ) ) + Type + Identifier + Optional( ":=" + ( Expression | Minus ) );

# InOutParKeyword ::= "inout"
InOutParKeyword << Keyword("inout");

# OutParKeyword ::= "out"
OutParKeyword << Keyword("out");

# InParKeyword ::= "in"
InParKeyword << Keyword("in");

# OmitKeyword ::= "omit"
OmitKeyword << Keyword("omit");

# AddressValue ::= "null"
AddressValue = Keyword("null");

# FreeText ::= """ { ExtendedAlphaNum } """
ExtendedAlphaNum = Forward();
FreeText << ( Combine( Suppress('"') + ZeroOrMore( ExtendedAlphaNum ) + Suppress('"') ) );
#FreeText << ( Combine( Suppress('"') + ZeroOrMore( Word(alphanums + " !") ) + Suppress('"') ) );

# ExtendedAlphaNum ::= 
# NOTE: A graphical character from the BASIC LATIN or from the LATIN-1
# SUPPLEMENT character sets defined in ISO/IEC 10646 (characters from char
# (0,0,0,33) to char (0,0,0,126), from char (0,0,0,161) to char (0,0,0,172) and
# from char (0,0,0,174) to char (0,0,0,255)
#ExtendedAlphaNum << Word(printables, max=1);
ExtendedAlphaNum << Regex(r'(?=[^"])[ -~]');

# Char ::= 
# NOTE: A character defined by the relevant CharacterString type. For
# charstring a character from the character set defined in ISO/IEC 646. For
# universal charstring a character from any character set defined in ISO/IEC 10646
#Char = Word(srange(r"[\0x00-\0x7F]"), max=1) | Word(unicodeString, max=1); 
Char = Regex(r'.'); 

# Cstring ::= """ { Char } """
Cstring = '"' + ZeroOrMore( Char ) + '"';

# ReferencedValue ::= ExtendedIdentifier [ ExtendedFieldReference ]
ReferencedValue = ExtendedIdentifier + Optional( ExtendedFieldReference );

# Exponential ::= "E"
Exponential = "E";

# FloatENotation ::= Number [ Dot DecimalNumber ] Exponential [ Minus ] Number
FloatENotation = Number + Optional( Dot + DecimalNumber ) + Exponential + Optional( Minus ) + Number;

# FloatDotNotation ::= Number Dot DecimalNumber
FloatDotNotation = Number + Dot + DecimalNumber;

# NaNKeyword ::= "not_a_number"
NaNKeyword = Keyword("not_a_number");

# FloatValue ::= FloatDotNotation | FloatENotation | NaNKeyword
FloatValue = FloatDotNotation | FloatENotation | NaNKeyword;

# CharKeyword ::= "char"
CharKeyword = Keyword("char");

# Quadruple ::= CharKeyword "(" Number "," Number "," Number "," Number ")"
Quadruple = CharKeyword + "(" + Number + "," + Number + "," + Number + "," + Number + ")";

# CharStringValue ::= Cstring | Quadruple
CharStringValue = Cstring | Quadruple;

# VerdictTypeValue ::= "pass" | "fail" | "inconc" | "none" | "error"
VerdictTypeValue = Keyword("pass") | Keyword("fail") | Keyword("inconc") | Keyword("none") | Keyword("error");

# BooleanValue ::= "true" | "false"
BooleanValue = Keyword("true") | Keyword("false");

# PredefinedValue ::= Bstring | BooleanValue | CharStringValue | Number | Ostring | Hstring | VerdictTypeValue | Identifier | FloatValue | AddressValue | OmitKeyword
PredefinedValue = Bstring | BooleanValue | CharStringValue | Number | Ostring | Hstring | VerdictTypeValue | Identifier | FloatValue | AddressValue | OmitKeyword;

# Value ::= PredefinedValue | ReferencedValue
Value << ( PredefinedValue | ReferencedValue );

# ArrayDef ::= { "[" SingleExpression [ ".." SingleExpression ] "]" }+
ArrayDef = OneOrMore( "[" + SingleExpression + Optional( ".." + SingleExpression ) + "]" );

## TypeReference ::= Identifier
#TypeReference = Identifier;
#
# ReferencedType ::= ExtendedIdentifier [ ExtendedFieldReference ]
ReferencedType = ExtendedIdentifier + Optional( ExtendedFieldReference );

# UniversalKeyword ::= "universal"
UniversalKeyword = Keyword("universal");

# UniversalCharString ::= UniversalKeyword CharStringKeyword
CharStringKeyword = Forward();
UniversalCharString = UniversalKeyword + CharStringKeyword;

# CharStringKeyword ::= "charstring"
CharStringKeyword << Keyword("charstring");

# AnyTypeKeyword ::= "anytype"
AnyTypeKeyword = Keyword("anytype");

# DefaultKeyword ::= "default"
DefaultKeyword = Keyword("default");

# AddressKeyword ::= "address"
AddressKeyword = Keyword("address");

# FloatKeyword ::= "float"
FloatKeyword = Keyword("float");

# VerdictTypeKeyword ::= "verdicttype"
VerdictTypeKeyword = Keyword("verdicttype");

# HexStringKeyword ::= "hexstring"
HexStringKeyword = Keyword("hexstring");

# OctetStringKeyword ::= "octetstring"
OctetStringKeyword = Keyword("octetstring");

# IntegerKeyword ::= "integer"
IntegerKeyword = Keyword("integer");

# BooleanKeyword ::= "boolean"
BooleanKeyword = Keyword("boolean");

# BitStringKeyword ::= "bitstring"
BitStringKeyword = Keyword("bitstring");

# PredefinedType ::= BitStringKeyword | BooleanKeyword | CharStringKeyword | UniversalCharString | IntegerKeyword | OctetStringKeyword | HexStringKeyword | VerdictTypeKeyword | FloatKeyword | AddressKeyword | DefaultKeyword | AnyTypeKeyword
PredefinedType << ( BitStringKeyword | BooleanKeyword | CharStringKeyword | UniversalCharString | IntegerKeyword | OctetStringKeyword | HexStringKeyword | VerdictTypeKeyword | FloatKeyword | AddressKeyword | DefaultKeyword | AnyTypeKeyword );

# Type ::= PredefinedType | ReferencedType
Type << ( PredefinedType | ReferencedType );

# TestcaseOperation ::= TestcaseKeyword "." StopKeyword [ "(" { ( FreeText | InLineTemplate ) [ "," ] } ")" ]
StopKeyword = Forward();
TestcaseOperation = TestcaseKeyword + "." + StopKeyword + Optional( "(" + ZeroOrMore( ( FreeText | InLineTemplate ) + Optional( "," ) ) + ")" );

# TimeoutKeyword ::= "timeout"
TimeoutKeyword = Keyword("timeout");

# TimerRefOrAny ::= ArrayIdentifierRef | ( AnyKeyword TimerKeyword )
ArrayIdentifierRef = Forward();
AnyKeyword = Forward();
TimerRefOrAny = ArrayIdentifierRef | ( AnyKeyword + TimerKeyword );

# TimeoutStatement ::= TimerRefOrAny Dot TimeoutKeyword
TimeoutStatement = TimerRefOrAny + Dot + TimeoutKeyword;

# RunningTimerOp ::= TimerRefOrAny Dot RunningKeyword
RunningKeyword = Forward();
RunningTimerOp = TimerRefOrAny + Dot + RunningKeyword;

# ReadKeyword ::= "read"
ReadKeyword = Keyword("read");

# ReadTimerOp ::= ArrayIdentifierRef Dot ReadKeyword
ReadTimerOp = ArrayIdentifierRef + Dot + ReadKeyword;

# TimerRefOrAll ::= ArrayIdentifierRef | AllKeyword TimerKeyword
TimerRefOrAll = ArrayIdentifierRef | AllKeyword + TimerKeyword;

# StopTimerStatement ::= TimerRefOrAll Dot StopKeyword
StopTimerStatement = TimerRefOrAll + Dot + StopKeyword;

# StartTimerStatement ::= ArrayIdentifierRef Dot StartKeyword [ "(" Expression ")" ]
StartKeyword = Forward();
StartTimerStatement = ArrayIdentifierRef + Dot + StartKeyword + Optional( "(" + Expression + ")" );

# TimerOps ::= ReadTimerOp | RunningTimerOp
TimerOps << ( ReadTimerOp | RunningTimerOp );

# TimerStatements ::= StartTimerStatement | StopTimerStatement | TimeoutStatement
TimerStatements = StartTimerStatement | StopTimerStatement | TimeoutStatement;

# CheckStateKeyword ::= "checkstate"
CheckStateKeyword = Keyword("checkstate");

# PortOrAllAny ::= PortOrAll | AnyKeyword PortKeyword
PortOrAll = Forward();
PortKeyword = Forward();
PortOrAllAny = PortOrAll | AnyKeyword + PortKeyword;

# CheckStateStatement ::= PortOrAllAny Dot CheckStateKeyword "(" SingleExpression ")"
CheckStateStatement = PortOrAllAny + Dot + CheckStateKeyword + "(" + SingleExpression + ")";

# AnyKeyword ::= "any"
AnyKeyword << Keyword("any");

# HaltKeyword ::= "halt"
HaltKeyword = Keyword("halt");

# HaltStatement ::= PortOrAll Dot HaltKeyword
HaltStatement = PortOrAll + Dot + HaltKeyword;

# StopKeyword ::= "stop"
StopKeyword << Keyword("stop");

# StopStatement ::= PortOrAll Dot StopKeyword
StopStatement = PortOrAll + Dot + StopKeyword;

# StartStatement ::= PortOrAll Dot StartKeyword
StartStatement = PortOrAll + Dot + StartKeyword;

# ClearOpKeyword ::= "clear"
ClearOpKeyword = Keyword("clear");

# PortOrAll ::= ArrayIdentifierRef | AllKeyword PortKeyword
PortOrAll << ( ArrayIdentifierRef | AllKeyword + PortKeyword );

# ClearStatement ::= PortOrAll Dot ClearOpKeyword
ClearStatement = PortOrAll + Dot + ClearOpKeyword;

# CatchOpParameter ::= Signature "," InLineTemplate | TimeoutKeyword
Signature = Forward();
CatchOpParameter = Signature + "," + InLineTemplate | TimeoutKeyword;

# CatchOpKeyword ::= "catch"
CatchOpKeyword = Keyword("catch");

# PortCatchOp ::= CatchOpKeyword [ "(" CatchOpParameter ")" ] [ FromClause ] [ PortRedirect ]
FromClause = Forward();
PortRedirect = Forward();
PortCatchOp = CatchOpKeyword + Optional( "(" + CatchOpParameter + ")" ) + Optional( FromClause ) + Optional( PortRedirect );

# CatchStatement ::= PortOrAny Dot PortCatchOp
PortOrAny = Forward();
CatchStatement = PortOrAny + Dot + PortCatchOp;

## CheckPortOpsPresent ::= PortReceiveOp | PortGetCallOp | PortGetReplyOp | PortCatchOp
#CheckPortOpsPresent = PortReceiveOp | PortGetCallOp | PortGetReplyOp | PortCatchOp;
#
## RedirectPresent ::= PortRedirectSymbol SenderSpec
SenderSpec = Forward();
#RedirectPresent = PortRedirectSymbol + SenderSpec;
#
## FromClausePresent ::= FromClause [ PortRedirectSymbol SenderSpec ]
#FromClausePresent = FromClause + Optional( PortRedirectSymbol + SenderSpec );
#
## CheckParameter ::= CheckPortOpsPresent | FromClausePresent | RedirectPresent
#CheckParameter = CheckPortOpsPresent | FromClausePresent | RedirectPresent;
#
## CheckOpKeyword ::= "check"
#CheckOpKeyword = Keyword("check");
#
## PortCheckOp ::= CheckOpKeyword [ "(" CheckParameter ")" ]
#PortCheckOp = CheckOpKeyword + Optional( "(" + CheckParameter + ")" );
#
## CheckStatement ::= PortOrAny Dot PortCheckOp
#CheckStatement = PortOrAny + Dot + PortCheckOp;
#
## ValueMatchSpec ::= ValueKeyword InLineTemplate
#ValueMatchSpec = ValueKeyword + InLineTemplate;
#
## GetReplyOpKeyword ::= "getreply"
#GetReplyOpKeyword = Keyword("getreply");
#
## RedirectWithValueAndParamSpec ::= ValueSpec [ ParamSpec ] [ SenderSpec ] | RedirectWithParamSpec
#RedirectWithValueAndParamSpec = ValueSpec + Optional( ParamSpec ) + Optional( SenderSpec ) | RedirectWithParamSpec;
#
## PortRedirectWithValueAndParam ::= PortRedirectSymbol RedirectWithValueAndParamSpec
#PortRedirectWithValueAndParam = PortRedirectSymbol + RedirectWithValueAndParamSpec;
#
## PortGetReplyOp ::= GetReplyOpKeyword [ "(" InLineTemplate [ ValueMatchSpec ] ")" ] [ FromClause ] [ PortRedirectWithValueAndParam ]
#PortGetReplyOp = GetReplyOpKeyword + Optional( "(" + InLineTemplate + [ + ValueMatchSpec ) + ")" + ] + Optional( FromClause ) + Optional( PortRedirectWithValueAndParam );
#
## GetReplyStatement ::= PortOrAny Dot PortGetReplyOp
#GetReplyStatement = PortOrAny + Dot + PortGetReplyOp;
#
## VariableEntry ::= VariableRef | Minus
#VariableEntry = VariableRef | Minus;
#
## VariableList ::= VariableEntry { "," VariableEntry }
#VariableList = delimitedList( VariableEntry );
#
## VariableAssignment ::= VariableRef AssignmentChar Identifier
#VariableAssignment = VariableRef + AssignmentChar + Identifier;
#
## AssignmentList ::= VariableAssignment { "," VariableAssignment }
#AssignmentList = delimitedList( VariableAssignment );
#
## ParamAssignmentList ::= "(" ( AssignmentList | VariableList ) ")"
#ParamAssignmentList = "(" + ( AssignmentList | VariableList ) + ")";
#
# ParamKeyword ::= "param"
ParamKeyword = Keyword("param");

## ParamSpec ::= ParamKeyword ParamAssignmentList
#ParamSpec = ParamKeyword + ParamAssignmentList;
#
## RedirectWithParamSpec ::= ParamSpec [ SenderSpec ] | SenderSpec
#RedirectWithParamSpec = ParamSpec + Optional( SenderSpec ) | SenderSpec;
#
## PortRedirectWithParam ::= PortRedirectSymbol RedirectWithParamSpec
#PortRedirectWithParam = PortRedirectSymbol + RedirectWithParamSpec;
#
## GetCallOpKeyword ::= "getcall"
#GetCallOpKeyword = Keyword("getcall");
#
## PortGetCallOp ::= GetCallOpKeyword [ "(" InLineTemplate ")" ] [ FromClause ] [ PortRedirectWithParam ]
#PortGetCallOp = GetCallOpKeyword + Optional( "(" + InLineTemplate + ")" ) + Optional( FromClause ) + Optional( PortRedirectWithParam );
#
## GetCallStatement ::= PortOrAny Dot PortGetCallOp
#GetCallStatement = PortOrAny + Dot + PortGetCallOp;
#
## TriggerOpKeyword ::= "trigger"
#TriggerOpKeyword = Keyword("trigger");
#
## PortTriggerOp ::= TriggerOpKeyword [ "(" InLineTemplate ")" ] [ FromClause ] [ PortRedirect ]
#PortTriggerOp = TriggerOpKeyword + Optional( "(" + InLineTemplate + ")" ) + Optional( FromClause ) + Optional( PortRedirect );
#
## TriggerStatement ::= PortOrAny Dot PortTriggerOp
#TriggerStatement = PortOrAny + Dot + PortTriggerOp;
#
# SenderKeyword ::= "sender"
SenderKeyword = Keyword("sender");

# SenderSpec ::= SenderKeyword VariableRef
SenderSpec << SenderKeyword + VariableRef;

# ValueKeyword ::= "value"
ValueKeyword << Keyword("value");

# SingleValueSpec ::= VariableRef [ AssignmentChar FieldReference ExtendedFieldReference ]
SingleValueSpec = VariableRef + Optional( AssignmentChar + FieldReference + ExtendedFieldReference );

# ValueSpec ::= ValueKeyword ( VariableRef | ( "(" SingleValueSpec { "," SingleValueSpec } ")" ) )
ValueSpec = ValueKeyword + ( VariableRef | ( "(" + delimitedList( SingleValueSpec ) + ")" ) );

# PortRedirectSymbol ::= "->"
PortRedirectSymbol = "->";

# PortRedirect ::= PortRedirectSymbol ( ValueSpec [ SenderSpec ] | SenderSpec )
PortRedirect << ( PortRedirectSymbol + ( ValueSpec + Optional( SenderSpec ) | SenderSpec ) );

# FromKeyword ::= "from"
FromKeyword = Keyword("from");

# FromClause ::= FromKeyword ( InLineTemplate | AddressRefList | AnyKeyword ComponentKeyword )
AddressRefList = Forward();
ComponentKeyword = Forward();
FromClause << ( FromKeyword + ( InLineTemplate | AddressRefList | AnyKeyword + ComponentKeyword ) );

# ReceiveOpKeyword ::= "receive"
ReceiveOpKeyword = Keyword("receive");

# PortReceiveOp ::= ReceiveOpKeyword [ "(" InLineTemplate ")" ] [ FromClause ] [ PortRedirect ]
PortReceiveOp = ReceiveOpKeyword + Optional( "(" + InLineTemplate + ")" ) + Optional( FromClause ) + Optional( PortRedirect );

# PortOrAny ::= ArrayIdentifierRef | AnyKeyword PortKeyword
PortOrAny << ( ArrayIdentifierRef | AnyKeyword + PortKeyword );

# ReceiveStatement ::= PortOrAny Dot PortReceiveOp
ReceiveStatement = PortOrAny + Dot + PortReceiveOp;

# RaiseKeyword ::= "raise"
RaiseKeyword = Keyword("raise");

# PortRaiseOp ::= RaiseKeyword "(" Signature "," InLineTemplate ")" [ ToClause ]
ToClause = Forward();
PortRaiseOp = RaiseKeyword + "(" + Signature + "," + InLineTemplate + ")" + Optional( ToClause );

# RaiseStatement ::= ArrayIdentifierRef Dot PortRaiseOp
RaiseStatement = ArrayIdentifierRef + Dot + PortRaiseOp;

# ReplyValue ::= ValueKeyword Expression
ReplyValue = ValueKeyword + Expression;

# ReplyKeyword ::= "reply"
ReplyKeyword = Keyword("reply");

# PortReplyOp ::= ReplyKeyword "(" InLineTemplate [ ReplyValue ] ")" [ ToClause ]
PortReplyOp = ReplyKeyword + "(" + InLineTemplate + Optional( ReplyValue ) + ")" + Optional( ToClause );

# ReplyStatement ::= ArrayIdentifierRef Dot PortReplyOp
ReplyStatement = ArrayIdentifierRef + Dot + PortReplyOp;

# CallBodyOps ::= GetReplyStatement | CatchStatement
CallBodyOps = GetReplyStatement | CatchStatement;

# CallBodyGuard ::= AltGuardChar CallBodyOps
CallBodyGuard = AltGuardChar + CallBodyOps;

# CallBodyStatement ::= CallBodyGuard StatementBlock
CallBodyStatement = CallBodyGuard + StatementBlock;

# CallBodyStatementList ::= { CallBodyStatement [ SemiColon ] }+
CallBodyStatementList = OneOrMore( CallBodyStatement + Optional( SemiColon ) );

# PortCallBody ::= "{" CallBodyStatementList "}"
PortCallBody = "{" + CallBodyStatementList + "}";

# NowaitKeyword ::= "nowait"
NowaitKeyword = Keyword("nowait");

# CallTimerValue ::= Expression | NowaitKeyword
CallTimerValue = Expression | NowaitKeyword;

# CallParameters ::= InLineTemplate [ "," CallTimerValue ]
CallParameters = InLineTemplate + Optional( "," + CallTimerValue );

# CallOpKeyword ::= "call"
CallOpKeyword = Keyword("call");

# PortCallOp ::= CallOpKeyword "(" CallParameters ")" [ ToClause ]
PortCallOp = CallOpKeyword + "(" + CallParameters + ")" + Optional( ToClause );

# CallStatement ::= ArrayIdentifierRef Dot PortCallOp [ PortCallBody ]
CallStatement = ArrayIdentifierRef + Dot + PortCallOp + Optional( PortCallBody );

# ToKeyword ::= "to"
ToKeyword = Keyword("to");

# AddressRefList ::= "(" InLineTemplate { "," InLineTemplate } ")"
AddressRefList << ( "(" + delimitedList( InLineTemplate ) + ")" );

# ToClause ::= ToKeyword ( InLineTemplate | AddressRefList | AllKeyword ComponentKeyword )
ToClause << ToKeyword + ( InLineTemplate | AddressRefList | AllKeyword + ComponentKeyword );

# SendOpKeyword ::= "send"
SendOpKeyword = Keyword("send");

# PortSendOp ::= SendOpKeyword "(" InLineTemplate ")" [ ToClause ]
PortSendOp = SendOpKeyword + "(" + InLineTemplate + ")" + Optional( ToClause );

# SendStatement ::= ArrayIdentifierRef Dot PortSendOp
SendStatement = ArrayIdentifierRef + Dot + PortSendOp;

# CommunicationStatements ::= SendStatement | CallStatement | ReplyStatement | RaiseStatement | ReceiveStatement | TriggerStatement | GetCallStatement | GetReplyStatement | CatchStatement | CheckStatement | ClearStatement | StartStatement | StopStatement | HaltStatement | CheckStateStatement
CommunicationStatements = SendStatement | CallStatement | ReplyStatement | RaiseStatement | ReceiveStatement | TriggerStatement | GetCallStatement | GetReplyStatement | CatchStatement | CheckStatement | ClearStatement | StartStatement | StopStatement | HaltStatement | CheckStateStatement;

# KillKeyword ::= "kill"
KillKeyword = Keyword("kill");

# ComponentOrDefaultReference ::= VariableRef | FunctionInstance
ComponentOrDefaultReference << ( VariableRef | FunctionInstance );

# KillTCStatement ::= KillKeyword | ( ( ComponentReferenceOrLiteral | AllKeyword ComponentKeyword ) Dot KillKeyword )
ComponentReferenceOrLiteral = Forward();
KillTCStatement = KillKeyword | ( ( ComponentReferenceOrLiteral | AllKeyword + ComponentKeyword ) + Dot + KillKeyword );

# ComponentReferenceOrLiteral ::= ComponentOrDefaultReference | MTCKeyword | SelfOp
MTCKeyword = Forward();
SelfOp = Forward();
ComponentReferenceOrLiteral << ( ComponentOrDefaultReference | MTCKeyword | SelfOp );

## StopTCStatement ::= StopKeyword | ( ComponentReferenceOrLiteral | AllKeyword ComponentKeyword ) Dot StopKeyword
#StopTCStatement = StopKeyword | ( ComponentReferenceOrLiteral | AllKeyword + ComponentKeyword ) + Dot + StopKeyword;
#
# StartKeyword ::= "start"
StartKeyword << Keyword("start");

# StartTCStatement ::= ComponentOrDefaultReference Dot StartKeyword "(" FunctionInstance ")"
StartTCStatement = ComponentOrDefaultReference + Dot + StartKeyword + "(" + FunctionInstance + ")";

# UnmapKeyword ::= "unmap"
UnmapKeyword = Keyword("unmap");

# UnmapStatement ::= UnmapKeyword [ SingleConnectionSpec [ ParamClause ] | AllConnectionsSpec [ ParamClause ] | AllPortsSpec | AllCompsAllPortsSpec ]
SingleConnectionSpec = Forward();
ParamClause = Forward();
AllConnectionsSpec = Forward();
AllPortsSpec = Forward();
AllCompsAllPortsSpec = Forward();
UnmapStatement = UnmapKeyword + Optional( SingleConnectionSpec + Optional( ParamClause ) | AllConnectionsSpec + Optional( ParamClause ) | AllPortsSpec | AllCompsAllPortsSpec );

# MapKeyword ::= "map"
MapKeyword = Keyword("map");

# ParamClause ::= ParamKeyword FunctionActualParList
FunctionActualParList = Forward();
ParamClause << ( ParamKeyword + FunctionActualParList );

# MapStatement ::= MapKeyword SingleConnectionSpec [ ParamClause ]
MapStatement = MapKeyword + SingleConnectionSpec + Optional( ParamClause );

# DisconnectKeyword ::= "disconnect"
DisconnectKeyword = Keyword("disconnect");

# AllCompsAllPortsSpec ::= "(" AllKeyword ComponentKeyword ":" AllKeyword PortKeyword ")"
AllCompsAllPortsSpec = "(" + AllKeyword + ComponentKeyword + ":" + AllKeyword + PortKeyword + ")";

# AllPortsSpec ::= "(" ComponentRef ":" AllKeyword PortKeyword ")"
ComponentRef = Forward();
AllPortsSpec = "(" + ComponentRef + ":" + AllKeyword + PortKeyword + ")";

# AllConnectionsSpec ::= "(" PortRef ")"
PortRef = Forward();
AllConnectionsSpec << ( "(" + PortRef + ")" );

## DisconnectStatement ::= DisconnectKeyword [ SingleConnectionSpec | AllConnectionsSpec | AllPortsSpec | AllCompsAllPortsSpec ]
#DisconnectStatement = DisconnectKeyword + Optional( SingleConnectionSpec | AllConnectionsSpec | AllPortsSpec | AllCompsAllPortsSpec );

# ComponentRefAssignment ::= Identifier ":=" ComponentRef
ComponentRefAssignment = Identifier + ":=" + ComponentRef;

# ComponentRef ::= ComponentOrDefaultReference | SystemKeyword | SelfOp | MTCKeyword
SystemKeyword = Forward();
ComponentRef << ( ComponentOrDefaultReference | SystemKeyword | SelfOp | MTCKeyword );

# PortRef ::= ComponentRef Colon ArrayIdentifierRef
PortRef << ( ComponentRef + Colon + ArrayIdentifierRef );

# SingleConnectionSpec ::= "(" PortRef "," PortRef ")"
SingleConnectionSpec << "(" + PortRef + "," + PortRef + ")";

## ConnectKeyword ::= "connect"
#ConnectKeyword = Keyword("connect");
#
## ConnectStatement ::= ConnectKeyword SingleConnectionSpec
#ConnectStatement = ConnectKeyword + SingleConnectionSpec;
#
## AliveKeyword ::= "alive"
#AliveKeyword = Keyword("alive");
#
## CreateKeyword ::= "create"
#CreateKeyword = Keyword("create");
#
## AliveOp ::= ComponentId Dot AliveKeyword
#AliveOp = ComponentId + Dot + AliveKeyword;
#
# RunningKeyword ::= "running"
RunningKeyword << Keyword("running");

## RunningOp ::= ComponentId Dot RunningKeyword
#RunningOp = ComponentId + Dot + RunningKeyword;
#
## KilledKeyword ::= "killed"
#KilledKeyword = Keyword("killed");
#
# DoneKeyword ::= "done"
DoneKeyword = Keyword("done");

# ComponentId ::= ComponentOrDefaultReference | ( AnyKeyword | AllKeyword ) ComponentKeyword
ComponentId = ComponentOrDefaultReference | ( AnyKeyword | AllKeyword ) + ComponentKeyword;

## KilledStatement ::= ComponentId Dot KilledKeyword
#KilledStatement = ComponentId + Dot + KilledKeyword;
#
# DoneStatement ::= ComponentId Dot DoneKeyword
DoneStatement << ( ComponentId + Dot + DoneKeyword );

# SelfOp ::= "self"
SelfOp << Keyword("self");

## CreateOp ::= ComponentType Dot CreateKeyword [ "(" ( SingleExpression | Minus ) [ "," SingleExpression ] ")" ] [ AliveKeyword ]
#CreateOp = ComponentType + Dot + CreateKeyword + Optional( "(" + ( SingleExpression | Minus ) + [ + "," + SingleExpression ) + ")" + ] + Optional( AliveKeyword );
#
## ConfigurationOps ::= CreateOp | SelfOp | SystemKeyword | MTCKeyword | RunningOp | AliveOp
#ConfigurationOps = CreateOp | SelfOp | SystemKeyword | MTCKeyword | RunningOp | AliveOp;
#
## ConfigurationStatements ::= ConnectStatement | MapStatement | DisconnectStatement | UnmapStatement | DoneStatement | KilledStatement | StartTCStatement | StopTCStatement | KillTCStatement
#ConfigurationStatements = ConnectStatement | MapStatement | DisconnectStatement | UnmapStatement | DoneStatement | KilledStatement | StartTCStatement | StopTCStatement | KillTCStatement;
#
# ArrayIdentifierRef ::= Identifier { ArrayOrBitRef }
ArrayIdentifierRef << ( Identifier + ZeroOrMore( ArrayOrBitRef ) );

# TimerKeyword ::= "timer"
TimerKeyword << Keyword("timer");

## TimerInstance ::= TimerKeyword VarList
#TimerInstance = TimerKeyword + VarList;
#
## VariableRef ::= Identifier [ ExtendedFieldReference ]
#VariableRef = Identifier + Optional( ExtendedFieldReference );
#
## SingleTempVarInstance ::= Identifier [ ArrayDef ] [ AssignmentChar TemplateBody ]
#SingleTempVarInstance = Identifier + Optional( ArrayDef ) + Optional( AssignmentChar + TemplateBody );
#
## TempVarList ::= SingleTempVarInstance { "," SingleTempVarInstance }
#TempVarList = delimitedList( SingleTempVarInstance );
#
## VarKeyword ::= "var"
#VarKeyword = Keyword("var");
#
## SingleVarInstance ::= Identifier [ ArrayDef ] [ AssignmentChar Expression ]
#SingleVarInstance = Identifier + Optional( ArrayDef ) + Optional( AssignmentChar + Expression );
#
## VarList ::= SingleVarInstance { "," SingleVarInstance }
#VarList = delimitedList( SingleVarInstance );
#
## VarInstance ::= VarKeyword ( ( Type VarList ) | ( ( TemplateKeyword | RestrictedTemplate ) Type TempVarList ) )
#VarInstance = VarKeyword + ( ( Type + VarList ) | ( ( TemplateKeyword | RestrictedTemplate ) + Type + TempVarList ) );
#
## ControlStatement ::= TimerStatements | BasicStatements | BehaviourStatements | SUTStatements | StopKeyword
#ControlStatement = TimerStatements | BasicStatements | BehaviourStatements | SUTStatements | StopKeyword;
#
## ControlStatementOrDef ::= ( FunctionLocalDef | FunctionLocalInst ) [ WithStatement ] | ControlStatement
#ControlStatementOrDef = ( FunctionLocalDef | FunctionLocalInst ) + Optional( WithStatement ) | ControlStatement;
#
## ControlStatementOrDefList ::= { ControlStatementOrDef [ SemiColon ] }+
#ControlStatementOrDefList = OneOrMore( ControlStatementOrDef + Optional( SemiColon ) );
#
## ModuleControlBody ::= [ ControlStatementOrDefList ]
#ModuleControlBody = Optional( ControlStatementOrDefList );
#
## ControlKeyword ::= "control"
#ControlKeyword = Keyword("control");
#
## ModuleControlPart ::= ControlKeyword "{" ModuleControlBody "}" [ WithStatement ] [ SemiColon ]
#ModuleControlPart = ControlKeyword + "ZeroOrMore(" + ModuleControlBody + ")" + Optional( WithStatement ) + Optional( SemiColon );
#
## FriendModuleDef ::= "friend" "module" IdentifierList [ SemiColon ]
#FriendModuleDef = Keyword("friend") + Keyword("module") + IdentifierList + Optional( SemiColon );
#
## ModuleParList ::= Identifier [ AssignmentChar ConstantExpression ] { "," Identifier [ AssignmentChar ConstantExpression ] }
#ModuleParList = Identifier + Optional( AssignmentChar + ConstantExpression ) + ZeroOrMore( "," + Identifier + Optional( AssignmentChar + ConstantExpression ) );
#
## ModulePar ::= Type ModuleParList
#ModulePar = Type + ModuleParList;
#
## MultitypedModuleParList ::= { ModulePar [ SemiColon ] }
#MultitypedModuleParList = ZeroOrMore( ModulePar + Optional( SemiColon ) );
#
# ModuleParKeyword ::= "modulepar"
ModuleParKeyword << Keyword("modulepar");

## ModuleParDef ::= ModuleParKeyword ( ModulePar | ( "{" MultitypedModuleParList "}" ) )
#ModuleParDef = ModuleParKeyword + ( ModulePar | ( "{" + MultitypedModuleParList + "}" ) );
#
## ExtConstDef ::= ExtKeyword ConstKeyword Type IdentifierList
#ExtConstDef = ExtKeyword + ConstKeyword + Type + IdentifierList;
#
## ExtKeyword ::= "external"
#ExtKeyword = Keyword("external");
#
## ExtFunctionDef ::= ExtKeyword FunctionKeyword Identifier "(" [ FunctionFormalParList ] ")" [ ReturnType ]
#ExtFunctionDef = ExtKeyword + FunctionKeyword + Identifier + "(" + Optional( FunctionFormalParList ) + ")" + Optional( ReturnType );
#
# GroupKeyword ::= "group"
GroupKeyword << Keyword("group");

## GroupDef ::= GroupKeyword Identifier "{" [ ModuleDefinitionsList ] "}"
#GroupDef = GroupKeyword + Identifier + "{" + Optional( ModuleDefinitionsList ) + "}";
#
## ImportImportSpec ::= ImportKeyword AllKeyword
#ImportImportSpec = ImportKeyword + AllKeyword;
#
## ImportModuleParSpec ::= ModuleParKeyword IdentifierListOrAllWithExcept
#ImportModuleParSpec = ModuleParKeyword + IdentifierListOrAllWithExcept;
#
## ImportSignatureSpec ::= SignatureKeyword IdentifierListOrAllWithExcept
#ImportSignatureSpec = SignatureKeyword + IdentifierListOrAllWithExcept;
#
## ImportFunctionSpec ::= FunctionKeyword IdentifierListOrAllWithExcept
#ImportFunctionSpec = FunctionKeyword + IdentifierListOrAllWithExcept;
#
## ImportTestcaseSpec ::= TestcaseKeyword IdentifierListOrAllWithExcept
#ImportTestcaseSpec = TestcaseKeyword + IdentifierListOrAllWithExcept;
#
## ImportAltstepSpec ::= AltstepKeyword IdentifierListOrAllWithExcept
#ImportAltstepSpec = AltstepKeyword + IdentifierListOrAllWithExcept;
#
## ImportConstSpec ::= ConstKeyword IdentifierListOrAllWithExcept
#ImportConstSpec = ConstKeyword + IdentifierListOrAllWithExcept;
#
## ImportTemplateSpec ::= TemplateKeyword IdentifierListOrAllWithExcept
#ImportTemplateSpec = TemplateKeyword + IdentifierListOrAllWithExcept;
#
## AllWithExcept ::= AllKeyword [ ExceptKeyword IdentifierList ]
#AllWithExcept = AllKeyword + Optional( ExceptKeyword + IdentifierList );
#
## ImportTypeDefSpec ::= TypeDefKeyword IdentifierListOrAllWithExcept
#ImportTypeDefSpec = TypeDefKeyword + IdentifierListOrAllWithExcept;
#
## IdentifierListOrAllWithExcept ::= IdentifierList | AllWithExcept
#IdentifierListOrAllWithExcept = IdentifierList | AllWithExcept;
#
## QualifiedIdentifierWithExcept ::= QualifiedIdentifier [ ExceptsDef ]
#QualifiedIdentifierWithExcept = QualifiedIdentifier + Optional( ExceptsDef );
#
## AllGroupsWithExcept ::= AllKeyword [ ExceptKeyword QualifiedIdentifierList ]
#AllGroupsWithExcept = AllKeyword + Optional( ExceptKeyword + QualifiedIdentifierList );
#
## GroupRefListWithExcept ::= QualifiedIdentifierWithExcept { "," QualifiedIdentifierWithExcept }
#GroupRefListWithExcept = delimitedList( QualifiedIdentifierWithExcept );
#
## ImportGroupSpec ::= GroupKeyword ( GroupRefListWithExcept | AllGroupsWithExcept )
#ImportGroupSpec = GroupKeyword + ( GroupRefListWithExcept | AllGroupsWithExcept );
#
## RecursiveKeyword ::= "recursive"
#RecursiveKeyword = Keyword("recursive");
#
## ImportFromSpec ::= FromKeyword ModuleId [ RecursiveKeyword ]
#ImportFromSpec = FromKeyword + ModuleId + Optional( RecursiveKeyword );
#
## ImportElement ::= ImportGroupSpec | ImportTypeDefSpec | ImportTemplateSpec | ImportConstSpec | ImportTestcaseSpec | ImportAltstepSpec | ImportFunctionSpec | ImportSignatureSpec | ImportModuleParSpec | ImportImportSpec
#ImportElement = ImportGroupSpec | ImportTypeDefSpec | ImportTemplateSpec | ImportConstSpec | ImportTestcaseSpec | ImportAltstepSpec | ImportFunctionSpec | ImportSignatureSpec | ImportModuleParSpec | ImportImportSpec;
#
## ImportSpec ::= { ImportElement [ SemiColon ] }
#ImportSpec = ZeroOrMore( ImportElement + Optional( SemiColon ) );
#
## ExceptModuleParSpec ::= ModuleParKeyword IdentifierListOrAll
#ExceptModuleParSpec = ModuleParKeyword + IdentifierListOrAll;
#
## ExceptSignatureSpec ::= SignatureKeyword IdentifierListOrAll
#ExceptSignatureSpec = SignatureKeyword + IdentifierListOrAll;
#
## ExceptFunctionSpec ::= FunctionKeyword IdentifierListOrAll
#ExceptFunctionSpec = FunctionKeyword + IdentifierListOrAll;
#
## ExceptAltstepSpec ::= AltstepKeyword IdentifierListOrAll
#ExceptAltstepSpec = AltstepKeyword + IdentifierListOrAll;
#
## ExceptTestcaseSpec ::= TestcaseKeyword IdentifierListOrAll
#ExceptTestcaseSpec = TestcaseKeyword + IdentifierListOrAll;
#
## ExceptConstSpec ::= ConstKeyword IdentifierListOrAll
#ExceptConstSpec = ConstKeyword + IdentifierListOrAll;
#
## ExceptTemplateSpec ::= TemplateKeyword IdentifierListOrAll
#ExceptTemplateSpec = TemplateKeyword + IdentifierListOrAll;
#
## ExceptTypeDefSpec ::= TypeDefKeyword IdentifierListOrAll
#ExceptTypeDefSpec = TypeDefKeyword + IdentifierListOrAll;
#
## IdentifierListOrAll ::= IdentifierList | AllKeyword
#IdentifierListOrAll = IdentifierList | AllKeyword;
#
## ExceptGroupSpec ::= GroupKeyword ( QualifiedIdentifierList | AllKeyword )
#ExceptGroupSpec = GroupKeyword + ( QualifiedIdentifierList | AllKeyword );
#
## ExceptElement ::= ExceptGroupSpec | ExceptTypeDefSpec | ExceptTemplateSpec | ExceptConstSpec | ExceptTestcaseSpec | ExceptAltstepSpec | ExceptFunctionSpec | ExceptSignatureSpec | ExceptModuleParSpec
#ExceptElement = ExceptGroupSpec | ExceptTypeDefSpec | ExceptTemplateSpec | ExceptConstSpec | ExceptTestcaseSpec | ExceptAltstepSpec | ExceptFunctionSpec | ExceptSignatureSpec | ExceptModuleParSpec;
#
## ExceptSpec ::= { ExceptElement [ SemiColon ] }
#ExceptSpec = ZeroOrMore( ExceptElement + Optional( SemiColon ) );
#
# ExceptKeyword ::= "except"
ExceptKeyword << Keyword("except");

## ExceptsDef ::= ExceptKeyword "{" ExceptSpec "}"
#ExceptsDef = ExceptKeyword + "ZeroOrMore(" + ExceptSpec + ")";
#
## AllWithExcepts ::= AllKeyword [ ExceptsDef ]
#AllWithExcepts = AllKeyword + Optional( ExceptsDef );
#
## ImportKeyword ::= "import"
#ImportKeyword = Keyword("import");
#
## ImportDef ::= ImportKeyword ImportFromSpec ( AllWithExcepts | ( "{" ImportSpec "}" ) )
#ImportDef = ImportKeyword + ImportFromSpec + ( AllWithExcepts | ( "ZeroOrMore(" + ImportSpec + ")" ) );
#
# AltstepInstance ::= ExtendedIdentifier "(" [ FunctionActualParList ] ")"
AltstepInstance << ( ExtendedIdentifier + "(" + Optional( FunctionActualParList ) + ")" );

## AltstepLocalDef ::= VarInstance | TimerInstance | ConstDef | TemplateDef
#AltstepLocalDef = VarInstance | TimerInstance | ConstDef | TemplateDef;
#
## AltstepLocalDefList ::= { AltstepLocalDef [ WithStatement ] [ SemiColon ] }
#AltstepLocalDefList = ZeroOrMore( AltstepLocalDef + Optional( WithStatement ) + Optional( SemiColon ) );
#
# AltstepKeyword ::= "altstep"
AltstepKeyword << Keyword("altstep");

## AltstepDef ::= AltstepKeyword Identifier "(" [ FunctionFormalParList ] ")" [ RunsOnSpec ] "{" AltstepLocalDefList AltGuardList "}"
#AltstepDef = AltstepKeyword + Identifier + "(" + Optional( FunctionFormalParList ) + ")" + Optional( RunsOnSpec ) + "ZeroOrMore(" + AltstepLocalDefList + AltGuardList + ")";
#
## TestcaseActualParList ::= ( TemplateInstanceActualPar { "," TemplateInstanceActualPar } ) | ( TemplateInstanceAssignment { "," TemplateInstanceAssignment } )
#TestcaseActualParList = ( delimitedList( TemplateInstanceActualPar ) ) | ( delimitedList( TemplateInstanceAssignment ) );
#
## ExecuteKeyword ::= "execute"
#ExecuteKeyword = Keyword("execute");
#
## TestcaseInstance ::= ExecuteKeyword "(" ExtendedIdentifier "(" [ TestcaseActualParList ] ")" [ "," ( Expression | Minus ) [ "," SingleExpression ] ] ")"
#TestcaseInstance = ExecuteKeyword + "(" + ExtendedIdentifier + "(" + Optional( TestcaseActualParList ) + ")" + Optional( "," + ( Expression | Minus ) + [ + "," + SingleExpression ) + ] + ")";
#
# SystemKeyword ::= "system"
SystemKeyword << Keyword("system");

## SystemSpec ::= SystemKeyword ComponentType
#SystemSpec = SystemKeyword + ComponentType;
#
## ConfigSpec ::= RunsOnSpec [ SystemSpec ]
#ConfigSpec = RunsOnSpec + Optional( SystemSpec );
#
# TestcaseKeyword ::= "testcase"
TestcaseKeyword << Keyword("testcase");

## TestcaseDef ::= TestcaseKeyword Identifier "(" [ TemplateOrValueFormalParList ] ")" ConfigSpec StatementBlock
#TestcaseDef = TestcaseKeyword + Identifier + "(" + Optional( TemplateOrValueFormalParList ) + ")" + ConfigSpec + StatementBlock;
#
## NoBlockKeyword ::= "noblock"
#NoBlockKeyword = Keyword("noblock");
#
# Signature ::= ExtendedIdentifier
Signature << ExtendedIdentifier;

## ExceptionKeyword ::= "exception"
#ExceptionKeyword = Keyword("exception");
#
## ExceptionSpec ::= ExceptionKeyword "(" TypeList ")"
#ExceptionSpec = ExceptionKeyword + "(" + TypeList + ")";
#
## SignatureFormalParList ::= FormalValuePar { "," FormalValuePar }
#SignatureFormalParList = delimitedList( FormalValuePar );
#
# SignatureKeyword ::= "signature"
SignatureKeyword << Keyword("signature");

#TMP
InLineTemplate << ( Literal("42") | Identifier ); # TEMP
StatementBlock << ( Literal("{") + Literal("}") ); # TEMP
VarInstance << "var integer v_i := 0";
ConfigurationOps << Identifier; 
TestcaseInstance << Identifier;
FunctionInstance << Identifier; 
ExtendedFieldReference << Identifier;
TemplateOps << Identifier;
PredefinedType << Identifier;
ArrayOrBitRef << Identifier;
VariableRef << Identifier;
TemplateBody << Identifier;
ConstantExpression << SingleExpression;
FieldReference << Identifier;
## SignatureDef ::= SignatureKeyword Identifier "(" [ SignatureFormalParList ] ")" [ ReturnType | NoBlockKeyword ] [ ExceptionSpec ]
#SignatureDef = SignatureKeyword + Identifier + "(" + Optional( SignatureFormalParList ) + ")" + Optional( ReturnType | NoBlockKeyword ) + Optional( ExceptionSpec );
#
# ArrayIdentifierRefAssignment ::= Identifier ":=" ArrayIdentifierRef
ArrayIdentifierRefAssignment = Identifier + ":=" + ArrayIdentifierRef;

# FunctionActualParAssignment ::= TemplateInstanceAssignment | ComponentRefAssignment | ArrayIdentifierRefAssignment
TemplateInstanceAssignment = Forward();
FunctionActualParAssignment = TemplateInstanceAssignment | ComponentRefAssignment | ArrayIdentifierRefAssignment;

# FunctionActualPar ::= ArrayIdentifierRef | InLineTemplate | ComponentRef | Minus
FunctionActualPar = ArrayIdentifierRef | InLineTemplate | ComponentRef | Minus;

# FunctionActualParList ::= ( FunctionActualPar { "," FunctionActualPar } ) | ( FunctionActualParAssignment { "," FunctionActualParAssignment } )
FunctionActualParList << ( ( FunctionActualPar + ZeroOrMore( "," + FunctionActualPar ) ) | ( FunctionActualParAssignment + ZeroOrMore( "," + FunctionActualParAssignment ) ) );

## PreDefFunctionIdentifier ::= Identifier
#PreDefFunctionIdentifier = Identifier;
#
## FunctionRef ::= [ Identifier Dot ] ( Identifier | PreDefFunctionIdentifier )
#FunctionRef = Optional( Identifier + Dot ) + ( Identifier | PreDefFunctionIdentifier );
#
## FunctionInstance ::= FunctionRef "(" [ FunctionActualParList ] ")"
#FunctionInstance = FunctionRef + "(" + Optional( FunctionActualParList ) + ")";
#
## FunctionStatement ::= ConfigurationStatements | TimerStatements | CommunicationStatements | BasicStatements | BehaviourStatements | SetLocalVerdict | SUTStatements | TestcaseOperation
#FunctionStatement = ConfigurationStatements | TimerStatements | CommunicationStatements | BasicStatements | BehaviourStatements | SetLocalVerdict | SUTStatements | TestcaseOperation;
#
## FunctionLocalDef ::= ConstDef | TemplateDef
#FunctionLocalDef = ConstDef | TemplateDef;
#
## FunctionLocalInst ::= VarInstance | TimerInstance
#FunctionLocalInst = VarInstance | TimerInstance;
#
## FunctionStatementList ::= { FunctionStatement [ SemiColon ] }+
#FunctionStatementList = OneOrMore( FunctionStatement + Optional( SemiColon ) );
#
## FunctionDefList ::= { ( FunctionLocalDef | FunctionLocalInst ) [ WithStatement ] [ SemiColon ] }+
#FunctionDefList = OneOrMore( ( FunctionLocalDef | FunctionLocalInst ) + Optional( WithStatement ) + Optional( SemiColon ) );
#
## StatementBlock ::= "{" [ FunctionDefList ] [ FunctionStatementList ] "}"
#StatementBlock = "ZeroOrMore(" + Optional( FunctionDefList ) + Optional( FunctionStatementList ) + ")";
#
# MTCKeyword ::= "mtc"
MTCKeyword << Keyword("mtc");

## OnKeyword ::= "on"
#OnKeyword = Keyword("on");
#
## RunsKeyword ::= "runs"
#RunsKeyword = Keyword("runs");
#
## RunsOnSpec ::= RunsKeyword OnKeyword ComponentType
#RunsOnSpec = RunsKeyword + OnKeyword + ComponentType;
#
# ReturnKeyword ::= "return"
ReturnKeyword << Keyword("return");

## ReturnType ::= ReturnKeyword [ TemplateKeyword | RestrictedTemplate ] Type
#ReturnType = ReturnKeyword + Optional( TemplateKeyword | RestrictedTemplate ) + Type;
#
## FunctionFormalPar ::= FormalValuePar | FormalTimerPar | FormalTemplatePar | FormalPortPar
#FunctionFormalPar = FormalValuePar | FormalTimerPar | FormalTemplatePar | FormalPortPar;
#
## FunctionFormalParList ::= FunctionFormalPar { "," FunctionFormalPar }
#FunctionFormalParList = FunctionFormalPar + ZeroOrMore( "," + FunctionFormalPar );
#
# FunctionKeyword ::= "function"
FunctionKeyword << Keyword("function");

## FunctionDef ::= FunctionKeyword Identifier "(" [ FunctionFormalParList ] ")" [ RunsOnSpec ] [ ReturnType ] StatementBlock
#FunctionDef = FunctionKeyword + Identifier + "(" + Optional( FunctionFormalParList ) + ")" + Optional( RunsOnSpec ) + Optional( ReturnType ) + StatementBlock;
#
## ValueofKeyword ::= "valueof"
#ValueofKeyword = Keyword("valueof");
#
## ValueofOp ::= ValueofKeyword "(" InLineTemplate ")"
#ValueofOp = ValueofKeyword + "(" + InLineTemplate + ")";
#
## MatchKeyword ::= "match"
#MatchKeyword = Keyword("match");
#
## MatchOp ::= MatchKeyword "(" Expression "," InLineTemplate ")"
#MatchOp = MatchKeyword + "(" + Expression + "," + InLineTemplate + ")";
#
## TemplateOps ::= MatchOp | ValueofOp
#TemplateOps = MatchOp | ValueofOp;
#
## TemplateInstanceActualPar ::= InLineTemplate | Minus
#TemplateInstanceActualPar = InLineTemplate | Minus;
#
## TemplateActualParList ::= "(" [ ( TemplateInstanceActualPar { "," TemplateInstanceActualPar } ) | ( TemplateInstanceAssignment { "," TemplateInstanceAssignment } ) ] ")"
#TemplateActualParList = "(" + Optional( ( TemplateInstanceActualPar + ZeroOrMore( "," + TemplateInstanceActualPar ) ) | ( TemplateInstanceAssignment + ZeroOrMore( "," + TemplateInstanceAssignment ) ) ) + ")";
#
## DerivedRefWithParList ::= ModifiesKeyword TemplateRefWithParList
#DerivedRefWithParList = ModifiesKeyword + TemplateRefWithParList;
#
## InLineTemplate ::= [ ( Type | Signature ) Colon ] [ DerivedRefWithParList AssignmentChar ] TemplateBody
#InLineTemplate << Optional( ( Type | Signature ) + Colon ) + Optional( DerivedRefWithParList + AssignmentChar ) + TemplateBody;
#
## TemplateRefWithParList ::= ExtendedIdentifier [ TemplateActualParList ]
#TemplateRefWithParList = ExtendedIdentifier + Optional( TemplateActualParList );
#
## TemplateInstanceAssignment ::= Identifier ":=" InLineTemplate
#TemplateInstanceAssignment = Identifier + ":=" + InLineTemplate;
#
## InfinityKeyword ::= "infinity"
#InfinityKeyword = Keyword("infinity");
#
## Bound ::= ( [ "!" ] SingleExpression ) | ( [ Minus ] InfinityKeyword )
#Bound = ( Optional( "!" ) + SingleExpression ) | ( Optional( Minus ) + InfinityKeyword );
#
## Range ::= "(" Bound ".." Bound ")"
#Range = "(" + Bound + ".." + Bound + ")";
#
# PresentKeyword ::= "present"
PresentKeyword << Keyword("present");

## IfPresentKeyword ::= "ifpresent"
#IfPresentKeyword = Keyword("ifpresent");
#
## WildcardLengthMatch ::= LengthKeyword "(" SingleExpression ")"
#WildcardLengthMatch = LengthKeyword + "(" + SingleExpression + ")";
#
## TemplateList ::= "(" TemplateBody { "," TemplateBody }+ ")"
#TemplateList = "(" + TemplateBody + OneOrMore( "," + TemplateBody ) + ")";
#
## AnyOrOmit ::= "*"
#AnyOrOmit = "*";
#
## AnyValue ::= "?"
#AnyValue = "?";
#
## PermutationKeyword ::= "permutation"
#PermutationKeyword = Keyword("permutation");
#
## PermutationMatch ::= PermutationKeyword ListOfTemplates
#PermutationMatch = PermutationKeyword + ListOfTemplates;
#
## SupersetKeyword ::= "superset"
#SupersetKeyword = Keyword("superset");
#
## SupersetMatch ::= SupersetKeyword ListOfTemplates
#SupersetMatch = SupersetKeyword + ListOfTemplates;
#
## SubsetKeyword ::= "subset"
#SubsetKeyword = Keyword("subset");
#
## SubsetMatch ::= SubsetKeyword ListOfTemplates
#SubsetMatch = SubsetKeyword + ListOfTemplates;
#
## AllElementsFrom ::= AllKeyword FromKeyword TemplateBody
#AllElementsFrom = AllKeyword + FromKeyword + TemplateBody;
#
## TemplateListItem ::= TemplateBody | AllElementsFrom
#TemplateListItem = TemplateBody | AllElementsFrom;
#
## ListOfTemplates ::= "(" TemplateListItem { "," TemplateListItem } ")"
#ListOfTemplates = "(" + TemplateListItem + ZeroOrMore( "," + TemplateListItem ) + ")";
#
## ComplementKeyword ::= "complement"
#ComplementKeyword = "complement";
#
## Complement ::= ComplementKeyword ListOfTemplates
#Complement = ComplementKeyword + ListOfTemplates;
#
## PatternQuadruple ::= "\" "q" "(" Number "," Number "," Number "," Number ")"
#PatternQuadruple = "\" + "q" + "(" + Number + "," + Number + "," + Number + "," + Number + ")";
#
## EscapedPatternClassChar ::= "[" | "-" | "^" | "]"
#EscapedPatternClassChar = Literal("[") | Literal("-") | Literal("^") | Literal("]")
#
## NonSpecialPatternClassChar ::= Char
#NonSpecialPatternClassChar = Char;
#
## PatternClassChar ::= NonSpecialPatternClassChar | PatternQuadruple | "\" EscapedPatternClassChar
#PatternClassChar = NonSpecialPatternClassChar | PatternQuadruple | "\" + EscapedPatternClassChar;
#
## NonSpecialPatternChar ::= Char
#NonSpecialPatternChar = Char;
#
## PatternChar ::= NonSpecialPatternChar | PatternQuadruple
#PatternChar = NonSpecialPatternChar | PatternQuadruple;
#
# PatternElement ::= ( ( "\" ( "?" | "*" | "\" | "[" | "]" | "{" | "}" | """ | "|" | "(" | ")" | "#" | "+" | "d" | "w" | "t" | "n" | "r" | "s" | "b" ) ) | ( "?" | "*" | "\" | "|" | "+" ) | ( "[" [ "^" ] [ { PatternClassChar [ "-" PatternClassChar ] } ] "]" ) | ( "{" [ "\" ] ReferencedValue "}" ) | ( "\" "N" "{" ( ReferencedValue | Type ) "}" ) | ( """ """ ) | ( "(" PatternElement ")" ) | ( "#" ( Num | ( "(" Num "," [ Num ] ")" ) | ( "(" "," Num ")" ) ) ) | PatternChar )
#PatternElement = Forward();
#PatternElement << ( ( r'\' ( "?" | "*" | r'\' | "[" | "]" | "{" | "}" | '"' | r'|' | "(" | ")" | "#" | "+" | "d" | "w" | "t" | "n" | "r" | "s" | "b" ) ) | ( "?" | "*" | r"\" | r'|' | "+" ) | ( "[" + Optional( "^" ) + Optional( ZeroOrMore( PatternClassChar + [ + "-" + PatternClassChar ) ) + ] + "]" ) | ( "{" + Optional( "\" ) + ReferencedValue + "}" ) | ( "\" + "N" + "{" + ( ReferencedValue | Type ) + "}" ) | ( '"' + '"' ) | ( "(" + PatternElement + ")" ) | ( "#" + ( Num | ( "(" + Num + "," + Optional( Num ) + ")" ) | ( "(" + "," + Num + ")" ) ) ) | PatternChar );
#
### Pattern ::= """ { PatternElement } """
#Pattern = """ + ZeroOrMore( PatternElement ) + """;
#
## PatternKeyword ::= "pattern"
#PatternKeyword = Keyword("pattern");
#
## PatternParticle ::= Pattern | ReferencedValue
#PatternParticle = Pattern | ReferencedValue;
#
## CharStringMatch ::= PatternKeyword PatternParticle { "&" PatternParticle }
#CharStringMatch = PatternKeyword + PatternParticle + ZeroOrMore( "&" + PatternParticle );
#
## OctOrMatch ::= Oct | AnyValue | AnyOrOmit
#OctOrMatch = Oct | AnyValue | AnyOrOmit;
#
## OctetStringMatch ::= "'" { OctOrMatch } "'" "O"
#OctetStringMatch = "'" + ZeroOrMore( OctOrMatch ) + "'" + "O";
#
## HexOrMatch ::= Hex | AnyValue | AnyOrOmit
#HexOrMatch = Hex | AnyValue | AnyOrOmit;
#
## HexStringMatch ::= "'" { HexOrMatch } "'" "H"
#HexStringMatch = "'" + ZeroOrMore( HexOrMatch ) + "'" + "H";
#
## BinOrMatch ::= Bin | AnyValue | AnyOrOmit
#BinOrMatch = Bin | AnyValue | AnyOrOmit;
#
## BitStringMatch ::= "'" { BinOrMatch } "'" "B"
#BitStringMatch = "'" + ZeroOrMore( BinOrMatch ) + "'" + "B";
#
## ExtraMatchingAttributes ::= StringLength | IfPresentKeyword | ( StringLength IfPresentKeyword )
#ExtraMatchingAttributes = StringLength | IfPresentKeyword | ( StringLength + IfPresentKeyword );
#
## MatchingSymbol ::= Complement | ( AnyValue [ WildcardLengthMatch ] ) | ( AnyOrOmit [ WildcardLengthMatch ] ) | ListOfTemplates | Range | BitStringMatch | HexStringMatch | OctetStringMatch | CharStringMatch | SubsetMatch | SupersetMatch
#MatchingSymbol = Complement | ( AnyValue + Optional( WildcardLengthMatch ) ) | ( AnyOrOmit + Optional( WildcardLengthMatch ) ) | ListOfTemplates | Range | BitStringMatch | HexStringMatch | OctetStringMatch | CharStringMatch | SubsetMatch | SupersetMatch;
#
## ArrayElementSpec ::= Minus | PermutationMatch | TemplateBody
#ArrayElementSpec = Minus | PermutationMatch | TemplateBody;
#
## ArrayElementSpecList ::= ArrayElementSpec { "," ArrayElementSpec }
#ArrayElementSpecList = ArrayElementSpec + ZeroOrMore( "," + ArrayElementSpec );
#
## ArrayValueOrAttrib ::= "{" [ ArrayElementSpecList ] "}"
#ArrayValueOrAttrib = "{" + Optional( ArrayElementSpecList ) +"}"
#
## FieldOrBitNumber ::= SingleExpression
#FieldOrBitNumber = SingleExpression;
#
## ArrayOrBitRef ::= "[" FieldOrBitNumber "]"
#ArrayOrBitRef = "[" + FieldOrBitNumber + "]";
#
## ParRef ::= Identifier
#ParRef = Identifier;
#
## StructFieldRef ::= Identifier | PredefinedType | TypeReference
#StructFieldRef = Identifier | PredefinedType | TypeReference;
#
## FieldReference ::= StructFieldRef | ArrayOrBitRef | ParRef
#FieldReference = StructFieldRef | ArrayOrBitRef | ParRef;
#
## FieldSpec ::= FieldReference AssignmentChar ( TemplateBody | Minus )
#FieldSpec = FieldReference + AssignmentChar + ( TemplateBody | Minus );
#
## FieldSpecList ::= "{" FieldSpec { "," FieldSpec } "}"
#FieldSpecList = "{" + FieldSpec + ZeroOrMore( "," + FieldSpec ) + "}"
#
## SingleTemplateExpression ::= MatchingSymbol | ( TemplateRefWithParList [ ExtendedFieldReference ] )
#SingleTemplateExpression = MatchingSymbol | ( TemplateRefWithParList + Optional( ExtendedFieldReference ) );
#
## SimpleTemplateSpec ::= SingleTemplateExpression [ "&" SimpleSpec ]
#SimpleTemplateSpec = SingleTemplateExpression + Optional( "&" + SimpleSpec );
#
## SimpleSpec ::= ( SingleExpression [ "&" SimpleTemplateSpec ] ) | SimpleTemplateSpec
#SimpleSpec = ( SingleExpression + Optional( "&" + SimpleTemplateSpec ) ) | SimpleTemplateSpec;
#
## TemplateBody ::= ( SimpleSpec | FieldSpecList | ArrayValueOrAttrib ) [ ExtraMatchingAttributes ]
#TemplateBody = ( SimpleSpec | FieldSpecList | ArrayValueOrAttrib ) + Optional( ExtraMatchingAttributes );
#
## TemplateOrValueFormalPar ::= FormalValuePar | FormalTemplatePar
#TemplateOrValueFormalPar = FormalValuePar | FormalTemplatePar;
#
## TemplateOrValueFormalParList ::= TemplateOrValueFormalPar { "," TemplateOrValueFormalPar }
#TemplateOrValueFormalParList = TemplateOrValueFormalPar + ZeroOrMore( "," + TemplateOrValueFormalPar );
#
## ModifiesKeyword ::= "modifies"
#ModifiesKeyword = Keyword("modifies");
#
## DerivedDef ::= ModifiesKeyword ExtendedIdentifier
#DerivedDef = ModifiesKeyword + ExtendedIdentifier;
#
# TemplateKeyword ::= "template"
TemplateKeyword << Keyword("template");

## BaseTemplate ::= ( Type | Signature ) Identifier [ "(" TemplateOrValueFormalParList ")" ]
#BaseTemplate = ( Type | Signature ) + Identifier + Optional( "(" + TemplateOrValueFormalParList + ")" );
#
## TemplateDef ::= TemplateKeyword [ TemplateRestriction ] BaseTemplate [ DerivedDef ] AssignmentChar TemplateBody
#TemplateDef = TemplateKeyword + Optional( TemplateRestriction ) + BaseTemplate + Optional( DerivedDef ) + AssignmentChar + TemplateBody;
#
# ConstKeyword ::= "const"
ConstKeyword << Keyword("const");

## SingleConstDef ::= Identifier [ ArrayDef ] AssignmentChar ConstantExpression
#SingleConstDef = Identifier + Optional( ArrayDef ) + AssignmentChar + ConstantExpression;
#
## ConstList ::= SingleConstDef { "," SingleConstDef }
#ConstList = SingleConstDef + ZeroOrMore( "," + SingleConstDef );
#
## ConstDef ::= ConstKeyword Type ConstList
#ConstDef = ConstKeyword + Type + ConstList;
#
## PortElement ::= Identifier [ ArrayDef ]
#PortElement = Identifier + Optional( ArrayDef );
#
## PortInstance ::= PortKeyword ExtendedIdentifier PortElement { "," PortElement }
#PortInstance = PortKeyword + ExtendedIdentifier + PortElement + ZeroOrMore( "," + PortElement );
#
## ComponentElementDef ::= PortInstance | VarInstance | TimerInstance | ConstDef
#ComponentElementDef = PortInstance | VarInstance | TimerInstance | ConstDef;
#
## ComponentDefList ::= { ComponentElementDef [ WithStatement ] [ SemiColon ] }
#ComponentDefList = ZeroOrMore( ComponentElementDef + Optional( WithStatement ) + Optional( SemiColon ) );
#
## ComponentType ::= ExtendedIdentifier
#ComponentType = ExtendedIdentifier;
#
## ExtendsKeyword ::= "extends"
#ExtendsKeyword = Keyword("extends");
#
# ComponentKeyword ::= "component"
ComponentKeyword << Keyword("component");

## ComponentDef ::= ComponentKeyword Identifier [ ExtendsKeyword ComponentType { "," ComponentType } ] "{" [ ComponentDefList ] "}"
#ComponentDef = ComponentKeyword + Identifier + Optional( ExtendsKeyword + ComponentType + ZeroOrMore( "," + ComponentType ) ) + "ZeroOrMore(" + Optional( ComponentDefList ) + ")";
#
## ProcOrType ::= Signature | Type
#ProcOrType = Signature | Type;
#
## ProcOrTypeList ::= AllKeyword | ( ProcOrType { "," ProcOrType } )
#ProcOrTypeList = AllKeyword | ( ProcOrType + ZeroOrMore( "," + ProcOrType ) );
#
## MixedList ::= Direction ProcOrTypeList
#MixedList = Direction + ProcOrTypeList;
#
## MixedKeyword ::= "mixed"
#MixedKeyword = Keyword("mixed");
#
## MixedAttribs ::= MixedKeyword "{" { ( AddressDecl | MixedList | ConfigParamDef ) [ SemiColon ] }+ "}"
#MixedAttribs = MixedKeyword + "OneOrMore(" + ZeroOrMore( ( AddressDecl | MixedList | ConfigParamDef ) + Optional( SemiColon ) ) + ")";
#
## SignatureList ::= Signature { "," Signature }
#SignatureList = Signature + ZeroOrMore( "," + Signature );
#
## AllOrSignatureList ::= AllKeyword | SignatureList
#AllOrSignatureList = AllKeyword | SignatureList;
#
## ProcedureList ::= Direction AllOrSignatureList
#ProcedureList = Direction + AllOrSignatureList;
#
## ProcedureKeyword ::= "procedure"
#ProcedureKeyword = Keyword("procedure");
#
## ProcedureAttribs ::= ProcedureKeyword "{" { ( AddressDecl | ProcedureList | ConfigParamDef ) [ SemiColon ] }+ "}"
#ProcedureAttribs = ProcedureKeyword + "{" + OneOrMore( ( AddressDecl | ProcedureList | ConfigParamDef ) + Optional( SemiColon ) ) + "}"
#
## TypeList ::= Type { "," Type }
#TypeList = Type + ZeroOrMore( "," + Type );
#
# AllKeyword ::= "all"
AllKeyword << Keyword("all");

## AllOrTypeList ::= AllKeyword | TypeList
#AllOrTypeList = AllKeyword | TypeList;
#
## MessageKeyword ::= "message"
#MessageKeyword = Keyword("message");
#
## Direction ::= InParKeyword | OutParKeyword | InOutParKeyword
#Direction = InParKeyword | OutParKeyword | InOutParKeyword;
#
## MessageList ::= Direction AllOrTypeList
#MessageList = Direction + AllOrTypeList;
#
## AddressDecl ::= AddressKeyword Type
#AddressDecl = AddressKeyword + Type;
#
## UnmapParamDef ::= UnmapKeyword ParamKeyword "(" FormalValuePar { "," FormalValuePar } ")"
#UnmapParamDef = UnmapKeyword + ParamKeyword + "(" + FormalValuePar + ZeroOrMore( "," + FormalValuePar ) + ")";
#
## MapParamDef ::= MapKeyword ParamKeyword "(" FormalValuePar { "," FormalValuePar } ")"
#MapParamDef = MapKeyword + ParamKeyword + "(" + FormalValuePar + ZeroOrMore( "," + FormalValuePar ) + ")";
#
## ConfigParamDef ::= MapParamDef | UnmapParamDef
#ConfigParamDef = MapParamDef | UnmapParamDef;
#
## MessageAttribs ::= MessageKeyword "{" { ( AddressDecl | MessageList | ConfigParamDef ) [ SemiColon ] }+ "}"
#MessageAttribs = MessageKeyword + "OneOrMore(" + ZeroOrMore( ( AddressDecl | MessageList | ConfigParamDef ) + Optional( SemiColon ) ) + ")";
#
## PortDefAttribs ::= MessageAttribs | ProcedureAttribs | MixedAttribs
#PortDefAttribs = MessageAttribs | ProcedureAttribs | MixedAttribs;
#
# PortKeyword ::= "port"
PortKeyword << Keyword("port");

## PortDefBody ::= Identifier PortDefAttribs
#PortDefBody = Identifier + PortDefAttribs;
#
## PortDef ::= PortKeyword PortDefBody
#PortDef = PortKeyword + PortDefBody;
#
## LengthKeyword ::= "length"
#LengthKeyword = Keyword("length");
#
## StringLength ::= LengthKeyword "(" SingleExpression [ ".." Bound ] ")"
#StringLength = LengthKeyword + "(" + SingleExpression + Optional( ".." + Bound ) + ")";
#
## RangeDef ::= Bound ".." Bound
#RangeDef = Bound + ".." + Bound;
#
## TemplateOrRange ::= RangeDef | TemplateBody | Type
#TemplateOrRange = RangeDef | TemplateBody | Type;
#
## AllowedValuesSpec ::= "(" ( ( TemplateOrRange { "," TemplateOrRange } ) | CharStringMatch ) ")"
#AllowedValuesSpec = "(" + ( ( TemplateOrRange + ZeroOrMore( "," + TemplateOrRange ) ) | CharStringMatch ) + ")";
#
## SubTypeSpec ::= AllowedValuesSpec [ StringLength ] | StringLength
#SubTypeSpec = AllowedValuesSpec + Optional( StringLength ) | StringLength;
#
## SubTypeDef ::= Type ( Identifier | AddressKeyword ) [ ArrayDef ] [ SubTypeSpec ]
#SubTypeDef = Type + ( Identifier | AddressKeyword ) + Optional( ArrayDef ) + Optional( SubTypeSpec );
#
## Enumeration ::= Identifier [ "(" [ Minus ] Number ")" ]
#Enumeration = Identifier + Optional( "(" + [ + Minus ) + Number + ")" + ];
#
## EnumerationList ::= Enumeration { "," Enumeration }
#EnumerationList = Enumeration + ZeroOrMore( "," + Enumeration );
#
## EnumKeyword ::= "enumerated"
#EnumKeyword = Keyword("enumerated");
#
## EnumDef ::= EnumKeyword ( Identifier | AddressKeyword ) "{" EnumerationList "}"
#EnumDef = EnumKeyword + ( Identifier + | + AddressKeyword ) + "{" + EnumerationList + "}"
#
## SetOfDef ::= SetKeyword [ StringLength ] OfKeyword StructOfDefBody
#SetOfDef = SetKeyword + Optional( StringLength ) + OfKeyword + StructOfDefBody;
#
## StructOfDefBody ::= ( Type | NestedTypeDef ) ( Identifier | AddressKeyword ) [ SubTypeSpec ]
#StructOfDefBody = ( Type | NestedTypeDef ) + ( Identifier | AddressKeyword ) + Optional( SubTypeSpec );
#
## OfKeyword ::= "of"
#OfKeyword = Keyword("of");
#
## RecordOfDef ::= RecordKeyword [ StringLength ] OfKeyword StructOfDefBody
#RecordOfDef = RecordKeyword + Optional( StringLength ) + OfKeyword + StructOfDefBody;
#
## SetKeyword ::= "set"
#SetKeyword = Keyword("set");
#
## SetDef ::= SetKeyword StructDefBody
#SetDef = SetKeyword + StructDefBody;
#
## UnionFieldDef ::= ( Type | NestedTypeDef ) Identifier [ ArrayDef ] [ SubTypeSpec ]
#UnionFieldDef = ( Type | NestedTypeDef ) + Identifier + Optional( ArrayDef ) + Optional( SubTypeSpec );
#
## UnionDefBody ::= ( Identifier | AddressKeyword ) "{" UnionFieldDef { "," UnionFieldDef } "}"
#UnionDefBody = ( Identifier + | + AddressKeyword ) + "{" + UnionFieldDef + ZeroOrMore( "," + UnionFieldDef ) + "}";
#
## UnionKeyword ::= "union"
#UnionKeyword = Keyword("union");
#
## UnionDef ::= UnionKeyword UnionDefBody
#UnionDef = UnionKeyword + UnionDefBody;
#
# OptionalKeyword ::= "optional"
OptionalKeyword << Keyword("optional");

## NestedEnumDef ::= EnumKeyword "{" EnumerationList "}"
#NestedEnumDef = EnumKeyword + "{" + EnumerationList + "}"
#
## NestedSetOfDef ::= SetKeyword [ StringLength ] OfKeyword ( Type | NestedTypeDef )
#NestedSetOfDef = SetKeyword + Optional( StringLength ) + OfKeyword + ( Type | NestedTypeDef );
#
## NestedRecordOfDef ::= RecordKeyword [ StringLength ] OfKeyword ( Type | NestedTypeDef )
#NestedRecordOfDef = RecordKeyword + Optional( StringLength ) + OfKeyword + ( Type | NestedTypeDef );
#
## NestedSetDef ::= SetKeyword "{" [ StructFieldDef { "," StructFieldDef } ] "}"
#NestedSetDef = SetKeyword + "{" + Optional( delimitedList( StructFieldDef ) ) + "}";
#
## NestedUnionDef ::= UnionKeyword "{" UnionFieldDef { "," UnionFieldDef } "}"
#NestedUnionDef = UnionKeyword + "{" + delimitedList( UnionFieldDef ) + "}";
#
## NestedRecordDef ::= RecordKeyword "{" [ StructFieldDef { "," StructFieldDef } ] "}"
#NestedRecordDef = RecordKeyword + "{" + Optional( delimitedList( StructFieldDef ) ) + "}"
#
## NestedTypeDef ::= NestedRecordDef | NestedUnionDef | NestedSetDef | NestedRecordOfDef | NestedSetOfDef | NestedEnumDef
#NestedTypeDef = NestedRecordDef | NestedUnionDef | NestedSetDef | NestedRecordOfDef | NestedSetOfDef | NestedEnumDef;
#
## StructFieldDef ::= ( Type | NestedTypeDef ) Identifier [ ArrayDef ] [ SubTypeSpec ] [ OptionalKeyword ]
#StructFieldDef = ( Type | NestedTypeDef ) + Identifier + Optional( ArrayDef ) + Optional( SubTypeSpec ) + Optional( OptionalKeyword );
#
## StructDefBody ::= ( Identifier | AddressKeyword ) "{" [ StructFieldDef { "," StructFieldDef } ] "}"
#StructDefBody = ( Identifier + | + AddressKeyword ) + "{" + Optional( delimitedList( StructFieldDef ) ) + "}";
#
## RecordKeyword ::= "record"
#RecordKeyword = Keyword("record");
#
## RecordDef ::= RecordKeyword StructDefBody
#RecordDef = RecordKeyword + StructDefBody;
#
## StructuredTypeDef ::= RecordDef | UnionDef | SetDef | RecordOfDef | SetOfDef | EnumDef | PortDef | ComponentDef
#StructuredTypeDef = RecordDef | UnionDef | SetDef | RecordOfDef | SetOfDef | EnumDef | PortDef | ComponentDef;
#
# TypeDefKeyword ::= "type"
TypeDefKeyword << Keyword("type");

## TypeDefBody ::= StructuredTypeDef | SubTypeDef
#TypeDefBody = StructuredTypeDef | SubTypeDef;
#
## TypeDef ::= TypeDefKeyword TypeDefBody
#TypeDef = TypeDefKeyword + TypeDefBody;
#
## Visibility ::= "public" | "friend" | "private"
#Visibility = Keyword("public") | Keyword("friend") | Keyword("private");
#
## ModuleDefinition ::= ( ( [ Visibility ] ( TypeDef | ConstDef | TemplateDef | ModuleParDef | FunctionDef | SignatureDef | TestcaseDef | AltstepDef | ImportDef | ExtFunctionDef | ExtConstDef ) ) | ( [ "public" ] GroupDef ) | ( [ "private" ] FriendModuleDef ) ) [ WithStatement ]
#ModuleDefinition = ( ( Optional( Visibility ) + ( TypeDef | ConstDef | TemplateDef | ModuleParDef | FunctionDef | SignatureDef | TestcaseDef | AltstepDef | ImportDef | ExtFunctionDef | ExtConstDef ) ) | ( Optional( "public" ) + GroupDef ) | ( Optional( "private" ) + FriendModuleDef ) ) + Optional( WithStatement );
#
## ModuleDefinitionsList ::= { ModuleDefinition [ SemiColon ] }+
#ModuleDefinitionsList = OneOrMore( ModuleDefinition + Optional( SemiColon ) );
#
## LanguageKeyword ::= "language"
#LanguageKeyword = Keyword("language");
#
## LanguageSpec ::= LanguageKeyword FreeText { "," FreeText }
#LanguageSpec = LanguageKeyword + FreeText + ZeroOrMore( "," + FreeText );
#
## ModuleId ::= Identifier [ LanguageSpec ]
#ModuleId = Identifier + Optional( LanguageSpec );
#
## TTCN3ModuleKeyword ::= "module"
#TTCN3ModuleKeyword = Keyword("module");
#
## TTCN3Module ::= TTCN3ModuleKeyword ModuleId "{" [ ModuleDefinitionsList ] [ ModuleControlPart ] "}" [ WithStatement ] [ SemiColon ]
#TTCN3Module = TTCN3ModuleKeyword + ModuleId + "{" + Optional( ModuleDefinitionsList ) + Optional( ModuleControlPart ) + "}" + Optional( WithStatement ) + Optional( SemiColon );
#

def parse(text):
   print "**>>", (TTCN3Module & Eof()).parse(text);
