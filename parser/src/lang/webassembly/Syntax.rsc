module lang::webassembly::Syntax

layout Layout = Space* !>> " " !>> [\t\n\r] !>> ";;" !>> "(;";

// ### Syntax Format ###

start syntax WebAssembly = Module;

// -- Types --

lexical ValType = "i32" | "i64" | "f32" | "f64";

syntax ResultType = Result?;

syntax FuncType = "(" "func" Param* Result* ")"
                ;

syntax Param = "(" "param" Id ValType ")"
             | "(" "param" ValType* ")"
             ;

syntax Result = "(" "result" Id ValType ")"
              | "(" "result" ValType* ")"
              ;

syntax Limits = U32 | U32 U32;

syntax MemType = Limits;

syntax TableType = Limits ElemType;

syntax ElemType = "anyfunc";

syntax GlobalType = ValType
                  | "(" "mut" ValType ")"
                  ;
                  
// -- Instructions --

syntax Instr = PlainInstr | BlockInstr | FoldedInstr;

syntax Label = Id?;

// TODO: Validate Id constraint
syntax BlockInstr = "block" Label ResultType Instr* "end" Id?
                  | "loop" Label ResultType Instr* "end" Id?
                  | "if" Label ResultType Instr* "else" Id? Instr* "end" Id?
                  | "if" Label ResultType Instr* "end" Id?
                  ;
                  
syntax PlainInstr = "unreachable"
                  | "nop"
                  | "br" LabelIdx
                  | "br_if" LabelIdx
                  | "br_table" LabelIdx* LabelIdx
                  | "return"
                  | "call" FuncIdx
                  | "call_indirect" TypeUse
                  | "drop"
                  | "select"
                  | "get_local" LocalIdx
                  | "set_local" LocalIdx
                  | "tee_local" LocalIdx
                  | "get_global" GlobalIdx
                  | "set_global" GlobalIdx
                  | NxxArgInstr MemArg
                  | IxxArgInstr MemArg
                  | "i64.load32_s" MemArg
                  | "i64.load32_u" MemArg
                  | "i64.store32" MemArg
                  | "memory.size"
                  | "memory.grow"
                  | "i32.const" I32
                  | "i64.const" I64
                  | "f32.const" F32
                  | "f64.const" F64
                  | NxxInstr
                  | IxxInstr
                  | FxxInstr
                  | "i32.wrap/i64"
                  | "i32.trunc_s/f32"
                  | "i32.trunc_u/f32"
                  | "i32.trunc_s/f64"
                  | "i32.trunc_u/f64"
                  | "i64.extend_s/i32"
                  | "i64.extend_u/i32"
                  | "i64.trunc_s/f32"
                  | "i64.trunc_u/f32"
                  | "i64.trunc_s/f64"
                  | "i64.trunc_u/f64"
                  | "f32.convert_s/i32"
                  | "f32.convert_u/i32"
                  | "f32.convert_s/i64"
                  | "f32.convert_u/i64"
                  | "f32.demote/f64"
                  | "f64.convert_s/i32"
                  | "f64.convert_u/i32"
                  | "f64.convert_s/i64"
                  | "f64.convert_u/i64"
                  | "f64.promote/f32"
                  | "i32.reinterpret/f32"
                  | "i64.reinterpret/f64"
                  | "f32.reinterpret/i32"
                  | "f64.reinterpret/i64"
                  ;

syntax FoldedInstr = "(" PlainInstr FoldedInstr* ")"
                   | "(" "block" Label ResultType Instr* ")"
                   | "(" "loop" Label ResultType Instr* ")"
                   | "(" "if" Label ResultType FoldedInstr* "(" "then" Instr* ")" "(" "else" Instr* ")" ")"
                   | "(" "if" Label ResultType FoldedInstr* "(" "then" Instr* ")" ")"
                   ;

syntax MemArg = Offset Align;

syntax Expr = Instr*;

lexical TypeIdx = U32 | Id;
lexical FuncIdx = U32 | Id;
lexical TableIdx = U32 | Id;
lexical MemIdx = U32 | Id;
lexical GlobalIdx = U32 | Id;
lexical LocalIdx = U32 | Id;
lexical LabelIdx = U32 | Id;

lexical Offset = "offset=" U32
               |
               ;
              
lexical Align = "align=" U32
              |
              ;

lexical Ixx = "i" ("32" | "64");

lexical Fxx = "f" ("32" | "64");

lexical Nxx = Ixx | Fxx;

lexical NxxArgInstr = Nxx ".load"
                    | Nxx ".store"
                    ;

lexical IxxArgInstr = Ixx ".load8_s"
                    | Ixx ".load8_u"
                    | Ixx ".load16_s"
                    | Ixx ".load16_u"
                    | Ixx ".store8"
                    | Ixx ".store16"
                    ;
                    
lexical NxxInstr = Nxx ".add"
                 | Nxx ".sub"
                 | Nxx ".mul"
                 | Nxx ".eq"
                 | Nxx ".ne"
                 ;
                 
lexical IxxInstr = Ixx ".clz"
                 | Ixx ".ctz"
                 | Ixx ".popcnt"
                 | Ixx ".div_s"
                 | Ixx ".div_u"
                 | Ixx ".rem_s"
                 | Ixx ".rem_u"
                 | Ixx ".and"
                 | Ixx ".or"
                 | Ixx ".xor"
                 | Ixx ".shl"
                 | Ixx ".shr_s"
                 | Ixx ".shr_u"
                 | Ixx ".rotl"
                 | Ixx ".rotr"
                 | Ixx ".eqz"
                 | Ixx ".lt_s"
                 | Ixx ".lt_u"
                 | Ixx ".gt_s"
                 | Ixx ".gt_u"
                 | Ixx ".le_s"
                 | Ixx ".le_u"
                 | Ixx ".ge_s"
                 | Ixx ".ge_u"
                 ;
                 
lexical FxxInstr = Fxx ".abs"
                 | Fxx ".neg"
                 | Fxx ".ceil"
                 | Fxx ".floor"
                 | Fxx ".trunc"
                 | Fxx ".nearest"
                 | Fxx ".sqrt"
                 | Fxx ".div"
                 | Fxx ".min"
                 | Fxx ".max"
                 | Fxx ".copysign"
                 | Fxx ".lt"
                 | Fxx ".gt"
                 | Fxx ".le"
                 | Fxx ".ge"
                 ;

// -- Modules --

syntax Type = "(" "type" Id? FuncType ")";

// TypeUse is optional under the condition that it is entirely replaced by inline parameter and result declarations
syntax TypeUse = "(" "type" TypeIdx ")" Param* Result*
               | Param* Result*
               ;

syntax Import = "(" "import" Name Name ImportDesc ")";

syntax ImportDesc = "(" "func" Id? TypeUse ")" // TODO: Check TypeUse
                  | "(" "table" Id? TableType ")"
                  | "(" "memory" Id? MemType ")"
                  | "(" "global" Id? GlobalType ")"
                  ;
                  
// A function can have zero or more inline exports, followed by zero or one inline import
// A function with an inline import has no body
syntax Func = "(" "func" Id? FuncFields ")";

syntax FuncFields = InlineExport FuncFields
                  | InlineImport TypeUse
                  | TypeUse FuncFieldsBody
                  ;

syntax FuncFieldsBody = FuncBody;

syntax FuncBody = Local* Instr*;

syntax InlineImport = "(" "import" Name Name ")";

syntax InlineExport = "(" "export" Name ")";

syntax Local = "(" "local" Id ValType ")"
             | "(" "local" ValType* ")"
             ;

syntax Table = "(" "table" Id? TableFields ")";

syntax TableFields = InlineImport? TableType
                   | InlineExport TableFields
                   | ElemType InlineElem
                   ;
                 
syntax Mem = "(" "memory" Id? MemFields ")";

syntax MemFields = InlineExport MemFields
                 | "(" "data" DataString ")"
                 | InlineImport? MemType
                 ;

syntax Global = "(" "global" Id? GlobalFields ")"
              ;
              
syntax GlobalFields = InlineExport GlobalFields
                    | GlobalType Expr
                    | InlineImport GlobalType
                    ;
                  
syntax Export = "(" "export" Name ExportDesc ")";

syntax ExportDesc = "(" "func" FuncIdx ")"
                  | "(" "table" TableIdx ")"
                  | "(" "memory" MemIdx ")"
                  | "(" "global" GlobalIdx ")"
                  ;
                  
syntax Start = "(" "start" FuncIdx ")";

syntax InlineElem = "(" "elem" FuncIdx* ")";

syntax Elem = "(" "elem" TableIdx? ElemFields ")";
            
syntax ElemFields = "(" "offset" Expr ")" FuncIdx*
                  | Instr FuncIdx*
                  ;

syntax Data = "(" "data" MemIdx? DataFields ")"
            ;
            
syntax DataFields = "(" "offset" Expr ")" DataString
                  | Instr DataString
                  ;

syntax DataString = String*;

syntax Module = "(" "module" Id? ModuleField* ")";

syntax ModuleField = Type
                   | Import
                   | Func
                   | Table
                   | Mem
                   | Global
                   | Export
                   | Start
                   | Elem
                   | Data
                   ;
                 
// ### Lexical Format ###

lexical Space = " "
              | Format
              | Comment
              ;
              
lexical Format = [\t\n\r];

lexical Comment = LineComment
                | BlockComment
                ;
                
lexical LineComment = ";;" LineChar* "\n"
                    | ";;" LineChar* $ !>> "\n" // A newline should be consumed, as per the first case
                    ;

lexical LineChar = [ \u0000-\uD7FF \uE000-\u10FFFF ] - [ \n ];

lexical BlockComment = "(;" BlockChar* ";)";

lexical BlockChar = [ \u0000-\uD7FF \uE000-\u10FFFF ] - [ ; ( ]
                  | ";" !>> ")"
                  | "(" !>> ";"
                  | BlockComment
                  ;
// --- Values ---

lexical Sign = "+"
             | "-"
             |
             ;

lexical Digit = [0-9];

lexical HexDigit = Digit | [a-fA-F];

lexical NumDigits = Digit
                  | NumDigits "_"? Digit
                  ;
                  
lexical Num = NumDigits !>> [0-9];

lexical HexNumDigits = HexDigit
                     | HexNumDigits "_"? HexDigit
                     ;
                     
lexical HexNum = HexNumDigits !>> [ 0-9 a-f A-F ];

// TODO: Satisfy 'n < 2^N'
lexical UN = Num
           | "0x" HexNum
           ;

// TODO: Satisfy '-2^(N-1) <= n < 2^(N-1)'
lexical SN = Sign Num
           | Sign "0x" HexNum
           ;
           
lexical IN = SN;

lexical U32 = UN;
lexical U64 = UN;
lexical S32 = SN;
lexical S64 = SN;
lexical I32 = IN;
lexical I64 = IN;

// Modified
lexical Frac = Digit ( "_"? Digit )*
             |
             ;

// Modified
lexical HexFrac = HexDigit ( "_"? HexDigit )*
                |
                ;

lexical Float = Num "." Frac
              | Num [Ee] Sign Num
              | Num "." Frac [Ee] Sign Num
              ;

lexical HexFloat = "0x" HexNum "." HexFrac
                 | "0x" HexNum [Pp] Sign Num
                 | "0x" HexNum "." HexFrac [Pp] Sign Num
                 ;
                 
lexical FN = Sign FNMag
           // Not part of official syntax specification. Integer values do, apparently, not count as floats
           // However, the test suite has them like that.
           | SN
           ;

lexical FNMag = Float
              | HexFloat
              | "inf"
              | "nan"
              | "nan:0x" HexNum
              ;
              
lexical F32 = FN;
lexical F64 = FN;

lexical String = "\"" StringElem* "\"";

lexical StringElem = StringChar
                   | "\\" HexDigit HexDigit
                   ;

lexical StringChar = [ \u0020-\uD7FF \uE000-\u10FFFF ] - [ \" \\ \u007F ]
                   | "\\" EscapableChar
                   | "\\u" HexNum
                   ;
                   
lexical EscapableChar = [ t n r \" \' \\ ];
                   
lexical Name = String; // TODO: Look at (must be valid UTF-8)

lexical Id = "$" IdChar+ !>> [0-9 A-Z a-z !#$%&\'*+\-./:\<=\>?@\\^_`|~];

// Any printable ASCII character that does not contain a space, quotation mark, comma, semicolon, or bracket
lexical IdChar = [0-9 A-Z a-z !#$%&\'*+\-./:\<=\>?@\\^_`|~];

