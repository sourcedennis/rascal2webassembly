module lang::webassembly::Syntax

layout Layout = Space*;

// ### Syntax Format ###

start syntax WebAssembly = Module;

// -- Types --

lexical ValType = "i32" | "i64" | "f32" | "f64";

syntax ResultType = Result?;

// TODO: Validate vec(param)
syntax FuncType = "(" "func" ( Param | AbbrParam )* Result ")"
                | "(" "func" Param ")"
                ;

syntax Param = "(" "param" Id? ValType ")"
             | AbbrParam
             ;
             
syntax AbbrParam = "(" "param" ValType* ")";

syntax Result = "(" "result" ValType ")"
              | AbbrResult
              ;
              
syntax AbbrResult = "(" "result" ValType* ")";

syntax Limits = U32 | U32 U32;

syntax MemType = Limits;

syntax TableType = Limits ElemType;

syntax ElemType = "anyfunc";

syntax GlobalType = ValType
                  | "(" "mut" ValType ")"
                  ;
                  
// -- Instructions --

syntax Instr = PlainInstr | BlockInstr | FoldedInstr;

syntax Label = Id
             |
             ;

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
                  | "f64_convert_u/i32"
                  | "f64.convert_s/i64"
                  | "f64_convert_u/i64"
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
                   | "(" "if" Label ResultType FoldedInstr* "(" "then" Instr* ")" ")" // TODO: Verify this one
                   ;

syntax MemArg = Offset Align;

syntax Expr = Instr*;

lexical TypeIdx = U32 | Id;
lexical FuncIdx = U32 | Id;
lexical TableIdx = U32 | Id;
lexical MemIdx = U32 | Id;
lexical GlobalIdx = U32 | Id;
lexical LocalIdx = U32 | Id;
lexical LabelId = U32 | Id;

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

syntax TypeUse = "(" "type" TypeIdx ")"
               | "(" "type" TypeIdx ")" Param* Result*
               | Param* Result*
               ;

syntax Import = "(" "import" Name Name ImportDesc ")";

syntax ImportDesc = "(" "func" Id? TypeUse ")"
                  | "(" "table" Id? TableType ")"
                  | "(" "memory" Id? MemType ")"
                  | "(" "global" Id? GlobalType ")"
                  ;
                  
syntax Func = "(" "func" Id? TypeUse Local* Instr* ")"
            | "(" "func" Id? "(" "import" Name Name ")" TypeUser ")"
            //| "(" "func" Id? ( "(" "export" Name ")" )* ")" // Not sure what this one is supposed to do. TODO: Fix
            ;

syntax Local = "(" "local" Id? ValType ")"
             | AbbrLocal
             ;

syntax AbbrLocal = "(" "local" ValType* ")";

syntax Table = "(" "table" Id? TableType ")";

syntax AbbrTable = "(" "table" Id? ElemType "(" "elem" FuncIdx* ")" ")"
                 | "(" "table" Id "(" "import" Name Name ")" TableType ")"
                 //| "(" "table" Id? "(" "export" Name ")" // Not sure what this one is supposed to do. TODO: Fix
                 ;
                 
syntax Mem = "(" "memory" Id? MemType ")"
           | AbbrMem
           ;

syntax AbbrMem = "(" "memory" Id? "(" DataString ")" ")"
               | "(" "memory" Id? "(" "import" Name Name ")" MemType ")"
               //| "(" "memory" Id? "(" "export" Name ")" ")" // Not sure what this one is supposed to do. TODO: Fix
               ;

syntax Global = "(" "global" Id? GlobalType Expr ")"
              | AbbrGlobal
              ;
              
syntax AbbrGlobal = "(" "global" Id? "(" "import" Name Name ")" GlobalType ")"
                  //| // Another one. Not sure what this one is supposed to do. TODO: Fix
                  ;
                  
syntax Export = "(" "export" Name ExporDesc ")";

syntax ExportDesc = "(" "func" FuncIdx ")"
                  | "(" "table" TableIdx ")"
                  | "(" "memory" MemIdx ")"
                  | "(" "global" GlobalIdx ")"
                  ;
                  
syntax Start = "(" "start" FuncIdx ")";

syntax Elem = "(" "elem" TableIdx? "(" "offset" Expr ")" FuncIdx* ")"
            | AbbrElem
            ;
            
syntax AbbrElem = "(" "elem" TableIdx? Instr FuncIdx* ")"; // TODO: Verify this

syntax Data = "(" "data" MemIdx? "(" "offset" Expr ")" DataString ")"
            | AbbrData
            ;
            
syntax AbbrData = "(" "data" MemIdx? Instr FuncIdx* ")"; // TODO: Verify this

syntax DataString = String*;

syntax Module = "(" "module" Id ModuleField* ")"
              | "(" "module" ModuleField* ")";

syntax ModuleField = Type
                   | Import
                   | Func
                   | Table
                   | Mem
                   | Global
                   | Export
                   | Start
                   | Elem
                   | Data;
                 
// ### Lexical Format ###

lexical Char = [ \u0000-\uD7FF \uE000-\u10FFFF ];

lexical Token = Keyword !>> [a-z $]
              | UN
              | SN
              | FN
              | String
              | Id
              | "("
              | ")"
              ;

lexical Keyword = "pineapple"; // TODO

lexical Space = " "
              | Format
              | Comment;
              
lexical Format = [\t\n\r];

lexical Comment = LineComment
                | BlockComment
                ;
                
lexical LineComment = ";;" LineChar* "\n"
                    | ";;" LineChar* $
                    ;

lexical LineChar = Char \ "\n";

lexical BlockComment = "(;" BlockChar* ";)";

lexical BlockChar = Char \ (";" | "(")
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

lexical Num = Digit
            | Num "_"? Digit
            ;

lexical HexNum = HexDigit
               | HexNum "_"? HexDigit
               ;

// TODO: Satisfy 'n < 2^N'
lexical UN = Num !>> [0-9]
           | "0x" HexNum !>> [a-fA-F0-9]
           ;

// TODO: Satisfy '-2^(N-1) <= n < 2^(N-1)'
lexical SN = Sign Num !>> [0-9]
           | Sign "0x" HexNum !>> [a-fA-F0-9]
           ;
           
lexical IN = UN | SN; // TODO: Remove?

lexical U32 = UN;
lexical U64 = UN;
lexical S32 = SN;
lexical S64 = SN;
lexical I32 = IN;
lexical I64 = IN;

lexical Frac = Digit Frac
             | Digit "_" Digit Frac
             |
             ;
             
lexical HexFrac = HexDigit HexFrac
                | HexDigit "_" HexDigit HexFrac
                |
                ;
                
lexical Float = Num "." Frac
              | Num [Ee] PMSign Num
              | Num "." Frac [Ee] Sign Num
              ;

lexical HexFloat = "0x" HexNum "." HexFrac
                 | "0x" HexNum [Pp] Sign Num
                 | "0x" HexNum "." HexFrac [Pp] Sign Num
                 ;
                 
lexical FN = Sign FNMag;

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
                   
lexical StringChar = Char \ [\u0000-\u0019 \u007F \" \']
                   | [\t\n\r\"\'\\]
                   | "\\u" HexNum
                   ;
                   
lexical Name = String; // TODO: Look at (must be valid UTF-8)

lexical Id = "$" IdChar+ !>> [ 0-9 a-z A-Z ]; //[0-9 A-Z a-z !#$%&\'*+\-./:\<=\>?@\\^_`|~]

// Any printable ASCII character that does not contain a space, quotation mark, comma, semicolon, or bracket
lexical IdChar = [ 0-9 a-z A-Z ]; //[0-9 A-Z a-z !#$%&\'*+\-./:\<=\>?@\\^_`|~]

