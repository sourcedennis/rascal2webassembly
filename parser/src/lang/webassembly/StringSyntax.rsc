module lang::webassembly::StringSyntax

// By the WebAssembly spec, strings are lexical entities
// However, they need to be converted to UTF-8 byte sequences
// for their binary representation in the ADT. To facilitate this
// conversion this complementary grammar is introduced

start syntax String = "\"" StringElem* "\"";

syntax StringElem = StringChar
                  | "\\" HexDigit HexDigit
                  ;

syntax StringChar = [ \u0020-\uD7FF \uE000-\u10FFFF ] - [ \" \\ \u007F ]
                  | "\\" EscapableChar
                  | HexEscape
                  ;

lexical HexEscape = "\\u" HexNum;

lexical EscapableChar = [ t n r \" \' \\ ];

lexical Digit = [0-9];

syntax HexDigit = Digit | [a-fA-F];

syntax NumDigits = Digit
                  | NumDigits "_"? Digit
                  ;

syntax HexNumDigits = HexDigit
                    | HexNumDigits "_"? HexDigit
                    ;
                     
syntax HexNum = HexNumDigits !>> [ 0-9 a-f A-F ];