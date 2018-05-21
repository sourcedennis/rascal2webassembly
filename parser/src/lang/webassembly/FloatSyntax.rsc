module lang::webassembly::FloatSyntax

// By the WebAssembly spec, floats are lexical entities
// However, they need to be converted to Rascal reals. Though not all
// WebAssembly textual floats can be parsed with Rascals toReal()
// function. So these need to be parsed manually. Hence, this
// additional grammar is introduced.

start syntax FN = Sign FNMag
                // Not part of official syntax specification. Integer values do, apparently, not count as floats
                // However, the test suite has them like that.
                | SN
                ;
           
syntax Sign = "+"
            | "-"
            |
            ;
             
syntax Digit = [0-9];

syntax HexDigit = Digit | [a-fA-F];

syntax NumDigits = Digit
                 | NumDigits "_"? Digit
                 ;
                  
syntax Num = NumDigits !>> [0-9];

syntax HexNumDigits = HexDigit
                    | HexNumDigits "_"? HexDigit
                    ;
                     
syntax HexNum = HexNumDigits !>> [ 0-9 a-f A-F ];

// TODO: Satisfy '-2^(N-1) <= n < 2^(N-1)'
syntax SN = Sign Num
          | Sign "0x" HexNum
          ;

// Modified
syntax Frac = Digit ( "_"? Digit )*
            |
            ;

// Modified
syntax HexFrac = HexDigit ( "_"? HexDigit )*
               |
               ;

syntax Float = Num "." Frac
              | Num [Ee] Sign Num
              | Num "." Frac [Ee] Sign Num
              ;

syntax HexFloat = "0x" HexNum "." HexFrac
                | "0x" HexNum [Pp] Sign Num
                | "0x" HexNum "." HexFrac [Pp] Sign Num
                ;

syntax ArithmeticNan = "nan:0x" HexNum;

syntax FNMag = Float
             | HexFloat
             | "inf"
             | "nan"
             | ArithmeticNan
             ;