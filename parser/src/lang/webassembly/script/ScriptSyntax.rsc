module lang::webassembly::script::ScriptSyntax

extend lang::webassembly::Syntax;

start syntax WebAssemblyScript = Cmd* $
                               | InlineModule
                               ;

syntax ScriptModule = Module
                    | "(" "module" Id? "quote" String* ")"
                    | "(" "module" Id? "binary" String* ")"
                    ;

syntax InlineModule = ModuleField*;

syntax Cmd = Action
           | Assertion
           | ScriptModule
           | Register
           ;
           
syntax Register = "(" "register" Name Id? ")";
           
syntax Action = "(" "invoke" Id? Name Const* ")"
              | "(" "get" Id? Name ")"
              ;
              
syntax Assertion = "(" "assert_malformed" ScriptModule String ")"
                 | "(" "assert_invalid" ScriptModule String ")"
                 | "(" "assert_unlinkable" ScriptModule String ")"
                 | "(" "assert_trap" ScriptModule String ")"
                 | "(" "assert_trap" Action String ")"
                 | "(" "assert_return" Action Const* ")"
                 | "(" "assert_return_canonical_nan" Action ")"
                 | "(" "assert_return_arithmetic_nan" Action ")"
                 | "(" "assert_exhaustion" Action String ")"
                 ;

syntax Const = "(" "i32.const" IN ")"
             | "(" "i64.const" IN ")"
             | "(" "f32.const" FN ")"
             | "(" "f64.const" FN ")"
             ;
