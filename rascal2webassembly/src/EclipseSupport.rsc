module EclipseSupport

import ParseTree;
import util::IDE;
import IO;

import lang::webassembly::Syntax;
import lang::webassembly::ScriptSyntax;
import Helpers;

public void registerWebAssembly( ) {
  clearLanguage( "webassembly" );
  registerLanguage( "webassembly", "wast", parseWasmScript );
  
  // These don't work. Frankly, I'm not entirely sure what/when they would
  registerContributions( "webassembly", { annotator( wasmAnnotator ), setupMenu( ) } );
}

private WebAssemblyScript wasmAnnotator( WebAssemblyScript s )
  = visit( s ) {
  case Func f: {
    println( "Annotate" );
    println( f );
    f@doc = "bob";
    //f@link = |project://testsuite/address.wast|;
    //f@foldable = true;
    insert f;
  }
  };

public Contribution setupMenu( )
  = popup(menu("MyMenu", [action("Example item", void (Tree t, loc s) { println("<t> @ <s>");})]));

// Does not work either
public void setupConsole( ) {
  str newLineCallback( str s ) {
    return "Yes";
  }
  createConsole( "webassembly", "WASM\>", newLineCallback );
}