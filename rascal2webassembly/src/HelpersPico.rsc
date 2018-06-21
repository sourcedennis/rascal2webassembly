module HelpersPico

import ParseTree;
import demo::lang::Pico::Syntax;
import demo::lang::Pico::Abstract;

// These are in a separate file, otherwise they seem to interfere with WebAssembly's grammar definition

Program parsePico( loc l )
  = parse( #Program, l );
  
PROGRAM toADT( Program t ) = implode( #PROGRAM, t );
