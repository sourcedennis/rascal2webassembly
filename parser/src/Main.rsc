module Main

import IO;
import util::FileSystem;
import ParseTree;
import lang::webassembly::Syntax;

void main( ) {
  //loc lTestSuite = |project://testsuite/|;
  // The test suite also contains proposed extensions in sub directories
  // Therefore only take .wast files in root directory.
  //set[loc] lTestFiles = { f | f <- lTestSuite.ls, !isDirectory(f), f.extension == "wast" };
  //println( lTestFiles );
  
  str s = "(module
          ')";
  parse(#start[WebAssembly], s);
}