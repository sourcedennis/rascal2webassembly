module lang::pico2wasm::WasmFunctions

import lang::webassembly::Syntax;
import lang::webassembly::Abstract;
import lang::webassembly::Syntax2AST;
import lang::webassembly::Desugar;
import ParseTree;
import IO; // temp

// type 0: i32 => i32

/*
 * ### malloc:
 * A very simple implementation of malloc. No book-keeping of allocations is
 * kept at all, as freeing after allocation is not possible. Memory can
 * therefore only grow. Because of this property, this malloc implementation
 * should not be used in practice. However, for simple applications this should
 * pose no problem.
 *
 * Consider memory as conforming to the following stucture:
 *   UINT32 usedSize
 *   Block[] blocks
 *
 * Block:
 *   UINT8[] data
 *
 *
 * Analogous implementation in pseudo-C (of malloc):
 * 
 * uint8* memory; // all memory here
 * 
 * uint32 malloc( uint32 size ) {
 *   uint32 usedMemSize = ((uint32 *) memory )[ 0 ];
 *   uint32 newMemSize = usedMemSize + size;
 *   if ( newMemSize > size( memory ) * 64KiB ) {
 *     numNeededPages = ( newMemSize - size( memory ) * 64KiB + 64KiB - 1 ) / 64KiB;
 *     if ( grow_memory( numNeededPages ) == -1 ) {
 *       return 0;
 *     }
 *   }
 *   ((uint32 *) memory )[ 0 ] = newMemSize;
 *   return usedMemSize;
 * }
 *
 *
 * ### strlen:
 * uint32 strlen( uint32 str ) {
 *   uint32 count = 0;
 *   while ( true ) {
 *     if ( memory[str] == 0 )
 *       return count;
 *     str = str + 1;
 *   }
 * }
 *
 *
 * ### concat
 * uint32 concat( uint32 str1, uint32 str2 ) {
 *   uint32 dst = malloc( strlen( str1 ) + strlen( str2 ) + 1 );
 *   uint32 dstIt = dst;
 *   while ( mem[ str1 ] != 0 ) {
 *     mem[ dstIt ] = mem[ str1 ];
 *     src1++;
 *     dstIt++;
 *   }
 *   while ( mem[ str2 ] != 0 ) {
 *     mem[ dstIt ] = mem[ str2 ];
 *     src2++;
 *     dstIt++;
 *   }
 *   mem[ dstIt ] = 0;
 *   return dst;
 * }
 */
private str baseModule = "(module
                         '  (func $malloc (param $size i32) (result i32)
                         '    (local $usedMemSize i32)
                         '    (local $newMemSize i32)
                         '    (local $memSize i32)
                         '
                         '    (i32.gt_u
                         '      (tee_local $newMemSize
                         '        (i32.add
                         '          (tee_local $usedMemSize (i32.load (i32.const 0)))
                         '          (get_local $size)
                         '        )
                         '      )
                         '      (tee_local $memSize
                         '        (i32.shl (memory.size) (i32.const 16))
                         '      )
                         '    )
                         '    (if (result i32)
                         '      (then
                         '        (i32.eq
                         '          (memory.grow
                         '            (i32.div_u
                         '              (i32.add
                         '                (i32.sub (get_local $newMemSize) (get_local $memSize))
                         '                (i32.const 65535)
                         '              )
                         '              (i32.const 65536)
                         '            )
                         '          )
                         '          (i32.const -1)
                         '        )
                         '        (if (result i32)
                         '          (then
                         '            (return (i32.const 0))
                         '          )
                         '        )
                         '      )
                         '    )
                         '    (i32.store (i32.const 0) (get_local $newMemSize))
                         '    (get_local $usedMemSize)
                         '  )
                         '  
                         '  (func $strlen (param $str i32) (result i32)
                         '    (local $count i32)
                         '    
                         '    (set_local $count (i32.const 0))
                         '    (loop $l
                         '      (i32.eqz
                         '        (i32.load8_u (get_local $count))
                         '      )
                         '      (if
                         '        (then
                         '          (return (get_local $count))
                         '        )
                         '      )
                         '      (set_local $count
                         '        (i32.add
                         '          (get_local $count)
                         '          (i32.const 1)
                         '        )
                         '      )
                         '      (set_local $str
                         '        (i32.add
                         '          (get_local $str)
                         '          (i32.const 1)
                         '        )
                         '      )
                         '      (br $l)
                         '    )
                         '  )
                         '
                         '  (func $concat (param $str1 i32) (param $str2 i32) (result i32)
                         '    (local $dst i32)
                         '    (local $dstIt i32)
                         '
                         '    (tee_local $dst
                         '      (call $malloc
                         '        (i32.add
                         '          (i32.add
                         '            (call $strlen (get_local $str1))
                         '            (call $strlen (get_local $str2))
                         '          )
                         '          (i32.const 1)
                         '        )
                         '      )
                         '    )
                         '    (set_local $dstIt)
                         '    (loop $l
                         '      (i32.load8_u (get_local $str1))
                         '      (if
                         '        (then
                         '          (i32.store8
                         '            (get_local $dstIt)
                         '            (i32.load8_u (get_local $str1))
                         '          )
                         '          (set_local $str1
                         '            (i32.add
                         '              (get_local $str1)
                         '              (i32.const 1)
                         '            )
                         '          )
                         '          (set_local $dstIt
                         '            (i32.add
                         '              (get_local $dstIt)
                         '              (i32.const 1)
                         '            )
                         '          )
                         '          (br $l)
                         '        )
                         '      )
                         '    )
                         '    (loop $l
                         '      (i32.load8_u (get_local $str2))
                         '      (if
                         '        (then
                         '          (i32.store8
                         '            (get_local $dstIt)
                         '            (i32.load8_u (get_local $str2))
                         '          )
                         '          (set_local $str2
                         '            (i32.add
                         '              (get_local $str2)
                         '              (i32.const 1)
                         '            )
                         '          )
                         '          (set_local $dstIt
                         '            (i32.add
                         '              (get_local $dstIt)
                         '              (i32.const 1)
                         '            )
                         '          )
                         '          (br $l)
                         '        )
                         '      )
                         '    )
                         '    (i32.store8 (get_local $dstIt) (i32.const 0))
                         '    (get_local $dst)
                         '  )
                         ')";

public int FUNC_MALLOC_ID = 0;
public int FUNC_STRLEN_ID = 1;
public int FUNC_CONCAT_ID = 2;
public MODULE nativeFunctionsModule = toAST( desugar( parse( #start[WebAssembly], baseModule ) ) );
