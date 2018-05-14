module lang::webassembly::ADT

data VALTYPE = i32( ) | i64( ) | f32( ) | f64( );

data RESULTTYPE = resulttype( list[VALTYPE] );

data FUNCTYPE = functype( list[VALTYPE] input, list[VALTYPE] result );

data LIMITS = limits( int min, int max )
            | limits( int min )
            ;
            
data MEMTYPE = memtype( LIMITS l );

data ELEMTYPE = anyfunc( );

data TABLETYPE = tabletype( LIMITS l, ELEMTYPE e );

data GLOBALTYPE = globaltype( MUT mut, VALTYPE vt );

data MUT = const( )
         | var( )
         ;

data INSTR = i32_const( int ival )
           | i64_const( int ival )
           | f32_const( real fval )
           | f64_const( real fval )
           // iunop
           | i32_clz( )
           | i32_ctz( )
           | i32_popcnt( )
           | i64_clz( )
           | i64_ctz( )
           | i64_popcnt( )
           // funop
           | f32_abs( )
           | f32_neg( )
           | f32_sqrt( )
           | f32_ceil( )
           | f32_floor( )
           | f32_trunc( )
           | f32_nearest( )
           | f64_abs( )
           | f64_neg( )
           | f64_sqrt( )
           | f64_ceil( )
           | f64_floor( )
           | f64_trunc( )
           | f64_nearest( )
           // ibinop
           | i32_add( )
           | i32_sub( )
           | i32_mul( )
           | i32_div_u( )
           | i32_div_s( )
           | i32_rem_u( )
           | i32_rem_s( )
           | i32_and( )
           | i32_or( )
           | i32_xor( )
           | i32_shl( )
           | i32_shr_u( )
           | i32_shr_s( )
           | i32_rotl( )
           | i32_rotr( )
           | i64_add( )
           | i64_sub( )
           | i64_mul( )
           | i64_div_u( )
           | i64_div_s( )
           | i64_rem_u( )
           | i64_rem_s( )
           | i64_and( )
           | i64_or( )
           | i64_xor( )
           | i64_shl( )
           | i64_shr_u( )
           | i64_shr_s( )
           | i64_rotl( )
           | i64_rotr( )
           // fbinop
           | f32_add( )
           | f32_sub( )
           | f32_mul( )
           | f32_div( )
           | f32_min( )
           | f32_max( )
           | f32_copysign( )
           | f64_add( )
           | f64_sub( )
           | f64_mul( )
           | f64_div( )
           | f64_min( )
           | f64_max( )
           | f64_copysign( )
           // itestop
           | i32_eqz( )
           | i64_eqz( )
           // irelop
           | i32_eq( )
           | i32_ne( )
           | i32_lt_u( )
           | i32_lt_s( )
           | i32_gt_u( )
           | i32_gt_s( )
           | i32_le_u( )
           | i32_le_s( )
           | i32_ge_u( )
           | i32_ge_s( )
           | i64_eq( )
           | i64_ne( )
           | i64_lt_u( )
           | i64_lt_s( )
           | i64_gt_u( )
           | i64_gt_s( )
           | i64_le_u( )
           | i64_le_s( )
           | i64_ge_u( )
           | i64_ge_s( )
           // frelop
           | f32_eq( )
           | f32_ne( )
           | f32_lt( )
           | f32_gt( )
           | f32_le( )
           | f32_ge( )
           | f64_eq( )
           | f64_ne( )
           | f64_lt( )
           | f64_gt( )
           | f64_le( )
           | f64_ge( )
           // cvtop
           | i32_wrap_i64( )
           | i64_extend_u_i32( )
           | i64_extend_s_i32( )
           | i32_trunc_u_f32( )
           | i32_trunc_s_f32( )
           | i32_trunc_u_f64( )
           | i32_trunc_s_f64( )
           | i64_trunc_u_f32( )
           | i64_trunc_s_f32( )
           | i64_trunc_u_f64( )
           | i64_trunc_s_f64( )
           | f32_demote_f64( )
           | f64_promote_f32( )
           | f32_convert_u_i32( )
           | f32_convert_s_i32( )
           | f32_convert_u_i64( )
           | f32_convert_s_i64( )
           | f64_convert_u_i32( )
           | f64_convert_s_i32( )
           | f64_convert_u_i64( )
           | f64_convert_s_i64( )
           | i32_reinterpret_f32( )
           | i64_reinterpret_f64( )
           | f32_reinterpret_i32( )
           | f64_reinterpret_i64( )
           // parametric instructions
           | drop( )
           | select( )
           // variable instructions
           | get_local( int idx )
           | set_local( int idx )
           | tee_local( int idx )
           | get_global( int idx )
           | set_global( int idx )
           // memory instructions
           | i32_load( int offset, int align )
           | i64_load( int offset, int align )
           | f32_load( int offset, int align )
           | f64_load( int offset, int align )
           | i32_store( int offset, int align )
           | i64_store( int offset, int align )
           | f32_store( int offset, int align )
           | f64_store( int offset, int align )
           | i32_load8_u( int offset, int align )
           | i32_load8_s( int offset, int align )
           | i64_load8_u( int offset, int align )
           | i64_load8_s( int offset, int align )
           | i32_load16_u( int offset, int align )
           | i32_load16_s( int offset, int align )
           | i64_load16_u( int offset, int align )
           | i64_load16_s( int offset, int align )
           | i64_load32_u( int offset, int align )
           | i64_load32_s( int offset, int align )
           | i32_store8( int offset, int align )
           | i64_store8( int offset, int align )
           | i32_store16( int offset, int align )
           | i64_store16( int offset, int align )
           | i64_store32( int offset, int align )
           | memory_size( )
           | memory_grow( )
           // control instructions
           | nop( )
           | unreachable( )
           | block( RESULTTYPE restype, list[INSTR] instrs )
           | loop( RESULTTYPE restype, list[INSTR] instrs )
           | \if( RESULTTYPE, list[INSTR] ifInstrs, list[INSTR] elseInstrs )
           | br( LABELIDX li )
           | br_if( LABELIDX li )
           | br_table( list[LABELIDX] labels, LABELIDX \default )
           | \return( )
           | call( FUNCIDX fi )
           | call_indirect( TYPEIDX ti )
           ;

data EXPR = expr( list[INSTR] );

data MODULE = \module( list[FUNCTYPE] types,
                       list[FUNC] funcs,
                       list[TABLE] tables,
                       list[MEM] mems,
                       list[GLOBAL] globals,
                       list[ELEM] elems,
                       list[DATA] \data,
                       START \start,
                       list[IMPORT] imports,
                       list[EXPORT] exports )
            | \module( list[FUNCTYPE] types,
                       list[FUNC] funcs,
                       list[TABLE] tables,
                       list[MEM] mems,
                       list[GLOBAL] globals,
                       list[ELEM] elems,
                       list[DATA] \data,
                       list[IMPORT] imports,
                       list[EXPORT] exports )
            ;

alias TYPEIDX = int;
alias FUNCIDX = int;
alias TABLEIDX = int;
alias MEMIDX = int;
alias GLOBALIDX = int;
alias LOCALIDX = int;
alias LABELIDX = int;

alias byte = int;

alias NAME = str;

// The locals are the local variables only. Excludes the parameters
data FUNC = func( TYPEIDX, list[VALTYPE] locals, EXPR body );
data TABLE = table( TABLETYPE \type );
data MEM = mem( MEMTYPE \type );
data GLOBAL = global( GLOBALTYPE \type, EXPR init );
data ELEM = elem( TABLEIDX, EXPR offset, list[FUNCIDX] init );
data DATA = \data( MEMIDX, EXPR offset, list[byte] init );
data START = \start( FUNCIDX );
data IMPORT = \import( NAME \module, NAME name, IMPORTDESC desc );
data IMPORTDESC = importdesc_func( TYPEIDX i )
                | importdesc_table( TABLETYPE tt )
                | importdesc_mem( MEMTYPE mt )
                | importdesc_global( GLOBALTYPE gt )
                ;
data EXPORT = export( NAME name, EXPORTDESC desc );
data EXPORTDESC = exportdesc_func( FUNCIDX i )
                | exportdesc_table( TABLEIDX i )
                | exportdesc_mem( MEMIDX i )
                | exportdesc_global( GLOBALIDX i )
                ;
