module lang::webassembly::Desugar

import lang::webassembly::Syntax;
import ParseTree;
import IO; // temp

// Many of the desugaring clauses cannot be resolved with the visit structure
//   as in many cases a sub-ADT has to be replaced by a sub-ADT of a different
//   type. Hence, these trees have to be constructed manually.
//   (e.g. a Instr beeing replaced by Instrs, containing several Instr nodes)

start[WebAssembly] desugar( start[WebAssembly] w ) =
  visit ( w ) {
  //case Module m => m2 when <_,m2> := desugar( moduleDesc( [], occurringIds( w ) ), m )
  case Module m: {
    list[TypeDesc] initialTypes = getFuncTypes( m );
    <moduleDesc(finalTypes,_), m2> = desugar( moduleDesc( initialTypes, occurringIds( w ) ), m );
    list[TypeDesc] newTypes = finalTypes - initialTypes;
    m3 = appendTypes( m2, newTypes );
    insert m3;
  }
  };

// ## ValTypeDescs
// For these functions the Params/Results must already be desugared

list[ValType] getTypes( (Params)`(param <Id _> <ValType valType>) <Params params>` ) = valType + getTypes( params );
list[ValType] getTypes( (Params)`(param <ValType valType>) <Params params>` ) = valType + getTypes( params );
list[ValType] getTypes( (Params)`` ) = [];

list[ValType] getTypes( (Results)`(result <Id _> <ValType valType>) <Results results>` ) = valType + getTypes( results );
list[ValType] getTypes( (Results)`(result <ValType valType>) <Results results>` ) = valType + getTypes( results );
list[ValType] getTypes( (Results)`` ) = [];

// These datatypes are introduced to avoid functions having side-effects
// Instead, these are immutable "models" that may are brought into the
// recursion. 
data TypeDesc = typeDesc( list[ValType] params, list[ValType] results );
data ModuleDesc = moduleDesc( list[TypeDesc] types, set[str] ids );

ModuleFields concat( (ModuleFields)``, ModuleFields fields2 ) = fields2;
ModuleFields concat( (ModuleFields)`<ModuleField f> <ModuleFields fields>`, ModuleFields fields2 )
  = (ModuleFields)`<ModuleField f> <ModuleFields concatFields>`
  when concatFields := concat( fields, fields2 );
  
Params concat( (Params)``, Params fields2 ) = fields2;
Params concat( (Params)`<Param f> <Params fields>`, Params fields2 )
  = (Params)`<Param f> <Params concatFields>`
  when concatFields := concat( fields, fields2 );
  
Results concat( (Results)``, Results fields2 ) = fields2;
Results concat( (Results)`<Result f> <Results fields>`, Results fields2 )
  = (Results)`<Result f> <Results concatFields>`
  when concatFields := concat( fields, fields2 );
  
Locals concat( (Locals)``, Locals fields2 ) = fields2;
Locals concat( (Locals)`<Local f> <Locals fields>`, Locals fields2 )
  = (Locals)`<Local f> <Locals concatLocals>`
  when concatLocals := concat( fields, fields2 );
  
Instrs concat( (Instrs)``, Instrs fields2 ) = fields2;
Instrs concat( (Instrs)`<Instr f> <Instrs fields>`, Instrs fields2 )
  = (Instrs)`<Instr f> <Instrs concatLocals>`
  when concatLocals := concat( fields, fields2 );

// Param
Params desugar( p:(Param)`(param <Id id> <ValType \type>)` ) = (Params)`<Param p>`;
Params desugar( (Param)`(param)` ) = (Params)``;
Params desugar( (Param)`(param <ValType \type> <ValType* types>)` )
  = (Params)`(param <ValType \type>) <Params desParams>`
  when desParams := desugar( (Param)`(param <ValType* types>)` );
Params desugar( (Params)`<Param p> <Params ps>` )
  = (Params)`<Params concatPs>`
  when desP := desugar( p ),
       desPs := desugar( ps ),
       concatPs := concat( desP, desPs );
Params desugar( p:(Params)`` ) = p;

// Result (copy of Param)
Results desugar( p:(Result)`(result <Id id> <ValType \type>)` ) = (Results)`<Result p>`;
Results desugar( (Result)`(result)` ) = (Results)``;
Results desugar( (Result)`(result <ValType \type> <ValType* types>)` )
  = (Results)`(result <ValType \type>) <Results desResults>`
  when desResults := desugar( (Result)`(result <ValType* types>)` );
Results desugar( (Results)`<Result p> <Results ps>` )
  = (Results)`<Results concatPs>`
  when desP := desugar( p ),
       desPs := desugar( ps ),
       concatPs := concat( desP, desPs );
Results desugar( p:(Results)`` ) = p;

// Local (copy of Param)
Locals desugar( l:(Local)`(local <Id id> <ValType \type>)` ) = (Locals)`<Local l>`;
Locals desugar( (Local)`(local)` ) = (Locals)``;
Locals desugar( (Local)`(local <ValType \type> <ValType* types>)` )
  = (Locals)`(local <ValType \type>) <Locals desResults>`
  when desResults := desugar( (Local)`(local <ValType* types>)` );
Locals desugar( (Locals)`<Local p> <Locals ps>` )
  = (Locals)`<Locals concatPs>`
  when desP := desugar( p ),
       desPs := desugar( ps ),
       concatPs := concat( desP, desPs );
Locals desugar( p:(Locals)`` ) = p;

// Type Use
tuple[ModuleDesc,TypeUse] desugar( ModuleDesc desc, (TypeUse)`(type <TypeIdx idx>) <Params ps> <Results rs>` )
  = <desc, (TypeUse)`(type <TypeIdx idx>) <Params desPs> <Results desRs>`>
  when desPs := desugar( ps ),
       resPs := desugar( rs );

tuple[ModuleDesc,TypeUse] desugar( ModuleDesc desc, (TypeUse)`<Params ps> <Results rs>` )
  = <desc2, (TypeUse)`(type <UN idLex>) <Params desPs> <Results desRs>`>
  when desPs := desugar( ps ),
       desRs := desugar( rs ),
       <desc2, id> := findTypeIndex( desc, desPs, desRs ),
       idLex := parse( #UN, "<id>" );

// Functions (from Section 6.6.5)
//
// Note that for these functions the desugaring is not done on the FuncFields
//   themselves, as the Id must be known, and a single function module field can
//   be an abbreviation for several module fields
tuple[ModuleDesc,ModuleFields] desugar( ModuleDesc desc, (ModuleField)`(func <Id? id> <TypeUse typeUse> <Locals locals> <Instrs instrs>)` )
  = <desc2, (ModuleFields)`(func <Id? id> <TypeUse desTypeUse> <Locals desLocals> <Instrs desInstrs>)`>
  when <desc2,desTypeUse> := desugar( desc, typeUse ),
       desLocals := desugar( locals ),
       desInstrs := desugar( instrs );

tuple[ModuleDesc,ModuleFields] desugar( ModuleDesc desc, (ModuleField)`(func <Id? id> (import <Name modName> <Name funcName>) <TypeUse typeUse>)` )
  = <desc2, (ModuleFields)`(import <Name modName> <Name funcName> (func <Id? id> <TypeUse desTypeUse>))`>
  when <desc2,desTypeUse> := desugar( desc, typeUse );

tuple[ModuleDesc,ModuleFields] desugar( ModuleDesc desc, (ModuleField)`(func (export <Name name>) <FuncFields fields>)` )
  = <desc3, (ModuleFields)`(export <Name name> (func <FuncIdx id>)) <ModuleFields desFields>`>
  when <desc2,id> := getFreshId( desc ),
       <desc3,desFields> := desugar( desc2, (ModuleField)`(func <Id id> <FuncFields fields>)` );
  
tuple[ModuleDesc,ModuleFields] desugar( ModuleDesc desc, (ModuleField)`(func <Id id> (export <Name name>) <FuncFields fields>)` )
  = <desc2, (ModuleFields)`(export <Name name> (func <FuncIdx id>)) <ModuleFields desugaredFields>`>
  when <desc2, desugaredFields> := desugar( desc, (ModuleField)`(func <Id id> <FuncFields fields>)` );

// Instrs
Instrs desugar( (Instr)`<FoldedInstr instr>` ) = desugar( instr );
Instrs desugar( (Instr)`<PlainInstr plainInstr>` ) = desugar( plainInstr );

Instrs desugar( (Instrs)`<Instr instr> <Instrs instrs>` ) = concat( desugar( instr ), desugar( instrs ) );
Instrs desugar( i:(Instrs)`` ) = i;

Instrs desugar( (FoldedInstrs)`<FoldedInstr foldInstr> <FoldedInstrs foldInstrs>` ) = concat( desugar( foldInstr ), desugar( foldInstrs ) );
Instrs desugar( f:(FoldedInstrs)`` ) = (Instrs)``;

Instrs desugar( (FoldedInstr)`(<PlainInstr plainInstr> <FoldedInstrs foldInstrs>)` ) = concat( desugar( foldInstrs ), (Instrs)`<PlainInstr plainInstr>` );

default Instrs desugar( Instr instr ) = (Instrs)`<Instr instr>`;

// ModuleFields
default tuple[ModuleDesc,ModuleFields] desugar( ModuleDesc desc, ModuleField field ) = <desc, (ModuleFields)`<ModuleField field>`>;

tuple[ModuleDesc,ModuleFields] desugar( ModuleDesc desc, m:(ModuleFields)`` ) = <desc,m>;

tuple[ModuleDesc,ModuleFields] desugar( ModuleDesc desc, (ModuleFields)`<ModuleField field> <ModuleFields fields>` )
  = < desc3, concat( desField, desFields ) >
  when <desc2,desField> := desugar( desc, field ),
       <desc3,desFields> := desugar( desc2, fields );

tuple[ModuleDesc,Module] desugar( ModuleDesc desc, (Module)`(module <Id? id> <ModuleFields fields>)` )
  = <desc2,(Module)`(module <Id? id> <ModuleFields desFields>)`>
  when <desc2,desFields> := desugar( desc, fields );

// ## Add the new type fields to the end of the module
Params toSyntaxParams( [] ) = (Params)``;
Params toSyntaxParams( list[ValType] params )
  = (Params)`(param <ValType valType>) <Params sParams>`
  when sParams := toSyntaxParams( tail( params ) ),
       valType := head( params );
       
Results toSyntaxResults( [] ) = (Results)``;
Results toSyntaxResults( list[ValType] results )
  = (Results)`(result <ValType valType>) <Results sResults>`
  when sResults := toSyntaxResults( tail( results ) ),
       valType := head( results );
       
Type toSyntaxField( typeDesc( list[ValType] params, list[ValType] results ) ) = (Type)`(type (func <Params sParams> <Results sResults>))`
  when sParams := toSyntaxParams( params ),
       sResults := toSyntaxResults( results );
       
ModuleFields toSyntaxFields( [] ) = (ModuleFields)``;
ModuleFields toSyntaxFields( list[TypeDesc] types ) {
  sField = toSyntaxField( head( types ) );
  sFields = toSyntaxFields( tail( types ) );
  return (ModuleFields)`<Type sField> <ModuleFields sFields>`;
}

Module appendTypes( (Module)`(module <Id? id> <ModuleFields fields>)`, list[TypeDesc] types )
  = (Module)`(module <Id? id> <ModuleFields concatFields>)`
  when concatFields := concat( fields, toSyntaxFields( types ) );

// ## Utils
set[str] occurringIds( start[WebAssembly] w ) {
  set[str] ids = { };
  visit ( w ) {
  case Id i: {
    ids += "<i>";
  }
  }
  return ids;
}

// Obtains non-inlined function types (syntax "Type") from the module
list[TypeDesc] getFuncTypes( Module m ) {
  list[TypeDesc] types = [];
  visit ( m ) {
  case (Type)`(type <Id? id> (func <Params ps> <Results rs>))`: {
    types += typeDesc( getTypes( desugar( ps ) ), getTypes( desugar( rs ) ) );
  }
  }
  return types;
}

/**
 * A generator for fresh identifiers, satisfying the form "$t[id]", for any
 * such identifier that does not yet exist in the source text.
 *
 * From "6.3.5 Identifiers. Conventions":
 * The expansion rules of some abbreviations require insertion of a fresh
 * identifier. That may be any syntactically identifier that does not already
 * occur in the given source text.
 */
tuple[ModuleDesc,Id] getFreshId( moduleDesc( types, ids ) ) {
  // This is not very efficient, as it starts at '$t0' every time,
  // looping until a free one is found. Though, it has no side-effects,
  // which makes it cleaner.
  str id;
  int index = 0;
  do {
    id = "$t<index>";
    index = index + 1;
  } while ( id in ids );
  return <moduleDesc( types, ids + id ), parse(#Id, id)>;
}

/**
 * A 'typeuse' may alse be replaced entirely by inline parameter and result
 * declarations. In that case, a type index is automatically inserted, having
 * the smallest type index whose definition in the current module is the given
 * function type. If no such index exists, then a new type definition is
 * inserted at the end of the module.
 */
tuple[ModuleDesc,int] findTypeIndex( moduleDesc( types, ids ), Params params, Results results ) {
  newType = typeDesc( getTypes( params ), getTypes( results ) );
  int index = 0;
  for ( TypeDesc \type <- types ) {
    if ( \type == newType ) {
      return index;
    }
    index = index + 1;
  }
  return <moduleDesc( types + newType, ids ), index>;
}
