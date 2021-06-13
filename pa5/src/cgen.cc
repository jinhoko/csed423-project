//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully and add code to build an LLVM program 
//**************************************************************

#define EXTERN
#include "cgen.h"
#include <string>
#include <sstream>
#include <algorithm>

#define PA5

extern int cgen_debug;

void debug_( string str, int space ) {
		if (cgen_debug) {
			for( int i = 0; i < space; i++ ) {
				std::cerr << " ";
			}
			std::cerr << str << endl;
		}
	}

void debug( string str ) {
		debug_(str, 0);
	} 

ValuePrinter* vp;

bool attr_class::is_method( ) { return false; }
bool method_class::is_method( ) { return true; }
Symbol attr_class::get_name() { return name; }
Symbol method_class::get_name() { return name; }

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.  Feel free to add your
// own definitions as you see fit.
//
//////////////////////////////////////////////////////////////////////
EXTERN Symbol 
	// required classes
	Object,
	IO,
	String,
	Int,
	Bool,
	Main,

	// class methods
	cool_abort,
	type_name,
	cool_copy,
	out_string,
	out_int,
	in_string,
	in_int,
	length,
	concat,
	substr,

	// class members
	val,

	// special symbols
	No_class,    // symbol that can't be the name of any user-defined class
	No_type,     // If e : No_type, then no code is generated for e.
	SELF_TYPE,   // Special code is generated for new SELF_TYPE.
	self,        // self generates code differently than other references

	// extras
	arg,
	arg2,
	prim_string,
	prim_int,
	prim_bool;


////////// Helper functions

op_type get_symbol_op_type( Symbol t, int ptr_level, string class_name ) {					// returns value
	op_type type;
	if( t == prim_int ) { type = op_type(INT32); }
	else if( t == prim_bool ) { type = op_type(INT1); }
	else if( t == prim_string ) { type = op_type(INT8_PTR); }
	else if ( t == SELF_TYPE ) { type = op_type( class_name, ptr_level); }
	else type = op_type( string( t->get_string()), ptr_level );
	return type;
}

op_type* get_symbol_op_type_ptr( Symbol t, int ptr_level, string class_name ) {			// returns heap pointer
	op_type* type;
	if( t == prim_int ) { type = new op_type(INT32); }
	else if( t == prim_bool ) { type = new op_type(INT1); }
	else if( t == prim_string ) { type = new op_type(INT8_PTR); }
	else if ( t == SELF_TYPE ) { type = new op_type( class_name, ptr_level); }
	else type = new op_type( string( t->get_string()), ptr_level );
	return type;
}

op_type get_objsymbol_obj_op_type( Symbol t, int ptr_level, string class_name) {		// always return object
	op_type type;
	if ( t == SELF_TYPE ) { type = op_type( class_name, ptr_level); }
	else type = op_type( string( t->get_string()), ptr_level );
	return type;
}

op_type get_objsymbol_op_type( Symbol t, int ptr_level, string class_name) {			// returns value
	op_type type;
	if( t == Int ) { type = op_type(INT32); }
	else if( t == Bool ) { type = op_type(INT1); }
	else if ( t == SELF_TYPE ) { type = op_type( class_name, ptr_level); }
	else type = op_type( string( t->get_string()), ptr_level );
	return type;
}

op_type* get_objsymbol_op_type_ptr( Symbol t, int ptr_level, string class_name) {				// returns heap pointer
	op_type* type;
	if( t == Int ) { type = new op_type(INT32); }
	else if( t == Bool ) { type = new op_type(INT1); }
	else if ( t == SELF_TYPE ) { type = new op_type( class_name, ptr_level); }
	else type = new op_type( string( t->get_string()), ptr_level );
	return type;
}

op_type get_objsymbol_ptr_op_type( Symbol t , string class_name) {
	op_type type;
	if( t == Int ) { type = op_type(INT32_PTR); }
	else if( t == Bool ) {  type = op_type(INT1_PTR); }
	else if ( t == SELF_TYPE ) { type = op_type( class_name, 2); }
	else type = op_type( string( t->get_string()), 2 );
	return type;
}

operand get_default_value( op_type type ) {
	if( type.is_same_with( op_type(INT1) ) ) {				// Bool
		return const_value(op_type(INT1), "false", true);
	} else if( type.is_same_with( op_type(INT32) ) ){		// Int
		return const_value(op_type(INT32), "0", true);
	} else if( type.is_same_with( op_type( "String", 0 ) )
				|| type.is_same_with( op_type( "String", 1) )
		){	// String												// String
		return vp->call( vector<op_type>(), op_type("String", 1), "String_new", true, vector<operand>() );
	} else {
		return null_value( type );
	}
}

//********************************************************
//
// PREDEFINED FUNCTIONS:
//
// The following functions are already coded, you should
// not need to modify them, although you may if necessary.
//
//********************************************************

//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
	Object      = idtable.add_string("Object");
	IO          = idtable.add_string("IO");
	String      = idtable.add_string("String");
	Int         = idtable.add_string("Int");
	Bool        = idtable.add_string("Bool");
	Main        = idtable.add_string("Main");

	cool_abort  = idtable.add_string("abort");
	type_name   = idtable.add_string("type_name");
	cool_copy   = idtable.add_string("copy");
	out_string  = idtable.add_string("out_string");
	out_int     = idtable.add_string("out_int");
	in_string   = idtable.add_string("in_string");
	in_int      = idtable.add_string("in_int");
	length      = idtable.add_string("length");
	concat      = idtable.add_string("concat");
	substr      = idtable.add_string("substr");

	val         = idtable.add_string("val");

	No_class    = idtable.add_string("_no_class");
	No_type     = idtable.add_string("_no_type");
	SELF_TYPE   = idtable.add_string("SELF_TYPE");
	self        = idtable.add_string("self");

	arg         = idtable.add_string("arg");
	arg2        = idtable.add_string("arg2");
	prim_string = idtable.add_string("sbyte*");
	prim_int    = idtable.add_string("int");
	prim_bool   = idtable.add_string("bool");
}

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************
void program_class::cgen(ostream &os) 
{
	initialize_constants();
	class_table = new CgenClassTable(classes,os);
}


// Create definitions for all String constants
void StrTable::code_string_table(ostream& s, CgenClassTable* ct)
{
	for (List<StringEntry> *l = tbl; l; l = l->tl()) {
		l->hd()->code_def(s, ct);
	}
}

// Create definitions for all Int constants
void IntTable::code_string_table(ostream& s, CgenClassTable* ct)
{
	for (List<IntEntry> *l = tbl; l; l = l->tl()) {
		l->hd()->code_def(s, ct);
	}
}

//
// Sets up declarations for extra functions needed for code generation
// You should not need to modify this code for PA5
//
void CgenClassTable::setup_external_functions()
{
	ValuePrinter vp;
	// setup function: external int strcmp(sbyte*, sbyte*)
	op_type i32_type(INT32), i8ptr_type(INT8_PTR), vararg_type(VAR_ARG);
	vector<op_type> strcmp_args;
	strcmp_args.push_back(i8ptr_type);
	strcmp_args.push_back(i8ptr_type);	
	vp.declare(*ct_stream, i32_type, "strcmp", strcmp_args); 

	// setup function: external int printf(sbyte*, ...)
	vector<op_type> printf_args;
	printf_args.push_back(i8ptr_type);
	printf_args.push_back(vararg_type);
	vp.declare(*ct_stream, i32_type, "printf", printf_args);

	// setup function: external void abort(void)
	op_type void_type(VOID);
	vector<op_type> abort_args;
	vp.declare(*ct_stream, void_type, "abort", abort_args);

	// setup function: external i8* malloc(i32)
	vector<op_type> malloc_args;
	malloc_args.push_back(i32_type);
	vp.declare(*ct_stream, i8ptr_type, "malloc", malloc_args);

#ifdef PA5

    //Setup external functions for built in object class functions
    op_type objectp_type("Object", 1),
            intp_type("Int", 1),
            boolp_type("Bool", 1),
            stringp_type("String", 1),
            iop_type("IO", 1),
            i1_type(INT1)
            ;

    vector<op_type> objectnew_args;
	vp.declare(*ct_stream, objectp_type, "Object_new", objectnew_args);

    vector<op_type> objectabort_args;
    objectabort_args.push_back(objectp_type);
	vp.declare(*ct_stream, objectp_type, "Object_abort", objectabort_args);

    vector<op_type> objecttypename_args;
    objecttypename_args.push_back(objectp_type);
	vp.declare(*ct_stream, stringp_type, "Object_type_name", objecttypename_args);

    vector<op_type> objectcopy_args;
    objectcopy_args.push_back(objectp_type);
	vp.declare(*ct_stream, objectp_type, "Object_copy", objectcopy_args);

    vector<op_type> ionew_args;
	vp.declare(*ct_stream, iop_type, "IO_new", ionew_args);

    vector<op_type> iooutstring_args;
    iooutstring_args.push_back(iop_type);
    iooutstring_args.push_back(stringp_type);
	vp.declare(*ct_stream, iop_type, "IO_out_string", iooutstring_args);

    vector<op_type> iooutint_args;
    iooutint_args.push_back( iop_type );
    iooutint_args.push_back( i32_type );
	vp.declare(*ct_stream, iop_type, "IO_out_int", iooutint_args);

    vector<op_type> ioinstring_args;
    ioinstring_args.push_back(iop_type);
	vp.declare(*ct_stream, stringp_type, "IO_in_string", ioinstring_args);

    vector<op_type> ioinint_args;
    ioinint_args.push_back(iop_type);
	vp.declare(*ct_stream, i32_type, "IO_in_int", ioinint_args);

    vector<op_type> stringnew_args;
	vp.declare(*ct_stream, stringp_type, "String_new", stringnew_args);

    vector<op_type> stringlength_args;
    stringlength_args.push_back(stringp_type);
	vp.declare(*ct_stream, i32_type, "String_length", stringlength_args);

    vector<op_type> stringconcat_args;
    stringconcat_args.push_back(stringp_type);
    stringconcat_args.push_back(stringp_type);
	vp.declare(*ct_stream, stringp_type, "String_concat", stringconcat_args);

    vector<op_type> stringsubstr_args;
    stringsubstr_args.push_back( stringp_type );
    stringsubstr_args.push_back( i32_type );
    stringsubstr_args.push_back( i32_type );
	vp.declare(*ct_stream, stringp_type, "String_substr", stringsubstr_args);

    vector<op_type> intnew_args;
	vp.declare(*ct_stream, intp_type, "Int_new", intnew_args);

    vector<op_type> intinit_args;
    intinit_args.push_back(intp_type);
    intinit_args.push_back(i32_type);
	vp.declare(*ct_stream, void_type, "Int_init", intinit_args);

    vector<op_type> boolnew_args;
	vp.declare(*ct_stream, boolp_type, "Bool_new", boolnew_args);

    vector<op_type> boolinit_args;
    boolinit_args.push_back(boolp_type);
    boolinit_args.push_back(i1_type);
	vp.declare(*ct_stream, void_type, "Bool_init", boolinit_args);

#endif
}

// Creates AST nodes for the basic classes and installs them in the class list
void CgenClassTable::install_basic_classes()
{
	// The tree package uses these globals to annotate the classes built below.
	curr_lineno = 0;
	Symbol filename = stringtable.add_string("<basic class>");

	//
	// A few special class names are installed in the lookup table but not
	// the class list. Thus, these classes exist, but are not part of the
	// inheritance hierarchy.
	 
	// No_class serves as the parent of Object and the other special classes.
	Class_ noclasscls = class_(No_class,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(noclasscls, CgenNode::Basic, this));
	delete noclasscls;

#ifdef PA5
	// SELF_TYPE is the self class; it cannot be redefined or inherited.
	Class_ selftypecls = class_(SELF_TYPE,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(selftypecls, CgenNode::Basic, this));
	delete selftypecls;
	// 
	// Primitive types masquerading as classes. This is done so we can
	// get the necessary Symbols for the innards of String, Int, and Bool
	//
	Class_ primstringcls = class_(prim_string,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primstringcls, CgenNode::Basic, this));
	delete primstringcls;
#endif
	Class_ primintcls = class_(prim_int,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primintcls, CgenNode::Basic, this));
	delete primintcls;
	Class_ primboolcls = class_(prim_bool,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primboolcls, CgenNode::Basic, this));
	delete primboolcls;
	// 
	// The Object class has no parent class. Its methods are
	//        cool_abort() : Object   aborts the program
	//        type_name() : Str       returns a string representation of class name
	//        copy() : SELF_TYPE      returns a copy of the object
	//
	// There is no need for method bodies in the basic classes---these
	// are already built in to the runtime system.
	//
	Class_ objcls =
		class_(Object, 
		       No_class,
		       append_Features(
		       append_Features(
		       single_Features(method(cool_abort, nil_Formals(), 
		                              Object, no_expr())),
		       single_Features(method(type_name, nil_Formals(),
		                              String, no_expr()))),
		       single_Features(method(cool_copy, nil_Formals(), 
		                              SELF_TYPE, no_expr()))),
		       filename);
	install_class(new CgenNode(objcls, CgenNode::Basic, this));
	delete objcls;

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
	Class_ intcls=
		class_(Int, 
		       Object,
		       single_Features(attr(val, prim_int, no_expr())),
		       filename);
	install_class(new CgenNode(intcls, CgenNode::Basic, this));
	delete intcls;

//
// Bool also has only the "val" slot.
//
	Class_ boolcls=
		class_(Bool,  
		       Object, 
		       single_Features(attr(val, prim_bool, no_expr())),
		       filename);
	install_class(new CgenNode(boolcls, CgenNode::Basic, this));
	delete boolcls;

#ifdef PA5
//
// The class String has a number of slots and operations:
//       val                                  the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
	Class_ stringcls =
		class_(String, 
		       Object,
		       append_Features(
		       append_Features(
		       append_Features(
		       single_Features(attr(val, prim_string, no_expr())),
		       single_Features(method(length, nil_Formals(),
		                              Int, no_expr()))),
		       single_Features(method(concat,
		                              single_Formals(formal(arg, String)),
		                              String,
		                              no_expr()))),
		       single_Features(method(substr, 
		                              append_Formals(
		                                 single_Formals(formal(arg, Int)), 
		                                 single_Formals(formal(arg2, Int))),
		                              String, 
		                              no_expr()))),
		       filename);
	install_class(new CgenNode(stringcls, CgenNode::Basic, this));
	delete stringcls;
#endif

#ifdef PA5
// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
	Class_ iocls =
		class_(IO,
		       Object,
		       append_Features(
		       append_Features(
		       append_Features(
		       single_Features(method(out_string,
		                              single_Formals(formal(arg, String)),
		                              SELF_TYPE, no_expr())),
		       single_Features(method(out_int, single_Formals(formal(arg, Int)),
		                              SELF_TYPE, no_expr()))),
		       single_Features(method(in_string, nil_Formals(), String,
		                              no_expr()))),
		       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
		       filename);
	install_class(new CgenNode(iocls, CgenNode::Basic, this));
	delete iocls;
#endif
}

//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_classes(Classes cs)
{
	for (int i = cs->first(); cs->more(i); i = cs->next(i)) {
		install_class(new CgenNode(cs->nth(i),CgenNode::NotBasic,this));
	}
}

// 
// Add this CgenNode to the class list and the lookup table
// 
void CgenClassTable::install_class(CgenNode *nd)
{
	Symbol name = nd->get_name();

	if (probe(name))
		return;

	// The class name is legal, so add it to the list of classes
	// and the symbol table.
	nds = new List<CgenNode>(nd,nds);
	addid(name,nd);
}

// 
// Add this CgenNode to the special class list and the lookup table
// 
void CgenClassTable::install_special_class(CgenNode *nd)
{
	Symbol name = nd->get_name();

	if (probe(name))
		return;

	// The class name is legal, so add it to the list of special classes
	// and the symbol table.
	special_nds = new List<CgenNode>(nd, special_nds);
	addid(name,nd);
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
	for(List<CgenNode> *l = nds; l; l = l->tl())
		set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNode *nd)
{
	CgenNode *parent_node = probe(nd->get_parent());
	nd->set_parentnd(parent_node);
	parent_node->add_child(nd);
}

// Get the root of the class tree.
CgenNode *CgenClassTable::root()
{
	return probe(Object);
}

//////////////////////////////////////////////////////////////////////
//
// Special-case functions used for the method Int Main::main() for
// PA5 only.
//
//////////////////////////////////////////////////////////////////////

#ifndef PA5

assert(0 && "All PA4 implementations are deleted");

#endif

//-------------------------------------------------------------------
//
// END OF PREDEFINED FUNCTIONS
//
//-------------------------------------------------------------------


///////////////////////////////////////////////////////////////////////////////
//
// coding string, int, and boolean constants
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type stringEntry.  stringEntry methods are defined both for string
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Create global definitions for constant Cool objects
//
void CgenClassTable::code_constants()
{
#ifdef PA5

	int size, idx, stridx;
	for( size = stringtable.first() ; stringtable.more(size) ; size = stringtable.next(size) ) { }
	
	for( idx = size-1 ; idx >= 0 ; idx-=1 ) {	// reverse
		
		// str value
		string obj_str = string(stringtable.lookup(idx)->get_string()) ;
		const_value str_val ( op_arr_type(INT8, obj_str.size()+1), obj_str, true );
		string str_name = "str."+itos(idx);
		vp->init_constant( str_name, str_val );

		// globally define String
		// contents of "StringEntry::code_def"
		vector<op_type> types;
		vector<const_value> values;
		types.push_back( op_type("String_vtable", 1) );
		types.push_back( op_type(INT8_PTR) );
		values.push_back( const_value( op_type("String_vtable", 1), "@String_vtable_prototype", false) );
		values.push_back( const_value ( op_arr_type(INT8, obj_str.size()+1), "@"+str_name, false ) );
		
		vp->init_struct_constant( global_value( op_type("String"), "String."+itos(idx) ), types, values );

	}
#endif
}

// generate code to define a global string constant
void StringEntry::code_def(ostream& s, CgenClassTable* ct)
{
#ifdef PA5
	// Not using.
#endif
}

// generate code to define a global int constant
void IntEntry::code_def(ostream& s, CgenClassTable* ct)
{
	// Not using.
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//
// CgenClassTable constructor orchestrates all code generation
//
CgenClassTable::CgenClassTable(Classes classes, ostream& s) 
: nds(0)
{
	debug("building classtable");
	ct_stream = &s;
    vp = new ValuePrinter(*ct_stream);
	// Make sure we have a scope, both for classes and for constants
	enterscope();

	// Create an inheritance tree with one CgenNode per class.
	debug("install_basic_classes()");
	install_basic_classes();
	debug("install_classes(classes)");
	install_classes(classes);
	build_inheritance_tree();

	// First pass
	debug("setup()");
	setup();

	// Second pass
	debug("code_module()");
	code_module();
	// Done with code generation: exit scopes
	exitscope();

}

CgenClassTable::~CgenClassTable()
{
}

// The code generation first pass.  Define these two functions to traverse
// the tree and setup each CgenNode
void CgenClassTable::setup()
{
	setup_external_functions();
	setup_classes(root(), 0);
}


void CgenClassTable::setup_classes(CgenNode *c, int depth)
{
	// MAY ADD CODE HERE
	// if you want to give classes more setup information

	c->setup(current_tag++, depth);
	List<CgenNode> *children = c->get_children();
	for (List<CgenNode> *child = children; child; child = child->tl())
		setup_classes(child->hd(), depth + 1);
	
	c->set_max_child(current_tag-1);

	/*
	if (cgen_debug)
		std::cerr << "Class " << c->get_name() << " assigned tag " 
			<< c->get_tag() << ", max child " << c->get_max_child() 
			<< ", depth " << c->get_depth() << endl;
	*/
}


// The code generation second pass. Add code here to traverse the tree and
// emit code for each CgenNode
void CgenClassTable::code_module()
{
	code_constants();

#ifndef PA5
	assert(0 && "All PA4 implementations are deleted");
#endif
	debug_( "code_main", 2);
	code_main();

#ifdef PA5
	code_classes(root());
#else
#endif
}


#ifdef PA5
void CgenClassTable::code_classes(CgenNode *c)
{
	c->code_class();
	List<CgenNode> *children = c->get_children();
	for ( children ; children != NULL ; children = children->tl() ){
		code_classes( children->hd() );
	}
}
#endif


//
// Create LLVM entry point. This function will initiate our Cool program 
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main()
{
	ValuePrinter vp(*ct_stream);

	// NOT USED IN PA5
	// Make string before define
	// string _str = "Main_main() returned %d\n";
	// const_value _str_val ( op_arr_type(INT8, 25), _str, false );
	// vp.init_constant(".str", _str_val);

	// Define a function main that has no parameters and returns an i32
	vp.define( op_type(INT32), "main", vector<operand>() );
	// Define an entry basic block
	vp.begin_block( "entry" );

	// Call Main_new() to generate Main
	operand result_Main_obj( op_type("Main", 1), "main.obj" );
	vp.call(*ct_stream, vector<op_type>(), "Main_new", true, vector<operand>(), result_Main_obj );

	// Call Main_main(). This returns int* for phase 1, Object for phase 2
	CgenNode* main_class = lookup(Main);
	op_type main_type = main_class->find_main_type();

	operand result_Main_main( main_type, "main.retval" );
	
	vector<op_type> result_Main_main_args_type;
	vector<operand> result_Main_main_args;
	result_Main_main_args_type.push_back( op_type("Main", 1) );
	result_Main_main_args.push_back( result_Main_obj );
	vp.call(*ct_stream, result_Main_main_args_type, "Main_main", true, result_Main_main_args, result_Main_main );

#ifndef PA5
	assert(0 && "All PA4 implementations are deleted");
#else
    
#endif
	// End define
	vp.ret( int_value(0) );
	vp.end_define();
}

Symbol CgenClassTable::get_lub( Symbol s1, Symbol s2, Symbol self ) {

	debug_("get_lub", 6);
	if( s1 == SELF_TYPE ) s1 = self;
	if( s2 == SELF_TYPE ) s2 = self;

	CgenNode* node1 = lookup(s1);
	CgenNode* node2 = lookup(s2);
	assert( node1 != NULL & node2 != NULL );
	
	CgenNode* p1 = node1;
	CgenNode* p2 = node2;
	debug_("> p1 : " + string(p1->get_name()->get_string()) + " ,  " + "p2 : " + string(p1->get_name()->get_string()) , 6);

	while( true ) {
		while( p2->get_parentnd() != NULL ) {
			if( p1 == p2 ) return p1->get_name() ;		// comparing pointer
			p2 = p2->get_parentnd();
		}
		p1 = p1->get_parentnd();
		p2 = node2;
		if(p1->get_name() == Object && p2->get_name() == Object ) break;
	}

	// if not found, lub is object
	return Object;
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTable *ct)
: class__class((const class__class &) *nd), 
  parentnd(0), children(0), basic_status(bstatus), class_table(ct), tag(-1)
{ 
	// ADDED

	overridemethodtable = new cool::SymbolTable<Symbol, CgenNode>();
	overridemethodtable->enterscope();
	methodtable_idx = new cool::SymbolTable<Symbol,int>();
	methodtable_idx->enterscope();
	methodtable_return_type = new cool::SymbolTable<Symbol, op_type>();
	methodtable_return_type->enterscope();
	methodtable_arg_types = new cool::SymbolTable<Symbol, vector<op_type> >();
	methodtable_arg_types->enterscope();
	methodtable_idx_cnt = 0;


	attrtable_idx = new cool::SymbolTable<Symbol, int>();
	attrtable_idx->enterscope();
	attrtable_type = new cool::SymbolTable<Symbol, op_type>();
	attrtable_type->enterscope();
	attrtable_idx_cnt = 0;
}

void CgenNode::add_child(CgenNode *n)
{
	children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNode *p)
{
	assert(parentnd == NULL);
	assert(p != NULL);
	parentnd = p;
}

//
// Class setup.  You may need to add parameters to this function so that
// the classtable can provide setup information (such as the class tag
// that should be used by this class).  
// 
// Things that setup should do:
//  - layout the features of the class
//  - create the types for the class and its vtable
//  - create global definitions used by the class such as the class vtable
//
void CgenNode::setup(int tag, int depth)
{
	this->tag = tag;

#ifdef PA5

	string class_name = string(name->get_string());
	string vtable_name = class_name + "_vtable";
	string vtable_prototype_name = vtable_name + "_prototype";

	string class_name_obj = "str."+class_name;
	int class_name_obj_len = class_name.size()+1;
	const_value class_name_str ( op_arr_type(INT8, class_name_obj_len), class_name, true );
	vp->init_constant( class_name_obj, class_name_str);
	
	const_value class_name_str_op (
		op_arr_type(INT8, class_name_obj_len), "@"+class_name_obj, true
	);

    // define class type
	vector<op_type> class_types;
	class_types.push_back( op_type( vtable_name, 1) );
	attrtable_idx_cnt = 1;										// will start from 1
	layout_attributes( this, name, &class_types );
    vp->type_define( name->get_string(), class_types );

    // define vtable type, values
	vector<op_type> vtable_types;
	vector<const_value> vtable_values;

	// fetch objsize ; manually write code
	string objsize_str = "ptrtoint (%" + class_name + "* getelementptr (%" + class_name + ", %" + class_name + "* null, i32 1) to i32)";
	const_value objsize_const = const_value( op_type(INT32), objsize_str, true ) ;

	// vtable layout
	op_type new_func_type = op_func_type( op_type(class_name, 1), vector<op_type>() );
	vtable_types.push_back( op_type(INT32) );					// 0 - tag
	vtable_types.push_back( op_type(INT32) );					// 1 - objsize
	vtable_types.push_back( op_type(INT8_PTR) );				// 2 - name
	vtable_types.push_back( new_func_type );					// 3 - new
	vtable_values.push_back( int_value(tag) );					// 0 - tag
	vtable_values.push_back( objsize_const );					// 1 - objsize
	vtable_values.push_back( class_name_str_op );				// 2 - name
	vtable_values.push_back(									// 3 - new
		const_value(new_func_type, "@"+class_name+"_new", true ) ); 	

	methodtable_idx_cnt = 4; 									// will start from 4 
	layout_methods( this, name, &vtable_types, &vtable_values );		// 4~
	
    // vtable type/object definition
	assert( vtable_types.size() == vtable_values.size() );
	vp->type_define( vtable_name, vtable_types );
    vp->init_struct_constant(
        global_value( op_type(vtable_name), vtable_prototype_name ),
        vtable_types,
        vtable_values
    );
	
    
#endif
}

#ifdef PA5
// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features() { } // Not using.
void CgenNode::layout_attributes( CgenNode* node, Symbol cl, vector<op_type>* types ) {
	// recurse
	if( parentnd != NULL ) { parentnd->layout_attributes( node, cl, types ); }
	Feature f; int idx;

	for(idx = features->first() ; features->more(idx); idx = features->next(idx)) {
		f = features->nth(idx);

		f->add_attribute( node, cl, string(name->get_string()), types, basic());
	}
}
void attr_class::add_attribute( CgenNode* node, Symbol cl, string cname, vector<op_type>* types, bool basic ) {	
	if( basic ) {
		types->push_back( get_symbol_op_type( type_decl, 1, string(node->get_name()->get_string())) );
		node->attrtable_type->addid( name, get_symbol_op_type_ptr(type_decl,1,string(node->get_name()->get_string()) ) );
	} else {
		types->push_back( get_objsymbol_op_type( type_decl, 1, string(node->get_name()->get_string())) );
		node->attrtable_type->addid( name, get_objsymbol_op_type_ptr(type_decl, 1,string(node->get_name()->get_string())) );
	}
	node->attrtable_idx->addid( name, new int(node->attrtable_idx_cnt) );
	node->attrtable_idx_cnt+=1;
}
void method_class::add_attribute( CgenNode* node, Symbol cl, string cname,  vector<op_type>* types, bool basic ) { return; }

void CgenNode::layout_methods( CgenNode* node, Symbol cl, vector<op_type>* types, vector<const_value>* values ) {
	// recurse
	Feature f; int idx;
	for(idx = features->first() ; features->more(idx); idx = features->next(idx)) {
		f = features->nth(idx);
		// This is bottom-up symboltable, so that parent class can choose which function to actually locate
		// if method name is already defined in child class
		if( f->is_method() && (node->overridemethodtable->lookup( f->get_name() ) == NULL) ){
			node->overridemethodtable->addid( f->get_name(), this );
		}
	}

	if( parentnd != NULL ) { parentnd->layout_methods( node, cl, types, values ); }

	for(idx = features->first() ; features->more(idx); idx = features->next(idx)) {
		f = features->nth(idx);

		// If child's overridemethod definition already exists, select child's function prototype
		CgenNode* impl_class = node->overridemethodtable->lookup( f->get_name() );
		if( (this == node ) || (impl_class == NULL) ) {
			f->add_method( node, cl, string( name->get_string()), types, values);
		} else {
			f->add_method( node, cl, string( impl_class->get_name()->get_string()), types, values);
		}
	}
}
void method_class::add_method( CgenNode* node, Symbol cl, string cname, vector<op_type>* types, vector<const_value>* values ) {
	
	string target_class_name = cl->get_string();
	bool isInheritedMethod = target_class_name.compare( cname ) != 0;
	
	// result type
	op_type result_type = get_objsymbol_op_type( return_type, 1, target_class_name ) ;
	op_type orig_result_type = get_objsymbol_op_type( return_type, 1, cname ) ;
	op_type* result_type_table = get_objsymbol_op_type_ptr( return_type, 1, target_class_name);

	// args
	vector<op_type> args;
	vector<op_type> orig_args;
	vector<op_type>* args_table = new vector<op_type>();
	args.push_back( op_type(target_class_name, 1) );	// self
	orig_args.push_back( op_type(cname, 1) );
	args_table->push_back( op_type(target_class_name, 1) );
	Formal f; int idx;
	for( idx = formals->first() ; formals->more(idx) ; idx = formals->next(idx) ) {
		f = formals->nth(idx);
		args.push_back( get_objsymbol_op_type( f->get_type_decl() , 1, target_class_name ) );
		orig_args.push_back( get_objsymbol_op_type( f->get_type_decl() , 1, target_class_name ) );
		args_table->push_back( get_objsymbol_op_type( f->get_type_decl() , 1, target_class_name ) );
	}
	op_type t = op_func_type( result_type , args );
	op_type orig_t = op_func_type( orig_result_type, orig_args );

	// value
	const_value v =	const_value(t, "@"+cname+"_"+name->get_string(), true );
	if( isInheritedMethod ) {
		v = casted_value(t, "@"+cname+"_"+name->get_string(), orig_t);
	}
	types->push_back(t);
	values->push_back(v);

	// add to method table
	node->methodtable_idx->addid(name, new int(node->methodtable_idx_cnt) );
	node->methodtable_arg_types->addid( name, args_table );
	node->methodtable_return_type->addid( name, result_type_table );
	node->methodtable_idx_cnt +=1 ;

	
}
void attr_class::add_method( CgenNode* node, Symbol cl, string cname, vector<op_type>* types, vector<const_value>* values ){ return; }

//
// Class codegen. This should performed after every class has been setup.
// Generate code for each method of the class.
//
void CgenNode::code_class()
{
	// No code generation for basic classes. The runtime will handle that.
	if (basic()) return;

	debug_( "code_class "+ string(name->get_string()) , 4);
	
	string class_name = string( name->get_string() );
	// methods
	Feature f; int idx;
	CgenEnvironment* method_env;
	for( idx = features->first() ; features->more(idx); idx = features->next(idx) ) {
		f = features->nth(idx);
		method_env = new CgenEnvironment( *(class_table->ct_stream), this );
		if( f->is_method() ) { f->code(method_env); }
	}

	// Class_new ; features

	// ENV structure
	// - self
	//   - expr
	CgenEnvironment* instance_env = new CgenEnvironment( *(class_table->ct_stream), this );
	
	// define class_new
	op_type new_rettype( class_name, 1 );
	string new_name_str = class_name+"_new";
	vp->define(new_rettype, new_name_str, vector<operand>() );
	// block entry
	vp->begin_block("entry");
	// get vtable
	operand vtbl_ptr =
		vp->getelementptr(
			op_type(class_name+"_vtable", 0),
			global_value( op_type(class_name+"_vtable", 1), class_name+"_vtable_prototype"),
			int_value(0),
			int_value(1),
			op_type(INT32_PTR)  );
	operand vtbl_ptr_val = vp->load( op_type(INT32), vtbl_ptr );
	// call malloc
	vector<op_type> malloc_arg_types;
	vector<operand> malloc_args;
	malloc_arg_types.push_back( op_type(INT32) );
	malloc_args.push_back( vtbl_ptr_val );
	operand malloc_ptr = vp->call( malloc_arg_types, op_type(INT8_PTR), "malloc", true, malloc_args );
	// bitcast
	operand bitcast_ptr = vp->bitcast( malloc_ptr, op_type(class_name, 1) );
	// store vtable
	operand main_ptr = vp->getelementptr(
		op_type(class_name, 0),
			bitcast_ptr,
			int_value(0),
			int_value(0),
			op_type(class_name+"_vtable", 2)  );
	vp->store(
		global_value( op_type(class_name+"_vtable", 1), class_name+"_vtable_prototype"  ),
		main_ptr
	);
	operand main_ptr_addr = vp->alloca_mem( op_type(class_name, 1) );
	vp->store( bitcast_ptr, main_ptr_addr );
	instance_env->add_local( self, *(new operand(main_ptr_addr)) );

	// code
	int feature_idx = 0;
	code_class_new( &feature_idx, class_name, bitcast_ptr, instance_env);

	vp->ret( bitcast_ptr );
	// block abort
	vp->begin_block("abort");
	vp->call( vector<op_type>(), op_type(VOID), "abort", true, vector<operand>() );
	vp->unreachable();
	// end define
	vp->end_define();
}

void CgenNode::code_class_new( int* feature_idx, string class_name, operand bitcast_ptr, CgenEnvironment* instance_env) {

	// recursively call parent's 
	if( parentnd != NULL ) {
		parentnd->code_class_new( feature_idx, class_name, bitcast_ptr, instance_env);
	}

	operand attr_ptr; 
	Feature f; int idx;
	for( idx = features->first() ; features->more(idx); idx = features->next(idx) ) {
		f = features->nth(idx);
		if( !( f->is_method() ) ) {
			(*feature_idx)+=1;
			attr_ptr = vp->getelementptr(
				op_type( class_name	, 0),
				bitcast_ptr,
				int_value(0),
				int_value(*feature_idx),
				get_objsymbol_ptr_op_type( f->get_type(), class_name )
			);
			operand expr_op = f->get_expr()->code(instance_env);
			bool isExprEmpty = expr_op.get_type().is_same_with( op_type(EMPTY) );

			operand expr_op_conform;
			op_type target_op_type = get_objsymbol_op_type( f->get_type(), 1, class_name);

			if( isExprEmpty ) {	// if empty, store with default value
				expr_op_conform = get_default_value( target_op_type );
			} else {
				expr_op_conform = conform( expr_op, target_op_type, instance_env );
			}
			vp->store( expr_op_conform, attr_ptr);
		}
	}
}

op_type CgenNode::find_main_type() {
	assert( name == Main ); // only valid for main call
	Feature f; int idx;
	for( idx = features->first() ; features->more(idx); idx = features->next(idx) ) {
		f = features->nth(idx);

		char* c = "main";
		if( f->is_method() && f->get_name()->equal_string( c, 4 ) ) {
			Symbol main_type_sym = f->get_type();
			return get_objsymbol_op_type( main_type_sym, 1, string(name->get_string()) );
		}
	}
	assert( 0 && "Main_main should exist");
}


#else

assert(0 && "All PA4 implementations are deleted");

#endif



//
// CgenEnvironment functions
//

//
// Class CgenEnvironment should be constructed by a class prior to code
// generation for each method.  You may need to add parameters to this
// constructor.
//
CgenEnvironment::CgenEnvironment(std::ostream &o, CgenNode *c)
{
	cur_class = c;
	cur_stream = &o;
	var_table.enterscope();
	tmp_count = block_count = ok_count = 0;

}

// Look up a CgenNode given a symbol
CgenNode *CgenEnvironment::type_to_class(Symbol t) {
	return t == SELF_TYPE ? get_class() 
		: get_class()->get_classtable()->lookup(t);
}

// Provided CgenEnvironment methods
// Generate unique string names
std::string CgenEnvironment::new_name() {
	std::stringstream s;
	s << tmp_count++;
	return "tmp." + s.str();
}

std::string CgenEnvironment::new_ok_label() {
	std::stringstream s;
	s << ok_count++;
	return "ok." + s.str();
}
const std::string CgenEnvironment::new_label(const std::string& prefix,
		bool increment) {
	std::string suffix = itos(block_count);
	block_count += increment;
	return prefix + suffix;
}

void CgenEnvironment::enterscope() {
	var_table.enterscope();
}

void CgenEnvironment::add_local(Symbol name, operand &vb) {
	var_table.enterscope();			// Always enterscope
	var_table.addid(name, &vb);
}

void CgenEnvironment::kill_local() {
	var_table.exitscope();
}

/*
* LOGIC CHANGED
*  - if not found from lookup table,
*  - and then find from self.attributes
*/
operand *CgenEnvironment::lookup(Symbol name)	{

	debug_("vartable lookup", 6);
	operand* lookup_result = var_table.lookup(name);
	if( lookup_result == NULL ) {		
		operand self_op = *(var_table.lookup(self));
		operand self_load = vp->load( self_op.get_type().get_deref_type(), self_op );
		op_type result_type = (*(get_class()->attrtable_type->lookup(name)));
		debug_("result type : "+result_type.get_name(), 6);

		int feature_idx = *(get_class()->attrtable_idx->lookup(name));
		operand result = vp->getelementptr(
			self_op.get_type().get_deref_type().get_deref_type(),
			self_load,
			int_value(0),
			int_value(feature_idx),
			result_type.get_ptr_type()
		);
		lookup_result = new operand(result); // make new operand in heap
	}
	return lookup_result;
	
}


////////////////////////////////////////////////////////////////////////////
//
// APS class methods
//
////////////////////////////////////////////////////////////////////////////

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.handcode.h'.
//
//*****************************************************************

#ifdef PA5
// conform and get_class_tag are only needed for PA5

// conform - If necessary, emit a bitcast or boxing/unboxing operations
// to convert an object to a new type. This can assume the object
// is known to be (dynamically) compatible with the target type.
// It should only be called when this condition holds.
// (It's needed by the supplied code for typecase)
operand conform(operand src, op_type type, CgenEnvironment *env) {
	
	ValuePrinter vp(*(env->cur_stream));
	
	operand result;
	operand src_box; operand src_ptr;
	debug_( "conform : "+src.get_type().get_name() + " -> " + type.get_name() , 6);
	if( src.get_type().is_same_with( type ) ) {			// no need to bitcast
			result = src;
	} else {											// needs bitcast
		// BOX
		if( src.get_type().get_id() == INT32 ) {			// INT32 -> Int* / Int
			debug_("conform_box_int!", 8);
			src_box = vp.call( vector<op_type>(), op_type("Int", 1), "Int_new", true, vector<operand>() );
			vector<op_type> op_types; op_types.push_back( op_type("Int", 1)); op_types.push_back( INT32 );
			vector<operand> ops; ops.push_back( src_box ); ops.push_back( src );
			op_func_type fnc_type( op_type(VOID), op_types );
			vp.call( op_types, op_type(VOID), "Int_init", true, ops );
			// Int + typecast
			debug_("src_box " + src_box.get_typename() + "type" + type.get_name(), 8);
			result = vp.bitcast( src_box, type );
			// Int*
			if( type.is_same_with( op_type("Int", 0 ) ) ) {
				result = vp.load( op_type("Int", 0 ), src_box );
			}

		} else if( src.get_type().get_id() == INT1  ) {		// INT1 -> Bool* / Bool
			debug_("conform_box_bool!", 8);
			src_box = vp.call( vector<op_type>(), op_type("Bool", 1), "Bool_new", true, vector<operand>() );
			vector<op_type> op_types; op_types.push_back( op_type("Bool", 1)); op_types.push_back( INT1 );
			vector<operand> ops; ops.push_back( src_box ); ops.push_back( src );
			op_func_type fnc_type( op_type(VOID), op_types );
			vp.call( op_types, op_type(VOID), "Bool_init", true, ops );
			// Bool + typecast
			result = vp.bitcast( src_box, type );
			// Bool*
			if( type.is_same_with( op_type("Bool", 0 ) ) ) {
				result = vp.load( op_type("Bool", 0 ), src_box );
			}
		}
		// UNBOX 1 ( object value is input )
		else if ( src.get_type().is_same_with( op_type("Int", 0) ) && type.get_id()==INT32 ) {		// Int -> INT32
			debug_("conform_unbox_int!", 8);
			operand new_val_ptr = vp.alloca_mem( op_type("Int", 0) );
			vp.store( src, new_val_ptr );
			src_ptr = vp.getelementptr(
				op_type("Int", 0), new_val_ptr, int_value(0), int_value(1), op_type(INT32_PTR)
			);
			result = vp.load( op_type(INT32), src_ptr );
		} else if (src.get_type().is_same_with( op_type("Bool", 0) ) && type.get_id()==INT1 ) {		// Bool -> INT1
			debug_("conform_unbox_bool!", 8);
			operand new_val_ptr = vp.alloca_mem( op_type("Bool", 0) );
			vp.store( src, new_val_ptr );
			src_ptr = vp.getelementptr(
				op_type("Bool", 0), new_val_ptr, int_value(0), int_value(1), op_type(INT1_PTR)
			);
			result = vp.load( op_type(INT1), src_ptr );
		}
		// UNBOX 2 ( object value is input )
		
		// Int* -> INT32 / INT32_PTR
		else if ( src.get_type().is_same_with( op_type("Int", 1) ) && ( type.get_id()==INT32 || type.get_id()==INT32_PTR) ) {
			debug_("conform_unbox_int_ptr!", 8);
			operand new_val_ptr = vp.alloca_mem( op_type("Int", 1) );
			vp.store( src, new_val_ptr );
			operand new_val_load = vp.load( op_type("Int", 1), new_val_ptr );
			src_ptr = vp.getelementptr(
				op_type("Int", 0), new_val_load, int_value(0), int_value(1), op_type(INT32_PTR)
			);
			if( type.get_id()==INT32_PTR ) { result = src_ptr; }
			else { result = vp.load( op_type(INT32), src_ptr ); }
		}
		// Bool* -> INT1 / INT1_PTR
		else if (src.get_type().is_same_with( op_type("Bool", 1) ) && ( type.get_id()==INT1 || type.get_id()==INT1_PTR) ) {		
			debug_("conform_unbox_bool_ptr!", 8);
			operand new_val_ptr = vp.alloca_mem( op_type("Bool", 1) );
			vp.store( src, new_val_ptr );
			operand new_val_load = vp.load( op_type("Bool", 1), new_val_ptr );
			src_ptr = vp.getelementptr(
				op_type("Bool", 0), new_val_load, int_value(0), int_value(1), op_type(INT1_PTR)
			);
			if( type.get_id()==INT1_PTR ) { result = src_ptr; }
			else { result = vp.load( op_type(INT1), src_ptr ); }
		} 

		// LOAD PTR TO VALUE

		// OBJ* -> OBJ / OBJ** -> OBJ*
		else if( (src.get_type().is_ptr() || src.get_type().is_pptr()) &&
			src.get_type().get_deref_type().is_same_with( type )
		) {
			debug_("conform_load", 8);	
			result = vp.load( type, src );
		}
		//  OBJ** -> OBJ* ; for inheriting types, load and bitcast, only for ptr type
		else if( src.get_type().is_pptr() ) {
			operand loaded = vp.load( src.get_type().get_deref_type() , src );
			result = vp.bitcast( loaded, type );
		}
		// SIMPLE BITCAST
		else {			
			debug_("conform_bitcast", 8);								
			src_box = src;									// no box/unbox but bitcast
			result = vp.bitcast( src_box, type );
		}
		
	}
	return result;
}

// Retrieve the class tag from an object record.
// src is the object we need the tag from.
// src_class is the CgenNode for the *static* class of the expression.
// You need to look up and return the class tag for it's dynamic value
operand get_class_tag(operand src, CgenNode *src_cls, CgenEnvironment *env) {

	string class_name = string( src_cls->get_name()->get_string() );
	string class_vtable_name = class_name + "_vtable";
	
	// getelemptr vtable entry from object
	operand vtbl_ptr_ptr =  vp->getelementptr( 
			op_type( class_name , 0),
			src,
			int_value(0),
			int_value(0),					// 0th index is vtable
			op_type( class_vtable_name, 2 ) );
	operand vtbl_ptr = vp->load( op_type(class_vtable_name, 1) , vtbl_ptr_ptr );
	// lookup src vtable
	operand tag_ptr =
		vp->getelementptr( 
			op_type(class_name+"_vtable", 0),
			vtbl_ptr,
			int_value(0),
			int_value(0),					// 0th index is tag
			op_type(INT32_PTR));
	operand tag = vp->load( op_type(INT32), tag_ptr );

	return tag;
}
#endif

//
// Create a method body
// 
void method_class::code(CgenEnvironment *env)
{
	if (cgen_debug) std::cerr << "method" << endl;

	ValuePrinter vp(*(env->cur_stream));

	// ENV structure
	// - self
	// - parameters
	//		- expr

	// vp.define()
	string class_name = env->get_class()->get_name()->get_string();
	string method_name = string( class_name ) + "_" + name->get_string();
	vector<operand> method_args;
	operand self_op = operand( op_type(class_name, 1), "self" );
	method_args.push_back( self_op );	// self
// SCOPE 1
	env->enterscope();
	Formal f; int idx;
	for( idx = formals->first() ; formals->more(idx); idx = formals->next(idx) ) {
		f = formals->nth(idx);
		op_type arg_type = get_objsymbol_op_type( f->get_type_decl() , 1, class_name );
		operand arg = operand(arg_type, string(f->get_name()-> get_string()) );
		method_args.push_back( arg );
	}
	op_type method_return_type = (get_return_type() == SELF_TYPE)
							? op_type( class_name, 1 ) : get_objsymbol_op_type( get_return_type(), 1, class_name ) ;
	vp.define( method_return_type, method_name, method_args );
	// block entry
	vp.begin_block("entry");
	for( idx = 0; idx < method_args.size() ; idx+=1 ) {
		operand addr = vp.alloca_mem( method_args[idx].get_type() );
		vp.store( method_args[idx], addr );
		if(idx == 0) {	// self
			env->add_local( self, *(new operand(addr) ) );
		} else {		// other args
			env->add_local( formals->nth(idx-1)->get_name(), *(new operand(addr)) );
		}
	}
// SCOPE 2
	env->enterscope();
	operand exec_result = expr->code(env);
	operand exec_result_conform = conform( exec_result, method_return_type, env );
	vp.ret( exec_result_conform );

	// block abort
	vp.begin_block("abort");
	vp.call( vector<op_type>(), op_type(VOID), "abort", true, vector<operand>() );
	vp.unreachable();

	vp.end_define();
}

//
// Codegen for expressions.  Note that each expression has a value.
//

operand assign_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "assign" << endl;
	ValuePrinter vp(*(env->cur_stream));
	operand expr_code = expr->code(env);
	operand target = *(env->lookup(name));
	op_type target_type = target.get_type().get_deref_type();
	operand expr_code_conform = conform( expr_code, target_type, env );
	vp.store(	// store( RHS, LHS )
		expr_code_conform,
		target
	);
	return expr_code;
}

operand cond_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "cond" << endl;
	ValuePrinter vp(*(env->cur_stream));
	
	string label_then = env->new_label("then.", false);
	string label_else = env->new_label("else.", false);
	string label_endif = env->new_label("endif.", true); // increment for next block

	op_type then_type = get_objsymbol_op_type( then_exp->get_type(), 1, string(env->get_class()->get_name()->get_string()) );
	op_type else_type = get_objsymbol_op_type( else_exp->get_type(), 1, string(env->get_class()->get_name()->get_string()) );

	Symbol result_type_s = env->get_class()->get_classtable()->get_lub( then_exp->get_type(), else_exp->get_type(), env->get_class()->get_name() );
	op_type result_type = get_objsymbol_op_type( result_type_s, 1, string(env->get_class()->get_name()->get_string()) );

	operand result_op = vp.alloca_mem( result_type );
	// logic
	vp.branch_cond( conform(pred->code(env), op_type(INT1), env), label_then, label_else );
	
	vp.begin_block( label_then );
		operand then_code = then_exp->code(env);
		operand then_code_conform = conform( then_code, result_type, env);
		vp.store( then_code_conform, result_op );
		vp.branch_uncond( label_endif );
	vp.begin_block( label_else );
		operand else_code = else_exp->code(env);
		operand else_code_conform = conform( else_code, result_type, env);
		vp.store( else_code_conform, result_op );
		vp.branch_uncond( label_endif );

	vp.begin_block( label_endif );
	return vp.load( result_type, result_op );
}

operand loop_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "loop" << endl;
	ValuePrinter vp(*(env->cur_stream));

	string label_loop = env->new_label("loop.", false);
	string label_true = env->new_label("true.", false);
	string label_false = env->new_label("false.", true); // increment for next block

	vp.branch_uncond( label_loop );
	vp.begin_block( label_loop );
		operand pred_op = pred->code(env);
	vp.branch_cond( conform(pred_op, op_type(INT1), env) , label_true, label_false );
	vp.begin_block( label_true );
		operand body_op = body->code(env);
		vp.branch_uncond( label_loop );
	vp.begin_block( label_false );

	return null_value( op_type("Object",1) );
} 

operand block_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "block" << endl;
	operand return_expr;
	int idx;
	for( idx = body->first() ; body->more(idx); idx = body->next(idx) ) {
		return_expr = body->nth(idx)->code(env);
	}
	return return_expr;
}

operand let_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "let" << endl;
	ValuePrinter vp(*(env->cur_stream));

	// code init
	operand init_op = init->code(env);

	// allocate var
	op_type var_type;
	var_type = get_objsymbol_op_type( type_decl, 1, string(env->get_class()->get_name()->get_string()) );
	debug_("var_type : "+ var_type.get_name(), 4);

	operand var = vp.alloca_mem(var_type);
	env->add_local(identifier, *(new operand(var)) );

	// check empty & conform
	assert( init_op.get_type().is_self_type() == false ); 	// PA5 constraint.
	bool isExprEmpty = init_op.get_type().is_same_with( op_type(EMPTY) );
	operand init_op_conform;
	if( isExprEmpty ) {	// if empty, store with default value
		init_op_conform = get_default_value( var_type );
	} else {
		init_op_conform = conform( init_op, var_type, env);
	}
	// store( RHS, LHS )
	vp.store( init_op_conform, var );

	operand result = body->code(env);
	env->kill_local();	// exit scope
	
	return result;
}

operand plus_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "plus" << endl;
	ValuePrinter vp(*(env->cur_stream));
	operand e1_conform = conform( e1->code(env), op_type(INT32), env);
	operand e2_conform = conform( e2->code(env), op_type(INT32), env);
	return vp.add(e1_conform, e2_conform);
}

operand sub_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "sub" << endl;
	ValuePrinter vp(*(env->cur_stream));
	operand e1_conform = conform( e1->code(env), op_type(INT32), env);
	operand e2_conform = conform( e2->code(env), op_type(INT32), env);
	return vp.sub(e1_conform, e2_conform);
}

operand mul_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "mul" << endl;
	ValuePrinter vp(*(env->cur_stream));
	operand e1_conform = conform( e1->code(env), op_type(INT32), env);
	operand e2_conform = conform( e2->code(env), op_type(INT32), env);
	return vp.mul(e1_conform, e2_conform);
}

operand divide_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "div" << endl;
	ValuePrinter vp(*(env->cur_stream));

	string label_ok = env->new_ok_label();
	operand e1_conform = conform( e1->code(env), op_type(INT32), env);
	operand e2_conform = conform( e2->code(env), op_type(INT32), env);

	operand denom_zero = vp.icmp( EQ, e2_conform, int_value(0) );
	vp.branch_cond( denom_zero, "abort", label_ok );

	vp.begin_block(label_ok);
	return vp.div( e1_conform, e2_conform );
}

operand neg_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "neg" << endl;
	operand e1_conform = conform( e1->code(env), op_type(INT32), env);
	ValuePrinter vp(*(env->cur_stream));
	return vp.sub( int_value(0, true), e1_conform ); // 0 - val = -val
}

operand lt_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "lt" << endl;
	ValuePrinter vp(*(env->cur_stream));
	operand e1_conform = conform( e1->code(env), op_type(INT32), env);
	operand e2_conform = conform( e2->code(env), op_type(INT32), env);
	return vp.icmp(LT, e1_conform, e2_conform );
}

operand eq_class::code(CgenEnvironment *env) 
{

	if (cgen_debug) std::cerr << "eq" << endl;
	ValuePrinter vp(*(env->cur_stream));

	operand e1_op = e1->code(env);
	operand e2_op = e2->code(env);

	debug_( "e1_type : " + e1_op.get_typename() + ", e2_type : " + e2_op.get_typename(), 6);

	// int - compare value
	if( e1_op.get_type().is_same_with( op_type(INT32) ) ||
		e1_op.get_type().is_same_with( op_type(INT32_PTR) ) ||
		e1_op.get_type().is_int_object() ) {
		return vp.icmp(EQ, conform(e1_op, op_type(INT32), env),
							conform(e2_op, op_type(INT32), env));
	// bool - compare value
	} else if ( e1_op.get_type().is_same_with( op_type(INT1) ) ||
				e1_op.get_type().is_same_with( op_type(INT1_PTR) ) ||
				e1_op.get_type().is_bool_object()  ) {
		return vp.icmp(EQ, conform(e1_op, op_type(INT1), env),
							conform(e2_op, op_type(INT1), env));
	// string - compare using strcmp
	} else if ( e1_op.get_type().get_name().compare("%String*") == 0 || 
				e1_op.get_type().get_name().compare("%String**") == 0
	) {
		operand e1_ptr = vp.getelementptr( op_type("String", 0), conform(e1_op, op_type("String", 1), env), int_value(0), int_value(1), op_type(INT8_PPTR) );
		operand e1_load = vp.load( op_type(INT8_PTR), e1_ptr);
		operand e2_ptr = vp.getelementptr( op_type("String", 0), conform(e2_op, op_type("String", 1), env), int_value(0), int_value(1), op_type(INT8_PPTR) );
		operand e2_load = vp.load( op_type(INT8_PTR), e2_ptr);
		vector<operand> strcmp_args;
		strcmp_args.push_back(e1_load); strcmp_args.push_back(e2_load);
		operand strcmp_call = vp.call( vector<op_type>(2, op_type(INT8_PTR)), op_type(INT32), "strcmp", true, strcmp_args); 
		return vp.icmp(EQ, strcmp_call, int_value(0) );
	}
	// else -> compare pointer
	return vp.icmp(EQ, conform(e1_op, op_type(INT8_PTR), env),
						conform(e2_op, op_type(INT8_PTR), env));

}

operand leq_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "leq" << endl;
	ValuePrinter vp(*(env->cur_stream));
	operand e1_conform = conform( e1->code(env), op_type(INT32), env);
	operand e2_conform = conform( e2->code(env), op_type(INT32), env);
	return vp.icmp(LE, e1_conform, e2_conform);
}

operand comp_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "complement" << endl;
	ValuePrinter vp(*(env->cur_stream));
	operand e1_conform = conform( e1->code(env), op_type(INT1), env);
	return vp.xor_in( e1_conform, bool_value(true, true) ); // x XOR true = NOT x
}

operand int_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Integer Constant" << endl;
	return int_value( atoi(token->get_string()) );
}

operand bool_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Boolean Constant" << endl;
	return bool_value( val, false );
}

operand object_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Object" << endl;
	ValuePrinter vp(*(env->cur_stream));
	
	operand* obj_var_ptr = env->lookup(name);
	op_type obj_type = obj_var_ptr->get_type().get_deref_type();

	operand result;
	// Explicitly need to load self
	if( name == self ) {
		debug_(" Object type self" , 4);
		obj_type = op_type(string(env->get_class()->get_name()->get_string()), 1);
		result = vp.load( obj_type, *obj_var_ptr );
	} else {
		debug_(" Object type " + obj_type.get_name() , 4);
		// We return pointer of the object instead of the value
		result = *obj_var_ptr;
	}

	return result;
}

operand no_expr_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "No_expr" << endl;
	return operand(); // this returns operand with empty
}

//*****************************************************************
// The next few functions are for node types not supported in Phase 1
// but these functions must be defined because they are declared as
// methods via the Expression_SHARED_EXTRAS hack.
//*****************************************************************

operand string_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "string_const" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
	operand result;
#else
	operand result;
	ValuePrinter vp(*(env->cur_stream));
	int idx;
	for( idx = stringtable.first() ; stringtable.more(idx) ; idx = stringtable.next(idx) ) {
		if( 1 == stringtable.lookup(idx)->equal_string( token->get_string(), token->get_len() ) ) {
			// match
			result = const_value( op_type( "String", 1), "@String."+itos(idx), false );
			break;
		}
	}
#endif
	return result;
}

operand static_dispatch_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "static dispatch" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
	operand result;
#else
	ValuePrinter vp(*(env->cur_stream));

	// foreach expr actuals, code
	int idx; Expression e;
	operand arg_op;
	vector<operand> arg_ops;
	for( idx = 1 ; idx < actual->len()+1; idx++) {		// push other args
		e = actual->nth(idx-1);
		arg_op = e->code(env);
		arg_ops.push_back(arg_op);
	}

	// code expr and load
	operand expr_op = expr->code(env);
	string expr_classname = string(type_name->get_string()); 
	string expr_vtablename = expr_classname+"_vtable";
	string expr_vtableprotname = "@"+expr_classname+"_vtable_prototype";
	op_type expr_op_type = get_objsymbol_obj_op_type( expr->type, 0, expr_classname);
	op_type expr_op_ptr_type = get_objsymbol_obj_op_type( expr->type, 1, expr_classname);

	// check if expr == null -> "abort"
	string ok_label = env->new_ok_label();
	operand expr_op_conform = expr_op;

	// INT32 -> Int
	if( expr_op.get_type().is_same_with( op_type(INT32) ) ) {		
		expr_op_conform = conform(expr_op, op_type("Int", 0), env);
	// INT1  -> Bool
	} else if( expr_op.get_type().is_same_with( op_type(INT1)) ) {
		expr_op_conform = conform(expr_op, op_type("Bool", 0), env);	
	} 
	
	// check if expr == null -> abort
	if( expr_op_conform.get_type().is_ptr() ){
		operand is_expr_null = vp.icmp( EQ, expr_op_conform, null_value( expr_op_type ) );
		vp.branch_cond( is_expr_null, "abort", ok_label );
	} else {
		vp.branch_uncond( ok_label );
	}

	// get vtable
	vp.begin_block( ok_label );
	op_type expr_op_vt_type = op_type(expr_vtablename, 2);

	int fnc_idx = *(env->get_class()->get_classtable()->lookup(type_name)->methodtable_idx->lookup(name));		
	op_type ret_type = *(env->get_class()->get_classtable()->lookup(type_name)->methodtable_return_type->lookup(name));
	vector<op_type> arg_types = *(env->get_class()->get_classtable()->lookup(type_name)->methodtable_arg_types->lookup(name));

	op_type fnc_type = op_func_type( ret_type, arg_types );
	op_type fnc_ptr_type = op_func_ptr_type(ret_type, arg_types);
	fnc_type.set_id( OBJ_PPTR );		// forced
	fnc_ptr_type.set_id( OBJ_PPTR );	// forced
	operand fnc_ptr =
		vp.getelementptr( op_type(expr_vtablename, 0), 
						  const_value( op_type(expr_vtablename, 1), expr_vtableprotname, false)
						  , int_value(0), int_value(fnc_idx), fnc_ptr_type );
	
	// load function
	operand fnc = vp.load( fnc_type, fnc_ptr );

	// typmatch ops
	operand arg_typmatch_op;
	vector<operand> arg_typmatch_ops;
	for( idx = 1 ; idx < arg_types.size(); idx++) {	// push other args
		arg_typmatch_op = conform(arg_ops[idx-1], arg_types[idx], env);
		arg_typmatch_ops.push_back(arg_typmatch_op);
	}
	// push self to first of args list
	operand expr_op_conform_typmatch = conform( expr_op_conform, arg_types[0], env);
	arg_typmatch_ops.insert( arg_typmatch_ops.begin(), expr_op_conform_typmatch );		// push self to first; needs to be typecasted for static dispatch

	// call function
	string fn_name = fnc.get_name().erase(0, 1);
	operand result = vp.call(arg_types, ret_type, fn_name, false, arg_typmatch_ops );

#endif
	return result;
}

operand dispatch_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "dispatch" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
	operand result;
#else
	ValuePrinter vp(*(env->cur_stream));


	// foreach expr actuals, code
	int idx; Expression e;
	operand arg_op;
	vector<operand> arg_ops;
	for( idx = 1 ; idx < actual->len()+1; idx++) {	// push other args
		e = actual->nth(idx-1);
		arg_op = e->code(env);
		arg_ops.push_back(arg_op);
	}

	// code expr and load (different from static dispatch)
	operand expr_op = expr->code(env);
	Symbol type_name = expr->get_type();
	debug_("dyndispatch - expr type: "+string(expr_op.get_typename()), 4);
	debug_("dyndispatch - target class: "+string(type_name->get_string()), 4);

	string ok_label = env->new_ok_label();
	string expr_classname;
	if( type_name == SELF_TYPE ) {
		expr_classname = string( env->get_class()->get_name()->get_string()) ;
	} else {
		expr_classname = string(type_name->get_string()); 
	}
	string expr_vtablename = expr_classname+"_vtable";
	op_type expr_op_type = get_objsymbol_obj_op_type( expr->type, 0, expr_classname);
	op_type expr_op_ptr_type = get_objsymbol_obj_op_type( expr->type, 1, expr_classname);
	
	// for dynamic dispatch, bitcast check is necessary
	// but not for primitive types
	operand expr_op_conform = expr_op;

	// For Int/Bool expr_op
	if( (expr_op.get_type().get_name().find("Int") != string::npos) &&
		(expr_op.get_type().get_name().find("Bool") != string::npos) ) {
			
		expr_op_conform = expr_op;
	}
	// For expr_op with other types
	else if( (expr_op.get_type().get_name().compare( expr_op_ptr_type.get_name() ) != 0) ) {
		
		debug_("dyn dispatch bitcast : " + expr_op.get_type().get_name() + ", " + expr_op_ptr_type.get_name() , 4);
		expr_op_conform = conform( expr_op, expr_op_ptr_type, env );
	}
	// INT32 -> Int
	else if( expr_op.get_type().is_same_with( op_type(INT32) ) ) {		
		expr_op_conform = conform(expr_op, op_type("Int", 0), env);
	// INT1  -> Bool
	} else if( expr_op.get_type().is_same_with( op_type(INT1)) ) {
		expr_op_conform = conform(expr_op, op_type("Bool", 0), env);	
	} 

	// check if expr == null -> abort
	if( expr_op_conform.get_type().is_ptr() ){
		operand is_expr_null = vp.icmp( EQ, expr_op_conform, null_value( expr_op_type ) );
		vp.branch_cond( is_expr_null, "abort", ok_label );
	} else {
		vp.branch_uncond( ok_label );
	}
	
	// get vtable (different from static dispatch)
	vp.begin_block( ok_label );
	op_type expr_op_vt_type = op_type(expr_vtablename, 2);

	int fnc_idx;
	op_type ret_type;
	vector<op_type> arg_types;
	if( type_name == SELF_TYPE ) {
		fnc_idx = *(env->get_class()->methodtable_idx->lookup(name));		
		ret_type = *(env->get_class()->methodtable_return_type->lookup(name));
		arg_types = *(env->get_class()->methodtable_arg_types->lookup(name));
	} else {
		fnc_idx = *(env->get_class()->get_classtable()->lookup(type_name)->methodtable_idx->lookup(name));		
		ret_type = *(env->get_class()->get_classtable()->lookup(type_name)->methodtable_return_type->lookup(name));
		arg_types = *(env->get_class()->get_classtable()->lookup(type_name)->methodtable_arg_types->lookup(name));
	}
	
	operand expr_ptr =
		vp.getelementptr( expr_op_type, expr_op_conform, int_value(0), int_value(0), expr_op_vt_type );
	operand expr_vtable =
		vp.load( op_type(expr_vtablename, 1), expr_ptr );


	op_type fnc_type = op_func_type( ret_type, arg_types  );
	op_type fnc_ptr_type = op_func_ptr_type(ret_type, arg_types);
	fnc_type.set_id( OBJ_PPTR );		// forced
	fnc_ptr_type.set_id( OBJ_PPTR );	// forced
	operand fnc_ptr =
		vp.getelementptr( op_type(expr_vtablename, 0), 
						  expr_vtable	// calls vtable by runtime load
						  , int_value(0), int_value(fnc_idx), fnc_ptr_type );

	// load function
	operand fnc = vp.load( fnc_type, fnc_ptr );

	// typematch ops
	operand arg_typmatch_op;
	vector<operand> arg_typmatch_ops;
	for( idx = 1 ; idx < arg_types.size(); idx++) {	// push other args
		arg_typmatch_op = conform(arg_ops[idx-1], arg_types[idx], env);
		arg_typmatch_ops.push_back(arg_typmatch_op);
	}
	// push self to first of args list
	arg_typmatch_ops.insert( arg_typmatch_ops.begin(), expr_op_conform );	// push self to the very first

	// call function
	string fn_name = fnc.get_name().erase(0, 1);
	operand result = vp.call(arg_types, ret_type, fn_name, false, arg_typmatch_ops );
#endif
	return result;
}

operand new__class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "newClass" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
	operand result;
#else
	ValuePrinter vp(*(env->cur_stream));
	// assert( type_name != SELF_TYPE );		// Exclude - new SELF_TYPE support.
	operand result;

	string type_name_str = string( type_name->get_string() );;
	if( type_name == SELF_TYPE ) {

		type_name_str = string(env->get_class()->get_name()->get_string());
		operand self_val = vp.load( op_type(type_name_str, 1), *(env->lookup(self)) );
		operand self_vtbl_ptr = vp.getelementptr( op_type(type_name_str, 0), self_val, int_value(0), int_value(0), op_type(type_name_str+"_vtable", 2)  );
		operand self_vtbl = vp.load( op_type(type_name_str+"_vtable", 1), self_vtbl_ptr );

		op_type fnc_type = op_func_type( op_type(type_name_str, 1), vector<op_type>()  );
		op_type fnc_ptr_type = op_func_ptr_type( op_type(type_name_str, 1), vector<op_type>() );

		fnc_type.set_id( OBJ_PPTR );		// forced
		fnc_ptr_type.set_id( OBJ_PPTR );	// forced
		operand fnc_ptr = vp.getelementptr( op_type(type_name_str+"_vtable", 0), 
						  const_value( op_type(type_name_str+"_vtable", 1), self_vtbl.get_name(), false)
						  , int_value(0), int_value(3), fnc_ptr_type );
		// load function

		operand fnc = vp.load( fnc_type, fnc_ptr );
		result = vp.call(
			vector<op_type>(), op_type(type_name_str, 1), fnc.get_name().substr(1), false, vector<operand>()
		);

	} else {
		result = vp.call(
		vector<op_type>(), op_type(type_name_str, 1), type_name_str+"_new", true, vector<operand>()
		);
	}
	
#endif
	return result;
}

operand isvoid_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "isvoid" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
	operand result;
#else
	ValuePrinter vp(*(env->cur_stream));
	operand result;
	
	// if primitive type, pass
	operand expr_op = e1->code(env);
	debug_( "type: "+expr_op.get_type().get_name() , 4);
	if( expr_op.get_type().is_same_with( op_type(INT32) )
		|| expr_op.get_type().is_same_with( op_type(INT1) )   ) {
		return bool_value(false, true);
	}

	operand is_expr_null = vp.icmp( EQ, expr_op, null_value( expr_op.get_type() ) );
	operand result_ptr = vp.call( vector<op_type>(), op_type("Bool", 1), "Bool_new", true, vector<operand>() );
	
	vector<op_type> call_types;
	call_types.push_back( op_type("Bool", 1)); call_types.push_back( op_type(INT1) );
	vector<operand> call_args;
	call_args.push_back( result_ptr); call_args.push_back( is_expr_null );

	vp.call( call_types, op_type(VOID), "Bool_init", true, call_args );
	result = result_ptr;

#endif
	return result;
}


typedef std::pair<int, int> kv;
bool sort_comp ( const kv& l, const kv& r) {	// descending
	return l.second > r.second;
}

operand typcase_class::code(CgenEnvironment *env)
{
	if (cgen_debug) 
		std::cerr << "typecase::code()" << endl;
	operand result;

#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	ValuePrinter vp(*(env->cur_stream));
	
	operand expr_op = expr->code(env);
	op_type expr_op_type = expr_op.get_type();
	Symbol expr_type = expr->get_type();
	CgenNode* expr_cls = env->type_to_class(expr_type);

	string join_typename;
	if( type == SELF_TYPE ) {
		join_typename = string( env->get_class()->get_name()->get_string()) ;
	} else {
		join_typename = string( type->get_string() ); 
	}
	op_type join_type =  op_type( join_typename, 1);

	string label_ok = env->new_ok_label();
	string label_st = env->new_label( "case.st.", false); 	
	string label_en = env->new_label( "case.en.", false);

	operand expr_op_conform = expr_op;
	if( expr_op.get_type().is_same_with( op_type(INT32) ) ) {
		expr_op_conform = conform(expr_op, op_type("Int", 1), env);
	} else if( expr_op.get_type().is_same_with( op_type(INT1)) ) {
		expr_op_conform = conform(expr_op, op_type("Bool", 1), env);
	}
	if( expr_op.get_type().is_pptr() ) {
		expr_op_conform = conform(expr_op, expr_op.get_type().get_deref_type(), env);
	}

	if( expr_op_conform.get_type().is_ptr() ){
		operand is_expr_null = vp.icmp( EQ, expr_op_conform, null_value( expr_op_type ) );
		vp.branch_cond( is_expr_null, "abort", label_ok );
	} else {
		vp.branch_uncond( label_ok );
	}
	
	// OK BLOCK
	vp.begin_block( label_ok );
	operand expr_tag = get_class_tag( expr_op_conform, expr_cls, env );
	vp.branch_uncond( label_st );

	// START BLOCK
	vp.begin_block( label_st );
	operand ret_val_ptr = vp.alloca_mem( join_type );
	string first_branch = env->new_label("branch.0.st.", false);
	vp.branch_uncond( first_branch ) ;

	Case c; int i, v;
	// We need to sort cases by tag number in decending order
	// so that the closest type branch is matched first as the program semantics implies
	vector<kv> idx_tag_pair;
	for( i = cases->first() ; cases->more(i); i = cases->next(i)) {
		c = cases->nth(i);
		v = env->type_to_class(c->get_type_decl())->get_tag();
		idx_tag_pair.push_back( kv(i, v) );
	}
	std::stable_sort( idx_tag_pair.begin(), idx_tag_pair.end(), sort_comp );

	for( i = cases->first() ; cases->more(i) ; i = cases->next(i) ) {

		c = cases->nth( idx_tag_pair[i].first ); // fetch ith index from the sorted map

		env->case_branch_index = i;
		operand branch_expr = c->code( expr_op_conform, expr_tag, join_type, env );
		vp.store( branch_expr, ret_val_ptr );

		// (cont) branch.i.ok.0
		vp.branch_uncond( label_en );

		// branch.i.fail.0
		string branch_fail = env->new_label( "branch."+itos(i)+".fail.", false );
		vp.begin_block( branch_fail );
		if( cases->more(i+1) ) {
			string branch_next_st = env->new_label( "branch."+itos(i+1)+".st.", false);
			vp.branch_uncond( branch_next_st );
		} else {
			vp.branch_uncond( "abort" );
		}

	}

	vp.begin_block( label_en );
	string _label = env->new_label( "useless", true ); // increment for next case statement

	operand ret_val = vp.load( join_type, ret_val_ptr );
	result = ret_val;

#endif
	return result;
}

// If the source tag is >= the branch tag and <= (max child of the branch class) tag,
// then the branch is a superclass of the source
operand branch_class::code(operand expr_val, operand tag,
				op_type join_type, CgenEnvironment *env) {
	
	operand result;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	ValuePrinter vp(*(env->cur_stream));

	string branch_st = env->new_label( "branch."+itos(env->case_branch_index)+".st.", false);
	string branch_gt = env->new_label( "branch."+itos(env->case_branch_index)+".gt.", false);
	string branch_ok = env->new_label( "branch."+itos(env->case_branch_index)+".ok.", false);
	string branch_fail = env->new_label( "branch."+itos(env->case_branch_index)+".fail.", false );


	CgenNode* br_cls = env->type_to_class(type_decl);
	op_type expr_val_type = op_type( string(type_decl->get_string()), 1 );

	int_value br_tag( br_cls->get_tag() );
	int_value br_maxchild_tag( br_cls->get_max_child() );

	vp.begin_block( branch_st );
	operand comp1 = vp.icmp( LT, tag, br_tag);
	vp.branch_cond( comp1, branch_fail, branch_gt );

	vp.begin_block( branch_gt );
	operand comp2 = vp.icmp( GT, tag, br_maxchild_tag );
	vp.branch_cond( comp2, branch_fail, branch_ok);

	vp.begin_block( branch_ok );
	operand expr_val_conform = conform( expr_val, expr_val_type, env );
	env->add_local( name, expr_val_conform );
	operand branch_expr_op = expr->code(env);
	env->kill_local();
	
	operand branch_expr_op_conform = conform( branch_expr_op, join_type, env);
	result = branch_expr_op_conform;
	// note : rest of the branch will be covered in case::code()

#endif
	return result;
}

//////////////////////////////////
//////////////////////////////////
//////////////////////////////////

// Create the LLVM Function corresponding to this method.
void method_class::layout_feature(CgenNode *cls) 
{
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// Not Using. Using different functions.
#endif
}

// Assign this attribute a slot in the class structure
void attr_class::layout_feature(CgenNode *cls)
{
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// Not Using. Using different functions.
#endif
}

void attr_class::code(CgenEnvironment *env)
{
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// Not using. All handled in code_class()
#endif
}