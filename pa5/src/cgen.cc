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

#define PA5; // TODO must delete
// 
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
	// TODO ADD setup_external_functions
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

CgenNode* CgenClassTable::getMainmain(CgenNode* c)
{
	if (c && ! c->basic())
		return c;                   // Found it!

	List<CgenNode> *children = c->get_children();
	for (List<CgenNode> *child = children; child; child = child->tl()) {
		if (CgenNode* foundMain = this->getMainmain(child->hd()))
			return foundMain;   // Propagate it up the recursive calls
	}

	return 0;                           // Make the recursion continue
}

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
    // TODO code_constants
    // produce String.0 / String.1 / ...
	// ADD CODE HERE
#endif
}

// generate code to define a global string constant
void StringEntry::code_def(ostream& s, CgenClassTable* ct)
{
#ifdef PA5
	// ADD CODE HERE
    // TODO string code_def
#endif
}

// generate code to define a global int constant
void IntEntry::code_def(ostream& s, CgenClassTable* ct)
{
	// Leave this method blank, since we are not going to use global
	// declarations for int constants.
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
	// This must be after code_module() since that emits constants
	// needed by the code() method for expressions
	CgenNode* mainNode = getMainmain(root());
	debug_( "codeGenMainmain", 2);
	mainNode->codeGenMainmain();
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
    // TODO code_classes ; call code_class for each class / recursively
	// ADD CODE HERE

}
#endif


//
// Create LLVM entry point. This function will initiate our Cool program 
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main()
{
	ValuePrinter vp(*ct_stream);

	// Make string before define
	string _str = "Main_main() returned %d\n";
	const_value _str_val ( op_arr_type(INT8, 25), _str, false );
	vp.init_constant(".str", _str_val);

	// Define a function main that has no parameters and returns an i32
	vp.define( op_type(INT32), "main", vector<operand>() );
	// Define an entry basic block
	vp.begin_block( "entry" );
	// Call Main_main(). This returns int* for phase 1, Object for phase 2
	operand result_Main_main = 
		vp.call( vector<op_type>(), op_type(INT32), "Main_main", true, vector<operand>() );

#ifndef PA5

	// Get the address of the string "Main_main() returned %d\n" using getelementptr 
	operand _str_ptr = vp.getelementptr(
		op_arr_type(INT8, 25),
		global_value( op_arr_type(INT8_PTR, 25), ".str" , _str_val ),
		int_value(0),
		int_value(0),
		op_type(INT8_PTR) 
	);

	// Call printf with the string address of "Main_main() returned %d\n"
	// and the return value of Main_main() as its arguments
	
	vector<op_type> call_result_paramtypes;
	call_result_paramtypes.push_back( op_type(INT8_PTR) );
	call_result_paramtypes.push_back( op_type(VAR_ARG) ) ;
	vector<operand> call_result_params;
	call_result_params.push_back( _str_ptr );
	call_result_params.push_back( result_Main_main );

	operand call_result = vp.call(
			call_result_paramtypes,
			op_type(INT32),
			"printf",
			true,
			call_result_params
	);

	// Insert return 0
	vp.ret( int_value(0) );

#else
    // TODO change phase 2
	// Phase 2

#endif
	// End define
	vp.end_define();
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
	// ADD CODE HERE
    
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
	layout_features();
    // TODO CgenNode::setup

    // define class type 
    vp->type_define(name->get_string(), vector<op_type>() );
    //
    vp->end_define();

    // define vtable type
    string vtable_name = string(name->get_string()) + "_vtable";
    vp->type_define( vtable_name, vector<op_type>() );
    //
    vp->end_define();

    // vtable prototype
    vp->init_struct_constant(
        global_value( op_type("Object_vtable"), "Object_vtable_prototype"),
        vector<op_type>( 1, op_type(INT32) ),
        vector<const_value>( 1,  int_value(1) )
    );
    

#endif
}

#ifdef PA5
//
// Class codegen. This should performed after every class has been setup.
// Generate code for each method of the class.
//
void CgenNode::code_class()
{
	// No code generation for basic classes. The runtime will handle that.
	if (basic())
		return;
	
	// ADD CODE HERE
    // TODO CgenNode::setup
}

// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features()
{
	// ADD CODE HERE
    // TODO CgenNode::layout_features()


}
#else

// 
// code-gen function main() in class Main
//
void CgenNode::codeGenMainmain()
{
	// In Phase 1, this can only be class Main. Get method_class for main().
	assert(std::string(this->name->get_string()) == std::string("Main"));
	method_class* mainMethod = (method_class*) features->nth(features->first());

	// Generally what you need to do are:
	// -- setup or create the environment, env, for translating this method
	// -- invoke mainMethod->code(env) to translate the method

	ValuePrinter vp(*class_table->ct_stream);
	CgenEnvironment* env = new CgenEnvironment(*(class_table->ct_stream), this);

	vp.define( op_type(INT32), "Main_main", vector<operand>() );
	vp.begin_block("entry");

	mainMethod->code(env); // returns here

	vp.end_define();

}

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

void CgenEnvironment::add_local(Symbol name, operand &vb) {
	var_table.enterscope();
	var_table.addid(name, &vb);
}

void CgenEnvironment::kill_local() {
	var_table.exitscope();
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
	// ADD CODE HERE (PA5 ONLY)
    // TODO conform
	return operand();
}

// Retrieve the class tag from an object record.
// src is the object we need the tag from.
// src_class is the CgenNode for the *static* class of the expression.
// You need to look up and return the class tag for it's dynamic value
operand get_class_tag(operand src, CgenNode *src_cls, CgenEnvironment *env) {
	// ADD CODE HERE (PA5 ONLY)
    // TODO get_class_tag
	return operand();
}
#endif

//
// Create a method body
// 
void method_class::code(CgenEnvironment *env)
{
	if (cgen_debug) std::cerr << "method" << endl;

	ValuePrinter vp(*(env->cur_stream));

	// Return expr value
	vp.ret( expr->code(env) );
}

//
// Codegen for expressions.  Note that each expression has a value.
//

operand assign_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "assign" << endl;
	ValuePrinter vp(*(env->cur_stream));
	operand expr_code = expr->code(env);
	vp.store(	// store( RHS, LHS )
		expr_code,
		*(env->lookup(name))
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
	
	assert( then_exp->get_type()->equal_string(	// assertation for PA4
		else_exp->get_type()->get_string(), else_exp->get_type()->get_len()
	));
	op_type result_type = ( string(then_exp->get_type()->get_string()).compare("Bool") == 0 )
		? op_type(INT1) : op_type(INT32);
	operand result_op = vp.alloca_mem( result_type );
	
	// logic
	vp.branch_cond( pred->code(env), label_then, label_else );
	vp.begin_block( label_then );
		vp.store( then_exp->code(env), result_op );
		vp.branch_uncond( label_endif );
	vp.begin_block( label_else );
		vp.store( else_exp->code(env), result_op );
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
	vp.branch_cond( pred_op, label_true, label_false );
	vp.begin_block( label_true );
		operand body_op = body->code(env);
		vp.branch_uncond( label_loop );
	vp.begin_block( label_false );

	return operand(); // TODO (pa4) loop_class is returning void right?
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

	// allocate var
	op_type var_type = ( string(type_decl->get_string()).compare("Bool") == 0 )
		? op_type(INT1) : op_type(INT32);
	operand var = vp.alloca_mem(var_type);
	env->add_local(identifier, var);

	// code for init
	operand init_op = init->code(env);
	bool isExprEmpty = init_op.get_type().is_same_with( op_type(EMPTY) );
	if( isExprEmpty ) {	// if empty, store with default value
		init_op = var_type.is_same_with( op_type(INT1) )
			? const_value(op_type(INT1), "false", true) : const_value(op_type(INT32), "0", true);
	}
	// store( RHS, LHS )
	vp.store( init_op, var );
	
	return body->code(env);
}

operand plus_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "plus" << endl;
	ValuePrinter vp(*(env->cur_stream));
	return vp.add(e1->code(env), e2->code(env));
}

operand sub_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "sub" << endl;
	ValuePrinter vp(*(env->cur_stream));
	return vp.sub(e1->code(env), e2->code(env));
}

operand mul_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "mul" << endl;
	ValuePrinter vp(*(env->cur_stream));
	return vp.mul(e1->code(env), e2->code(env));
}

operand divide_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "div" << endl;
	ValuePrinter vp(*(env->cur_stream));

	string label_err = env->new_label("diverr.", false);
	string label_ok = env->new_label("div.", true);
	operand e1_code = e1->code(env);
	operand e2_code = e2->code(env);

	operand denom_zero = vp.icmp( EQ, e2_code, int_value(0) );
	vp.branch_cond( denom_zero, label_err, label_ok );
	vp.begin_block(label_err);
		operand err = 
			vp.call( vector<op_type>(), op_type(VOID), "abort", true, vector<operand>() );
		vp.unreachable(); // TODO (pa4) divide_class is it right?
	vp.begin_block(label_ok);
		return vp.div( e1_code, e2_code );
}

operand neg_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "neg" << endl;
	ValuePrinter vp(*(env->cur_stream));
	return vp.sub( int_value(0, true), e1->code(env) ); // 0 - val = -val
}

operand lt_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "lt" << endl;
	ValuePrinter vp(*(env->cur_stream));
	return vp.icmp(LT, e1->code(env), e2->code(env));
}

operand eq_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "eq" << endl;
	ValuePrinter vp(*(env->cur_stream));
	return vp.icmp(EQ, e1->code(env), e2->code(env));
}

operand leq_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "leq" << endl;
	ValuePrinter vp(*(env->cur_stream));
	return vp.icmp(LE, e1->code(env), e2->code(env));
}

operand comp_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "complement" << endl;
	ValuePrinter vp(*(env->cur_stream));
	return vp.xor_in( e1->code(env), bool_value(true, true) ); // x XOR true = NOT x
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
	
	op_type obj_type = env->lookup(name)->get_type().is_same_with( op_type(INT32_PTR) )
		? op_type(INT32) : op_type(INT1) ;
	debug_(" Object type " + obj_type.get_name() , 4);
	return vp.load( op_type(obj_type), *(env->lookup(name)) );
}

operand no_expr_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "No_expr" << endl;
	return operand();
}

//*****************************************************************
// The next few functions are for node types not supported in Phase 1
// but these functions must be defined because they are declared as
// methods via the Expression_SHARED_EXTRAS hack.
//*****************************************************************

// TODO operands
operand static_dispatch_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "static dispatch" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand string_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "string_const" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand dispatch_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "dispatch" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand typcase_class::code(CgenEnvironment *env)
{
	if (cgen_debug) 
		std::cerr << "typecase::code()" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand new__class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "newClass" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand isvoid_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "isvoid" << endl;
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

// Create the LLVM Function corresponding to this method.
void method_class::layout_feature(CgenNode *cls) 
{
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE
#endif
}

// If the source tag is >= the branch tag and <= (max child of the branch class) tag,
// then the branch is a superclass of the source
operand branch_class::code(operand expr_val, operand tag,
				op_type join_type, CgenEnvironment *env) {
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

// Assign this attribute a slot in the class structure
void attr_class::layout_feature(CgenNode *cls)
{
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE
#endif
}

void attr_class::code(CgenEnvironment *env)
{
#ifndef PA5
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE
#endif
}

