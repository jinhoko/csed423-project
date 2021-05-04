

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <vector>

extern int semant_debug;
extern char *curr_filename;

using namespace cool;

// finish definition of methods declared in cool-tree.handcode.h
Symbol class__class::get_filename() { return filename; }
Symbol class__class::get_name() { return name; }
Symbol class__class::get_parent() { return parent; }

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    base_classes =
        append_Classes(
            append_Classes(
                append_Classes(
                    append_Classes(
                        single_Classes(Object_class),
                        single_Classes(IO_class)
                    ),
                    single_Classes(Int_class)
                ),
                single_Classes(Bool_class)
            ),
            single_Classes(Str_class)
        );

    return;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

////////////////////////////////////////////////////////////////////
//
// Semantic error printer
//
///////////////////////////////////////////////////////////////////

void ClassTable::printerr_prevdef( Class_ c1, Symbol c2 ) {
    semant_error( c1 ) << "Class " << c2->get_string() <<  " was previously defined.\n";
}
void ClassTable::printerr_redefine_reserved( Class_ c1, Symbol c2 ) {
    semant_error( c1 ) << "Redefinition of basic class " << c2->get_string() << ".\n";
}
void ClassTable::printerr_inherit_base( Class_ c1, Symbol c2, Symbol c3) {
    semant_error( c1 ) << "Class " << c2->get_string() << " cannot inherit class " << c3->get_string() << ".\n";
}
void ClassTable::printerr_isdangling( Class_ c1, Symbol c2, Symbol c3 ) {
    semant_error( c1 ) << "Class " << c2->get_string() <<  " inherits from an undefined class " << c3->get_string() <<  ".\n";
};
void ClassTable::printerr_cyclefound( Class_ c1, Symbol c2) {
    semant_error( c1 ) << "Class " << c2->get_string() << ", or an ancestor of " << c2->get_string() << ", is involved in an inheritance cycle.\n";
}

////////////////////////////////////////////////////////////////////
//
// Extending ClassTable function definitions
//
///////////////////////////////////////////////////////////////////

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    // Install basic classes to language_classes
    install_basic_classes();
    // Install user_classes
    user_classes = classes;
    
    // Initialize others
    latestNodeIdx = 0;

    // table for duplicate check
    valid_scope_symbols = new SymbolTable<Symbol, Class__class>();
    valid_scope_symbols->enterscope();

    // table for invalid inheritance
    invalid_inheritance_symbols = new SymbolTable<Symbol, Class__class>();
    invalid_inheritance_symbols->enterscope();

    // tables for inheritance graph (maintain inverted table as well; for printing error )
    graph_nodes = new SymbolTable<Symbol, nodeIdx>();
    graph_nodes->enterscope();
    nodeIdx *_tmp;
    graph_nodes->addid( Object,  get_new_nodeindex() ); 
    graph_nodes->addid( IO,  get_new_nodeindex() ); 
    graph_nodes->addid( SELF_TYPE,  get_new_nodeindex() ); 
    graph_nodes->addid( Int,  get_new_nodeindex() );
    graph_nodes->addid( Bool,  get_new_nodeindex() );
    graph_nodes->addid( Str,  get_new_nodeindex() );
    
    graph_edges = new SymbolTable<nodeIdx, nodeIdx>();
    graph_edges->enterscope();

}

nodeIdx* ClassTable::get_new_nodeindex() {
    return new nodeIdx(++latestNodeIdx); // generate index object every time
}
int ClassTable::num_nodes() { return latestNodeIdx; }

void ClassTable::check_graph_node_build() {

    // Checks following in first pass: 
    //   1. no duplicated definition of user class
    //   2. not redefinition of base classes
    //   3. user class not inheriting from base class
    //   => build nodes of inheritance graph

    // first pass
    int idx;
    Class_ _class;
    Symbol _class_name, _class_parent;
    bool isAlreadyInScope;
    bool isRedefiningReserved;
    bool isSelfType;
    bool isInheritingFromBaseClasses;

    for( idx = user_classes->first();
        user_classes->more(idx);
        idx = user_classes->next(idx) ) {

        _class = user_classes->nth(idx);
        _class_name = _class->get_name();
        _class_parent = _class->get_parent();

        isAlreadyInScope =
            valid_scope_symbols->lookup(_class_name) != NULL;
        isRedefiningReserved = 
            _class_name == Object || _class_name == No_class || _class_name == SELF_TYPE ||
            _class_name == Int || _class_name == Bool || _class_name == Str || _class_name == IO;
        isInheritingFromBaseClasses =
            _class_parent == Int || _class_parent == Bool || _class_parent == Str || _class_parent == SELF_TYPE;
        
        if( isAlreadyInScope ) {
            printerr_prevdef( _class, _class_name );
        } else if ( isRedefiningReserved ) {
            printerr_redefine_reserved( _class, _class_name );
            invalid_inheritance_symbols->addid(_class_name, _class);
        } else if ( isInheritingFromBaseClasses ) {
            printerr_inherit_base( _class, _class_name, _class_parent );
            valid_scope_symbols->addid(_class_name, _class);
            invalid_inheritance_symbols->addid(_class_name, _class);
            graph_nodes->addid( _class_name, get_new_nodeindex() );
        } else { // ok
            valid_scope_symbols->addid(_class_name, _class);
            graph_nodes->addid( _class_name, get_new_nodeindex() );
        }
    }
    

} 

void ClassTable::check_graph_edge_build() {

    // Checks following in second pass: 
    //   1. check dangling classes
    //   => build edges of inheritance graph
    //

    // second pass
    int idx;
    Class_ _class;
    Symbol _class_name, _class_parent;

    bool isClassDangling;
    bool isNodeWithInvalidInheritance;
    nodeIdx *parent_idx, *child_idx;
    for( idx = user_classes->first();
        user_classes->more(idx);
        idx = user_classes->next(idx) ) {

        _class = user_classes->nth(idx);
        _class_name = _class->get_name();
        _class_parent = _class->get_parent();

        isClassDangling =
            graph_nodes->lookup( _class_parent ) == NULL;
        
        isNodeWithInvalidInheritance = 
            invalid_inheritance_symbols->lookup( _class_name ) != NULL;
        if( isNodeWithInvalidInheritance ) { continue; }
    
        if( isClassDangling ) {
            printerr_isdangling( _class, _class_name, _class_parent );
        } else { // ok
            child_idx = graph_nodes->lookup( _class_name );
            parent_idx = graph_nodes->lookup( _class_parent );
            graph_edges->addid( *child_idx , parent_idx );
        }
    }

}

void ClassTable::check_inheritance_cycle() {

    // check cycles
    // (since each node has one outgoing edge, deploy naive algorithm.)

    int idx;
    Class_ _class;
    Symbol _class_name, _class_parent;
    nodeIdx *startIdx, *parent_idx;
    nodeIdx currIdx, parentIdx;

    for( idx = user_classes->first();
        user_classes->more(idx);
        idx = user_classes->next(idx) ) {
        
        _class = user_classes->nth(idx);
        _class_name = _class->get_name();

        startIdx = graph_nodes->lookup(_class_name);
        if( startIdx == NULL ) { continue; }
        
        currIdx = *startIdx;                                     // start from startIdx
        std::vector<bool> visited(num_nodes() + 1, false);       // use index 1~n
        do {
            visited[currIdx] = true;
            parent_idx = graph_edges->lookup( currIdx );         // fetch index value
            if( parent_idx == NULL ) { break; }                  // ok because idx starts from 1

            parentIdx = *parent_idx;
            if( visited[parentIdx] ) {                           // cycle found
                printerr_cyclefound( _class, _class_name );
                break;
            }
            currIdx = parentIdx;
        } while(1);
    }

}

void ClassTable::check_name_scope() {


}

void ClassTable::check_entrypoint() {

    bool isMainClassExists = true; // TODO change condition
    bool isMainMethodDefined = true;
    if( ! isMainClassExists ) {
        semant_error() << "Class Main is not defined.\n";
    } else {
        // TODO if Main defined, check if main defined
    }
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */

void halt_if_error(ClassTable* classtable) {
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

void program_class::semant()
{
    initialize_constants();

    // ===================
    // VERIFICATION UNIT 1
    // ===================

    // Generate classtable by constructor call
    ClassTable *classtable = new ClassTable(classes);
    // Build inheritance graph & Check inheritance validity
    classtable->check_graph_node_build();
    classtable->check_graph_edge_build();
    halt_if_error(classtable);
    classtable->check_inheritance_cycle();
    halt_if_error(classtable);

    // ===================
    // VERIFICATION UNIT 2
    // ===================

    // After inheritance graph is checked, merge user_classes and language_classes
    classtable->program_classes = 
        append_Classes( classtable->base_classes, classtable->user_classes );
    // Check program entrypoint
    classtable->check_entrypoint();

    // Fill symbol table

    // Scopechecking
    classtable->check_name_scope();
    
    // Typechecking


    // Return & Terminate
    halt_if_error(classtable);
    return;
}


// Scopechecking-related functions

// Typechecking-related functions

