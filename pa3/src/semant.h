#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <map>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.


typedef int nodeIdx;
typedef int noData;

typedef cool::SymbolTable<Symbol, Entry> ObjectTable;
typedef cool::SymbolTable<Symbol, Feature_class> MethodTable;

typedef struct _OMTable {
    ObjectTable *ot;
    MethodTable *mt;

    _OMTable() {
      ot = new ObjectTable();
      ot->enterscope();
      mt = new MethodTable();
      mt->enterscope();
    }

    void enterscope() {
        ot->enterscope();
        mt->enterscope();
    }
    void exitscope() {
        ot->exitscope();
        mt->exitscope();
    }

    void add_attr( Symbol name, Symbol type ) {
      ot->addid( name, type );
    }
    void add_method( Symbol name, Feature f ) {
      mt->addid( name, f);
    }

} OMTable;

typedef std::map<Symbol, OMTable*> Env;

using namespace cool;


class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  /* custom */
  Classes user_classes;       // all list of classes during sement execution
  Classes base_classes;       // language_defined classes
  Classes program_classes;    // user_classes + language_classes
  bool is_class_exists_in_program ( Symbol s );
  bool is_class_base_class ( Symbol s );

  // check inheritance
  nodeIdx _latestNodeIdx;
  noData _garbageData;         // for symtable use without data
  noData* _garbage;
  SymbolTable<Symbol, noData> *valid_scope_symbols;
  SymbolTable<Symbol, noData> *invalid_inheritance_symbols;
  SymbolTable<Symbol, nodeIdx> *graph_nodes;
  SymbolTable<nodeIdx, nodeIdx> *graph_edges;

  void check_graph_node_build();
  void check_graph_edge_build();
  void check_inheritance_cycle();

  nodeIdx* get_new_nodeindex();
  int num_nodes();

  // check entrypoint
  void check_entrypoint();

  // check name_scope & fill symbol table
  Env environment;           // environment map
  SymbolTable<Symbol, Class__class> *class_map;     // mapping from symbol to class object
  SymbolTable<Symbol, noData> error_checked;          // check if the errors of the class is checked

  Class_ get_class( Symbol s );
  void init_symboltable();
  void check_name_scope();
  void check_class_symboltable_build( Class_ c, Symbol target );
  void check_parent_symboltable_build( Symbol s_c, Symbol target );

  // check types
  void check_types();
  Symbol get_lub( Symbol t1, Symbol t2, Class_ c);
  bool is_poset( Symbol t1 , Symbol t2, Class_ c );

  // all error functions
  void printerr_prevdef( Class_ c1, Symbol c2 );
  void printerr_redefine_reserved( Class_ c1, Symbol c2);
  void printerr_inherit_base( Class_ c1, Symbol c2, Symbol c3);
  void printerr_isdangling( Class_ c1, Symbol c2, Symbol c3 );
  void printerr_cyclefound( Class_ c1, Symbol c2);

  void printerr_main_method_not_exists( Class_ c1 );
  void printerr_main_method_param_error( Class_ c1);
  void printerr_main_class_not_exists( );
  

  // due to recursive print_err, the following printerr functions will
  // omit error after checking error_checked
  bool _is_printerr_available( Class_ c1, Symbol target );
  void printerr_attr_name_self( Class_ c1, Feature f,Symbol target );
  void printerr_attr_type_undefined( Class_ c1, Feature f, Symbol type, Symbol name, Symbol target );
  void printerr_method_type_undefined( Class_ c1, Feature f, Symbol type, Symbol meth, Symbol target );
  void printerr_attr_multiple_defined( Class_ c1, Feature f, Symbol name, Symbol target);
  void printerr_method_multiple_defined( Class_ c1, Feature f, Symbol name, Symbol target );
  void printerr_attr_in_inherited_class( Class_ c1, Feature f, Symbol name, Symbol target );
  void printerr_method_redefined_typeerror (Class_ c1, Feature f, Symbol name, Symbol type, Symbol type2, Symbol target);
  void printerr_method_redefined_numargerror ( Class_ c1, Feature f, Symbol name, Symbol target);
  void printerr_method_paramtypeerror( Class_ c1, Feature f, Symbol name, Symbol type, Symbol type2, Symbol target );
  void printerr_method_multiple_formal( Class_ c1, Feature f, Symbol name, Symbol target );
  void printerr_method_formal_selftype( Class_ c1, Feature f, Symbol name, Symbol target   ) ;
  void printerr_method_formal_selfname( Class_ c1, Feature f, Symbol target );
  void printerr_method_formal_wrongtype( Class_ c1, Feature f, Symbol t, Symbol fname, Symbol target  );

  // errors when typechecking
  void printerr_arith_nonint( Class_ c1, Expression e, Symbol t1, Symbol t2 );
  void printerr_neg_nonint( Class_ c1, Expression e, Symbol t);
  void printerr_new_undefined( Class_ c1, Expression e, Symbol type );
  void printerr_eq_basictype( Class_ c1, Expression e  );
  void printerr_undeclared_id( Class_ c1, Expression e, Symbol name );
  void printerr_comp( Class_ c1, Expression e, Symbol type);
  void printerr_loop_notbool( Class_ c1, Expression e);
  void printerr_if_notbool( Class_ c1, Expression e);
  void printerr_assign_self( Class_ c1, Expression e);
  void printerr_assign_mismatch( Class_ c1, Expression e, Symbol t1, Symbol t2, Symbol name);
  void printerr_assign_undeclared( Class_ c1, Expression e, Symbol name);
  void printerr_let_self( Class_ c1, Expression e);
  void printerr_let_undefined( Class_ c1, Expression e, Symbol type, Symbol name);
  void printerr_let_mismatch( Class_ c1, Expression e, Symbol t1, Symbol name, Symbol t2);
  void printerr_dispatch_undefined_method( Class_ c1, Expression e, Symbol name);
  void printerr_dispatch_undefined( Class_ c1, Expression e, Symbol name);
  void printerr_dispatch_paramerror( Class_ c1, Expression e, Symbol name, Symbol t1, Symbol name2, Symbol t2);
  void printerr_dispatch_paramnumerror( Class_ c1, Expression e, Symbol name );
  void printerr_staticdispatch_selftype( Class_ c1, Expression e);
  void printerr_staticdispatch_undefined( Class_ c1, Expression e, Symbol name);
  void printerr_staticdispatch_typeerror( Class_ c1, Expression e, Symbol t1, Symbol t2);
  void printerr_case_self( Class_ c1, Case e);
  void printerr_case_selftype( Class_ c1, Case e, Symbol name);
  void printerr_case_duplicate( Class_ c1, Case e, Symbol type );
  void printerr_case_undefined( Class_ c1, Case e, Symbol name );
  void printerr_method_mismatch( Class_ c1, Feature f, Symbol t1, Symbol t2);
  void printerr_attr_mismatch( Class_ c1, Feature f, Symbol t1, Symbol t2 );
};


#endif

