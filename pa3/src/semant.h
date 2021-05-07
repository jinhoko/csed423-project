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
  void check_entrypoint(); // TODO when?

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

  // all error functions
  // TODO check error messages
  void printerr_prevdef( Class_ c1, Symbol c2 );
  void printerr_redefine_reserved( Class_ c1, Symbol c2);
  void printerr_inherit_base( Class_ c1, Symbol c2, Symbol c3);
  void printerr_isdangling( Class_ c1, Symbol c2, Symbol c3 );
  void printerr_cyclefound( Class_ c1, Symbol c2);

  void printerr_main_method_not_exists( Class_ c1 );
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

};


#endif

