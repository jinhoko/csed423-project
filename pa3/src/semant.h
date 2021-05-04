#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

typedef int nodeIdx;

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
  Classes language_classes;   // language_defined classes
  Classes program_classes;    // user_classes + language_classes
  
  // inheritance
  void check_inheritance();
  void is_inheritance_graph_cyclic();
  nodeIdx latestNodeIdx;
  nodeIdx* get_new_nodeindex();
  int num_nodes();

  // entrypoint
  void check_entrypoint();


  // all error functions
  void printerr_prevdef( Class_ c1, Symbol c2 );
  void printerr_redefine_reserved( Class_ c1, Symbol c2);
  void printerr_inherit_base( Class_ c1, Symbol c2, Symbol c3);
  void printerr_isdangling( Class_ c1, Symbol c2, Symbol c3 );
  void printerr_cyclefound( Class_ c1, Symbol c2);

};


#endif

