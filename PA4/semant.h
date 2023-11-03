#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <map>
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

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  std::map<Symbol, class__class *> class_by_name;
  std::map<class__class *, class__class *> parent_graph;
  std::multimap<class__class *, class__class *> child_graph;
  std::set<Symbol> built_in_classes;
  MethodTable method_table;
  ObjectTable object_table;
  class__class *active_class;

  void find_cycle_in_subgraph(
          class__class *start_node,
          std::map<class__class*, class__class*> graph,
          std::set<class__class*> &visited,
          std::set<class__class*> &visiting,
          std::set<class__class*> &cycle_nodes);

  bool is_subtype(Symbol clazz_b, Symbol clazz_a);

  void type_check_class(class__class *cls);
  Symbol get_type(Expression expression);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  Symbol lookup_object(Symbol object);
  method_class *lookup_method(Symbol method);
  class__class *get_active_class();
  Symbol verify_arith(Expression expr, Symbol type, Symbol a, Symbol b, std::string op);
  Symbol least_upper_bound(Symbol a, Symbol b, Symbol current);
  Symbol least_upper_bound(Symbol a, Symbol b);
};


#endif

