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

class ClassTable : public TypeChecker {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  std::map<Symbol, Class_> class_by_name;
  std::map<Class_, Class_> parent_graph;
  std::multimap<Class_, Class_> child_graph;
  std::set<Symbol> built_in_classes;
  MethodTable method_table;
  ObjectTable object_table;
  Class_ active_class;
  Class_ Object_class;

  void find_cycle_in_subgraph(
          Class_ start_node,
          std::map<Class_, Class_> graph,
          std::set<Class_> &visited,
          std::set<Class_> &visiting,
          std::set<Class_> &cycle_nodes);

  Symbol get_type(Expression expression);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t) override ;
  Symbol lookup_object(Symbol object) override ;
  method_class *lookup_method(Symbol method) override ;
  Class_ get_active_class() override;
  void set_active_class(Class_ cls) override;
  MethodTable *get_method_table() override;
  ObjectTable *get_object_table() override ;
  std::multimap<Class_, Class_> *get_child_graph() override;
  bool is_subtype(Symbol clazz_b, Symbol clazz_a) override ;
  Symbol verify_arith(Expression expr, Symbol type, Symbol a, Symbol b, std::string op) override ;
  Symbol least_upper_bound(std::set<Symbol> nodes, Symbol current) override ;
  Symbol least_upper_bound(std::set<Symbol> nodes) override ;
  method_class *find_method(Symbol class_name, Symbol method_name) override ;
  Symbol validate_dispatch(Expression dispatch, Expression expr, Symbol static_type, Symbol method_name, Expressions arguments) override ;
};


#endif

