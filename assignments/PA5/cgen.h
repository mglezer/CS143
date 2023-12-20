#include <assert.h>
#include <stdio.h>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class MethodIdxTable : public SymbolTable<Symbol, int> {};
class MethodImplTable : public SymbolTable<Symbol, std::string> {};
class CgenClassTable : public SymbolTable<Symbol,CgenNode>, public ExpressionHelper {

private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   MethodIdxTable method_indices;
   MethodImplTable method_impls;
   VariableScope variable_scope;
   List<Entry> *attr_list;
   List<Entry> *method_list;
   std::vector<CgenNode *> class_tag_table;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void determine_offsets();
   void determine_offsets(CgenNodeP curr, int starting_method_index, int starting_attr_offset);
   void generate_dispatch_tables();
   void generate_proto_objects();
   void assign_class_tags();
   void generate_class_name_table();
   void generate_class_object_table();
   void generate_init_methods();
   void generate_init_method(CgenNode *cls);
   void generate_class_methods();
   CgenNode *find_class(Symbol class_name);

   void assign_next_class_tag(CgenNode *curr);
   void assign_nonbasic_class_tags(CgenNode *curr);
   void assign_basic_class_tags();

public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   int get_method_index(VariableScope &scope, Symbol static_type, Symbol method_name);
   int get_class_tag(Symbol class_name);
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int next_method_index = 0;
   int next_attr_offset = 0;
   int tag_number;

   MethodIdxTable method_indices;
   MethodImplTable method_impls;
   VariableScope variable_scope;
   List<Entry> *attr_list;
   List<Entry> *method_list;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   std::pair<int, int> determine_offsets(MethodIdxTable *method_indices, MethodImplTable *method_impls, VariableScope *variable_scope, List<Entry> **attr_list, List<Entry> **method_list, int starting_method_index, int starting_attr_offset);
   int get_and_increment_method_index() { return next_method_index++; }
   int get_and_increment_attr_offset() { return next_attr_offset++; }
   void generate_dispatch_table(ostream &s);
   void set_tag_number(int n) {
       tag_number = n;
   }
   int get_tag_number() {
       return tag_number;
   }

   VariableScope get_variable_scope() {
       return variable_scope;
   }
   List<Entry> *get_attr_list() {
       return attr_list;
   }
   int get_method_idx(Symbol method) {
       int *val = method_indices.lookup(method);
       assert(val != NULL);
       return *val;
   }

   void generate_class_methods(CgenClassTable *table, ostream &str);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

