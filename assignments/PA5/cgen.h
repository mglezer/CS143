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

class ClassTag {
    private:
        std::vector<Symbol> class_names;
        int curr_tag = 5;

    public:
        static const int OBJECT = 0;
        static const int IO = 1;
        static const int INT = 2;
        static const int BOOL = 3;
        static const int STRING = 4;

        int get_new_tag(Symbol class_name) {
            // Assign then increment.
            class_names[curr_tag] = class_name;
            return curr_tag++;
        }

        std::vector<Symbol> *get_class_names() {
            return &class_names;
        }
};

class MethodIdxTable : public SymbolTable<Symbol, int> {};
class MethodImplTable : public SymbolTable<Symbol, std::string> {};
class AttributeTable : public SymbolTable<Symbol, int> {};
class CgenClassTable : public SymbolTable<Symbol,CgenNode> {

private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   MethodIdxTable method_indices;
   MethodImplTable method_impls;
   AttributeTable attr_offsets;


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
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int next_method_index = 0;
   int next_attr_offset = 0;

   MethodIdxTable method_indices;
   MethodImplTable method_impls;
   AttributeTable attr_offsets;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   std::pair<int, int> determine_offsets(MethodIdxTable *method_indices, MethodImplTable *method_impls, AttributeTable *attributeTable, int starting_method_index, int starting_attr_offset);
   int get_and_increment_method_index() { return next_method_index++; }
   int get_and_increment_attr_offset() { int val = next_attr_offset; next_attr_offset += 4; return val; }
   void generate_dispatch_table(ostream &s);
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

