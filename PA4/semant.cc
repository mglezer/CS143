

#include <list>
#include <set>
#include <typeinfo>
#include <string>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */

    // Create a map of class names to classes for easy lookup, and make sure no classes are defined
    // multiple times.
    // 
    // For convenience, insert built-in classes into the name map.
    class__class* object_class = new class__class(Object, NULL, nil_Features(), NULL);
    class__class *string_class = new class__class(Str, NULL, nil_Features(), NULL);
    class__class *bool_class = new class__class(Bool, NULL, nil_Features(), NULL);
    class__class *int_class = new class__class(Int, NULL, nil_Features(), NULL);
    class__class *io_class = new class__class(IO, NULL, nil_Features(), NULL);
    class_by_name.insert(std::pair(Object, object_class));
    class_by_name.insert(std::pair(Str, string_class));
    class_by_name.insert(std::pair(Bool, bool_class));
    class_by_name.insert(std::pair(Int, int_class));
    class_by_name.insert(std::pair(IO, io_class));
    parent_graph.insert(std::pair(string_class, object_class));
    parent_graph.insert(std::pair(bool_class, object_class));
    parent_graph.insert(std::pair(int_class, object_class));
    parent_graph.insert(std::pair(io_class, object_class));
    child_graph.insert(std::pair(object_class, string_class));
    child_graph.insert(std::pair(object_class, bool_class));
    child_graph.insert(std::pair(object_class, int_class));
    child_graph.insert(std::pair(object_class, io_class));
    built_in_classes.insert(Object);
    built_in_classes.insert(Str);
    built_in_classes.insert(Bool);
    built_in_classes.insert(Int);
    built_in_classes.insert(IO);

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        // Unfortunately it seems we must cast to the subclass here.
        class__class *cls = dynamic_cast<class__class*>(classes->nth(i));
        Symbol parent = cls->get_parent();
        Symbol child = cls->get_name();

        // Step 1b: Make sure no classes inherit from Int, String or Bool.
        if (parent == Bool || parent == Int || parent == Str) {
            semant_error(cls) << "Class " << child << " cannot inherit class " << parent << "." << endl;
            return;
        }
         
        // Make sure no class is defined more than once.
        if (!class_by_name.insert(std::pair(child, cls)).second) {
            semant_error(cls) << "Class " << child << " was previously defined." << endl;
            return;
        }
    }

    // Build the inheritance graph and verify no parent classes are undefined.
    for (std::map<Symbol, class__class *>::iterator it = class_by_name.begin(); it != class_by_name.end(); ++it) {
        class__class *cls = it->second;
        if (cls->get_parent() == NULL) {
            continue;
        } else {
            if (cls->get_parent() != Object) {
                Symbol name = cls->get_name();
                Symbol parent_name = cls->get_parent();
                std::map<Symbol, class__class *>::iterator parent_iterator = class_by_name.find(parent_name);
                if (parent_iterator == class_by_name.end()) {
                    semant_error(cls) << "Class " << name << " inherits from an undefined class " << parent_name << "." << endl;
                }
                parent_graph.insert(std::pair(cls, parent_iterator->second));
                child_graph.insert(std::pair(parent_iterator->second, cls));
            } else {
                parent_graph.insert(std::pair(cls, object_class));
            }
        }
    }

    // Make sure there are no cycles in the inheritance graph.
    std::set<class__class *> visited;
    std::set<class__class *> cycle_nodes;
    for (std::map<class__class *, class__class *>::iterator it = parent_graph.begin(); it != parent_graph.end(); ++it) {
        class__class *current_class = it->first;
        if (!visited.insert(current_class).second) {
            continue;
        }
        std::set<class__class*> visiting;
        visiting.insert(current_class);
        this->find_cycle_in_subgraph(current_class, parent_graph, visited, visiting, cycle_nodes);
        visiting.erase(current_class);
        visited.insert(current_class);
    }

    if (cycle_nodes.size() > 0) {
        for (class__class *cls : cycle_nodes) {
            Symbol name = cls->get_name();
            semant_error(cls) << "Class " << name << ", or an ancestor of " << name << ", is involved in an inheritance cycle." << endl;
        }
        return;
    }

    // Validate that a Main class exists and that it has a main method.
    std::map<Symbol, class__class*>::iterator main_it = class_by_name.find(Main);
    if (main_it == class_by_name.end()) {
        semant_error() << "Class Main is not defined." << endl;
        return;
    }

    class__class* main_class = main_it->second;
    Features features = main_class->get_features();
    bool main_method_found = false;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature_class *feat = features->nth(i);
        if (typeid(*feat) == typeid(method_class)) {
            method_class* method = dynamic_cast<method_class *>(feat);
            if (method->get_name() == main_meth) {
                main_method_found = true;
                break;
            }
        }
    }
    if (!main_method_found) {
        semant_error(main_class) << "No 'main' method in class Main." << endl;
        return;
    }


    // Gather all user-defined classes at the top of the hierarchy (i.e. which inherit directly from
    // Object). Each child class will inherit the top-level scope of its parent, i.e. its fields and
    // methods.
    std::list<class__class *> top_level_classes;
    for (const auto &pair : parent_graph) {
        bool is_user_defined = built_in_classes.find(pair.first->get_name()) == built_in_classes.end();
        bool inherits_from_object = pair.second == object_class;
        if (inherits_from_object && is_user_defined) {
            top_level_classes.push_back(pair.first);
        }
    }
    for (const auto &cls : top_level_classes) {
        type_check_class(cls);
    }
}

void ClassTable::type_check_class(class__class *cls) {
    method_table.enterscope();
    object_table.enterscope();
    active_class = cls;

    // First add all features to the scope.
    Features features = cls->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature_class *feat = features->nth(i);
        if (typeid(*feat) == typeid(attr_class)) {
            attr_class* attr = dynamic_cast<attr_class *>(feat);
            Symbol declared_type = attr->get_type_decl();
            object_table.addid(attr->get_name(), declared_type);
        } else if (typeid(*feat) == typeid(method_class)) {
            method_class* method = dynamic_cast<method_class *>(feat);
            method_table.addid(method->get_name(), method);
        } else {
            // Attributes and methods are the only possible children of a class.
            assert(false);
        }
    }

    // Once the class-level scope is initialized, we can evaluate all attribute expressions.
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature_class *feat = features->nth(i);
        if (typeid(*feat) == typeid(attr_class)) {
            attr_class* attr = dynamic_cast<attr_class *>(feat);
            Expression expr = attr->get_expression();
            if (typeid(*attr->get_expression()) != typeid(no_expr_class)) {
                Symbol declared_type = attr->get_type_decl();
                Symbol inferred_type = expr->check_type(this);
                if (!is_subtype(inferred_type, declared_type)) {
                    semant_error(cls->get_filename(), feat) << "Inferred type " << inferred_type << " of initialization of attribute " << attr->get_name() << " does not conform to declared type " << declared_type << "." << endl;
                } else {
                    expr->set_type(inferred_type);
                }
            }
        } else if (typeid(*feat) == typeid(method_class)) {
            method_class* method = dynamic_cast<method_class *>(feat);
            Expression expr = method->get_expression();
            if (typeid(*method->get_expression()) != typeid(no_expr_class)) {
                Symbol declared_type = method->get_return_type();
                Symbol inferred_type = expr->check_type(this);
                if (!is_subtype(inferred_type, declared_type)) {
                    semant_error(cls->get_filename(), feat) << "Inferred return type " << inferred_type << " of method " << method->get_name() << " does not conform to declared type " << declared_type << "." << endl;
                } else {
                    expr->set_type(declared_type);
                }
            }
        }
    }

    for (std::multimap<class__class *, class__class *>::iterator it = child_graph.find(cls); it != child_graph.end(); it++) {
        type_check_class(it->second);
    }

    method_table.exitscope();
    object_table.exitscope();
}

Symbol object_class::check_type(void *ptr) {
    ClassTable class_table = *(ClassTableP)ptr;
    Symbol sym = class_table.lookup_object(this->name);
    if (sym == NULL) {
        class_table.semant_error(class_table.get_active_class()->get_filename(), this) << "Undeclared identifier " << this->name << "." << endl;
        return Object;
    }
    return sym;
}

Symbol int_const_class::check_type(void *ptr) {
   return Int;
}

Symbol string_const_class::check_type(void *ptr) {
    return Str;
}

Symbol bool_const_class::check_type(void *ptr) {
   return Bool;
}

Symbol ClassTable::lookup_object(Symbol object) {
    return object_table.lookup(object);
}

method_class *ClassTable::lookup_method(Symbol method) {
    return method_table.lookup(method);
}

class__class *ClassTable::get_active_class() {
    return active_class;
}

bool ClassTable::is_subtype(Symbol class_name_b, Symbol class_name_a) {
    std::map<Symbol, class__class*>::iterator it_b = class_by_name.find(class_name_b);
    std::map<Symbol, class__class*>::iterator it_a = class_by_name.find(class_name_a);
    // Both classes should be valid names which exist in the map.
    assert(it_b != class_by_name.end());
    assert(it_a != class_by_name.end());
    class__class* class_b = it_b->second;
    class__class* class_a = it_a->second;

    class__class* curr = class_b;

    while (curr != NULL) {
        if (curr == class_a) {
            return true;
        }
        std::map<class__class *, class__class *>::iterator it = parent_graph.find(curr);
        curr = it == parent_graph.end() ? NULL : it->second;
    }

    return false;
}

void ClassTable::find_cycle_in_subgraph(
        class__class *start_node,
        std::map<class__class*, class__class*> graph,
        std::set<class__class*> &visited,
        std::set<class__class*> &visiting,
        std::set<class__class*> &cycle_nodes) {
    std::map<class__class*, class__class*>::iterator it = graph.find(start_node);
    if (it == graph.end()) {
        return;
    }
    class__class* parent = it->second;
    bool already_visiting = visiting.find(parent) != visiting.end();
    bool on_known_cycle = cycle_nodes.find(parent) != cycle_nodes.end();
    if (already_visiting || on_known_cycle) {
        for (class__class *cls : visiting) {
            cycle_nodes.insert(cls);
        }
        return;
    }
    bool already_visited = visited.find(parent) != visited.end();
    if (already_visited) {
        // This implies we have reached a part of the graph known to be cycle-free.
        return;
    }
    visiting.insert(parent);
    find_cycle_in_subgraph(parent, graph, visited, visiting, cycle_nodes);
    visiting.erase(parent);
    visited.insert(parent);
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
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
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


