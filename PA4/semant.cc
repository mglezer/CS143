

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
    class__class *self_class = new class__class(SELF_TYPE, NULL, nil_Features(), NULL);
    class_by_name.insert(std::pair(Object, object_class));
    class_by_name.insert(std::pair(Str, string_class));
    class_by_name.insert(std::pair(Bool, bool_class));
    class_by_name.insert(std::pair(Int, int_class));
    class_by_name.insert(std::pair(IO, io_class));
    class_by_name.insert(std::pair(SELF_TYPE, self_class));
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
                child_graph.insert(std::pair(object_class, cls));
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
    // We must enter a scope before initializing the object table.
    method_table.enterscope();
    object_table.enterscope();
    // Make self globally  available.
    object_table.addid(self, SELF_TYPE);
    for (const auto &cls : top_level_classes) {
        type_check_class(cls);
    }
}

void ClassTable::type_check_class(class__class *cls) {
    active_class = cls;

    // First add all features to the scope.
    Features features = cls->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature_class *feat = features->nth(i);
        if (typeid(*feat) == typeid(attr_class)) {
            attr_class* attr = dynamic_cast<attr_class *>(feat);
            Symbol declared_type = attr->get_type_decl();
            if (object_table.probe(attr->get_name()) != NULL) {
                semant_error(cls->get_filename(), feat) << "Attribute " << attr->get_name() << " is multiply defined in class." << endl;
                continue;
            } else if (object_table.lookup(attr->get_name()) != NULL) {
                semant_error(cls->get_filename(), feat) << "Attribute " << attr->get_name() << " is an attribute of an inherited class." << endl;
                continue;
            }
            object_table.addid(attr->get_name(), declared_type);
        } else if (typeid(*feat) == typeid(method_class)) {
            method_class* method = dynamic_cast<method_class *>(feat);
            if (method_table.probe(method->get_name()) != NULL) {
                semant_error(cls->get_filename(), feat) << "Method " << method->get_name() << " is multiply defined." << endl;
                continue;
            } else if (method_class *ancestor_method = method_table.lookup(method->get_name()); ancestor_method != NULL) {
                // The formals and return type need to be exactly the same.
                if (ancestor_method->get_return_type() != method->get_return_type()) {
                    semant_error(cls->get_filename(), feat) << "In redefined method " << method->get_name() << ", return type " << method->get_return_type() << " is different from original return type " << ancestor_method->get_return_type() << "." << endl;
                    continue;
                }
                Formals ancestor_formals = ancestor_method->get_formals();
                Formals child_formals = method->get_formals();
                if (ancestor_formals->len() != child_formals->len()) {
                    semant_error(cls->get_filename(), feat) << "Incompatible number of formal parameters in redefined method " << method->get_name() << endl;
                    continue;
                }
                for (int i = child_formals->first(); child_formals->more(i); i = child_formals->next(i)) {
                    // The name of the formal can differ, but the type cannot.
                    formal_class *ancestor_formal = dynamic_cast<formal_class *>(ancestor_formals->nth(i));
                    formal_class *child_formal = dynamic_cast<formal_class *>(child_formals->nth(i));
                    if (ancestor_formal->get_type_decl() != child_formal->get_type_decl()) {
                        semant_error(cls->get_filename(), feat) << "In redefined method " << method->get_name() << " parameter type " << child_formal->get_type_decl() << " is different from original type " << ancestor_formal->get_type_decl() << "." << endl;
                        continue;
                    }
                }
            }
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
                    expr->set_type(inferred_type);
                }
            }
        }
    }

    std::pair<std::multimap<class__class *, class__class *>::iterator, 
std::multimap<class__class *, class__class*>::iterator> ret = child_graph.equal_range(cls);
    for (std::multimap<class__class *, class__class *>::iterator it = ret.first; it != ret.second; ++it) {
        method_table.enterscope();
        object_table.enterscope();
        type_check_class(it->second);
        method_table.exitscope();
        object_table.exitscope();
    }

}

Symbol eq_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    Symbol t1 = e1->check_type(ptr);
    Symbol t2 = e2->check_type(ptr);
    if ((t1 == Str  || 
         t2 == Str  ||
         t1 == Bool ||
         t2 == Bool ||
         t1 == Int  ||
         t2 == Int) && t1 != t2) {
        class_table->semant_error(class_table->get_active_class()->get_filename(), this) << "Illegal comparison with a basic type." << endl;
    }
    this->set_type(Bool);
    return Bool;
}

Symbol loop_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    Symbol pred_type = pred->check_type(ptr);
    if (pred_type != Bool) {
        class_table->semant_error(class_table->get_active_class()->get_filename(), this) << "Loop condition does not have type Bool." << endl;
    }
    body->check_type(ptr); // The type of the loop is always Object so this value is ignored.
    this->set_type(Object);
    return Object;
}

Symbol cond_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    Symbol pred_type = pred->check_type(ptr);
    if (pred_type != Bool) {
        class_table->semant_error(class_table->get_active_class()->get_filename(), this) << "Predicate of 'if' does not have type Bool." << endl;
    }
    Symbol type = class_table->least_upper_bound(then_exp->check_type(ptr), else_exp->check_type(ptr));
    assert(type != NULL);
    this->set_type(type);
    return type;
}

Symbol ClassTable::least_upper_bound(Symbol a, Symbol b) {
    return least_upper_bound(a, b, Object);
}

Symbol ClassTable::least_upper_bound(Symbol a, Symbol b, Symbol current) {
    if (current == NULL) {
        return NULL;
    }
    if (current == a) {
        return a;
    }
    if (current == b) {
        return b;
    }
    std::list<Symbol> ancestors;
    std::map<Symbol, class__class*>::iterator it = class_by_name.find(current);
    assert(it != class_by_name.end());
    class__class* cls = it->second;
    std::pair<std::multimap<class__class *, class__class *>::iterator, 
std::multimap<class__class *, class__class*>::iterator> ret = child_graph.equal_range(cls);
    for (std::multimap<class__class *, class__class *>::iterator it = ret.first; it != ret.second; ++it) {
        Symbol candidate = least_upper_bound(a, b, it->second->get_name());
        if (candidate != NULL) {
            ancestors.push_back(candidate);
        }
    }
    assert(ancestors.size() <= 2);
    if (ancestors.size() == 2) {
        return current;
    } else if (ancestors.size() == 1) {
        return ancestors.front();
    } else {
        return NULL;
    }
}

Symbol object_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    Symbol sym = class_table->lookup_object(this->name);
    if (sym == NULL) {
        class_table->semant_error(class_table->get_active_class()->get_filename(), this) << "Undeclared identifier " << this->name << "." << endl;
        return Object;
    }
    set_type(sym);
    return sym;
}

Symbol isvoid_class::check_type(void *ptr) {
    set_type(Bool);
    return Bool;
}

Symbol comp_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    Symbol type = e1->check_type(ptr);
    if (type != Bool) {
        class_table->semant_error(class_table->get_active_class()->get_filename(), this) << "Argument of not has type " << type << " instead of Bool." << endl;
    }
    set_type(Bool);
    return Bool;
}


Symbol neg_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    Symbol type = e1->check_type(ptr);
    if (type != Int) {
        class_table->semant_error(class_table->get_active_class()->get_filename(), this) << "Argument of '~' has type " << type << " instead of Int." << endl;
    }
    set_type(Int);
    return Int;
}

Symbol lt_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    return class_table->verify_arith(this, Bool, e1->check_type(ptr), e2->check_type(ptr), "<");
}

Symbol leq_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    return class_table->verify_arith(this, Bool, e1->check_type(ptr), e2->check_type(ptr), "<=");
}

Symbol plus_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    return class_table->verify_arith(this, Int, e1->check_type(ptr), e2->check_type(ptr), "+");
}

Symbol sub_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    return class_table->verify_arith(this, Int, e1->check_type(ptr), e2->check_type(ptr), "-");
}

Symbol mul_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    return class_table->verify_arith(this, Int, e1->check_type(ptr), e2->check_type(ptr), "*");
}

Symbol divide_class::check_type(void *ptr) {
    ClassTable *class_table = (ClassTable *)ptr;
    return class_table->verify_arith(this, Int, e1->check_type(ptr), e2->check_type(ptr), "/");
}

Symbol ClassTable::verify_arith(Expression expr, Symbol type, Symbol t1, Symbol t2, std::string op) {
    if (t1 != Int || t2 != Int) {
        semant_error(get_active_class()->get_filename(), expr) << "non-Int arguments: " << t1 << " " << op << " " << t2 << endl;
    }
    expr->set_type(type);
    return type;
}

Symbol block_class::check_type(void *ptr) {
    Symbol last_type;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        last_type = body->nth(i)->check_type(ptr);
    }
    set_type(last_type);
    return last_type;
}

Symbol new__class::check_type(void *ptr) {
    set_type(type_name);
    return type_name;
}

Symbol int_const_class::check_type(void *ptr) {
   set_type(Int);
   return Int;
}

Symbol string_const_class::check_type(void *ptr) {
    set_type(Str);
    return Str;
}

Symbol bool_const_class::check_type(void *ptr) {
    set_type(Bool);
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
    assert(class_name_b != NULL);
    assert(class_name_a != NULL);
    if (class_name_b == class_name_a) {
        return true;
    }
    // SELF_TYPE_b <= a iff b <= a, so we can resolve SELF_TYPE to the active class.
    if (class_name_b == SELF_TYPE) {
        class_name_b = active_class->get_name();
    }
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


