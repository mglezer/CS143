

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
    install_basic_classes();

    // Create a map of class names to classes for easy lookup, and make sure no classes are defined
    // multiple times.
    // 
    // For convenience, insert built-in classes into the name map.

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        // Unfortunately it seems we must cast to the subclass here.
        Class_ cls = classes->nth(i);
        Symbol parent = cls->get_parent();
        Symbol child = cls->get_name();

        // Step 1b: Make sure no classes inherit from Int, String or Bool.
        if (parent == Bool || parent == Int || parent == Str || parent == SELF_TYPE) {
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
    for (std::map<Symbol, Class_>::iterator it = class_by_name.begin(); it != class_by_name.end(); ++it) {
        Class_ cls = it->second;
        if (cls->get_parent() == No_class) {
            continue;
        } else {
            if (cls->get_parent() != Object) {
                Symbol name = cls->get_name();
                Symbol parent_name = cls->get_parent();
                std::map<Symbol, Class_>::iterator parent_iterator = class_by_name.find(parent_name);
                if (parent_iterator == class_by_name.end()) {
                    semant_error(cls) << "Class " << name << " inherits from an undefined class " << parent_name << "." << endl;
                    return;
                } else {
                    parent_graph.insert(std::pair(cls, parent_iterator->second));
                    child_graph.insert(std::pair(parent_iterator->second, cls));
                }
            } else {
                child_graph.insert(std::pair(Object_class, cls));
                parent_graph.insert(std::pair(cls, Object_class));
            }
        }
    }

    // Make sure there are no cycles in the inheritance graph.
    std::set<Class_> visited;
    std::set<Class_> cycle_nodes;
    for (std::map<Class_, Class_>::iterator it = parent_graph.begin(); it != parent_graph.end(); ++it) {
        Class_ current_class = it->first;
        if (!visited.insert(current_class).second) {
            continue;
        }
        std::set<Class_> visiting;
        visiting.insert(current_class);
        this->find_cycle_in_subgraph(current_class, parent_graph, visited, visiting, cycle_nodes);
        visiting.erase(current_class);
        visited.insert(current_class);
    }

    if (cycle_nodes.size() > 0) {
        for (Class_ cls : cycle_nodes) {
            Symbol name = cls->get_name();
            semant_error(cls) << "Class " << name << ", or an ancestor of " << name << ", is involved in an inheritance cycle." << endl;
        }
        return;
    }

    // Validate that a Main class exists and that it has a main method.
    std::map<Symbol, Class_>::iterator main_it = class_by_name.find(Main);
    if (main_it == class_by_name.end()) {
        semant_error() << "Class Main is not defined." << endl;
        return;
    }

    Class_ main_class = main_it->second;
    Features features = main_class->get_features();
    bool main_method_found = false;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feat = features->nth(i);
        if (feat->is_method() && feat->get_name() == main_meth) {
            main_method_found = true;
            break;
        }
    }
    if (!main_method_found) {
        semant_error(main_class) << "No 'main' method in class Main." << endl;
        return;
    }


    // Gather all user-defined classes at the top of the hierarchy (i.e. which inherit directly from
    // Object). Each child class will inherit the top-level scope of its parent, i.e. its fields and
    // methods.
    std::list<Class_> top_level_classes;
    for (const auto &pair : parent_graph) {
        bool inherits_from_user_defined_class = built_in_classes.find(pair.second->get_name()) == built_in_classes.end();
        bool is_user_defined_class = built_in_classes.find(pair.first->get_name()) == built_in_classes.end();
        if (is_user_defined_class && !inherits_from_user_defined_class) {
            top_level_classes.push_back(pair.first);
        }
    }

    // We must enter a scope before initializing the object table.
    // Make self globally  available.
    for (const auto &cls : top_level_classes) {
        method_table.enterscope();
        object_table.enterscope();
        object_table.addid(self, SELF_TYPE);
        cls->type_check(this);
        method_table.exitscope();
        object_table.exitscope();
    }
}

void Class__class::type_check(TypeChecker *type_checker) {
    Class_ old_class = type_checker->get_active_class();
    type_checker->set_active_class(this);
    Features features = get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        feature->observe_feature(type_checker);
    }
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        feature->check_feature(type_checker);
    }
    std::pair<std::multimap<Class_, Class_>::iterator, 
std::multimap<Class_, Class_>::iterator> ret = type_checker->get_child_graph()->equal_range(this);
    MethodTable *method_table = type_checker->get_method_table();
    ObjectTable *object_table = type_checker->get_object_table();
    for (std::multimap<Class_, Class_>::iterator it = ret.first; it != ret.second; ++it) {
        Class_ child_class = it->second;
        method_table->enterscope();
        object_table->enterscope();
        child_class->type_check(type_checker);
        method_table->exitscope();
        object_table->exitscope();
    }
    type_checker->set_active_class(old_class);
}


void attr_class::observe_feature(TypeChecker *type_checker) {
    Symbol declared_type = this->get_type_decl();
    Class_ cls = type_checker->get_active_class();
    ObjectTable *object_table = type_checker->get_object_table();
    if (object_table->probe(this->get_name()) != NULL) {
        type_checker->semant_error(cls->get_filename(), this) << "Attribute " << this->get_name() << " is multiply defined in class." << endl;
        return;
    } else if (object_table->lookup(this->get_name()) != NULL) {
        type_checker->semant_error(cls->get_filename(), this) << "Attribute " << this->get_name() << " is an attribute of an inherited class." << endl;
        return;
    }
    object_table->addid(this->get_name(), declared_type);
}

void attr_class::check_feature(TypeChecker *type_checker) {
    Expression expr = this->get_expression();
    Class_ cls = type_checker->get_active_class();
    if (!this->get_expression()->is_empty()) {
        Symbol declared_type = this->get_type_decl();
        Symbol inferred_type = expr->check_type(type_checker);
        if (type_checker->get_class_by_name()->count(declared_type) == 0) {
            type_checker->semant_error(cls->get_filename(), this) << "Class " << declared_type << " of attribute " << this->get_name() << " is undefined." << endl;
        }
        if (!type_checker->is_subtype(inferred_type, declared_type)) {
            type_checker->semant_error(cls->get_filename(), this) << "Inferred type " << inferred_type << " of initialization of attribute " << this->get_name() << " does not conform to declared type " << declared_type << "." << endl;
        } else {
            expr->set_type(inferred_type);
        }
    }
}

void method_class::observe_feature(TypeChecker *type_checker) {
    MethodTable *method_table = type_checker->get_method_table();
    Class_ cls = type_checker->get_active_class();
    if (method_table->probe(this->get_name()) != NULL) {
        type_checker->semant_error(cls->get_filename(), this) << "Method " << this->get_name() << " is multiply defined." << endl;
        return;
    } else if (method_class *ancestor_method = method_table->lookup(this->get_name()); ancestor_method != NULL) {
        // The formals and return type need to be exactly the same.
        if (ancestor_method->get_return_type() != this->get_return_type()) {
            type_checker->semant_error(cls->get_filename(), this) << "In redefined method " << this->get_name() << ", return type " << this->get_return_type() << " is different from original return type " << ancestor_method->get_return_type() << "." << endl;
            return;
        }
        Formals ancestor_formals = ancestor_method->get_formals();
        Formals child_formals = this->get_formals();
        if (ancestor_formals->len() != child_formals->len()) {
            type_checker->semant_error(cls->get_filename(), this) << "Incompatible number of formal parameters in redefined method " << this->get_name() << "." << endl;
            return;
        }
        for (int i = child_formals->first(); child_formals->more(i); i = child_formals->next(i)) {
            // The name of the formal can differ, but the type cannot.
            Formal ancestor_formal = ancestor_formals->nth(i);
            Formal child_formal = child_formals->nth(i);
            if (ancestor_formal->get_type_decl() != child_formal->get_type_decl()) {
                type_checker->semant_error(cls->get_filename(), this) << "In redefined method " << this->get_name() << ", parameter type " << child_formal->get_type_decl() << " is different from original type " << ancestor_formal->get_type_decl() <<  endl;
                return;
            }
        }
    }
    method_table->addid(this->get_name(), this);
}

void method_class::check_feature(TypeChecker *type_checker) {
    Expression expr = this->get_expression();
    ObjectTable *object_table = type_checker->get_object_table();
    if (!this->get_expression()->is_empty()) {
        object_table->enterscope();
        Formals formals = this->get_formals();
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Formal formal = formals->nth(i); 
            if (formal->get_name() == self) {
                type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "'self' cannot be the name of a formal parameter." << endl;
                continue;
            }
            if (formal->get_type_decl() == SELF_TYPE) {
                type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Formal parameter " << formal->get_name() << " cannot have type " << SELF_TYPE << "."  << endl;
            }
            if (object_table->probe(formal->get_name()) != NULL) {
                type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Formal parameter " << formal->get_name() << " is multiply defined." << endl;
            }
            if (type_checker->get_class_by_name()->count(formal->get_type_decl()) == 0) {
                type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Class " << formal->get_type_decl() << " of formal parameter " << formal->get_name() << " is undefined." << endl;
            }
            object_table->addid(formal->get_name(), formal->get_type_decl());
        }
        Symbol declared_type = this->get_return_type();
        std::map<Symbol, Class_> *class_by_name = type_checker->get_class_by_name();
        bool undefined_return_type = false;
        if (class_by_name->find(declared_type) == class_by_name->end()) {
            type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Undefined return type " << declared_type << " in method " << this->get_name() << "." << endl;
            undefined_return_type = true;
        }
        Symbol inferred_type = expr->check_type(type_checker);
        if (!undefined_return_type && !type_checker->is_subtype(inferred_type, declared_type)) {
            type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Inferred return type " << inferred_type << " of method " << this->get_name() << " does not conform to declared return type " << declared_type << "." << endl;
        } 
        expr->set_type(inferred_type);
        object_table->exitscope();
    }
}

Symbol dispatch_class::check_type(TypeChecker *type_checker) {
    Symbol type = type_checker->validate_dispatch(this, expr, NULL, name, actual);
    set_type(type);
    return type;
}

Symbol static_dispatch_class::check_type(TypeChecker *type_checker) {
    Symbol type = type_checker->validate_dispatch(this, expr, type_name, name, actual);
    set_type(type);
    return type;
}

Symbol ClassTable::validate_dispatch(Expression dispatch, Expression expr, Symbol static_type, Symbol method_name, Expressions arguments) {

    Symbol expression_type = expr->check_type(this);
    bool is_static = static_type != NULL;
    Symbol class_name;
    if (is_static) {
        if (!is_subtype(expression_type, static_type)) {
            semant_error(active_class->get_filename(), dispatch) << "Expression type " << expression_type << " does not conform to declared static dispatch type " << static_type << "." << endl;
            return Object;
        }
        class_name = static_type;
    } else {
        // First get the type of the base expression.
        class_name = expr->is_empty() ? get_active_class()->get_name() : expression_type;
    }
    if (class_name == SELF_TYPE) {
        class_name = get_active_class()->get_name();
    }
    Class_ cls = get_active_class();
    if (get_class_by_name()->count(class_name) == 0) {
        semant_error(cls->get_filename(), dispatch) << std::string(is_static ? "Static dispatch " : "Dispatch ") << "on undefined class " << class_name << "." << endl;
        return Object;
    }
    method_class *method = find_method(class_name, method_name);
    if (method == NULL) {
        // The method does not exist.
        semant_error(cls->get_filename(), dispatch) << std::string(is_static ? "Static dispatch " : "Dispatch ") << "to undefined method " << method_name << "." << endl;
        return Object;
    }
    Formals formals = method->get_formals();
    if (formals->len() != arguments->len()) {
        semant_error(cls->get_filename(), dispatch) << "Method " << method_name << std::string(is_static ? " invoked with " : " called with ") << "wrong number of arguments." << endl;
    } else {
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Formal formal = formals->nth(i);
            Symbol passed_type = arguments->nth(i)->check_type(this);
Symbol intended_type = formal->get_type_decl();
            if (!is_subtype(passed_type, intended_type)) {
                semant_error(cls->get_filename(), dispatch) << "In call of method " << method_name << ", type " << passed_type << " of parameter " << formal->get_name() << " does not conform to declared type " << intended_type << "." << endl;
            }
        }
    }

    if (method->get_return_type() == SELF_TYPE) {
        return expression_type;
    }
    return method->get_return_type();
}

method_class *ClassTable::find_method(Symbol class_name, Symbol method_name) {
    std::map<Symbol, Class_>::iterator it = class_by_name.find(class_name);
    assert(it != class_by_name.end());
    Class_ cls = it->second;

    while (cls != NULL) {
        Features features = cls->get_features();
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            Feature feat = features->nth(i);
            if (feat->is_method() && feat->get_name() == method_name) {
                return dynamic_cast<method_class *>(feat);
            }
        }
        // If we cannot find the method defined in the class, try the parent class;
        // this may be an inherited method.
        std::map<Class_, Class_>::iterator it = parent_graph.find(cls);
        if (it == parent_graph.end()) {
            return NULL;
        }
        cls = it->second;
    }
    return NULL;
}

Symbol typcase_class::check_type(TypeChecker *type_checker) {
    expr->check_type(type_checker);
    std::set<Symbol> expr_types;
    std::set<Symbol> decl_types;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Case cs = cases->nth(i);
        expr_types.insert(cs->check_type(type_checker));
        Symbol decl_type = cs->get_type_decl();
        if (!decl_types.insert(decl_type).second) {
            type_checker->semant_error(type_checker->get_active_class()->get_filename(), cs) << "Duplicate branch " << decl_type << " in case statement." << endl;
        }
    }
    Symbol lub = type_checker->least_upper_bound(expr_types);
    this->set_type(lub);
    return lub;
}

Symbol branch_class::check_type(TypeChecker *type_checker) {
    if (type_decl == SELF_TYPE) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Identifier " << name << " declared with type SELF_TYPE in case branch." << endl;
    } else if (type_checker->get_class_by_name()->count(type_decl) == 0) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Class " << type_decl << " of case branch is undefined." << endl;
    }
    // Evaluate the expression with the case object added to the scope.
    ObjectTable *object_table = type_checker->get_object_table();
    object_table->enterscope();
    object_table->addid(name, type_decl);
    Symbol inferred_type = expr->check_type(type_checker);
    object_table->exitscope();
    return inferred_type;
}

Symbol eq_class::check_type(TypeChecker *type_checker) {
    Symbol t1 = e1->check_type(type_checker);
    Symbol t2 = e2->check_type(type_checker);
    if ((t1 == Str  || 
         t2 == Str  ||
         t1 == Bool ||
         t2 == Bool ||
         t1 == Int  ||
         t2 == Int) && t1 != t2) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Illegal comparison with a basic type." << endl;
    }
    this->set_type(Bool);
    return Bool;
}

Symbol loop_class::check_type(TypeChecker *type_checker) {
    Symbol pred_type = pred->check_type(type_checker);
    if (pred_type != Bool) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Loop condition does not have type Bool." << endl;
    }
    body->check_type(type_checker); // The type of the loop is always Object so this value is ignored.
    this->set_type(Object);
    return Object;
}

Symbol cond_class::check_type(TypeChecker *type_checker) {
    Symbol pred_type = pred->check_type(type_checker);
    if (pred_type != Bool) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Predicate of 'if' does not have type Bool." << endl;
    }
    Symbol type = type_checker->least_upper_bound(std::set<Symbol>({then_exp->check_type(type_checker), else_exp->check_type(type_checker)}));
    assert(type != NULL);
    this->set_type(type);
    return type;
}

Symbol ClassTable::least_upper_bound(std::set<Symbol> nodes) {
    std::set<Symbol> defined_types;
    for (const auto &node : nodes) {
        if (class_by_name.count(node) != 0) {
            defined_types.insert(node);
        }
    }
    if (defined_types.size() == 0) {
        return No_type;
    }
    return least_upper_bound(defined_types, Object);
}

Symbol ClassTable::least_upper_bound(std::set<Symbol> nodes, Symbol current) {
    if (current == NULL) {
        return NULL;
    }
    if (nodes.count(current) > 0) {
        // The current node is one of the nodes we're looking for, so it or something above it
        // must be the LUB.
        return current;
    }
    std::list<Symbol> ancestors;
    std::map<Symbol, Class_>::iterator it = class_by_name.find(current);
    assert(it != class_by_name.end());
    Class_ cls = it->second;
    std::pair<std::multimap<Class_, Class_>::iterator, 
std::multimap<Class_, Class_>::iterator> ret = child_graph.equal_range(cls);
    for (std::multimap<Class_, Class_>::iterator it = ret.first; it != ret.second; ++it) {
        Symbol candidate = least_upper_bound(nodes, it->second->get_name());
        if (candidate != NULL) {
            ancestors.push_back(candidate);
        }
    }
    if (nodes.count(SELF_TYPE) > 0 && current == active_class->get_name()) {
        // SELF_TYPE is treated like another child of current (since it is always <= current).
        // If there are any other descendants of current amongst the nodes, then the LUB must be 
        // >= current; otherwise the LUB could be SELF_TYPE itself.
        ancestors.push_back(SELF_TYPE);
    }

    assert(ancestors.size() <= nodes.size());
    if (ancestors.size() >= 2) {
        return current;
    } else if (ancestors.size() == 1) {
        return ancestors.front();
    } else {
        return NULL;
    }
}

Symbol let_class::check_type(TypeChecker *type_checker) {
    // The initialization expression is evaluated before the object is added to the scope.
    bool has_binding_to_self = identifier == self;

    bool invalid_type = false;
    if (type_checker->get_class_by_name()->count(type_decl) == 0) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Class " << type_decl << " of let-bound identifier " << identifier << " is undefined."  << endl;
        invalid_type = true;
    }
    if (!init->is_empty()) {
        Symbol inferred_type = init->check_type(type_checker);
        if (!invalid_type && !type_checker->is_subtype(inferred_type, type_decl)) {
            type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Inferred type " << inferred_type << " of initialization of " << identifier << " does not conform to identifier's declared type " << type_decl << "." << endl;
        }
    }
    ObjectTable *object_table = type_checker->get_object_table();
    object_table->enterscope();
    if (has_binding_to_self) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "'self' cannot be bound in a 'let' expression." << endl;
    } else {
        object_table->addid(identifier, type_decl);
    }
    Symbol body_type = body->check_type(type_checker);
    object_table->exitscope();
    set_type(body_type);
    return body_type;
}

Symbol no_expr_class::check_type(TypeChecker *type_checker) {
    assert(false);
}

Symbol assign_class::check_type(TypeChecker *type_checker) {
    Symbol inferred_type = expr->check_type(type_checker);
    Symbol decl_type = type_checker->get_object_table()->lookup(name);
    if (name == self) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Cannot assign to 'self'." << endl;
    }
    if (!type_checker->is_subtype(inferred_type, decl_type)) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Type " << inferred_type << " of assigned expression does not conform to declared type " << decl_type << " of identifier " << name << "." << endl;
    }
    set_type(inferred_type);
    return inferred_type;
}

Symbol object_class::check_type(TypeChecker *type_checker) {
    Symbol sym = type_checker->lookup_object(this->name);
    if (sym == NULL) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Undeclared identifier " << this->name << "." << endl;
        return Object;
    }
    set_type(sym);
    return sym;
}

Symbol isvoid_class::check_type(TypeChecker *type_checker) {
    e1->check_type(type_checker); 
    set_type(Bool);
    return Bool;
}

Symbol comp_class::check_type(TypeChecker *type_checker) {
    Symbol type = e1->check_type(type_checker);
    if (type != Bool) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Argument of 'not' has type " << type << " instead of Bool." << endl;
    }
    set_type(Bool);
    return Bool;
}


Symbol neg_class::check_type(TypeChecker *type_checker) {
    Symbol type = e1->check_type(type_checker);
    if (type != Int) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "Argument of '~' has type " << type << " instead of Int." << endl;
    }
    set_type(Int);
    return Int;
}

Symbol lt_class::check_type(TypeChecker *type_checker) {
    return type_checker->verify_arith(this, Bool, e1->check_type(type_checker), e2->check_type(type_checker), "<");
}

Symbol leq_class::check_type(TypeChecker *type_checker) {
    return type_checker->verify_arith(this, Bool, e1->check_type(type_checker), e2->check_type(type_checker), "<=");
}

Symbol plus_class::check_type(TypeChecker *type_checker) {
    return type_checker->verify_arith(this, Int, e1->check_type(type_checker), e2->check_type(type_checker), "+");
}

Symbol sub_class::check_type(TypeChecker *type_checker) {
    return type_checker->verify_arith(this, Int, e1->check_type(type_checker), e2->check_type(type_checker), "-");
}

Symbol mul_class::check_type(TypeChecker *type_checker) {
    return type_checker->verify_arith(this, Int, e1->check_type(type_checker), e2->check_type(type_checker), "*");
}

Symbol divide_class::check_type(TypeChecker *type_checker) {
    return type_checker->verify_arith(this, Int, e1->check_type(type_checker), e2->check_type(type_checker), "/");
}

Symbol ClassTable::verify_arith(Expression expr, Symbol type, Symbol t1, Symbol t2, std::string op) {
    if (t1 != Int || t2 != Int) {
        semant_error(get_active_class()->get_filename(), expr) << "non-Int arguments: " << t1 << " " << op << " " << t2 << endl;
    }
    expr->set_type(type);
    return type;
}

Symbol block_class::check_type(TypeChecker *type_checker) {
    Symbol last_type;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        last_type = body->nth(i)->check_type(type_checker);
    }
    set_type(last_type);
    return last_type;
}

Symbol new__class::check_type(TypeChecker *type_checker) {
    std::map<Symbol, Class_> *class_by_name = type_checker->get_class_by_name();
    if (class_by_name->find(type_name) == class_by_name->end()) {
        type_checker->semant_error(type_checker->get_active_class()->get_filename(), this) << "'new' used with undefined class " << type_name << "." << endl;
        set_type(Object);
        return Object;
    } else {
        set_type(type_name);
        return type_name;
    }
}

Symbol int_const_class::check_type(TypeChecker *type_checker) {
   set_type(Int);
   return Int;
}

Symbol string_const_class::check_type(TypeChecker *type_checker) {
    set_type(Str);
    return Str;
}

Symbol bool_const_class::check_type(TypeChecker *type_checker) {
    set_type(Bool);
   return Bool;
}

Symbol ClassTable::lookup_object(Symbol object) {
    return object_table.lookup(object);
}

method_class *ClassTable::lookup_method(Symbol method) {
    return method_table.lookup(method);
}

Class_ ClassTable::get_active_class() {
    return active_class;
}

void ClassTable::set_active_class(Class_ cls) {
    active_class = cls;
}

std::map<Symbol, Class_> *ClassTable::get_class_by_name() {
    return &class_by_name;
}

std::multimap<Class_, Class_> *ClassTable::get_child_graph() {
    return &child_graph;
}

MethodTable *ClassTable::get_method_table() {
    return &method_table;
}

ObjectTable *ClassTable::get_object_table() {
    return &object_table;
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
    std::map<Symbol, Class_>::iterator it_b = class_by_name.find(class_name_b);
    std::map<Symbol, Class_>::iterator it_a = class_by_name.find(class_name_a);
    if (it_b == class_by_name.end() || it_a == class_by_name.end()) {
        // To avoid cascading failures, we consider UKNOWN <= a and b <= UNKNOWN.
        return true;
    }

    assert(it_b != class_by_name.end());
    assert(it_a != class_by_name.end());
    Class_ class_b = it_b->second;
    Class_ class_a = it_a->second;

    Class_ curr = class_b;

    while (curr != NULL) {
        if (curr == class_a) {
            return true;
        }
        std::map<Class_, Class_>::iterator it = parent_graph.find(curr);
        curr = it == parent_graph.end() ? NULL : it->second;
    }

    return false;
}

void ClassTable::find_cycle_in_subgraph(
        Class_ start_node,
        std::map<Class_, Class_> graph,
        std::set<Class_> &visited,
        std::set<Class_> &visiting,
        std::set<Class_> &cycle_nodes) {
    std::map<Class_, Class_>::iterator it = graph.find(start_node);
    if (it == graph.end()) {
        return;
    }
    Class_ parent = it->second;
    bool already_visiting = visiting.find(parent) != visiting.end();
    bool on_known_cycle = cycle_nodes.find(parent) != cycle_nodes.end();
    if (already_visiting || on_known_cycle) {
        for (Class_ cls : visiting) {
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

    Object_class =
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
    Class_ Self_class = class_(SELF_TYPE, No_class, nil_Features(), filename);

    class_by_name.insert(std::pair(Object, Object_class));
    class_by_name.insert(std::pair(Str, Str_class));
    class_by_name.insert(std::pair(Bool, Bool_class));
    class_by_name.insert(std::pair(Int, Int_class));
    class_by_name.insert(std::pair(IO, IO_class));
    class_by_name.insert(std::pair(SELF_TYPE, Self_class));
    built_in_classes.insert(Object);
    built_in_classes.insert(Str);
    built_in_classes.insert(Bool);
    built_in_classes.insert(Int);
    built_in_classes.insert(IO);
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


