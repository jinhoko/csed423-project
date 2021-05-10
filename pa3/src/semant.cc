

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <vector>

extern int semant_debug;
extern char *curr_filename;

using namespace cool;

ClassTable* CT;

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

// finish definition of methods declared in cool-tree.handcode.h
Symbol class__class::get_filename() { return filename; }
Symbol class__class::get_name() { return name; }
Symbol class__class::get_parent() { return parent; }
Features class__class::get_features() { return features; }

Symbol attr_class::get_name() { return name; }
Symbol method_class::get_name() { return name; }
Symbol attr_class::get_type() { return type_decl; }
Symbol method_class::get_type() { return return_type; }
Formals method_class::get_formals() { return formals; }
Formals attr_class::get_formals() { return NULL; }
Symbol formal_class::get_type() { return type_decl; }
Symbol formal_class::get_name() { return name; }
Symbol branch_class::get_type() { return type_decl; }
Symbol branch_class::get_name() { return name; }
Expression branch_class::get_expr() { return expr; }

Expression attr_class::get_expr() { return init; }
Expression method_class::get_expr() { return expr; }

void attr_class::add_feature( Symbol c ) { if( get_name() != self ) CT->environment[c]->add_attr(get_name(), this->type_decl); }
void method_class::add_feature( Symbol c ) { CT->environment[c]->add_method(get_name(), this); }

bool attr_class::check_feature_name_type( Class_ c, Symbol target ) { 
    bool isAttrNameSelf = name == self;
    bool isAttrTypeDefined = CT->is_class_exists_in_program( type_decl ) || type_decl == prim_slot || type_decl == SELF_TYPE;

    if( isAttrNameSelf ) { CT->printerr_attr_name_self( c, this, target ); }
    if( !isAttrTypeDefined ) { CT->printerr_attr_type_undefined( c, this, type_decl, name, target ); }
    return isAttrNameSelf || !isAttrTypeDefined;
}
bool attr_class::check_feature_duplicate(  Class_ c, Symbol target  ) {
    bool isAttrDuplicated = (CT->environment[ target ]->ot->probe( name ) != NULL) && (name != self);
    if( isAttrDuplicated ) { CT->printerr_attr_multiple_defined( c, this, name, target ); }
    return isAttrDuplicated;
}
bool attr_class::check_feature_method_formals( Class_ c, Symbol target  ) { return false; }
bool attr_class::check_feature_inheritance( Class_ c, Symbol target ) {
    bool isAttrDefinedInParent = CT->environment[ target ]->ot->lookup( name ) != NULL;
    if( isAttrDefinedInParent ) { CT->printerr_attr_in_inherited_class( c, this, name, target ); }
    return isAttrDefinedInParent;
}

bool method_class::check_feature_name_type( Class_ c, Symbol target ) {
    bool isMethodTypeDefined = CT->is_class_exists_in_program( return_type ) || (return_type == SELF_TYPE);
    if( !isMethodTypeDefined ) { CT->printerr_method_type_undefined( c, this, return_type, name, target); }
    return !isMethodTypeDefined;
}
bool method_class::check_feature_duplicate(Class_ c, Symbol target ) {  
    bool isMethodDuplicated = CT->environment[ target ]->mt->probe( name ) != NULL;
    if( isMethodDuplicated ) { CT->printerr_method_multiple_defined( c, this, name, target ); }
    return isMethodDuplicated;
} 
bool method_class::check_feature_method_formals( Class_ c, Symbol target ) {
    
    // use temporary symboltable for duplicate check
    SymbolTable<Symbol, noData>* fTable = new SymbolTable<Symbol, noData>();
    fTable->enterscope();
    bool isFormalNameSelf; bool isFormalTypeSelf; bool isFormalDuplicated; bool isFormalWrongType;
    bool isErrorFound = false;
    Formal f; Symbol fname; Symbol ftype;
    int idx;
    for( idx = get_formals()->first();
        get_formals()->more(idx);
        idx = get_formals()->next(idx) ) {

        f = get_formals()->nth(idx);
        fname = f->get_name();
        ftype = f->get_type();

        isFormalDuplicated = fTable->lookup( f->get_name() ) != NULL;
        isFormalNameSelf = fname == self;
        isFormalTypeSelf = ftype == SELF_TYPE;
        isFormalWrongType = !( CT->is_class_exists_in_program( ftype ) ) && ftype != SELF_TYPE;
        
        if( isFormalDuplicated || isFormalNameSelf || isFormalTypeSelf || isFormalWrongType ) { isErrorFound = true; }
        if( isFormalDuplicated ){ CT->printerr_method_multiple_formal( c, this, fname, target); }
        if ( isFormalNameSelf ) { CT->printerr_method_formal_selfname( c, this, target ); }
        if ( isFormalTypeSelf ) { CT->printerr_method_formal_selftype( c, this, fname, target); }
        if ( isFormalWrongType ) { CT->printerr_method_formal_wrongtype( c, this, ftype, fname, target ); }

        fTable->addid( fname, CT->_garbage );
    }
    return isErrorFound;
} 
bool method_class::check_feature_inheritance(Class_ c, Symbol target ) {
    
    Feature orig_method = CT->environment[ target ]->mt->lookup( name );
    if(orig_method == NULL) { return false; }

    bool isRedefinedMethodReturnTypeError = orig_method->get_type() != return_type ;
    bool isRedefinedMethodNumArgsError = orig_method->get_formals()->len() != get_formals()->len();

    if( isRedefinedMethodReturnTypeError ) {
        CT->printerr_method_redefined_typeerror( c, this, name,  return_type, orig_method->get_type(), target);
    }
    if( isRedefinedMethodNumArgsError ) {
        CT->printerr_method_redefined_numargerror( c, this, name, target);
        return true ;
    }
    // If number of args are different, typechecking each formal is impossible
    // thus return immediately.
    assert( orig_method->get_formals()->len() == get_formals()->len() );

    bool isRedefinedMethodParamTypeError;
    Formals orig_fs = orig_method->get_formals();
    Formal orig_f, this_f;
    int idx;
    for( idx = orig_fs->first();
        orig_fs->more(idx);
        idx = orig_fs->next(idx) ) {

        orig_f = orig_fs->nth(idx);
        this_f = this->get_formals()->nth(idx);

        isRedefinedMethodParamTypeError = orig_f->get_type() != this_f->get_type();
        if( isRedefinedMethodParamTypeError ) { 
            CT->printerr_method_paramtypeerror( c, this, get_name(), this_f->get_type(), orig_f->get_type(), target );
            break; // the reference solution exits immediately when first mismatch is found
        }
    }
    return isRedefinedMethodReturnTypeError || isRedefinedMethodParamTypeError;

}

void attr_class::check_type( Class_ c ) {

    CT->environment[c->get_name()]->enterscope();
    CT->environment[c->get_name()]->add_attr(self, c->get_name()); // add self
    Symbol t1 = init->infer_type(c);
    if( CT->is_class_exists_in_program(type_decl) || type_decl==SELF_TYPE ) { // for invalid type_decl, pass type inference
        if( ! (CT->is_poset(t1, type_decl, c)) ) {
            CT->printerr_attr_mismatch( c, this, t1, type_decl);
        }   
    } 
    CT->environment[c->get_name()]->exitscope();
}
void method_class::check_type( Class_ c ) {

    CT->environment[c->get_name()]->enterscope();
    // add formals (except one has name self)
    CT->environment[c->get_name()]->add_attr(self, c->get_name()); // add self
    Formal f;
    int idx;
    for( idx = get_formals()->first();
        get_formals()->more(idx);
        idx = get_formals()->next(idx) ) {

        f = get_formals()->nth(idx);
        if( f->get_name() == self ) { continue; }
        if( CT->environment[c->get_name()]->ot->probe(f->get_name()) != NULL ) { continue; }   // pass duplicates
        CT->environment[c->get_name()]->add_attr(f->get_name(), f->get_type());                 // add formal
    }
    // infer type
    Symbol t0_ = get_expr()->infer_type(c);
    Symbol t0 = return_type;
    if( CT->is_class_exists_in_program(t0) || return_type==SELF_TYPE ) {     // for invalid type_decl, pass type inference

        if( ! (CT->is_poset(t0_, t0, c)) ) {   
            CT->printerr_method_mismatch( c, this, t0_, return_type);
        }
    } 
    CT->environment[c->get_name()]->exitscope();
}

Symbol assign_class::infer_type( Class_ c) {
    if( name == self ) { CT->printerr_assign_self(c, this); }
    Symbol t = CT->environment[c->get_name()]->ot->lookup(name);
    if( t == NULL ) {
        CT->printerr_assign_undeclared( c, this, name);
        return set_type(Object)->type;
    }
    Symbol t_ = expr->infer_type(c);
    if( CT->is_poset(t_, t, c) ) { return set_type(t_)->type; }
    CT->printerr_assign_mismatch( c, this, t_, t, name );
    return t_;  // recover as type t_!
}
Symbol static_dispatch_class::infer_type( Class_ c) {
    if( type_name == SELF_TYPE ) {
        CT->printerr_staticdispatch_selftype(c, this);
        return set_type(Object)->type;
    }
    if( ! CT->is_class_exists_in_program( type_name ) && type_name!=SELF_TYPE ) {
        CT->printerr_staticdispatch_undefined( c, this, type_name);
        return set_type(Object)->type;
    }
    Symbol t0 = expr->infer_type(c);
    if( ! CT->is_poset( t0, type_name, c) ) { 
        CT->printerr_staticdispatch_typeerror( c, this, t0, type_name);
        return set_type(Object)->type;
    }
    // method lookup
    Feature method = CT->environment[type_name]->mt->lookup(name);
    if( method == NULL ) {
        CT->printerr_dispatch_undefined_method(c, this, name);
        return set_type(Object)->type;
    }
    Symbol tn1_ = method->get_type();
    // poset check for each formals
    bool isNumParamRight = true, isPosetRight = true;
    if( actual->len() != method->get_formals()->len() ) {
        CT->printerr_dispatch_paramnumerror( c, this, name );
        isNumParamRight = false;
    } else {
        Expression en; Symbol tn, tn_;
        int idx;
        for( idx = actual->first();
            actual->more(idx);
            idx = actual->next(idx) ) {
            
            en = actual->nth(idx);
            tn = en->infer_type(c);
            tn_ = method->get_formals()->nth(idx)->get_type();
            if( ! CT->is_poset( tn, tn_, c ) ) {
                CT->printerr_dispatch_paramerror( c, this, method->get_name(), 
                        tn, method->get_formals()->nth(idx)->get_name(), tn_ );
                isPosetRight = false;
            }
        }
    }
    if( isNumParamRight && isPosetRight ) { 
        return set_type( (tn1_==SELF_TYPE) ? t0 : method->get_type() )->type;
    }
    return set_type(Object)->type;
}
Symbol dispatch_class::infer_type( Class_ c ){
    Symbol t0 = expr->infer_type(c);
    // class check
    if( ! CT->is_class_exists_in_program( t0 ) && t0!=SELF_TYPE ) {
        CT->printerr_dispatch_undefined( c, this, t0);
        return set_type(Object)->type;
    }
    Symbol t0_ = (t0 == SELF_TYPE) ? c->get_name() : t0;
    // method lookup
    Feature method = CT->environment[t0_]->mt->lookup(name);
    if( method == NULL ) {
        CT->printerr_dispatch_undefined_method(c, this, name);
        return set_type(Object)->type;
    }
    Symbol tn1_ = method->get_type();
    // poset check for each formals
    bool isNumParamRight = true, isPosetRight = true;
    if( actual->len() != method->get_formals()->len() ) {
        CT->printerr_dispatch_paramnumerror( c, this, name );
        isNumParamRight = false;
    } else {
        Expression en; Symbol tn, tn_;
        int idx;
        for( idx = actual->first();
            actual->more(idx);
            idx = actual->next(idx) ) {
            
            en = actual->nth(idx);
            tn = en->infer_type(c);
            tn_ = method->get_formals()->nth(idx)->get_type();

            if( ! CT->is_poset( tn, tn_, c ) ) {
                CT->printerr_dispatch_paramerror( c, this, method->get_name(), 
                        tn, method->get_formals()->nth(idx)->get_name(), tn_ );
                isPosetRight = false;
            }
        }
    }
    if( isNumParamRight && isPosetRight ) { 
        return set_type( (tn1_==SELF_TYPE) ? t0 : method->get_type() )->type;
    }
    return set_type(Object)->type;
}
Symbol cond_class::infer_type( Class_ c) {
    if( pred->infer_type(c) != Bool ) {
        CT->printerr_if_notbool(c, this);
    }
    Symbol lub = CT->get_lub( then_exp->infer_type(c), else_exp->infer_type(c), c );
    return set_type(lub)->type;
}
Symbol loop_class::infer_type( Class_ c ) {
    if( pred->infer_type(c) != Bool ){
        CT->printerr_loop_notbool(c, this);
    }
    body->infer_type(c);
    return set_type(Object)->type;
}
Symbol typcase_class::infer_type( Class_ c ){
    Symbol t0 = expr->infer_type(c);
    Symbol lub = No_type;
    assert( cases->len() >= 1 );

    int idx; Case ca; Symbol ca_type; Symbol ca_name; Symbol ca_inferred;
    SymbolTable<Symbol, int> case_types; case_types.enterscope();
    for( idx = cases->first();
        cases->more(idx);
        idx = cases->next(idx) ) {
            
        ca = cases->nth(idx);
        ca_type = ca->get_type();
        ca_name = ca->get_name();

        if( !CT->is_class_exists_in_program(ca_type) && ca_type!=SELF_TYPE) { CT->printerr_case_undefined(c, ca, ca_type); }
        if( case_types.lookup(ca_type) != NULL ) { CT->printerr_case_duplicate(c, ca, ca_type); }

        if( ca_name == self ) { CT->printerr_case_self(c, ca); } 
        if( ca_type == SELF_TYPE ) { CT->printerr_case_selftype(c, ca, ca_name); 
        }
        CT->environment[c->get_name()]->enterscope();
        CT->environment[c->get_name()]->add_attr(ca_name, ca_type);
        ca_inferred = ca->get_expr()->infer_type(c);
        CT->environment[c->get_name()]->exitscope();
    //    std::cout << "cmp " << ca_inferred << " " << lub->get_string() << std::endl; // TODO del
        lub = CT->get_lub( ca_inferred, lub, c);
        case_types.addid(ca_type, new int(1));
    }
    //std::cout << "infered " << lub->get_string() << std::endl; // TODO del

    return set_type(lub)->type;
}
Symbol block_class::infer_type( Class_ c ) {
    int idx;
    for (idx = body->first(); body->more(idx); idx = body->next(idx)) {
        type = body->nth(idx)->infer_type(c);
    }
    return type;
}
Symbol let_class::infer_type( Class_ c ){
    if( identifier == self ) { CT->printerr_let_self(c, this); }
    bool isTypeCheckPossible = true;
    if( ! (CT->is_class_exists_in_program(type_decl)) && type_decl!=SELF_TYPE ) {
        CT->printerr_let_undefined(c, this, type_decl, identifier);
        isTypeCheckPossible = false;
    }
    Symbol t = init->infer_type(c);
    if( isTypeCheckPossible && !CT->is_poset(t, type_decl, c) ) {
        CT->printerr_let_mismatch(c, this, t, identifier, type_decl);
    }
    CT->environment[c->get_name()]->enterscope();
    CT->environment[c->get_name()]->add_attr(identifier, type_decl);
    type = body->infer_type(c);
    CT->environment[c->get_name()]->exitscope();
    return type;
} 
Symbol plus_class::infer_type( Class_ c ) {
    Symbol t1, t2;
    t1 = e1->infer_type(c); t2 = e2->infer_type(c);
    if ( t1 == Int && t2 == Int) { return set_type(Int)->type; }
    CT->printerr_arith_nonint( c, this, t1, t2 );
    return set_type(Int)->type; 
}
Symbol sub_class::infer_type( Class_ c ){
    Symbol t1, t2;
    t1 = e1->infer_type(c); t2 = e2->infer_type(c);
    if ( t1 == Int && t2 == Int) { return set_type(Int)->type; }
    CT->printerr_arith_nonint( c, this, t1, t2 );
    return set_type(Int)->type; 
}
Symbol mul_class::infer_type( Class_ c) {
    Symbol t1, t2;
    t1 = e1->infer_type(c); t2 = e2->infer_type(c);
    if ( t1 == Int && t2 == Int) { return set_type(Int)->type; }
    CT->printerr_arith_nonint( c, this, t1, t2 );
    return set_type(Int)->type; 
}
Symbol divide_class::infer_type( Class_ c ){
    Symbol t1, t2;
    t1 = e1->infer_type(c); t2 = e2->infer_type(c);
    if ( t1 == Int && t2 == Int) { return set_type(Int)->type; }
    CT->printerr_arith_nonint( c, this, t1, t2 );
    return set_type(Int)->type;
}
Symbol neg_class::infer_type( Class_ c){
    type = e1->infer_type(c);
    if( type == Int ) { return type; }
    CT->printerr_neg_nonint( c, this, type );
    return set_type(Int)->type; 
}
Symbol lt_class::infer_type( Class_ c){
    Symbol t1, t2;
    t1 = e1->infer_type(c); t2 = e2->infer_type(c);
    if ( t1 == Int && t2 == Int) { return set_type(Bool)->type; }
    CT->printerr_arith_nonint( c, this, t1, t2 );
    return set_type(Bool)->type;  
}
Symbol eq_class::infer_type( Class_ c){
    Symbol t1, t2;
    t1 = e1->infer_type(c); t2 = e2->infer_type(c);

    bool isPrimitive = t1 == Int || t2 == Int || t1 == Bool || t2 == Bool || t1 == Str || t2 == Str;
    bool isTypeDifferent = t1 != t2;
    if ( isPrimitive && isTypeDifferent ) {
        CT->printerr_eq_basictype( c, this );
        return set_type(Bool)->type;
    }
    return set_type(Bool)->type; 
}
Symbol leq_class::infer_type( Class_ c){
    Symbol t1, t2;
    t1 = e1->infer_type(c); t2 = e2->infer_type(c);
    if ( t1 == Int && t2 == Int) { return set_type(Bool)->type; }
    CT->printerr_arith_nonint( c, this, t1, t2 );
    return set_type(Bool)->type; 
}
Symbol comp_class::infer_type( Class_ c){ 
    type = e1->infer_type(c);
    if( type == Bool ) { return type; }
    CT->printerr_comp( c, this, type);
    return set_type(Bool)->type; 
}
Symbol int_const_class::infer_type( Class_ c){ return set_type(Int)->type; }
Symbol bool_const_class::infer_type( Class_ c){ return set_type(Bool)->type; }
Symbol string_const_class::infer_type( Class_ c){ return set_type(Str)->type; }
Symbol new__class::infer_type( Class_ c ){
    if( type_name == SELF_TYPE) { return set_type( SELF_TYPE )->type; }
    if( CT->get_class(type_name) != NULL ) { return set_type(type_name)->type; }
    CT->printerr_new_undefined( c, this, type_name );
    return set_type(Object)->type;  
}
Symbol isvoid_class::infer_type( Class_ c){ e1->infer_type( c ); type = Bool; return type; }
Symbol no_expr_class::infer_type( Class_ c){ type = No_type; return type; }
Symbol object_class::infer_type( Class_ c ){
    if(name == self) { return set_type(SELF_TYPE)->type; }
    type = CT->environment[c->get_name()]->ot->lookup(name);
    if( type != NULL ){ return type; }
    CT->printerr_undeclared_id( c, this, name);
    return set_type(Object)->type; 
}

///////////////////////////
// ClassTable definition
///////////////////////////


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    curr_lineno  = 0;
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

    base_classes =
        append_Classes(
            append_Classes(
                append_Classes(
                    append_Classes(
                        single_Classes(Object_class),
                        single_Classes(IO_class)
                    ),
                    single_Classes(Int_class)
                ),
                single_Classes(Bool_class)
            ),
            single_Classes(Str_class)
        );

    return;
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

////////////////////////////////////////////////////////////////////
//
// Semantic error printer
//
///////////////////////////////////////////////////////////////////

void ClassTable::printerr_prevdef( Class_ c1, Symbol c2 ) {
    semant_error( c1 ) << "Class " << c2->get_string() <<  " was previously defined.\n";
}
void ClassTable::printerr_redefine_reserved( Class_ c1, Symbol c2 ) {
    semant_error( c1 ) << "Redefinition of basic class " << c2->get_string() << ".\n";
}
void ClassTable::printerr_inherit_base( Class_ c1, Symbol c2, Symbol c3) {
    semant_error( c1 ) << "Class " << c2->get_string() << " cannot inherit class " << c3->get_string() << ".\n";
}
void ClassTable::printerr_isdangling( Class_ c1, Symbol c2, Symbol c3 ) {
    semant_error( c1 ) << "Class " << c2->get_string() <<  " inherits from an undefined class " << c3->get_string() <<  ".\n";
}
void ClassTable::printerr_cyclefound( Class_ c1, Symbol c2) {
    semant_error( c1 ) << "Class " << c2->get_string() << ", or an ancestor of " << c2->get_string() << ", is involved in an inheritance cycle.\n";
}
void ClassTable::printerr_main_method_not_exists( Class_ c1 ) {
    semant_error( c1 ) << "No 'main' method in class Main.\n";
}
void ClassTable::printerr_main_class_not_exists() {
    semant_error( ) << "Class Main is not defined.\n";
}
void ClassTable::printerr_main_method_param_error( Class_ c1) {
    semant_error( c1 ) << "'main' method in class Main should have no arguments.\n";
}
  

bool ClassTable::_is_printerr_available( Class_ c1, Symbol target ) { return c1->get_name() == target; }
void ClassTable::printerr_attr_name_self( Class_ c1, Feature f, Symbol target){
    if( _is_printerr_available(c1, target)) semant_error( c1->get_filename(), f ) << "'self' cannot be the name of an attribute.\n";
}
void ClassTable::printerr_attr_type_undefined( Class_ c1, Feature f, Symbol type, Symbol name, Symbol target ) {
    if( _is_printerr_available(c1, target)) semant_error( c1->get_filename(), f ) << "Class " << type->get_string() << " of attribute " << name->get_string() << " is undefined.\n";
}
void ClassTable::printerr_method_type_undefined( Class_ c1, Feature f, Symbol type, Symbol meth, Symbol target ){
    if( _is_printerr_available(c1, target)) semant_error( c1->get_filename(), f ) << "Undefined return type " << type->get_string() << " in method " << meth->get_string() << ".\n";
}
void ClassTable::printerr_attr_multiple_defined( Class_ c1, Feature f,Symbol name, Symbol target){
    if( _is_printerr_available(c1, target)) semant_error( c1->get_filename(), f ) << "Attribute " << name->get_string() << " is multiply defined in class.\n";
}
void ClassTable::printerr_method_multiple_defined( Class_ c1, Feature f,Symbol name, Symbol target ){
    if( _is_printerr_available(c1, target)) semant_error( c1->get_filename(), f ) << "Method " << name->get_string() << " is multiply defined.\n";
}
void ClassTable::printerr_attr_in_inherited_class( Class_ c1, Feature f, Symbol name, Symbol target ){
    if( _is_printerr_available(c1, target)) semant_error( c1->get_filename(), f ) << "Attribute " << name->get_string() << " is an attribute of an inherited class.\n";
}
void ClassTable::printerr_method_redefined_typeerror (Class_ c1, Feature f, Symbol name, Symbol type, Symbol type2, Symbol target){
    if( _is_printerr_available(c1, target))
        semant_error( c1->get_filename(), f ) << "In redefined method " << name->get_string() << ", return type " << type->get_string() << " is different from original return type "<< type2->get_string() << ".\n";
}
void ClassTable::printerr_method_redefined_numargerror ( Class_ c1, Feature f, Symbol name, Symbol target){
    if( _is_printerr_available(c1, target))
        semant_error( c1->get_filename(), f ) << "Incompatible number of formal parameters in redefined method "<< name->get_string() <<".\n";
}
void ClassTable::printerr_method_paramtypeerror( Class_ c1, Feature f, Symbol name, Symbol type, Symbol type2, Symbol target){
    if( _is_printerr_available(c1, target))
        semant_error( c1->get_filename(), f ) << "In redefined method " << name->get_string() << ", parameter type " << type->get_string() << " is different from original type " << type2->get_string() << "\n";
}
void ClassTable::printerr_method_multiple_formal( Class_ c1, Feature f, Symbol name, Symbol target ) {
    if( _is_printerr_available(c1, target))
        semant_error( c1->get_filename(), f ) << "Formal parameter " << name->get_string() << " is multiply defined.\n";
}
void ClassTable::printerr_method_formal_selftype( Class_ c1, Feature f, Symbol name, Symbol target   ) {
    if( _is_printerr_available(c1, target))
        semant_error( c1->get_filename(), f ) << "Formal parameter " << name->get_string() << " cannot have type SELF_TYPE.\n";
}
void ClassTable::printerr_method_formal_selfname( Class_ c1, Feature f, Symbol target ){
    if( _is_printerr_available(c1, target))
        semant_error( c1->get_filename(), f ) << "'self' cannot be the name of a formal parameter.\n";
}
void ClassTable::printerr_method_formal_wrongtype( Class_ c1, Feature f, Symbol t, Symbol fname, Symbol target ){
    if( _is_printerr_available(c1, target))
        semant_error( c1->get_filename(), f ) << "Class " << t->get_string() << " of formal parameter " << fname->get_string() << " is undefined.\n";
}

/////

void ClassTable::printerr_arith_nonint( Class_ c1, Expression e, Symbol t1, Symbol t2 ){
    semant_error( c1->get_filename(), e ) << "non-Int arguments: " << t1->get_string()<< " + "<<t2->get_string() << "\n";
}
void ClassTable::printerr_neg_nonint( Class_ c1, Expression e, Symbol t){
    semant_error( c1->get_filename(), e ) << "non-Int argument: " << t->get_string()<< ".\n";
}
void ClassTable::printerr_new_undefined( Class_ c1, Expression e, Symbol type ){
    semant_error( c1->get_filename(), e ) << "'new' used with undefined class " << type->get_string() << ".\n";
}
void ClassTable::printerr_eq_basictype( Class_ c1, Expression e  ){
    semant_error( c1->get_filename(), e ) << "Illegal comparison with a basic type.\n";
}
void ClassTable::printerr_undeclared_id( Class_ c1, Expression e, Symbol name ){
    semant_error( c1->get_filename(), e ) << "Undeclared identifier " << name->get_string() << ".\n";
}
void ClassTable::printerr_comp( Class_ c1, Expression e, Symbol type){
    semant_error( c1->get_filename(), e ) << "Argument of 'not' has type "<< type->get_string() << " instead of Bool.\n";
}
void ClassTable::printerr_loop_notbool( Class_ c1, Expression e){
    semant_error( c1->get_filename(), e ) << "Loop condition does not have type Bool.\n";
}
void ClassTable::printerr_if_notbool( Class_ c1, Expression e){
    semant_error( c1->get_filename(), e ) << "Predicate of 'if' does not have type Bool.\n";
}
void ClassTable::printerr_assign_self( Class_ c1, Expression e){
    semant_error( c1->get_filename(), e ) << "Cannot assign to 'self'.\n";
}
void ClassTable::printerr_assign_mismatch( Class_ c1, Expression e, Symbol t1, Symbol t2, Symbol name){
    semant_error( c1->get_filename(), e ) << "Type " << t1->get_string() << " of assigned expression does not conform to declared type " << t2->get_string() << " of identifier " << name->get_string() << ".\n";
}
void ClassTable::printerr_assign_undeclared( Class_ c1, Expression e, Symbol name){
    semant_error( c1->get_filename(), e ) << "Assignment to undeclared variable "<< name->get_string() << ".\n";
}
void ClassTable::printerr_let_self( Class_ c1, Expression e) {
    semant_error( c1->get_filename(), e ) << "'self' cannot be bound in a 'let' expression.\n";
}
void ClassTable::printerr_let_undefined( Class_ c1, Expression e, Symbol type, Symbol name) {
    semant_error( c1->get_filename(), e ) << "Class " << type->get_string() << " of let-bound identifier " << name->get_string() << " is undefined.\n";
}
void ClassTable::printerr_let_mismatch( Class_ c1, Expression e, Symbol t1, Symbol name, Symbol t2) {
    semant_error( c1->get_filename(), e ) << "Inferred type " << t1->get_string() << " of initialization of " << name->get_string()  << " does not conform to identifier's declared type " << t2->get_string() << ".\n";
}
void ClassTable::printerr_dispatch_undefined( Class_ c1, Expression e, Symbol name) {
    semant_error( c1->get_filename(), e ) << "Dispatch on undefined class " << name << ".\n";
}
void ClassTable::printerr_dispatch_undefined_method( Class_ c1, Expression e, Symbol name) {
    semant_error( c1->get_filename(), e ) << "Dispatch to undefined method " << name->get_string() << ".\n";
}
void ClassTable::printerr_dispatch_paramerror( Class_ c1, Expression e, Symbol name, Symbol t1, Symbol name2, Symbol t2) {
    semant_error( c1->get_filename(), e ) << "In call of method " << name->get_string() <<  ", type " << t1->get_string()
                                            << " of parameter " <<  name2->get_string() << " does not conform to " <<
                                            "declared type " << t2->get_string() << ".\n";
}
void ClassTable::printerr_dispatch_paramnumerror( Class_ c1, Expression e, Symbol name ) {
    semant_error( c1->get_filename(), e ) << "Method " << name->get_string() << " called with wrong number of arguments.\n";
}
void ClassTable::printerr_staticdispatch_selftype( Class_ c1, Expression e) {
    semant_error( c1->get_filename(), e ) << "Static dispatch to SELF_TYPE.\n";
}
void ClassTable::printerr_staticdispatch_undefined( Class_ c1, Expression e, Symbol type) {
    semant_error( c1->get_filename(), e ) << "Static dispatch to undefined class " << type->get_string() << ".\n";
}
void ClassTable::printerr_staticdispatch_typeerror( Class_ c1, Expression e, Symbol t1, Symbol t2) {
    semant_error( c1->get_filename(), e ) << "Expression type " << t1->get_string() << " does not conform to declared static dispatch type " << t2->get_string() << ".\n";
}
void ClassTable::printerr_case_self( Class_ c1, Case e) {
    semant_error( c1->get_filename(), e ) << "'self' bound in 'case'.\n";
}
void ClassTable::printerr_case_selftype( Class_ c1, Case e, Symbol name) {
    semant_error( c1->get_filename(), e ) << "Identifier " << name->get_string() << " declared with type SELF_TYPE in case branch.\n";
}
void ClassTable::printerr_case_duplicate( Class_ c1, Case e, Symbol type ) {
    semant_error( c1->get_filename(), e ) << "Duplicate branch " << type->get_string() << " in case statement.\n"; 
}
void ClassTable::printerr_case_undefined( Class_ c1, Case e, Symbol name ) {
    semant_error( c1->get_filename(), e ) << "Class " << name->get_string() << " of case branch is undefined.\n";    
}

////

void ClassTable::printerr_method_mismatch( Class_ c1, Feature f, Symbol t1, Symbol t2){
    semant_error( c1->get_filename(), f ) << "Inferred return type " << t1->get_string() << " of method " << f->get_name()->get_string()<< " does not conform to declared return type " << t2->get_string() << ".\n";
}
void ClassTable::printerr_attr_mismatch( Class_ c1, Feature f, Symbol t1, Symbol t2 ){
    semant_error( c1->get_filename(), f ) << "Inferred type " << t1->get_string() << " of initialization of attribute " << f->get_name()->get_string() << " does not conform to declared type " << t2->get_string() << ".\n";
}
// 
////////////////////////////////////////////////////////////////////
//
// Extending ClassTable function definitions
//
///////////////////////////////////////////////////////////////////

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    // Install basic classes to language_classes
    install_basic_classes();
    // Install user_classes
    user_classes = classes;
    
    // Initialize others
    _latestNodeIdx = 0;
    _garbageData = 1;
    _garbage = &_garbageData;
    assert( _garbageData != 0 ); // prevent NULL overlap

    // table for duplicate check
    valid_scope_symbols = new SymbolTable<Symbol, noData>();
    valid_scope_symbols->enterscope();

    // table for invalid inheritance
    invalid_inheritance_symbols = new SymbolTable<Symbol, noData>();
    invalid_inheritance_symbols->enterscope();

    // tables for inheritance graph (maintain inverted table as well; for printing error )
    graph_nodes = new SymbolTable<Symbol, nodeIdx>();
    graph_nodes->enterscope();
    graph_nodes->addid( Object,  get_new_nodeindex() ); 
    graph_nodes->addid( IO,  get_new_nodeindex() ); 
    graph_nodes->addid( SELF_TYPE,  get_new_nodeindex() ); 
    graph_nodes->addid( Int,  get_new_nodeindex() );
    graph_nodes->addid( Bool,  get_new_nodeindex() );
    graph_nodes->addid( Str,  get_new_nodeindex() );
    
    graph_edges = new SymbolTable<nodeIdx, nodeIdx>();
    graph_edges->enterscope();

    // table for symbol table
    class_map = new SymbolTable<Symbol, Class__class>();
    class_map->enterscope();
    error_checked.enterscope();
}

nodeIdx* ClassTable::get_new_nodeindex() {
    return new nodeIdx(++_latestNodeIdx); // generate index object every time
}

bool ClassTable::is_class_exists_in_program( Symbol s ) {

    bool result = false;
    int idx;
    Class_ _class;
    Symbol _class_name;
    for( idx = program_classes->first();
        program_classes->more(idx);
        idx = program_classes->next(idx) ) {

        _class = program_classes->nth(idx);
        _class_name = _class->get_name();

        if( _class_name->equal_string( s->get_string(), s->get_len() ) ) {
            result = true;
        }
    }
    return result;
}

bool ClassTable::is_class_base_class( Symbol s ) {
    bool result = false;
    int idx;
    Class_ _class;
    Symbol _class_name;
    for( idx = base_classes->first();
        base_classes->more(idx);
        idx = base_classes->next(idx) ) {

        _class = base_classes->nth(idx);
        _class_name = _class->get_name();

        if( _class_name->equal_string( s->get_string(), s->get_len() ) ) {
            result = true;
        }
    }
    return result;
}

int ClassTable::num_nodes() { return _latestNodeIdx; }

void ClassTable::check_graph_node_build() {

    // Checks following in first pass: 
    //   1. no duplicated definition of user class
    //   2. not redefinition of base classes
    //   3. user class not inheriting from base class
    //   => build nodes of inheritance graph

    // first pass
    int idx;
    Class_ _class;
    Symbol _class_name, _class_parent;
    bool isAlreadyInScope;
    bool isRedefiningReserved;
    bool isSelfType;
    bool isInheritingFromBaseClasses;

    for( idx = user_classes->first();
        user_classes->more(idx);
        idx = user_classes->next(idx) ) {

        _class = user_classes->nth(idx);
        _class_name = _class->get_name();
        _class_parent = _class->get_parent();

        isAlreadyInScope =
            valid_scope_symbols->lookup(_class_name) != NULL;
        isRedefiningReserved = 
            _class_name == Object || _class_name == No_class || _class_name == SELF_TYPE ||
            _class_name == Int || _class_name == Bool || _class_name == Str || _class_name == IO;
        isInheritingFromBaseClasses =
            _class_parent == Int || _class_parent == Bool || _class_parent == Str || _class_parent == SELF_TYPE;
        
        if( isAlreadyInScope ) {
            printerr_prevdef( _class, _class_name );
        } else if ( isRedefiningReserved ) {
            printerr_redefine_reserved( _class, _class_name );
            invalid_inheritance_symbols->addid(_class_name, _garbage );
        } else if ( isInheritingFromBaseClasses ) {
            printerr_inherit_base( _class, _class_name, _class_parent );
            valid_scope_symbols->addid(_class_name, _garbage );
            invalid_inheritance_symbols->addid(_class_name, _garbage );
            graph_nodes->addid( _class_name, get_new_nodeindex() );
        } else { // ok
            valid_scope_symbols->addid(_class_name, _garbage );
            graph_nodes->addid( _class_name, get_new_nodeindex() );
        }
    }
    

} 

void ClassTable::check_graph_edge_build() {

    // Checks following in second pass: 
    //   1. check dangling classes
    //   => build edges of inheritance graph
    //

    // second pass
    int idx;
    Class_ _class;
    Symbol _class_name, _class_parent;

    bool isClassDangling;
    bool isNodeWithInvalidInheritance;
    nodeIdx *parent_idx, *child_idx;
    for( idx = user_classes->first();
        user_classes->more(idx);
        idx = user_classes->next(idx) ) {

        _class = user_classes->nth(idx);
        _class_name = _class->get_name();
        _class_parent = _class->get_parent();

        isNodeWithInvalidInheritance = 
            invalid_inheritance_symbols->lookup( _class_name ) != NULL;
        if( isNodeWithInvalidInheritance ) { continue; }

        isClassDangling =
            graph_nodes->lookup( _class_parent ) == NULL;
        
        if( isClassDangling ) {
            printerr_isdangling( _class, _class_name, _class_parent );
        } else { // ok
            child_idx = graph_nodes->lookup( _class_name );
            parent_idx = graph_nodes->lookup( _class_parent );
            graph_edges->addid( *child_idx , parent_idx );
        }
    }

}

void ClassTable::check_inheritance_cycle() {

    // check cycles
    // (since each node has one outgoing edge, deploy naive algorithm.)

    int idx;
    Class_ _class;
    Symbol _class_name, _class_parent;
    nodeIdx *startIdx, *parent_idx;
    nodeIdx currIdx, parentIdx;

    for( idx = user_classes->first();
        user_classes->more(idx);
        idx = user_classes->next(idx) ) {
        
        _class = user_classes->nth(idx);
        _class_name = _class->get_name();

        startIdx = graph_nodes->lookup(_class_name);
        if( startIdx == NULL ) { continue; }
        
        currIdx = *startIdx;                                     // start from startIdx
        std::vector<bool> visited(num_nodes() + 1, false);       // use index 1~n
        do {
            visited[currIdx] = true;
            parent_idx = graph_edges->lookup( currIdx );         // fetch index value
            if( parent_idx == NULL ) { break; }                  // ok because idx starts from 1

            parentIdx = *parent_idx;
            if( visited[parentIdx] ) {                           // cycle found
                printerr_cyclefound( _class, _class_name );
                break;
            }
            currIdx = parentIdx;
        } while(1);
    }

}

void ClassTable::init_symboltable() {
    // After inheritance graph is checked, merge user_classes and base_classes
    program_classes = 
        append_Classes( base_classes, user_classes );    

    // build class_map & init symbol table
    int idx;
    Class_ _class;
    Symbol _class_name;
    
    for( idx = program_classes->first();
        program_classes->more(idx);
        idx = program_classes->next(idx) ) {

        _class = program_classes->nth(idx);
        _class_name = _class->get_name();

        // init following
        class_map->addid( _class_name, _class );
        environment[ _class_name ] = new OMTable();
    }
}

Class_ ClassTable::get_class( Symbol s ) {
    Class_ c = class_map->lookup(s);
    return c;
}

void ClassTable::check_class_symboltable_build( Class_ c, Symbol target ) {

    Class_ _class, _class_parent;
    Symbol _class_name, _class_parent_name;

    _class = c;
    _class_name = _class->get_name();

    // For each features, following cases are checked:
    int idx;
    Features features = c->get_features();
    Feature f;

    // note that this function is called multiple times,
    // but error checking is announced only once.
    // by checking if c->get_name == target
    std::vector<bool> isFeatureError( features->len() , false );
    if( ! is_class_base_class(_class_name) ) {     
        for( idx = features->first();
            features->more(idx);
            idx = features->next(idx) ) {

            f = features->nth(idx);
            isFeatureError[idx] = f->check_feature_name_type(c, target) || isFeatureError[idx];       // this may call either attr_class:: or method_class::
            isFeatureError[idx] = f->check_feature_method_formals(c, target) || isFeatureError[idx];
            isFeatureError[idx] = f->check_feature_inheritance(c, target)|| isFeatureError[idx];     // all inheritance scoping is already determined.
        }
    }

    for( idx = features->first();
        features->more(idx);
        idx = features->next(idx) ) {

        f = features->nth(idx);
        if( ! is_class_base_class(_class_name) ) { 
            // Note) the function calls must avoid short circuting
            isFeatureError[idx] = f->check_feature_duplicate(c, target) || isFeatureError[idx]; // check duplicate features
        }
        if( (_class_name != target) && isFeatureError[idx] ){ continue; }        // for parents, admit only one duplicate
        f->add_feature( target );
        
    }
}

void ClassTable::check_types() {

    int idx, idx2;
    Class_ c;
    Features features;
    Feature f;

    for( idx = user_classes->first();
        user_classes->more(idx);
        idx = user_classes->next(idx) ) {       // for each user-defined class

        c = user_classes->nth(idx);
        features = c->get_features();
        
        for( idx2 = features->first();
            features->more(idx2);
            idx2 = features->next(idx2) ) {     // for each feature

            f = features->nth(idx2);
            f->check_type( c );
        }
    }
}

Symbol ClassTable::get_lub( Symbol t1, Symbol t2, Class_ c ){

   if( t1 == t2 ) { return t1; }                         // if equal, return one

   if( t1 == No_type ) { return t2; }                   // exceptionally handle no_type
   if( t2 == No_type ) { return t1; }

   if( t1 == SELF_TYPE || t2 == SELF_TYPE ) {           // set SELF_TYPE with actual type
       if( t1 == SELF_TYPE ) { t1 = c->get_name(); }
       else { t2 = c->get_name(); }
   }

    
    Symbol p1, p2;
    Class_ c1, c2;
    Symbol lub = Object;
    bool breakFlag = false;
    for( p1 = CT->class_map->lookup(t1)->get_name() ;       // from t1 until p1(before object), 
         p1 != Object ;
         p1 = CT->class_map->lookup(p1)->get_parent() 
        ) {

        for( p2 = CT->class_map->lookup(t2)->get_name() ;   // from t2 until p2(before object), 
         p2 != Object ;
         p2 = CT->class_map->lookup(p2)->get_parent()   
        ) {

            if( p1 == p2 ) { lub = p1; breakFlag = true; break; }     // find, directly break.
        }
        if( breakFlag ) { break; }
    }
    return lub;
}

bool ClassTable::is_poset( Symbol left, Symbol right, Class_ c ) {

    if( left == right ) { return true; }
    if( right == Object) { return true; }
    if( right == SELF_TYPE && left == c->get_name() ) { return false; } // special logic : poset not possible                                                   
    if( left == SELF_TYPE || right == SELF_TYPE ) {                     // set SELF_TYPE with actual type
       if( left == SELF_TYPE ) { left = c->get_name(); }
       else { right = c->get_name(); }
   }
    if( left == No_type ) { return true; }
    if( right == No_type && left != No_type ) { return false; }

    Symbol p;
    for( p = CT->class_map->lookup(left)->get_name() ;       // from t1 until p1(before object), 
         p != Object ;
         p = CT->class_map->lookup(p)->get_parent() 
        ) {
            
        if( p == right ) { return true; }                   // found l<r
    }
    if( right == Object ) { return true; }                  // case when l,r = Object
    return false;                                           // otherwise not poset
}

void ClassTable::check_parent_symboltable_build( Symbol s_c, Symbol target ) {

    // case when no parent
    bool isThisNoclass = s_c == No_class;
    if( isThisNoclass ) {
        return;
    }

    Class_ _class, _class_parent;
    Symbol _class_name, _class_parent_name;

    _class = class_map->lookup(s_c);
    _class_name = s_c;
    _class_parent_name = _class->get_parent();
    _class_parent = class_map->lookup(_class_parent_name);
    //assert( _class_name != NULL && _class_parent_name != NULL && _class_parent != NULL );
    
    check_parent_symboltable_build( _class_parent_name , target );    // recursively call parent
    check_class_symboltable_build( _class, target );            // add current table
    environment[target]->enterscope();                          // enter scope for child to start in new scope
}

void ClassTable::check_name_scope() {

    int idx;
    Class_ _class, _class_parent;
    Symbol _class_name, _class_parent_name;
    for( idx = program_classes->first();
        program_classes->more(idx);
        idx = program_classes->next(idx) ) {

        _class = program_classes->nth(idx);
        _class_name = _class->get_name();
        _class_parent_name = _class->get_parent();
        _class_parent = class_map->lookup(_class_parent_name);
        assert( _class_name != NULL && _class_parent_name != NULL );
        
        // check build on _class_parent
        check_parent_symboltable_build( _class_parent_name, _class_name );
        // build on _class_name
        check_class_symboltable_build( _class, _class_name );
        environment[_class_name]->add_attr( self, _class_name ); // add self
    }
}


void ClassTable::check_entrypoint() {

    bool isMainClassExists = is_class_exists_in_program(Main);
    if( ! isMainClassExists ) {
        printerr_main_class_not_exists();
        return;
    }
    Class_ c = class_map->lookup(Main);
    bool isMainMethodDefined = false;
    bool isMainMethodDefinedAndNumParamZero = false;
    Features mainf = c->get_features();
    Feature f; int idx;
    for( idx = mainf->first();
        mainf->more(idx);
        idx = mainf->next(idx) ) {
            
            f = mainf->nth(idx);
            if( f->get_name() == main_meth
                && f->get_formals() != NULL  ) {  // only for M

                isMainMethodDefined = true; 
                if( f->get_formals()->len() == 0 ) {    // first main method should have no parameter
                    isMainMethodDefinedAndNumParamZero = true;
                }
                break;
            }
    }
    if( isMainMethodDefined ) {
        if( ! isMainMethodDefinedAndNumParamZero ) {
            printerr_main_method_param_error( c );
        }
    } else {
        printerr_main_method_not_exists( c ); 
    }
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

void halt_if_error(ClassTable* classtable) {
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

void program_class::semant()
{
    initialize_constants();

    // ===================
    // VERIFICATION UNIT 1
    // ===================

    // Generate classtable by constructor call
    ClassTable *classtable = new ClassTable(classes);
    CT = classtable;
    // Build inheritance graph & Check inheritance validity
    classtable->check_graph_node_build();
    classtable->check_graph_edge_build();
    halt_if_error(classtable);
    classtable->check_inheritance_cycle();
    halt_if_error(classtable);

    // ===================
    // VERIFICATION UNIT 2
    // ===================

    // Scopechecking
    classtable->init_symboltable();
    classtable->check_name_scope();
    
    // Check program entrypoint
    classtable->check_entrypoint();

    // Typechecking
    classtable->check_types();

    // Return & Terminate
    halt_if_error(classtable);
    return;
}