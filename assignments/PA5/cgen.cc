
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <map>
#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

static int stack_offset = 0;

static int label_no = 0;

static int get_unique_label() {
    return label_no++;
}

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

static std::string *get_dispatch_label(Symbol class_name) {
    return new std::string(std::string(class_name->get_string()) + DISPTAB_SUFFIX);
}

static std::string *get_proto_label(Symbol class_name) {
    return new std::string(std::string(class_name->get_string()) + PROTOBJ_SUFFIX);
}

static std::string *get_init_label(Symbol class_name) {
    return new std::string(std::string(class_name->get_string()) + CLASSINIT_SUFFIX);
}

static std::string *get_method_label(Symbol class_name, Symbol method_name) {
    return new std::string(std::string(class_name->get_string()) + METHOD_SEP + method_name->get_string());
}

static void emit_default_code_ref_for_type(Symbol type, ostream &s) {
    if (type == Str) {
        static_cast<StringEntry *>(stringtable.lookup_string(""))->code_ref(s);
    } else if (type == Bool) {
        falsebool.code_ref(s); 
    } else if (type == Int) {
        static_cast<IntEntry *>(inttable.lookup_string("0"))->code_ref(s);
    } else {
        s << 0;
    }
}

static void write_default_value_for_attr(CgenNode *cls, ostream &s) {
    s << WORD;
    emit_default_code_ref_for_type(cls->get_name(), s);
    s << endl;
}

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_load_byte(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LB << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_load_self(char *dst, ostream &s) {
    emit_load(dst, -1, FP, s);
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_j(const char *address, ostream &s) {
    s << J << address << endl;
}

static void emit_jump_to_label(int label, ostream &s) {
    emit_j((std::string("label") + std::to_string(label)).c_str(), s);
}

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

static void emit_and(char *dst, char *src1, char *src2, ostream &s) {
    s << AND << dst << " " << src1 << " " << src2 << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  stack_offset--;
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Pop a word off the stack and store it in a register. The stack shrinks towards larger addresses.
//
static void emit_pop(char *reg, ostream& str)
{
  stack_offset++;
  emit_addiu(SP,SP,4,str);
  emit_load(reg,0,SP,str);
}

//
// Pop a word off the stack and ignore its value.
//
static void emit_pop(ostream& str)
{
  stack_offset++;
  emit_addiu(SP,SP,4,str);
}

// Emits a new object of the given type in $a0.
static void emit_new_object(Symbol type_name, ostream &s) {
    // Store the address of the prototype object in $a0.
    emit_load_address(ACC, get_proto_label(type_name)->c_str(), s);
    // Get a fresh copy of an integer object in $a0.
    emit_jal(get_method_label(Object, ::copy)->c_str(), s);
    // Call the initializer method on the object
    emit_jal(get_init_label(type_name)->c_str(), s);
}

static void emit_load_variable_address(char *dst, VariableInfo *var_info, ostream &s) {
    switch (var_info->get_scope_type()) {
        case PARAM:
            // Directly load the address of the variable.
            emit_addiu(dst, FP, WORD_SIZE * var_info->get_offset(), s);
            return;
        case ATTRIBUTE:
            // Load 'self' into $a0, located just below the frame pointer.
            emit_load_self(dst, s);
            // Load the address, which is some offset from self.
            emit_addiu(dst, dst, WORD_SIZE * (DEFAULT_OBJFIELDS + var_info->get_offset()), s);
            return;
        default:
            // Unreachable.
            assert(false);
    }
}

// The address of the variable is stored in the $dst register.
static void emit_load_variable_value(char *dst, VariableInfo *var_info, ostream &s) {
    switch (var_info->get_scope_type()) {
        case PARAM:
            // Directly load the value itself from the frame pointer.
            emit_load(dst, var_info->get_offset(), FP, s);
            return;
        case ATTRIBUTE:
            // Load 'self' into $a0, located just below the frame pointer.
            emit_load_self(dst, s);
            // Load the value itself.
            emit_load(dst, DEFAULT_OBJFIELDS + var_info->get_offset(), dst, s);
            return;
        default:
            // Unreachable.
            assert(false);
    }
}

static void emit_print_error_msg_and_exit(char *msg, ostream &s) {
    emit_load_string(ACC, stringtable.lookup_string(msg), s);
    emit_addiu(ACC, ACC, (DEFAULT_OBJFIELDS + 1) * WORD_SIZE, s);
    // Print the error message to the screen.
    emit_load_imm("$v0", 4, s);
    s << "\tsyscall" << endl;
    // Exit the program early.
    emit_load_imm("$v0", 10, s);
    s << "\tsyscall" << endl;
}

//
// Stores callee-saved registers on the stack.
//
static void setup_activation_record_for_callee(ostream &str) {
    stack_offset = -1;
    emit_push(ACC, str);
    emit_push(FP, str);
    emit_push(RA, str);
    // Set up the new frame pointer to point to the first parameter on the stack.
    emit_addiu(FP, SP, 4 * WORD_SIZE, str);
}

//
// Loads callee-saved registers by popping the contents of the stack.
//
static void cleanup_activation_record_for_caller(ostream &str) {
    emit_pop(RA, str);
    emit_pop(FP, str);
    emit_pop(str); // Simply discard the old $a0 value; $a0 is now holding the return value.
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << GC_TAG << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD << *get_dispatch_label(Str) << endl;

      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << GC_TAG << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD << *get_dispatch_label(Int) << endl;

      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << GC_TAG << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD << *get_dispatch_label(Bool) << endl;

      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  if (cgen_debug) {
      str << endl << GLOBAL << "breakpoint";
  }
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************
//

static char *CASE_ERROR = "Error: no matching case in switch!\n";
static char *VOID_DISPATCH_ERROR = "Error: dispatch on void!\n";

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  stringtable.add_string(CASE_ERROR);
  stringtable.add_string(VOID_DISPATCH_ERROR);
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = 4;
   intclasstag =    2;
   boolclasstag =   3;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   determine_offsets();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

//
// CgenClassTable::determine_offsets
//
void CgenClassTable::determine_offsets()
{
    determine_offsets(root(), 0, 0);
}

void CgenClassTable::generate_class_name_table() {
    assign_basic_class_tags();
    assign_nonbasic_class_tags(root());

    // Create the name table
    str << CLASSNAMETAB << LABEL;
    for (int i = 0; i < class_tag_table.size(); i++) {
        str << WORD;
        StringEntry *entry = static_cast<StringEntry *>(stringtable.lookup_string(class_tag_table[i]->get_name()->get_string()));
        entry->code_ref(str);
        str << endl;
    }

    // Create the hierarchy table, where table[i] = tag(parent(class with i))
    str << CLASS_HIERARCHY << LABEL;
    // Object does not have a parent.
    str << WORD << -1 << endl;
    for (int i = 1; i < class_tag_table.size(); i++) {
        // Get the tag number of the parent
        str << WORD << class_tag_table[i]->get_parentnd()->get_tag_number() << endl;
    }
}

void CgenClassTable::assign_basic_class_tags() {
    assign_next_class_tag(probe(Object)); // 0
    assign_next_class_tag(probe(::IO));    // 1
    assign_next_class_tag(probe(Int));    // 2
    assign_next_class_tag(probe(Bool));   // 3
    assign_next_class_tag(probe(Str));    // 4
}
    

void CgenClassTable::assign_next_class_tag(CgenNode *curr) {
    class_tag_table.push_back(curr);
    curr->set_tag_number(class_tag_table.size() - 1);
}

void CgenClassTable::assign_nonbasic_class_tags(CgenNode *curr) {
    // Class tags at least have the following property:
    // If A <= B, then idx(A) >= idx(B), or if idx(A) < idx(B), then A is not a B.
    if (!curr->basic()) {
        assign_next_class_tag(curr);
    }
    List<CgenNode> *child = curr->get_children();
    while (child != NULL) {
        assign_nonbasic_class_tags(child->hd());
        child = child->tl();
     }
}

void CgenClassTable::generate_class_object_table() {
    str << CLASSOBJTAB << LABEL;
    List<CgenNode> *curr = nds;
    for (int i = 0; i < class_tag_table.size(); i++) {
        str << WORD << class_tag_table[i]->get_name() << DISPTAB_SUFFIX << endl;
        str << WORD << class_tag_table[i]->get_name() << CLASSINIT_SUFFIX << endl;
    }
}


void CgenClassTable::generate_dispatch_tables() {
    List<CgenNode> *curr = nds;
    while (curr != NULL) {
        curr->hd()->generate_dispatch_table(str);
        curr = curr->tl();
    }
}

void CgenNode::generate_dispatch_table(ostream &s) {
    s << name << DISPTAB_SUFFIX << LABEL;
    std::list<SymtabEntry<Symbol, int>*> *entries = method_indices.all_flattened_entries();
    for (const auto &entry : *entries) {
        s << WORD << *method_impls.lookup(entry->get_id()) << endl;
    }
}

void CgenClassTable::determine_offsets(CgenNodeP curr, int starting_method_index, int starting_attr_offset)
{
    method_indices.enterscope();
    variable_scope.enterscope();
    method_impls.enterscope();
    std::pair<int, int> pair = curr->determine_offsets(&method_indices, &method_impls, &variable_scope, starting_method_index, starting_attr_offset);
    starting_method_index = pair.first;
    starting_attr_offset = pair.second;
    List<CgenNode> *child = curr->get_children();
    while (child != NULL) {
        determine_offsets(child->hd(), starting_method_index, starting_attr_offset);
        child = child->tl();
    }
    method_impls.exitscope();
    method_indices.exitscope();
    variable_scope.exitscope();
}


void CgenClassTable::generate_proto_objects() {
    List<CgenNode> *curr = nds;
    while (curr != NULL) {
        CgenNode *curr_class = curr->hd();

        int tag = curr_class->get_tag_number();
        assert(tag > -1);

        str << WORD << GC_TAG << endl;

        str << *get_proto_label(curr_class->get_name()) << LABEL
            << WORD << tag << endl  // class tag
            << WORD <<  DEFAULT_OBJFIELDS + curr_class->get_variable_scope().count() << endl   // object size
            << WORD << *get_dispatch_label(curr_class->get_name()) << endl;

        std::list<SymtabEntry<Symbol, VariableInfo>*> *entries = curr_class->get_variable_scope().all_flattened_entries();
        for (const auto &entry : *entries) {
            CgenNode *attr_cls = lookup(entry->get_info()->get_type());
            assert(attr_cls != NULL);
            write_default_value_for_attr(attr_cls, str);
        }
        curr = curr->tl();
    }
}

void CgenClassTable::generate_init_methods() {
    List<CgenNode> *curr = nds;
    while (curr != NULL) {
        generate_init_method(curr->hd());
        curr = curr->tl();
    }
}

void CgenClassTable::generate_init_method(CgenNode *cls) {
    str << cls->get_name() << CLASSINIT_SUFFIX << LABEL;
    
    // Store callee-saved registers on the stack.
    setup_activation_record_for_callee(str);

    // Call the parent initializer unless this is the root (Object).
    if (cls->get_name() != Object) {
        emit_jal(get_init_label(cls->get_parentnd()->get_name())->c_str(), str);
    }

    // Only look through the attributes that were defined in this class, not parent classes.
    std::list<attr_class *> attrs = cls->get_attributes();

    // Iterate through attributes and initialize each to its assigned or default value.
    for (const auto &attr : attrs) {
        Expression expr = attr->get_init();
        VariableScope scope = cls->get_variable_scope();
        VariableInfo *attr_info = scope.lookup(attr->name);
        assert(attr_info != NULL);

        if (!expr->is_empty()) {
            // Save the contents of $a0 on the stack.
            emit_push(ACC, str);

            // Evaluate the expression.
            expr->code(this, scope, str);

            // The return value should be recorded in $a0.
            // Store $a0 in a temporary.
            emit_move(T1, ACC, str);

            // Pop the old $a0 off the stack and load it into $a0.
            emit_pop(ACC, str);

            // Store the result at the appropriate offset from $a0.
            emit_store(T1, DEFAULT_OBJFIELDS + attr_info->get_offset(), ACC, str);
        }
    }

    // Restore callee-saved registers.
    cleanup_activation_record_for_caller(str);

    // Return control to the caller.
    emit_return(str);
}

void CgenClassTable::generate_class_methods() {
    List<CgenNode> *curr = nds;
    while (curr != NULL) {
        if (!curr->hd()->basic()) {
            curr->hd()->generate_class_methods(this, str);
        }
        curr = curr->tl();
    }
}

void CgenNode::generate_class_methods(CgenClassTable *table, ostream &str) {
    for (const auto &method : get_methods()) {
        emit_method_ref(get_name(), method->get_name(), str);
        str << LABEL;

        // Add the method parameters to the scope.
        VariableScope scope = get_variable_scope();
        scope.enterscope();
        for (int i = method->formals->first(); method->formals->more(i); i = method->formals->next(i)) {
            Formal_class *formal = method->formals->nth(i);
            scope.addid(formal->get_name(), new VariableInfo(i, formal->get_type_decl(), ScopeType::PARAM));
        }
        // Bound self to the class, treating it as another parameter.
        // The offset is -1 wrt the frame pointer.
        scope.addid(self, new VariableInfo(-1, name, ScopeType::PARAM));

        // Save the callee-saved registers on the stack.
        setup_activation_record_for_callee(str);

        // Generate code for the expression. The value of the expression should be 
        // stored in $a0.
        method->get_expression()->code(table, scope, str);
        scope.exitscope();

        // Restore callee-saved registers.
        cleanup_activation_record_for_caller(str);

        // Pop the params off of the stack.
        int num_params = method->get_formals()->len();
        if (num_params > 0) {
            emit_addiu(SP, SP, WORD_SIZE * num_params, str);
        }

        // Return control to the caller.
        emit_return(str);
    }
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

std::string *to_method_label(class__class *clz, method_class *method) {
    return new std::string(std::string(clz->get_name()->get_string()) + "." + std::string(method->get_name()->get_string()));
}

std::pair<int, int> CgenNode::determine_offsets(MethodIdxTable *method_indices, MethodImplTable *method_impls, VariableScope *variable_scope, int starting_method_index, int starting_attr_offset) {
    // Initialize the offsets so we start where the parent class left off.
    this->next_method_index = starting_method_index;
    this->next_attr_offset = starting_attr_offset;

    for (const auto &method : get_methods()) {
        Symbol name = method->get_name();
        if (method_indices->probe(name)) {
            // A duplicate method is defined in the class, which should be impossible at this stage.
            assert(false);
        }
        int *existing_index = method_indices->lookup(name);
        if (!existing_index) {
            method_indices->addid(name, new int(get_and_increment_method_index()));
        }
        // Unlike the method index, the method label will get overridden if there is an overriding
        // implementation in a child class.
        method_impls->addid(name, to_method_label(this, method));
    }
    for (const auto &attr : get_attributes()) {
        Symbol name = attr->get_name();
        if (variable_scope->probe(name)) {
            // A duplicate method is defined in the class, which should be impossible at this stage.
            assert(false);
        }
        VariableInfo *existing_offset = variable_scope->lookup(name);
        int offset;
        if (!existing_offset) {
            variable_scope->addid(name, new VariableInfo(
                        get_and_increment_attr_offset(), attr->type_decl, ScopeType::ATTRIBUTE));
        }
    }

    // Create independent copies.
    this->method_indices = *method_indices; 
    this->variable_scope = *variable_scope; 
    this->method_impls = *method_impls; 

    return std::pair<int, int>(next_method_index, next_attr_offset);
}


void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "generating dispatch tables" << endl;
  generate_class_name_table();
  generate_class_object_table();
  generate_dispatch_tables();
  generate_proto_objects();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  generate_init_methods();

  generate_class_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

CgenNode *CgenClassTable::find_class(Symbol class_name) {
    List<CgenNode> *curr = nds;
    while (curr != NULL) {
        CgenNode *nd = curr->hd();
        if (nd->name == class_name) {
            return nd;
        }
        curr = curr->tl();
    }
    return NULL;
}

int CgenClassTable::get_method_index(VariableScope &scope, Symbol static_type, Symbol method_name) {
    if (static_type == SELF_TYPE) {
        // Resolve to the class self is bound to.
        VariableInfo *var_info = scope.lookup(self);
        static_type = var_info->get_type();
    }
    // TODO: simply replace with probe
    CgenNode *cls = find_class(static_type);
    assert(cls != NULL);
    return cls->get_method_idx(method_name);
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // First evaluate the expression. The result is in $a0.
    expr->code(helper, scope, s);
    VariableInfo *var_info = scope.lookup(name);
    assert(var_info != NULL);
    // Load the address of the variable in $t1.
    emit_load_variable_address(T1, var_info, s);
    // Store the new value at the variable address.
    emit_store(ACC, 0, T1, s);
    if (var_info->get_scope_type() == ATTRIBUTE) {
        if (cgen_Memmgr == GC_GENGC) {
            // Notify the garbage collector of the address of the attribute.
            emit_move(A1, T1, s);
            emit_jal("_GenGC_Assign", s);
        }
    }
}

void static_dispatch_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    int n = actual->len();
    if (n > 0) {
        // Allocate space on the stack for the additional arguments.
        emit_addiu(SP, SP, -4*n, s);
        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            Expression expr = actual->nth(i);
            expr->code(helper, scope, s);
            // Store the result in $a0 at the corresponding offset in the stack.
            // Skip over the empty value.
            emit_store(ACC, 1 + i, SP, s);
        }
    }
    if (expr->is_empty()) {
        emit_load_self(ACC, s);
    } else {
        // Now evaluate the expression. The result should now be in $a0.
        expr->code(helper, scope, s);
    }
    // Get the dispatch table for the target object.
    emit_load_address(T1, get_dispatch_label(type_name)->c_str(), s);
    // Get the offset from the dispatch table
    int method_idx = helper->get_method_index(scope, type_name, name);
    emit_load(T1, method_idx, T1, s);
    // Call the method. The callee handles cleaning up the stack.
    emit_jalr(T1, s);
}



void dispatch_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    int n = actual->len();
    if (n > 0) {
        // Allocate space on the stack for the additional arguments.
        emit_addiu(SP, SP, -4*n, s);
        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            Expression expr = actual->nth(i);
            expr->code(helper, scope, s);
            // Store the result in $a0 at the corresponding offset in the stack.
            // Skip over the empty value.
            emit_store(ACC, 1 + i, SP, s);
        }
    }
    if (expr->is_empty()) {
        emit_load_self(ACC, s);
    } else {
        // Now evaluate the expression. The result should now be in $a0.
        expr->code(helper, scope, s);
        // Check if $a0 is NULL. If it is we must throw a runtime error.
        int valid_dispatch_label = get_unique_label();
        emit_bne(ACC, ZERO, valid_dispatch_label, s);
        emit_print_error_msg_and_exit(VOID_DISPATCH_ERROR, s);
        emit_label_def(valid_dispatch_label, s);
    }
    // Get the dispatch table for the target object.
    emit_load(T1, 2, ACC, s);
    // Get the offset from the dispatch table
    int method_idx = helper->get_method_index(scope, expr->get_type(), name);
    emit_load(T1, method_idx, T1, s);
    // Call the method. The callee handles cleaning up the stack.
    emit_jalr(T1, s);
}

void cond_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // First evaluate the expression. The result should be in $a0.
    pred->code(helper, scope, s);
    int false_branch = get_unique_label();
    int end_label = get_unique_label();
    // Load the raw boolean value into $a0.
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    emit_beqz(ACC, false_branch, s);
    // Execute the true branch.
    then_exp->code(helper, scope, s);
    emit_jump_to_label(end_label, s);
    // Execute the false branch.
    emit_label_def(false_branch, s);
    else_exp->code(helper, scope, s);
    emit_label_def(end_label, s);
    // $a0 will contain the result of the executed branch.
}

void loop_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    int label_pred = get_unique_label();
    int label_after = get_unique_label();
    emit_label_def(label_pred, s);
    // Evaluate the predicate. The result is in $a0.
    pred->code(helper, scope, s);
    // Load the raw integer value of the Bool.
    emit_fetch_int(ACC, ACC, s);
    emit_load_imm(T1, TRUE, s);
    // Break if the predicate is not true.
    emit_bne(ACC, T1, label_after, s);
    // If the predicate is true continue executing the body.
    body->code(helper, scope, s); 
    // Jump to the predicate.
    emit_j((std::string("label") + std::to_string(label_pred)).c_str(), s);
    emit_label_def(label_after, s);
    // void must be returned upon termination.
    emit_load_imm(ACC, 0, s);
}

// Stores in the destination register whether type A is a subtype of type B.
static void emit_is_subtype(char *dst, int tag_a, int tag_b, ostream &s) {
    int label_true = get_unique_label();
    int label_false = get_unique_label();
    int label_end = get_unique_label();
    int label_loop = get_unique_label();

    emit_load_imm(T1, tag_a, s);
    emit_load_imm(T2, tag_b, s);

    // Loop
    emit_label_def(label_loop, s);
    emit_load_imm(T3, -1, s);
    // If we have reached the parent of the Object class return false.
    emit_beq(T1, T3, label_false, s);
    // If the tags are equal return true.
    emit_beq(T1, T2, label_true, s);
    // Replace T2 with its parent
    emit_load_address(T3, CLASS_HIERARCHY, s);
    // Get the address of the parent tag is located
    // Store T3 on the stack.
    emit_push(T3, s); 
    emit_load_imm(T3, WORD_SIZE, s);
    emit_mul(T1, T1, T3, s);
    // Restore T3 as the class hierarchy address.
    emit_pop(T3, s);
    // Compute the address of T2's parent tag.
    emit_addu(T3, T3, T1, s);
    // Load the parent tag into T2.
    emit_load(T1, 0, T3, s);
    // Repeat.
    emit_jump_to_label(label_loop, s);

    // Jump here in the false case.
    emit_label_def(label_false, s);
    emit_load_imm(dst, 0, s);
    emit_jump_to_label(label_end, s);

    // Jump here in the true case.
    emit_label_def(label_true, s);
    emit_load_imm(dst, 1, s);

    // End here.
    emit_label_def(label_end, s);
}

int CgenClassTable::get_class_tag(Symbol class_name) {
    return probe(class_name)->get_tag_number();
};

void typcase_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // Sort the cases by their class tags in descending order.
    // This way, the first case that matches is guaranteed to be "minimal",
    // i.e. the most specific match.
    std::list<Case> cases;
    for (int i = this->cases->first(); this->cases->more(i); i = this->cases->next(i)) {
        cases.push_back(this->cases->nth(i));
    }
    cases.sort([helper](const Case a, const Case b) {
            return helper->get_class_tag(a->get_type_decl()) > helper->get_class_tag(b->get_type_decl());
            });

    std::vector<int> labels;
    for (int i = 0; i < cases.size() + 1; i++) {
        int unique_label = get_unique_label();
        labels.push_back(unique_label);
    }
    int success_label = get_unique_label();
    // Execute the expression and store its value on the stack.
    expr->code(helper, scope, s);
    // The result should be in $a0.
    emit_push(ACC, s);

    int i = 0;
    for (const auto &cs : cases) {
        emit_label_def(labels[i], s);
        emit_is_subtype(T1, helper->get_class_tag(expr->get_type()), helper->get_class_tag(cs->get_type_decl()), s);
        // Jump to the next label if it's not a match.
        emit_beq(T1, ZERO, labels[i+1], s);
        scope.enterscope();
        // Now we assume it is match. We must add the variable to the scope and execute the expression.
        scope.addid(cs->get_name(), new VariableInfo(stack_offset + 1, cs->get_type_decl(), ScopeType::PARAM));
        cs->get_expression()->code(helper, scope, s);
        scope.exitscope();
        // Now we must jump to the success label.
        emit_jump_to_label(success_label, s);
        i++;
    }
    // Final label, only used if no cases match.
    emit_label_def(labels[labels.size() - 1], s);
    emit_print_error_msg_and_exit(CASE_ERROR, s);

    // We will jump here upon successful completion.
    emit_label_def(success_label, s);

    // Pop the result of the expression from the stack.
    emit_pop(s);

}

void block_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        body->nth(i)->code(helper, scope, s);
    }
    // $a0 will contain the result of evaluating the last
    // expression in the body.
}

void let_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // store the temporary variable on the stack a known offset from the frame pointer.
    // Record the offset BEFORE pushing.
    scope.enterscope();
    // Add the temporary variable to the scope.
    scope.addid(identifier, new VariableInfo(stack_offset, type_decl, ScopeType::PARAM));
    if (init->is_empty()) {
        emit_partial_load_address(T1, s);
        emit_default_code_ref_for_type(type_decl, s);
        s << endl;
        emit_push(T1, s);
    } else {
        init->code(helper, scope, s);
        // The result should be in $a0.
        emit_push(ACC, s);
    }
    // Execute the body. The result should be in $a0.
    body->code(helper, scope, s);
    scope.exitscope();
    // Pop the value off of the stack; it's no longer in scope.
    emit_pop(s);
}

static void emit_arith(void (*emit_binary_op)(char*,char*,char*,ostream &s), Binary_operation *op, ExpressionHelper *helper, VariableScope scope, ostream &s) {
    // Execute the first expression; its result is in $a0.
    op->e1->code(helper, scope, s);                
    // Load the actual integer value into a register. The actual value is the first attribute.
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    // Store the value on the stack.
    emit_push(ACC, s);
    // Execute the second expression; its result is in $a0.
    op->e2->code(helper, scope, s);
    // Load the actual integer value.
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    // Restore the result of the first expression to $t1.
    emit_pop(T1, s);
    // Compute the operation.
    emit_binary_op(T1, T1, ACC, s);  
    // Store the result on the stack.
    emit_push(T1, s);
    // Store the address of the prototype object in $a0.
    emit_load_address(ACC, get_proto_label(Int)->c_str(), s);
    // Get a fresh copy of an integer object in $a0.
    emit_jal(get_method_label(Object, ::copy)->c_str(), s);
    // Restore the result of the operation to $t1.
    emit_pop(T1, s);
    // Initialize the numerical value of the new int object to the sum.
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void plus_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    emit_arith(emit_addu, this, helper, scope, s);
}

void sub_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    emit_arith(emit_sub, this, helper, scope, s);
}

void mul_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    emit_arith(emit_mul, this, helper, scope, s);
}

void divide_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    emit_arith(emit_div, this, helper, scope, s);
}

void neg_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
}

void lt_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // The result of the first expression is in $a0.
    e1->code(helper, scope, s);
    // Load the raw integer value into $a0.
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    // Store the value on the stack.
    emit_push(ACC, s);
    // Execute the second expression. The result is in $a0.
    e2->code(helper, scope, s);
    // Load the raw value into $a0.
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    // Pop the result of the first expression into $t1.
    emit_pop(T1, s);
    int true_branch = get_unique_label();
    int end_label = get_unique_label();
    emit_blt(T1, ACC, true_branch, s);
    // false case
    emit_load_imm(T1, 0, s);
    emit_jump_to_label(end_label, s);
    emit_label_def(true_branch, s);
    emit_load_imm(T1, 1, s);
    emit_label_def(end_label, s);
    // Store the raw value on the stack.
    emit_push(T1, s);
    // $a0 contains the new Bool object.
    emit_new_object(Bool, s);
    emit_pop(T1, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

// Emits into dst whether $src1 == $src2.
static void emit_eq(char *dst, char *src1, char* src2, ostream &s) {
    int true_branch = get_unique_label();
    int end_label = get_unique_label();
    emit_beq(src1, src2, true_branch, s);
    // False case.
    emit_load_imm(dst, 0, s);
    emit_jump_to_label(end_label, s);
    emit_label_def(true_branch, s);
    // True case.
    emit_load_imm(dst, 1, s);
    emit_label_def(end_label, s);
}

// Emits into dst whether the character sequence starting at address $src1 exactly equals
// the character string starting at address $src2.
static void emit_cmp_strs(char *dst, char *src1, char *src2, ostream &s) {
    int loop_label = get_unique_label();
    int true_branch = get_unique_label();
    int false_branch = get_unique_label();
    int end_label = get_unique_label();

    // Defensively push the addresses to the top of the stack and $a0, respectively.
    emit_push(src1, s);
    emit_move(ACC, src2, s);

    emit_label_def(loop_label, s);
    // Load the first address into $t1.
    emit_pop(T1, s);
    // Load the first value into $t2.
    emit_load_byte(T2, 0, T1, s);
    // Push the address onto the stack.
    emit_push(T1, s);
    // Move the first value to $t1.
    emit_move(T1, T2, s); 
    // Load the second value into $t2.
    emit_load_byte(T2, 0, ACC, s);
    // Compare $t1 and $t2.
    emit_bne(T1, T2, false_branch, s);
    // Check if we have reached the end of the string.
    emit_beqz(T1, true_branch, s);
    // Increment the pointers.
    emit_addiu(ACC, ACC, 1, s);
    emit_pop(T1, s);
    emit_addiu(T1, T1, 1, s);
    emit_push(T1, s);
    // Repeat.
    emit_jump_to_label(loop_label, s);


    emit_label_def(true_branch, s);
    emit_load_imm(dst, 1, s);
    emit_jump_to_label(end_label, s);

    emit_label_def(false_branch, s);
    emit_load_imm(dst, 0, s);

    emit_label_def(end_label, s);
    // Pop the first address from the stack. It was never removed earlier.
    emit_pop(s);
}


void eq_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // The result of e1 should be in $a0.
    e1->code(helper, scope, s);
    if (e1->get_type() == Int || e1->get_type() == Bool) {
        assert(e2->get_type() == Int);
        // $t1 now contains the raw value.
        emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
        // push $t1 on the stack.
        emit_push(T1, s);
        // $a0 now contains the result of the second expression.
        e2->code(helper, scope, s);
        emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
        emit_pop(T1, s);
        // $t1 will contain the raw boolean of the equality check.
        emit_eq(T1, T1, ACC, s);
    } else if (e1->get_type() == Str) {
        // We need to do a string comparison
        // Load the raw strings into temporary registers.
        emit_addiu(T1, ACC, (DEFAULT_OBJFIELDS + 1)*WORD_SIZE, s);
        emit_push(T1, s);
        e2->code(helper, scope, s);
        emit_addiu(ACC, ACC, (DEFAULT_OBJFIELDS + 1)*WORD_SIZE, s);
        emit_pop(T1, s);
        // The string pointers are now in $t1 and $a0.
        emit_cmp_strs(T1, T1, ACC, s);
    } else {
        // Push the address of e1 directly onto the stack.
        emit_push(ACC, s);
        // Evaluate e2. $a0 contains its result.
        e2->code(helper, scope, s);
        // Pop the result of e1 from the stack.
        emit_pop(T1, s);
        // Simply compare the values of the pointers.
        emit_eq(T1, T1, ACC, s);
    }

    // Store the raw result on the stack.
    emit_push(T1, s);
    // After this $a0 should contain the address of the new object.
    emit_new_object(Bool, s);
    // Pop the value to use from the stack.
    emit_pop(T1, s);
    // Set the correct value to use.
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void leq_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // The result of the first expression is in $a0.
    e1->code(helper, scope, s);
    // Load the raw integer value into $a0.
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    // Store the value on the stack.
    emit_push(ACC, s);
    // Execute the second expression. The result is in $a0.
    e2->code(helper, scope, s);
    // Load the raw value into $a0.
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    // Pop the result of the first expression into $t1.
    emit_pop(T1, s);
    int true_branch = get_unique_label();
    int end_label = get_unique_label();
    emit_blt(T1, ACC, true_branch, s);
    emit_beq(T1, ACC, true_branch, s);
    // false case
    emit_load_imm(T1, 0, s);
    emit_jump_to_label(end_label, s);
    emit_label_def(true_branch, s);
    emit_load_imm(T1, 1, s);
    emit_label_def(end_label, s);
    // Store the raw value on the stack.
    emit_push(T1, s);
    // $a0 contains the new Bool object.
    emit_new_object(Bool, s);
    emit_pop(T1, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void comp_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // Evaluate the expression; the result should be in $a0.
    e1->code(helper, scope, s);
    // Get the raw boolean value in $a0.
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    // Add 1.
    emit_addiu(ACC, ACC, 1, s);
    // Store 0x1 (the mask) in $t1.
    emit_load_imm(T1, 0x1, s);
    // Mask everything but the ones digit.
    emit_and(ACC, ACC, T1, s);
    // Save the value on the stack.
    emit_push(ACC, s);
    // Create a new Boolean object; its address is in $a0.
    emit_new_object(Bool, s);
    // Pop the negated value into $t1.
    emit_pop(T1, s);
    // Store the new value.
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void int_const_class::code(ExpressionHelper *helper, VariableScope &scope, ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ExpressionHelper *helper, VariableScope &scope, ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ExpressionHelper *helper, VariableScope &scope, ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    emit_new_object(type_name, s);
}

void isvoid_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    int label_void = get_unique_label();
    int label_after = get_unique_label();
    // Execute the passed expression. The result is in $a0.
    e1->code(helper, scope, s);
    emit_beqz(ACC, label_void, s);
    emit_load_imm(T1, 0, s);
    emit_jump_to_label(label_after, s);
    emit_label_def(label_void, s);
    emit_load_imm(T1, 1, s);
    emit_label_def(label_after, s);
    // After this $a0 will contain the address of the resulting Boolean.
    emit_new_object(Bool, s);
    // Set the inner boolean value to $t1, which contains isvoid.
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void no_expr_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    // Nothing to do.
}

void object_class::code(ExpressionHelper *helper, VariableScope &scope, ostream &s) {
    VariableInfo *var_info = scope.lookup(name);
    assert(var_info != NULL);
    emit_load_variable_value(ACC, var_info, s);
}


