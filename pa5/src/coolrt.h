/* 
 * This file provides the runtime library for cool. It implements
 * the cool classes in C.  Feel free to change it to match the structure
 * of your code generator.
*/

#include <stdbool.h>

typedef struct Object Object;
typedef struct String String;
typedef struct IO IO;
typedef struct Int Int;
typedef struct Bool Bool;

typedef struct Object_vtable Object_vtable;
typedef struct String_vtable String_vtable;
typedef struct IO_vtable IO_vtable;
typedef struct Int_vtable Int_vtable;
typedef struct Bool_vtable Bool_vtable;

/* class type definitions */
struct Object {
	Object_vtable* vtblptr;
};

struct Int {
	Int_vtable* vtblptr;
	int val;
};

struct Bool {
	Bool_vtable* vtblptr;
	bool val;
};

struct String {
	String_vtable* vtblptr;
	char* val;
};

struct IO {
	IO_vtable* vtblptr;
};


/* vtable type definitions */
struct Object_vtable {
	/* ADD CODE HERE */
	// int tag?
	// int null?
	char* name;
	// 4 methods;
};

struct IO_vtable {
	/* ADD CODE HERE */
};

struct Int_vtable {
	/* ADD CODE HERE */
};

struct Bool_vtable {
	/* ADD CODE HERE */
};
   
struct String_vtable {
	/* ADD CODE HERE */
	

};

/* methods in class Object */
Object* Object_new(void);
Object* Object_abort(Object *self);
const String* Object_type_name(Object *self);
Object* Object_copy(Object *self);

/* methods in class IO */
IO* IO_new(void);
void IO_init(IO *self);
IO* IO_out_string(IO *self, String *x);
IO* IO_out_int(IO *self, Int *x);
String* IO_in_string(IO *self);
Int* IO_in_int(IO *self);

/* methods in class Int */
int* Int_new(void);


/* methods in class Bool */
bool* Bool_new(void);


/* methods in class String */
String* String_new(void);
Int* String_length(String *s);
String* String_concat(String *s1, String *s2);
String* String_substr(String *s, Int *st, Int *en);

