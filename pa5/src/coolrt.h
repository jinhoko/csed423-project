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
	const Object_vtable* vtblptr;
};

struct Int {
	const Int_vtable* vtblptr;
	int val;
};

struct Bool {
	const Bool_vtable* vtblptr;
	bool val;
};

struct String {
	const String_vtable* vtblptr;
	char* val;
};

struct IO {
	const IO_vtable* vtblptr;
};


/* vtable type definitions */
struct Object_vtable {
	int tag;
	int objsize;
	char* name;
	Object* (*Object_new)(void);
	
	Object* (*Object_abort)(Object *self);
	const String* (*Object_type_name)(Object *self);
	Object* (*Object_copy)(Object *self);
};

struct IO_vtable {
	int tag;
	int objsize; 
	char* name;
	IO* (*IO_new)(void);

	Object* (*Object_abort)(IO *self);				// Object
	const String* (*Object_type_name)(IO *self);	// Object
	IO* (*Object_copy)(IO *self);					// Object

	void (*IO_init)(IO *self);
	IO* (*IO_out_string)(IO *self, String *x);
	IO* (*IO_out_int)(IO *self, int x);
	String* (*IO_in_string)(IO *self);
	Int* (*IO_in_int)(IO *self);
};

struct Int_vtable {
	int tag;
	int objsize;
	char* name;
	Int* (*Int_new)(void);	

	Object* (*Object_abort)(Int *self);				// Object
	const String* (*Object_type_name)(Int *self);	// Object
	Int* (*Object_copy)(Int *self);					// Object
};

struct Bool_vtable {
	int tag;
	int objsize;
	char* name;
	Bool* (*Bool_new)(void);

	Object* (*Object_abort)(Bool *self);				// Object
	const String* (*Object_type_name)(Bool *self);		// Object
	Bool* (*Object_copy)(Bool *self);					// Object
};
   
struct String_vtable {
	int tag;
	int objsize;
	char* name;
	String* (*String_new)(void);

	Object* (*Object_abort)(String *self);					// Object
	const String* (*Object_type_name)(String *self);		// Object
	String* (*Object_copy)(String *self);					// Object

	Int* (*String_length)(String *s);
	String* (*String_concat)(String *s1, String *s2);
	String* (*String_substr)(String *s, int st, int en);
};

/* methods in class Object */
Object* Object_new(void);
Object* Object_abort(Object *self);
const String* Object_type_name(Object *self);
Object* Object_copy(Object *self);

/* methods in class IO */
IO* IO_new(void);
//void IO_init(IO *self);	// no need
IO* IO_out_string(IO *self, String *x);
IO* IO_out_int(IO *self, int x);						// int
String* IO_in_string(IO *self);
int IO_in_int(IO *self);

/* methods in class Int */
Int* Int_new(void);
void Int_init(Int* self, int val);						// int


/* methods in class Bool */
Bool* Bool_new(void);
void Bool_init(Bool* self, bool val);					// bool


/* methods in class String */
String* String_new(void);
int String_length(String *s);
String* String_concat(String *s1, String *s2);
String* String_substr(String *s, int st, int len);		// int x2

/* etc */
void* handle_malloc( int size, Object* self );