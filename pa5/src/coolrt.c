#include "coolrt.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* This file provides the runtime library for cool. It implements
   the functions of the cool classes in C 
   */

/* Class name strings */
// const char Object_string[] 	= "Object";
// const char String_string[] 	= "String";
// const char Int_string[] 	= "Int";
// const char Bool_string[] 	= "Bool";
// const char IO_string[] 		= "IO";

char default_string[]	= "";

/* Class vtable prototypes */
// will be linked from the cgen program.
extern const Object_vtable Object_vtable_prototype;
extern const String_vtable String_vtable_prototype;
extern const IO_vtable IO_vtable_prototype;
extern const Int_vtable Int_vtable_prototype;
extern const Bool_vtable Bool_vtable_prototype;

/*
	All Class_new methods
*/
Object* Object_new(void) {
	Object* ptr = handle_malloc(sizeof(Object), NULL);
	ptr->vtblptr = &Object_vtable_prototype;	// init 0
	return ptr;
}
IO* IO_new(void) {
	IO* ptr = handle_malloc(sizeof(IO), NULL);
	ptr->vtblptr = &IO_vtable_prototype;		// init 0
	return ptr;
}
Int* Int_new(void) {
	Int* ptr = handle_malloc(sizeof(Int), NULL);
	ptr->vtblptr = &Int_vtable_prototype;		// init 0
	ptr->val = 0;								// init 1
	return ptr;
}
Bool* Bool_new(void) {
	Bool* ptr = handle_malloc(sizeof(Bool), NULL);
	ptr->vtblptr = &Bool_vtable_prototype;		// init 0
	ptr->val = false;							// init 1
	return ptr;
}
String* String_new(void) {
	String* ptr = handle_malloc(sizeof(String), NULL);
	ptr->vtblptr = &String_vtable_prototype;	// init 0
	ptr->val = default_string;					// init 1
	return ptr;
}

/*
// Methods in class object
*/
Object* Object_abort(Object *self)
{
	printf("Abort called from class %s\n",
	       !self? "Unknown" : self->vtblptr->name);
	exit(1);
	return self;
}

const String* Object_type_name(Object *self)
{
	if (self == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
		abort();
	}
	String *s = String_new();
	s->val = self->vtblptr->name;
	return s;
}

Object* Object_copy(Object *self) {
	Object* newobj = Object_new();
	memcpy(newobj, self, sizeof(Object));
	return newobj;
}

/*
// Methods in class IO
*/

IO* IO_out_string(IO *self, String* x)
{
	if (self == 0 || x == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__): NULL object\n");
		abort();
	}
	printf("%s",x->val);
	fflush(stdout);			// for immediate print
	return self;
}

IO* IO_out_int(IO *self, int x)
{
	// if (self == 0 || x == 0) {
	// 	fprintf(stderr, "At __FILE__(line __LINE__): NULL object\n");
	// 	abort();
	// }
	// printf("%d",x->val);

	if (self == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__): NULL object\n");
		abort();
	}
	printf("%d", x );
	fflush(stdout);			// for immediate print
	return self;
}


/*
 * Get one line from stream using get_line(), then discard newline character.
 * Allocate string *in_string_p and store result there.
 * Return number of chars read. 
 */
static int get_one_line(char** in_string_p, FILE* stream)
{
	/* Get one line worth of input */
	size_t len = 0;
	ssize_t num_chars_read;
	num_chars_read = getline(in_string_p, &len, stdin);
	if (*in_string_p == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__):\n   ");
		fprintf(stderr, "    allocation failed in IO::in_string()\n");
		exit(1);
	}
	
	/* Discard the newline char, if any.  It may not exist if EOF reached. */
	if (num_chars_read > 0 && (*in_string_p)[num_chars_read-1] == '\n') {
		(*in_string_p)[num_chars_read-1] = '\0';
		--len;
	}

	return len;
}

/*
 * The method IO::in_string(): String reads a string from 
 * the standard input, up to but not including a newline character.
 */
String* IO_in_string(IO *self)
{
	if (self == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
		abort();
	}

	/* Get one line worth of input with the newline, if any, discarded */
	char* in_string = 0;
	ssize_t len = get_one_line(&in_string, stdin);
	assert(in_string);
	
	/* We can take advantage of knowing the internal layout of String objects */
	String *str = String_new();
	str->val = in_string;
	return str;
}

/*
 * The method IO::in_int(): Int reads a single integer, which may be preceded
 * by whitespace. 
 * Any characters following the integer, up to and including the next newline,
 * are discarded by in_int.
 */
int IO_in_int(IO *self)
{
	if (self == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
		abort();
	}

	/* Get one line worth of input with the newline, if any, discarded */
	char* in_string = 0;
	ssize_t len = get_one_line(&in_string, stdin);
	assert(in_string);

	/* Now extract initial int and ignore the rest of the line */
	Int *x = Int_new();
	int num_ints = 0;
	if (len)
		num_ints = sscanf(in_string, " %d", &(x->val)); /* Discards initial spaces*/

	/* If no text found, abort. */
	if (num_ints == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__):\n   ");
		fprintf(stderr, "    Invalid integer on input in IO::in_int()\n");
		Object_abort((Object*) self);
	}
	return x->val;
}

/*
// Methods in class String
*/

// strlen returns size of char array excluding '\0'

int String_length(String *self) {

	if (self == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
		abort();
	}
	Int* x = Int_new();
	x->val = strlen( self->val );
	return x->val;
}

String* String_concat(String *s1, String *s2) {
	
	if (s1 == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
		abort();
	}
	int s1len = strlen(s1->val);
	int s2len = strlen(s2->val);
	char* str = handle_malloc( s1len + s2len + 1, (Object*) s1 );	// call handle_malloc
	memcpy( str, s1->val, s1len );					// copy s1
 	memcpy( (str + s1len) , s2->val, s2len );		// copy s2
	str[ s1len+s2len ] = '\0';						// append \0
	
	String* x = String_new();
	x->val = str;
	return x;
}

String* String_substr(String *s, int st, int len) {

	if (s == 0) {
		fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
		abort();
	}
	// idx [0,slen-1]
	int st_val = st; 
	int len_val = len;
	int slen = strlen(s->val);

	// support negative index
	if( st_val < 0 ) { st_val = slen + st_val; }
	// handle start out of index
	if( st_val > slen-1 ) { len_val = 0; }
	
	// check condition
	bool c1 = true; // len_val >= 0;
	bool c2 = true; // st_val >= 0 && st_val <= slen-1;
	bool c3 = (st_val+len_val-1) >= 0 && (st_val+len_val-1) <= slen-1;

	if( ! (c1 && c2 && c3) ) {	// if condition fails, then runtime error
		fprintf(stderr, "At __FILE__(line __LINE__):\n   ");
		fprintf(stderr, "    Invalid integers on input in String::substr()\n");
		Object_abort((Object*) s);
	}

	char* str = handle_malloc( len_val + 1 , (Object*) s );
	memcpy( str, (s->val)+st_val, len_val );
	str[ len_val ] = '\0';					// append \0

	String* x = String_new();
	x->val = str;
	return x;
}

void* handle_malloc( int size, Object* self ) {	// handle heap overflow at runtime
	void* ptr = malloc( size );
	if ( ptr == NULL ) {					// Heap overflow occured
		fprintf(stderr, "At __FILE__(line __LINE__): Heap overflow\n");
		abort();
		// unreachable
	}
	return ptr;
}

void Int_init(Int* self, int val) {
	self->val = val;
}

void Bool_init(Bool* self, bool val) {
	self->val = val;
}