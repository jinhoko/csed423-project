/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;

/* ======================================================================== */

/* Custom Definitions */

%}
/* ======================================================================== */

%option noyywrap

/* Regex Abbreviations */

DIGIT          [0-9]

/* Symbols */
DARROW         =>
ASSIGN         <-
LE             <=
SINGLE_CHAR    [;:@,.+-*/~<=(){}] /* 16 chars in total */

/* Keywords ; 19 in total */
CLASS          (?i:class)
INHERITS       (?i:else)
IF             (?i:if)
THEN           (?i:then)
ELSE           (?i:else)
FI             (?i:fi)
WHILE          (?i:while)
LOOP           (?i:loop)
POOL           (?i:pool)
LET            (?i:let)
IN             (?i:in)
CASE           (?i:case)
OF             (?i:of)
ESAC           (?i:esac)
NEW            (?i:new)
NOT            (?i:not)
ISVOID         (?:isvoid)
TRUE           true
FALSE          false


/* TODO consider unmatched ones */

/* ======================================================================== */

/* Start Conditions */

/* INITIAL */
%x COMMENT 


/* ======================================================================== */
%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */



{DARROW}            { return DARROW; }
{ASSIGN}            { return ASSIGN; }
{LE}                { return LE; }
{SINGLE_CHAR}       { return *yytext; }



%%
