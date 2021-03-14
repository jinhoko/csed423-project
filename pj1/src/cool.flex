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

int comment_level;
int is_string_invalid;
int is_string_long;
int check_length();

%}
/* ======================================================================== */

%option noyywrap

/* Regex Abbreviations */

INTEGER        [0-9]+
TYPEID         [A-Z][a-zA-Z0-9_]*
OBJECTID       [a-z][a-zA-Z0-9_]*

/* Symbols */

WHITESPACE     [ \f\t\r\v]+

DARROW         =>
ASSIGN         <-
LE             <=
SINGLE_CHAR    [;:@,.*/~<=(){}+-]
  /* 16 single char symbols in total */

/* Keywords ; 19 in total */
CLASS          (?i:class)
INHERITS       (?i:inherits)
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
ISVOID         (?i:isvoid)
TRUE           t(?i:rue)
FALSE          f(?i:alse)


/* ======================================================================== */

/* Conditions */

/* %x INITIAL */
%x MULTICOMMENT
%x SINGLECOMMENT
%x STRING

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

 /* Comment */

<INITIAL>--                   { BEGIN(SINGLECOMMENT); }
<SINGLECOMMENT>\n             { curr_lineno++; BEGIN(INITIAL); }
<SINGLECOMMENT><<EOF>>        { BEGIN(INITIAL);  }
<SINGLECOMMENT>.              { } 

<INITIAL>"(*"                 { comment_level = 1; BEGIN(MULTICOMMENT); }
<INITIAL>"*)"                 { cool_yylval.error_msg = "Unmatched *)";
                                return ERROR;
                              }
<MULTICOMMENT>"(*"            { comment_level++; }
<MULTICOMMENT>\n              { curr_lineno++; }
<MULTICOMMENT><<EOF>>         { cool_yylval.error_msg = "EOF in comment"; 
                                BEGIN(INITIAL);
                                return ERROR;
                              }
<MULTICOMMENT>"*)"            { comment_level--;
                                if(comment_level==0) BEGIN(INITIAL);  
                              }
<MULTICOMMENT>.               { }

 /* String */

<INITIAL>\"                  { string_buf_ptr = string_buf;
                               is_string_invalid = 0;
                               is_string_long = 0;
                               BEGIN(STRING);
                             }
<STRING><<EOF>>              { cool_yylval.error_msg = "EOF in string constant";
                               BEGIN(INITIAL);
                               return ERROR;
                             }
<STRING>\n                   { cool_yylval.error_msg = "Unterminated string constant";
                               curr_lineno++;
                               BEGIN(INITIAL);
                               return ERROR;
                             }
<STRING>\0                   { is_string_invalid = 1; }
<STRING>\\\0                 { is_string_invalid = 1; }

<STRING>\\b                  { if(check_length()) *string_buf_ptr++ = '\b'; }
<STRING>\\t                  { if(check_length()) *string_buf_ptr++ = '\t'; }
<STRING>\\n                  { if(check_length()) *string_buf_ptr++ = '\n'; }
<STRING>\\f                  { if(check_length()) *string_buf_ptr++ = '\f'; }

<STRING>\\\n                 { curr_lineno++;
                               if(check_length()) *string_buf_ptr++ = '\n';
                             }
<STRING>\"                   { BEGIN(INITIAL);
                               if(is_string_invalid == 0) {
                                 if(is_string_long == 0) {
                                   *string_buf_ptr = '\0';
                                   cool_yylval.symbol = stringtable.add_string(string_buf);
                                   return STR_CONST;
                                 } else {
                                   cool_yylval.error_msg = "String constant too long";
                                   return ERROR;
                                 } 
                               } else {
                                 cool_yylval.error_msg = "String contains invalid character";
                                 return ERROR;
                               }
                             }

<STRING>\\.                  { if(check_length()) *string_buf_ptr++ = *(yytext+1); }

<STRING>.                    { if(check_length()) {*string_buf_ptr++ = *yytext;}  }

 /* Default */

{DARROW}            { return DARROW; } /* Multi-char symbols should come first */
{ASSIGN}            { return ASSIGN; }
{LE}                { return LE; }
{SINGLE_CHAR}       { return *yytext; }

{CLASS}             { return CLASS; }
{INHERITS}          { return INHERITS; }
{IF}                { return IF; }
{THEN}              { return THEN; }
{ELSE}              { return ELSE; }
{FI}                { return FI; }
{WHILE}             { return WHILE; }
{LOOP}              { return LOOP; }
{POOL}              { return POOL; }
{LET}               { return LET; }
{IN}                { return IN; }
{CASE}              { return CASE; }
{OF}                { return OF; }
{ESAC}              { return ESAC; }
{NEW}               { return NEW; }
{NOT}               { return NOT; }
{ISVOID}            { return ISVOID; }

{TRUE}              { cool_yylval.boolean = 1;  return BOOL_CONST; }
{FALSE}             { cool_yylval.boolean = 0; return BOOL_CONST; }

{INTEGER}           { cool_yylval.symbol = inttable.add_string(yytext);
                      return INT_CONST;
                    }
{TYPEID}            { cool_yylval.symbol = stringtable.add_string(yytext);
                      return TYPEID;
                    }
{OBJECTID}          { cool_yylval.symbol = stringtable.add_string(yytext);
                      return OBJECTID;
                    }

{WHITESPACE}        { }
\n                  { curr_lineno++; }


 /* handle all unmatched cases (to ensure complete lexer) */

.                   { cool_yylval.error_msg = yytext;
                      return ERROR;
                    }

%%

int check_length() {
 if( string_buf_ptr - string_buf >= MAX_STR_CONST - 1) {
   is_string_long = 1;
   return 0;
 }
 return 1;
}
