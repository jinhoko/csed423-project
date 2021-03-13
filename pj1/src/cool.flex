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

%}
/* ======================================================================== */

%option noyywrap

/* Regex Abbreviations */

INTEGER        [0-9]+
TYPEID         [A-Z][a-zA-Z0-9_]*
OBJECTID       [a-z][a-zA-Z0-9_]*

/* Symbols */

WHITESPACE     [ \f\t\r\v]+
NEWLINE        \n

S_COMMENT      "--"
M_COMMENT_ST   "(*"
M_COMMENT_EN   "*)"

STRING         \"
STR_ESCEXCEPT  [\b\t\n\f]
STR_INVALID    \0
STR_NEWLINE    \\
 
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

<INITIAL>{S_COMMENT}          { BEGIN(SINGLECOMMENT); }
<SINGLECOMMENT>{NEWLINE}      { curr_lineno++; BEGIN(INITIAL); }
<SINGLECOMMENT><<EOF>>        { BEGIN(INITIAL);  }

<INITIAL>{M_COMMENT_ST}       { comment_level = 1; BEGIN(MULTICOMMENT); }
<INITIAL>{M_COMMENT_EN}       { cool_yylval.error_msg = "Unmatched *)";
                                BEGIN(INITIAL);
                                return ERROR;
                              }
<MULTICOMMENT>{M_COMMENT_ST}  { comment_level++; }
<MULTICOMMENT>{NEWLINE}       { curr_lineno++; }
<MULTICOMMENT><<EOF>>         { cool_yylval.error_msg = "EOF in comment"; 
                                BEGIN(INITIAL);
                                return ERROR;
                              }
<MULTICOMMENT>{M_COMMENT_EN}  { comment_level--;
                                if(comment_level==0) BEGIN(INITIAL);  
                              }

 /* String */

<INITIAL>{STRING}            { string_buf_ptr = string_buf;
                               BEGIN(STRING);
                             }
<STRING><<EOF>>              { cool_yylval.error_msg = "EOF in string constant";
                               BEGIN(INITIAL);
                               return ERROR;
                             }
<STRING>\\b                  { *string_buf_ptr++ = '\b'; }
<STRING>\\t                  { *string_buf_ptr++ = '\t'; }
<STRING>\\n                  { *string_buf_ptr++ = '\n'; }
<STRING>\\f                  { *string_buf_ptr++ = '\f'; }

<STRING>{STR_NEWLINE}        { curr_lineno++; }
<STRING>{STRING}             { BEGIN(INITIAL);
                               *string_buf_ptr = '\0';
                               return STR_CONST;
                             }
<STRING>.                    { *string_buf_ptr++ = *yytext;  }

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
{NEWLINE}           { curr_lineno++; }


 /* handle all unmatched cases (to ensure complete lexer) */

.                   { cool_yylval.error_msg = yytext;
                      return ERROR;
                    }

%%
