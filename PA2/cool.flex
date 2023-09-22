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
#include <stdlib.h>

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

char string_buf[MAX_STR_CONST]; /* to assemble strcing constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

/* Indicates how deep into a nested comment we are. I hate nested comments. */
int comment_depth = 0;


/* Error messages. */
static char STR_TOO_LONG[] = "String constant too long.";
static char STR_CONTAINS_NULL[] = "String contains null character.";
static char STR_UNTERMINATED[] = "Unterminated string constant.";
static char STR_CONTAINS_EOF[] = "EOF in string constant.";
static char UNRECOGNIZED_CHAR[] = "Unrecognized character.";
static char COMMENT_UNMATCHED_CLOSE[] = "Unmatched *)";
static char COMMENT_UNMATCHED_OPEN[] = "EOF in comment.";

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>

DISALLOWED_CHARS [\0]

ALLOWED_CHARS [^{DISALLOWED_CHARS}]

MULTILINE_COMMENT "(*"

OPEN_COMMENT    "(*"

CLOSE_COMMENT   "*)"

INLINE_COMMENT  --[^\n]*

WHITE_SPACE     (\t|\b|\v|\f|\a|\r|" ")+

NEW_LINE        \n

STRING_LITERAL  \"([^\"\n]|\\\n)*\"

/* 
 * Because of maximal matching we can simplify this; if it turns out the new line is
 * properly escaped and then the string is later closed, the STRING_LITERAL matcher
 * above will capture it.
 */
OPEN_STRING     \"[^\"]*\n

INTEGER_LITERAL [0-9]+

/* We use maximal matching to easily detect open strings which end in EOF. */
EOF_STRING      \"[^\"]*

%x COMMENT_MODE

%%

 /*
  *  Nested comments
  */


{WHITE_SPACE} {
  /** Ignore. */
}

<INITIAL,COMMENT_MODE>{NEW_LINE} {
  curr_lineno++;
}

{INLINE_COMMENT} {
  /** Ignore. */
}


<COMMENT_MODE,INITIAL>{OPEN_COMMENT} {
  comment_depth++;
  BEGIN(COMMENT_MODE);
}

<<EOF>> {
  if (comment_depth > 0) {
    cool_yylval.error_msg = COMMENT_UNMATCHED_OPEN;
    return  ERROR;
  }
  yyterminate();
}


 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

{OPEN_STRING} {
  // Increment the line number since the open string by definition ends with a new line.
  curr_lineno++;
  cool_yylval.error_msg = STR_UNTERMINATED;
  return ERROR;
}

{EOF_STRING} {
  cool_yylval.error_msg = STR_CONTAINS_EOF;
  return ERROR;
}

{STRING_LITERAL} {
  if (strlen(yytext) > MAX_STR_CONST) {
    cool_yylval.error_msg = STR_TOO_LONG;
    return ERROR;
  }
  /*
  if (strchr(yytext, 0)) {
    cool_yylval.error_msg = STR_CONTAINS_NULL;
    return ERROR;
  }
  */


  // Count the new lines. 
  int n = strlen(yytext);
  for (int i = 0; i < n; i++) {
    if (yytext[i] == '\n') {
      curr_lineno++;
    }
  }

  // Extract the actual string.
  // Exclude the first and last matching characters, which will be quotation marks by definition.
  // Note that the buffer must include space for the terminating null character.
  char buffer[n - 1];
  char *string_contents = (char *)memcpy(buffer, yytext + 1, n - 2);
  buffer[n - 2] = '\0';
  cool_yylval.symbol = stringtable.add_string(string_contents);
  return STR_CONST;
}

{INTEGER_LITERAL} {
  cool_yylval.symbol = stringtable.add_string(yytext);
  return INT_CONST;
}

<COMMENT_MODE>{CLOSE_COMMENT} {
  assert(comment_depth > 0);
  if (--comment_depth == 0) {
    // Reset the state.
    BEGIN(INITIAL);
  }
}

{CLOSE_COMMENT} {
  cool_yylval.error_msg = COMMENT_UNMATCHED_CLOSE;
  return ERROR;
}

<COMMENT_MODE>. {
  // Ignore.
}

. {
 /* Fallback rule. This should go last. */
   cool_yylval.error_msg = UNRECOGNIZED_CHAR;
   return ERROR;
}

%%
