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
int string_buf_idx = 0;
char *string_err_msg = NULL;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

/* Indicates how deep into a nested comment we are. I hate nested comments. */
static int comment_depth = 0;

static bool buffer_is_full() {
  return string_buf_idx == MAX_STR_CONST;
}

static bool set_str_error_msg(char *error_msg) {
  if (string_err_msg  == NULL) {
    string_err_msg = error_msg;
  }
}
    

static void clear_string_state() {
  string_buf_idx = 0;
  string_err_msg = NULL;
}


/* Error messages. */
static char STR_TOO_LONG[] = "String constant too long";
static char STR_CONTAINS_NULL[] = "String contains null character";
static char STR_UNTERMINATED[] = "Unterminated string constant";
static char STR_CONTAINS_EOF[] = "EOF in string constant";
static char UNRECOGNIZED_CHAR[] = "Unrecognized character";
static char COMMENT_UNMATCHED_CLOSE[] = "Unmatched *)";
static char COMMENT_UNMATCHED_OPEN[] = "EOF in comment";

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

QUOTE           \"

INLINE_COMMENT  --[^\n]*

WHITE_SPACE     (\t|\b|\v|\f|\a|\r|" ")+

NEW_LINE        \n

ESCAPED_CHAR    \\(.|\n)

INTEGER_LITERAL [0-9]+

%x COMMENT_MODE

%x STRING_MODE

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

<STRING_MODE>{NEW_LINE} {
  curr_lineno++;
  cool_yylval.error_msg = STR_UNTERMINATED;
  clear_string_state();
  BEGIN(INITIAL);
  return ERROR;
}


{INLINE_COMMENT} {
  /** Ignore. */
}

<COMMENT_MODE,INITIAL>{OPEN_COMMENT} {
  comment_depth++;
  BEGIN(COMMENT_MODE);
}

<COMMENT_MODE><<EOF>> {
  cool_yylval.error_msg = COMMENT_UNMATCHED_OPEN;
  /* Hack to get out of the stupid EOF loop. It seems that returning something other 
   * than 0 causes Flex to continue parsing the file, but it hits another EOF, 
   * so this rule loops. */
  BEGIN(INITIAL);
  return ERROR;
}

<STRING_MODE><<EOF>> {
  cool_yylval.error_msg = STR_CONTAINS_EOF;
  /* See comment in the <COMMENT_MODE><<EOF>> rule. */
  BEGIN(INITIAL);
  return ERROR;
}

<<EOF>> {
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


{QUOTE} {
  /* Open quote. */
  BEGIN(STRING_MODE);
}

<STRING_MODE>{ESCAPED_CHAR} {
  if (yytext[1] == '\n') {
    curr_lineno++;
  }

  if (string_err_msg == NULL) {
    if (buffer_is_full()) {
      set_str_error_msg(STR_TOO_LONG);
    } else {
      char c;
      switch (yytext[1]) {
        case 'b':
          c = '\b';
          break;
        case 't':
          c = '\t';
          break;
        case 'n':
          c = '\n';
          break;
        case 'f':
          c = '\f';
          break;
        default:
          c = yytext[1];
      }
      string_buf[string_buf_idx++] = c;
    }
  }
}

<STRING_MODE>[^\"] {
  if (string_err_msg == NULL) {
    if (yytext[0] == '\0') {
      set_str_error_msg(STR_CONTAINS_NULL);
    } else if (buffer_is_full()) {
      set_str_error_msg(STR_TOO_LONG);
    } else {
      string_buf[string_buf_idx++] = yytext[0];
    }
  }
}

<STRING_MODE>{QUOTE} {
  /* Close quote. */
  BEGIN(INITIAL);
  char *old_string_err_msg = string_err_msg;
  int old_string_buf_idx = string_buf_idx;

  clear_string_state();

  if (old_string_err_msg) {
    cool_yylval.error_msg = old_string_err_msg;
    return ERROR;
  } else {
    char buffer[old_string_buf_idx + 1];
    memcpy(buffer, string_buf, old_string_buf_idx);
    buffer[old_string_buf_idx] = '\0';
    cool_yylval.symbol = stringtable.add_string(buffer);
    return STR_CONST;
  }
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
