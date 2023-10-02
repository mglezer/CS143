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
bool string_err = false;

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


static void clear_string_state() {
  BEGIN(INITIAL);
  string_buf_idx = 0;
  string_err = false;
}


/* Error messages. */
static char STR_TOO_LONG[] = "String constant too long";
static char STR_CONTAINS_NULL[] = "String contains null character.";
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

LE              "<="

OPEN_COMMENT    "(*"

CLOSE_COMMENT   "*)"

QUOTE           \"

INLINE_COMMENT  --[^\n]*

WHITE_SPACE     (\t|\b|\v|\f|\a|\r|" ")+

NEW_LINE        \n

ESCAPED_CHAR    \\(.|\n)

INTEGER_LITERAL [0-9]+

OBJ_ID          [a-z][a-zA-Z0-9_]*

TYPE_ID         [A-Z][a-zA-Z0-9_]*

ASSIGN          "<-"

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
CLASS           [cC][lL][aA][sS][sS]   
INHERITS        [iI][nN][hH][eE][rR][iI][tT][sS]   
IF              [iI][fF]   
THEN            [tT][hH][eE][nN]   
ELSE            [eE][lL][sS][eE]   
FI              [fF][iI]   
WHILE           [wW][hH][iI][lL][eE]   
LOOP            [lL][oO][oO][pP]   
POOL            [pP][oO][oO][lL]   
LET             [lL][eE][tT]   
IN              [iI][nN]   
CASE            [cC][aA][sS][eE]   
OF              [oO][fF]   
ESAC            [eE][sS][aA][cC]   
NEW             [nN][eE][wW]   
ISVOID          [iI][sS][vV][oO][iI][dD]   
NOT             [nN][oO][tT]   
TRUE            t[r|R][u|U][e|E]
FALSE           f[a|A][l|L][s|S][e|E]

OPERATOR        "="|"+"|"-"|"*"|"/"|"~"|"<"|"("|")"|";"|"{"|"}"|":"|"."|","|"@"

%x COMMENT_MODE

%x STRING_MODE

%%

%{
/*
 * Put keywords and operators first so they have the highest precedence.
 * This should ensure that keywords and operators are never interpreted as
 * regular identifiers.
 */
%}

{TRUE} {
  cool_yylval.boolean = true;
  return BOOL_CONST;
}

{FALSE} {
  cool_yylval.boolean = false;
  return BOOL_CONST;
}

{ASSIGN} {
  return ASSIGN;
}

{CLASS} {
  return CLASS;
}

{CASE} {
  return CASE;
}

{ESAC} {
  return ESAC;
}

{IF} {
  return IF;
}

{FI} {
  return FI;
}

{IN} {
  return IN;
}

{INHERITS} {
  return INHERITS;
}

{ISVOID} {
  return ISVOID;
}

{LET} {
  return LET;
}

{WHILE} {
  return WHILE;
}

{LOOP} {
  return LOOP;
}

{POOL} {
  return POOL;
}

{NOT} {
  return NOT;
}

{OF} {
  return OF;
}

{NEW} {
  return NEW;
}

{THEN} {
  return THEN;
}

{ELSE} {
  return ELSE;
}

{OPERATOR} {
  return (int)yytext[0];
}

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

{OBJ_ID} {
  cool_yylval.symbol = stringtable.add_string(yytext);
  return OBJECTID;
}

{TYPE_ID} {
  cool_yylval.symbol = stringtable.add_string(yytext);
  return TYPEID;
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

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }

{LE} {
  return LE;
}

{QUOTE} {
  /* Open quote. */
  BEGIN(STRING_MODE);
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

<STRING_MODE>{ESCAPED_CHAR} {
  if (yytext[1] == '\n') {
    curr_lineno++;
  }

  if (!string_err) {
    if (yytext[1] == '\0') {
      string_err = true;
      cool_yylval.error_msg = STR_CONTAINS_NULL;
      return ERROR;
    } else if (buffer_is_full()) {
      string_err = true;
      cool_yylval.error_msg = STR_TOO_LONG;
      return ERROR;
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
  if (!string_err) {
    if (yytext[0] == '\0') {
      string_err = true;
      cool_yylval.error_msg = STR_CONTAINS_NULL;
      return ERROR;
    } else if (buffer_is_full()) {
      string_err = true;
      cool_yylval.error_msg = STR_TOO_LONG;
      return ERROR;
    } else {
      string_buf[string_buf_idx++] = yytext[0];
    }
  }
}

<STRING_MODE>{QUOTE} {
  /* Close quote. */
  bool prev_string_err = string_err;
  int prev_string_buf_idx = string_buf_idx;

  clear_string_state();

  if (!prev_string_err) {
    char buffer[prev_string_buf_idx + 1];
    memcpy(buffer, string_buf, prev_string_buf_idx);
    buffer[prev_string_buf_idx] = '\0';
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
  // Ignore the contents of comments.
}

. {
 /* Fallback rule. This should go last. */
   cool_yylval.error_msg = yytext;
   return ERROR;
}

%%
