%{
/*
 * this file implements the grammar for the password tests
 * the escapes are handled in the caller
 */

#include "passwd.h"

/* Add the additional parameter for the "struct _options" pointer */
#define YYPARSE_PARAM		yyparse_opt
#define YYLEX_PARAM		yyparse_opt

/* Ensure that the option is passed to the yyeerror code as well */
#define yyerror(msg)		test_yyerror(msg,yyparse_opt)
%}

/*
 * the lexer returns both numbers and strings
 */
%union {
	char *cval;		/* something parsed as a string */
	int ival;		/* something parsed as a number */
}

%pure_parser			/* Don't use global storage */

/*
 * tokens returned by the lexical analyzer yylex()
 */
%token <ival> AND		/* number: (logical) disjunction */
%token <ival> LAND		/* number: (logical) disjunction */
%token <ival> LOR		/* number: (logical) conjunction */
%token <ival> DIV		/* number: (arithmetic) division */
%token <ival> EOL		/* number: no more input */
%token <ival> EQ		/* number: (relational) equals */
%token <cval> FILENAME		/* string: a file */
%token <ival> GE		/* number: (relational) greater than or equal*/
%token <ival> GT		/* number: (relational) greater than */
%token <ival> LE		/* number: (relational) less than or equal */
%token <ival> LPAR		/* number: begin grouping */
%token <ival> LT		/* number: (relational) less than */
%token <ival> MINUS		/* number: (arithmetic) subtraction, negation*/
%token <ival> MOD		/* number: (arithmetic) remaindering */
%token <ival> NE		/* number: (relational) not equal */
%token <ival> NOT		/* number: (logical) negation */
%token <ival> NUMBER		/* number: number or value of a variable */
%token <ival> OR		/* number: (logical) conjunction */
%token <ival> PATEQ		/* number: (pattern) match */
%token <ival> PATNE		/* number: (pattern) no match */
%token <cval> STRING		/* string: compare to a pattern */
%token <ival> PLUS		/* number: (arithmetic) addition */
%token <cval> PROGRAM		/* string: a program */
%token <ival> RPAR		/* number: end grouping */
%token <ival> TIMES		/* number: (arithmetic) multiplication */
%token <ival> UNK 		/* number: unknown token */

/*
 * productions analyzed by the parser
 */

%type <ival> number		/* number: something arithmetic */
%type <ival> prim		/* number: 1 if relation is satisfied */
%type <ival> stat		/* number: test with EOL tacked on */
%type <ival> string		/* number: 1 if string relation true */
%type <ival> test		/* number: 1 if test passed so far */

/*
 * expression operators
 * these must be on the same line since they are of equal precedence
 */

%right NOT			/* negation of tests */
%left OR AND			/* grouping of tests */
%left TIMES DIV MOD		/* usual arithmetic ordering */
%left PLUS MINUS
%nonassoc LT GT NE EQ GE LE	/* relational operators don't associate */

%{
/* Variables were moved to the "struct _options" data */

static int yylex(YYSTYPE *yylval, void *yyparse_opt);
void test_yyerror (const char *msg, void *yyparse_opt);
%}

/*
 * start analysis at state stat
 */

%start stat

%%
stat		: test EOL
			{ ((struct _options *)yyparse_opt)->retval = $1 ; }
		| error EOL
			{ ((struct _options *)yyparse_opt)->retval = 1; }
		| EOL
			{ ((struct _options *)yyparse_opt)->retval = 0; }
		;

test		: LPAR test RPAR
			{ $$ = $2 ; }
		| test LAND test
			{ $$ = $1 && $3 ; }
		| test LOR test
			{ $$ = $1 || $3 ; }
		| NOT test
			{ $$ = ! $2 ; }
		| string
			{ $$ = $1 ; }
		| prim
			{ $$ = $1 ; }
		;

string		: STRING EQ STRING
			{ $$ = (strcmp( $1 , $3 ) == 0);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING NE STRING
			{ $$ = (strcmp( $1 , $3 ) != 0);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING PATEQ STRING
			{ if (smatch(((struct _options *)yyparse_opt), $3 ))
			    YYERROR;
			  $$ = match((struct _options *)yyparse_opt, $1 );
			  free_pattern ((struct _options *)yyparse_opt);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING PATNE STRING
			{ if (smatch(((struct _options *)yyparse_opt), $3 ))
			    YYERROR;
			  $$ = !match(((struct _options *)yyparse_opt), $1 );
			  free_pattern ((struct _options *)yyparse_opt);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING EQ FILENAME
			{ $$ = strfp(((struct _options *)yyparse_opt), 1, $1 , $3 , fopen, fclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING NE FILENAME
			{ $$ = strfp(((struct _options *)yyparse_opt), 0, $1 , $3 , fopen, fclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING PATEQ FILENAME
			{ $$ = patinfp(((struct _options *)yyparse_opt), 1, $1 , $3 , fopen, fclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING PATNE FILENAME
			{ $$ = patinfp(((struct _options *)yyparse_opt), 0, $1 , $3 , fopen, fclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING EQ PROGRAM
			{ $$ = strfp(((struct _options *)yyparse_opt), 1, $1 , $3 , popen, pclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING NE PROGRAM
			{ $$ = strfp(((struct _options *)yyparse_opt), 0, $1 , $3 , popen, pclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING PATEQ PROGRAM
			{ $$ = patinfp(((struct _options *)yyparse_opt), 1, $1 , $3 , popen, pclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| STRING PATNE PROGRAM
			{ $$ = patinfp(((struct _options *)yyparse_opt), 0, $1 , $3 , popen, pclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| FILENAME EQ STRING
			{ $$ = strfp(((struct _options *)yyparse_opt), 1, $3 , $1 , fopen, fclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| FILENAME NE STRING
			{ $$ = strfp(((struct _options *)yyparse_opt), 0, $3 , $1 , fopen, fclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| FILENAME PATEQ STRING
			{ $$ = patfp(((struct _options *)yyparse_opt), 1, $3 , $1 , fopen, fclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| FILENAME PATNE STRING
			{ $$ = patfp(((struct _options *)yyparse_opt), 0, $3 , $1 , fopen, fclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| PROGRAM EQ STRING
			{ $$ = strfp(((struct _options *)yyparse_opt), 1, $3 , $1 , popen, pclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| PROGRAM NE STRING
			{ $$ = strfp(((struct _options *)yyparse_opt), 0, $3 , $1 , popen, pclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| PROGRAM PATEQ STRING
			{ $$ = patfp(((struct _options *)yyparse_opt), 1, $3 , $1 , popen, pclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		| PROGRAM PATNE STRING
			{ $$ = patfp(((struct _options *)yyparse_opt), 0, $3 , $1 , popen, pclose);
			  (void) free( $1 ); (void) free( $3 );
			}
		;

prim		: number LT number
			{ $$ = $1 < $3 ; }
		| number GT number
			{ $$ = $1 > $3 ; }
		| number NE number
			{ $$ = $1 != $3 ; }
		| number EQ number
			{ $$ = $1 == $3 ; }
		| number GE number
			{ $$ = $1 >= $3 ; }
		| number LE number
			{ $$ = $1 <= $3 ; }
		;

number		: LPAR number RPAR
			{ $$ = $2 ; }
		| number AND number
			{ $$ = $1 & $3 ; }
		| number PLUS number
			{ $$ = $1 + $3 ; }
		| number MINUS number
			{ $$ = $1 - $3 ; }
		| number MOD number
			{ $$ = $1 % $3 ; }
		| number OR number
			{ $$ = $1 | $3 ; }
		| PLUS number		%prec TIMES
			{ $$ = $1 ; }
		| MINUS number		%prec TIMES
			{ $$ = - $1 ; }
		| number TIMES number
			{ $$ = $1 * $3 ; }
		| number DIV number
			{ $$ = $1 / $3 ; }
		| NUMBER
			{ $$ = $1 ; }
		;

%%

static int yylex_number (YYSTYPE *yylval, struct _options *opt, int next_char)
  {
    register int rval;		/* used for return values */
/*
 * Process numerical data
 */
    if (next_char != '0')
      {
	rval      = next_char - '0';
	next_char = (int) (*opt->lptr++) & 0xFF;
	while (isdigit (next_char))
	  {
	    rval = (rval * 10) + (next_char - '0');
	    next_char = (int) (*opt->lptr++) & 0xFF;
	  }
	--opt->lptr;
	yylval->ival = rval;
	return(NUMBER);
      }
/*
 * Process hexadecimal input
 */
    next_char = (*opt->lptr++) & 0xFF;
    rval      = 0;
    if (next_char == 'x' || next_char == 'X')
      {
	next_char = (*opt->lptr++) & 0xFF;
	while (isxdigit (next_char))
	  {
	    if (next_char >= 'a')
	      {
		next_char ^= ('a' ^ 'A');
	      }
	    next_char -= '0';
	    if (next_char >= 10)
	      {
		next_char -= 7;
	      }
	    rval = (rval << 4) + next_char;
	    next_char = (*opt->lptr++) & 0xFF;
	  }
	--opt->lptr;
	yylval->ival = rval;
	return(NUMBER);
      }
/*
 * Process an octal digit
 */ 

#define isoctal(c) (((c) >= '0') && ((c) <= '7'))

    if (isoctal (next_char))
      {
	rval      = next_char - '0';
	next_char = (*opt->lptr++) & 0xFF;
	if (isoctal (next_char))
	  {
	    rval      = (rval << 3) + (next_char - '0');
	    next_char = (*opt->lptr++) & 0xFF;
	    if (isoctal (next_char))
	      {
		rval      = (rval << 3) + (next_char - '0');
		next_char = (*opt->lptr++) & 0xFF;
	      }
	  }
      }
    --opt->lptr;
    yylval->ival = rval;
    return(NUMBER);
#undef isoctal
  }

static int yylex_escape (YYSTYPE *yylval, struct _options *opt, int next_char)
  {
    next_char = (*opt->lptr++) & 0xFF;
    switch (next_char)
      {
    case 'a':
        yylval->ival = 7;  /* '\a' */
	return(NUMBER);

    case 'b':
        yylval->ival = '\b';
	return(NUMBER);

    case 'f':
        yylval->ival = '\f';
	return(NUMBER);

    case 'n':
        yylval->ival = '\n';
	return(NUMBER);

    case 'r':
        yylval->ival = '\r';
	return(NUMBER);

    case 't':
        yylval->ival = '\t';
	return(NUMBER);

    case 0:
	--opt->lptr;
	break;

    default:
        break;
      }

    yylval->ival = next_char;
    return(NUMBER);
  }

static int yylex_string (YYSTYPE *yylval, struct _options *opt,
			 int quo, int type)
  {
    char parbuf[BUFSIZ];	/* used to save strings */
/*
 * collect the file or program or pattern
 */
    opt->lptr = getcstring (opt->lptr, parbuf, quo);
/*
 * if a closing quote, skip it
 */
    if (*opt->lptr)
        opt->lptr++;
/*
 * return the file or program as the value
 */
    yylval->cval = strdup(parbuf);
    return (type);
  }

/*
 * this is the lexer -- it's pretty dumb
 */

static int yylex(YYSTYPE *yylval, void *yyparse_opt)
  {
    struct _options *opt = (struct _options *) yyparse_opt;
    int next_char;

#define dual(code,yes,no) {		\
    next_char = (*opt->lptr++) & 0xFF;	\
    if (next_char == code) {		\
        return yes;			\
    }					\
    --opt->lptr;			\
    return no;				\
}

#define tri(code1,yes1,code2,yes2,no) {	\
    next_char = (*opt->lptr++) & 0xFF;	\
    if (next_char == code1) {		\
        return yes1;			\
    }					\
    if (next_char == code2) {		\
        return yes2;			\
    }					\
    --opt->lptr;			\
    return no;				\
}

/*
 * this is hit at the end of string
 * we need to do it this way because the '\0' (EOL)
 * token must be returned, so we have to return
 * another "end of input" token -- in other words,
 * the end of input character is NOT the same as 
 * the end of string (EOL) character
 */
    if (opt->ateol)
      {
	opt->ateol = 0;
	return (-1);	/* YACC's end of file character */
      }
/*
 * eat leading white spaces; may have a backslash in front
 * (since a tab separates the test field and the message
 * field, if there is a tab in the test field it must be
 * escaped)
 */
    for (;;)
      {
	next_char = (int) (*opt->lptr) & 0xFF;
	if (next_char == 0)
	  {
	    opt->ateol = 1;
	    return(EOL);
	  }
	
	++opt->lptr;
	if (isspace (next_char))
	  {
	    continue;
	  }
/*
 * Process numerical data
 */
	if (isdigit (next_char))
	  {
	    return (yylex_number (yylval, opt, next_char));
	  }
/*
 * Process the escape character
 */
	if (next_char == '\\')
	  {
	    return (yylex_escape (yylval, opt, next_char));
	  }
/*
 * The character sequence is not special. Try to analyize it.
 */
	switch (next_char)
	  {
	case '@':		/* rest of line is comment */
	case '\t':		/* rest of line is error message */
	case '\n':		/* end of line */
	case '\0':		/* end of line */
	    opt->ateol = 1;
	    return(EOL);
	case '(':		/* begin grouping */
	    return(LPAR);
	case ')':		/* end grouping */
	    return(RPAR);
	case '~':		/* negation */
	    return(NOT);
	case '+':		/* add */
	    return(PLUS);
	case '-':		/* subtract */
	    return(MINUS);
	case '*':		/* multiply */
	    return(TIMES);
	case '/':		/* divide */
	    return(DIV);
	case '%':		/* remainder */
	    return(MOD);
	case '&':		/* disjunction */
	    dual ('&', LAND, AND);
	case '|':		/* conjunction */
	    dual ('|', LOR, OR);
	case '>':		/* relation */
	    dual ('=', GE, GT);
	case '<':		/* relation */
	    dual ('=', LE, LT);
	case '!':		/* relation, logical negation */
	    tri ('=', NE, '~', PATNE, NOT);
	case '=':
	    tri ('=', EQ, '~', PATEQ, EQ);

	case '\'':			/* pattern */
	case '\"':
	    return (yylex_string (yylval, opt, next_char, STRING));

	case '{':			/* program */
	    return (yylex_string (yylval, opt, '}', PROGRAM));

	case '[':			/* file */
	    return (yylex_string (yylval, opt, ']', FILENAME));
	  }
	break;
      }
/*
 * unknown
 */
    return(UNK);

#undef dual
#undef tri
}

/*
 * report grammar errors (yacc)
 */

void test_yyerror (const char *msg, void *yyparse_opt)
{
    logfunc ((struct _options *)yyparse_opt, LG_SYNTAX,
	     "%s at line %d (at \"%8s\")",
	     msg,
	     ((struct _options *)yyparse_opt)->linect,
	     ((struct _options *)yyparse_opt)->lptr - 1);

    ((struct _options *)yyparse_opt)->ateol = 1;
}

/*==================== DRIVER ====================*/
/*
 * this runs the test in "buf" on the password
 * and returns 1 if it is a bad password, 0 if not.
 */

int passtest (struct _options *opt, char *buf)
{
    /* clear the end of line flag */
    opt->ateol = 0;

    /* clobber any trailing newline */
    opt->lptr = strchr (buf, '\n');
    if (opt->lptr)
        *opt->lptr = '\0';
/*
 * set up the pointer to the input
 * for yylex, the lexical analyzer
 */
    opt->lptr   = buf;
/*
 * parse the statement and if the password is bad, let retval be 1.
 * yyparse returns 0 if the grammer is valid and reaches a final
 * state. It return non-zero if there is a 'stop' condition. Neither
 * of these indicate the state of the grammer finding a good rule.
 */
    opt->retval = 1;
    if (yyparse((void *) opt) != 0)
        opt->retval = 1;
/*
 * test is syntactically and semantically correct
 * and return the result
 */
    return (opt->retval == 0);
}
