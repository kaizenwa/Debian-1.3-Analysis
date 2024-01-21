/* Copyright 1989 GROUPE BULL -- See license conditions in file COPYRIGHT
** Copyright 1989 Massachusetts Institute of Technology
**/
/****************************\
* 			     *
*  Lex generator for Wool   *
* 			     *
\****************************/

%{

/* to have always at least MAX_TOKEN_LENGTH chars in all lexes */
#undef YYLMAX
#define YYLMAX MAX_TOKEN_LENGTH

/* here goes the definition of lex I/O, as macros for efficiency */

/* first, we forget the previous definitions, */
#undef output
#undef YY_INPUT
#undef ECHO

/* So here are OUR macros */

static int yyin_is_string;	/* 0 ==> reading from STREAM yyin */
				/* 1 ==> reading from STRING yystrin */
static int yyout_is_string;	/* 0 ==> printing on STREAM yyout */
		/* 1 ==> printing on WOOL_STRING_STREAM yystrout */
static char *yystrin;		/* current pointer on input string */
static WOOL_STRING_STREAM yystrout; /* current wool output stream */
int yylineno=0;			/* flex doesn't have this */

#define output(c) {\
    if(yyout_is_string){\
        *(yystrout->ptr)++ = c;\
        *(yystrout->ptr) = '\0';\
        if((yystrout->ptr) >= (yystrout->last)){\
	    yyoutflush();\
	}\
    }else{\
	putc(c,yyout);\
    }\
}

#define ECHO	{\
	int i;\
	for (i=0; i<yyleng; i++)\
	    output(yytext[i]);\
	}


#define YY_INPUT(buf, result, max_size) \
	if(yyin_is_string) {\
	    strncpy(buf, yystrin, max_size-1);\
	    yystrin += result = strlen(buf);\
	} else {\
	    if((result = read(fileno(yyin), buf, max_size)) < 0)\
	        YY_FATAL_ERROR("read() in flex scanner failed");\
	}
/*
#define YY_INPUT(buf, result, max_size) \
	if(yyin_is_string)\
	    (*buf = *yystrin)? yystrin++:0;\
	    result=1;\
	} else {\
	    if((result = read(fileno(yyin), buf, max_size)) < 0)\
	        YY_FATAL_ERROR("read() in flex scanner failed");\
	}
*/

/* handling of wool buffer stack */
#define WOOL_MAX_BUFFER_STACK	50
static YY_BUFFER_STATE	wool_buffer_stack[WOOL_MAX_BUFFER_STACK];
static int wool_buffer_sp = 0;


/* counting the parentheses -- hack for wool_pool */

#define INC_PAR wool_pool_parentheses_level++
#define DEC_PAR if(wool_pool_parentheses_level)wool_pool_parentheses_level--
%}

FNCP	[^-+\004 \t\n"'()0-9]
FNC	[^\004 \t\n"'()0-9]
NC	[^\004 \t\n"'()]
SIGN	[-+]

%%
\n		yylineno++; REJECT;		/* That's faster than in the macro */
.[\010]		;				/* handles backspacing */
.[\177]		;				/* handles deleting */
\"([^\\\"\n]|\\(.|\n)|\"\")*\"  return(STRING); /* strings */
\"([^\\\"\n]|\\(.|\n)|\"\")*\n  return(NON_CLOSED_STRING); /* error */
\;.*\n		;				/* comments */
[-+]?[0-9]+	return(NUMBER);			/* integers */
0[xX][0-9a-fA-F]+	return(HEX_NUMBER);	/* hex integers */
"("		{INC_PAR; return(LEFTPAR);}	/* start of list */
")"		{DEC_PAR; return(RIGHTPAR);}    /* end of list */
"'"		return(QUOTECHAR);		/* quoting */
"{"		{INC_PAR; return(LEFTBRA);}	/* (progn */
"}"		{DEC_PAR; return(RIGHTBRA);}	/* ) */
{FNCP}{NC}*	return (NAME);			/* identifier */
{SIGN}{FNC}{NC}*	return (NAME);		/* +foo */
{SIGN}		return (NAME);			/* + */
[ \t\n]		;				/* blanks */
<<EOF>>		return(END_OF_FILE);		/* pseudo-EOF handling */
.		;				/* skip over control codes */
%%

/**********************\
* 		       *
*  WOOL's I/O package  *
* 		       *
\**********************/



/* externally callable function for unput:
 * Doesn't do anything, since we solved include files by
 * using switch_buffer() calls.
 */

wool_unput(buffer)
char *buffer;
{
}


/*
 * yyoutflush
 * to flush wool output buffer.
 */

void
yyoutflush(){
    if(yyout_is_string){
        ASSERT(yystrout->overflow_handler);
/*XXX-UWE-XXX*/
	(*(yystrout->overflow_handler))(yystrout);
	/* yystrout->ptr = yystrout-> buffer; */
/*XXX-UWE-XXX*/
    }else{
	fflush(yyout);
    }
}

/*
 * wool_input_redirect
 * to set wool's parsing to the stream or string argument.
 * arg1 = type (0 = string, 1 = stream);
 * arg2 = stream or string
 * arg3 = POINTER to (FILE *) or (char *) where will go the old stream
 *        (if NULL not set)(returns the old type)
 * arg4 = used to be:
 *		where to save contents of unput buffer (if not NULL)
 *	  now is: 
 *		if not NULL, open new buffer; otherwise pop most
 *		recent.
 */

int wool_input_redirect(type, stream, oldstream_p, old_buffer_contents)
int type;
char *stream;
char **oldstream_p;
char *old_buffer_contents;
{
    int oldtype = yyin_is_string;

    if(oldstream_p) *oldstream_p =
    	(oldtype ? (char *) yystrin : (char *) yyin);
    if(yyin_is_string = type){
	yystrin = stream;
    }else{
	yyin = (FILE *) stream;
    }

    if(old_buffer_contents != NULL) {
    	/* open new buffer */

	if(wool_buffer_sp >= WOOL_MAX_BUFFER_STACK)
	    YY_FATAL_ERROR("wool_buffer_stack overflow");

	wool_buffer_stack[wool_buffer_sp++] = YY_CURRENT_BUFFER;
	yy_switch_to_buffer(yy_create_buffer(yyin, YY_BUF_SIZE));
    } else{
	/* pop most recent buffer */

	if(--wool_buffer_sp < 0)
	    YY_FATAL_ERROR("wool_buffer_stack empty");

	yy_delete_buffer( YY_CURRENT_BUFFER );
	yy_switch_to_buffer(wool_buffer_stack[wool_buffer_sp]);
    }
    return oldtype;
}

/*
 * wool_output_redirect
 * to set wool's outputs to the stream or string argument.
 * arg1 = type (0 = string, 1 = stream);
 * arg2 = stream or string
 * arg4 = POINTER to (FILE *) or WOOL_STRING_STREAM tu put the old stream
 *        (if NULL not set)(returns the old type)
 */

wool_output_redirect(type, stream, oldstream_p)
int type;
char *stream;
char **oldstream_p;
{
    int oldtype = yyout_is_string;
    if(oldstream_p) *oldstream_p =
    	(oldtype ? (char *) yystrout : (char *) yyout);
    yyoutflush();
    if(yyout_is_string = type){
	yystrout = (WOOL_STRING_STREAM) stream;
    }else{
	yyout = (FILE *) stream;
    }
    return oldtype;
}

/*
 * now, some functions to provide a printf - like service on wool's output.
 */

/* prints a string */

void
wool_puts(string)
register char *string;
{
    while(*string) output(*string++);
}

/* put a newline */

wool_newline()
{
    output('\n');
}

/* does a format with ONE arg. */

wool_printf(string, arg)
register char *string;
char *arg;
{
    static char wool_temp_string[MAX_TEMP_STRING_SIZE];
    sprintf(wool_temp_string, string, arg);
    wool_puts(wool_temp_string);
}

/* prints a char */

void
wool_putchar(c)
char c;
{
    output(c);
}

/*
 * function to make a WOOL_STRING_STREAM of a given capacity
 * arg1 = capactity in bytes
 * arg2 = user function to be called on buffer when overflow occurs
 */

WOOL_STRING_STREAM WOOL_STRING_STREAM_make(nbytes, handler)
int nbytes;
int (* handler)();
{
    WOOL_STRING_STREAM str = (WOOL_STRING_STREAM)
    	Malloc(sizeof(struct _WOOL_STRING_STREAM));
    str->buffer = (char *) Malloc(nbytes);
    *str->buffer = '\0';        /*XXX-UWE-XXX*/
    str->ptr = str->buffer;
    str->last = str->buffer + nbytes -1;
    str->overflow_handler = handler;
    return str;
}

/* 
 * and the function to free a stream
 */

WOOL_STRING_STREAM_free (str)
WOOL_STRING_STREAM str;
{
    Free(str->buffer);
    Free(str);
}
