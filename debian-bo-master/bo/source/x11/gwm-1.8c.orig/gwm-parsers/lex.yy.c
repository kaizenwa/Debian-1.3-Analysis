# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;

/* to have always at least MAX_TOKEN_LENGTH chars in all lexes */
#undef YYLMAX
#define YYLMAX MAX_TOKEN_LENGTH

/* here goes the definition of lex I/O, as macros for efficiency */

/* first, we forget the previous definitions, */
#undef output
#undef input
#undef unput
/* which were :
#define output(c) putc(c,yyout)
#define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):\
getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;\
*yysptr++=yytchar;}
*/

/* So here are OUR macros */

static int yyin_is_string;	/* 0 ==> reading from STREAM yyin */
				/* 1 ==> reading from STRING yystrin */
static int yyout_is_string;	/* 0 ==> printing on STREAM yyout */
		/* 1 ==> printing on WOOL_STRING_STREAM yystrout */
static char *yystrin;		/* current pointer on input string */
static WOOL_STRING_STREAM yystrout; /* current wool output stream */

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

#define input() (((\
    	yytchar =\
        (yysptr>yysbuf ?\
	    U(*--yysptr)\
	:   (yyin_is_string ?\
	        ( *yystrin ? *yystrin++ : 0)\
	    :   getc(yyin))))\
	==10 ?\
	    (yylineno++,yytchar)\
	:   yytchar)\
    ==EOF ?\
        0\
    :   yytchar)

#define unput(c) {\
    yytchar= (c);\
    if(yytchar=='\n')\
        yylineno--;\
    *yysptr++=yytchar;\
}

/* externally callable function for unput:
 */

wool_unput(buffer)
char *buffer;
{
    while (*buffer) {
	unput(*buffer);
	buffer++;
    }
}

/* counting the parentheses -- hack for wool_pool */

#define INC_PAR wool_pool_parentheses_level++
#define DEC_PAR if(wool_pool_parentheses_level)wool_pool_parentheses_level--
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
	;
break;
case 2:
	;
break;
case 3:
 return(STRING);
break;
case 4:
 return(NON_CLOSED_STRING);
break;
case 5:
	;
break;
case 6:
return(NUMBER);
break;
case 7:
return(HEX_NUMBER);
break;
case 8:
	{INC_PAR; return(LEFTPAR);}
break;
case 9:
	{DEC_PAR; return(RIGHTPAR);}
break;
case 10:
	return(QUOTECHAR);
break;
case 11:
	{INC_PAR; return(LEFTBRA);}
break;
case 12:
	{DEC_PAR; return(RIGHTBRA);}
break;
case 13:
return (NAME);
break;
case 14:
return (NAME);
break;
case 15:
	return (NAME);
break;
case 16:
	;
break;
case 17:
	return(END_OF_FILE);
break;
case 18:
	;
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

/**********************\
* 		       *
*  WOOL's I/O package  *
* 		       *
\**********************/

/* 
 * yywrap
 * we treat EOF (or EOString) as a TOKEN, for yacc.
 */

yywrap(){		/* to make EOF a "normal" character */
    unput('\004');	/* EOF is pushed back on the input as ^D */
    return 0;		/* tell Lex there is more to read */
}

/*
 * yyinflush
 * to flush wool input buffers (i.e. the unput buffer)
 * stores old input buffer in arg (should be ~ 16) if not NULL
 */

void
yyinflush(wool_old_unput_buffer)
char *wool_old_unput_buffer;
{
    if (wool_old_unput_buffer) {
	if (yysptr != yysbuf)
	    strncpy(wool_old_unput_buffer, yysbuf, yysptr - yysbuf);
	wool_old_unput_buffer[yysptr - yysbuf] = '\0';
    }
    yysptr = yysbuf;
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
 * arg4 = where to save contents of unput buffer (if not NULL)
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
    yyinflush(old_buffer_contents);
    if(yyin_is_string = type){
	yystrin = stream;
    }else{
	yyin = (FILE *) stream;
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
int yyvstop[] = {
0,

13,
18,
0,

17,
18,
0,

16,
18,
0,

16,
0,

18,
0,

10,
18,
0,

8,
18,
0,

9,
18,
0,

15,
18,
0,

6,
18,
0,

6,
18,
0,

13,
18,
0,

11,
13,
18,
0,

12,
13,
18,
0,

13,
0,

1,
13,
0,

2,
13,
0,

1,
0,

2,
0,

1,
0,

4,
0,

3,
0,

2,
0,

14,
0,

1,
14,
0,

6,
0,

2,
14,
0,

13,
0,

1,
13,
0,

5,
0,

2,
13,
0,

7,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	1,4,	0,0,	0,0,	
0,0,	1,3,	1,5,	1,6,	
4,20,	0,0,	22,22,	0,0,	
0,0,	0,0,	23,22,	27,22,	
33,33,	35,33,	0,0,	37,33,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,7,	
0,0,	0,0,	0,0,	0,0,	
1,8,	1,9,	1,10,	0,0,	
1,11,	25,22,	0,0,	0,0,	
0,0,	1,12,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	0,0,	
1,14,	2,9,	2,10,	0,0,	
13,20,	0,0,	1,3,	0,0,	
0,0,	0,0,	2,13,	2,13,	
2,13,	2,13,	2,13,	2,13,	
2,13,	2,13,	2,13,	0,0,	
2,14,	0,0,	0,0,	3,17,	
0,0,	0,0,	3,0,	0,0,	
0,0,	1,3,	3,18,	3,0,	
3,0,	1,3,	7,22,	0,0,	
0,0,	7,22,	0,0,	0,0,	
0,0,	7,23,	7,22,	7,24,	
13,30,	13,30,	13,30,	13,30,	
13,30,	13,30,	13,30,	13,30,	
13,30,	13,30,	3,0,	0,0,	
3,0,	26,22,	0,0,	26,22,	
0,0,	3,0,	3,0,	3,0,	
1,15,	3,17,	1,16,	7,25,	
1,3,	15,0,	3,17,	4,21,	
7,22,	22,22,	15,0,	15,0,	
7,22,	23,22,	27,22,	33,33,	
35,33,	7,22,	37,33,	26,22,	
2,15,	0,0,	2,16,	3,17,	
0,0,	0,0,	0,0,	0,0,	
0,0,	11,28,	0,0,	0,0,	
11,0,	15,0,	7,22,	15,0,	
11,29,	11,0,	11,0,	0,0,	
15,0,	15,0,	15,0,	0,0,	
0,0,	14,33,	3,17,	0,0,	
14,34,	0,0,	3,17,	0,0,	
14,35,	14,34,	14,36,	0,0,	
0,0,	7,22,	12,20,	13,21,	
11,0,	7,26,	11,0,	0,0,	
0,0,	0,0,	0,0,	11,0,	
11,0,	11,0,	0,0,	11,28,	
0,0,	0,0,	0,0,	0,0,	
11,30,	26,22,	14,34,	0,0,	
0,0,	0,0,	0,0,	14,34,	
0,0,	3,19,	0,0,	14,33,	
0,0,	16,0,	0,0,	0,0,	
14,33,	11,28,	16,0,	16,0,	
7,27,	0,0,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
0,0,	14,33,	0,0,	0,0,	
26,22,	0,0,	0,0,	0,0,	
11,28,	16,0,	0,0,	16,0,	
11,28,	0,0,	0,0,	17,0,	
16,0,	16,0,	16,0,	17,17,	
17,0,	17,0,	0,0,	0,0,	
14,33,	0,0,	0,0,	0,0,	
14,33,	18,0,	12,32,	0,0,	
0,0,	18,17,	18,0,	18,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	17,0,	
0,0,	17,0,	0,0,	11,31,	
0,0,	19,0,	17,0,	17,0,	
17,0,	19,17,	19,0,	19,0,	
0,0,	18,0,	0,0,	18,0,	
0,0,	0,0,	12,32,	14,37,	
18,0,	18,0,	18,0,	28,0,	
0,0,	12,21,	29,0,	28,28,	
28,0,	28,0,	29,28,	29,0,	
29,0,	19,0,	0,0,	19,0,	
0,0,	0,0,	0,0,	0,0,	
19,0,	19,0,	19,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	28,0,	
0,0,	28,0,	29,0,	0,0,	
29,0,	0,0,	28,0,	28,0,	
28,0,	29,0,	29,0,	29,0,	
0,0,	0,0,	0,0,	28,28,	
0,0,	0,0,	29,28,	30,30,	
30,30,	30,30,	30,30,	30,30,	
30,30,	30,30,	30,30,	30,30,	
30,30,	31,0,	0,0,	0,0,	
34,34,	31,28,	31,0,	31,0,	
0,0,	0,0,	0,0,	34,34,	
0,0,	0,0,	17,17,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
18,17,	31,0,	0,0,	31,0,	
0,0,	0,0,	0,0,	0,0,	
31,0,	31,0,	31,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	31,28,	34,34,	0,0,	
19,17,	0,0,	0,0,	34,34,	
0,0,	32,38,	32,38,	32,38,	
32,38,	32,38,	32,38,	32,38,	
32,38,	32,38,	32,38,	0,0,	
0,0,	0,0,	28,28,	0,0,	
34,34,	29,28,	32,38,	32,38,	
32,38,	32,38,	32,38,	32,38,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	34,34,	
0,0,	0,0,	0,0,	34,34,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	32,38,	32,38,	
32,38,	32,38,	32,38,	32,38,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
31,28,	0,0,	0,0,	0,0,	
0,0,	0,0,	34,34,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-21,	yysvec+1,	0,	
yycrank+-82,	0,		yyvstop+1,
yycrank+4,	0,		yyvstop+4,
yycrank+0,	yysvec+4,	yyvstop+7,
yycrank+0,	0,		yyvstop+10,
yycrank+-93,	0,		yyvstop+12,
yycrank+0,	yysvec+4,	yyvstop+14,
yycrank+0,	yysvec+4,	yyvstop+17,
yycrank+0,	yysvec+4,	yyvstop+20,
yycrank+-152,	0,		yyvstop+23,
yycrank+174,	0,		yyvstop+26,
yycrank+56,	0,		yyvstop+29,
yycrank+-168,	0,		yyvstop+32,
yycrank+-125,	yysvec+3,	yyvstop+35,
yycrank+-209,	yysvec+3,	yyvstop+39,
yycrank+-243,	yysvec+3,	yyvstop+43,
yycrank+-257,	yysvec+3,	yyvstop+45,
yycrank+-277,	yysvec+3,	yyvstop+48,
yycrank+0,	0,		yyvstop+51,
yycrank+0,	0,		yyvstop+53,
yycrank+-6,	yysvec+7,	0,	
yycrank+-10,	yysvec+7,	yyvstop+55,
yycrank+0,	0,		yyvstop+57,
yycrank+11,	0,		yyvstop+59,
yycrank+-109,	yysvec+7,	0,	
yycrank+-11,	yysvec+7,	yyvstop+61,
yycrank+-295,	yysvec+11,	yyvstop+63,
yycrank+-298,	yysvec+11,	yyvstop+65,
yycrank+299,	0,		yyvstop+68,
yycrank+-353,	yysvec+11,	yyvstop+70,
yycrank+361,	0,		0,	
yycrank+-12,	yysvec+14,	yyvstop+73,
yycrank+-359,	yysvec+14,	0,	
yycrank+-13,	yysvec+14,	yyvstop+75,
yycrank+0,	0,		yyvstop+78,
yycrank+-15,	yysvec+14,	yyvstop+80,
yycrank+0,	yysvec+32,	yyvstop+83,
0,	0,	0};
struct yywork *yytop = yycrank+486;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,04  ,01  ,01  ,01  ,
010 ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,'"' ,01  ,01  ,01  ,01  ,047 ,
047 ,047 ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'X' ,01  ,01  ,01  ,0134,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'X' ,01  ,01  ,01  ,01  ,01  ,01  ,0177,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
