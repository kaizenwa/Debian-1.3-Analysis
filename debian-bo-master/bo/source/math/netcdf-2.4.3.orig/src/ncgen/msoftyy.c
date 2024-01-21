#include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
#ifndef __cplusplus
# define output(c) (void)putc(c,yyout)
#else
# define lex_output(c) (void)putc(c,yyout)
#endif

#if defined(__cplusplus) || defined(__STDC__)

#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int yylex(void);
#ifdef YYLEX_E
	void yywoutput(wchar_t);
	wchar_t yywinput(void);
#endif
#ifndef yyless
	void yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
	void exit(int);
#ifdef __cplusplus
}
#endif

#endif
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
#ifndef __cplusplus
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#else
# define lex_input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif
#define ECHO fprintf(yyout, "%s",yytext)
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

# line 3 "ncgen.l"
/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf/ncgen/msoftyy.c,v 1.4 1995/12/06 15:48:48 russ Exp $
 *********************************************************************/

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)


# line 11 "ncgen.l"
/* lex specification for tokens for ncgen */


# line 13 "ncgen.l"
/* Fill value used by ncdump from version 2.4 and later.  Should match
   definition of FILL_STRING in ../ncdump/vardata.h */
#define FILL_STRING "_"

char errstr[100];		/* for short error messages */
extern long strtol();
void expand_escapes();

#include <string.h>
#include <ctype.h>
#include <limits.h>
#include "ncgentab.h"
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
#ifdef __cplusplus
/* to avoid CC and lint complaining yyfussy not being used ...*/
static int __lex_hack = 0;
if (__lex_hack) goto yyfussy;
#endif
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 30 "ncgen.l"
	/* comment */ ;
break;
case 2:

# line 32 "ncgen.l"
	{
			 if(yyleng > MAXTRST) {
				yyerror("string too long, truncated\n");
			        yytext[MAXTRST-1] = '\0';
			 }
			 expand_escapes(termstring,yytext,yyleng);
		 	 return (TERMSTRING);
		        }
break;
case 3:

# line 41 "ncgen.l"
{return (FLOAT_K);}
break;
case 4:

# line 42 "ncgen.l"
	{return (CHAR_K);}
break;
case 5:

# line 43 "ncgen.l"
	{return (BYTE_K);}
break;
case 6:

# line 44 "ncgen.l"
	{return (SHORT_K);}
break;
case 7:

# line 45 "ncgen.l"
{return (LONG_K);}
break;
case 8:

# line 46 "ncgen.l"
	{return (DOUBLE_K);}
break;
case 9:

# line 47 "ncgen.l"
{long_val = -1;
			 return (NC_UNLIMITED_K);}
break;
case 10:

# line 50 "ncgen.l"
{return (DIMENSIONS);}
break;
case 11:

# line 51 "ncgen.l"
{return (VARIABLES);}
break;
case 12:

# line 52 "ncgen.l"
	{return (DATA);}
break;
case 13:

# line 53 "ncgen.l"
{
		char *s = (char*)yytext+strlen("netcdf");
		char *t = (char*)yytext+yyleng-1;
		while (isspace(*s))
			s++;
		while (isspace(*t))
			t--;
		t++;
		netcdfname = (char *) emalloc(t-s+1);
		(void) strncpy(netcdfname, s, t-s);
		netcdfname[t-s] = '\0';
		return (NETCDF);
		}
break;
case 14:

# line 66 "ncgen.l"
{    /* missing value (pre-2.4 backward compatibility) */
		double_val = FILL_DOUBLE;  /* IEEE double infinity */
		return (DOUBLE_CONST);
		}
break;
case 15:

# line 70 "ncgen.l"
{ /* missing value (pre-2.4 backward compatibility) */
		float_val = FILL_FLOAT;  /* IEEE float infinity */
		return (FLOAT_CONST);
		}
break;
case 16:

# line 74 "ncgen.l"
{
                if (STREQ((char *)yytext, FILL_STRING))
		        return (FILLVALUE);
		if ((yylval = lookup(yytext)) == NULL) {
			yylval = install(yytext);
			}
		return (IDENT);
		}
break;
case 17:

# line 83 "ncgen.l"
	{
		lineno++ ;
		}
break;
case 18:

# line 86 "ncgen.l"
{
		if (sscanf((char*)yytext, "%le", &double_val) != 1) {
		    sprintf(errstr,"bad long or double constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                return (DOUBLE_CONST);
                }
break;
case 19:

# line 93 "ncgen.l"
{
		if (sscanf((char*)yytext, "%e", &float_val) != 1) {
		    sprintf(errstr,"bad float constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                return (FLOAT_CONST);
                }
break;
case 20:

# line 100 "ncgen.l"
{
		if (sscanf((char*)yytext, "%hd", &short_val) != 1) {
		    sprintf(errstr,"bad short constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		return (SHORT_CONST);
	        }
break;
case 21:

# line 107 "ncgen.l"
{
#ifdef cray	/* machines where longs have more precision than doubles. */
    		char *ptr;
		long_val = strtol((char*)yytext, &ptr, 0);
		if (ptr == (char*)yytext) {
		    sprintf(errstr,"bad long constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		return (LONG_CONST);
#else		/* machines where doubles have more precision than longs. */
		/*
		 * Because strtol and sscanf with "%ld" may silently give
		 * bad results from undetected overflow for strings like
		 * "30000000000", we scan as double first.
		 */
		double dd;
#ifdef VMS  /* work around bug in VMS strtol() */
		if (STREQ((char*)yytext, "-2147483648")) {
		    long_val = -2147483648;
		    return (LONG_CONST);
		}
#endif /* VMS */
		if (sscanf((char*)yytext, "%le", &dd) != 1) {
		    sprintf(errstr,"bad long constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		if (dd < LONG_MIN  ||  dd > LONG_MAX) {
		    double_val = dd;
		    return DOUBLE_CONST;
		} else {
		    long_val = dd;
		    return LONG_CONST;
		}
#endif /* cray */
	        }
break;
case 22:

# line 142 "ncgen.l"
{
		long dd;
#ifdef VMS  /* work around bug in VMS strtol() */
		if (STREQ((char*)yytext, "-2147483648")) {
		    long_val = -2147483648;
		    return (LONG_CONST);
		}
#endif /* VMS */
		if (sscanf((char*)yytext, "%li", &dd) != 1) {
		    sprintf(errstr,"bad long constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		long_val = dd;
		return LONG_CONST;
	        }
break;
case 23:

# line 157 "ncgen.l"
         {
	        (void) sscanf((char*)&yytext[1],"%c",&byte_val);
		return (BYTE_CONST);
                }
break;
case 24:

# line 161 "ncgen.l"
 {
		byte_val = strtol((char*)&yytext[2], (char **) 0, 8);
		return (BYTE_CONST);
                }
break;
case 25:

# line 165 "ncgen.l"
 {
		byte_val = strtol((char*)&yytext[3], (char **) 0, 16);
		return (BYTE_CONST);
                }
break;
case 26:

# line 169 "ncgen.l"
       {
	       switch ((char)yytext[2]) {
	          case 'a': byte_val = '\007'; break; /* not everyone under-
						       * stands '\a' yet */
     	          case 'b': byte_val = '\b'; break;
		  case 'f': byte_val = '\f'; break;
		  case 'n': byte_val = '\n'; break;
		  case 'r': byte_val = '\r'; break;
		  case 't': byte_val = '\t'; break;
		  case 'v': byte_val = '\v'; break;
		  case '\\': byte_val = '\\'; break;
		  case '?': byte_val = '\177'; break;
		  case '\'': byte_val = '\''; break;
		  default: byte_val = (char)yytext[2];
	           }
		return (BYTE_CONST);
                }
break;
case 27:

# line 187 "ncgen.l"
{/* whitespace */ ;
		}
break;
case 28:

# line 189 "ncgen.l"
	return (yytext[0]) ;
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

28,
0,

27,
28,
0,

17,
0,

28,
0,

28,
0,

28,
0,

28,
0,

18,
28,
0,

28,
0,

21,
28,
0,

21,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

16,
28,
0,

27,
0,

2,
0,

18,
0,

21,
0,

21,
0,

18,
0,

19,
0,

1,
0,

22,
0,

22,
0,

22,
0,

21,
0,

20,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
18,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

2,
0,

23,
0,

18,
0,

18,
0,

22,
0,

18,
22,
0,

22,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
18,
0,

16,
19,
0,

16,
0,

16,
0,

7,
16,
0,

15,
16,
0,

16,
0,

16,
0,

14,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

7,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

26,
0,

24,
26,
0,

14,
0,

18,
22,
0,

19,
22,
0,

18,
22,
0,

5,
16,
0,

4,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

7,
16,
0,

16,
0,

3,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

24,
0,

25,
0,

12,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

6,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

8,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

15,
16,
0,

13,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

9,
16,
0,

16,
0,

16,
0,

16,
0,

16,
0,

11,
0,

16,
0,

10,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	1,4,	4,38,	0,0,	
0,0,	4,38,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,6,	
41,93,	4,38,	0,0,	0,0,	
1,7,	42,94,	95,138,	0,0,	
1,8,	0,0,	1,9,	1,10,	
1,11,	1,12,	1,13,	11,52,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,13,	0,0,	0,0,	
13,46,	148,176,	0,0,	0,0,	
0,0,	0,0,	1,14,	1,15,	
1,16,	1,17,	1,18,	1,19,	
1,14,	23,76,	1,20,	16,61,	
22,74,	1,21,	19,69,	1,22,	
9,48,	21,73,	17,62,	1,23,	
1,24,	20,71,	1,25,	1,26,	
13,56,	1,14,	17,63,	15,60,	
24,77,	1,3,	25,78,	13,57,	
17,64,	26,79,	60,106,	1,27,	
1,28,	1,29,	37,92,	1,30,	
22,75,	2,7,	1,31,	28,81,	
33,88,	1,32,	19,70,	1,33,	
2,10,	2,11,	30,85,	1,34,	
1,35,	20,72,	1,36,	1,37,	
13,56,	29,82,	31,86,	27,80,	
1,3,	32,87,	34,89,	13,57,	
17,65,	29,83,	35,90,	36,91,	
2,15,	2,16,	48,100,	29,84,	
61,107,	62,108,	63,109,	2,20,	
64,110,	65,111,	69,114,	70,115,	
2,22,	6,39,	71,116,	72,117,	
2,23,	73,118,	74,119,	2,25,	
2,26,	6,39,	6,39,	75,120,	
6,39,	76,121,	77,122,	78,123,	
79,124,	80,125,	81,126,	82,127,	
2,27,	2,28,	2,29,	83,128,	
2,30,	84,129,	85,130,	2,31,	
86,131,	87,132,	2,32,	46,46,	
2,33,	88,133,	6,40,	89,134,	
2,34,	2,35,	90,135,	2,36,	
2,37,	91,136,	7,42,	6,39,	
92,137,	6,39,	100,142,	106,146,	
6,39,	6,39,	7,42,	7,42,	
107,147,	7,42,	108,148,	105,103,	
6,39,	109,149,	110,150,	46,56,	
111,151,	114,152,	105,57,	115,153,	
116,154,	6,39,	46,57,	117,155,	
6,39,	6,39,	6,39,	6,39,	
118,156,	119,157,	121,158,	7,42,	
6,39,	122,159,	123,160,	124,161,	
125,146,	126,147,	127,162,	6,39,	
7,42,	128,163,	7,42,	105,103,	
6,39,	7,42,	7,42,	46,56,	
6,41,	129,164,	105,57,	130,165,	
131,166,	7,42,	46,57,	132,156,	
134,158,	135,169,	136,170,	137,171,	
149,177,	150,178,	7,42,	151,179,	
152,158,	7,42,	7,42,	7,42,	
7,42,	153,180,	154,181,	155,182,	
157,183,	7,42,	159,184,	160,185,	
161,186,	162,176,	163,187,	6,39,	
7,42,	133,167,	164,188,	165,158,	
166,189,	7,42,	167,190,	168,191,	
8,44,	7,43,	8,45,	8,46,	
8,46,	8,46,	8,46,	8,46,	
8,46,	8,46,	8,46,	8,46,	
169,184,	10,44,	10,44,	10,44,	
10,44,	10,44,	10,44,	10,44,	
10,44,	10,44,	10,44,	8,47,	
170,192,	133,168,	171,193,	173,172,	
175,174,	177,194,	178,195,	179,196,	
7,42,	10,49,	10,50,	10,51,	
180,197,	181,198,	182,199,	183,200,	
12,44,	10,49,	12,53,	12,53,	
12,53,	12,53,	12,53,	12,53,	
12,53,	12,53,	12,53,	12,53,	
185,201,	186,202,	187,203,	8,47,	
188,195,	189,204,	190,200,	12,54,	
12,54,	12,54,	12,54,	12,55,	
12,54,	10,49,	10,50,	10,51,	
191,200,	192,205,	12,56,	193,206,	
194,207,	10,49,	96,139,	196,208,	
197,209,	12,57,	198,156,	199,210,	
200,211,	201,212,	12,58,	96,140,	
96,140,	96,140,	96,140,	96,140,	
96,140,	96,140,	96,140,	12,54,	
12,54,	12,54,	12,54,	12,55,	
12,54,	202,213,	203,214,	204,156,	
205,215,	206,216,	12,56,	200,211,	
207,217,	208,218,	209,219,	210,219,	
212,221,	12,57,	213,222,	214,223,	
14,59,	215,224,	12,58,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	216,225,	217,226,	218,120,	
220,0,	221,227,	222,228,	223,229,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	224,227,	225,230,	
226,231,	228,232,	14,59,	229,233,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	14,59,	14,59,	
14,59,	14,59,	18,66,	230,232,	
18,67,	231,234,	233,234,	18,68,	
18,68,	18,68,	18,68,	18,68,	
18,68,	18,68,	18,68,	18,68,	
18,68,	43,95,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	43,95,	43,0,	0,0,	
43,95,	0,0,	0,0,	0,0,	
0,0,	45,44,	0,0,	45,98,	
45,98,	45,98,	45,98,	45,98,	
45,98,	45,98,	45,98,	45,98,	
45,98,	0,0,	0,0,	0,0,	
0,0,	0,0,	43,95,	0,0,	
0,0,	0,0,	0,0,	0,0,	
45,47,	0,0,	0,0,	43,95,	
0,0,	43,95,	0,0,	45,56,	
43,96,	43,96,	99,49,	0,0,	
99,51,	102,49,	45,57,	102,51,	
43,95,	0,0,	99,49,	0,0,	
0,0,	102,49,	0,0,	0,0,	
0,0,	43,95,	0,0,	0,0,	
43,95,	43,95,	43,95,	43,95,	
45,47,	0,0,	0,0,	0,0,	
43,95,	0,0,	0,0,	45,56,	
0,0,	0,0,	99,49,	43,95,	
99,51,	102,49,	45,57,	102,51,	
43,97,	47,66,	99,49,	47,66,	
43,95,	102,49,	47,99,	47,99,	
47,99,	47,99,	47,99,	47,99,	
47,99,	47,99,	47,99,	47,99,	
50,101,	0,0,	50,101,	0,0,	
0,0,	50,102,	50,102,	50,102,	
50,102,	50,102,	50,102,	50,102,	
50,102,	50,102,	50,102,	52,52,	
0,0,	0,0,	0,0,	43,95,	
0,0,	0,0,	0,0,	52,52,	
52,0,	55,66,	52,52,	55,66,	
0,0,	0,0,	55,104,	55,104,	
55,104,	55,104,	55,104,	55,104,	
55,104,	55,104,	55,104,	55,104,	
140,172,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
52,52,	140,173,	140,173,	140,173,	
140,173,	140,173,	140,173,	140,173,	
140,173,	52,52,	0,0,	52,52,	
0,0,	0,0,	52,52,	52,52,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	52,52,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	52,52,	
0,0,	0,0,	52,52,	52,52,	
52,52,	52,52,	0,0,	0,0,	
0,0,	0,0,	52,52,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	52,52,	0,0,	0,0,	
0,0,	0,0,	52,52,	0,0,	
0,0,	53,44,	52,52,	53,53,	
53,53,	53,53,	53,53,	53,53,	
53,53,	53,53,	53,53,	53,53,	
53,53,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
53,54,	53,54,	53,54,	53,54,	
53,55,	53,54,	0,0,	0,0,	
0,0,	0,0,	0,0,	53,103,	
0,0,	52,52,	0,0,	0,0,	
0,0,	0,0,	53,57,	66,99,	
66,99,	66,99,	66,99,	66,99,	
66,99,	66,99,	66,99,	66,99,	
66,99,	0,0,	0,0,	0,0,	
53,54,	53,54,	53,54,	53,54,	
53,55,	53,54,	0,0,	0,0,	
0,0,	0,0,	0,0,	53,103,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	53,57,	54,54,	
54,54,	54,54,	54,54,	54,54,	
54,54,	54,54,	54,54,	54,54,	
54,54,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
54,54,	54,54,	54,54,	54,54,	
54,54,	54,54,	0,0,	0,0,	
0,0,	0,0,	0,0,	54,103,	
67,68,	67,68,	67,68,	67,68,	
67,68,	67,68,	67,68,	67,68,	
67,68,	67,68,	98,98,	98,98,	
98,98,	98,98,	98,98,	98,98,	
98,98,	98,98,	98,98,	98,98,	
54,54,	54,54,	54,54,	54,54,	
54,54,	54,54,	0,0,	0,0,	
0,0,	0,0,	0,0,	54,103,	
58,105,	58,105,	58,105,	58,105,	
58,105,	58,105,	58,105,	58,105,	
58,105,	58,105,	0,0,	0,0,	
0,0,	98,57,	0,0,	0,0,	
0,0,	58,105,	58,105,	58,105,	
58,105,	58,105,	58,105,	101,102,	
101,102,	101,102,	101,102,	101,102,	
101,102,	101,102,	101,102,	101,102,	
101,102,	0,0,	0,0,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	98,57,	0,0,	0,0,	
0,0,	58,105,	58,105,	58,105,	
58,105,	58,105,	58,105,	68,112,	
0,0,	68,113,	0,0,	0,0,	
0,0,	0,0,	0,0,	68,112,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	68,112,	
97,138,	68,113,	0,0,	0,0,	
0,0,	0,0,	0,0,	68,112,	
0,0,	97,141,	97,141,	97,141,	
97,141,	97,141,	97,141,	97,141,	
97,141,	97,141,	97,141,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	97,141,	97,141,	
97,141,	97,141,	97,141,	97,141,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
104,104,	104,104,	104,104,	104,104,	
104,104,	104,104,	104,104,	104,104,	
104,104,	104,104,	0,0,	0,0,	
0,0,	0,0,	97,141,	97,141,	
97,141,	97,141,	97,141,	97,141,	
104,143,	0,0,	104,144,	0,0,	
0,0,	0,0,	0,0,	0,0,	
104,145,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
104,143,	141,174,	104,144,	0,0,	
0,0,	0,0,	0,0,	0,0,	
104,145,	0,0,	141,175,	141,175,	
141,175,	141,175,	141,175,	141,175,	
141,175,	141,175,	141,175,	141,175,	
0,0,	0,0,	0,0,	0,0,	
0,0,	211,220,	0,0,	141,175,	
141,175,	141,175,	141,175,	141,175,	
141,175,	211,220,	211,220,	0,0,	
211,220,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	211,220,	141,175,	
141,175,	141,175,	141,175,	141,175,	
141,175,	0,0,	0,0,	211,220,	
0,0,	211,220,	0,0,	0,0,	
211,220,	211,220,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
211,220,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	211,220,	0,0,	0,0,	
211,220,	211,220,	211,220,	211,220,	
0,0,	0,0,	0,0,	0,0,	
211,220,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	211,220,	
0,0,	0,0,	0,0,	0,0,	
211,220,	0,0,	0,0,	0,0,	
211,220,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	211,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-66,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+5,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+-144,	0,		yyvstop+8,
yycrank+-185,	0,		yyvstop+10,
yycrank+230,	0,		yyvstop+12,
yycrank+2,	yysvec+8,	yyvstop+14,
yycrank+241,	0,		yyvstop+16,
yycrank+4,	0,		yyvstop+19,
yycrank+270,	0,		yyvstop+21,
yycrank+12,	yysvec+8,	yyvstop+24,
yycrank+343,	0,		yyvstop+27,
yycrank+2,	yysvec+14,	yyvstop+30,
yycrank+3,	yysvec+14,	yyvstop+33,
yycrank+17,	yysvec+14,	yyvstop+36,
yycrank+423,	yysvec+14,	yyvstop+39,
yycrank+2,	yysvec+14,	yyvstop+42,
yycrank+7,	yysvec+14,	yyvstop+45,
yycrank+2,	yysvec+14,	yyvstop+48,
yycrank+7,	yysvec+14,	yyvstop+51,
yycrank+4,	yysvec+14,	yyvstop+54,
yycrank+20,	yysvec+14,	yyvstop+57,
yycrank+16,	yysvec+14,	yyvstop+60,
yycrank+32,	yysvec+14,	yyvstop+63,
yycrank+2,	yysvec+14,	yyvstop+66,
yycrank+3,	yysvec+14,	yyvstop+69,
yycrank+24,	yysvec+14,	yyvstop+72,
yycrank+6,	yysvec+14,	yyvstop+75,
yycrank+12,	yysvec+14,	yyvstop+78,
yycrank+14,	yysvec+14,	yyvstop+81,
yycrank+7,	yysvec+14,	yyvstop+84,
yycrank+25,	yysvec+14,	yyvstop+87,
yycrank+26,	yysvec+14,	yyvstop+90,
yycrank+21,	yysvec+14,	yyvstop+93,
yycrank+5,	yysvec+14,	yyvstop+96,
yycrank+0,	yysvec+4,	yyvstop+99,
yycrank+0,	yysvec+6,	0,	
yycrank+0,	0,		yyvstop+101,
yycrank+-2,	yysvec+6,	0,	
yycrank+2,	0,		0,	
yycrank+-480,	0,		0,	
yycrank+0,	yysvec+10,	yyvstop+103,
yycrank+451,	0,		yyvstop+105,
yycrank+127,	yysvec+8,	yyvstop+107,
yycrank+526,	0,		0,	
yycrank+37,	0,		0,	
yycrank+0,	0,		yyvstop+109,
yycrank+541,	0,		0,	
yycrank+0,	0,		yyvstop+111,
yycrank+-598,	0,		yyvstop+113,
yycrank+643,	0,		yyvstop+115,
yycrank+711,	0,		yyvstop+117,
yycrank+566,	yysvec+54,	yyvstop+119,
yycrank+0,	0,		yyvstop+121,
yycrank+0,	0,		yyvstop+123,
yycrank+772,	0,		0,	
yycrank+0,	yysvec+14,	yyvstop+125,
yycrank+14,	yysvec+14,	yyvstop+127,
yycrank+71,	yysvec+14,	yyvstop+129,
yycrank+53,	yysvec+14,	yyvstop+131,
yycrank+61,	yysvec+14,	yyvstop+133,
yycrank+55,	yysvec+14,	yyvstop+135,
yycrank+24,	yysvec+14,	yyvstop+137,
yycrank+679,	0,		0,	
yycrank+740,	yysvec+14,	yyvstop+139,
yycrank+807,	yysvec+14,	yyvstop+141,
yycrank+63,	yysvec+14,	yyvstop+144,
yycrank+32,	yysvec+14,	yyvstop+146,
yycrank+62,	yysvec+14,	yyvstop+148,
yycrank+45,	yysvec+14,	yyvstop+150,
yycrank+71,	yysvec+14,	yyvstop+152,
yycrank+66,	yysvec+14,	yyvstop+154,
yycrank+77,	yysvec+14,	yyvstop+156,
yycrank+92,	yysvec+14,	yyvstop+158,
yycrank+79,	yysvec+14,	yyvstop+160,
yycrank+83,	yysvec+14,	yyvstop+162,
yycrank+78,	yysvec+14,	yyvstop+164,
yycrank+45,	yysvec+14,	yyvstop+166,
yycrank+65,	yysvec+14,	yyvstop+168,
yycrank+47,	yysvec+14,	yyvstop+170,
yycrank+58,	yysvec+14,	yyvstop+172,
yycrank+52,	yysvec+14,	yyvstop+174,
yycrank+59,	yysvec+14,	yyvstop+176,
yycrank+56,	yysvec+14,	yyvstop+178,
yycrank+63,	yysvec+14,	yyvstop+180,
yycrank+61,	yysvec+14,	yyvstop+182,
yycrank+82,	yysvec+14,	yyvstop+184,
yycrank+71,	yysvec+14,	yyvstop+186,
yycrank+77,	yysvec+14,	yyvstop+188,
yycrank+74,	yysvec+14,	yyvstop+190,
yycrank+0,	yysvec+6,	yyvstop+192,
yycrank+0,	0,		yyvstop+194,
yycrank+3,	0,		0,	
yycrank+311,	0,		0,	
yycrank+869,	0,		0,	
yycrank+750,	yysvec+8,	0,	
yycrank+462,	yysvec+66,	yyvstop+196,
yycrank+112,	0,		0,	
yycrank+795,	0,		0,	
yycrank+465,	yysvec+101,	yyvstop+198,
yycrank+0,	0,		yyvstop+200,
yycrank+904,	yysvec+54,	yyvstop+202,
yycrank+123,	yysvec+58,	yyvstop+205,
yycrank+122,	yysvec+14,	yyvstop+207,
yycrank+114,	yysvec+14,	yyvstop+209,
yycrank+133,	yysvec+14,	yyvstop+211,
yycrank+132,	yysvec+14,	yyvstop+213,
yycrank+136,	yysvec+14,	yyvstop+215,
yycrank+106,	yysvec+14,	yyvstop+217,
yycrank+0,	yysvec+14,	yyvstop+219,
yycrank+0,	yysvec+14,	yyvstop+222,
yycrank+140,	yysvec+14,	yyvstop+225,
yycrank+110,	yysvec+14,	yyvstop+227,
yycrank+139,	yysvec+14,	yyvstop+229,
yycrank+106,	yysvec+14,	yyvstop+232,
yycrank+145,	yysvec+14,	yyvstop+235,
yycrank+150,	yysvec+14,	yyvstop+237,
yycrank+0,	yysvec+14,	yyvstop+239,
yycrank+142,	yysvec+14,	yyvstop+242,
yycrank+139,	yysvec+14,	yyvstop+244,
yycrank+149,	yysvec+14,	yyvstop+246,
yycrank+150,	yysvec+14,	yyvstop+248,
yycrank+123,	yysvec+14,	yyvstop+250,
yycrank+111,	yysvec+14,	yyvstop+252,
yycrank+129,	yysvec+14,	yyvstop+254,
yycrank+128,	yysvec+14,	yyvstop+256,
yycrank+139,	yysvec+14,	yyvstop+258,
yycrank+142,	yysvec+14,	yyvstop+260,
yycrank+139,	yysvec+14,	yyvstop+262,
yycrank+140,	yysvec+14,	yyvstop+265,
yycrank+202,	yysvec+14,	yyvstop+267,
yycrank+136,	yysvec+14,	yyvstop+269,
yycrank+131,	yysvec+14,	yyvstop+271,
yycrank+141,	yysvec+14,	yyvstop+273,
yycrank+142,	yysvec+14,	yyvstop+275,
yycrank+0,	0,		yyvstop+277,
yycrank+0,	0,		yyvstop+279,
yycrank+585,	0,		0,	
yycrank+966,	0,		0,	
yycrank+0,	0,		yyvstop+282,
yycrank+0,	yysvec+54,	yyvstop+284,
yycrank+0,	yysvec+54,	yyvstop+287,
yycrank+0,	0,		yyvstop+290,
yycrank+0,	yysvec+14,	yyvstop+293,
yycrank+0,	yysvec+14,	yyvstop+296,
yycrank+3,	yysvec+14,	yyvstop+299,
yycrank+170,	yysvec+14,	yyvstop+301,
yycrank+173,	yysvec+14,	yyvstop+303,
yycrank+143,	yysvec+14,	yyvstop+305,
yycrank+168,	yysvec+14,	yyvstop+307,
yycrank+141,	yysvec+14,	yyvstop+309,
yycrank+187,	yysvec+14,	yyvstop+311,
yycrank+149,	yysvec+14,	yyvstop+313,
yycrank+0,	yysvec+14,	yyvstop+315,
yycrank+192,	yysvec+14,	yyvstop+318,
yycrank+0,	yysvec+14,	yyvstop+320,
yycrank+178,	yysvec+14,	yyvstop+323,
yycrank+186,	yysvec+14,	yyvstop+325,
yycrank+199,	yysvec+14,	yyvstop+327,
yycrank+207,	yysvec+14,	yyvstop+329,
yycrank+156,	yysvec+14,	yyvstop+331,
yycrank+162,	yysvec+14,	yyvstop+333,
yycrank+155,	yysvec+14,	yyvstop+335,
yycrank+169,	yysvec+14,	yyvstop+337,
yycrank+206,	yysvec+14,	yyvstop+339,
yycrank+175,	yysvec+14,	yyvstop+341,
yycrank+172,	yysvec+14,	yyvstop+343,
yycrank+191,	yysvec+14,	yyvstop+345,
yycrank+205,	yysvec+14,	yyvstop+347,
yycrank+0,	0,		yyvstop+349,
yycrank+264,	0,		0,	
yycrank+0,	0,		yyvstop+351,
yycrank+265,	0,		0,	
yycrank+0,	0,		yyvstop+353,
yycrank+222,	yysvec+14,	yyvstop+355,
yycrank+237,	yysvec+14,	yyvstop+357,
yycrank+206,	yysvec+14,	yyvstop+359,
yycrank+239,	yysvec+14,	yyvstop+361,
yycrank+244,	yysvec+14,	yyvstop+363,
yycrank+209,	yysvec+14,	yyvstop+365,
yycrank+245,	yysvec+14,	yyvstop+367,
yycrank+0,	yysvec+14,	yyvstop+369,
yycrank+255,	yysvec+14,	yyvstop+372,
yycrank+263,	yysvec+14,	yyvstop+374,
yycrank+215,	yysvec+14,	yyvstop+376,
yycrank+231,	yysvec+14,	yyvstop+378,
yycrank+232,	yysvec+14,	yyvstop+380,
yycrank+264,	yysvec+14,	yyvstop+382,
yycrank+242,	yysvec+14,	yyvstop+384,
yycrank+240,	yysvec+14,	yyvstop+386,
yycrank+249,	yysvec+14,	yyvstop+388,
yycrank+275,	yysvec+14,	yyvstop+390,
yycrank+0,	yysvec+14,	yyvstop+392,
yycrank+278,	yysvec+14,	yyvstop+395,
yycrank+242,	yysvec+14,	yyvstop+397,
yycrank+272,	yysvec+14,	yyvstop+399,
yycrank+239,	yysvec+14,	yyvstop+401,
yycrank+347,	yysvec+14,	yyvstop+403,
yycrank+273,	yysvec+14,	yyvstop+405,
yycrank+297,	yysvec+14,	yyvstop+407,
yycrank+269,	yysvec+14,	yyvstop+409,
yycrank+261,	yysvec+14,	yyvstop+411,
yycrank+260,	yysvec+14,	yyvstop+413,
yycrank+269,	yysvec+14,	yyvstop+415,
yycrank+301,	yysvec+14,	yyvstop+417,
yycrank+271,	yysvec+14,	yyvstop+419,
yycrank+280,	yysvec+14,	yyvstop+421,
yycrank+262,	yysvec+14,	yyvstop+423,
yycrank+-1028,	0,		0,	
yycrank+315,	yysvec+14,	yyvstop+425,
yycrank+317,	yysvec+14,	yyvstop+427,
yycrank+276,	yysvec+14,	yyvstop+429,
yycrank+288,	yysvec+14,	yyvstop+431,
yycrank+300,	yysvec+14,	yyvstop+433,
yycrank+324,	yysvec+14,	yyvstop+435,
yycrank+301,	yysvec+14,	yyvstop+437,
yycrank+0,	yysvec+14,	yyvstop+439,
yycrank+-281,	yysvec+211,	yyvstop+442,
yycrank+337,	yysvec+14,	yyvstop+444,
yycrank+323,	yysvec+14,	yyvstop+446,
yycrank+297,	yysvec+14,	yyvstop+448,
yycrank+334,	yysvec+14,	yyvstop+450,
yycrank+320,	yysvec+14,	yyvstop+452,
yycrank+353,	yysvec+14,	yyvstop+454,
yycrank+0,	yysvec+14,	yyvstop+456,
yycrank+379,	yysvec+14,	yyvstop+459,
yycrank+324,	yysvec+14,	yyvstop+461,
yycrank+409,	yysvec+14,	yyvstop+463,
yycrank+411,	yysvec+14,	yyvstop+465,
yycrank+0,	0,		yyvstop+467,
yycrank+412,	yysvec+14,	yyvstop+469,
yycrank+0,	0,		yyvstop+471,
0,	0,	0};
struct yywork *yytop = yycrank+1151;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
  0,   1,   1,   1,   1,   1,   1,   1, 
  1,   9,  10,   1,  12,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  9,   1,  34,   1,   1,   1,   1,   1, 
  1,   1,   1,  43,   1,  45,   1,   1, 
 48,  49,  49,  49,  49,  49,  49,  49, 
 56,  56,   1,   1,   1,   1,   1,   1, 
  1,  65,  65,  65,  68,  69,  70,  71, 
 71,  71,  71,  71,  76,  71,  71,  71, 
 71,  71,  71,  83,  71,  71,  71,  71, 
 88,  71,  71,   1,  92,   1,   1,  71, 
  1,  65,  65,  65,  68,  69,  70,  71, 
 71,  71,  71,  71,  76,  71,  71,  71, 
 71,  71,  71,  83,  71,  71,  71,  71, 
 88,  71,  71, 123,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#pragma ident	"@(#)ncform	6.7	93/06/07 SMI"

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
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
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
#ifndef __cplusplus
			*yylastch++ = yych = input();
#else
			*yylastch++ = yych = lex_input();
#endif
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
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
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
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
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
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
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
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
#ifndef __cplusplus
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
#else
		yyprevious = yytext[0] = lex_input();
		if (yyprevious>0)
			lex_output(yyprevious);
#endif
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
#ifndef __cplusplus
	return(input());
#else
	return(lex_input());
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
#ifndef __cplusplus
	output(c);
#else
	lex_output(c);
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
