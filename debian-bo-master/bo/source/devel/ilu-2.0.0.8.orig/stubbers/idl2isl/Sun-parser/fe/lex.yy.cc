# include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define output(c) (void)putc(c,yyout)
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
#endif
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO (void)fprintf(yyout, "%s",yytext)
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

# line 4 "idl.ll"
/*

COPYRIGHT

Copyright 1992, 1993, 1994 Sun Microsystems, Inc.  Printed in the United
States of America.  All Rights Reserved.

This product is protected by copyright and distributed under the following
license restricting its use.

The Interface Definition Language Compiler Front End (CFE) is made
available for your use provided that you include this license and copyright
notice on all media and documentation and the software program in which
this product is incorporated in whole or part. You may copy and extend
functionality (but may not remove functionality) of the Interface
Definition Language CFE without charge, but you are not authorized to
license or distribute it to anyone else except as part of a product or
program developed by you or with the express written consent of Sun
Microsystems, Inc. ("Sun").

The names of Sun Microsystems, Inc. and any of its subsidiaries or
affiliates may not be used in advertising or publicity pertaining to
distribution of Interface Definition Language CFE as permitted herein.

This license is effective until terminated by Sun for failure to comply
with this license.  Upon termination, you shall destroy or return all code
and documentation for the Interface Definition Language CFE.

INTERFACE DEFINITION LANGUAGE CFE IS PROVIDED AS IS WITH NO WARRANTIES OF
ANY KIND INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS
FOR A PARTICULAR PURPOSE, NONINFRINGEMENT, OR ARISING FROM A COURSE OF
DEALING, USAGE OR TRADE PRACTICE.

INTERFACE DEFINITION LANGUAGE CFE IS PROVIDED WITH NO SUPPORT AND WITHOUT
ANY OBLIGATION ON THE PART OF Sun OR ANY OF ITS SUBSIDIARIES OR AFFILIATES
TO ASSIST IN ITS USE, CORRECTION, MODIFICATION OR ENHANCEMENT.

SUN OR ANY OF ITS SUBSIDIARIES OR AFFILIATES SHALL HAVE NO LIABILITY WITH
RESPECT TO THE INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY
INTERFACE DEFINITION LANGUAGE CFE OR ANY PART THEREOF.

IN NO EVENT WILL SUN OR ANY OF ITS SUBSIDIARIES OR AFFILIATES BE LIABLE FOR
ANY LOST REVENUE OR PROFITS OR OTHER SPECIAL, INDIRECT AND CONSEQUENTIAL
DAMAGES, EVEN IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

Use, duplication, or disclosure by the government is subject to
restrictions as set forth in subparagraph (c)(1)(ii) of the Rights in
Technical Data and Computer Software clause at DFARS 252.227-7013 and FAR
52.227-19.

Sun, Sun Microsystems and the Sun logo are trademarks or registered
trademarks of Sun Microsystems, Inc.

SunSoft, Inc.  
2550 Garcia Avenue 
Mountain View, California  94043

NOTE:

SunOS, SunSoft, Sun, Solaris, Sun Microsystems or the Sun logo are
trademarks or registered trademarks of Sun Microsystems, Inc.

 */


# line 68 "idl.ll"
/*
 * idl.ll - Lexical scanner for IDL 1.1
 */

#include <idl.hh>
#include <idl_extern.hh>

#include <fe_private.hh>

#include <y.tab.hh>

#include <string.h>

static char	idl_escape_reader(char **);
static double	idl_atof(char *);
static unsigned long	idl_atoi(char *, long);
static void	idl_parse_line_and_file(char *);
static void	idl_store_pragma(char *);

// HPUX has yytext typed to unsigned char *. We make sure here that
// we'll always use char *
static char	*__yytext = (char *) yytext;

# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 94 "idl.ll"
	return MODULE;
break;
case 2:

# line 95 "idl.ll"
	return RAISES;
break;
case 3:

# line 96 "idl.ll"
return READONLY;
break;
case 4:

# line 97 "idl.ll"
return ATTRIBUTE;
break;
case 5:

# line 98 "idl.ll"
return EXCEPTION;
break;
case 6:

# line 99 "idl.ll"
	return CONTEXT;
break;
case 7:

# line 100 "idl.ll"
return INTERFACE;
break;
case 8:

# line 101 "idl.ll"
	return CONST;
break;
case 9:

# line 102 "idl.ll"
	return TYPEDEF;
break;
case 10:

# line 103 "idl.ll"
	return STRUCT;
break;
case 11:

# line 104 "idl.ll"
	return ENUM;
break;
case 12:

# line 105 "idl.ll"
	return STRING;
break;
case 13:

# line 106 "idl.ll"
return WSTRING;
break;
case 14:

# line 107 "idl.ll"
return SEQUENCE;
break;
case 15:

# line 108 "idl.ll"
	return UNION;
break;
case 16:

# line 109 "idl.ll"
	return SWITCH;
break;
case 17:

# line 110 "idl.ll"
	return CASE;
break;
case 18:

# line 111 "idl.ll"
	return DEFAULT;
break;
case 19:

# line 112 "idl.ll"
	return FLOAT;
break;
case 20:

# line 113 "idl.ll"
	return DOUBLE;
break;
case 21:

# line 114 "idl.ll"
	return LONG;
break;
case 22:

# line 115 "idl.ll"
	return SHORT;
break;
case 23:

# line 116 "idl.ll"
return UNSIGNED;
break;
case 24:

# line 117 "idl.ll"
	return CHAR;
break;
case 25:

# line 118 "idl.ll"
	return WCHAR;
break;
case 26:

# line 119 "idl.ll"
	return BOOLEAN;
break;
case 27:

# line 120 "idl.ll"
	return OCTET;
break;
case 28:

# line 121 "idl.ll"
	return VOID;
break;
case 29:

# line 123 "idl.ll"
	return TRUETOK;
break;
case 30:

# line 124 "idl.ll"
	return FALSETOK;
break;
case 31:

# line 126 "idl.ll"
	return INOUT;
break;
case 32:

# line 127 "idl.ll"
	return IN;
break;
case 33:

# line 128 "idl.ll"
	return OUT;
break;
case 34:

# line 129 "idl.ll"
	return ONEWAY;
break;
case 35:

# line 131 "idl.ll"
	return LEFT_SHIFT;
break;
case 36:

# line 132 "idl.ll"
	return RIGHT_SHIFT;
break;
case 37:

# line 133 "idl.ll"
	{
		  yylval.strval = "::";    
		  return SCOPE_DELIMITOR;
		}
break;
case 38:

# line 138 "idl.ll"
{
    char *z = (char *) malloc(strlen(__yytext) + 1);
    strcpy(z, __yytext);
    yylval.strval = z;
    return IDENTIFIER;
}
break;
case 39:

# line 145 "idl.ll"
     {
                  yylval.dval = idl_atof(__yytext);
                  return FLOATING_PT_LITERAL;
                }
break;
case 40:

# line 149 "idl.ll"
 {
                  yylval.dval = idl_atof(__yytext);
                  return FLOATING_PT_LITERAL;
                }
break;
case 41:

# line 154 "idl.ll"
{
		  yylval.ival = idl_atoi(__yytext, 10);
		  return INTEGER_LITERAL;
	        }
break;
case 42:

# line 158 "idl.ll"
{
		  yylval.ival = idl_atoi(__yytext, 16);
		  return INTEGER_LITERAL;
	        }
break;
case 43:

# line 162 "idl.ll"
{
		  yylval.ival = idl_atoi(__yytext, 8);
		  return INTEGER_LITERAL;
	      	}
break;
case 44:

# line 167 "idl.ll"
{
		  __yytext[strlen(__yytext)-1] = '\0';
		  yylval.sval = new String(__yytext + 1);
		  return STRING_LITERAL;
	      	}
break;
case 45:

# line 172 "idl.ll"
{
		  char *src = __yytext + 1;
		  char *dst = __yytext;
		  int ch;

		  while ((ch = *src++) != '"') {
		    if (ch == '\\')
		      ch = idl_escape_reader(&src);
		    *dst++ = ch;
		  }
		  *dst = 0;
		  yylval.sval = new String(__yytext);
		  return STRING_LITERAL;
	      	}
break;
case 46:

# line 186 "idl.ll"
	{
		  yylval.cval = __yytext[1];
		  return CHARACTER_LITERAL;
	      	}
break;
case 47:

# line 190 "idl.ll"
{
		  // octal character constant
		  char *cursor = __yytext + 2;
		  yylval.cval = idl_escape_reader(&cursor);
		  return CHARACTER_LITERAL;
		}
break;
case 48:

# line 196 "idl.ll"
{
		  char *cursor = __yytext + 2;
		  yylval.cval = idl_escape_reader(&cursor);
		  return CHARACTER_LITERAL;
		}
break;
case 49:

# line 201 "idl.ll"
{/* remember pragma */
  		  idl_global->set_lineno(idl_global->lineno() + 1);
		  idl_store_pragma(__yytext);
		}
break;
case 50:

# line 205 "idl.ll"
	{
		  idl_parse_line_and_file(__yytext);
		}
break;
case 51:

# line 208 "idl.ll"
		{
		  idl_parse_line_and_file(__yytext);
		}
break;
case 52:

# line 211 "idl.ll"
{
		  idl_parse_line_and_file(__yytext);
	        }
break;
case 53:

# line 214 "idl.ll"
{
		  /* ignore cpp ident */
  		  idl_global->set_lineno(idl_global->lineno() + 1);
		}
break;
case 54:

# line 218 "idl.ll"
{
		  /* ignore comments */
  		  idl_global->set_lineno(idl_global->lineno() + 1);
		}
break;
case 55:

# line 222 "idl.ll"
	{
		  for(;;) {
		    char c = yyinput();
		    if (c == '*') {
		      char next = yyinput();
		      if (next == '/')
			break;
		      else
			yyunput(c);
	              if (c == '\n') 
		        idl_global->set_lineno(idl_global->lineno() + 1);
		    }
	          }
	        }
break;
case 56:

# line 236 "idl.ll"
	;
break;
case 57:

# line 237 "idl.ll"
	{
  		  idl_global->set_lineno(idl_global->lineno() + 1);
		}
break;
case 58:

# line 240 "idl.ll"
	return __yytext[0];
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
	/* subroutines */

/*
 * Strip down a name to the last component, i.e. everything after the last
 * '/' character
 */
static char *
stripped_name(UTL_String *fn)
{
    char	*n = fn->get_string();
    long	l;

    if (n == NULL)
	return NULL;
    l = strlen(n);
    for (n += l; l > 0 && *n != '/'; l--, n--);
    if (*n == '/') n++;
    return n;
}

/*
 * Parse a #line statement generated by the C preprocessor
 */
static void
idl_parse_line_and_file(char *buf)
{
  char		*r = buf;
  char 		*h;
  UTL_String	*nm;

  /* Skip initial '#' */
  if (*r != '#') {
    return;
  }

  /* Find line number */
  for (r++; *r == ' ' || *r == '\t'; r++);
  h = r;
  for (; *r != '\n' && *r != ' ' && *r != '\t'; r++);
  *r++ = 0;
  idl_global->set_lineno(idl_atoi(h, 10));
  
  /* Find file name, if present */
  for (; *r != '"'; r++) {
    if (*r == '\n' || *r == '\0')
      return;
  }
  h = ++r;
  for (; *r != '"'; r++);
  *r = 0;
  if (*h == '\0')
    idl_global->set_filename(new String("standard input"));
  else
    idl_global->set_filename(new String(h));

  idl_global->set_in_main_file(
    (idl_global->filename()->compare(idl_global->real_filename())) ?
    I_TRUE :
    I_FALSE
  );
  /*
   * If it's an import file store the stripped name for the BE to use
   */
  if (!(idl_global->in_main_file()) && idl_global->import()) {
    nm = new UTL_String(stripped_name(idl_global->filename()));
    idl_global->store_include_file_name(nm);
  }
}
    
/*
 * Store a #pragma line into the list of pragmas
 */
static void
idl_store_pragma(char *buf)
{
  char *cp = buf + 1;
  while(*cp != 'p')
    cp++;
  while(*cp != ' ' && *cp != '\t')
    cp++;
  while(*cp == ' ' || *cp == '\t')
    cp++;
  char pragma[80];
  char *pp = pragma;
  while(*cp != '\n') {
    *pp++ = *cp++;
  }
  *pp = 0;
  if (strcmp(pragma, "import") == 0) {
    idl_global->set_import(I_TRUE);
    return;
  } 
  if (strcmp(pragma, "include") == 0) {
    idl_global->set_import(I_FALSE);
    return;
  }
  UTL_StrList *p = idl_global->pragmas();
  if (p == NULL)
    idl_global->set_pragmas(new UTL_StrList(new String(buf), NULL));
  else {
    p->nconc(new UTL_StrList(new String(buf), NULL));
    idl_global->set_pragmas(p);
  }
}

/*
 * idl_atoi - Convert a string of digits into an integer according to base b
 */
static unsigned long
idl_atoi(char *s, long b)
{
	unsigned long	r = 0;

	if (b == 8 && *s == '0')
	  s++;
	else if (b == 16 && *s == '0' && (*(s + 1) == 'x' || *(s + 1) == 'X'))
	  s += 2;

	for (; *s; s++)
	  if (*s <= '9' && *s >= '0')
	    r = (r * b) + (*s - '0');
	  else if (b > 10 && *s <= 'f' && *s >= 'a')
	    r = (r * b) + (*s - 'a' + 10);
	  else if (b > 10 && *s <= 'F' && *s >= 'A')
	    r = (r * b) + (*s - 'A' + 10);
	  else
	    break;

	return r;
}

/*
 * Convert a string to a float; atof doesn't seem to work, always.
 */
static double
idl_atof(char *s)
{
	char    *h = s;
	double	d = 0.0;
	double	f = 0.0;
	double	e, k;
	long	neg = 0, negexp = 0;

	if (*s == '-') {
	  neg = 1;
	  s++;
	}
	while (*s >= '0' && *s <= '9') {
		d = (d * 10) + *s - '0';
		s++;
	}
	if (*s == '.') {
		s++;
		e = 10;
		while (*s >= '0' && *s <= '9') {
			d += (*s - '0') / (e * 1.0);
			e *= 10;
			s++;
		}
	}
	if (*s == 'e' || *s == 'E') {
		s++;
		if (*s == '-') {
			negexp = 1;
			s++;
		} else if (*s == '+')
			s++;
		e = 0;
		while (*s >= '0' && *s <= '9') {
			e = (e * 10) + *s - '0';
			s++;
		}
		if (e > 0) {
			for (k = 1; e > 0; k *= 10, e--);
			if (negexp)
				d /= k;
			else
				d *= k;
		}
	}

	if (neg) d *= -1.0;

	return d;
}	

/*
 * Convert (some) escaped characters into their ascii values
 */
static char
idl_escape_reader(
    char **p_cursor
)
{
    int ch = *(*p_cursor)++;

    switch (ch) {
      case 'n':
	return '\n';
      case 't':
	return '\t';
      case 'v':
	return '\v';
      case 'b':
	return '\b';
      case 'r':
	return '\r';
      case 'f':
	return '\f';
      case 'a':
	return '\a';
      case '\\':
	return '\\';
      case '\?':
	return '?';
      case '\'':
	return '\'';
      case '"':
	return '"';
      case 'x':
	{
	    // hex value
	    int out = 0;
	    for (int i = 0; i < 2 && isxdigit(ch = **p_cursor); i++) {
		out = out * 16 + ch -
			(isdigit(ch) ? '0' : ch >= 'a' ? 'a' - 10 : 'A' - 10);
		*p_cursor += 1;
	    }
	    return out;
	}
	break;
      default:
	// check for octal value
	if (ch >= '0' && ch <= '7') {
	    int out = ch - '0';
	    for (int i = 0; i < 2 && (ch = **p_cursor) >= '0' && ch <= '7'; i++)
		out = out * 8 + ch - '0', *p_cursor += 1;
	    return out;
	} else {
	  return ch - 'a';
	}
	break;
    }
}
int yyvstop[] = {
0,

56,
0,

56,
0,

58,
0,

56,
58,
0,

57,
0,

58,
0,

58,
0,

58,
0,

58,
0,

43,
58,
0,

41,
58,
0,

58,
0,

58,
0,

58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

38,
58,
0,

58,
0,

56,
0,

44,
45,
0,

55,
0,

39,
0,

43,
0,

41,
0,

37,
0,

35,
0,

36,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

32,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

52,
0,

46,
0,

46,
0,

54,
0,

39,
0,

40,
0,

42,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

33,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

45,
0,

48,
0,

47,
48,
0,

39,
0,

40,
0,

38,
0,

29,
38,
0,

38,
0,

38,
0,

17,
38,
0,

24,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

11,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

21,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

28,
38,
0,

38,
0,

38,
0,

47,
0,

30,
38,
0,

38,
0,

38,
0,

8,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

19,
38,
0,

31,
38,
0,

38,
0,

38,
0,

27,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

22,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

15,
38,
0,

38,
0,

38,
0,

38,
0,

51,
0,

38,
0,

38,
0,

38,
0,

38,
0,

20,
38,
0,

38,
0,

38,
0,

1,
38,
0,

34,
38,
0,

2,
38,
0,

38,
0,

38,
0,

12,
38,
0,

10,
38,
0,

16,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

50,
0,

38,
0,

26,
38,
0,

6,
38,
0,

18,
38,
0,

38,
0,

38,
0,

38,
0,

38,
0,

9,
38,
0,

38,
0,

25,
38,
0,

38,
0,

53,
0,

38,
0,

38,
0,

38,
0,

3,
38,
0,

14,
38,
0,

23,
38,
0,

38,
0,

4,
38,
0,

5,
38,
0,

7,
38,
0,

13,
38,
0,

49,
0,
0};
# define YYTYPE int
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	4,35,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,6,	
4,35,	0,0,	0,0,	0,0,	
1,7,	0,0,	0,0,	0,0,	
1,3,	0,0,	1,8,	84,130,	
1,9,	1,10,	1,11,	39,89,	
90,135,	0,0,	0,0,	9,42,	
0,0,	1,11,	0,0,	1,12,	
9,43,	1,13,	0,0,	1,14,	
0,0,	0,0,	1,15,	0,0,	
0,0,	0,0,	1,15,	1,16,	
1,15,	12,49,	13,50,	14,51,	
0,0,	1,15,	16,53,	0,0,	
0,0,	0,0,	0,0,	100,142,	
0,0,	1,17,	0,0,	0,0,	
53,99,	1,15,	0,0,	0,0,	
131,130,	1,3,	17,54,	0,0,	
1,3,	54,100,	1,18,	1,19,	
1,20,	1,21,	1,22,	1,23,	
99,141,	6,36,	1,24,	0,0,	
0,0,	1,25,	1,26,	58,104,	
1,27,	6,36,	6,36,	1,28,	
1,29,	1,30,	1,31,	1,32,	
1,33,	2,34,	20,57,	19,56,	
21,60,	2,7,	23,64,	22,62,	
18,55,	20,58,	24,65,	2,8,	
25,66,	2,9,	21,61,	26,67,	
20,59,	22,63,	6,37,	27,68,	
29,73,	28,71,	30,77,	29,74,	
2,12,	28,72,	2,13,	6,36,	
2,14,	31,78,	27,69,	32,79,	
6,36,	6,36,	41,44,	29,75,	
33,80,	27,70,	29,76,	55,101,	
6,36,	56,102,	57,103,	59,105,	
60,106,	61,107,	62,108,	63,109,	
64,110,	6,36,	2,17,	65,111,	
33,81,	6,36,	6,36,	6,36,	
65,112,	41,46,	66,113,	67,114,	
6,36,	7,39,	68,115,	2,18,	
2,19,	2,20,	2,21,	2,22,	
2,23,	7,39,	7,0,	2,24,	
6,36,	69,116,	2,25,	2,26,	
6,38,	2,27,	70,117,	6,36,	
2,28,	2,29,	2,30,	2,31,	
2,32,	2,33,	71,118,	72,119,	
73,120,	41,46,	74,121,	75,122,	
76,123,	77,124,	7,39,	78,125,	
79,127,	80,128,	81,129,	86,132,	
87,133,	101,143,	102,144,	7,39,	
103,145,	78,126,	104,146,	106,149,	
7,39,	7,39,	88,88,	105,147,	
105,148,	107,150,	108,151,	97,140,	
7,39,	109,152,	110,153,	111,154,	
112,155,	97,140,	113,156,	114,157,	
115,158,	7,39,	116,159,	118,160,	
119,161,	7,39,	7,39,	7,39,	
120,162,	121,163,	88,134,	123,166,	
7,39,	8,41,	8,41,	8,41,	
8,41,	8,41,	8,41,	8,41,	
8,41,	8,41,	8,41,	97,140,	
7,39,	124,167,	125,168,	126,169,	
7,40,	97,140,	10,44,	7,39,	
10,45,	10,45,	10,45,	10,45,	
10,45,	10,45,	10,45,	10,45,	
10,41,	10,41,	45,45,	45,45,	
45,45,	45,45,	45,45,	45,45,	
45,45,	45,45,	45,41,	45,41,	
11,44,	10,46,	11,48,	11,48,	
11,48,	11,48,	11,48,	11,48,	
11,48,	11,48,	11,48,	11,48,	
127,170,	128,171,	129,172,	92,136,	
88,38,	132,174,	133,175,	141,178,	
10,47,	143,179,	122,164,	11,46,	
92,137,	92,137,	92,137,	92,137,	
92,137,	92,137,	92,137,	92,137,	
144,180,	10,46,	122,165,	147,181,	
148,182,	149,183,	150,184,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	152,185,	153,186,	154,187,	
10,47,	155,188,	157,189,	11,46,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	158,190,	159,191,	
160,192,	161,193,	15,52,	162,194,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	15,52,	15,52,	
15,52,	15,52,	34,82,	34,83,	
46,96,	163,195,	46,96,	164,196,	
165,197,	46,97,	46,97,	46,97,	
46,97,	46,97,	46,97,	46,97,	
46,97,	46,97,	46,97,	38,88,	
166,198,	167,199,	168,200,	169,201,	
94,138,	34,84,	94,138,	38,88,	
38,0,	94,139,	94,139,	94,139,	
94,139,	94,139,	94,139,	94,139,	
94,139,	94,139,	94,139,	171,202,	
172,203,	34,85,	34,85,	34,85,	
34,85,	34,85,	34,85,	34,85,	
34,85,	34,85,	34,85,	173,204,	
38,88,	139,95,	174,206,	175,207,	
177,176,	179,208,	180,209,	139,95,	
182,210,	38,88,	183,211,	184,212,	
185,213,	188,214,	38,88,	38,88,	
189,215,	40,90,	191,216,	192,217,	
193,218,	173,205,	38,88,	194,219,	
196,220,	40,90,	40,0,	197,221,	
198,222,	199,223,	201,224,	38,88,	
202,225,	139,95,	203,226,	38,88,	
38,88,	38,88,	206,228,	139,95,	
207,229,	208,230,	38,88,	209,231,	
210,232,	211,233,	34,86,	213,234,	
214,235,	218,236,	40,90,	219,237,	
223,238,	34,87,	38,88,	40,91,	
224,239,	225,240,	38,88,	40,90,	
226,241,	38,88,	229,243,	230,244,	
40,92,	40,92,	234,245,	43,43,	
235,246,	236,247,	237,248,	239,249,	
40,90,	241,250,	243,251,	43,43,	
43,93,	244,252,	245,253,	246,254,	
250,255,	40,90,	0,0,	0,0,	
0,0,	40,90,	40,90,	40,90,	
0,0,	0,0,	0,0,	0,0,	
40,90,	0,0,	0,0,	0,0,	
0,0,	243,251,	0,0,	0,0,	
43,43,	0,0,	0,0,	0,0,	
40,90,	0,0,	0,0,	0,0,	
40,90,	43,43,	0,0,	40,90,	
0,0,	0,0,	43,43,	43,43,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	43,43,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	43,43,	
0,0,	0,0,	0,0,	43,43,	
43,43,	43,43,	0,0,	0,0,	
0,0,	0,0,	43,43,	44,44,	
44,44,	44,44,	44,44,	44,44,	
44,44,	44,44,	44,44,	44,44,	
44,44,	0,0,	43,43,	0,0,	
0,0,	0,0,	43,43,	0,0,	
0,0,	43,43,	0,0,	0,0,	
44,94,	44,95,	85,83,	0,0,	
0,0,	0,0,	0,0,	44,95,	
47,98,	47,98,	47,98,	47,98,	
47,98,	47,98,	47,98,	47,98,	
47,98,	47,98,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
85,131,	47,98,	47,98,	47,98,	
47,98,	47,98,	47,98,	0,0,	
44,94,	44,95,	0,0,	0,0,	
0,0,	0,0,	0,0,	44,95,	
85,85,	85,85,	85,85,	85,85,	
85,85,	85,85,	85,85,	85,85,	
85,85,	85,85,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	47,98,	47,98,	47,98,	
47,98,	47,98,	47,98,	96,97,	
96,97,	96,97,	96,97,	96,97,	
96,97,	96,97,	96,97,	96,97,	
96,97,	130,130,	137,176,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	130,130,	130,130,	137,177,	
137,177,	137,177,	137,177,	137,177,	
137,177,	137,177,	137,177,	138,139,	
138,139,	138,139,	138,139,	138,139,	
138,139,	138,139,	138,139,	138,139,	
138,139,	0,0,	0,0,	0,0,	
0,0,	0,0,	130,173,	0,0,	
0,0,	0,0,	0,0,	205,227,	
0,0,	0,0,	0,0,	130,130,	
0,0,	0,0,	0,0,	0,0,	
130,130,	130,130,	0,0,	0,0,	
0,0,	0,0,	228,228,	0,0,	
130,130,	0,0,	0,0,	0,0,	
0,0,	0,0,	228,228,	228,242,	
251,251,	130,130,	0,0,	0,0,	
0,0,	130,130,	130,130,	130,130,	
251,251,	251,256,	0,0,	0,0,	
130,130,	205,205,	205,205,	205,205,	
205,205,	205,205,	205,205,	205,205,	
205,205,	205,205,	205,205,	228,228,	
130,130,	0,0,	0,0,	0,0,	
130,130,	0,0,	0,0,	130,130,	
228,228,	251,251,	0,0,	0,0,	
0,0,	228,228,	228,228,	0,0,	
0,0,	0,0,	251,251,	0,0,	
0,0,	228,228,	0,0,	251,251,	
251,251,	0,0,	0,0,	0,0,	
0,0,	0,0,	228,228,	251,251,	
0,0,	0,0,	228,228,	228,228,	
228,228,	0,0,	0,0,	0,0,	
251,251,	228,228,	0,0,	0,0,	
251,251,	251,251,	251,251,	0,0,	
0,0,	0,0,	0,0,	251,251,	
0,0,	228,228,	0,0,	0,0,	
0,0,	228,228,	0,0,	0,0,	
228,228,	0,0,	0,0,	251,251,	
0,0,	0,0,	0,0,	251,251,	
0,0,	0,0,	251,251,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		yyvstop+1,
yycrank+-86,	yysvec+1,	yyvstop+3,
yycrank+0,	0,		yyvstop+5,
yycrank+4,	0,		yyvstop+7,
yycrank+0,	0,		yyvstop+10,
yycrank+-104,	0,		yyvstop+12,
yycrank+-180,	0,		yyvstop+14,
yycrank+209,	0,		yyvstop+16,
yycrank+13,	0,		yyvstop+18,
yycrank+228,	0,		yyvstop+20,
yycrank+250,	0,		yyvstop+23,
yycrank+15,	0,		yyvstop+26,
yycrank+14,	0,		yyvstop+28,
yycrank+13,	0,		yyvstop+30,
yycrank+287,	0,		yyvstop+32,
yycrank+13,	yysvec+15,	yyvstop+35,
yycrank+12,	yysvec+15,	yyvstop+38,
yycrank+12,	yysvec+15,	yyvstop+41,
yycrank+12,	yysvec+15,	yyvstop+44,
yycrank+25,	yysvec+15,	yyvstop+47,
yycrank+23,	yysvec+15,	yyvstop+50,
yycrank+17,	yysvec+15,	yyvstop+53,
yycrank+18,	yysvec+15,	yyvstop+56,
yycrank+20,	yysvec+15,	yyvstop+59,
yycrank+21,	yysvec+15,	yyvstop+62,
yycrank+24,	yysvec+15,	yyvstop+65,
yycrank+40,	yysvec+15,	yyvstop+68,
yycrank+44,	yysvec+15,	yyvstop+71,
yycrank+39,	yysvec+15,	yyvstop+74,
yycrank+21,	yysvec+15,	yyvstop+77,
yycrank+39,	yysvec+15,	yyvstop+80,
yycrank+40,	yysvec+15,	yyvstop+83,
yycrank+57,	yysvec+15,	yyvstop+86,
yycrank+401,	0,		yyvstop+89,
yycrank+0,	yysvec+4,	yyvstop+91,
yycrank+0,	yysvec+6,	0,	
yycrank+0,	0,		yyvstop+93,
yycrank+-426,	0,		0,	
yycrank+12,	0,		0,	
yycrank+-476,	0,		0,	
yycrank+108,	yysvec+8,	0,	
yycrank+0,	0,		yyvstop+96,
yycrank+-526,	0,		0,	
yycrank+555,	0,		yyvstop+98,
yycrank+238,	yysvec+11,	yyvstop+100,
yycrank+369,	0,		0,	
yycrank+584,	0,		0,	
yycrank+0,	yysvec+11,	yyvstop+102,
yycrank+0,	0,		yyvstop+104,
yycrank+0,	0,		yyvstop+106,
yycrank+0,	0,		yyvstop+108,
yycrank+0,	yysvec+15,	yyvstop+110,
yycrank+12,	yysvec+15,	yyvstop+112,
yycrank+12,	yysvec+15,	yyvstop+114,
yycrank+43,	yysvec+15,	yyvstop+116,
yycrank+50,	yysvec+15,	yyvstop+118,
yycrank+47,	yysvec+15,	yyvstop+120,
yycrank+14,	yysvec+15,	yyvstop+122,
yycrank+53,	yysvec+15,	yyvstop+124,
yycrank+62,	yysvec+15,	yyvstop+126,
yycrank+48,	yysvec+15,	yyvstop+128,
yycrank+49,	yysvec+15,	yyvstop+130,
yycrank+68,	yysvec+15,	yyvstop+132,
yycrank+57,	yysvec+15,	yyvstop+134,
yycrank+60,	yysvec+15,	yyvstop+136,
yycrank+68,	yysvec+15,	yyvstop+139,
yycrank+79,	yysvec+15,	yyvstop+141,
yycrank+66,	yysvec+15,	yyvstop+143,
yycrank+92,	yysvec+15,	yyvstop+145,
yycrank+82,	yysvec+15,	yyvstop+147,
yycrank+101,	yysvec+15,	yyvstop+149,
yycrank+110,	yysvec+15,	yyvstop+151,
yycrank+95,	yysvec+15,	yyvstop+153,
yycrank+99,	yysvec+15,	yyvstop+155,
yycrank+97,	yysvec+15,	yyvstop+157,
yycrank+107,	yysvec+15,	yyvstop+159,
yycrank+101,	yysvec+15,	yyvstop+161,
yycrank+110,	yysvec+15,	yyvstop+163,
yycrank+111,	yysvec+15,	yyvstop+165,
yycrank+113,	yysvec+15,	yyvstop+167,
yycrank+102,	yysvec+15,	yyvstop+169,
yycrank+0,	yysvec+34,	0,	
yycrank+0,	0,		yyvstop+171,
yycrank+13,	yysvec+34,	0,	
yycrank+616,	0,		0,	
yycrank+119,	0,		0,	
yycrank+106,	0,		0,	
yycrank+-220,	yysvec+38,	0,	
yycrank+0,	0,		yyvstop+173,
yycrank+13,	0,		0,	
yycrank+0,	yysvec+90,	yyvstop+175,
yycrank+272,	0,		0,	
yycrank+0,	0,		yyvstop+177,
yycrank+389,	0,		0,	
yycrank+0,	0,		yyvstop+179,
yycrank+639,	0,		0,	
yycrank+165,	yysvec+96,	yyvstop+181,
yycrank+0,	yysvec+47,	yyvstop+183,
yycrank+21,	yysvec+15,	yyvstop+185,
yycrank+14,	yysvec+15,	yyvstop+187,
yycrank+107,	yysvec+15,	yyvstop+189,
yycrank+114,	yysvec+15,	yyvstop+191,
yycrank+123,	yysvec+15,	yyvstop+193,
yycrank+112,	yysvec+15,	yyvstop+195,
yycrank+116,	yysvec+15,	yyvstop+197,
yycrank+130,	yysvec+15,	yyvstop+199,
yycrank+135,	yysvec+15,	yyvstop+201,
yycrank+125,	yysvec+15,	yyvstop+203,
yycrank+136,	yysvec+15,	yyvstop+205,
yycrank+141,	yysvec+15,	yyvstop+207,
yycrank+122,	yysvec+15,	yyvstop+209,
yycrank+139,	yysvec+15,	yyvstop+211,
yycrank+139,	yysvec+15,	yyvstop+213,
yycrank+126,	yysvec+15,	yyvstop+215,
yycrank+143,	yysvec+15,	yyvstop+217,
yycrank+127,	yysvec+15,	yyvstop+219,
yycrank+0,	yysvec+15,	yyvstop+221,
yycrank+132,	yysvec+15,	yyvstop+224,
yycrank+148,	yysvec+15,	yyvstop+226,
yycrank+135,	yysvec+15,	yyvstop+228,
yycrank+139,	yysvec+15,	yyvstop+230,
yycrank+213,	yysvec+15,	yyvstop+232,
yycrank+139,	yysvec+15,	yyvstop+234,
yycrank+168,	yysvec+15,	yyvstop+236,
yycrank+159,	yysvec+15,	yyvstop+238,
yycrank+166,	yysvec+15,	yyvstop+240,
yycrank+208,	yysvec+15,	yyvstop+242,
yycrank+212,	yysvec+15,	yyvstop+244,
yycrank+196,	yysvec+15,	yyvstop+246,
yycrank+-696,	0,		0,	
yycrank+58,	0,		0,	
yycrank+212,	0,		0,	
yycrank+217,	0,		0,	
yycrank+0,	0,		yyvstop+248,
yycrank+0,	0,		yyvstop+250,
yycrank+0,	0,		yyvstop+252,
yycrank+659,	0,		0,	
yycrank+667,	0,		0,	
yycrank+391,	yysvec+138,	yyvstop+255,
yycrank+0,	0,		yyvstop+257,
yycrank+246,	yysvec+15,	yyvstop+259,
yycrank+0,	yysvec+15,	yyvstop+261,
yycrank+212,	yysvec+15,	yyvstop+264,
yycrank+227,	yysvec+15,	yyvstop+266,
yycrank+0,	yysvec+15,	yyvstop+268,
yycrank+0,	yysvec+15,	yyvstop+271,
yycrank+215,	yysvec+15,	yyvstop+274,
yycrank+231,	yysvec+15,	yyvstop+276,
yycrank+216,	yysvec+15,	yyvstop+278,
yycrank+226,	yysvec+15,	yyvstop+280,
yycrank+0,	yysvec+15,	yyvstop+282,
yycrank+233,	yysvec+15,	yyvstop+285,
yycrank+230,	yysvec+15,	yyvstop+287,
yycrank+231,	yysvec+15,	yyvstop+289,
yycrank+235,	yysvec+15,	yyvstop+291,
yycrank+0,	yysvec+15,	yyvstop+293,
yycrank+242,	yysvec+15,	yyvstop+296,
yycrank+262,	yysvec+15,	yyvstop+298,
yycrank+282,	yysvec+15,	yyvstop+300,
yycrank+279,	yysvec+15,	yyvstop+302,
yycrank+270,	yysvec+15,	yyvstop+304,
yycrank+282,	yysvec+15,	yyvstop+306,
yycrank+297,	yysvec+15,	yyvstop+308,
yycrank+305,	yysvec+15,	yyvstop+310,
yycrank+317,	yysvec+15,	yyvstop+312,
yycrank+329,	yysvec+15,	yyvstop+314,
yycrank+329,	yysvec+15,	yyvstop+316,
yycrank+320,	yysvec+15,	yyvstop+318,
yycrank+328,	yysvec+15,	yyvstop+320,
yycrank+0,	yysvec+15,	yyvstop+322,
yycrank+333,	yysvec+15,	yyvstop+325,
yycrank+343,	yysvec+15,	yyvstop+327,
yycrank+449,	0,		0,	
yycrank+352,	0,		0,	
yycrank+360,	0,		0,	
yycrank+0,	0,		yyvstop+329,
yycrank+425,	0,		0,	
yycrank+0,	yysvec+15,	yyvstop+331,
yycrank+367,	yysvec+15,	yyvstop+334,
yycrank+369,	yysvec+15,	yyvstop+336,
yycrank+0,	yysvec+15,	yyvstop+338,
yycrank+348,	yysvec+15,	yyvstop+341,
yycrank+362,	yysvec+15,	yyvstop+343,
yycrank+370,	yysvec+15,	yyvstop+345,
yycrank+356,	yysvec+15,	yyvstop+347,
yycrank+0,	yysvec+15,	yyvstop+349,
yycrank+0,	yysvec+15,	yyvstop+352,
yycrank+371,	yysvec+15,	yyvstop+355,
yycrank+375,	yysvec+15,	yyvstop+357,
yycrank+0,	yysvec+15,	yyvstop+359,
yycrank+357,	yysvec+15,	yyvstop+362,
yycrank+364,	yysvec+15,	yyvstop+364,
yycrank+370,	yysvec+15,	yyvstop+366,
yycrank+373,	yysvec+15,	yyvstop+368,
yycrank+0,	yysvec+15,	yyvstop+370,
yycrank+381,	yysvec+15,	yyvstop+373,
yycrank+371,	yysvec+15,	yyvstop+375,
yycrank+384,	yysvec+15,	yyvstop+377,
yycrank+388,	yysvec+15,	yyvstop+379,
yycrank+0,	yysvec+15,	yyvstop+381,
yycrank+380,	yysvec+15,	yyvstop+384,
yycrank+397,	yysvec+15,	yyvstop+386,
yycrank+384,	yysvec+15,	yyvstop+388,
yycrank+0,	0,		yyvstop+390,
yycrank+725,	0,		0,	
yycrank+382,	0,		0,	
yycrank+391,	0,		0,	
yycrank+384,	yysvec+15,	yyvstop+392,
yycrank+393,	yysvec+15,	yyvstop+394,
yycrank+388,	yysvec+15,	yyvstop+396,
yycrank+389,	yysvec+15,	yyvstop+398,
yycrank+0,	yysvec+15,	yyvstop+400,
yycrank+402,	yysvec+15,	yyvstop+403,
yycrank+411,	yysvec+15,	yyvstop+405,
yycrank+0,	yysvec+15,	yyvstop+407,
yycrank+0,	yysvec+15,	yyvstop+410,
yycrank+0,	yysvec+15,	yyvstop+413,
yycrank+401,	yysvec+15,	yyvstop+416,
yycrank+412,	yysvec+15,	yyvstop+418,
yycrank+0,	yysvec+15,	yyvstop+420,
yycrank+0,	yysvec+15,	yyvstop+423,
yycrank+0,	yysvec+15,	yyvstop+426,
yycrank+410,	yysvec+15,	yyvstop+429,
yycrank+415,	yysvec+15,	yyvstop+431,
yycrank+401,	yysvec+15,	yyvstop+433,
yycrank+417,	yysvec+15,	yyvstop+435,
yycrank+0,	0,		yyvstop+437,
yycrank+-749,	0,		0,	
yycrank+425,	0,		0,	
yycrank+407,	yysvec+15,	yyvstop+439,
yycrank+0,	yysvec+15,	yyvstop+441,
yycrank+0,	yysvec+15,	yyvstop+444,
yycrank+0,	yysvec+15,	yyvstop+447,
yycrank+415,	yysvec+15,	yyvstop+450,
yycrank+429,	yysvec+15,	yyvstop+452,
yycrank+408,	yysvec+15,	yyvstop+454,
yycrank+429,	yysvec+15,	yyvstop+456,
yycrank+0,	yysvec+15,	yyvstop+458,
yycrank+431,	yysvec+15,	yyvstop+461,
yycrank+0,	yysvec+15,	yyvstop+463,
yycrank+438,	yysvec+15,	yyvstop+466,
yycrank+0,	0,		yyvstop+468,
yycrank+525,	0,		0,	
yycrank+436,	yysvec+15,	yyvstop+470,
yycrank+428,	yysvec+15,	yyvstop+472,
yycrank+438,	yysvec+15,	yyvstop+474,
yycrank+0,	yysvec+15,	yyvstop+476,
yycrank+0,	yysvec+15,	yyvstop+479,
yycrank+0,	yysvec+15,	yyvstop+482,
yycrank+424,	yysvec+15,	yyvstop+485,
yycrank+-759,	0,		0,	
yycrank+0,	yysvec+15,	yyvstop+487,
yycrank+0,	yysvec+15,	yyvstop+490,
yycrank+0,	yysvec+15,	yyvstop+493,
yycrank+0,	yysvec+15,	yyvstop+496,
yycrank+0,	0,		yyvstop+499,
0,	0,	0};
struct yywork *yytop = yycrank+854;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,'"' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,
'8' ,'8' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'E' ,'F' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'L' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,01  ,0134,01  ,01  ,'_' ,
01  ,'A' ,'A' ,'A' ,'A' ,'E' ,'F' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'L' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
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

#ident "@(#)ncform 6.4 92/06/19 SMI"

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
			*yylastch++ = yych = input();
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
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
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
	return(input());
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
	output(c);
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
