#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include    "Compat.h"
#include    "LayoutP.h"
#include    "laygram.h"

static char *yysourcebase, *yysource;

#define input()		(*yysource++)
#define unput(c)	(--yysource)

static unsigned char is_word_sep[256] = {0,};
static unsigned int  word_sep_inited = False;

static int word_sep(unsigned char c)
{
    return c < 256 && is_word_sep[c];
}

static void init_word_sep(void)
{
    word_sep_inited = True;

    is_word_sep['\0'] = True;
    is_word_sep['\t'] = True;
    is_word_sep['\n'] = True;
    is_word_sep[' ']  = True;
    is_word_sep['$']  = True;
    is_word_sep['%']  = True;
    is_word_sep['(']  = True;
    is_word_sep[')']  = True;
    is_word_sep['*']  = True;
    is_word_sep['+']  = True;
    is_word_sep['-']  = True;
    is_word_sep['/']  = True;
    is_word_sep['<']  = True;
    is_word_sep['=']  = True;
    is_word_sep['>']  = True;
    is_word_sep['[']  = True;
    is_word_sep['\\'] = True;
    is_word_sep[']']  = True;
    is_word_sep['{']  = True;
    is_word_sep['}']  = True;
}

int yylex(void)
{
    char	buffer[256];
    char	c;
    char	*p;
    int		n;

    for (;;) { /* so that we can do continue to skip e.g. whitespace */
	c = input();

	switch (c) {
	case '\0':
	    return 0;
	case ' ':
	case '\t':
	case '\n':
	    continue;
	case '{':
	    return OC;
	case '}':
	    return CC;
	case '(':
	    return OP;
	case ')':
	    return CP;
	case '<':
	    return OA;
	case '>':
	    return CA;
	case '=':
	    return EQUAL;
	case '$':
	    return DOLLAR;
	case '+':	
	    yylval.oval = Plus;
	    return PLUS;
	case '-':
	    yylval.oval = Minus;
	    return MINUS;
	case '*':
	    yylval.oval = Times;
	    return TIMES;
	case '/':
	    yylval.oval = Divide;
	    return DIVIDE;
	case '%':
	    yylval.oval = Percent;
	    while (*yysource == ' ' ||
		   *yysource == '\t' ||
		   *yysource == '\n')
		yysource++;

	    if (yysource[0] == 'o' &&
		yysource[1] == 'f' &&
		word_sep(yysource[2])) {
		yysource += 2;
		return PERCENTOF;
	    }
	    return PERCENT;
	case '_':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	    break;
	case 'h':
	    if (strncmp(yysource, "orizontal", 9) == 0 &&
		word_sep(yysource[9])) {
		yysource += 9;
		return HORIZONTAL;
	    }
	    if (strncmp(yysource, "eight", 5) == 0 &&
		word_sep(yysource[5])) {
		yysource += 5;
		return HEIGHT;
	    }
	    break;
	case 'i':
	    if (yysource[0] != 'n' ||
		yysource[1] != 'f')
		break;
	    if (strncmp(yysource, "nfinity", 7) == 0 &&
		word_sep(yysource[7])) {
		yysource += 7;
		yylval.ival = 1;
		return INFINITY;
	    }
	    for (p = yysource + 2 ; *p == 'f' ; p++);
	    if (word_sep(*p)) {
		yylval.ival = p - yysource - 1;
		yysource = p;
		return INFINITY;
	    }
	    break;
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	    if (strncmp(yysource, "ertical", 7) == 0 &&
		word_sep(yysource[7])) {
		yysource += 7;
		return VERTICAL;
	    }
	    break;
	case 'w':
	    if (strncmp(yysource, "idth", 4) == 0 &&
		word_sep(yysource[4])) {
		yysource += 4;
		return WIDTH;
	    }
	    break;
	case 'x':
	case 'y':
	case 'z':
	    break;
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	    break;
	case '\\':
	    c = input();
	    break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	    yylval.ival = atoi(yysource - 1);
	    while (*yysource >= '0' && *yysource <= '9')
		yysource++;
	    return NUMBER;
	default:
	    fprintf(stderr, "ignoring %c\n", c);
	    continue;
	}

	buffer[0] = c;
	for (n = 1 ; n < sizeof(buffer) ; n++) {
	    c = input();
	    if ((c >= '0' && c <= '9') ||
		(c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		c == '_')
		buffer[n] = c;
	    else
		break;
	}
	if (n >= sizeof(buffer))
	    return 0;
	if (!word_sep(c))
	    return 0;
	unput(c);
	buffer[n] = '\0';

	yylval.qval = XrmStringToQuark(buffer);

	return NAME;
    }
}

int yysetsource(char *s)
{
    yysourcebase = yysource = s;

    if (!word_sep_inited)
	init_word_sep();

    return 0;
}

int yyerror(char *s)
{
    char    *t;
    
    fprintf (stderr, "%s\n", s);
    t = yysource - 50;
    if (t < yysourcebase)
	t = yysourcebase;
    while (*t && t < yysource + 50) {
	if (t == yysource)
	    fputc('@', stderr);
	fputc(*t++, stderr);
    }
    if (t == yysource)
	fputc('@', stderr);
    if (!*t)
	fputs("<EOF>", stderr);
    fputc('\n', stderr);

    return 0;
}
