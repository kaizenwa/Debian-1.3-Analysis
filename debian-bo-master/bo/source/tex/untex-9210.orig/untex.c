
/* untex.c: Copyright Michael Staats (michael@hal6000.uni-duisburg.de)
   This source can be freely distributed according to the GNU Public
   license, which is available via ftp at many anon ftp servers. 
   
   Warning: I give no warranty whatever. Please note, that this is
   one of the first versions of untex, there might be bugs. I haven't
   tested it completely.

   4.10.92: modified for conversation of "a -> ae etc.: 
	   Denis Endisch (denis@smoky.ikf.physik.uni-frankfurt.de)

*/


/* untex.c:     remove all (?) LaTeX commands from input,
                Options:
		-o remove options to commands
		-a remove arguments 
		-u replace all \"a (etc.) with ibm characters
		-g also replace all "a (etc.) with ibm characters (german.sty)
		-e remove environment names
		-m remove all math
		-  read from stdin (no ioctl call to determine piped input,
		                    this program is intended to be portable)
		
		-g implies -u, -a implies -o
*/


#include <stddef.h>
#include <stdio.h>
#include <ctype.h>


#define MAXCMDLEN 80
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
    
#define GET(c) if (((c)=getc(inf)) == EOF ) return    
int nomath 	= FALSE;
int noenv 	= FALSE;
int germ 	= FALSE;
int uml 	= FALSE;
int ascii   = FALSE;
int iso 	= FALSE;
int noarg 	= FALSE;
int noopt 	= FALSE;


void usage();
void untex(FILE *inf);
int umlaut(int c);
void uml2ascii(int c);
int parsecmd(FILE *inf);
int skipcomment(FILE *inf, int *c);


int main(int argc, char *argv[])
{
	int i;
	FILE *inf;

	if (argc <= 1) usage();

	i = 1;
	inf = NULL;
	while (i < argc && argv[i][0]=='-') switch(argv[i++][1]) {
	      case 'm':nomath = TRUE;
		break;
	      case 'a':noarg = TRUE;   /* no break here! */
	      case 'o':noopt = TRUE;
		break;
	      case 'e':noenv = TRUE;
		break;
	      case 'g':germ = TRUE;   /* no break here ! */
	      case 'u':uml = TRUE;
		iso = !strcmp(&argv[i-1][2], "iso");
		ascii = !strcmp(&argv[i-1][2], "ascii");
		break;
	      default:
		if (!strcmp(argv[i-1],"-")) inf = stdin;
		else usage();
		break;
	}
	
	if (i >= argc && inf == NULL) usage();

	if (inf == NULL) for (;i<argc;i++) {
		if ((inf = fopen(argv[i],"r"))==NULL) perror(argv[i]);
		else untex(inf);
		fclose(inf);
	} else untex(inf);
	
	exit(0);
}

void usage()
{
	fprintf(stderr,"usage:\n   untex [-o] [-a] [-m] [-e] [-u[iso,ascii]] [-g[iso,ascii]] file1 [file2]  ...\n");
	fprintf(stderr,"or:\n   untex [-o] [-a] [-m] [-e] [-u[iso,ascii]] [-g[iso,ascii]] -\n");
	fprintf(stderr,"Options:   - : read from stdin\n");
	fprintf(stderr,"          -m : remove all math code\n");
	fprintf(stderr,"          -o : remove options to commands\n");
	fprintf(stderr,"          -u : replace all \\\"a (etc.) with ibm characters\n");
	fprintf(stderr,"          -g : also replace all \"a (etc.) with ibm characters (german.sty)\n");
	fprintf(stderr,"       -uiso : replace all \\\"a (etc.) with iso characters\n");
	fprintf(stderr,"       -giso : also replace all \"a (etc.) with iso characters (german.sty)\n");
	fprintf(stderr,"     -uascii : replace all \\\"a (etc.) with ascii characters (ae, ss, ...)\n");
	fprintf(stderr,"     -gascii : also replace all \"a (etc.) with ascii characters (german.sty)\n");
	fprintf(stderr,"          -e : remove environment names\n");
	fprintf(stderr,"          -a : remove arguments of commands\n");
	fprintf(stderr,"          -g implies -u, -a implies -o\n");
	fprintf(stderr,"          output is written to stdout\n");

	exit(1);
}

void untex(FILE *inf)
{
	int c, c1;

	while ( (c=getc(inf)) != EOF ) {
		switch (c) {
		      case '\\':
			GET(c);
			switch (c) {
				
				/* following code treats chars preceeded
				   by a backslash */
				
			      case '|':  /* print escaped special chars */
			      case '$':
			      case '&':
			      case '%':
			      case '#':
			      case '_':
			      case '{':
			      case '}':
				putchar(c);
				break;

			      case ' ': /* print space instead of these */
			      case '`':
			      case '\'':
			      case '+':
			      case '/':
			      case ':':
			      case ';':
			      case ',':
			      case '<':
			      case '>':
			      case '@':
				putchar(' ');
				break;
				
			      case '-': /* ignore these */
			      case '=':
			      case '!':			      
			      case '^':
			      case '~':
			      case '.':
				break;
				
			      case '(': /* if -m is set, ignore math */
			      case '[':
				c1 = (c == '(') ? ')' : ']';
				if (!nomath) putchar(c);
				while (nomath && c != c1) {
					while ( (c=getc(inf)) != EOF &&
					       skipcomment(inf, &c) &&
					       c != '\\' );
					GET(c);
				}
				break;
	
			      case '3':
				if ((germ)&&(!ascii)) 	
					putchar(umlaut('s'));
				else if ((germ)&&(ascii)) 
					uml2ascii('s');
				break;
				
			      
				
			      case '\"':
				GET(c);
				if ((uml)&&(!ascii))
					putchar(umlaut(c));
				else if ((uml)&&(ascii))
					uml2ascii(c);
				else putchar(c);
				break;
				
			      case '\\':putchar('\n');  
				GET(c);
				if (c == '[') 
				    while ((c=getc(inf)) != EOF  &&
					   skipcomment(inf, &c) &&
					   c != ']');
				else ungetc(c, inf);
				break;
				
			      default:
				ungetc(c, inf);
				if ((c=parsecmd(inf)) == EOF) return;
				if (c=='\n') putchar(c); else putchar(' ');
				break;
			}
			break; 
		      case '%':while ((c=getc(inf)) != EOF && c != '\n');
			break; 
			
		      case '{':
		      case '}':
		      case '~':
		      case '_':
			putchar(' ');
			break;
		      case '\"':
			if ((germ)&&(!ascii)) {
				GET(c);
				putchar(umlaut(c));
			} 
			else if ((germ)&&(ascii)) {
				GET(c);
				uml2ascii(c);
			}
			else 
				putchar(c);
			break;
			      
		      case '$':
			GET(c);
			if (c != '$') ungetc(c, inf);
			if (nomath) {
				while ((c=getc(inf)) != EOF && 
				       skipcomment(inf, &c) &&
				       c != '$');
				if (c == EOF) return;
				GET(c);
				if (c != '$') ungetc(c, inf);
			}
			break;
			
			
		      default:putchar(c);
			break;
		}
		if (c == EOF) return;
	}
	return;
}

								   
int skipcomment(FILE *inf, int *c)
{
	static int oldc = 0;
	
	while (oldc != '\\' && *c == '%') {
		while ((*c=getc(inf)) != EOF && *c != '\n');
		*c = getc(inf);
	}
	oldc = *c;
	return((*c != EOF));
}



int umlaut(int c)
{
	switch (c) {
		  case 'a':return(iso ? '\344' : 0x84);
		  case 'A':return(iso ? '\304' : 0x8E);
		  case 'o':return(iso ? '\366' : 0x94);
		  case 'O':return(iso ? '\326' : 0x99);
		  case 'u':return(iso ? '\374' : 0x81);
		  case 'U':return(iso ? '\334' : 0x9A);
		  case 's':return(iso ? '\337' : 0xE1);
	}
	return(c);
}

void uml2ascii(int c)
{
	putchar(c);
	switch (c) {
		  case 'a':
		  case 'A':
		  case 'o':
		  case 'O':
		  case 'u':
		  case 'U':
			putchar('e');
			break;
		  case 's':
			putchar('s');
			break;
	}
}



int parsecmd(FILE *inf)
{
	int c;
	char cmd[MAXCMDLEN], env[MAXCMDLEN], envtst[MAXCMDLEN];
	int cmdc, envc, i, open, close;
	int proceed;

	
	envc = cmdc = 0;
	c = getc(inf);
	
	while (c != EOF &&  (isalnum(c) || c == '@' || c == '*' )) {
		cmd[cmdc++] = c;
		if (cmdc >= MAXCMDLEN - 1) cmdc--;
		c = getc(inf);
	}
	cmd[cmdc] = 0;
	if (c == EOF) return(c);
	
	if (noopt && c == '[') {
		while ((c=getc(inf)) != EOF && c != ']');
		if ((c = getc(inf)) == EOF) return(c);
	}

	if (noarg && c == '{' && strcmp(cmd,"begin") && strcmp(cmd,"end")) {
		open = 1;
		close = 0;
		while ((c = getc(inf)) != EOF && skipcomment(inf, &c) &&
		       close != open) 
		    if (c =='{') open++; else if (c == '}') close++;
		ungetc(c, inf);
		return(c);
	}
	
	if (!strcmp(cmd,"begin") || !strcmp(cmd,"end")) {
		if ((c != '{' || c == EOF)) {
			ungetc(c, inf);
			return(c);
		}
	        while (c != EOF && c != '}') {
			c = getc(inf);
			env[envc++] = c;
			if (envc >= MAXCMDLEN - 1) envc--;
		}
		env[envc-1] = 0;
		if (!noenv) printf(env);
		envtst[0] = 0;
		if (nomath && !strcmp(cmd,"begin") &&
		    (!strcmp(env,"displaymath") ||
		     !strcmp(env,"equation") ||
		     !strcmp(env,"eqnarray") ||
		     !strcmp(env,"equation*") ||
		     !strcmp(env,"eqnarray*")
		     )
		    ) { 
			putchar('\n');
			while (strcmp(envtst, env)) {
				while ((c=getc(inf)) != EOF &&
				       skipcomment(inf, &c) &&
				       c != '\\');
				if (getc(inf) == 'e' &&
				    getc(inf) == 'n' &&
				    getc(inf) == 'd' &&
				    getc(inf) == '{') {
					i = 0;
					while ((c=getc(inf)) != EOF &&
					       c != '}') {
						envtst[i++] = c;
						if (i >= MAXCMDLEN - 1) i--;
					}
					envtst[i] = 0;
				}
			}
			if (!noenv) printf(envtst);
		}
	}
	ungetc(c, inf);
	return(c);
}

