/*
 * main.c		
 *
 * Anthony's Editor February 96
 *
 * Copyright 1993, 1996 by Anthony Howe.  All rights reserved.  No warranty.
 */

#include <ctype.h>
#include <errno.h>
#include "header.h"

static int intsig = FALSE;

#ifdef SIGINT
static void
sigint(sig)
int sig;
{
	intsig = TRUE;
}
#endif

#ifdef ATARI_ST
#include <setjmp.h>

static jmp_buf ignore;
static void (*old_sigint) _((int));

static void
sigint(sig)
int sig;
{
	intsig = TRUE;
	longjmp(ignore, 1);
}
#endif /* ATARI_ST */

#ifndef KEY_LENGTH
#define KEY_LENGTH	10
#endif

#ifdef TERMCAP
extern char *tgetstr _((char *, char **));
char termcap[1024];
#endif

int
main(argc, argv)
int argc;
char **argv;
{
	int i;
	t_keymap *kp;
	char *ap, *config;

	/* Find basename. */
	prog_name = *argv; 
	i = strlen(prog_name);
	while (0 <= i && prog_name[i] != '\\' && prog_name[i] != '/')
		--i;
	prog_name += i+1;

	/* Parse options. */
	config = CONFIG;
	for (--argc, ++argv; 0 < argc && **argv == '-'; --argc, ++argv) {
		ap = &argv[0][1];
		if (*ap == '-' && ap[1] == '\0') {
			/* -- terminates options. */
			--argc;
			++argv;
			break;
		}
		while (*ap != '\0') {
			switch (*ap++) {
			case 'f':
				/* -f <config_file>  or -f<config_file> */

				if (*ap != '\0') {
					config = ap;
				} else if (1 < argc) {
					config = *++argv;
					--argc;
				} else {
					fatal(f_usage);
				}
				break;
			default:
				fatal(f_usage);
			}
			break;
		}
	}

	if (initscr() == NULL)
		fatal(f_initscr);

#ifdef TERMCAP
	(void) tgetent(termcap, getenv("TERM"));
#endif

	if ((key_map = initkey(config)) == (t_keymap *) 0)
		fatal(f_config, 0L);

	/* Determine if editor is modeless or not.
	 * Define insert mode keys from the master table. 
	 */
	for (modeless = TRUE, kp = key_map; kp->code != K_ERROR; ++kp) {
		switch (kp->code) {
		case K_INSERT_ENTER:
			modeless = FALSE;
			break;
		case K_INSERT_EXIT:
			kp->code = K_DISABLED;
			key_mode[0].lhs = kp->lhs;
			break;
		case K_STTY_ERASE:
			key_mode[1].lhs = kp->lhs;
			break;
		case K_LITERAL:
			key_mode[2].lhs = kp->lhs;
			break;
		case K_HELP_OFF:
			textline = 0;
			break;
		}
	}

	noecho();
	lineinput(FALSE);
	idlok(stdscr, TRUE);

	if (0 < argc) {
		(void) load(*argv);
		/* Save filename irregardless of load() success. */
		strcpy(filename, *argv);
		modified = FALSE;
	}
	if (!growgap(CHUNK))
		fatal(f_alloc);

	top();
	i = msgflag;
	help();
	msgflag = i;

	/* Disable recognition of user interrupt. */
#ifdef SIGQUIT
	if (signal(SIGQUIT, SIG_IGN) == SIG_ERR)
		fatal(f_error);
#endif
#ifdef SIGINT
	if (signal(SIGINT, SIG_IGN) == SIG_ERR)
		fatal(f_error);
#endif
#ifdef ATARI_ST
	old_sigint = (void (*)()) Setexc(0x102, sigint);
	if (setjmp(ignore) != 0)
		msg(m_interrupt);
#endif
	while (!done) {
		i = 0;
		input = getkey(key_map);
		while (table[i].key != 0 && input != table[i].key)
			++i;
		if (table[i].func != NULL) 
			(*table[i].func)();
		else if (modeless)
			insert();
		display(table[i].disp);
	}

#ifdef SIGQUIT
	(void) signal(SIGQUIT, SIG_DFL);
#endif
#ifdef SIGINT
	(void) signal(SIGINT, SIG_DFL);
#else
#ifdef ATARI_ST
	(void) Setexc(0x102, old_sigint);
#endif
#ifdef __MSDOS__
#endif
#endif /* SIGINT */	

	if (scrap != NULL)
		free(scrap);
	finikey(key_map);
	move(LINES-1, 0);
	refresh();
	endwin();
	putchar('\n');
	return (0);
}

#ifdef TERMIOS
#include <termios.h>

/*
 *	Set the desired input mode.
 *
 *	FALSE enables immediate character processing (disable line processing
 *	and signals for INTR, QUIT, and SUSP).  TRUE enables line processing 
 *	and signals (disables immediate character processing).  In either 
 *	case flow control (XON/XOFF) is still active.  
 *
 *	If the termios function calls fail, then fall back on using 
 *	CURSES' cbreak()/nocbreak() functions; however signals will be
 *	still be in effect.
 */
void
lineinput(bf)
int bf;
{
	int error;
	struct termios term;
	error = tcgetattr(fileno(stdin), &term) < 0;
	if (!error) {
		if (bf)
			term.c_lflag |= ISIG | ICANON;
		else
			term.c_lflag &= ~(ISIG | ICANON);
		error = tcsetattr(fileno(stdin), TCSANOW, &term) < 0;
	}
	/* Fall back on CURSES functions that do almost what we need if
	 * either tcgetattr() or tcsetattr() fail.
	 */
	if (error) {
		if (bf)
			nocbreak();
		else
			cbreak();
	}
}
#endif /* TERMIOS */

#ifdef va_dcl
void
fatal(va_alist)
va_dcl
{
	char *f, *m;
	va_list args;

	if (curscr != NULL) {
		move(LINES-1, 0);
		refresh();
		endwin();
		putchar('\n');
	}

	va_start(args);
	f = m = getmsg(va_arg(args, t_msg));

	if (m == f_usage) {
		(void) fprintf(stderr, getmsg(m), prog_name);
		exit(2);
	}
	
	(void) fprintf(stderr, "%s: ", prog_name);

	while (*f != '\0') {
		if (*f == '%') {
			switch (*++f) {
			case 's':
				(void) fputs(va_arg(args, char *), stderr);
				break;
			case 'l':
				switch (*++f) {
				case 'd':
					(void) fprintf(
						stderr, "%ld",
						va_arg(args, long)
					);
					break;
				case 'u':
					(void) fprintf(
						stderr, "%lu",
						va_arg(args, unsigned long)
					);
					break;
				default:
					/* Unsupported. */
					(void) fprintf(stderr, "%%l%c", *f);
				}
				break;
			case 'd':
				(void) fprintf(
					stderr, "%d", va_arg(args, int)
				);
				break;
			case 'u':
				(void) fprintf(
					stderr, "%u", va_arg(args, unsigned)
				);
				break;
			case '%':
				(void) fputc('%', stderr);
				break;
			default:
				/* Unsupported. */
				(void) fprintf(stderr, "%%%c", *f);
			}
			++f;
		} else {
			(void) fputc(*f++, stderr);
		}
	}
	va_end(args);

	if (m == f_ok)
		exit(0);
	if (m == f_error)
		exit(1);
	exit(3);
}
#else /* not va_dcl */
#ifdef __STDC__
void
fatal(t_msg m, ...)
#else
void
fatal(m)
t_msg m;
#endif /* __STDC__ */
{
	va_list args;

	if (curscr != NULL) {
		move(LINES-1, 0);
		refresh();
		endwin();
		putchar('\n');
	}

	if (m == f_usage)
		(void) fprintf(stderr, getmsg(m), prog_name);
	else {
		va_start(args, m);
		(void) fprintf(stderr, "%s: ", prog_name);
		(void) vfprintf(stderr, getmsg(m), args);
		va_end(args);
	}

	if (m == f_ok)
		exit(0);
	if (m == f_error)
		exit(1);
	if (m == f_usage)
		exit(2);
	exit(3);
}
#endif /* va_dcl */

#ifdef va_dcl
void
msg(va_alist)
va_dcl
{
	long num;
	char *f, *m;
	va_list args;

	va_start(args);
	f = getmsg(va_arg(args, t_msg));
	for (m = msgline; *f != '\0'; ) { 
		if (*f == '%') {
			switch (*++f) {
			case 's':
				(void) strcpy(m, va_arg(args, char *));
				break;
			case 'l':
				if (*++f != 'd') {
					(void) strcpy(m, "UNSUPPORTED");
					break;
				}
				num = va_arg(args, long);
				/* fall */
			case 'd':
				if (f[-1] == '%')
					num = (long) va_arg(args, int);
				sprintf(m, "%ld", num);
				break;
			}
			m += strlen(m);
			++f;
		} else {
			*m++ = *f++;
		}
	}
	*m = '\0';
	va_end(args);
	msgflag = TRUE;
}
#else /* not va_dcl */
#ifdef __STDC__
void
msg(t_msg m, ...)
#else
void
msg(m)
t_msg m;
#endif /* __STDC__ */
{
	va_list args;
	va_start(args, m);
	(void) vsprintf(msgline, getmsg(m), args);
	va_end(args);
	msgflag = TRUE;
}
#endif /* va_dcl */

/*
 * Return a pointer to a message.
 * Messages have the format "number:text", where
 * number is an index into a message table and text
 * is the default text if no message defined.
 */
char *
getmsg(m)
t_msg m;
{
	char *text;
	long index;

	index = strtol(m, &text, 0);
	if (0 <= index && message[index] != NULL)
		return (message[index]);
	return (text+1);
}
 
/*
 *	Convert a string to lower case.  Return the string pointer.
 */
char *
strlwr(str)
char *str;
{
	register char *s;
	for (s = str; *s != '\0'; ++s)
		if (isupper(*s))
			*s = tolower(*s);
	return (str);
}

/*
 *	Make a duplicate of a string.  Return a pointer to an allocated
 *	copy of the string, or NULL if malloc() failed.
 */
char *
strdup(str)
const char *str;
{
	char *new;
	if ((new = (char*) malloc(strlen(str)+1)) != NULL)
		(void) strcpy(new, str);
	return (new);
}	

/*
 * Replace old with new characters.  If method
 *	negative	Nth occurence from the end;
 *	0		all occurences;
 *	positive	Nth occurence from the beginning.
 */
char *
strrep(str, old, new, method)
char *str;
int old, new, method;
{
	register char *ptr = str;
	register int direction = 1;

	if (method == 0) {
		/* All occurences. */
		for (; *ptr != '\0'; ++ptr)
			if (*ptr == old)
				*ptr = new;
		return (str);
	} else if (method < 0) {
		/* Start from the end going backwards. */
		direction = -1;
		ptr = &str[strlen(str)] - 1;
		method = -method;
	}

	/* Change the Nth occurence. */
	for (; *ptr != '\0'; ptr += direction) {
		if (*ptr == old && --method <= 0) {
			*ptr = new;
			break;
		}
	}

	return (str);
}

/*
 *
 */
char *
pathname(path, file)
char *path, *file;
{
	char *buf;
	size_t plen, flen;

	plen = path == NULL ? 0 : strlen(path);
	flen = file == NULL ? 0 : strlen(file);
	buf = (char*) malloc(plen + flen + 2);
	if (buf == NULL)
		return (NULL);
	(void) strcpy(buf, path);
	buf[plen] = '/';
	(void) strcpy(&buf[plen+1], file);
	return (buf);
}

/*
 * Open resource file.
 * Search order is: abs, ./rel, $HOME/rel, $ETCDIR/rel
 */
FILE *
openrc(fn)
char *fn;
{
	FILE *fp;
	char *ptr, *buf;

	if ((fp = fopen(fn, "r")) != NULL)
		return (fp);

	if ((ptr = getenv("HOME")) != NULL 
	&& (buf = pathname(ptr, fn)) != NULL) {
		fp = fopen(buf, "r");
#ifdef EITHER_SLASH
		if (fp == NULL)
			fp = fopen(strrep(buf, '/', '\\', 0), "r");
#endif /* EITHER_SLASH */
		free(buf);
		if (fp != NULL)
			return (fp);
	}

	if ((ptr = getenv("ETCDIR")) != NULL 
	&& (buf = pathname(ptr, fn)) != NULL) {
		fp = fopen(buf, "r");
#ifdef EITHER_SLASH
		if (fp == NULL)
			fp = fopen(strrep(buf, '/', '\\', 0), "r");
#endif /* EITHER_SLASH */
		free(buf);
		if (fp != NULL)
			return (fp);
	}

	return (NULL);
}

/*
 * Get an arbitrarily long line of text from a file.
 * The read is terminated when an unescaped newline is found.
 * The buffer that is passed back in ptr will be '\0' terminated.  
 * If an error occurs, the contents of ptr will be undefined.
 */
long
getblock(fp, ptr)
FILE *fp;
char **ptr;
{
	int ch, escape;
	char *buf, *new;
	size_t blen, len = 0;

	*ptr = NULL;
	if ((buf = (char *) malloc(blen = BUFSIZ)) == NULL)
		return (GETBLOCK_ALLOC);

	escape = FALSE;
	while ((ch = fgetc(fp)) != EOF) {
		buf[len++] = ch;

		if (ch == '\n') {
			if (escape) {
				len -= 2;
				escape = FALSE;
				continue;
			}
			buf[len] = '\0';
			break;
		}
		escape = !escape && ch == '\\'; 
			
		if (blen <= len) {
			blen += BUFSIZ;
			if ((new = (char*) realloc(buf, blen)) == NULL) {
				free(buf);
				return (GETBLOCK_ALLOC);
			}
			buf = new;
		}
	}

	if (ferror(fp)) {
		free(buf);
		return (GETBLOCK_ERROR);
	}
	if (feof(fp)) {
		free(buf);
		return (GETBLOCK_EOF);
	} 

	buf[len++] = '\0';

	/* Shrink buffer to exact size required for the string. */
	if ((new = (char *) realloc(buf, len)) == NULL)
		new = buf;
	*ptr = new;
	return (len);
}

/*
 * Return a pointer to an encoded dynamic string, else null.
 */
char *
encode(buf)
char *buf;
{
	int count;
	unsigned long number;
	char *new, *store, *fetch, *ctrl;
	static char escmap[] = "\033\033bfnrst";
	static char escvalue[] = "eE\b\f\n\r \t";
	static char control[] = "@abcdefghijklmnopqrstuvwxyz[\\]^_";

	/* Find end of string and count '$'. */
	for (count = 0, fetch = buf; *fetch != '\0'; ++fetch) 
		switch (*fetch) {
		case '\\':
			if (*++fetch == '\0')
				--fetch;
			break;
		case '$':
			++count;
			break;
		}

	/* Allocate new string buffer with room for expansion of
	 * $key_name strings.
	 */
	new = (char *) malloc((fetch - buf) + 1 + count * KEY_LENGTH);
	if (new == (char *) 0)
		goto error1;

	for (store = new, fetch = buf; *fetch != '\0'; ) {
		switch (*fetch) {
		case '$':
			if (*++fetch != '(')
				goto error2;
			++fetch;
#ifdef TERMCAP
			/* Termcap ids are two characters long. */
			if (fetch[2] != ')')
				goto error2;

			if (tgetstr(fetch, &store) == (char *) 0) 
				*store++ = '\a';
			
			fetch += 3;
#else
			/* Terminfo capnames are 2 to 5 characters long. */
			for (ctrl = fetch; *fetch != ')'; ++fetch)
				if (*fetch == '\0')
					goto error2;

			/* Modify the source buffer! */
			*fetch++ = '\0';

			if ((ctrl = tigetstr(ctrl)) == (char *) -1)
				goto error2;

			/* Is it a valid string, but undefined? */
			if (ctrl == (char *) 0)
				ctrl = "\a";

			(void) strcpy(store, ctrl);
			store += strlen(ctrl);
#endif
			break;
		case '^':
			++fetch;
			if (*fetch == '?') {
				/* ^? equals ASCII DEL character. */ 
				*store++ = 0x7f;
			} else {
				/* ASCII dependant control key mapping. */
				if (isupper(*fetch))
					*fetch = tolower(*fetch);
				if ((ctrl = strchr(control, *fetch)) == NULL)
					/* Not a control key mapping. */
					goto error2;
				*store++ = (char) (ctrl - control);	
			}
			++fetch;
			break;
		case '\\':
			/* Escapes. */
			++fetch;
			if (isdigit(*fetch)) {
				/* Numeric escapes allow for
				 *  octal	\0nnn
				 *  hex		\0xnn
				 *  decimal	\nnn 
				 */
				number = strtol(fetch, &fetch, 0);
				if (number < 0 || 255 < number)
					/* Number not in range 0..255. */
					goto error2;
				*store++ = (char) number;
				break;
			}
			if ((ctrl = strchr(escmap, *fetch)) != NULL) {
				*store++ = escvalue[ctrl - escmap];
				++fetch;
				break;
			}
			/* Literal escapes. */
		default:
			/* Character. */
			*store++ = *fetch++;
		}
	}
	*store++ = '\0';

	return new;
error2:
	free(new);
error1:
	return (char *) 0;
}

