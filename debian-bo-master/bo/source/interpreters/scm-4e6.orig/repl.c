/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

/* "repl.c" error, read-eval-print loop, read, write and load code.
   Author: Aubrey Jaffer */

#include "scm.h"
#include "setjump.h"
void	igc P((char *what, STACKITEM *stackbase));

#ifdef ARM_ULIB
# include <termio.h>
int set_erase()
{
   struct termio tin;

   ioctl(0, TCGETA, &tin);
   tin.c_cc[VERASE] = '\010';

   ioctl(0, TCSETA,&tin);
   return(0);
}
#endif

unsigned char upcase[CHAR_CODE_LIMIT];
unsigned char downcase[CHAR_CODE_LIMIT];
unsigned char lowers[] = "abcdefghijklmnopqrstuvwxyz";
unsigned char uppers[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
extern int verbose;
void init_tables()
{
  int i;
  for(i = 0;i<CHAR_CODE_LIMIT;i++) upcase[i] = downcase[i] = i;
  for(i = 0;i<sizeof lowers/sizeof(char);i++) {
    upcase[lowers[i]] = uppers[i];
    downcase[uppers[i]] = lowers[i];
  }
  verbose = 1;			/* Here so that monitor info won't be */
				/* printed while in init_storage. (BOOM) */
}

#ifdef EBCDIC
char *charnames[] = {
  "nul","soh","stx","etx", "pf", "ht", "lc","del",
   0   , 0   ,"smm", "vt", "ff", "cr", "so", "si",
  "dle","dc1","dc2","dc3","res", "nl", "bs", "il",
  "can", "em", "cc", 0   ,"ifs","igs","irs","ius",
   "ds","sos", "fs", 0   ,"byp", "lf","eob","pre",
   0   , 0   , "sm", 0   , 0   ,"enq","ack","bel",
   0   , 0   ,"syn", 0   , "pn", "rs", "uc","eot",
   0   , 0   , 0   , 0   ,"dc4","nak", 0   ,"sub",
  "space", s_newline, "tab", "backspace", "return", "page", "null"};
char charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
\040\041\042\043\044\045\046\047\
\050\051\052\053\054\055\056\057\
\060\061\062\063\064\065\066\067\
\070\071\072\073\074\075\076\077\
 \n\t\b\r\f\0";
#endif /* def EBCDIC */
#ifdef ASCII
char *charnames[] = {
  "nul","soh","stx","etx","eot","enq","ack","bel",
   "bs", "ht", "nl", "vt", "np", "cr", "so", "si",
  "dle","dc1","dc2","dc3","dc4","nak","syn","etb",
  "can", "em","sub","esc", "fs", "gs", "rs", "us",
  "space", s_newline, "tab", "backspace", "return", "page", "null", "del"};
char charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
 \n\t\b\r\f\0\177";
#endif /* def ASCII */
char *isymnames[] = {
				/* Special Forms */
				/*  NUM_ISPCSYMS ISPCSYMS here */
  "#@and", "#@begin", "#@case", "#@cond", "#@do", "#@if", "#@lambda",
  "#@let", "#@let*", "#@letrec", "#@or", "#@quote", "#@set!",
  "#@define", "#@apply", "#@call-with-current-continuation",
				/* user visible ISYMS */
				/* other keywords */
				/* Flags */
  "#f", "#t", "#<undefined>", "#<eof>", "()", "#<unspecified>"
  };

static char	s_read_char[] = "read-char", s_peek_char[] = "peek-char";
char	s_read[] = "read", s_write[] = "write", s_newline[] = "newline";
static char	s_display[] = "display", s_write_char[] = "write-char";

static char	s_eofin[] = "end of file in ";
static char	s_unknown_sharp[] = "unknown # object";

static SCM lreadr P((SCM tok_buf, SCM port));
static SCM lreadparen P((SCM tok_buf, SCM port, char *name));
static sizet read_token P((int ic, SCM tok_buf, SCM port));

void intprint(n, radix, port)
     long n;
     int radix;
     SCM port;
{
  char num_buf[INTBUFLEN];
  lfwrite(num_buf, (sizet)sizeof(char), iint2str(n, radix, num_buf), port);
}

void ipruk(hdr, ptr, port)
     char *hdr;
     SCM ptr;
     SCM port;
{
  lputs("#<unknown-", port);
  lputs(hdr, port);
  if CELLP(ptr) {
    lputs(" (0x", port);
    intprint(CAR(ptr), 16, port);
    lputs(" . 0x", port);
    intprint(CDR(ptr), 16, port);
    lputs(") @", port);
  }
  lputs(" 0x", port);
  intprint(ptr, 16, port);
  lputc('>', port);
}

void iprlist(hdr, exp, tlr, port, writing)
     char *hdr, tlr;
     SCM exp;
     SCM port;
     int writing;
{
  lputs(hdr, port);
  /* CHECK_INTS; */
  iprin1(CAR(exp), port, writing);
  exp = CDR(exp);
  for(;NIMP(exp);exp = CDR(exp)) {
    if NECONSP(exp) break;
    lputc(' ', port);
    /* CHECK_INTS; */
    iprin1(CAR(exp), port, writing);
  }
  if NNULLP(exp) {
    lputs(" . ", port);
    iprin1(exp, port, writing);
  }
  lputc(tlr, port);
}
void iprin1(exp, port, writing)
     SCM exp;
     SCM port;
int writing;
{
  register long i;
taloop:
  switch (7 & (int)exp) {
  case 2:
  case 6:
    intprint(INUM(exp), 10, port);
    break;
  case 4:
    if ICHRP(exp) {
      i = ICHR(exp);
      if (writing) lputs("#\\", port);
      if (!writing) lputc((int)i, port);
      else if ((i <= ' ') && charnames[i]) lputs(charnames[i], port);
#ifndef EBCDIC
      else if (i=='\177')
	lputs(charnames[(sizeof charnames/sizeof(char *))-1], port);
#endif /* ndef EBCDIC */
      else if (i > '\177')
	intprint(i, 8, port);
      else lputc((int)i, port);
    }
    else if (IFLAGP(exp) && (ISYMNUM(exp)<(sizeof isymnames/sizeof(char *))))
      lputs(ISYMCHARS(exp), port);
    else if ILOCP(exp) {
      lputs("#@", port);
      intprint((long)IFRAME(exp), 10, port);
      lputc(ICDRP(exp)?'-':'+', port);
      intprint((long)IDIST(exp), 10, port);
    }
    else goto idef;
    break;
  case 1:			/* gloc */
    lputs("#@", port);
    exp = CAR(exp-1);
    goto taloop;
  default:
  idef:
    ipruk("immediate", exp, port);
    break;
  case 0:
    switch TYP7(exp) {
    case tcs_cons_gloc:
    case tcs_cons_imcar:
    case tcs_cons_nimcar:
      iprlist("(", exp, ')', port, writing);
      break;
    case tcs_closures:
      exp = CODE(exp);
      iprlist("#<CLOSURE ", exp, '>', port, writing);
      break;
    case tc7_string:
      if (writing) {
	lputc('\"', port);
	for(i = 0;i<LENGTH(exp);++i) switch (CHARS(exp)[i]) {
	case '"':
	case '\\':
	  lputc('\\', port);
	default:
	  lputc(CHARS(exp)[i], port);
	}
	lputc('\"', port);
	break;
      }
    case tcs_symbols:
      lfwrite(CHARS(exp), (sizet)sizeof(char), (sizet)LENGTH(exp), port);
      break;
    case tc7_vector:
      lputs("#(", port);
      for(i = 0;i+1<LENGTH(exp);++i) {
	/* CHECK_INTS; */
	iprin1(VELTS(exp)[i], port, writing);
	lputc(' ', port);
      }
      if (i<LENGTH(exp)) {
	/* CHECK_INTS; */
	iprin1(VELTS(exp)[i], port, writing);
      }
      lputc(')', port);
      break;
    case tc7_bvect:
    case tc7_ivect:
    case tc7_uvect:
    case tc7_fvect:
    case tc7_dvect:
    case tc7_cvect:
      raprin1(exp, port, writing);
      break;
    case tcs_subrs:
      lputs("#<primitive-procedure ", port);
      lputs(CHARS(SNAME(exp)), port);
      lputc('>', port);
      break;
#ifdef CCLO
    case tc7_cclo:
      lputs("#<compiled-closure ", port);
      iprin1(CCLO_SUBR(exp), port, writing);
      lputc('>', port);
      break;
#endif
    case tc7_contin:
      lputs("#<continuation ", port);
      intprint(LENGTH(exp), 10, port);
      lputs(" @ ", port);
      intprint((long)CHARS(exp), 16, port);
      lputc('>', port);
      break;
    case tc7_port:
      i = PTOBNUM(exp);
      if (i<numptob && ptobs[i].print && (ptobs[i].print)(exp, port, writing))
	break;
      goto punk;
    case tc7_smob:
      i = SMOBNUM(exp);
      if (i<numsmob && smobs[i].print && (smobs[i].print)(exp, port, writing))
	break;
      goto punk;
    default: punk: ipruk("type", exp, port);
    }
  }
}

#ifdef __IBMC__
# define MSDOS
#endif
#ifdef MSDOS
# ifndef GO32
#  include <io.h>
#  include <conio.h>
static int input_waiting(f)
     FILE *f;
{
  if (feof(f)) return 1;
  if (fileno(f)==fileno(stdin) && (isatty(fileno(stdin)))) return kbhit();
  return -1;
}
# endif
#else
# ifdef _DCC
#  include <ioctl.h>
# else
#  ifndef AMIGA
#   ifndef vms
#    ifdef MWC
#     include <sys/io.h>
#    else
#     ifndef THINK_C
#      ifndef ARM_ULIB
#       include <sys/ioctl.h>
#      endif
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef HAVE_SELECT
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
# endif

static int input_waiting(f)
     FILE *f;
{
# ifdef HAVE_SELECT
  fd_set ifds;
  struct timeval tv;

  FD_ZERO(&ifds);
  FD_SET(fileno(f), &ifds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  select((fileno(f) + 1), &ifds, (fd_set *) NULL, (fd_set *) NULL, &tv);
  return FD_ISSET(fileno(f), &ifds);
# else
#  ifdef FIONREAD
  long remir;
  if (feof(f)) return 1;
  ioctl(fileno(f), FIONREAD, &remir);
  return remir;
#  else
  return -1;
#  endif
# endif
}
#endif
/* perhaps should undefine MSDOS from __IBMC__ here */
#ifndef GO32
static char s_char_readyp[]="char-ready?";
SCM char_readyp(port)
     SCM port;
{
  if UNBNDP(port) port = cur_inp;
  else ASSERT(NIMP(port) && OPINPORTP(port), port, ARG1, s_char_readyp);
  if (CRDYP(port) || !(BUF0 & CAR(port))) return BOOL_T;
  return input_waiting(STREAM(port)) ? BOOL_T : BOOL_F;
}
#endif

SCM eof_objectp(x)
     SCM x;
{
	return (EOF_VAL==x) ? BOOL_T : BOOL_F;
}

void lfflush(port)		/* internal SCM call */
     SCM port;
{
  sizet i = PTOBNUM(port);
  (ptobs[i].fflush)(STREAM(port));
}
static char	s_flush[] = "force-output";
SCM lflush(port)		/* user accessible as force-output */
     SCM port;
{
	if UNBNDP(port) port = cur_outp;
	else ASSERT(NIMP(port) && OPOUTPORTP(port), port, ARG1, s_flush);
	{
	  sizet i = PTOBNUM(port);
	  SYSCALL((ptobs[i].fflush)(STREAM(port)););
	  return UNSPECIFIED;
	}
}

SCM lwrite(obj, port)
     SCM obj, port;
{
	if UNBNDP(port) port = cur_outp;
	else ASSERT(NIMP(port) && OPOUTPORTP(port), port, ARG2, s_write);
	iprin1(obj, port, 1);
#ifdef HAVE_PIPE
# ifdef EPIPE
	if (EPIPE==errno) close_port(port);
# endif
#endif
	return UNSPECIFIED;
}
SCM display(obj, port)
     SCM obj, port;
{
	if UNBNDP(port) port = cur_outp;
	else ASSERT(NIMP(port) && OPOUTPORTP(port), port, ARG2, s_display);
	iprin1(obj, port, 0);
#ifdef HAVE_PIPE
# ifdef EPIPE
	if (EPIPE==errno) close_port(port);
# endif
#endif
	return UNSPECIFIED;
}
SCM newline(port)
     SCM port;
{
	if UNBNDP(port) port = cur_outp;
	else ASSERT(NIMP(port) && OPOUTPORTP(port), port, ARG1, s_newline);
	lputc('\n', port);
#ifdef HAVE_PIPE
# ifdef EPIPE
	if (EPIPE==errno) close_port(port);
	else
# endif
#endif
	  if (port==cur_outp) lfflush(port);
	return UNSPECIFIED;
}
SCM write_char(chr, port)
     SCM chr, port;
{
	if UNBNDP(port) port = cur_outp;
	else ASSERT(NIMP(port) && OPOUTPORTP(port), port, ARG2, s_write_char);
	ASSERT(ICHRP(chr), chr, ARG1, s_write_char);
	lputc((int)ICHR(chr), port);
#ifdef HAVE_PIPE
# ifdef EPIPE
	if (EPIPE==errno) close_port(port);
# endif
#endif
	return UNSPECIFIED;
}

FILE *trans = 0;
SCM trans_on(fil)
     SCM fil;
{
  transcript = open_file(fil, makfromstr("w", (sizet)sizeof(char)));
  if FALSEP(transcript) trans = 0;
  else trans = STREAM(transcript);
  return UNSPECIFIED;
}
SCM trans_off()
{
  if (!FALSEP(transcript)) close_port(transcript);
  transcript = BOOL_F;
  trans = 0;
  return UNSPECIFIED;
}

void lputc(c, port)
     int c;
     SCM port;
{
  sizet i = PTOBNUM(port);
  SYSCALL((ptobs[i].fputc)(c, STREAM(port)););
  if (trans && (port==def_outp || port==cur_errp))
    SYSCALL(fputc(c, trans););
}
void lputs(s, port)
     char *s;
     SCM port;
{
  sizet i = PTOBNUM(port);
  SYSCALL((ptobs[i].fputs)(s, STREAM(port)););
  if (trans && (port==def_outp || port==cur_errp))
    SYSCALL(fputs(s, trans););
}
int lfwrite(ptr, size, nitems, port)
     char *ptr;
     sizet size;
     sizet nitems;
     SCM port;
{
  int ret;
  sizet i = PTOBNUM(port);
  SYSCALL(ret = (ptobs[i].fwrite)
	  (ptr, size, nitems, STREAM(port)););
  if (trans && (port==def_outp || port==cur_errp))
    SYSCALL(fwrite(ptr, size, nitems, trans););
  return ret;
}

int lgetc(port)
  SCM port;
{
  FILE *f;
  int c;
  sizet i;
  /* One char may be stored in the high bits of (car port) orre@nada.kth.se. */
  if CRDYP(port)
    {
      c = CGETUN(port);
      CLRDY(port);		/* Clear ungetted char */
      return c;
    }
  f=STREAM(port);
  i = PTOBNUM(port);
#ifdef linux
  c = (ptobs[i].fgetc)(f);
#else
  SYSCALL(c = (ptobs[i].fgetc)(f););
#endif
  if (trans && (f==stdin)) SYSCALL(fputc(c, trans););
  return c;
}
void lungetc(c, port)
  int c;
  SCM port;
{
/*	ASSERT(!CRDYP(port), port, ARG2, "too many lungetc");*/
	CUNGET(c, port);
}

SCM scm_read_char(port)
     SCM port;
{
  int c;
  if UNBNDP(port) port = cur_inp;
  else ASSERT(NIMP(port) && OPINPORTP(port), port, ARG1, s_read_char);
  c = lgetc(port);
  if (EOF==c) return EOF_VAL;
  return MAKICHR(c);
}
SCM peek_char(port)
  SCM port;
{
	int c;
	if UNBNDP(port) port = cur_inp;
	else ASSERT(NIMP(port) && OPINPORTP(port), port, ARG1, s_peek_char);
	c = lgetc(port);
	if (EOF==c) return EOF_VAL;
	lungetc(c, port);
	return MAKICHR(c);
}

char *grow_tok_buf(tok_buf)
     SCM tok_buf;
{
  sizet len = LENGTH(tok_buf);
  len += len / 2;
  resizuve(tok_buf, (SCM)MAKINUM(len));
  return CHARS(tok_buf);
}

static int flush_ws(port, eoferr)
     SCM port;
char *eoferr;
{
	register int c;
	while(1) switch (c = lgetc(port)) {
	case EOF:
goteof:
		if (eoferr) wta(UNDEFINED, s_eofin, eoferr);
		return c;
	case ';':
lp:
		switch (c = lgetc(port)) {
		case EOF:
			goto goteof;
		default:
			goto lp;
		case LINE_INCREMENTORS:
			break;
		}
	case LINE_INCREMENTORS:
		if (port==loadport) linum++;
	case WHITE_SPACES:
		break;
	default:
		return c;
	}
}
SCM lread(port)
     SCM port;
{
	int c;
	SCM tok_buf;
	if UNBNDP(port) port = cur_inp;
	else ASSERT(NIMP(port) && OPINPORTP(port), port, ARG1, s_read);
	do {
	  c = flush_ws(port, (char *)NULL);
	  if (EOF==c) return EOF_VAL;
	  lungetc(c, port);
	  tok_buf = makstr(30L);
	} while (EOF_VAL==(tok_buf = lreadr(tok_buf, port)));
	return tok_buf;
}
static SCM lreadr(tok_buf, port)
     SCM tok_buf;
SCM port;
{
	int c;
	sizet j;
	SCM p;
tryagain:
	c = flush_ws(port, s_read);
	switch (c) {
/*	case EOF: return EOF_VAL;*/
#ifdef BRACKETS_AS_PARENS
	case '[':
#endif
	case '(': return lreadparen(tok_buf, port, s_list);
#ifdef BRACKETS_AS_PARENS
	case ']':
#endif
	case ')': warn("unexpected \")\"", "");
	  goto tryagain;
	case '\'': return cons2(i_quote, lreadr(tok_buf, port), EOL);
	case '`': return cons2(i_quasiquote, lreadr(tok_buf, port), EOL);
	case ',':
		c = lgetc(port);
		if ('@'==c) p = i_uq_splicing;
		else {
			lungetc(c, port);
			p = i_unquote;
		}
		return cons2(p, lreadr(tok_buf, port), EOL);
	case '#':
		c = lgetc(port);
		switch (c) {
#ifdef BRACKETS_AS_PARENS
		case '[':
#endif
		case '(':
			p = lreadparen(tok_buf, port, s_vector);
			return NULLP(p) ? nullvect : vector(p);
		case 't': case 'T': return BOOL_T;
		case 'f': case 'F': return BOOL_F;
		case 'b': case 'B': case 'o': case 'O':
		case 'd': case 'D': case 'x': case 'X':
		case 'i': case 'I': case 'e': case 'E':
			lungetc(c, port);
			c = '#';
			goto num;
		case '*':
			j = read_token(c, tok_buf, port);
			p = istr2bve(CHARS(tok_buf)+1, (long)(j-1));
			if (NFALSEP(p)) return p;
			else goto unkshrp;
		case '\\':
			c = lgetc(port);
			j = read_token(c, tok_buf, port);
			if (j==1) return MAKICHR(c);
			if (c >= '0' && c < '8') {
			  p = istr2int(CHARS(tok_buf), (long)j, 8);
			  if (NFALSEP(p)) return MAKICHR(INUM(p));
			}
			for (c = 0;c<sizeof charnames/sizeof(char *);c++)
			  if (charnames[c]
			      && (0==strcmp(charnames[c], CHARS(tok_buf))))
			    return MAKICHR(charnums[c]);
			wta(UNDEFINED, "unknown # object: #\\", CHARS(tok_buf));
		case '|':
			j = 1;	/* here j is the comment nesting depth */
lp:			c = lgetc(port);
lpc:			switch (c) {
			case EOF:
			  wta(UNDEFINED, s_eofin, "balanced comment");
			case LINE_INCREMENTORS:
			  if (port==loadport) linum++;
			default:
			  goto lp;
			case '|':
			  if ('#' != (c = lgetc(port))) goto lpc;
			  if (--j) goto lp;
			  break;
			case '#':
			  if ('|' != (c = lgetc(port))) goto lpc;
			  ++j; goto lp;
			}
			goto tryagain;
		case '.':
			p = lreadr(tok_buf, port);
			return EVAL(p, (SCM)EOL);
		default: callshrp:
			p = CDR(intern("read:sharp", (sizeof "read:sharp")-1));
			if NIMP(p) {
			  p = apply(p, MAKICHR(c), acons(port, EOL, EOL));
			  if (UNSPECIFIED==p) goto tryagain;
			  return p;
			}
		      unkshrp: wta((SCM)MAKICHR(c), s_unknown_sharp, "");
		}
	case '\"':
		j = 0;
		while ('\"' != (c = lgetc(port))) {
			ASSERT(EOF != c, UNDEFINED, s_eofin, s_string);
			if (j+1 >= LENGTH(tok_buf)) grow_tok_buf(tok_buf);
			if (c=='\\') switch (c = lgetc(port)) {
			case '\n': continue;
			case '0': c = '\0'; break;
			case 'f': c = '\f'; break;
			case 'n': c = '\n'; break;
			case 'r': c = '\r'; break;
			case 't': c = '\t'; break;
			case 'a': c = '\007'; break;
			case 'v': c = '\v'; break;
			}
			CHARS(tok_buf)[j] = c;
			++j;
		}
		if (j==0) return nullstr;
		CHARS(tok_buf)[j] = 0;
		return makfromstr(CHARS(tok_buf), j);
	case DIGITS:
	case '.': case '-': case '+':
num:
		j = read_token(c, tok_buf, port);
		p = istring2number(CHARS(tok_buf), (long)j, 10L);
		if NFALSEP(p) return p;
	        if (c=='#') {
		  if ((j==2) && (lgetc(port)=='(')) {
		    lungetc('(', port);
		    c = CHARS(tok_buf)[1];
		    goto callshrp;
		  }
		  wta(UNDEFINED, s_unknown_sharp, CHARS(tok_buf));
		}
	        goto tok;
	default:
		j = read_token(c, tok_buf, port);
tok:
		p = intern(CHARS(tok_buf), j);
		return CAR(p);
	}
}

#ifdef _UNICOS
_Pragma("noopt");		/* # pragma _CRI noopt */
#endif
static sizet read_token(ic, tok_buf, port)
     int ic;
     SCM tok_buf;
     SCM port;
{
	register sizet j = 1;
	register int c = ic;
	register char *p = CHARS(tok_buf);
	p[0] = downcase[c];
	while(1) {
		if (j+1 >= LENGTH(tok_buf)) p = grow_tok_buf(tok_buf);
		switch (c = lgetc(port)) {
#ifdef BRACKETS_AS_PARENS
		case '[': case ']':
#endif
		case '(': case ')': case '\"': case ';':
		case ',': case '`': case '#':
		case WHITE_SPACES:
		case LINE_INCREMENTORS:
			lungetc(c, port);
		case EOF:
			p[j] = 0;
			return j;
		default:
			p[j++] = downcase[c];
		}
	}
}
#ifdef _UNICOS
_Pragma("opt");			/* # pragma _CRI opt */
#endif

static SCM lreadparen(tok_buf, port, name)
     SCM tok_buf;
     SCM port;
     char *name;
{
  SCM tmp, tl, ans;
  int c = flush_ws(port, name);
  if (')'==c
#ifdef BRACKETS_AS_PARENS
      || ']'==c
#endif
      ) return EOL;
  lungetc(c, port);
  if (i_dot==(tmp = lreadr(tok_buf, port))) {
    ans = lreadr(tok_buf, port);
  closeit:
    if (')' != (c = flush_ws(port, name))
#ifdef BRACKETS_AS_PARENS
	&& ']' != c
#endif
	)
      wta(UNDEFINED, "missing close paren", "");
    return ans;
  }
  ans = tl = cons(tmp, EOL);
  while (')' != (c = flush_ws(port, name))
#ifdef BRACKETS_AS_PARENS
	 && ']' != c
#endif
	 ) {
    lungetc(c, port);
    if (i_dot==(tmp = lreadr(tok_buf, port))) {
      CDR(tl) = lreadr(tok_buf, port);
      goto closeit;
    }
    tl = (CDR(tl) = cons(tmp, EOL));
  }
  return ans;
}

/* These procedures implement synchronization primitives.  Processors
   with an atomic test-and-set instruction can use it here (and not
   DEFER_INTS). */
char s_tryarb[] = "try-arbiter";
char s_relarb[] = "release-arbiter";
long tc16_arbiter;
SCM tryarb(arb)
     SCM arb;
{
  ASSERT((TYP16(arb)==tc16_arbiter), arb, ARG1, s_tryarb);
  DEFER_INTS;
  if (CAR(arb) & (1L<<16))
    arb = BOOL_F;
  else {
    CAR(arb) = tc16_arbiter | (1L<<16);
    arb = BOOL_T;
  }
  ALLOW_INTS;
  return arb;
}
SCM relarb(arb)
     SCM arb;
{
  ASSERT((TYP16(arb)==tc16_arbiter), arb, ARG1, s_relarb);
  if (!(CAR(arb) & (1L<<16))) return BOOL_F;
  CAR(arb) = tc16_arbiter;
  return BOOL_T;
}
SCM makarb(name)
     SCM name;
{
  register SCM z;
  NEWCELL(z);
  CDR(z) = name;
  CAR(z) = tc16_arbiter;
  return z;
}
static int prinarb(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  lputs("#<arbiter ", port);
  if (CAR(exp) & (1L<<16)) lputs("locked ", port);
  iprin1(CDR(exp), port, writing);
  lputc('>', port);
  return !0;
}

static char s_tryload[] = "try-load";
#define s_load (&s_tryload[4])

struct errdesc {char *msg;char *s_response;short parent_err;};
struct errdesc errmsgs[] = {
  {"Wrong number of args", 0, 0},
  {"numerical overflow", 0, FPE_SIGNAL},
  {"Argument out of range", 0, FPE_SIGNAL},
  {"Could not allocate", "out-of-storage", 0},
  {"EXIT", "end-of-program", -1},
  {"hang up", "hang-up", EXIT},
  {"user interrupt", "user-interrupt", 0},
  {"arithmetic error", "arithmetic-error", 0},
  {"bus error", 0, 0},
  {"segment violation", 0, 0},
  {"alarm", "alarm-interrupt", 0}
};

int errjmp_bad = 1, ints_disabled = 1, sig_deferred = 0, alrm_deferred;
SCM err_exp, err_env;
char *err_pos, *err_s_subr;
cell tmp_errobj = {(SCM)UNDEFINED, (SCM)EOL};
cell tmp_loadpath = {(SCM)BOOL_F, (SCM)EOL};
SCM *loc_errobj = (SCM *)&tmp_errobj;
SCM *loc_loadpath = (SCM *)&tmp_loadpath;
SCM loadport = UNDEFINED;
long linum = 1;
int verbose = 1;
long cells_allocated = 0, lcells_allocated = 0,
  mallocated = 0, lmallocated = 0,
  rt = 0, gc_rt, gc_time_taken;
long gc_cells_collected, gc_malloc_collected, gc_ports_collected;
long gc_syms_collected;
static void def_err_response P((void));

int handle_it(i)
     int i;
{
  char *name = errmsgs[i-WNA].s_response;
  SCM proc;
  if (errjmp_bad) return -1;	/* sends it to def_err_response */
  if (name) {
    NEWCELL(proc);		/* discard possibly-used cell */
    proc = CDR(intern(name, (sizet)strlen(name)));
    if NIMP(proc) {
      apply(proc, EOL, EOL);
      return i;
    }
  }
  return errmsgs[i-WNA].parent_err;
}
static char s_eval_string[] = "eval-string";
static char s_load_string[] = "load-string";
SCM scm_eval_string(str)
     SCM str;
{
  str = mkstrport(INUM0, str, OPN | RDNG, s_eval_string);
  str = lread(str);
  return EVAL(str, (SCM)EOL);
}
SCM scm_load_string(str)
     SCM str;
{
  ASSERT(NIMP(str) && (STRINGP(str) || SYMBOLP(str)), str, ARG1,
	 s_load_string);
  str = mkstrport(INUM0, str, OPN | RDNG, s_load_string);
  while(1) {
    SCM form = lread(str);
    if (EOF_VAL==form) break;
    SIDEVAL(form, EOL);
  }
  return BOOL_T;
}

SCM exitval;			/* INUM with return value */
extern char s_unexec[];
SCM repl_driver(initpath)
     char *initpath;
{
#ifdef _UNICOS
  int i;
#else
  long i;
#endif
  CONT(rootcont)->stkbse = (STACKITEM *)&i;
  i = setjmp(CONT(rootcont)->jmpbuf);
#ifndef SHORT_INT
  if (i) i = UNCOOK(i);
#endif
  /* printf("repl_driver got %d\n", i); */
 drloop:
  switch ((int)i) {
  default: {
    char *name = errmsgs[i-WNA].s_response;
    if (name) {
      SCM proc = CDR(intern(name, (sizet)strlen(name)));
      if NIMP(proc) apply(proc, EOL, EOL);
    }
    if ((i = errmsgs[i-WNA].parent_err)) goto drloop;
    def_err_response();
    goto reset_toplvl;
  }
  case 0:
    exitval = MAKINUM(EXIT_SUCCESS);
    errjmp_bad = 0;
    errno = 0;
    alrm_deferred = 0;
    sig_deferred = 0;
    ints_disabled = 0;
    if (dumped) {
      lcells_allocated = cells_allocated;
      lmallocated = mallocated;
      rt = INUM(my_time());
      gc_time_taken = 0;
    }
    else if (scm_ldfile(initpath)) /* load Scheme init files */
      wta(*loc_errobj, "Could not open file", s_load);
    scm_evstr("(boot-tail)");	/* initialization tail-call */
  case -2:			/* abrt */
  reset_toplvl:
    errjmp_bad = 0;
    alrm_deferred = 0;
    sig_deferred = 0;
    ints_disabled = 0;

    /* Closing the loading file turned out to be a bad idea. */
    /* But I will leave the code here in case someone wants it. */
#ifdef CLOSE_LOADING_PORTS_ON_ABORT
    if (NIMP(loadport) && OPINPORTP(loadport)) {
      if (verbose > 1) {
	lputs("; Aborting load (closing): ", cur_errp);
	display(*loc_loadpath, cur_errp);
	newline(cur_errp);
      }
      close_port(loadport);	/* close loading file. */
    }
#endif
    *loc_loadpath = BOOL_F;
    loadport = UNDEFINED;
    repl();
    err_pos = (char *)EXIT;
    i = EXIT;
    goto drloop;		/* encountered EOF on stdin */
  case -1:			/* quit */
    return exitval;
  case -3:			/* restart. */
    return 0;
#ifdef CAN_DUMP
  case -4:			/* dump */
    igc(s_unexec, (STACKITEM *)0);
    dumped = 1;
    unexec(CHARS(*loc_errobj), execpath, 0, 0, 0);
    goto reset_toplvl;
#endif
  }
}

SCM line_num()
{
  return MAKINUM(linum);
}
SCM prog_args()
{
  return progargs;
}

extern char s_heap[];
extern sizet hplim_ind;
extern CELLPTR *hplims;
void growth_mon(obj, size, units)
     char *obj;
     long size;
     char *units;
{
  if (verbose>2)
    {
      lputs("; grew ", cur_errp);
      lputs(obj, cur_errp);
      lputs(" to ", cur_errp);
      intprint(size, 10, cur_errp);
      lputc(' ', cur_errp);
      lputs(units, cur_errp);
      if ((verbose>4) && (obj==s_heap)) heap_report();
      lputs("\n", cur_errp);
    }
}

void gc_start(what)
     char *what;
{
  if (verbose>3 && FPORTP(cur_errp)) {
    ALLOW_INTS;
    lputs(";GC(", cur_errp);
    lputs(what, cur_errp);
    lputs(")", cur_errp);
    lfflush(cur_errp);
    DEFER_INTS;
  }
  gc_rt = INUM(my_time());
  gc_cells_collected = 0;
  gc_malloc_collected = 0;
  gc_ports_collected = 0;
  gc_syms_collected = 0;
}
void gc_end()
{
  gc_rt = INUM(my_time()) - gc_rt;
  gc_time_taken = gc_time_taken + gc_rt;
  if (verbose>3) {
    ALLOW_INTS;
    if (!FPORTP(cur_errp)) lputs(";GC ", cur_errp);
    intprint(time_in_msec(gc_rt), 10, cur_errp);
    lputs(" cpu mSec, ", cur_errp);
    intprint(gc_cells_collected, 10, cur_errp);
    lputs(" cells, ", cur_errp);
    intprint(gc_malloc_collected, 10, cur_errp);
    lputs(" malloc, ", cur_errp);
    intprint(gc_syms_collected, 10, cur_errp);
    lputs(" syms, ", cur_errp);
    intprint(gc_ports_collected, 10, cur_errp);
    lputs(" ports collected\n", cur_errp);
    lfflush(cur_errp);
    DEFER_INTS;
  }
}
void repl_report()
{
  if (verbose>1) {
    lfflush(cur_outp);
    lputs(";Evaluation took ", cur_errp);
    intprint(time_in_msec(INUM(my_time())-rt), 10, cur_errp);
    lputs(" mSec (", cur_errp);
    intprint(time_in_msec(gc_time_taken), 10, cur_errp);
    lputs(" in gc) ", cur_errp);
    intprint(cells_allocated - lcells_allocated, 10, cur_errp);
    lputs(" cells work, ", cur_errp);
    intprint(mallocated - lmallocated, 10, cur_errp);
    lputs(" bytes other\n", cur_errp);
    lfflush(cur_errp);
  }
}
SCM lroom(args)
     SCM args;
{
  intprint(cells_allocated, 10, cur_errp);
  lputs(" out of ", cur_errp);
  intprint(heap_size, 10, cur_errp);
  lputs(" cells in use, ", cur_errp);
  intprint(mallocated, 10, cur_errp);
  lputs(" bytes allocated (of ", cur_errp);
  intprint(mtrigger, 10, cur_errp);
  lputs(")\n", cur_errp);
  if NIMP(args) {
    heap_report();
    lputs("\n", cur_errp);
    stack_report();
  }
  return UNSPECIFIED;
}
void heap_report()
{
  sizet i = 0;
  lputs("; heap segments:", cur_errp);
  while(i<hplim_ind) {
    lputs("\n; 0x", cur_errp);
    intprint((long)hplims[i++], 16, cur_errp);
    lputs(" - 0x", cur_errp);
    intprint((long)hplims[i++], 16, cur_errp);
  }
}
void exit_report()
{
  if (verbose>2) {
    lputs(";Totals: ", cur_errp);
    intprint(time_in_msec(INUM(my_time())), 10, cur_errp);
    lputs(" mSec my time, ", cur_errp);
    intprint(time_in_msec(INUM(your_time())), 10, cur_errp);
    lputs(" mSec your time\n", cur_errp);
  }
}

SCM prolixity(arg)
     SCM arg;
{
  int old = verbose;
  if (!UNBNDP(arg)) {
    if FALSEP(arg) verbose = 1;
    else verbose = INUM(arg);
  }
  return MAKINUM(old);
}

void repl()
{
  SCM x;
  repl_report();
  while(1) {
    if OPOUTPORTP(cur_inp) {	/* This case for curses window */
      lfflush(cur_outp);
      if (verbose) lputs(PROMPT, cur_inp);
      lfflush(cur_inp);
    }
    else {
      if (verbose) lputs(PROMPT, cur_outp);
      lfflush(cur_outp);
    }
    lcells_allocated = cells_allocated;
    lmallocated = mallocated;
    x = lread(cur_inp);
    rt = INUM(my_time());
    gc_time_taken = 0;
    if (EOF_VAL==x) break;
    if (!CRDYP(cur_inp))	/* assure newline read (and transcripted) */
      lungetc(lgetc(cur_inp), cur_inp);
#ifdef __TURBOC__
    if ('\n' != CGETUN(cur_inp))
      if OPOUTPORTP(cur_inp)	/* This case for curses window */
	{lfflush(cur_outp); newline(cur_inp);}
      else newline(cur_outp);
#endif
    x = EVAL(x, (SCM)EOL);
    repl_report();
    iprin1(x, cur_outp, 1);
    lputc('\n', cur_outp);
  }
}
SCM quit(n)
     SCM n;
{
  if (UNBNDP(n) || BOOL_T==n) n = MAKINUM(EXIT_SUCCESS);
  else if INUMP(n) exitval = n;
  else exitval = MAKINUM(EXIT_FAILURE);
  if (errjmp_bad) exit(INUM(exitval));
  dowinds(EOL, ilength(dynwinds));
  longjmp(CONT(rootcont)->jmpbuf, COOKIE(-1));
}
SCM abrt()
{
  if (errjmp_bad) exit(INUM(exitval));
  dowinds(EOL, ilength(dynwinds));
#ifdef CAUTIOUS
  stacktrace = EOL;
#endif
  longjmp(CONT(rootcont)->jmpbuf, COOKIE(-2));
}
char s_restart[] = "restart";
SCM restart()
{
  /* ASSERT(!dumped, UNDEFINED, "dumped can't", s_restart); */
  dowinds(EOL, ilength(dynwinds));
#ifdef CAUTIOUS
  stacktrace = EOL;
#endif
  longjmp(CONT(rootcont)->jmpbuf, COOKIE(-3));
}

#ifdef CAN_DUMP
char s_unexec[] = "unexec";
SCM scm_unexec(newpath)
     SCM newpath;
{
  ASSERT(NIMP(newpath) && STRINGP(newpath), newpath, ARG1, s_unexec);
  *loc_errobj = newpath;
# ifdef CAUTIOUS
  stacktrace = EOL;
# endif
  longjmp(CONT(rootcont)->jmpbuf, COOKIE(-4));
}
#endif

char s_execpath[] = "execpath";
SCM scm_execpath(newpath)
     SCM newpath;
{
  SCM retval = execpath ? makfrom0str(execpath) : BOOL_F;
  if (UNBNDP(newpath))
    return retval;
  if (FALSEP(newpath)) {
    if (execpath) free(execpath);
    execpath = 0;
    return retval;
  }
  ASSERT(NIMP(newpath) && STRINGP(newpath), newpath, ARG1, s_execpath);
  if (execpath) free(execpath);
  execpath = scm_cat_path(0L, CHARS(newpath), 0L);
  return retval;
}

void han_sig()
{
  sig_deferred = 0;
  if (INT_SIGNAL != handle_it(INT_SIGNAL))
    wta(UNDEFINED, (char *)INT_SIGNAL, "");
}
void han_alrm()
{
  alrm_deferred = 0;
  if (ALRM_SIGNAL != handle_it(ALRM_SIGNAL))
    wta(UNDEFINED, (char *)ALRM_SIGNAL, "");
}

SCM tryload(filename)
     SCM filename;
{
  ASSERT(NIMP(filename) && STRINGP(filename), filename, ARG1, s_load);
  {
    SCM oloadpath = *loc_loadpath;
    SCM oloadport = loadport;
    long olninum = linum;
    SCM port, newform = BOOL_F;
    port = open_file(filename, makfromstr("r", (sizet)sizeof(char)));
    if FALSEP(port) return port;
    *loc_loadpath = filename;
    loadport = port;
    linum = 1;
    while(1) {
      SCM form = newform;
      newform = lread(port);
      if (EOF_VAL==newform) {
	close_port(port);
	linum = olninum;
	loadport = oloadport;
	*loc_loadpath = oloadpath;
	SIDEVAL(form, EOL);
	return BOOL_T;
      }
      SIDEVAL(form, EOL);
    }
  }
  return BOOL_T;
}
#ifdef CAUTIOUS
void scm_print_stack(stk)
     SCM stk;
{
  switch (ilength(stk)) {
  case -1:
    lputs("\n; circular stacktrace!", cur_errp);
    return;
  case -2:
    lputs("\n; stacktrace not a list?", cur_errp);
    iprin1(stk, cur_errp, 1);
    return;
  default:
    while NNULLP(stk) {
      SCM ste = CAR(stk);
      lputc('\n', cur_errp);
      iprin1(ste, cur_errp, 1);
      stk = CDR(stk);
    }
  }
}
SCM scm_stack_trace()
{
  if (0==ilength(stacktrace)) return BOOL_F;
  scm_print_stack(stacktrace);
  return BOOL_T;
}
#endif

static void err_head(str)
     char *str;
{
  int oerrno = errno;
  exitval = MAKINUM(EXIT_FAILURE);
  if NIMP(cur_outp) lfflush(cur_outp);
  lputc('\n', cur_errp);
  if(BOOL_F != *loc_loadpath) {
    iprin1(*loc_loadpath, cur_errp, 1);
    lputs(", line ", cur_errp);
    intprint((long)linum, 10, cur_errp);
    lputs(": ", cur_errp);
  }
  lfflush(cur_errp);
  errno = oerrno;
  if (cur_errp==def_errp) {
    if (errno>0) perror(str);
    fflush(stderr);
    return;
  }
}
void warn(str1, str2)
     char *str1, *str2;
{
  err_head("WARNING");
  lputs("WARNING: ", cur_errp);
  lputs(str1, cur_errp);
  lputs(str2, cur_errp);
  lputc('\n', cur_errp);
  lfflush(cur_errp);
}

SCM lerrno(arg)
     SCM arg;
{
  int old = errno;
  if (!UNBNDP(arg)) {
    if FALSEP(arg) errno = 0;
    else errno = INUM(arg);
  }
  return MAKINUM(old);
}
static char s_perror[] = "perror";
SCM lperror(arg)
     SCM arg;
{
  ASSERT(NIMP(arg) && STRINGP(arg), arg, ARG1, s_perror);
  err_head(CHARS(arg));
  return UNSPECIFIED;
}
static void def_err_response()
{
  SCM obj = *loc_errobj;
#ifdef CAUTIOUS
  SCM stk = stacktrace;
#endif
  DEFER_INTS;
  err_head("ERROR");
  lputs("ERROR: ", cur_errp);
  if (err_s_subr && *err_s_subr) {
    lputs(err_s_subr, cur_errp);
    lputs(": ", cur_errp);
  }
  if (err_pos==(char *)ARG1 && UNBNDP(*loc_errobj)) err_pos = (char *)WNA;
#ifdef nosve
  if ((~0x1fL) & (short)err_pos) lputs(err_pos, cur_errp);
  else if (WNA>(short)err_pos) {
    lputs("Wrong type in arg", cur_errp);
    lputc(err_pos ? '0'+(short)err_pos : ' ', cur_errp);
  }
#else
  if ((~0x1fL) & (long)err_pos) lputs(err_pos, cur_errp);
  else if (WNA>(long)err_pos) {
    lputs("Wrong type in arg", cur_errp);
    lputc(err_pos ? '0'+(int)err_pos : ' ', cur_errp);
  }
#endif
  else {
    lputs(errmsgs[((int)err_pos)-WNA].msg, cur_errp);
    goto outobj;
  }
  if (IMP(obj) || SYMBOLP(obj) || (TYP16(obj)==tc7_port)
      || (NFALSEP(procedurep(obj))) || (NFALSEP(numberp(obj)))) {
outobj:
    if (!UNBNDP(obj)) {
      lputs(((long)err_pos==WNA)?" given ":" ", cur_errp);
      iprin1(obj, cur_errp, 1);
    }
  }
  else lputs(" (see errobj)", cur_errp);
#ifdef CAUTIOUS
  if NNULLP(stk) scm_print_stack(stk);
#endif
  if UNBNDP(err_exp) goto getout;
  if NIMP(err_exp) {
    lputs("\n; in expression: ", cur_errp);
    if NCONSP(err_exp) iprin1(err_exp, cur_errp, 1);
    else if (UNDEFINED==CDR(err_exp))
      iprin1(CAR(err_exp), cur_errp, 1);
    else iprlist("(... ", err_exp, ')', cur_errp, 1);
  }
  if NULLP(err_env) lputs("\n; in top level environment.", cur_errp);
  else {
    SCM env = err_env;
    lputs("\n; in scope:", cur_errp);
    while NNULLP(env) {
      lputc('\n', cur_errp);
      lputs(";   ", cur_errp);
      iprin1(CAR(CAR(env)), cur_errp, 1);
      env = CDR(env);
    }
  }
 getout:
  lputc('\n', cur_errp);
  lfflush(cur_errp);
  err_exp = err_env = UNDEFINED;
  if (errjmp_bad) {
    iprin1(obj, cur_errp, 1);
    lputs("\nFATAL ERROR DURING CRITICAL CODE SECTION\n", cur_errp);
#ifdef vms
    exit(EXIT_FAILURE);
#else
    exit(errno? (long)errno : EXIT_FAILURE);
#endif
  }
  errno = 0;
  ALLOW_INTS;
}
void everr(exp, env, arg, pos, s_subr)
     SCM exp, env, arg;
     char *pos, *s_subr;
{
  err_exp = exp;
  err_env = env;
  *loc_errobj = arg;
  err_pos = pos;
  err_s_subr = s_subr;
#ifndef CAUTIOUS
  if (((~0x1fL) & (long)pos) || (WNA>(long)pos)
      || NIMP(dynwinds) || errjmp_bad)
#endif
    {
      def_err_response();
      dowinds(EOL, ilength(dynwinds));
      abrt();
    }
#ifndef CAUTIOUS
  /* We don't have to clear stacktrace because CAUTIOUS never gets here */
  /* We don't have to dowinds() because dynwinds is EOL */
  longjmp(CONT(rootcont)->jmpbuf, COOKIE((int)pos));
  /* will do error processing at stack base */
#endif
}
void wta(arg, pos, s_subr)
     SCM arg;
char *pos, *s_subr;
{
 everr(UNDEFINED, EOL, arg, pos, s_subr);
}
SCM cur_input_port()
{
  return cur_inp;
}
SCM cur_output_port()
{
  return cur_outp;
}
SCM cur_error_port()
{
  return cur_errp;
}
char s_cur_inp[] = "set-current-input-port";
char s_cur_outp[] = "set-current-output-port";
char s_cur_errp[] = "set-current-error-port";
SCM set_inp(port)
     SCM port;
{
  SCM oinp = cur_inp;
  ASSERT(NIMP(port) && OPINPORTP(port), port, ARG1, s_cur_inp);
  cur_inp = port;
  return oinp;
}
SCM set_outp(port)
     SCM port;
{
  SCM ooutp = cur_outp;
  ASSERT(NIMP(port) && OPOUTPORTP(port), port, ARG1, s_cur_outp);
  cur_outp = port;
  return ooutp;
}
SCM set_errp(port)
     SCM port;
{
  SCM oerrp = cur_errp;
  ASSERT(NIMP(port) && OPOUTPORTP(port), port, ARG1, s_cur_errp);
  cur_errp = port;
  return oerrp;
}

static iproc subr0s[] = {
	{&s_cur_inp[4], cur_input_port},
	{&s_cur_outp[4], cur_output_port},
	{&s_cur_errp[4], cur_error_port},
	{"transcript-off", trans_off},
	{"program-arguments", prog_args},
	{"line-number", line_num},
	{"abort", abrt},
	{s_restart, restart},
#ifdef CAUTIOUS
	{"stack-trace", scm_stack_trace},
#endif
	{0, 0}};

static iproc subr1s[] = {
	{s_cur_inp, set_inp},
	{s_cur_outp, set_outp},
	{s_cur_errp, set_errp},
	{"transcript-on", trans_on},
	{s_tryload, tryload},
	{s_load_string, scm_load_string},
	{s_eval_string, scm_eval_string},
	{s_perror, lperror},
	{"make-arbiter", makarb},
	{s_tryarb, tryarb},
	{s_relarb, relarb},
	{0, 0}};

static iproc subr1os[] = {
	{s_read, lread},
	{s_read_char, scm_read_char},
	{s_peek_char, peek_char},
	{s_newline, newline},
	{s_flush, lflush},
#ifndef GO32
	{s_char_readyp, char_readyp},
#endif
	{"quit", quit},
	{"verbose", prolixity},
	{"errno", lerrno},
	{s_execpath, scm_execpath},
	{0, 0}};

static iproc subr2os[] = {
	{s_write, lwrite},
	{s_display, display},
	{s_write_char, write_char},
#ifdef CAN_DUMP
	{s_unexec, scm_unexec},
#endif
	{0, 0}};

static smobfuns arbsmob = {markcdr, free0, prinarb};
char s_ccl[] = "char-code-limit";

void init_repl( iverbose )
     int iverbose;
{
	sysintern(s_ccl, MAKINUM(CHAR_CODE_LIMIT));
	loc_errobj = &CDR(sysintern("errobj", UNDEFINED));
	loc_loadpath = &CDR(sysintern("*load-pathname*", BOOL_F));
	transcript = BOOL_F;
	trans = 0;
	linum = 1;
	verbose = iverbose;
	init_iprocs(subr0s, tc7_subr_0);
	init_iprocs(subr1os, tc7_subr_1o);
	init_iprocs(subr1s, tc7_subr_1);
	init_iprocs(subr2os, tc7_subr_2o);
	make_subr("room", tc7_lsubr, lroom);
#ifndef GO32
	add_feature(s_char_readyp);
#endif
#ifdef CAN_DUMP
	if (!execpath) execpath = dld_find_executable(CHARS(CAR(progargs)));
	add_feature("dump");
	scm_ldstr("\
(define (dump file . thunk)\n\
  (cond ((null? thunk) (set! *interactive* #f) (set! *argv* #f))\n\
	((not (car thunk)) (set! *argv* #f))\n\
	((boolean? (car thunk)))\n\
	(else (set! boot-tail (car thunk))))\n\
  (set! restart exec-self)\n\
  (unexec file))\n\
");
#endif
#ifdef ARM_ULIB
	set_erase();
#endif
	tc16_arbiter = newsmob(&arbsmob);
}
void final_repl()
{
  loc_errobj = (SCM *)&tmp_errobj;
  loc_loadpath = (SCM *)&tmp_loadpath;
  loadport = UNDEFINED;
  transcript = BOOL_F;
  trans = 0;
  linum = 1;
}
