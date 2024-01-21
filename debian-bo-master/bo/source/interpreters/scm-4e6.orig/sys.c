/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
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

/* "sys.c" opening and closing files, storage, and GC. */

#include <ctype.h>

#include "scm.h"
#include "setjump.h"
void	igc P((char *what, STACKITEM *stackbase));

/* ttyname() etc. should be defined in <unistd.h>.  But unistd.h is
   missing on many systems. */

#ifndef STDC_HEADERS
	char *ttyname P((int fd));
	char *tmpnam P((char *s));
	sizet fwrite ();
# ifdef sun
#  ifndef __svr4__
        int fputs P((char *s, FILE* stream));
        int fputc P((char c, FILE* stream));
        int fflush P((FILE* stream));
#  endif
# endif
	int fgetc P((FILE* stream));
	int fclose P((FILE* stream));
	int pclose P((FILE* stream));
	int unlink P((const char *pathname));
	char *mktemp P((char *template));
#endif

static void gc_sweep P((void));

char	s_nogrow[] = "could not grow", s_heap[] = "heap",
	s_hplims[] = "hplims";
static char	s_input_portp[] = "input-port?",
		s_output_portp[] = "output-port?";
static char	s_open_file[] = "open-file";
char	s_close_port[] = "close-port";

#ifdef __IBMC__
# include <io.h>
# include <direct.h>
# define ttyname(x) "CON:"
#else
# ifndef MSDOS
#  ifndef ultrix
#   ifndef vms
#    ifdef _DCC
#     include <ioctl.h>
#     define setbuf(stream, buf) setvbuf(stream, buf, _IONBF, 0)
#    else
#     ifdef MWC
#      include <sys/io.h>
#     else
#      ifndef THINK_C
#       ifndef ARM_ULIB
#        include <sys/ioctl.h>
#       endif
#      endif
#     endif
#    endif
#   endif
#  endif
# endif
#endif /* __IBMC__ */
SCM i_setbuf0(port)		/* should be called with DEFER_INTS active */
     SCM port;
{
#ifndef NOSETBUF
# ifndef MSDOS
#  ifdef FIONREAD
#   ifndef ultrix
  SYSCALL(setbuf(STREAM(port), 0););
#   endif
#  endif
# endif
#endif
  return UNSPECIFIED;
}

long mode_bits(modes)
     char *modes;
{
  return OPN | (strchr(modes, 'r') || strchr(modes, '+') ? RDNG : 0)
    | (strchr(modes, 'w') || strchr(modes, 'a') || strchr(modes, '+') ? WRTNG : 0)
      | (strchr(modes, '0') ? BUF0 : 0);
}

SCM open_file(filename, modes)
     SCM filename, modes;
{
  register SCM port;
  FILE *f;
  ASSERT(NIMP(filename) && STRINGP(filename), filename, ARG1, s_open_file);
  ASSERT(NIMP(modes) && STRINGP(modes), modes, ARG2, s_open_file);
  NEWCELL(port);
  DEFER_INTS;
  SYSCALL(f = fopen(CHARS(filename), CHARS(modes)););
  if (!f) port = BOOL_F;
  else {
    SETSTREAM(port, f);
    if (BUF0 & (CAR(port) = tc16_fport | mode_bits(CHARS(modes))))
      i_setbuf0(port);
    ALLOW_INTS;
  }
  return port;
}

SCM close_port(port)
     SCM port;
{
	sizet i;
	ASSERT(NIMP(port) && PORTP(port), port, ARG1, s_close_port);
	if CLOSEDP(port) return UNSPECIFIED;
	i = PTOBNUM(port);
	DEFER_INTS;
	if (ptobs[i].fclose) {
	  SYSCALL((ptobs[i].fclose)(STREAM(port)););
	}
	CAR(port) &= ~OPN;
	ALLOW_INTS;
	return UNSPECIFIED;
}
SCM input_portp(x)
     SCM x;
{
	if IMP(x) return BOOL_F;
	return INPORTP(x) ? BOOL_T : BOOL_F;
}
SCM output_portp(x)
     SCM x;
{
	if IMP(x) return BOOL_F;
	return OUTPORTP(x) ? BOOL_T : BOOL_F;
}

#if (__TURBOC__==1)
# undef L_tmpnam		/* Not supported in TURBOC V1.0 */
#endif
#ifdef GO32
# undef L_tmpnam
#endif
#ifdef MWC
# undef L_tmpnam
#endif

#ifdef L_tmpnam
SCM ltmpnam()
{
  char name[L_tmpnam];
  SYSCALL(tmpnam(name););
  return makfrom0str(name);
}
#else
/* TEMPTEMPLATE is used only if mktemp() is being used instead of
   tmpnam(). */

# ifdef AMIGA
#  define TEMPTEMPLATE "T:SchemeaaaXXXXXX";
# else
#  ifdef vms
#   define TEMPTEMPLATE "sys$scratch:aaaXXXXXX";
#  else /* vms */
#   ifdef __MSDOS__
#    ifdef GO32
#     define TEMPTEMPLATE "\\tmp\\TMPaaaXXXXXX";
#    else
#     define TEMPTEMPLATE "TMPaaaXXXXXX";
#    endif
#   else /* __MSDOS__ */
#    define TEMPTEMPLATE "/tmp/aaaXXXXXX";
#   endif /* __MSDOS__ */
#  endif /* vms */
# endif /* AMIGA */

char template[] = TEMPTEMPLATE;
# define TEMPLEN (sizeof template/sizeof(char) - 1)
SCM ltmpnam()
{
  SCM name;
  int temppos = TEMPLEN-9;
  name = makfromstr(template, (sizet)TEMPLEN);
  DEFER_INTS;
inclp:
  template[temppos]++;
  if (!isalpha(template[temppos])) {
    template[temppos++] = 'a';
    goto inclp;
  }
# ifndef AMIGA
#  ifndef __MSDOS__
  SYSCALL(temppos = !*mktemp(CHARS(name)););
  if (temppos) name = BOOL_F;
#  endif
# endif
  ALLOW_INTS;
  return name;
}
#endif /* L_tmpnam */

#ifdef M_SYSV
# define remove unlink
#endif
static char s_del_fil[] = "delete-file";
SCM del_fil(str)
     SCM str;
{
  int ans;
  ASSERT(NIMP(str) && STRINGP(str), str, ARG1, s_del_fil);
#ifdef STDC_HEADERS
  SYSCALL(ans = remove(CHARS(str)););
#else
  SYSCALL(ans = unlink(CHARS(str)););
#endif
  return ans ? BOOL_F : BOOL_T;
}

void prinport(exp, port, type)
     SCM exp; SCM port; char *type;
{
  lputs("#<", port);
  if CLOSEDP(exp) lputs("closed-", port);
  else {
    if (RDNG & CAR(exp)) lputs("input-", port);
    if (WRTNG & CAR(exp)) lputs("output-", port);
  }
  lputs(type, port);
  lputc(' ', port);
#ifndef MSDOS
# ifndef __EMX__
#  ifndef _DCC
#   ifndef AMIGA
#    ifndef THINK_C
  if (OPENP(exp) && tc16_fport==TYP16(exp) && isatty(fileno(STREAM(exp))))
    lputs(ttyname(fileno(STREAM(exp))), port);
  else
#    endif
#   endif
#  endif
# endif
#endif
    if OPFPORTP(exp) intprint((long)fileno(STREAM(exp)), 10, port);
    else intprint(CDR(exp), 16, port);
  lputc('>', port);
}
static int prinfport(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  prinport(exp, port, s_port_type);
  return !0;
}
static int prinstpt(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  prinport(exp, port, s_string);
  return !0;
}
static int prinsfpt(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  prinport(exp, port, "soft");
  return !0;
}

static int stputc(c, p)
     int c; SCM p;
{
  sizet ind = INUM(CAR(p));
  if (ind >= LENGTH(CDR(p))) resizuve(CDR(p), MAKINUM(ind + (ind>>1)));
  CHARS(CDR(p))[ind] = c;
  CAR(p) = MAKINUM(ind + 1);
  return c;
}
sizet stwrite(str, siz, num, p)
     sizet siz, num;
     char *str; SCM p;
{
  sizet ind = INUM(CAR(p));
  sizet len = siz * num;
  char *dst;
  if (ind + len >= LENGTH(CDR(p)))
    resizuve(CDR(p), MAKINUM(ind + len + ((ind + len)>>1)));
  dst = &(CHARS(CDR(p))[ind]);
  while (len--) dst[len] = str[len];
  CAR(p) = MAKINUM(ind + siz*num);
  return num;
}
static int stputs(s, p)
     char *s; SCM p;
{
  stwrite(s, 1, strlen(s), p);
  return 0;
}
static int stgetc(p)
     SCM p;
{
  sizet ind = INUM(CAR(p));
  if (ind >= LENGTH(CDR(p))) return EOF;
  CAR(p) = MAKINUM(ind + 1);
  return CHARS(CDR(p))[ind];
}
int noop0(stream)
     FILE *stream;
{
  return 0;
}
SCM mkstrport(pos, str, modes, caller)
     SCM pos;
     SCM str;
     long modes;
     char *caller;
{
  SCM z;
  ASSERT(INUMP(pos) && INUM(pos) >= 0, pos, ARG1, caller);
  ASSERT(NIMP(str) && (STRINGP(str) || SYMBOLP(str)), str, ARG1, caller);
  str = cons(pos, str);
  NEWCELL(z);
  DEFER_INTS;
  SETCHARS(z, str);
  CAR(z) = tc16_strport | modes;
  ALLOW_INTS;
  return z;
}
static char s_cwos[] = "call-with-output-string";
static char s_cwis[] = "call-with-input-string";
SCM cwos(proc)
     SCM proc;
{
  SCM p = mkstrport(INUM0, make_string(MAKINUM(30), UNDEFINED),
		    OPN | WRTNG,
		    s_cwos);
  apply(proc, p, listofnull);
  return resizuve(CDR(CDR(p)), CAR(CDR(p)));
}
SCM cwis(str, proc)
     SCM str, proc;
{
  SCM p = mkstrport(INUM0, str, OPN | RDNG, s_cwis);
  return apply(proc, p, listofnull);
}
#ifdef vms
sizet pwrite(ptr, size, nitems, port)
     char *ptr;
     sizet size, nitems;
     FILE* port;
{
  sizet len = size * nitems;
  sizet i = 0;
  for(;i < len;i++) putc(ptr[i], port);
  return len;
}
# define ffwrite pwrite
#else
# define ffwrite fwrite
#endif

static ptobfuns fptob = {
  mark0,
  fclose,
  prinfport,
  0,
  fputc,
  fputs,
  ffwrite,
  fflush,
  fgetc,
  fclose};
ptobfuns pipob = {
  mark0,
  0, 				/* replaced by pclose in init_ioext() */
  0, 				/* replaced by prinpipe in init_ioext() */
  0,
  fputc,
  fputs,
  ffwrite,
  fflush,
  fgetc,
  0};				/* replaced by pclose in init_ioext() */
static ptobfuns stptob = {
  markcdr,
  noop0,
  prinstpt,
  0,
  stputc,
  stputs,
  stwrite,
  noop0,
  stgetc,
  0};

				/* Soft ports */

/* fputc, fwrite, fputs, and fclose are called within a
   SYSCALL.  So we need to set errno to 0 before returning.  fflush
   may be called within a SYSCALL.  So we need to set errno to 0
   before returning. */

static int sfputc(c, p)
     int c; SCM p;
{
  apply(VELTS(p)[0], MAKICHR(c), listofnull);
  errno = 0;
  return c;
}
sizet sfwrite(str, siz, num, p)
     sizet siz, num;
     char *str; SCM p;
{
  SCM sstr;
  sstr = makfromstr(str, siz * num);
  apply(VELTS(p)[1], sstr, listofnull);
  errno = 0;
  return num;
}
static int sfputs(s, p)
     char *s; SCM p;
{
  sfwrite(s, 1, strlen(s), p);
  return 0;
}
int sfflush(stream)
     SCM stream;
{
  SCM f = VELTS(stream)[2];
  if (BOOL_F==f) return 0;
  f = apply(f, EOL, EOL);
  errno = 0;
  return BOOL_F==f ? EOF : 0;
}
static int sfgetc(p)
     SCM p;
{
  SCM ans;
  ans = apply(VELTS(p)[3], EOL, EOL);
  errno = 0;
  if (FALSEP(ans) || EOF_VAL==ans) return EOF;
  ASSERT(ICHRP(ans), ans, ARG1, "getc");
  return ICHR(ans);
}
static int sfclose(p)
     SCM p;
{
  SCM f = VELTS(p)[4];
  if (BOOL_F==f) return 0;
  f = apply(f, EOL, EOL);
  errno = 0;
  return BOOL_F==f ? EOF : 0;
}
static char s_mksfpt[] = "make-soft-port";
SCM mksfpt(pv, modes)
     SCM pv, modes;
{
  SCM z;
  ASSERT(NIMP(pv) && VECTORP(pv) && 5==LENGTH(pv), pv, ARG1, s_mksfpt);
  ASSERT(NIMP(modes) && STRINGP(modes), modes, ARG2, s_mksfpt);
  NEWCELL(z);
  DEFER_INTS;
  CAR(z) = tc16_sfport | mode_bits(CHARS(modes));
  SETSTREAM(z, pv);
  ALLOW_INTS;
  return z;
}

static ptobfuns sfptob = {
  markcdr,
  noop0,
  prinsfpt,
  0,
  sfputc,
  sfputs,
  sfwrite,
  sfflush,
  sfgetc,
  sfclose};

static smobfuns freecell = {
  mark0,
  free0,
  0,
  0};
static smobfuns flob = {
  mark0,
  /*flofree*/0,
  floprint,
  floequal};
static smobfuns bigob = {
  mark0,
  /*bigfree*/0,
  bigprint,
  bigequal};
void (**finals)() = 0;
sizet num_finals = 0;
static char s_final[] = "final";

void init_types()
{
  numptob = 0;
  ptobs = (ptobfuns *)malloc(4*sizeof(ptobfuns));
  /* These newptob calls must be done in this order */
  /* tc16_fport = */ newptob(&fptob);
  /* tc16_pipe = */ newptob(&pipob);
  /* tc16_strport = */ newptob(&stptob);
  /* tc16_sfport = */ newptob(&sfptob);
  numsmob = 0;
  smobs = (smobfuns *)malloc(7*sizeof(smobfuns));
  /* These newsmob calls must be done in this order */
  newsmob(&freecell);
  newsmob(&flob);
  newsmob(&bigob);
  newsmob(&bigob);
  finals = (void(**)())malloc(2 * sizeof(finals[0]));
  num_finals = 0;
}

void add_final(final)
     void (* final)();
{
  DEFER_INTS;
  finals = (void (**)()) must_realloc((char *)finals,
				      1L*(num_finals)*sizeof(finals[0]),
				      (1L+num_finals)*sizeof(finals[0]),
				      s_final);
  finals[num_finals++] = final;
  ALLOW_INTS;
  return;
}

char s_obunhash[] = "object-unhash";
static iproc subr0s[] = {
	{"gc", gc},
	{"tmpnam", ltmpnam},
	{0, 0}};

static iproc subr1s[] = {
	{s_input_portp, input_portp},
	{s_output_portp, output_portp},
	{s_close_port, close_port},
	{"eof-object?", eof_objectp},
	{s_cwos, cwos},
	{"object-hash", obhash},
	{s_obunhash, obunhash},
	{s_del_fil, del_fil},
	{0, 0}};

static iproc subr2s[] = {
	{s_open_file, open_file},
	{s_cwis, cwis},
	{s_mksfpt, mksfpt},
	{0, 0}};

SCM dynwind P((SCM thunk1, SCM thunk2, SCM thunk3));
void init_io(){
  make_subr("dynamic-wind", tc7_subr_3, dynwind);
  init_iprocs(subr0s, tc7_subr_0);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2s, tc7_subr_2);
#ifndef CHEAP_CONTINUATIONS
  add_feature("full-continuation");
#endif
}

void grew_lim(nm)
     long nm;
{
  ALLOW_INTS;
  growth_mon(s_limit, nm, "bytes");
  DEFER_INTS;
}
int expmem = 0;
sizet hplim_ind = 0;
long heap_size = 0;
CELLPTR *hplims, heap_org;
SCM freelist = EOL;
long mtrigger;
char *must_malloc(len, what)
     long len;
     char *what;
{
	char *ptr;
	sizet size = len;
	long nm = mallocated+size;
	if (len != size)
malerr:
		wta(MAKINUM(len), (char *)NALLOC, what);
	if ((nm <= mtrigger)) {
	  SYSCALL(ptr = (char *)malloc(size););
	  if (NULL != ptr) {mallocated = nm; return ptr;}
	}
	igc(what, CONT(rootcont)->stkbse);
	nm = mallocated+size;
	if (nm > mtrigger) grew_lim(nm+nm/2); /* must do before malloc */
	SYSCALL(ptr = (char *)malloc(size););
	if (NULL != ptr) {
	  mallocated = nm;
	  if (nm > mtrigger) mtrigger = nm + nm/2;
	  return ptr;}
	goto malerr;
}
char *must_realloc(where, olen, len, what)
     char *where;
     long olen, len;
     char *what;
{
	char *ptr;
	sizet size = len;
	long nm = mallocated+size-olen;
	if (len != size)
ralerr:
		wta(MAKINUM(len), (char *)NALLOC, what);
	if ((nm <= mtrigger)) {
	  SYSCALL(ptr = (char *)realloc(where, size););
	  if (NULL != ptr) {mallocated = nm; return ptr;}
	}
	igc(what, CONT(rootcont)->stkbse);
	nm = mallocated+size-olen;
	if (nm > mtrigger) grew_lim(nm+nm/2); /* must do before realloc */
	SYSCALL(ptr = (char *)realloc(where, size););
	if (NULL != ptr) {
	  mallocated = nm;
	  if (nm > mtrigger) mtrigger = nm + nm/2;
	  return ptr;}
	goto ralerr;
}
void must_free(obj)
     char *obj;
{
  if (obj) free(obj);
  else wta(INUM0, "already free", "");
}

SCM symhash;			/* This used to be a sys_protect, but
				   Radey Shouman <shouman@zianet.com>
				   added GC for unuesd, UNDEFINED
				   symbols.*/
int symhash_dim = NUM_HASH_BUCKETS;
/* sym2vcell looks up the symbol in the symhash table. */
SCM sym2vcell(sym)
     SCM sym;
{
  SCM lsym, z;
  sizet hash = strhash(UCHARS(sym), (sizet)LENGTH(sym),
		       (unsigned long)symhash_dim);
  for(lsym = VELTS(symhash)[hash];NIMP(lsym);lsym = CDR(lsym)) {
    z = CAR(lsym);
    if (CAR(z)==sym) return z;
  }
  wta(sym, "uninterned symbol? ", "");
}
/* intern() and sysintern() return a pair;
   CAR is the symbol, CDR is the value. */
SCM intern(name, len)
     char *name;
     sizet len;
{
  SCM lsym, z;
  register sizet i = len;
  register unsigned char *tmp = (unsigned char *)name;
  sizet hash = strhash(tmp, i, (unsigned long)symhash_dim);
  for(lsym = VELTS(symhash)[hash];NIMP(lsym);lsym = CDR(lsym)) {
    z = CAR(lsym);
    z = CAR(z);
    tmp = UCHARS(z);
    if (LENGTH(z) != len) goto trynext;
    for(i = len;i--;) if (((unsigned char *)name)[i] != tmp[i]) goto trynext;
    return CAR(lsym);
  trynext: ;
  }
  lsym = makfromstr(name, len);
  DEFER_INTS;
  SETLENGTH(lsym, (long)len, tc7_msymbol);
  ALLOW_INTS;
  z = acons(lsym, UNDEFINED, UNDEFINED);
  DEFER_INTS;			/* Operations on symhash must be atomic. */
  CDR(z) = VELTS(symhash)[hash];
  VELTS(symhash)[hash] = z;
  z = CAR(z);
  ALLOW_INTS;
  return z;
}
SCM sysintern(name, val)
     char *name;
     SCM val;
{
  SCM lsym, z;
  sizet len = strlen(name);
  register sizet i = len;
  register unsigned char *tmp = (unsigned char *)name;
  sizet hash = strhash(tmp, i, (unsigned long)symhash_dim);
  for(lsym = VELTS(symhash)[hash];NIMP(lsym);lsym = CDR(lsym)) {
    z = CAR(lsym);
    z = CAR(z);
    tmp = UCHARS(z);
    if (LENGTH(z) != len) goto trynext;
    for(i = len;i--;) if (((unsigned char *)name)[i] != tmp[i]) goto trynext;
    lsym = CAR(lsym);
    CDR(lsym) = val;
    return lsym;
  trynext: ;
  }
  NEWCELL(lsym);
  SETLENGTH(lsym, (long)len, tc7_ssymbol);
  SETCHARS(lsym, name);
  lsym = cons(lsym, val);
  z = cons(lsym, UNDEFINED);
  CDR(z) = VELTS(symhash)[hash];
  VELTS(symhash)[hash] = z;
  return lsym;
}
SCM cons(x, y)
     SCM x, y;
{
	register SCM z;
	NEWCELL(z);
	CAR(z) = x;
	CDR(z) = y;
	return z;
}
SCM cons2(w, x, y)
     SCM w, x, y;
{
	register SCM z;
	NEWCELL(z);
	CAR(z) = x;
	CDR(z) = y;
	x = z;
	NEWCELL(z);
	CAR(z) = w;
	CDR(z) = x;
	return z;
}
SCM acons(w, x, y)
     SCM w, x, y;
{
	register SCM z;
	NEWCELL(z);
	CAR(z) = w;
	CDR(z) = x;
	x = z;
	NEWCELL(z);
	CAR(z) = x;
	CDR(z) = y;
	return z;
}

SCM makstr(len)
     long len;
{
	SCM s;
	NEWCELL(s);
	DEFER_INTS;
	SETCHARS(s, must_malloc(len+1, s_string));
	SETLENGTH(s, len, tc7_string);
	ALLOW_INTS;
	CHARS(s)[len] = 0;
	return s;
}

SCM make_subr(name, type, fcn)
     char *name;
     int type;
     SCM (*fcn)();
{
	SCM symcell = sysintern(name, UNDEFINED);
	long tmp = ((((CELLPTR)(CAR(symcell)))-heap_org)<<8);
	register SCM z;
	if ((tmp>>8) != ((CELLPTR)(CAR(symcell))-heap_org))
	  tmp = 0;
	NEWCELL(z);
	SUBRF(z) = fcn;
	CAR(z) = tmp + type;
	CDR(symcell) = z;
	return z;
}

#ifdef CCLO
SCM makcclo(proc, len)
     SCM proc;
     long len;
{
  SCM s;
  NEWCELL(s);
  DEFER_INTS;
  SETCHARS(s, must_malloc(len*sizeof(SCM), "compiled-closure"));
  SETLENGTH(s, len, tc7_cclo);
  while (--len) VELTS(s)[len] = UNSPECIFIED;
  CCLO_SUBR(s) = proc;
  ALLOW_INTS;
  return s;
}
#endif

#ifdef STACK_LIMIT
void stack_check()
{
  STACKITEM *start = CONT(rootcont)->stkbse;
  STACKITEM stack;
# ifdef STACK_GROWS_UP
  if (&stack - start > STACK_LIMIT/sizeof(STACKITEM))
# else
  if (start - &stack > STACK_LIMIT/sizeof(STACKITEM))
# endif /* def STACK_GROWS_UP */
    wta(UNDEFINED, (char *)SEGV_SIGNAL, "stack");
}
#endif
void stack_report()
{
  STACKITEM stack;
  intprint(stack_size(CONT(rootcont)->stkbse)*sizeof(STACKITEM), 16, cur_errp);
  lputs(" of stack: 0x", cur_errp);
  intprint((long)CONT(rootcont)->stkbse, 16, cur_errp);
  lputs(" - 0x", cur_errp);
  intprint((long)&stack, 16, cur_errp);
  lputs("\n", cur_errp);
}

SCM dynwind(thunk1, thunk2, thunk3)
     SCM thunk1, thunk2, thunk3;
{
  SCM ans;
  apply(thunk1, EOL, EOL);
  dynwinds = acons(thunk1, thunk3, dynwinds);
  ans = apply(thunk2, EOL, EOL);
  dynwinds = CDR(dynwinds);
  apply(thunk3, EOL, EOL);
  return ans;
}
void dowinds(to, delta)
     SCM to;
     long delta;
{
 tail:
  if (dynwinds==to);
  else if (0 > delta) {
    dowinds(CDR(to), 1+delta);
    apply(CAR(CAR(to)), EOL, EOL);
    dynwinds = to;
  }
  else {
    SCM from = CDR(CAR(dynwinds));
    dynwinds = CDR(dynwinds);
    apply(from, EOL, EOL);
    delta--; goto tail;		/* dowinds(to, delta-1); */
  }
}

/* Remember that setjmp needs to be called after scm_make_cont */

SCM scm_make_cont()
{
  SCM cont;
  CONTINUATION *ncont;
  NEWCELL(cont);
  DEFER_INTS;
  ncont = make_continuation(CONT(rootcont));
  if (!ncont) wta(MAKINUM(-1), (char *)NALLOC, s_cont);
  ncont->other.parent = rootcont;
  SETCONT(cont, ncont);
  SETLENGTH(cont, ncont->length, tc7_contin);
  ncont->other.dynenv = dynwinds;
#ifdef CAUTIOUS
  CONT(cont)->other.stack_trace = stacktrace;
#endif
  ALLOW_INTS;
  return cont;
}
static char s_sstale[] = "strangely stale";
void scm_dynthrow(cont, val)
     CONTINUATION *cont;
     SCM val;
{
  if (cont->stkbse != CONT(rootcont)->stkbse)
    wta(cont->other.dynenv, &s_sstale[10], s_cont);
  dowinds(cont->other.dynenv,
	  ilength(dynwinds)-ilength(cont->other.dynenv));
#ifdef CAUTIOUS
  stacktrace = cont->other.stack_trace;
#endif
  throw_to_continuation(cont, val, CONT(rootcont));
  wta(cont->other.dynenv, s_sstale, s_cont);
}

SCM obhash(obj)
     SCM obj;
{

#ifdef BIGDIG
  long n = SRS(obj, 1);
  if (!FIXABLE(n)) return long2big(n);
#endif
  return (obj<<1)+2L;
}

SCM obunhash(obj)
     SCM obj;
{
#ifdef BIGDIG
  if (NIMP(obj) && BIGP(obj)) {
    sizet i = NUMDIGS(obj);
    BIGDIG *ds = BDIGITS(obj);
    if (TYP16(obj)==tc16_bigpos) {
      obj = 0;
      while (i--) obj = BIGUP(obj) + ds[i];
    }
    else {
      obj = 0;
      while (i--) obj = BIGUP(obj) - ds[i];
    }
    obj <<= 1;
    goto comm;
  }
#endif
  ASSERT(INUMP(obj), obj, ARG1, s_obunhash);
  obj = SRS(obj, 1) & ~1L;
 comm:
  if IMP(obj) return obj;
  if NCELLP(obj) return BOOL_F;
  {				/* code is adapted from mark_locations */
    register CELLPTR ptr = (CELLPTR)SCM2PTR(obj);
    register sizet i = 0, j = hplim_ind;
    do {
      if PTR_GT(hplims[i++], ptr) break;
      if PTR_LE(hplims[--j], ptr) break;
      if ((i != j)
	  && PTR_LE(hplims[i++], ptr)
	  && PTR_GT(hplims[--j], ptr)) continue;
      if NFREEP(obj) return obj;
      break;
    } while(i<j);
  }
  return BOOL_F;
}

unsigned long strhash(str, len, n)
     unsigned char *str;
     sizet len;
     unsigned long n;
{
  if (len>5)
    {
      sizet i = 5;
      unsigned long h = 264 % n;
      while (i--) h = ((h<<8) + ((unsigned)(downcase[str[h % len]]))) % n;
      return h;
    }
  else {
    sizet i = len;
    unsigned long h = 0;
    while (i) h = ((h<<8) + ((unsigned)(downcase[str[--i]]))) % n;
    return h;
  }
}

static void fixconfig(s1, s2, s)
     char *s1, *s2;
     int s;
{
  fputs(s1, stderr);
  fputs(s2, stderr);
  fputs("\nin ", stderr);
  fputs(s ? "setjump" : "scmfig", stderr);
  fputs(".h and recompile scm\n", stderr);
  quit(MAKINUM(1L));
}

sizet init_heap_seg(seg_org, size)
     CELLPTR seg_org;
     sizet size;
{
  register CELLPTR ptr = seg_org;
#ifdef POINTERS_MUNGED
  register SCM scmptr;
#else
# define scmptr ptr
#endif
  CELLPTR seg_end = CELL_DN((char *)ptr + size);
  sizet i = hplim_ind, ni = 0;
  if (ptr==NULL) return 0;
  while((ni < hplim_ind) && PTR_LE(hplims[ni], seg_org)) ni++;
  while(i-- > ni) hplims[i+2] = hplims[i];
  hplim_ind += 2;
  hplims[ni++] = ptr;		/* same as seg_org here */
  hplims[ni++] = seg_end;
  ptr = CELL_UP(ptr);
  ni = seg_end - ptr;
  for (i = ni;i--;ptr++) {
#ifdef POINTERS_MUNGED
    scmptr = PTR2SCM(ptr);
#endif
    CAR(scmptr) = (SCM)tc_free_cell;
    CDR(scmptr) = PTR2SCM(ptr+1);
  }
/*  CDR(scmptr) = freelist; */
  CDR(PTR2SCM(--ptr)) = freelist;
  freelist = PTR2SCM(CELL_UP(seg_org));
  heap_size += ni;
  return size;
#ifdef scmptr
# undef scmptr
#endif
}
static void alloc_some_heap()
{
  CELLPTR ptr, *tmplims;
  sizet len = (2+hplim_ind)*sizeof(CELLPTR);
  ASRTGO(len==(2+hplim_ind)*sizeof(CELLPTR), badhplims);
  if (errjmp_bad) wta(UNDEFINED, "need larger initial", s_heap);
  SYSCALL(tmplims = (CELLPTR *)realloc((char *)hplims, len););
  if (!tmplims)
badhplims:
    wta(UNDEFINED, s_nogrow, s_hplims);
  else hplims = tmplims;
  /* hplim_ind gets incremented in init_heap_seg() */
  if (expmem) {
    len = (sizet)(EXPHEAP(heap_size)*sizeof(cell));
    if ((sizet)(EXPHEAP(heap_size)*sizeof(cell)) != len) len = 0;
  }
  else len = HEAP_SEG_SIZE;
  while (len >= MIN_HEAP_SEG_SIZE) {
    SYSCALL(ptr = (CELLPTR) malloc(len););
    if (ptr) {
      init_heap_seg(ptr, len);
      return;
    }
    len /= 2;
  }
  wta(UNDEFINED, s_nogrow, s_heap);
}

smobfuns *smobs;
sizet numsmob;
long newsmob(smob)
     smobfuns *smob;
{
  char *tmp;
  if (255 <= numsmob) goto smoberr;
  DEFER_INTS;
  SYSCALL(tmp = (char *)realloc((char *)smobs, (1+numsmob)*sizeof(smobfuns)););
  if (tmp) {
    smobs = (smobfuns *)tmp;
    smobs[numsmob].mark = smob->mark;
    smobs[numsmob].free = smob->free;
    smobs[numsmob].print = smob->print;
    smobs[numsmob].equalp = smob->equalp;
    numsmob++;
  }
  ALLOW_INTS;
  if (!tmp) smoberr: wta(MAKINUM((long)numsmob), (char *)NALLOC, "newsmob");
  return tc7_smob + (numsmob-1)*256;
}
ptobfuns *ptobs;
sizet numptob;
long newptob(ptob)
     ptobfuns *ptob;
{
  char *tmp;
  if (255 <= numptob) goto ptoberr;
  DEFER_INTS;
  SYSCALL(tmp = (char *)realloc((char *)ptobs, (1+numptob)*sizeof(ptobfuns)););
  if (tmp) {
    ptobs = (ptobfuns *)tmp;
    ptobs[numptob].mark = ptob->mark;
    ptobs[numptob].free = ptob->free;
    ptobs[numptob].print = ptob->print;
    ptobs[numptob].equalp = ptob->equalp;
    ptobs[numptob].fputc = ptob->fputc;
    ptobs[numptob].fputs = ptob->fputs;
    ptobs[numptob].fwrite = ptob->fwrite;
    ptobs[numptob].fflush = ptob->fflush;
    ptobs[numptob].fgetc = ptob->fgetc;
    ptobs[numptob].fclose = ptob->fclose;
    numptob++;
  }
  ALLOW_INTS;
  if (!tmp) ptoberr: wta(MAKINUM((long)numptob), (char *)NALLOC, "newptob");
  return tc7_port + (numptob-1)*256;
}
SCM markcdr(ptr)
     SCM ptr;
{
  if GC8MARKP(ptr) return BOOL_F;
  SETGC8MARK(ptr);
  return CDR(ptr);
}
SCM mark0(ptr)
     SCM ptr;
{
  SETGC8MARK(ptr);
  return BOOL_F;
}
sizet free0(ptr)
     CELLPTR ptr;
{
  return 0;
}
SCM equal0(ptr1, ptr2)
     SCM ptr1, ptr2;
{
  return (CDR(ptr1)==CDR(ptr2)) ? BOOL_T : BOOL_F;
}

/* statically allocated port for diagnostic messages */
cell tmp_errp = {(SCM)((0L<<8)|tc16_fport|OPN|WRTNG), 0};

static char remsg[] = "remove\n#define ", addmsg[] = "add\n#define ";
extern sizet num_protects;	/* sys_protects now in scl.c */
void init_storage(stack_start_ptr, init_heap_size)
     STACKITEM *stack_start_ptr;
     long init_heap_size;
{
	sizet j = num_protects;
	/* Because not all protects may get initialized */
	while(j) sys_protects[--j] = BOOL_F;
	tmp_errp.cdr = (SCM)stderr;
	cur_errp = PTR2SCM(&tmp_errp);
	freelist = EOL;
	expmem = 0;

#ifdef SHORT_INT
	if (sizeof(int) >= sizeof(long))
	  fixconfig(remsg, "SHORT_INT", 1);
#else
	if (sizeof(int) < sizeof(long))
	  fixconfig(addmsg, "SHORT_INT", 1);
#endif
#ifdef CDR_DOUBLES
	if (sizeof(double) != sizeof(long))
	  fixconfig(remsg, "CDR_DOUBLES", 0);
#else
# ifdef SINGLES
	if (sizeof(float) != sizeof(long))
	  if (sizeof(double) == sizeof(long))
	    fixconfig(addmsg, "CDR_DOUBLES", 0);
	  else
	    fixconfig(remsg, "SINGLES", 0);
# endif
#endif
#ifdef BIGDIG
	if (2*BITSPERDIG/CHAR_BIT > sizeof(long))
	  fixconfig(remsg, "BIGDIG", 0);
# ifndef DIGSTOOBIG
	if (DIGSPERLONG*sizeof(BIGDIG) > sizeof(long))
	  fixconfig(addmsg, "DIGSTOOBIG", 0);
# endif
#endif
#ifdef STACK_GROWS_UP
	if (((STACKITEM *)&j - stack_start_ptr) < 0)
	  fixconfig(remsg, "STACK_GROWS_UP", 1);
#else
	if ((stack_start_ptr - (STACKITEM *)&j) < 0)
	  fixconfig(addmsg, "STACK_GROWS_UP", 1);
#endif
	j = HEAP_SEG_SIZE;
	if (HEAP_SEG_SIZE != j)
	  fixconfig("reduce", "size of HEAP_SEG_SIZE", 0);

	mtrigger = INIT_MALLOC_LIMIT;
	hplims = (CELLPTR *) must_malloc(2L*sizeof(CELLPTR), s_hplims);
	if (0L==init_heap_size) init_heap_size = INIT_HEAP_SIZE;
	j = init_heap_size;
	if ((init_heap_size != j) || !init_heap_seg((CELLPTR) malloc(j), j)) {
	  j = HEAP_SEG_SIZE;
	  if (!init_heap_seg((CELLPTR) malloc(j), j))
	    wta(MAKINUM(j), (char *)NALLOC, s_heap);
	}
	else expmem = 1;
	heap_org = CELL_UP(hplims[0]);
		/* hplims[0] can change. do not remove heap_org */

	NEWCELL(def_inp);
	CAR(def_inp) = (tc16_fport|OPN|RDNG);
	SETSTREAM(def_inp, stdin);
	NEWCELL(def_outp);
	CAR(def_outp) = (tc16_fport|OPN|WRTNG);
	SETSTREAM(def_outp, stdout);
	NEWCELL(def_errp);
	CAR(def_errp) = (tc16_fport|OPN|WRTNG);
	SETSTREAM(def_errp, stderr);
	cur_inp = def_inp;
	cur_outp = def_outp;
	cur_errp = def_errp;
	dynwinds = EOL;
	NEWCELL(rootcont);
	SETCONT(rootcont, make_root_continuation(stack_start_ptr));
	CAR(rootcont) = tc7_contin;
	CONT(rootcont)->other.dynenv = EOL;
	CONT(rootcont)->other.parent = BOOL_F;
	stacktrace = EOL;
#ifdef CAUTIOUS
	CONT(rootcont)->other.stack_trace = EOL;
#endif
	listofnull = cons(EOL, EOL);
	undefineds = cons(UNDEFINED, EOL);
	CDR(undefineds) = undefineds;
	nullstr = makstr(0L);
	nullvect = make_vector(INUM0, UNDEFINED);
	/* NEWCELL(nullvect);
	   CAR(nullvect) = tc7_vector;
	   SETCHARS(nullvect, NULL); */
	symhash = make_vector((SCM)MAKINUM(symhash_dim), EOL);
	sysintern("most-positive-fixnum", (SCM)MAKINUM(MOST_POSITIVE_FIXNUM));
	sysintern("most-negative-fixnum", (SCM)MAKINUM(MOST_NEGATIVE_FIXNUM));
#ifdef BIGDIG
	sysintern("bignum-radix", MAKINUM(BIGRAD));
#endif
	/* flo0 is now setup in scl.c */
}

/* The way of garbage collecting which allows use of the cstack is due to */
/* Scheme In One Defun, but in C this time.

 *			  COPYRIGHT (c) 1989 BY				    *
 *	  PARADIGM ASSOCIATES INCORPORATED, CAMBRIDGE, MASSACHUSETTS.	    *
 *			   ALL RIGHTS RESERVED				    *

Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all copies
and that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Paradigm Associates
Inc not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.

PARADIGM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
PARADIGM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

gjc@paradigm.com

Paradigm Associates Inc		 Phone: 617-492-6079
29 Putnam Ave, Suite 6
Cambridge, MA 02138
*/
char s_cells[] = "cells";
SCM gc_for_newcell()
{
	SCM fl;
	DEFER_INTS;
	igc(s_cells, CONT(rootcont)->stkbse);
	ALLOW_INTS;
	if ((gc_cells_collected < MIN_GC_YIELD) || IMP(freelist)) {
	  DEFER_INTS;
	  alloc_some_heap();
	  ALLOW_INTS;
	  growth_mon("number of heaps", (long)(hplim_ind/2), "segments");
	  growth_mon(s_heap, heap_size, s_cells);
	}
	++cells_allocated;
	fl = freelist;
	freelist = CDR(fl);
	return fl;
}

static char	s_bad_type[] = "unknown type in ";
jmp_buf save_regs_gc_mark;
void mark_locations P((STACKITEM x[], sizet n));
static void mark_syms P((SCM v));
static void mark_sym_values P((SCM v));
static void sweep_symhash P((SCM v));

SCM gc()
{
  DEFER_INTS;
  igc("call", CONT(rootcont)->stkbse);
  ALLOW_INTS;
  return UNSPECIFIED;
}
void igc(what, stackbase)
     char *what;
     STACKITEM *stackbase;
{
  int j = num_protects;
  long oheap_size = heap_size;
  gc_start(what);
  ++errjmp_bad;
  /* By marking symhash first, we provide the best immunity from
     accidental references.  In order to accidentally protect a
     symbol, a pointer will have to point directly at the symbol (as
     opposed to the vector or bucket lists).  */
  mark_syms(symhash);
  /* mark_sym_values() can be called anytime after mark_syms.  */
#ifdef NO_SYM_GC
  gc_mark(symhash);
#else
  mark_sym_values(symhash);
#endif
  if (stackbase) {
    FLUSH_REGISTER_WINDOWS;
    /* This assumes that all registers are saved into the jmp_buf */
    setjmp(save_regs_gc_mark);
    mark_locations((STACKITEM *) save_regs_gc_mark,
		   (sizet) (sizeof(STACKITEM) - 1 + sizeof save_regs_gc_mark) /
		   sizeof(STACKITEM));
    {
      /* stack_len is long rather than sizet in order to guarantee that
	 &stack_len is long aligned */
#ifdef STACK_GROWS_UP
# ifdef nosve
      long stack_len = (STACKITEM *)(&stack_len) - stackbase;
# else
      long stack_len = stack_size(stackbase);
# endif
      mark_locations(stackbase, (sizet)stack_len);
#else
# ifdef nosve
      long stack_len = stackbase - (STACKITEM *)(&stack_len);
# else
      long stack_len = stack_size(stackbase);
# endif
      mark_locations((stackbase - stack_len), (sizet)stack_len);
#endif
    }
  }
  while(j--) gc_mark(sys_protects[j]);
  sweep_symhash(symhash);
  gc_sweep();
  --errjmp_bad;
  gc_end();
  if (oheap_size != heap_size) {
    ALLOW_INTS;
    growth_mon(s_heap, heap_size, s_cells);
    DEFER_INTS;
  }
}

static char s_not_free[] = "not freed";
void free_storage()
{
  DEFER_INTS;
  gc_start("free");
  ++errjmp_bad;
  cur_inp = BOOL_F; cur_outp = BOOL_F; cur_errp = PTR2SCM(&tmp_errp);
  gc_mark(def_inp);		/* don't want to close stdin */
  gc_mark(def_outp);		/* don't want to close stdout */
  gc_mark(def_errp);		/* don't want to close stderr */
  gc_sweep();
  rootcont = BOOL_F;
  while (hplim_ind) {		/* free heap segments */
    hplim_ind -= 2;
    {
      CELLPTR ptr = CELL_UP(hplims[hplim_ind]);
      sizet seg_size = CELL_DN(hplims[hplim_ind+1]) - ptr;
      heap_size -= seg_size;
      must_free((char *)hplims[hplim_ind]);
      hplims[hplim_ind] = 0;
      growth_mon(s_heap, heap_size, s_cells);
    }}
  if (heap_size) wta(MAKINUM(heap_size), s_not_free, s_heap);
  if (hplim_ind) wta((SCM)MAKINUM(hplim_ind), s_not_free, s_hplims);
  /* Not all cells get freed (see gc_mark() calls above). */
  /* if (cells_allocated) wta(MAKINUM(cells_allocated), s_not_free, "cells"); */
  /* either there is a small memory leak or I am counting wrong. */
  /* if (mallocated) wta(MAKINUM(mallocated), s_not_free, "malloc"); */
  must_free((char *)hplims);
  hplims = 0;
  must_free((char *)smobs);
  smobs = 0;
  gc_end();
  ALLOW_INTS; /* A really bad idea, but printing does it anyway. */
  exit_report();
  must_free((char *)ptobs);
  ptobs = 0;
  lmallocated = mallocated = 0;
  /* Can't do gc_end() here because it uses ptobs which have been freed */
}

void gc_mark(p)
     SCM p;
{
  register long i;
  register SCM ptr = p;
 gc_mark_loop:
  if IMP(ptr) return;
 gc_mark_nimp:
  if (NCELLP(ptr)
      /* #ifndef RECKLESS
	 || PTR_GT(hplims[0], (CELLPTR)ptr)
	 || PTR_GE((CELLPTR)ptr, hplims[hplim_ind-1])
#endif */
      ) wta(ptr, "rogue pointer in ", s_heap);
  switch TYP7(ptr) {
  case tcs_cons_nimcar:
    if GCMARKP(ptr) break;
    SETGCMARK(ptr);
    if IMP(CDR(ptr)) {		/* IMP works even with a GC mark */
      ptr = CAR(ptr);
      goto gc_mark_nimp;
    }
    gc_mark(CAR(ptr));
    ptr = GCCDR(ptr);
    goto gc_mark_nimp;
  case tcs_cons_imcar:
  case tcs_cons_gloc:
    if GCMARKP(ptr) break;
    SETGCMARK(ptr);
    ptr = GCCDR(ptr);
    goto gc_mark_loop;
  case tcs_closures:
    if GCMARKP(ptr) break;
    SETGCMARK(ptr);
    if IMP(CDR(ptr)) {
      ptr = CODE(ptr);
      goto gc_mark_nimp;
    }
    gc_mark(CODE(ptr));
    ptr = GCCDR(ptr);
    goto gc_mark_nimp;
  case tc7_vector:
#ifdef CCLO
  case tc7_cclo:
#endif
    if GC8MARKP(ptr) break;
    SETGC8MARK(ptr);
    i = LENGTH(ptr);
    if (i==0) break;
    while(--i>0) if NIMP(VELTS(ptr)[i]) gc_mark(VELTS(ptr)[i]);
    ptr = VELTS(ptr)[0];
    goto gc_mark_loop;
  case tc7_contin:
    if GC8MARKP(ptr) break;
    SETGC8MARK(ptr);
    mark_locations((STACKITEM *)VELTS(ptr),
		   (sizet)(LENGTH(ptr) +
			   (sizeof(STACKITEM) - 1 + sizeof(CONTINUATION)) /
			   sizeof(STACKITEM)));
    break;
  case tc7_bvect:
  case tc7_ivect:
  case tc7_uvect:
  case tc7_fvect:
  case tc7_dvect:
  case tc7_cvect:
  case tc7_string:
  case tc7_msymbol:
  case tc7_ssymbol:
    SETGC8MARK(ptr);
  case tcs_subrs:
    break;
  case tc7_port:
    i = PTOBNUM(ptr);
    if (!(i < numptob)) goto def;
    ptr = (ptobs[i].mark)(ptr);
    goto gc_mark_loop;
  case tc7_smob:
    if GC8MARKP(ptr) break;
    switch TYP16(ptr) {		/* should be faster than going through smobs */
    case tc_free_cell:
      /* printf("found free_cell %X ", ptr); fflush(stdout); */
      SETGC8MARK(ptr);
      CDR(ptr) = EOL;
      break;
    case tcs_bignums:
    case tc16_flo:
      SETGC8MARK(ptr);
      break;
    default:
      i = SMOBNUM(ptr);
      if (!(i < numsmob)) goto def;
      ptr = (smobs[i].mark)(ptr);
      goto gc_mark_loop;
    }
    break;
  default: def: wta(ptr, s_bad_type, "gc_mark");
  }
}

void mark_locations(x, n)
     STACKITEM x[];
     sizet n;
{
	register long m = n;
	register int i, j;
	register CELLPTR ptr;
	while(0 <= --m) if CELLP(*(SCM **)&x[m]) {
		ptr = (CELLPTR)SCM2PTR((*(SCM **)&x[m]));
		i = 0;
		j = hplim_ind;
		do {
			if PTR_GT(hplims[i++], ptr) break;
			if PTR_LE(hplims[--j], ptr) break;
			if ((i != j)
			    && PTR_LE(hplims[i++], ptr)
			    && PTR_GT(hplims[--j], ptr)) continue;
			/* if NFREEP(*(SCM **)&x[m]) */ gc_mark(*(SCM *)&x[m]);
			break;
		} while(i<j);
	}
}

#define HUGE_LENGTH(x) (LENGTH_MAX==LENGTH(x) ? *((long *)VELTS(x)) : LENGTH(x))

static void gc_sweep()
{
  register CELLPTR ptr;
#ifdef POINTERS_MUNGED
  register SCM scmptr;
#else
#define scmptr (SCM)ptr
#endif
  register SCM nfreelist = EOL;
  register long n = 0, m = 0;
  register sizet j;
  sizet i = 0;
  sizet seg_size;
  while (i<hplim_ind) {
    ptr = CELL_UP(hplims[i++]);
    seg_size = CELL_DN(hplims[i++]) - ptr;
    for(j = seg_size;j--;++ptr) {
#ifdef POINTERS_MUNGED
      scmptr = PTR2SCM(ptr);
#endif
      switch TYP7(scmptr) {
      case tcs_cons_imcar:
      case tcs_cons_nimcar:
      case tcs_cons_gloc:
      case tcs_closures:
	if GCMARKP(scmptr) goto cmrkcontinue;
	break;
      case tc7_vector:
#ifdef CCLO
      case tc7_cclo:
#endif
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += (LENGTH(scmptr)*sizeof(SCM));
      freechars:
	must_free(CHARS(scmptr));
/*	SETCHARS(scmptr, 0);*/
	break;
      case tc7_bvect:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += sizeof(long)*((HUGE_LENGTH(scmptr)+LONG_BIT-1)/LONG_BIT);
	goto freechars;
      case tc7_ivect:
      case tc7_uvect:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += HUGE_LENGTH(scmptr)*sizeof(long);
	goto freechars;
      case tc7_fvect:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += HUGE_LENGTH(scmptr)*sizeof(float);
	goto freechars;
      case tc7_dvect:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += HUGE_LENGTH(scmptr)*sizeof(double);
	goto freechars;
      case tc7_cvect:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += HUGE_LENGTH(scmptr)*2*sizeof(double);
	goto freechars;
      case tc7_string:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += HUGE_LENGTH(scmptr)+1;
	goto freechars;
      case tc7_msymbol:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += LENGTH(scmptr)+1;
	goto freechars;
      case tc7_contin:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	m += LENGTH(scmptr)*sizeof(STACKITEM) + sizeof(CONTINUATION);
/*	free_continuation(CONT(scmptr)); */
	goto freechars;
      case tc7_ssymbol:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	/* Do not free storage because tc7_ssymbol means scmptr's
           storage was not created by a call to malloc(). */
	break;
      case tcs_subrs:
	continue;
      case tc7_port:
	if GC8MARKP(scmptr) goto c8mrkcontinue;
	if OPENP(scmptr) {
	  int k = PTOBNUM(scmptr);
	  if (!(k < numptob)) goto sweeperr;
				/* Yes, I really do mean ptobs[k].free */
				/* rather than ftobs[k].close.  .close */
				/* is for explicit CLOSE-PORT by user */
	  (ptobs[k].free)(STREAM(scmptr));
	  gc_ports_collected++;
	  SETSTREAM(scmptr, 0);
	  CAR(scmptr) &= ~OPN;
	}
	break;
      case tc7_smob:
	switch GCTYP16(scmptr) {
	case tc_free_cell:
	  if GC8MARKP(scmptr) goto c8mrkcontinue;
	  break;
#ifdef BIGDIG
	case tcs_bignums:
	  if GC8MARKP(scmptr) goto c8mrkcontinue;
	  m += (NUMDIGS(scmptr)*BITSPERDIG/CHAR_BIT);
	  goto freechars;
#endif /* def BIGDIG */
	case tc16_flo:
	  if GC8MARKP(scmptr) goto c8mrkcontinue;
	  switch ((int)(CAR(scmptr)>>16)) {
	  case (IMAG_PART | REAL_PART)>>16:
	    m += sizeof(double);
	  case REAL_PART>>16:
	  case IMAG_PART>>16:
	    m += sizeof(double);
	    goto freechars;
	  case 0:
	    break;
	  default:
	    goto sweeperr;
	  }
	  break;
	default:
	  if GC8MARKP(scmptr) goto c8mrkcontinue;
	  {
	    int k = SMOBNUM(scmptr);
	    if (!(k < numsmob)) goto sweeperr;
	    m += (smobs[k].free)((CELLPTR)scmptr);
	  }
	}
	break;
      default: sweeperr: wta(scmptr, s_bad_type, "gc_sweep");
      }
      ++n;
      CAR(scmptr) = (SCM)tc_free_cell;
      CDR(scmptr) = nfreelist;
      nfreelist = scmptr;
      continue;
    c8mrkcontinue:
      CLRGC8MARK(scmptr);
      continue;
    cmrkcontinue:
      CLRGCMARK(scmptr);
    }
#ifdef GC_FREE_SEGMENTS
    if (n==seg_size) {
      heap_size -= seg_size;
      must_free((char *)hplims[i-2]);
      hplims[i-2] = 0;
      for(j = i;j < hplim_ind;j++) hplims[j-2] = hplims[j];
      hplim_ind -= 2;
      i -= 2;			/* need to scan segment just moved. */
      nfreelist = freelist;
    }
    else
#endif /* ifdef GC_FREE_SEGMENTS */
	freelist = nfreelist;
    gc_cells_collected += n;
    n = 0;
  }
  lcells_allocated += (heap_size - gc_cells_collected - cells_allocated);
  cells_allocated = (heap_size - gc_cells_collected);
  lmallocated -= m;
  mallocated -= m;
  gc_malloc_collected = m;
}

/* mark_syms marks those symbols of hash table V which have
   non-UNDEFINED values.  */
static char s_gc_sym[] = "mark_syms";
static void mark_syms(v)
     SCM v;
{
  SCM x, al;
  int k = LENGTH(v);
  while (k--)
    for (al = VELTS(v)[k]; NIMP(al); al = GCCDR(al)) {
      /* If this bucket has already been marked, then something is wrong.  */
      ASSERT(!GCMARKP(al), al, s_bad_type, s_gc_sym);
      x = CAR(al);
      SETGCMARK(al);
      ASSERT(!GCMARKP(x), x, s_bad_type, s_gc_sym);
      if (UNDEFINED==CDR(x) && tc7_msymbol==TYP7(CAR(x)))
	goto used;		/* Don't mark symbol.  */
      SETGC8MARK(CAR(x));
    used:
      SETGCMARK(x);		/* Do mark value cell.  */
    }
  SETGC8MARK(v);		/* Mark bucket list.  */
}

/* mark_symhash marks the values of hash table V.  */
static void mark_sym_values(v)
     SCM v;
{
  SCM x, al;
  int k = LENGTH(v);
  SETGC8MARK(v);
  while (k--)
    for (al = VELTS(v)[k]; NIMP(al); al = GCCDR(al)) {
      x = GCCDR(CAR(al));
      if IMP(x) continue;
      gc_mark(x);
    }
}

/* Splice any unused valueless symbols out of the hash buckets. */
static void sweep_symhash(v)
     SCM v;
{
  SCM al, x, *lloc;
  int k = LENGTH(v);
  while (k--) {
    lloc = &(VELTS(v)[k]);
    while NIMP(al = (*lloc & ~1L)) {
      x = CAR(al);
      if GC8MARKP(CAR(x))
	lloc = &(CDR(al));
      else {
	*lloc = CDR(al);
	CLRGCMARK(al);		/* bucket pair to be collected by gc_sweep */
	CLRGCMARK(x);		/* value cell to be collected by gc_sweep */
	gc_syms_collected++;
      }
    }
    VELTS(v)[k] &= ~1L;		/* We may have deleted the first cell */
  }
}
