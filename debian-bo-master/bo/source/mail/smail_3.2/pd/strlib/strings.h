/* @(#) strings.h,v 1.2 1990/10/24 05:19:29 tron Exp */
/*
 * File   : strings.h
 * Author : Richard A. O'Keefe.
 * Updated: 1 June 1984
 * Purpose: Header file for the "string(3C)" package.
 * 
 * All  the  routines  in  this  package  are  the  original  work   of
 * R.A.O'Keefe.   Any  resemblance  between  them  and  any routines in
 * licensed software is due entirely  to  these  routines  having  been
 * written  using the "man 3 string" UNIX manual page, or in some cases
 * the "man 1 sort" manual page as a specification.  See the README to
 * find the conditions under which these routines may be used & copied.
 */

#ifndef	NullS
#define	NullS	(char*)0
#define NUL	'\0'

/*  MAKE SURE THE RIGHT VERSION OF THE FOLLOWING MACRO IS INSTALLED!  */

#ifndef M_XENIX
# if '\377' < 0
# define CharsAreSigned 1			/* default is unsigned */
# endif
#else
# define CharsAreSigned 1 /* force this for MS Xenix -- foriley@masa.com */
#endif

#if	CharsAreSigned
#define	int2char(i)	(((i)<<((sizeof (int) -1)*8))>>((sizeof (int) -1)*8))
#else
#define	int2char(i)	((i)&255)
#endif
/*  If characters are signed, but the above doesn't work,
    try	((127-(255&~(i)))^(-128))
*/

#ifndef	_AlphabetSize
#define	_AlphabetSize	128
#endif

#if	_AlphabetSize == 128
typedef	char _char_;
#endif
#if	_AlphabetSize == 256
typedef	unsigned char _char_;
#endif

/*
 * NullS is the "nil" character  pointer.   NULL  would  work  in  most
 * cases,  but  in  some  C  compilers  pointers and integers may be of
 * different sizes, so it is handy to have a nil pointer that  one  can
 * pass to a function as well as compare pointers against.
 * 
 * NUL is the ASCII name for the character with code 0.  Its use to end
 * strings is a convention of the C programming language.  There are in
 * fact three different end of string conventions supported by routines
 * in this package:
 *      str<opn>	: end at the first NUL character
 *	strn<opn>	: end at the first NUL character, or when the
 *			  extra "len" parameter runs out.
 *	mem<opn>,b<opn>	: length determined solely by "len" parameter.
 * Unfortunately, the VAX hardware only supports the last convention, a
 * pity really.  Fortran 77 users BEWARE: Fortran 77's convention is an
 * entirely different one, and there are NO routines in this package as
 * yet which support it.  (But see section 3F of the 4.2 BSD manual.)
 * 
 * The routines which move characters around don't  care  whether  they
 * are  signed or unsigned.  But the routines which compare a character
 * in a string with an argument, or use a character from a string as an
 * index into an array, do care.  I have assumed that
 *	_AlphabetSize = 128 => only 0..127 appear in strings
 *	_AlphabetSize = 256 => only 0..255 appear in strings
 * The files _str2set.c and _str2map.c declare character vectors  using
 * this  size.  If you don't have unsigned char, your machine may treat
 * char as unsigned anyway.
 * 
 * Some string operations (*cmp, *chr) are explicitly defined in various
 * UNIX manuals to use "native" comparison, so I have not used _char_ in
 * them.  This package is meant to be compatible, not rational!
 */

extern	char	*strcat(/*char^,char^*/);
extern	char	*strncat(/*char^,char^,int*/);

extern	int	strcmp(/*char^,char^*/);
extern	int	strncmp(/*char^,char^,int*/);

#define streql	!strcmp
#define strneql	!strncmp	/* (str-N)-eql not str-(neq-l)! */

extern	char	*strcpy(/*char^,char^*/);
extern	char	*strncpy(/*char^,char^,int*/);

extern	int	strlen(/*char^*/);
extern	int	strnlen(/*char^,int*/);

extern	char	*strchr(/*char^,_char_*/);
extern	char	*strrchr(/*char^,_char_*/);
#define	index	strchr
#define	rindex	strrchr

extern	char	*strmov(/*char^,char^*/);
extern	char	*strnmov(/*char^,char^,int*/);

extern	void	strrev(/*char^,char^*/);
extern	void	strnrev(/*char^,char^,int*/);

extern	char	*strend(/*char^*/);
extern	char	*strnend(/*char^*/);

extern	char	*strpbrk(/*char^,char^*/);
extern	char	*strcpbrk(/*char^,char^*/);

extern	int	strspn(/*char^,char^*/);
extern	int	strcspn(/*char^,char^*/);

extern	char	*strtok(/*char^,char^*/);
extern	void	istrtok(/*char^,char^*/);

extern	char	*strpack(/*_char_^,_char_^,char^,int*/);
extern	char	*strcpack(/*_char_^,_char_^,char^,int*/);

extern	int	strrpt(/*char^,char^,int*/);
extern	int	strnrpt(/*char^,int,char^,int*/);

extern	void	strtrans(/*_char_^,_char_^,_char_^,_char_^*/);
extern	void	strntrans(/*_char_^,_char_^,int,_char_^,_char_^*/);

extern	char	*strtrim(/*char^,char^,char^,int*/);
extern	char	*strctrim(/*char^,char^,char^,int*/);

extern	char	*strfield(/*char^,int,int,int,int*/);
extern	char	*strkey(/*char^,char^,char^,char^*/);

extern	char	*strfind(/*char^,char^*/);
extern	char	*strrepl(/*char^,char^,char^,char^*/);

extern	void	bcopy(/*char^,char^,int*/);
extern	void	bmove(/*char^,char^,int*/);

extern	void	bfill(/*char^,int,char*/);
extern	void	bzero(/*char^,int*/);

extern	int	bcmp(/*char^,char^,int*/);
#define	beql	!bcmp

extern	int	ffs(/*int*/);
extern	int	ffc(/*int*/);

extern	char	*substr(/*char^,char^,int,int*/);

extern	char	*strxcat(/*VARARGS*/);
extern	char	*strxcpy(/*VARARGS*/);
extern	char	*strxmov(/*VARARGS*/);

extern	char	*strxncat(/*VARARGS*/);
extern	char	*strxncpy(/*VARARGS*/);
extern	char	*strxnmov(/*VARARGS*/);

#endif	/* NullS */
