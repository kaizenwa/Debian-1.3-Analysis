/* -*-C-*-
********************************************************************************
*
* File:         compresslib.c
* RCS:          $Header: compresslib.c,v 1.2 89/09/14 20:34:00 chip Rel $
* Description:  Embedded "decompress" routine.
* Author:       Chip Chapin, Hewlett Packard Company
* Created:      Sat May 27 14:53:55 1989
* Modified:     Thu Sep 14 12:37:36 1989 (Chip Chapin) chip@hpcllcc
* Language:     C
* Package:      Bible Retrieval System
* Status:       Experimental (Do Not Distribute)
*
* $Log:	compresslib.c,v $
 * Revision 1.2  89/09/14  20:34:00  20:34:00  chip (Chip Chapin)
 * Release 1-2.  Supports -f and -l options for formatting the output.
 * Updates primarily brl.c, bible.c, and bible.1.
 * 
 * Revision 1.1  89/09/05  17:49:30  17:49:30  chip (Chip Chapin)
 * Initial revision
 * 
*
********************************************************************************
*/

/*----------------------------------------------------------------------
|   NAME:
|       compresslib.c
|
|   PURPOSE:
|       Provides a programmatic interface to Lempel-Ziv-Welch
|       uncompression.  Code is entirely swiped from the
|       "compress" utility, the authors of which are:
|	Spencer W. Thomas, Jim McKie, Steve Davies, Ken
|	Turkowski, James A. Woods, and Joe Orost.
|
|   FUNCTIONS:
|       cmp_init	Initialize the library.
|       cmp_decompress	Decompress a buffer.
|
|   HISTORY:
|       890527 cc Created.
|
\*----------------------------------------------------------------------*/



#define	min(a,b)	((a>b) ? b : a)

/*
 * machine variants which require cc -Dmachine:  pdp11, z8000, pcxt
 */

/*
 * Set USERMEM to the maximum amount of physical user memory available
 * in bytes.  USERMEM is used to determine the maximum BITS that can be used
 * for compression.
 *
 * SACREDMEM is the amount of physical memory saved for others; compress
 * will hog the rest.
 */
#ifndef SACREDMEM
#define SACREDMEM	0
#endif

#ifndef USERMEM
# define USERMEM 	450000	/* default user memory */
#endif

#ifdef interdata		/* (Perkin-Elmer) */
#define SIGNED_COMPARE_SLOW	/* signed compare is slower than unsigned */
#endif

#ifdef pdp11
# define BITS 	12	/* max bits/code for 16-bit machine */
# define NO_UCHAR	/* also if "unsigned char" functions as signed char */
# undef USERMEM 
#endif /* pdp11 */	/* don't forget to compile with -i */

#ifdef z8000
# define BITS 	12
# undef vax		/* weird preprocessor */
# undef USERMEM 
#endif /* z8000 */

#ifdef pcxt
# define BITS   12
# undef USERMEM
#endif /* pcxt */

#ifdef USERMEM
# if USERMEM >= (433484+SACREDMEM)
#  define PBITS	16
# else
#  if USERMEM >= (229600+SACREDMEM)
#   define PBITS	15
#  else
#   if USERMEM >= (127536+SACREDMEM)
#    define PBITS	14
#   else
#    if USERMEM >= (73464+SACREDMEM)
#     define PBITS	13
#    else
#     define PBITS	12
#    endif
#   endif
#  endif
# endif
# undef USERMEM
#endif /* USERMEM */

#ifdef PBITS		/* Preferred BITS for this memory size */
# ifndef BITS
#  define BITS PBITS
# endif BITS
#endif /* PBITS */

#if BITS == 16
# define HSIZE	69001		/* 95% occupancy */
#endif
#if BITS == 15
# define HSIZE	35023		/* 94% occupancy */
#endif
#if BITS == 14
# define HSIZE	18013		/* 91% occupancy */
#endif
#if BITS == 13
# define HSIZE	9001		/* 91% occupancy */
#endif
#if BITS <= 12
# define HSIZE	5003		/* 80% occupancy */
#endif

#ifdef M_XENIX			/* Stupid compiler can't handle arrays with */
# if BITS == 16			/* more than 65535 bytes - so we fake it */
#  define XENIX_16
# else
#  if BITS > 13			/* Code only handles BITS = 12, 13, or 16 */
#   define BITS	13
#  endif
# endif
#endif

/*
 * a code_int must be able to hold 2**BITS values of type int, and also -1
 */
#if BITS > 15
typedef long int	code_int;
#else
typedef int		code_int;
#endif

#ifdef SIGNED_COMPARE_SLOW
typedef unsigned long int count_int;
typedef unsigned short int count_short;
#else
typedef long int	  count_int;
#endif

#ifdef NO_UCHAR
 typedef char	char_type;
#else
 typedef	unsigned char	char_type;
#endif /* UCHAR */
char_type magic_header[] = { "\037\235" };	/* 1F 9D */

/* Defines for third byte of header */
#define BIT_MASK	0x1f
#define BLOCK_MASK	0x80
/* Masks 0x40 and 0x20 are free.  I think 0x20 should mean that there is
   a fourth header byte (for expansion).
*/
#define INIT_BITS 9			/* initial number of bits/code */


static char rcs_ident[]="@(#)$Header: compresslib.c,v 1.2 89/09/14 20:34:00 chip Rel $";

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>

#define ARGVAL() (*++(*argv) || (--argc && *++argv))

int n_bits;				/* number of bits/code */
int maxbits = BITS;			/* user settable max # bits/code */
code_int maxcode;			/* maximum code, given n_bits */
code_int maxmaxcode = 1 << BITS;	/* should NEVER generate this code */
#ifdef COMPATIBLE		/* But wrong! */
# define MAXCODE(n_bits)	(1 << (n_bits) - 1)
#else
# define MAXCODE(n_bits)	((1 << (n_bits)) - 1)
#endif /* COMPATIBLE */

#ifdef XENIX_16
count_int htab0[8192];
count_int htab1[8192];
count_int htab2[8192];
count_int htab3[8192];
count_int htab4[8192];
count_int htab5[8192];
count_int htab6[8192];
count_int htab7[8192];
count_int htab8[HSIZE-65536];
count_int * htab[9] = {
	htab0, htab1, htab2, htab3, htab4, htab5, htab6, htab7, htab8 };

#define htabof(i)	(htab[(i) >> 13][(i) & 0x1fff])
unsigned short code0tab[16384];
unsigned short code1tab[16384];
unsigned short code2tab[16384];
unsigned short code3tab[16384];
unsigned short code4tab[16384];
unsigned short * codetab[5] = {
	code0tab, code1tab, code2tab, code3tab, code4tab };

#define codetabof(i)	(codetab[(i) >> 14][(i) & 0x3fff])

#else	/* Normal machine */
count_int htab [HSIZE];
unsigned short codetab [HSIZE];
#define htabof(i)	htab[i]
#define codetabof(i)	codetab[i]
#endif	/* XENIX_16 */
code_int hsize = HSIZE;			/* for dynamic table sizing */

#ifndef vax
char_type lmask[9] = {0xff, 0xfe, 0xfc, 0xf8, 0xf0, 0xe0, 0xc0, 0x80, 0x00};
char_type rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};
#endif /* vax */


/*
 * To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**BITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab_prefixof(i)	codetabof(i)
#ifdef XENIX_16
# define tab_suffixof(i)	((char_type *)htab[(i)>>15])[(i) & 0x7fff]
# define de_stack		((char_type *)(htab2))
#else	/* Normal machine */
# define tab_suffixof(i)	((char_type *)(htab))[i]
# define de_stack		((char_type *)&tab_suffixof(1<<BITS))
#endif	/* XENIX_16 */

code_int free_ent = 0;			/* first unused entry */

code_int getcode();

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
int block_compress = BLOCK_MASK;
int clear_flg = 0;

/*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 */ 
#define FIRST	257	/* first free entry */
#define	CLEAR	256	/* table clear output code */



cmp_init(  )
/*----------------------------------------------------------------------
|   NAME:
|       cmp_init
|
|   ALGORITHM:
|       Initialize state required by the decompress
|       routines.  This replaces the "main" of the original
|       compress program.
|
|   HISTORY:
|       Stripped from "main", 890825 cc
|
\*----------------------------------------------------------------------*/
{
    /* Initialize global variables.
       In the original compress, these are just initialized statically.
       We're doing it dynamically so that the package can be reinitialized.
       */
    maxbits = BITS;			/* user settable max # bits/code */
    maxmaxcode = 1 << BITS;	/* should NEVER generate this code */
    hsize = HSIZE;			/* for dynamic table sizing */
    free_ent = 0;			/* first unused entry */

    /* Argument Processing
     * All flags are optional.
     * -D => debug
     * -V => print Version; debug verbose
     * -d => do_decomp
     * -v => unquiet
     * -f => force overwrite of output file
     * -n => no header: useful to uncompress old files
     * -b maxbits => maxbits.  If -b is specified, then maxbits MUST be
     *	    given also.
     * -c => cat all output to stdout
     * -C => generate output compatible with compress 2.0.
     *
     * (890527) -w window-size => sync the output with "windows" of fixed
     *		size in the input file.  If you then keep track of where each
     *		compressed window starts in the output file, you can
     *		start decompressing at a window boundary instead of having
     *		decompress the entire file.
     *
     * if a string is left, must be an input filename.
     */
#ifdef DEBUG
    /* -D debug = 1; */
    /* -V verbose = 1;
       version(); */
#endif /* DEBUG */
    /* -v quiet = 0; */
    /* -d do_decomp = 1; */
    /* -f overwrite = 1;
       force = 1; */
    /* -n nomagic = 1; */
    /* -C block_compress = 0; */
    /* -b maxbits = atoi(*argv); */
    /* -c zcat_flg = 1; */
    /* -q quiet = 1; */
    /* -w piecesize = atoi(*argv); */

    if(maxbits < INIT_BITS) maxbits = INIT_BITS;
    if (maxbits > BITS) maxbits = BITS;
    maxmaxcode = 1 << maxbits;
} /* cmp_init */



/* Does NOT update input buffer pointer */
cmp_checkheader(inb)
char_type *inb;
{
    if ((*inb++ != (magic_header[0] & 0xFF))
	|| (*inb++ != (magic_header[1] & 0xFF))) {
	fprintf(stderr, "not in compressed format\n");
	return 0;
    }
    maxbits = *inb++;	/* set -b from file */
    block_compress = maxbits & BLOCK_MASK;
    maxbits &= BIT_MASK;
    maxmaxcode = 1 << maxbits;
    if(maxbits > BITS) {
	fprintf(stderr,
		"compressed with %d bits, can only handle %d bits\n",
		maxbits, BITS);
	return 0;
    }
    return 1;
}




cmp_decompress( inb, outb, insize )
/*----------------------------------------------------------------------
|   NAME:
|       cmp_decompress
|
|   ALGORITHM:
|       Decompress from buffer to buffer.  This routine adapts to
|       the codes in the file building the "string" table
|       on-the-fly; requiring no table to be stored in the
|       compressed file.  The tables used herein are shared with
|       those of the compress() routine.  See the definitions
|       above.
|       
|       We assume that the input buffer points to the start of a
|       block of compressed text, past any magic number or other
|       header info.
|       
|       inb	Pointer to input buffer.
|       outb	Pointer to output buffer.
|       insize	Size (bytes) of input data.
|       
|       Note that *no* checking is performed regarding the size of
|       the output data.  The calling program is responsible for
|       providing a large enough output buffer.
|       
|       Returns the number of bytes of decompressed text.
|
|   HISTORY:
|       890825 cc Derived from the original "decompress", which
|       	worked from stdin to stdout.
|
\*----------------------------------------------------------------------*/

char_type *inb;
char_type *outb;
int insize;
{
    register char_type *stackp;
    register int finchar;
    register code_int code, oldcode, incode;
    char_type *eofchar, *outbstart;

    /*
     * As above, initialize the first 256 entries in the table.
     */
    maxcode = MAXCODE(n_bits = INIT_BITS);
    for ( code = 255; code >= 0; code-- ) {
	tab_prefixof(code) = 0;
	tab_suffixof(code) = (char_type)code;
    }
    free_ent = ((block_compress) ? FIRST : 256 );

    outbstart = outb;		/* Remember where outb started */
    eofchar = inb + insize;	/* Point just past last char in input buffer */

    finchar = oldcode = getcode(&inb, eofchar);
    if(oldcode == -1)	/* EOF already? */
	return 0;			/* Get out of here */
    *outb++ = (char)finchar;		/* first code must be 8 bits = char */
    /* putchar( (char)finchar );	/* first code must be 8 bits = char */
    stackp = de_stack;

    while ( (code = getcode(&inb, eofchar)) > -1 ) {

	if ( (code == CLEAR) && block_compress ) {
	    for ( code = 255; code >= 0; code-- )
		tab_prefixof(code) = 0;
	    clear_flg = 1;
	    free_ent = FIRST - 1;
	    if ( (code = getcode (&inb, eofchar)) == -1 )/*O, untimely death!*/
		break;
	}
	incode = code;
	/*
	 * Special case for KwKwK string.
	 */
	if ( code >= free_ent ) {
            *stackp++ = finchar;
	    code = oldcode;
	}

	/*
	 * Generate output characters in reverse order
	 */
#ifdef SIGNED_COMPARE_SLOW
	while ( ((unsigned long)code) >= ((unsigned long)256) ) 
#else
	while ( code >= 256 ) 
#endif
	{
	    *stackp++ = tab_suffixof(code);
	    code = tab_prefixof(code);
	}
	*stackp++ = finchar = tab_suffixof(code);

	/*
	 * And put them out in forward order
	 */
	do
	    *outb++ =  *--stackp; /* putchar ( *--stackp ); */
	while ( stackp > de_stack );

	/*
	 * Generate the new entry.
	 */
	if ( (code=free_ent) < maxmaxcode ) {
	    tab_prefixof(code) = (unsigned short)oldcode;
	    tab_suffixof(code) = finchar;
	    free_ent = code+1;
	} 
	/*
	 * Remember previous code.
	 */
	oldcode = incode;
    } /* while */

    return outb - outbstart;
}



code_int getcode(inbp, eofp)
/*----------------------------------------------------------------------
|   NAME:
|       getcode
|
|   ALGORITHM:
|       Read one code from the input buffer.  If EOF, return -1.
|       Inputs:
|       	inbp	Pointer to the buffer pointer.
|       	eofp	Pointer just past last char in input buffer.
|       Outputs:
|        	code or -1 is returned.
|
|   HISTORY:
|       890825 cc Derived from the original getcode(), which read
|       	from stdin.
|
\*----------------------------------------------------------------------*/
char_type **inbp;
char_type *eofp;
{
    /*
     * On the VAX, it is important to have the register declarations
     * in exactly the order given, or the asm will break.
     *
     * This is also true for the 68020 asm code.
     */
    register code_int code;
    static int offset = 0, size = 0;
    static char_type buf[BITS];
    register int r_off, bits;
    register char_type *bp = buf;

    if ( clear_flg > 0 || offset >= size || free_ent > maxcode ) {
	/*
	 * If the next entry will be too big for the current code
	 * size, then we must increase the size.  This implies reading
	 * a new buffer full, too.
	 */
	if ( free_ent > maxcode ) {
	    n_bits++;
	    if ( n_bits == maxbits )
		maxcode = maxmaxcode;	/* won't get any bigger now */
	    else
		maxcode = MAXCODE(n_bits);
	}
	if ( clear_flg > 0) {
    	    maxcode = MAXCODE (n_bits = INIT_BITS);
	    clear_flg = 0;
	}
	
	/* 890825 cc Replaced fread with buffer read:
	   size = fread( buf, 1, n_bits, stdin );
	   */
	size = n_bits;
	if (*inbp + size > eofp)
	    size = eofp - *inbp;
	if ( size <= 0 )
	    return -1;
	memcpy(buf, *inbp, size);
	*inbp += size;

	offset = 0;
	/* Round size down to integral number of codes */
	size = (size << 3) - (n_bits - 1);
    }
    r_off = offset;
    bits = n_bits;
#ifdef vax
    asm( "extzv   r10,r9,(r8),r11" );
#else /* not a vax */
#ifdef MC68020
	/*
	 * MC68020 DEPENDENT!!
	 * This code mimics the "#ifndef vax" code below, because the
	 * 68020 'bfextu' instruction is *not* the same as the vax 'extzv'.
	 */
	{
	/* register code_int code; 	/* d7 */
    	/* register int r_off = offset, bits= n_bits;	/* d6,d5 */
		register tmp;		/* d4 */

		bp += (r_off >> 3);
		r_off &= 7;
		/* Get first part (low order bits) */
		code = *bp++; code >>= r_off;
		tmp = 8; tmp -= r_off;
		bits -= tmp;
		r_off = tmp;			/* now, offset into code word */
		/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
		if ( bits > 8 ) {
			tmp = *bp++; tmp <<= r_off; code |= tmp;
		    	r_off += 8;
		    	bits -= 8;
		}
		/* high order bits. */
		tmp = 8; tmp -= bits;
		asm("	bfextu	(%a5){%d4:%d5},%d4");
		tmp <<= r_off;
		code |= tmp;
	}
#else /* not 68020 */
	/*
	 * Get to the first byte.
	 */
	bp += (r_off >> 3);
	r_off &= 7;
	/* Get first part (low order bits) */
#ifdef NO_UCHAR
	code = ((*bp++ >> r_off) & rmask[8 - r_off]) & 0xff;
#else
	code = (*bp++ >> r_off);
#endif /* NO_UCHAR */
	bits -= (8 - r_off);
	r_off = 8 - r_off;		/* now, offset into code word */
	/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
	if ( bits >= 8 ) {
#ifdef NO_UCHAR
	    code |= (*bp++ & 0xff) << r_off;
#else
	    code |= *bp++ << r_off;
#endif /* NO_UCHAR */
	    r_off += 8;
	    bits -= 8;
	}
	/* high order bits. */
	code |= (*bp & rmask[bits]) << r_off;
#endif /* MC68020 */
#endif /* vax */
    offset += n_bits;

    return code;
}
