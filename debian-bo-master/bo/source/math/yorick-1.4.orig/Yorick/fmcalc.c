/*
    FMCALC.C

    FMCALC is a stand-alone code which calculates the the binary
    encoding of six primitive data types on a wide variety of
    machine/compiler combinations.  The six primitives are:
       char  short  int  long  float  double
    The size and alignment of generic pointers is also computed.

    FMCALC makes the following assumptions:
    (1) The char is always and 8-bit byte.
    (2) The sizes of all data types are multiples of sizeof(char).
    (3) The order of the bytes can be described in terms of a
        word size (possibly more than one byte) in which the
	bytes are laid out either from most-to-least or least-to-most
	significant, and the words are also either most-to-least or
	least-to-most ordered.  (The VAX floating point formats are
	the only known examples of opposite ordering between words
	within the object and bytes within the words.)
    (4) The floating point format consists of an overall sign bit,
        a group of 31 or fewer contiguous exponent bits from which
	a fixed bias value can be subtracted to get the binary exponent,
        and contiguous mantissa bits.  The leading 1 bit on mantissas
	may be explicit or implicit.  Floating point zero must be
	represented as all bytes 0.
    The only obvious machines that this excludes are those which use
    hex exponents instead of binary exponents.

    The ANSI C standard requires:
      sizeof(char)  == 1
      sizeof(short) >= 2
      sizeof(int)   >= 2
      sizeof(long)  >= 4
      float exponent >= 8-bits    (1.0e-37 to 1.0e37 representable)
      float mantissa >= 18-bits   (1.0+1.0e-5 distinct from 1.0)
         --> sizeof(float) >= 4
      double exponent >= 8-bits   (1.0e-37 to 1.0e37 representable)
      double mantissa >= 31-bits  (1.0+1.0e-9 distinct from 1.0)
         --> sizeof(double) >= 5

    $Id: fmcalc.c,v 1.1 1993/08/27 18:32:09 munro Exp munro $
*/
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

/* If you want "float" or "double" to mean something else, change these.  */
#define FLOAT float
#define DOUBLE double

/*--------------------------------------------------------------------------*/

#define T_CHAR 0
#define T_SHORT 1
#define T_INT 2
#define T_LONG 3
#define T_FLOAT 4
#define T_DOUBLE 5
#define T_POINTER 6

/*--------------------------------------------------------------------------*/

/* Each of the primitive data types has a size (in bytes).  */
int size[7]= {
  sizeof(char), sizeof(short), sizeof(int), sizeof(long),
  sizeof(FLOAT), sizeof(DOUBLE), sizeof(char *) };

/*--------------------------------------------------------------------------*/

/* The following data structures exhibit the definition of "alignment": */
struct CharAlign { char x; char y[1]; };
struct ShortAlign { char x; short y[1]; };
struct IntAlign { char x; int y[1]; };
struct LongAlign { char x; long y[1]; };
struct FloatAlign { char x; FLOAT y[1]; };
struct DoubleAlign { char x; DOUBLE y[1]; };
struct PointerAlign { char x; char *y[1]; };
struct StructAlign { char x; struct { char x; } y[1]; };

/* Since this has failed at least once (scc/UNICOS), just set it.  */
#define BAD_ALIGNOF

#ifndef BAD_ALIGNOF
/* The basic data type alignments can be determined at compile time
   if the offsetof macro (ANSI standard in <stddef.h>) works.  This
   definition is based on the ANSI standard offsetof in <stddef.h>.  */
#define alignof(XxAlign)  ((int)(((struct XxAlign*)0)->y))

#else
/* Must use the runtime fallback technique for computing alignments.  */
#define alignof(XxAlign) 0
#endif

/* Each of the primitive data types has an alignment (in bytes).
   The offset of a structure member from the beginning of the structure
   is constrained to be a multiple of this alignment.  */
int alignment[7]= {
  alignof(CharAlign), alignof(ShortAlign),
  alignof(IntAlign), alignof(LongAlign),
  alignof(FloatAlign), alignof(DoubleAlign),
  alignof(PointerAlign) };

/* Structure members which are themselves structure instances may have
   a more restrictive alignment than required by their own members.  */
int structAlign= alignof(StructAlign);

/*--------------------------------------------------------------------------*/

/* The six primitive data types also have a byte order.  */
int order[6];

/*--------------------------------------------------------------------------*/

/* The two floating point data types have a floating point layout.  */
struct FPLayout {
  /* Addresses are in bits with 0 128-bit of the most significant byte
     of the object, 8 the 128-bit of the 2nd most significant byte,
     and so on until 8*size-1 is the 1-bit of the least significant
     byte.  The actual storage order of the bytes is given by the
     order member of the DataLayout.
     sgnAddr            - bit address of overall sign
     expAddr, expSize   - bit address and size of exponent
     manAddr, manSize   - bit address and size of mantissa
     manNorm            - if non-zero, address of leading 1 in mantissa
     expBias            - exponent bias
   */
  int references;      /* reference counter */
  int sgnAddr, expAddr, expSize, manAddr, manSize, manNorm;
  long expBias;
} fpLayout[2];

/*--------------------------------------------------------------------------*/

#undef PROTO
#ifdef NEED_PROTOTYPES
#define PROTO(x) (x)
#else
#define PROTO(x) ()
#endif

extern int DecodeOrder PROTO((char *permute, long n));
extern int CompareBits PROTO((char *obj0, char *obj1, long n));
extern int MantissaOrder PROTO((int *mord, int nmo, long size));
extern int BigEndian PROTO((char *obj0, char *obj1, long size, int order));
extern int SwapBytes PROTO((char *obj, int n));
extern int HybridCBug PROTO((int hi, int lo));
extern long ExponentBias PROTO((char *one, int expAddr, int expSize));
extern int BadLayout PROTO((struct FPLayout *fpLayout));

extern int flt_compare PROTO((float, float));
extern int dbl_compare PROTO((double, double));

#include <stdio.h>

int main(argc, argv)
     int argc;
     char *argv[];
{
  short sord= 0;
  int iord= 0;
  long lord= 0;
  int mord[sizeof(DOUBLE)], manAddr;
  FLOAT f0, f1, fx;
  DOUBLE d0, d1, dx;
  int i, tmp, mask;
  FILE *file;

#ifdef BAD_ALIGNOF
  /* This is the fallback technique for computing data type alignments.  */
  struct CharAlign alignC;
  struct ShortAlign alignS;
  struct IntAlign alignI;
  struct LongAlign alignL;
  struct FloatAlign alignF;
  struct DoubleAlign alignD;
  struct PointerAlign alignP;
  struct StructAlign alignT;

  /* fill in correct data alignments */
  alignment[0]= (int)((char *)(alignC.y) - (char *)(&alignC));
  alignment[1]= (int)((char *)(alignS.y) - (char *)(&alignS));
  alignment[2]= (int)((char *)(alignI.y) - (char *)(&alignI));
  alignment[3]= (int)((char *)(alignL.y) - (char *)(&alignL));
  alignment[4]= (int)((char *)(alignF.y) - (char *)(&alignF));
  alignment[5]= (int)((char *)(alignD.y) - (char *)(&alignD));
  alignment[6]= (int)((char *)(alignP.y) - (char *)(&alignP));
  structAlign= (int)((char *)(alignT.y) - (char *)(&alignT));
#endif

  /* Byte ordering for integers is directly testable -- just need to
     compare set values in successive bytes by the shift left operator,
     then re-interpret them as characters to find the byte order.  */
  for (i=0 ; i<sizeof(short) ; i++) { sord|= (sizeof(short)-i)<<(8*i); }
  for (i=0 ; i<sizeof(int) ; i++) { iord|= (sizeof(int)-i)<<(8*i); }
  for (i=0 ; i<sizeof(long) ; i++) { lord|= (sizeof(long)-i)<<(8*i); }
  order[T_SHORT]= DecodeOrder((char *)&sord, (long)sizeof(short));
  order[T_INT]= DecodeOrder((char *)&iord, (long)sizeof(int));
  order[T_LONG]= DecodeOrder((char *)&lord, (long)sizeof(long));
  order[T_CHAR]= 0;  /* char order not meaningful */

  /* Cracking the floating point formats relies on comparing signed and
     unsigned 1 to get the sign bit, watching 1, 2, and 4 increment the
     exponent, and watching the successive addition of a 1-bit to the
     mantissa caused by successive addition of powers of 1/256.
     Also need to compare 1.0 and 1.5 to find most significant bit
     of mantissa.  */

  f1= 1.0;
  fx= 0.5;
  for (i=0 ; i<sizeof(FLOAT) ; i++) {
    f0= f1;
    f1+= fx;
    if (flt_compare(f1,f0)) break;  /* loop always exits here */
    mord[i]= CompareBits((char *)&f0, (char *)&f1, (long)sizeof(FLOAT));
    fx*= (1.0/256.0);
  }

  if ((order[T_FLOAT]= MantissaOrder(mord, i, (long)sizeof(FLOAT)))) {
    f0= 1.0;
    f1= -1.0;
    BigEndian((char *)&f0, (char *)&f1, (long)sizeof(FLOAT), order[T_FLOAT]);
    fpLayout[0].sgnAddr=
      CompareBits((char *)&f0, (char *)&f1, (long)sizeof(FLOAT));

    /* pick multiples of two "guaranteed" bigger than exponent bias
       in order to find most significant bit of exponent */
    f0= 65536.0*65536.0*65536.0*65536.0*65536.0*65536.0;  /* 2^96 */
    f1= 1.0/(65536.0*65536.0*65536.0*65536.0*65536.0*65536.0);
    BigEndian((char *)&f0, (char *)&f1, (long)sizeof(FLOAT), order[T_FLOAT]);
    fpLayout[0].expAddr=
      CompareBits((char *)&f0, (char *)&f1, (long)sizeof(FLOAT));

    /* finding the least significant bit of the exponent requires
       an extra reversal, since the bias is as yet unknown */
    f0= 1.0;
    f1= 2.0;
    BigEndian((char *)&f0, (char *)&f1, (long)sizeof(FLOAT), order[T_FLOAT]);
    SwapBytes((char *)&f0, (int)sizeof(FLOAT));
    SwapBytes((char *)&f1, (int)sizeof(FLOAT));
    tmp= CompareBits((char *)&f0, (char *)&f1, (long)sizeof(FLOAT));
    mask= ((char *)&f0)[tmp>>3] ^ ((char *)&f1)[tmp>>3];
    for (i=0 ; i<8 ; i++) {
      if (mask&1) break;
      else mask>>= 1;
    }
    tmp= ((int)sizeof(FLOAT)) - (tmp>>3) - 1;
    fpLayout[0].expSize= (tmp<<3) + (7-i) - fpLayout[0].expAddr + 1;

    f0= 1.0;
    f1= 1.5;
    BigEndian((char *)&f0, (char *)&f1, (long)sizeof(FLOAT), order[T_FLOAT]);
    fpLayout[0].manAddr= manAddr=
      CompareBits((char *)&f0, (char *)&f1, (long)sizeof(FLOAT));

    f0= 1.0;
    fx= 1.0/65536.0;  /* 2^-16 */
    for (i=16, f1=f0+fx ; !flt_compare(f0,f1) ; i++, f1=f0+fx) fx*= 0.5;
    fpLayout[0].manSize= i-1;

    fpLayout[0].manNorm= 0;
    if (manAddr>0) {
      /* try to determine whether the bit before manAddr is always set */
      int ba= (manAddr-1)>>3;
      int mask= 128>>(manAddr-1 - (ba<<3));
      fx= 1.5;
      for (i=0 ; i<16 ; i++) {
	f0= fx;
	f1= -1.75*fx;
	BigEndian((char *)&f0, (char *)&f1, (long)sizeof(FLOAT),
		  order[T_FLOAT]);
	if ((((char *)&f0)[ba]&mask)==0 || (((char *)&f0)[ba]&mask)==0)
	  break;
	fx*= 2.0;
      }
      if (i>=16) {
	fpLayout[0].manNorm= 1;
	fpLayout[0].manAddr= manAddr-1;
	fpLayout[0].manSize++;
      }
    }

    f0= f1= 1.0;
    BigEndian((char *)&f0, (char *)&f1, (long)sizeof(FLOAT), order[T_FLOAT]);
    fpLayout[0].expBias= ExponentBias((char *)&f0, fpLayout[0].expAddr,
				      fpLayout[0].expSize);
    if (fpLayout[0].manNorm) fpLayout[0].expBias--;

    /* guard against things like hex exponent machines... */
    if (BadLayout(&fpLayout[0])) order[T_FLOAT]= 0;
  }

  /* repeat the excercise for doubles */

  d1= 1.0;
  dx= 0.5;
  for (i=0 ; i<sizeof(DOUBLE) ; i++) {
    d0= d1;
    d1+= dx;
    if (dbl_compare(d1,d0)) break;  /* loop always exits here */
    mord[i]= CompareBits((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE));
    dx*= (1.0/256.0);
  }

  if ((order[T_DOUBLE]= MantissaOrder(mord, i, (long)sizeof(DOUBLE)))) {
    d0= 1.0;
    d1= -1.0;
    BigEndian((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE), order[T_DOUBLE]);
    fpLayout[1].sgnAddr=
      CompareBits((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE));

    /* pick multiples of two "guaranteed" bigger than exponent bias
       in order to find most significant bit of exponent */
    d0= 65536.0*65536.0*65536.0*65536.0*65536.0*65536.0;  /* 2^96 */
    d1= 1.0/(65536.0*65536.0*65536.0*65536.0*65536.0*65536.0);
    BigEndian((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE), order[T_DOUBLE]);
    fpLayout[1].expAddr=
      CompareBits((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE));

    /* finding the least significant bit of the exponent requires
       an extra reversal, since the bias is as yet unknown */
    d0= 1.0;
    d1= 2.0;
    BigEndian((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE), order[T_DOUBLE]);
    SwapBytes((char *)&d0, (int)sizeof(DOUBLE));
    SwapBytes((char *)&d1, (int)sizeof(DOUBLE));
    tmp= CompareBits((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE));
    mask= ((char *)&d0)[tmp>>3] ^ ((char *)&d1)[tmp>>3];
    for (i=0 ; i<8 ; i++) {
      if (mask&1) break;
      else mask>>= 1;
    }
    tmp= ((int)sizeof(DOUBLE)) - (tmp>>3) - 1;
    fpLayout[1].expSize= (tmp<<3) + (7-i) - fpLayout[1].expAddr + 1;

    d0= 1.0;
    d1= 1.5;
    BigEndian((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE), order[T_DOUBLE]);
    fpLayout[1].manAddr= manAddr=
      CompareBits((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE));

    d0= 1.0;
    dx= 1.0/65536.0;  /* 2^-16 */
    for (i=16, d1=d0+dx ; !dbl_compare(d0,d1) ; i++, d1=d0+dx) dx*= 0.5;
    fpLayout[1].manSize= i-1;

    fpLayout[1].manNorm= 0;
    if (manAddr>0) {
      /* try to determine whether the bit before manAddr is always set */
      int ba= (manAddr-1)>>3;
      int mask= 128>>(manAddr-1 - (ba<<3));
      dx= 1.5;
      for (i=0 ; i<16 ; i++) {
	d0= dx;
	d1= -1.75*dx;
	BigEndian((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE),
		  order[T_DOUBLE]);
	if ((((char *)&d0)[ba]&mask)==0 || (((char *)&d0)[ba]&mask)==0)
	  break;
	dx*= 2.0;
      }
      if (i>=16) {
	fpLayout[1].manNorm= 1;
	fpLayout[1].manAddr= manAddr-1;
	fpLayout[1].manSize++;
      }
    }

    d0= d1= 1.0;
    BigEndian((char *)&d0, (char *)&d1, (long)sizeof(DOUBLE), order[T_DOUBLE]);
    fpLayout[1].expBias= ExponentBias((char *)&d0, fpLayout[1].expAddr,
				      fpLayout[1].expSize);
    if (fpLayout[1].manNorm) fpLayout[1].expBias--;

    /* guard against things like hex exponent machines... */
    if (BadLayout(&fpLayout[1])) order[T_DOUBLE]= 0;
  }

  if (argc>1) file= fopen(argv[1], "w");
  else file= stdout;

  fprintf(file,
	  "/* Native primitive data formats computed by FMCALC */\n\n");

  fprintf(file,
	  "#define CHAR_ALIGN %d\n\n", alignment[T_CHAR]);

  fprintf(file,
	  "#define SHORT_SIZE %dL\n", size[T_SHORT]);
  fprintf(file,
	  "#define SHORT_ALIGN %d\n", alignment[T_SHORT]);
  fprintf(file,
	  "#define SHORT_ORDER %d\n\n", order[T_SHORT]);

  fprintf(file,
	  "#define INT_SIZE %dL\n", size[T_INT]);
  fprintf(file,
	  "#define INT_ALIGN %d\n", alignment[T_INT]);
  fprintf(file,
	  "#define INT_ORDER %d\n\n", order[T_INT]);

  fprintf(file,
	  "#define LONG_SIZE %dL\n", size[T_LONG]);
  fprintf(file,
	  "#define LONG_ALIGN %d\n", alignment[T_LONG]);
  fprintf(file,
	  "#define LONG_ORDER %d\n\n", order[T_LONG]);

  fprintf(file,
	  "#define FLOAT_SIZE %dL\n", size[T_FLOAT]);
  fprintf(file,
	  "#define FLOAT_ALIGN %d\n", alignment[T_FLOAT]);
  fprintf(file,
	  "#define FLOAT_ORDER %d\n", order[T_FLOAT]);
  fprintf(file,
	  "#define FLOAT_LAYOUT {0, %d, %d, %d, %d, %d, %d, %ld}\n\n",
	  fpLayout[0].sgnAddr, fpLayout[0].expAddr, fpLayout[0].expSize,
	  fpLayout[0].manAddr, fpLayout[0].manSize, fpLayout[0].manNorm,
	  fpLayout[0].expBias);

  fprintf(file,
	  "#define DOUBLE_SIZE %dL\n", size[T_DOUBLE]);
  fprintf(file,
	  "#define DOUBLE_ALIGN %d\n", alignment[T_DOUBLE]);
  fprintf(file,
	  "#define DOUBLE_ORDER %d\n", order[T_DOUBLE]);
  fprintf(file,
	  "#define DOUBLE_LAYOUT {0, %d, %d, %d, %d, %d, %d, %ld}\n\n",
	  fpLayout[1].sgnAddr, fpLayout[1].expAddr, fpLayout[1].expSize,
	  fpLayout[1].manAddr, fpLayout[1].manSize, fpLayout[1].manNorm,
	  fpLayout[1].expBias);

  fprintf(file,
	  "#define POINTER_ALIGN %d\n\n", alignment[T_POINTER]);

  fprintf(file,
	  "#define STRUCT_ALIGN %d\n", structAlign);

  if (argc>1) fclose(file);
  return 0;
}

/* without these, optimizers may break the above comparison tests */
int flt_compare(x, y)
     float x, y;
{ return x==y; }
int dbl_compare(x, y)
     double x, y;
{ return x==y; }

int DecodeOrder(permute, n)
     char *permute;
     long n;
{
  int i, lswFirst=0, wordSize, order;

  /* first, scan for the "1" or "n" in the permutation */
  for (i=0 ; i<n ; i++) {
    if (permute[i]==1) {
      lswFirst= 0;
      break;
    } else if (permute[i]==n) {
      lswFirst= 1;
      break;
    }
  }
  if (i<n/2) {
    wordSize= i+1;
  } else if (lswFirst) {
    /* this handles situation in which integer is stored in a larger
       size memory location than the register used to manipulate it --
       Cray short uses 24-bit register, but stores to 64-bit word */
    lswFirst= 0;
    wordSize= n-i;
  } else {
    /* this will return an error below */
    lswFirst= 0;
    wordSize= 1;
  }

  order= lswFirst? -wordSize : wordSize;

  /* n must be a multiple of imputed wordSize */
  if (n%wordSize) return 0;

  /* check to be sure that entire permutation is in fact described by
     lswFirst and wordSize */
  if (lswFirst) {
    int w, b;
    for (w=n-1 ; w>=0 ; w-=wordSize)
      for (b=1 ; b<=wordSize ; b++) {
	if (*permute && *permute!=(w+b)) return 0;
	permute++;
      }
  } else {
    int w, b;
    for (w=0 ; w<n ; w+=wordSize) {
      for (b=wordSize ; b>0 ; b--) {
	if (*permute && *permute!=(w+b)) return 0;
	permute++;
      }
    }
  }

  return order;
}

/* returns big-endian style bit address of first difference encountered
   between two sequences of bytes */
int CompareBits(obj0, obj1, n)
     char *obj0;
     char *obj1;
     long n;
{
  int i, j;
  char diff, mask;
  for (i=0 ; i<n ; i++) if (obj0[i]!=obj1[i]) break;
  if (i>=n) return n<<3;
  mask= '\200';
  diff= obj0[i] ^ obj1[i];
  for (j=0 ; j<8 ; j++) {
    if (mask&diff) break;
    else mask>>= 1;
  }
  return (i<<3)+j;
}

int MantissaOrder(mord, nmo, size)
     int *mord;
     int nmo;
     long size;
{
  int order, i;
  if (nmo<2) return 0; /* impossibly short order list */

  if (mord[0]<mord[nmo-1]) {  /* big-endian at first glance */
    for (i=1 ; i<nmo ; i++) if (mord[i-1]+8!=mord[i]) break;
    if (i>=nmo) {
      return 1;        /* big-endian */
    } else {
      int j, first= mord[nmo-1];
      for (i=nmo-1 ; i>0 ; i--) if (mord[i-1]-8!=mord[i]) break;
      order= nmo-i;
      if (size%order) return 0;
      for (i-- ; i>=0 ; i--) {
	if (mord[i]+8*order != first) return 0;  /* give up */
	first= mord[i];
	for (j=1 ; j<order && (--i)>=0 ; j++)
	  if (mord[i-1]-8!=mord[i]) return 0;    /* give up */
      }
      return order;    /* VAX-style middle-endian */
    }

  } else {                    /* little-endian at first glance */
    for (i=1 ; i<nmo ; i++) if (mord[i-1]-8!=mord[i]) break;
    if (i>=nmo) {
      return -1;       /* little-endian */
    } else {
      int j, first= mord[nmo-1];
      for (i=nmo-1 ; i>0 ; i--) if (mord[i-1]+8!=mord[i]) break;
      order= nmo-i;
      if (size%order) return 0;
      for (i-- ; i>=0 ; i--) {
	if (mord[i]-8*order != first) return 0;  /* give up */
	first= mord[i];
	for (j=1 ; j<order && (--i)>=0 ; j++)
	  if (mord[i-1]+8!=mord[i]) return 0;    /* give up */
      }
      return order;    /* unknown middle-endian */
    }
  }
}

/* in-place permutation of size bytes of obj0 and obj1 into
   "standard" big-endian order (order=1), given the actual order */
int BigEndian(obj0, obj1, size, order)
     char *obj0;
     char *obj1;
     long size;
     int order;
{
  if (order>1) {
    /* VAX-style middle-endian, must byte-reverse groups of order bytes */
    int i;
    for (i=0 ; i<size ; i+=order) {
      SwapBytes(obj0+i, order);
      SwapBytes(obj1+i, order);
    }
  } else if (order<0) {
    SwapBytes(obj0, (int)size);
    SwapBytes(obj1, (int)size);
    BigEndian(obj0, obj1, size, -order);
  }
  return 0;
}

int SwapBytes(obj, n)
     char *obj;
     int n;
{
  char *end= obj+n-1;
  register char c;
  while (obj<end) { c=*obj; *(obj++)=*end; *(end--)=c; }
  return 0;
}

long ExponentBias(one, expAddr, expSize)
     char *one;
     int expAddr, expSize;
{
  long bias;
  int expEnd= expAddr+expSize-1;
  int n= expEnd>>3;
  int i= expAddr>>3;
  int rshft= 7 - (expEnd - (n<<3));
  int rmask= (1<<rshft) - 1;
  int byt= one[i]&0xff;

  bias= byt>>rshft;
  while (i<n) {
    bias<<= 8;
    bias|= (byt&rmask)<<(8-rshft);
    byt= one[++i]&0xff;
    bias|= byt>>rshft;
  }
  bias|= (byt&rmask)<<(8-rshft);

  bias= bias & ((1<<expSize)-1);  /* this is the exponent of 1.0 */
  return bias;
}

int BadLayout(fpLayout)
     struct FPLayout *fpLayout;
{
  int sgnAddr= fpLayout->sgnAddr;
  int expAddr= fpLayout->expAddr;
  int expPlus= expAddr+fpLayout->expSize;
  int manAddr= fpLayout->manAddr;
  int manPlus= manAddr+fpLayout->manSize;

  if ((sgnAddr>=expAddr && sgnAddr<expPlus) ||
      (sgnAddr>=manAddr && sgnAddr<manPlus)) return 1;

  if (expAddr==manAddr) return 1;
  if (expAddr<manAddr) {
    if (expPlus>manAddr) return 1;
  } else {
    if (manPlus>expAddr) return 1;
  }

  return 0;  /* no overlapping fields -- looks OK */
}
