
#define IBMPC 1
#define DENORMAL 1

#ifdef IBMPC
#define EXPMSK 0x800f
#define MEXP 0x7ff
#define NBITS 64
#endif

#ifdef MIEEE
#define EXPMSK 0x800f
#define MEXP 0x7ff
#define NBITS 64
#endif

extern double MAXNUML;


long double frexpl( long double x, int *pw2);

long double frexpl( long double x, int *pw2)
{
   union {
      long double y;
      unsigned short sh[6];
   } u;
   int i, k;
   short *q;

   u.y = x;

#ifdef UNK
   mtherr( "frexp", DOMAIN );
   return(0.0L);
#endif

/* find the exponent (power of 2) */
#ifdef IBMPC
   q = (short *)&u.sh[4];
   i  = *q & 0x7fff;
#endif

#ifdef MIEEE
   q = (short *)&u.sh[0];
   i  = *q & 0x7fff;
#endif


   if( i == 0 ) {
      if( u.y == 0.0L ) {
			*pw2 = 0;
			return(0.0L);
		}
/* Number is denormal or zero */
#ifdef DENORMAL
/* Handle denormal number. */
		do {
			u.y *= 2.0L;
			i -= 1;
			k  = *q & 0x7fff;
		} while( (k == 0) && (i > -66) );
		i = i + k;
#else
		*pw2 = 0;
		return(0.0L);
#endif /* DENORMAL */
	}

	*pw2 = i - 0x3ffe;
	*q = 0x3ffe;
	return( u.y );
}
