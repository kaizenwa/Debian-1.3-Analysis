/* mathl.h */

#if __STDC__

#if 0
extern long double __infnanl ( int );
extern long double __polevll ( long double, long double *, int );
extern long double __p1evll ( long double, long double *, int );
#endif
extern long double fabsl ( long double );
extern long double hypotl ( long double, long double );
extern long double acoshl ( long double );
extern long double acosl ( long double );
extern long double asinl ( long double );
extern long double atanhl ( long double );
extern long double coshl ( long double );
extern long double tanhl ( long double );
extern long double asinhl ( long double );
extern long double sinhl ( long double );
extern long double cosl ( long double );
extern long double sinl ( long double );
extern long double tanl ( long double );
extern long double atanl ( long double );
extern long double atan2l ( long double, long double );
extern long double expl ( long double );
extern long double exp2l ( long double );
extern long double exp10l ( long double );
extern long double expm1l ( long double );
extern long double fmodl ( long double, long double );
extern long double cbrtl ( long double );
extern long double ceill ( long double );
extern long double erfcl ( long double );
extern long double erfl ( long double );
extern long double floorl ( long double );
/*
extern int fpclassifyf ( unsigned int );
extern int fpclassifyd ( unsigned int, unsigned int );
extern int fpclassifyl ( unsigned int, unsigned int, unsigned int );
*/
extern long double frexpl ( long double, int * );
extern long double j0l ( long double );
extern long double y0l ( long double );
extern long double j1l ( long double );
extern long double y1l ( long double );
extern long double jnl ( int, long double );
extern long double ynl ( int, long double );
extern long double ldexpl ( long double, int );
extern int ldrand ( long double * );
extern long double lgammal ( long double );
extern long double log10l ( long double );
extern long double log1pl ( long double );
extern long double log2l ( long double );
extern long double logl ( long double );
extern long double modfl ( long double, long double * );
extern long double powl ( long double, long double );
extern long double sqrtl ( long double );

#else /* __STDC__ */

#if 0
extern long double __infnanl ();
extern long double __polevll ();
extern long double __p1evll ();
#endif
extern long double fabsl ();
extern long double hypotl ();
extern long double acoshl ();
extern long double acosl ();
extern long double asinl ();
extern long double atanhl ();
extern long double coshl ();
extern long double tanhl ();
extern long double asinhl ();
extern long double sinhl ();
extern long double cosl ();
extern long double sinl ();
extern long double tanl ();
extern long double atanl ();
extern long double atan2l ();
extern long double expl ();
extern long double exp2l ();
extern long double exp10l ();
extern long double expm1l ();
extern long double fmodl ();
extern long double cbrtl ();
extern long double ceill ();
extern long double erfcl ();
extern long double erfl ();
extern long double floorl ();
/*
extern int fpclassifyf ();
extern int fpclassifyd ();
extern int fpclassifyl ();
*/
extern long double frexpl ();
extern long double j0l ();
extern long double y0l ();
extern long double j1l ();
extern long double y1l ();
extern long double jnl ();
extern long double ynl ();
extern long double ldexpl ();
extern int ldrand ();
extern long double lgammal ();
extern long double log10l ();
extern long double log1pl ();
extern long double log2l ();
extern long double logl ();
extern long double modfl ();
extern long double powl ();
extern long double sqrtl ();

#endif /* __STDC__ */
