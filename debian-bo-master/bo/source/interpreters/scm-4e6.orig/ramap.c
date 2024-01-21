/* Copyright (C) 1994, 1995 Free Software Foundation, Inc.
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

/* "ramap.c" Array mapping functions for APL-Scheme.
   Author: Radey Shouman */

#include "scm.h"

typedef struct {
  char *name;
  SCM sproc;
  int (* vproc)();
} ra_iproc;

# define BVE_REF(a, i) ((VELTS(a)[(i)/LONG_BIT] & (1L<<((i)%LONG_BIT))) ? 1 : 0)
# define BVE_SET(a, i) (VELTS(a)[(i)/LONG_BIT] |= (1L<<((i)%LONG_BIT)))
# define BVE_CLR(a, i) (VELTS(a)[(i)/LONG_BIT] &= ~(1L<<((i)%LONG_BIT)))
/* Fast, recycling vector ref */
# define RVREF(ra, i, e) (e = cvref(ra, i, e))
/* #define RVREF(ra, i, e) (cvref(ra, i, UNDEFINED)) to turn off */

/* IVDEP means "ignore vector dependencies", meaning we guarantee that
   elements of vector operands are not aliased */
# ifdef _UNICOS
#  define IVDEP(test, line) if (test) {_Pragma("ivdep"); line} else {line}
# else
#  define IVDEP(test, line) line
# endif

				/* inds must be a uvect or ivect, no check. */
static sizet cind(ra, inds)
     SCM ra, inds;
{
  sizet i;
  int k;
  long *ve = VELTS(inds);
  if (!ARRAYP(ra))
    return *ve;
  i = ARRAY_BASE(ra);
  for (k = 0; k < ARRAY_NDIM(ra); k++)
    i += (ve[k] - ARRAY_DIMS(ra)[k].lbnd)*ARRAY_DIMS(ra)[k].inc;
  return i;
}

  /* Checker for array mapping functions:
     return values: 4 --> shapes, increments, and bases are the same;
		    3 --> shapes and increments are the same;
		    2 --> shapes are the same;
		    1 --> ras are at least as big as ra0;
		    0 --> no match.
   */
int ra_matchp(ra0, ras)
     SCM ra0, ras;
{
  SCM ra1;
  array_dim dims;
  array_dim *s0 = &dims;
  array_dim *s1;
  sizet bas0 = 0;
  int i, ndim = 1;
  int exact = 2    /* 4 */;	/* Don't care about values >2 (yet?) */
  if IMP(ra0) return 0;
  switch TYP7(ra0) {
  default: return 0;
  case tc7_vector: case tc7_string: case tc7_bvect: case tc7_uvect:
  case tc7_ivect: case tc7_fvect: case tc7_dvect: case tc7_cvect:
    s0->lbnd = 0;
    s0->inc = 1;
    s0->ubnd = (long)LENGTH(ra0) - 1;
    break;
  case tc7_smob:
    if (!ARRAYP(ra0)) return 0;
    ndim = ARRAY_NDIM(ra0);
    s0 = ARRAY_DIMS(ra0);
    bas0 = ARRAY_BASE(ra0);
    break;
  }
  while NIMP(ras) {
    ra1 = CAR(ras);
    switch (IMP(ra1) ? 0 : TYP7(ra1)) {
    default: scalar:
      CAR(ras) = sc2array(ra1,ra0,EOL); break;
    case tc7_vector: case tc7_string: case tc7_bvect: case tc7_uvect:
    case tc7_ivect: case tc7_fvect: case tc7_dvect: case tc7_cvect:
      if (1 != ndim) return 0;
      switch (exact) {
      case 4: if (0 != bas0) exact = 3;
      case 3: if (1 != s0->inc) exact = 2;
      case 2: if ((0==s0->lbnd) && (s0->ubnd==LENGTH(ra1) - 1)) break;
	exact = 1;
      case 1: if (s0->lbnd < 0 || s0->ubnd >= LENGTH(ra1))
	if (s0->lbnd <= s0->ubnd) return 0;
      }
      break;
    case tc7_smob:
      if (!ARRAYP(ra1)) goto scalar;
      if (ndim != ARRAY_NDIM(ra1)) 
	if (0==ARRAY_NDIM(ra1)) 
	  goto scalar;
	else
	  return 0;
      s1 = ARRAY_DIMS(ra1);
      if (bas0 != ARRAY_BASE(ra1)) exact = 3;
      for (i = 0; i < ndim; i++)
	switch (exact) {
	case 4: case 3:
	  if (s0[i].inc != s1[i].inc)
	    exact = 2;
	case 2:
	  if (s0[i].lbnd==s1[i].lbnd && s0[i].ubnd==s1[i].ubnd)
	    break;
	  exact = 1;
	default:
	  if (s0[i].lbnd < s1[i].lbnd || s0[i].ubnd > s1[i].ubnd)
	    if (s0[i].lbnd <= s0[i].ubnd) return 0;
	}
      break;
    }
    ras = CDR(ras);
  }
  return exact;
}

static char s_ra_mismatch[] = "array shape mismatch";
int ramapc(cproc, data, ra0, lra, what)
     int (*cproc)();
     SCM data, ra0, lra;
     char *what;
{
  SCM inds, z;
  SCM vra0, ra1, vra1;
  SCM lvra, *plvra;
  long *vinds;
  int k, kmax = (ARRAYP(ra0) ? ARRAY_NDIM(ra0) - 1 : 0);
  switch (ra_matchp(ra0, lra)) {
  default:
  case 0: wta(ra0, s_ra_mismatch, what);
  case 2: case 3: case 4:	/* Try unrolling arrays */
    if (kmax < 0) goto gencase;
    vra0 = (0==kmax ? ra0 : array_contents(ra0, UNDEFINED));
    if IMP(vra0) goto gencase;
    if (!ARRAYP(vra0)) {
      vra1 = make_ra(1);
      ARRAY_BASE(vra1) = 0;
      ARRAY_DIMS(vra1)->lbnd = 0;
      ARRAY_DIMS(vra1)->ubnd = LENGTH(vra0) - 1;
      ARRAY_DIMS(vra1)->inc = 1;
      ARRAY_V(vra1) = vra0;
      vra0 = vra1;
    }
    lvra = EOL;
    plvra = &lvra;
    for (z = lra; NIMP(z); z = CDR(z)) {
      vra1 = ra1 = (0==kmax ? CAR(z) : array_contents(CAR(z), UNDEFINED));
      if FALSEP(ra1) goto gencase;
      if (!ARRAYP(ra1)) {
	vra1 = make_ra(1);
	ARRAY_DIMS(vra1)->lbnd = ARRAY_DIMS(vra0)->lbnd;
	ARRAY_DIMS(vra1)->ubnd = ARRAY_DIMS(vra0)->ubnd;
	ARRAY_BASE(vra1) = 0;
	ARRAY_DIMS(vra1)->inc = 1;
	ARRAY_V(vra1) = ra1;
      }
      *plvra = cons(vra1, EOL);
      plvra = &CDR(*plvra);
    }
    return (UNBNDP(data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra));
  case 1: gencase:		/* Have to loop over all dimensions. */
    vra0 = make_ra(1);
    if ARRAYP(ra0) {
      if (kmax < 0) {
	ARRAY_DIMS(vra0)->lbnd = 0;
	ARRAY_DIMS(vra0)->ubnd = 0;
	ARRAY_DIMS(vra0)->inc = 1;
      }
      else {
	ARRAY_DIMS(vra0)->lbnd = ARRAY_DIMS(ra0)[kmax].lbnd;
	ARRAY_DIMS(vra0)->ubnd = ARRAY_DIMS(ra0)[kmax].ubnd;
	ARRAY_DIMS(vra0)->inc = ARRAY_DIMS(ra0)[kmax].inc;
      }
      ARRAY_BASE(vra0) = ARRAY_BASE(ra0);
      ARRAY_V(vra0) = ARRAY_V(ra0);
    }
    else {
      ARRAY_DIMS(vra0)->lbnd = 0;
      ARRAY_DIMS(vra0)->ubnd = LENGTH(ra0) - 1;
      ARRAY_DIMS(vra0)->inc = 1;
      ARRAY_BASE(vra0) = 0;
      ARRAY_V(vra0) = ra0;
      ra0 = vra0;
    }
    lvra = EOL;
    plvra = &lvra;
    for (z = lra; NIMP(z); z = CDR(z)) {
      ra1 = CAR(z);
      vra1 = make_ra(1);
      ARRAY_DIMS(vra1)->lbnd = ARRAY_DIMS(vra0)->lbnd;
      ARRAY_DIMS(vra1)->ubnd = ARRAY_DIMS(vra0)->ubnd;
      if ARRAYP(ra1) {
	if (kmax >= 0)
	  ARRAY_DIMS(vra1)->inc = ARRAY_DIMS(ra1)[kmax].inc;
	ARRAY_V(vra1) = ARRAY_V(ra1);
      }
      else {
	ARRAY_DIMS(vra1)->inc = 1;
	ARRAY_V(vra1) = ra1;
      }
      *plvra = cons(vra1, EOL);
      plvra = &CDR(*plvra);
    }
    inds = make_uve(ARRAY_NDIM(ra0)+0L, MAKINUM(-1L));
    vinds = (long *)VELTS(inds);
    for (k = 0; k <= kmax; k++)
      vinds[k] = ARRAY_DIMS(ra0)[k].lbnd;
    k = kmax;
    do {
      if (k==kmax) {
	SCM y = lra;
	ARRAY_BASE(vra0) = cind(ra0, inds);
	for (z = lvra; NIMP(z); z = CDR(z), y = CDR(y))
	  ARRAY_BASE(CAR(z)) = cind(CAR(y), inds);
	if (0==(UNBNDP(data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra)))
	  return 0;
	k--;
	continue;
      }
      if (vinds[k] < ARRAY_DIMS(ra0)[k].ubnd) {
	vinds[k]++;
	k++;
	continue;
      }
      vinds[k] = ARRAY_DIMS(ra0)[k].lbnd - 1;
      k--;
    } while (k >= 0);
    return 1;
  }
}

static char s_array_fill[] = "array-fill!";
SCM array_fill(ra, fill)
     SCM ra, fill;
{
  ramapc(rafill, fill, ra, EOL, s_array_fill);
  return UNSPECIFIED;
}

static char s_sarray_copy[] = "serial-array-copy!";
# define s_array_copy  (s_sarray_copy + 7)
static int racp(src, dst)
     SCM dst, src;
{
  long n = (ARRAY_DIMS(src)->ubnd - ARRAY_DIMS(src)->lbnd + 1);
  long inc_d, inc_s = ARRAY_DIMS(src)->inc;
  sizet i_d, i_s = ARRAY_BASE(src);
  dst = CAR(dst);
  inc_d = ARRAY_DIMS(dst)->inc;
  i_d = ARRAY_BASE(dst);
  src = ARRAY_V(src);
  dst = ARRAY_V(dst);
  switch TYP7(dst) {
  default: gencase: case tc7_vector:
    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
      aset(dst, cvref(src, i_s, UNDEFINED), MAKINUM(i_d));
    break;
  case tc7_string: if (tc7_string != TYP7(dst)) goto gencase;
    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
      CHARS(dst)[i_d] = CHARS(src)[i_s];
    break;
  case tc7_bvect: if (tc7_bvect != TYP7(dst)) goto gencase;
    if (1==inc_d && 1==inc_s && i_s%LONG_BIT==i_d%LONG_BIT && n>=LONG_BIT) {
      long *sv = (long *)VELTS(src);
      long *dv = (long *)VELTS(dst);
      sv += i_s/LONG_BIT;
      dv += i_d/LONG_BIT;
      if (i_s % LONG_BIT) {	/* leading partial word */
	*dv = (*dv & ~(~0L<<(i_s%LONG_BIT))) | (*sv & (~0L<<(i_s%LONG_BIT)));
	dv++;
	sv++;
	n -= LONG_BIT - (i_s % LONG_BIT);
      }
      IVDEP(src != dst,
	    for (; n >= LONG_BIT; n -= LONG_BIT, sv++, dv++)
	      *dv = *sv;)
      if (n)			/* trailing partial word */
	*dv = (*dv & (~0L<<n)) | (*sv & ~(~0L<<n));
    }
    else {
      for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	if (VELTS(src)[i_s/LONG_BIT] & (1L << (i_s%LONG_BIT)))
	  VELTS(dst)[i_d/LONG_BIT] |= (1L << (i_d%LONG_BIT));
	else
	  VELTS(dst)[i_d/LONG_BIT] &= ~(1L << (i_d%LONG_BIT));
    }
    break;
  case tc7_uvect:
  case tc7_ivect: {
    long *d = (long *)VELTS(dst), *s = (long *)VELTS(src);
    if (TYP7(src)==TYP7(dst)) {
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	      d[i_d] = s[i_s];)
    }
    else if (tc7_ivect==TYP7(dst))
      for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	d[i_d] = num2long(cvref(src, i_s, UNDEFINED),
			  (char *)ARG2, s_array_copy);
    else
      for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	d[i_d] = num2ulong(cvref(src, i_s, UNDEFINED),
			   (char *)ARG2, s_array_copy);
    break;
  }
# ifdef FLOATS
#  ifdef SINGLES
  case tc7_fvect: {
    float *d = (float *)VELTS(dst);
    float *s = (float *)VELTS(src);
    switch TYP7(src) {
    default: goto gencase;
    case tc7_ivect: case tc7_uvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	      d[i_d] = ((long *)s)[i_s]; )
      break;
    case tc7_fvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	      d[i_d] = s[i_s]; )
      break;
    case tc7_dvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	      d[i_d] = ((double *)s)[i_s]; )
      break;
    }
    break;
  }
#  endif /* SINGLES */
  case tc7_dvect: {
    double *d = (double *)VELTS(dst);
    double *s = (double *)VELTS(src);
    switch TYP7(src) {
    default: goto gencase;
    case tc7_ivect: case tc7_uvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	      d[i_d] = ((long *)s)[i_s]; )
      break;
    case tc7_fvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	      d[i_d] = ((float *)s)[i_s];)
      break;
    case tc7_dvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
	      d[i_d] = s[i_s];)
      break;
    }
    break;
  }
  case tc7_cvect: {
    double (*d)[2] = (double (*)[2])VELTS(dst);
    double (*s)[2] = (double (*)[2])VELTS(src);
    switch TYP7(src) {
    default: goto gencase;
    case tc7_ivect: case tc7_uvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d) {
	      d[i_d][0] = ((long *)s)[i_s];
	      d[i_d][1] = 0.0;
	    })
      break;
    case tc7_fvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d) {
	      d[i_d][0] = ((float *)s)[i_s];
	      d[i_d][1] = 0.0;
	    })
      break;
    case tc7_dvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d) {
	      d[i_d][0] = ((double *)s)[i_s];
	      d[i_d][1] = 0.0;
	    })
      break;
    case tc7_cvect:
      IVDEP(src != dst,
	    for (; n-- > 0; i_s += inc_s, i_d += inc_d) {
	      d[i_d][0] = s[i_s][0];
	      d[i_d][1] = s[i_s][1];
	    })
    }
    break;
  }
# endif /* FLOATS */
  }
  return 1;
}
SCM array_copy(src, dst)
     SCM src;
     SCM dst;
{
#ifndef RECKLESS
  if (INUM0==array_rank(dst))
    ASSERT(NIMP(dst) && ARRAYP(dst) && INUM0==array_rank(src),
	   dst, ARG2, s_array_copy);
#endif
  ramapc(racp, UNDEFINED, src, cons(dst, EOL), s_array_copy);
  return UNSPECIFIED;
}

SCM ra2contig(ra, copy)
     SCM ra;
     int copy;
{
  SCM ret;
  long inc = 1;
  sizet k, len = 1;
  for (k = ARRAY_NDIM(ra); k--;)
    len *= ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd + 1;
  k = ARRAY_NDIM(ra);
  if (ARRAY_CONTP(ra) && ((0==k) || (1==ARRAY_DIMS(ra)[k-1].inc))) {
    if (tc7_bvect != TYP7(ARRAY_V(ra)))
      return ra;
    if ((len==LENGTH(ARRAY_V(ra)) &&
	 0==ARRAY_BASE(ra) % LONG_BIT &&
	 0==len % LONG_BIT))
      return ra;
  }
  ret = make_ra(k);
  ARRAY_BASE(ret) = 0;
  while (k--) {
    ARRAY_DIMS(ret)[k].lbnd = ARRAY_DIMS(ra)[k].lbnd;
    ARRAY_DIMS(ret)[k].ubnd = ARRAY_DIMS(ra)[k].ubnd;
    ARRAY_DIMS(ret)[k].inc = inc;
    inc *= ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd + 1;
  }
  CAR(ret) |= ARRAY_CONTIGUOUS;
  ARRAY_V(ret) = make_uve(inc+0L, array_prot(ra));
  if (copy) array_copy(ra, ret);
  return ret;
}

static char s_ura_rd[] = "uniform-array-read!";
SCM ura_read(ra, port)
     SCM ra, port;
{
  SCM ret, cra;
  if (NIMP(ra) && ARRAYP(ra)) {
    cra = ra2contig(ra, 0);
    ret = uve_read(cra, port);
    if (cra != ra) array_copy(cra, ra);
    return ret;
  }
  else return uve_read(ra, port);
}

static char s_ura_wr[] = "uniform-array-write";
SCM ura_write(ra, port)
     SCM ra, port;
{
  if (NIMP(ra) && ARRAYP(ra))
    return uve_write(ra2contig(ra,1), port);
  else
    return uve_write(ra, port);
}

static char s_sc2array[] = "scalar->array";
SCM sc2array(s, ra, prot)
     SCM s, ra, prot;
{
  SCM res;
  ASSERT(NIMP(ra), ra, ARG2, s_sc2array);
  if ARRAYP(ra) {
    int k = ARRAY_NDIM(ra);
    res = make_ra(k);
    while (k--) {
      ARRAY_DIMS(res)[k].ubnd = ARRAY_DIMS(ra)[k].ubnd;
      ARRAY_DIMS(res)[k].lbnd = ARRAY_DIMS(ra)[k].lbnd;
      ARRAY_DIMS(res)[k].inc = 0;
    }
    ra = ARRAY_V(ra);
  }
  else {
    ASSERT(BOOL_T==arrayp(ra, UNDEFINED), ra, ARG2, s_sc2array);
    res = make_ra(1);
    ARRAY_DIMS(res)->ubnd = LENGTH(ra) - 1;
    ARRAY_DIMS(res)->lbnd = 0;
    ARRAY_DIMS(res)->inc = 0;
  }
  if (NIMP(s) && ARRAYP(s) && 0==ARRAY_NDIM(s)) {
    ARRAY_BASE(res) = ARRAY_BASE(s);
    ARRAY_V(res) = ARRAY_V(s);
    return res;
  }
  ARRAY_BASE(res) = 0;
  ARRAY_V(res) = make_uve(1L, NULLP(prot) ? array_prot(ra) : CAR(prot));
  switch TYP7(ARRAY_V(res)) {
  case tc7_vector:
    break;
  case tc7_string:
    if ICHRP(s) break;
    goto mismatch;
  case tc7_uvect: 
    if (INUMP(s) && INUM(s)>=0) break;
#ifdef BIGDIG
    if (NIMP(s) && tc16_bigpos==TYP16(s) && NUMDIGS(s)<=DIGSPERLONG) break;
#endif
    goto mismatch;
  case tc7_ivect:
    if INUMP(s) break;
#ifdef BIGDIG
    if (NIMP(s) && BIGP(s) && NUMDIGS(s)<=DIGSPERLONG) break;
#endif
    goto mismatch;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
#endif
  case tc7_dvect:
    if (NUMBERP(s) && !(NIMP(s) && CPLXP(s))) break;
    goto mismatch;
  case tc7_cvect:
    if NUMBERP(s) break;
    goto mismatch;
#endif
  mismatch: ARRAY_V(res) = make_vector(MAKINUM(1), s);
    return res;
  }
  aset(ARRAY_V(res), s, INUM0);
  return res;
}

/* Functions callable by ARRAY-MAP! */
int ra_eqp(ra0, ras)
     SCM ra0, ras;
{
  SCM ra1 = CAR(ras), ra2 = CAR(CDR(ras));
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0), i1 = ARRAY_BASE(ra1), i2 = ARRAY_BASE(ra2);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  long inc1 = ARRAY_DIMS(ra1)->inc;
  long inc2 = ARRAY_DIMS(ra2)->inc;
  ra0 = ARRAY_V(ra0);
  ra1 = ARRAY_V(ra1);
  ra2 = ARRAY_V(ra2);
  switch (TYP7(ra1)==TYP7(ra2) ? TYP7(ra1) : 0) {
  default: {
    SCM e1 = UNDEFINED, e2 = UNDEFINED;
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if BVE_REF(ra0, i0)
	if FALSEP(eqp(RVREF(ra1, i1, e1), RVREF(ra2, i2, e2)))
	  BVE_CLR(ra0, i0);
    break;
  }
  case tc7_uvect:
  case tc7_ivect:
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if BVE_REF(ra0, i0)
	if (VELTS(ra1)[i1] != VELTS(ra2)[i2]) BVE_CLR(ra0, i0);
    break;
# ifdef FLOATS
#  ifdef SINGLES
  case tc7_fvect:
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if BVE_REF(ra0, i0)
	if (((float *)VELTS(ra1))[i1] != ((float *)VELTS(ra2))[i2])
	  BVE_CLR(ra0, i0);
    break;
#  endif /*SINGLES*/
  case tc7_dvect:
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if BVE_REF(ra0, i0)
	if (((double *)VELTS(ra1))[i1] != ((double *)VELTS(ra2))[i2])
	  BVE_CLR(ra0, i0);
    break;
  case tc7_cvect:
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if BVE_REF(ra0, i0)
	if (((double *)VELTS(ra1))[2*i1] != ((double *)VELTS(ra2))[2*i2] ||
	    ((double *)VELTS(ra1))[2*i1+1] != ((double *)VELTS(ra2))[2*i2+1])
	  BVE_CLR(ra0, i0);
    break;
# endif /*FLOATS*/
  }
  return 1;
}
/* opt 0 means <, nonzero means >= */
static int ra_compare(ra0, ra1, ra2, opt)
     SCM ra0, ra1, ra2;
     int opt;
{
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0), i1 = ARRAY_BASE(ra1), i2 = ARRAY_BASE(ra2);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  long inc1 = ARRAY_DIMS(ra1)->inc;
  long inc2 = ARRAY_DIMS(ra2)->inc;
  ra0 = ARRAY_V(ra0);
  ra1 = ARRAY_V(ra1);
  ra2 = ARRAY_V(ra2);
  switch (TYP7(ra1)==TYP7(ra2) ? TYP7(ra1) : 0) {
  default: {
    SCM e1 = UNDEFINED, e2 = UNDEFINED;
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if BVE_REF(ra0, i0)
	if (opt ?
	    NFALSEP(lessp(RVREF(ra1, i1, e1), RVREF(ra2, i2, e2))) :
	    FALSEP(lessp(RVREF(ra1, i1, e1), RVREF(ra2, i2, e2))) )
	  BVE_CLR(ra0, i0);
    break;
  }
  case tc7_uvect:
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2) {
      if BVE_REF(ra0, i0)
	if (opt ?
	    ((unsigned long*)VELTS(ra1))[i1] < ((unsigned long*)VELTS(ra2))[i2] :
	    ((unsigned long*)VELTS(ra1))[i1] >= ((unsigned long*)VELTS(ra2))[i2])
	  BVE_CLR(ra0, i0);
    }
    break;
  case tc7_ivect:
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2) {
      if BVE_REF(ra0, i0)
	if (opt ?
	    VELTS(ra1)[i1] < VELTS(ra2)[i2] :
	    VELTS(ra1)[i1] >= VELTS(ra2)[i2])
	  BVE_CLR(ra0, i0);
    }
    break;
# ifdef FLOATS
#  ifdef SINGLES
  case tc7_fvect:
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if BVE_REF(ra0, i0)
	if (opt ?
	    ((float *)VELTS(ra1))[i1] < ((float *)VELTS(ra2))[i2] :
	    ((float *)VELTS(ra1))[i1] >= ((float *)VELTS(ra2))[i2])
	  BVE_CLR(ra0, i0);
    break;
#  endif /*SINGLES*/
  case tc7_dvect:
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if BVE_REF(ra0, i0)
	if (opt ?
	    ((double *)VELTS(ra1))[i1] < ((double *)VELTS(ra2))[i2] :
	    ((double *)VELTS(ra1))[i1] >= ((double *)VELTS(ra2))[i2])
	  BVE_CLR(ra0, i0);
    break;
# endif /*FLOATS*/
  }
  return 1;
}
int ra_lessp(ra0, ras)
     SCM ra0, ras;
{
 return ra_compare(ra0, CAR(ras), CAR(CDR(ras)), 0);
}
int ra_leqp(ra0, ras)
     SCM ra0, ras;
{
  return ra_compare(ra0, CAR(CDR(ras)), CAR(ras), 1);
}
int ra_grp(ra0, ras)
     SCM ra0, ras;
{
  return ra_compare(ra0, CAR(CDR(ras)), CAR(ras), 0);
}
int ra_greqp(ra0, ras)
     SCM ra0, ras;
{
  return ra_compare(ra0, CAR(ras), CAR(CDR(ras)), 1);
}

int ra_sum(ra0, ras)
     SCM ra0, ras;
{
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  ra0 = ARRAY_V(ra0);
  if NNULLP(ras) {
    SCM ra1 = CAR(ras);
    sizet i1 = ARRAY_BASE(ra1);
    long inc1 = ARRAY_DIMS(ra1)->inc;
    ra1 = ARRAY_V(ra1);
    switch (TYP7(ra0)==TYP7(ra1) ? TYP7(ra0) : 0) {
    ovflow: wta(ra0, (char *)OVFLOW, "+");
    default: {
      SCM e0 = UNDEFINED, e1 = UNDEFINED;
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	aset(ra0, sum(RVREF(ra0, i0, e0), RVREF(ra1, i1, e1)),
	     MAKINUM(i0));
      break;
    }
    case tc7_uvect: {
      unsigned long r;
      unsigned long *v0 = (unsigned long *)VELTS(ra0);
      unsigned long *v1 = (unsigned long *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      r = v0[i0] + v1[i1];
	      ASRTGO(r >= v0[i0], ovflow); /* Will prevent vectorization */
	      v0[i0] = r;
	    } );
      break;
    }
    case tc7_ivect: {
      long r, *v0 = (long *)VELTS(ra0), *v1 = (long *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      r = v0[i0] + v1[i1];
	      ASRTGO((v0[i0]>0 ? r>=0 || v1[i1]<0 : r<=0 || v1[i1]>0), ovflow);
	      v0[i0] = r;
	    } );
      break;
    }
# ifdef FLOATS
#  ifdef SINGLES
    case tc7_fvect: {
      float *v0 = (float *)VELTS(ra0);
      float *v1 = (float *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      v0[i0] += v1[i1]);
      break;
    }
#  endif /* SINGLES */
    case tc7_dvect: {
      double *v0 = (double *)VELTS(ra0);
      double *v1 = (double *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      v0[i0] += v1[i1]);
      break;
    }
    case tc7_cvect: {
      double (*v0)[2] = (double (*)[2])VELTS(ra0);
      double (*v1)[2] = (double (*)[2])VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      v0[i0][0] += v1[i1][0];
	      v0[i0][1] += v1[i1][1];
	    });
      break;
    }
# endif /* FLOATS */
    }
  }
  return 1;
}

int ra_difference(ra0, ras)
     SCM ra0, ras;
{
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  ra0 = ARRAY_V(ra0);
  if NULLP(ras) {
    switch TYP7(ra0) {
    default: {
      SCM e0 = UNDEFINED;
      for (; n-- > 0; i0 += inc0)
	aset(ra0, difference(RVREF(ra0, i0, e0), UNDEFINED), MAKINUM(i0));
      break;
    }
    case tc7_ivect: {
      long *v0 = VELTS(ra0);
      for (; n-- > 0; i0 += inc0)
	v0[i0] = -v0[i0];
      break;
    }
# ifdef FLOATS
#  ifdef SINGLES
    case tc7_fvect: {
      float *v0 = (float *)VELTS(ra0);
      for (; n-- > 0; i0 += inc0)
	v0[i0] = -v0[i0];
      break;
    }
#  endif /* SINGLES */
    case tc7_dvect: {
      double *v0 = (double *)VELTS(ra0);
      for (; n-- > 0; i0 += inc0)
	v0[i0] = -v0[i0];
      break;
    }
    case tc7_cvect: {
      double (*v0)[2] = (double (*)[2])VELTS(ra0);
      for (; n-- > 0; i0 += inc0) {
	v0[i0][0] = -v0[i0][0];
	v0[i0][1] = -v0[i0][1];
      }
      break;
    }
# endif /* FLOATS */
    }
  }
  else {
    SCM ra1 = CAR(ras);
    sizet i1 = ARRAY_BASE(ra1);
    long inc1 = ARRAY_DIMS(ra1)->inc;
    ra1 = ARRAY_V(ra1);
    switch (TYP7(ra0)==TYP7(ra1) ? TYP7(ra0) : 0) {
    ovflow: wta(ra0, (char *)OVFLOW, "-");
    default: {
      SCM e0 = UNDEFINED, e1 = UNDEFINED;
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	aset(ra0, difference(RVREF(ra0, i0, e0), RVREF(ra1, i1, e1)), MAKINUM(i0));
      break;
    }
    case tc7_uvect: {
      unsigned long r;
      unsigned long *v0 = (unsigned long *)VELTS(ra0);
      unsigned long *v1 = (unsigned long*)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      r = v0[i0] - v1[i1];
	      ASRTGO(r <= v0[i0], ovflow);
	      v0[i0] = r;
	    } );
      break;
    }
    case tc7_ivect: {
      long r, *v0 = VELTS(ra0), *v1 = VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      r = v0[i0] - v1[i1];
	      ASRTGO((v0[i0]>0 ? r>=0 || v1[i1]>0 : r<=0 || v1[i1]<0), ovflow);
	      v0[i0] = r;
	    } );
      break;
    }
# ifdef FLOATS
#  ifdef SINGLES
    case tc7_fvect: {
      float *v0 = (float *)VELTS(ra0);
      float *v1 = (float *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      v0[i0] -= v1[i1]);
      break;
    }
#  endif /* SINGLES */
    case tc7_dvect: {
      double *v0 = (double *)VELTS(ra0);
      double *v1 = (double *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      v0[i0] -= v1[i1]);
      break;
    }
    case tc7_cvect: {
      double (*v0)[2] = (double (*)[2])VELTS(ra0);
      double (*v1)[2] = (double (*)[2])VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      v0[i0][0] -= v1[i1][0];
	      v0[i0][1] -= v1[i1][1];
	    })
      break;
    }
# endif /* FLOATS */
    }
  }
  return 1;
}

int ra_product(ra0, ras)
     SCM ra0, ras;
{
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  ra0 = ARRAY_V(ra0);
  if NNULLP(ras) {
    SCM ra1 = CAR(ras);
    sizet i1 = ARRAY_BASE(ra1);
    long inc1 = ARRAY_DIMS(ra1)->inc;
    ra1 = ARRAY_V(ra1);
    switch (TYP7(ra0)==TYP7(ra1) ? TYP7(ra0) : 0) {
    ovflow: wta(ra0, (char *)OVFLOW, "*");
    default: {
      SCM e0 = UNDEFINED, e1 = UNDEFINED;
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	aset(ra0, product(RVREF(ra0, i0, e0), RVREF(ra1, i1, e1)),
	     MAKINUM(i0));
      break;
    }
    case tc7_uvect: {
      unsigned long r;
      unsigned long *v0 = (unsigned long *)VELTS(ra0);
      unsigned long *v1 = (unsigned long *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      r = v0[i0] * v1[i1];
	      ASRTGO(0==v0[i0] || v1[i1]==r/v0[i0], ovflow);
	      v0[i0] = r;
	    } );
      break;
    }
    case tc7_ivect: {
      long r, *v0 = VELTS(ra0), *v1 =VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      r = v0[i0] * v1[i1];
	      ASRTGO(0==v0[i0] || v1[i1]==r/v0[i0], ovflow);
	      v0[i0] = r;
	    } );
      break;
    }
# ifdef FLOATS
#  ifdef SINGLES
    case tc7_fvect: {
      float *v0 = (float *)VELTS(ra0);
      float *v1 = (float *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      v0[i0] *= v1[i1]);
      break;
    }
#  endif /* SINGLES */
    case tc7_dvect: {
      double *v0 = (double *)VELTS(ra0);
      double *v1 = (double *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      v0[i0] *= v1[i1]);
      break;
    }
    case tc7_cvect: {
      double (*v0)[2] = (double (*)[2])VELTS(ra0);
      register double r;
      double (*v1)[2] = (double (*)[2])VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      r = v0[i0][0]*v1[i1][0] - v0[i0][1]*v1[i1][1];
	      v0[i0][1] = v0[i0][0]*v1[i1][1] + v0[i0][1]*v1[i1][0];
	      v0[i0][0] = r;
	    });
      break;
    }
# endif /* FLOATS */
    }
  }
  return 1;
}
int ra_divide(ra0, ras)
     SCM ra0, ras;
{
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  ra0 = ARRAY_V(ra0);
  if NULLP(ras) {
    switch TYP7(ra0) {
    default: {
      SCM e0 = UNDEFINED;
      for (; n-- > 0; i0 += inc0)
	aset(ra0, divide(RVREF(ra0, i0, e0), UNDEFINED), MAKINUM(i0));
      break;
    }
# ifdef FLOATS
#  ifdef SINGLES
    case tc7_fvect: {
      float *v0 = (float *)VELTS(ra0);
      for (; n-- > 0; i0 += inc0)
	v0[i0] = 1.0/v0[i0];
      break;
    }
#  endif /* SINGLES */
    case tc7_dvect: {
      double *v0 = (double *)VELTS(ra0);
      for (; n-- > 0; i0 += inc0)
	v0[i0] = 1.0/v0[i0];
      break;
    }
    case tc7_cvect: {
      register double d;
      double (*v0)[2] = (double (*)[2])VELTS(ra0);
      for (; n-- > 0; i0 += inc0) {
	d = v0[i0][0]*v0[i0][0] + v0[i0][1]*v0[i0][1];
	v0[i0][0] /= d;
	v0[i0][1] /= -d;
      }
      break;
    }
# endif /* FLOATS */
    }
  }
  else {
    SCM ra1 = CAR(ras);
    sizet i1 = ARRAY_BASE(ra1);
    long inc1 = ARRAY_DIMS(ra1)->inc;
    ra1 = ARRAY_V(ra1);
    switch (TYP7(ra0)==TYP7(ra1) ? TYP7(ra0) : 0) {
    default: {
      SCM e0 = UNDEFINED, e1 = UNDEFINED;
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	aset(ra0, divide(RVREF(ra0, i0, e0), RVREF(ra1, i1, e1)), MAKINUM(i0));
      break;
    }
# ifdef FLOATS
#  ifdef SINGLES
    case tc7_fvect: {
      float *v0 = (float *)VELTS(ra0);
      float *v1 = (float *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      v0[i0] /= v1[i1]);
      break;
    }
#  endif /* SINGLES */
    case tc7_dvect: {
      double *v0 = (double *)VELTS(ra0);
      double *v1 = (double *)VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      v0[i0] /= v1[i1]);
      break;
    }
    case tc7_cvect: {
      register double d, r;
      double (*v0)[2] = (double (*)[2])VELTS(ra0);
      double (*v1)[2] = (double (*)[2])VELTS(ra1);
      IVDEP(ra0 != ra1,
	    for (; n-- > 0; i0 += inc0, i1 += inc1) {
	      d = v1[i1][0]*v1[i1][0] + v1[i1][1]*v1[i1][1];
	      r = (v0[i0][0]*v1[i1][0] + v0[i0][1]*v1[i1][1])/d;
	      v0[i0][1] = (v0[i0][1]*v1[i1][0] - v0[i0][0]*v1[i1][1])/d;
	      v0[i0][0] = r;
	    })
      break;
    }
# endif /* FLOATS */
    }
  }
  return 1;
}
static int ra_identity(dst, src)
     SCM src, dst;
{
  return racp(CAR(src), cons(dst, EOL));
}

static int ramap(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  long i = ARRAY_DIMS(ra0)->lbnd;
  long inc = ARRAY_DIMS(ra0)->inc;
  long n = ARRAY_DIMS(ra0)->ubnd;
  long base = ARRAY_BASE(ra0) - i*inc;
  ra0 = ARRAY_V(ra0);
  if NULLP(ras)
    for (; i <= n; i++)
      aset(ra0, apply(proc, EOL, EOL), MAKINUM(i*inc + base));
  else {
    SCM ra1 = CAR(ras);
    SCM args, *ve = &ras;
    sizet k, i1 = ARRAY_BASE(ra1);
    long inc1 = ARRAY_DIMS(ra1)->inc;
    ra1 = ARRAY_V(ra1);
    ras = CDR(ras);
    if NULLP(ras)
      ras = nullvect;
    else {
      ras = vector(ras);
      ve = VELTS(ras);
    }
    for (; i <= n; i++, i1 += inc1) {
      args = EOL;
      for (k = LENGTH(ras); k--;)
	args = cons(aref(ve[k], MAKINUM(i)), args);
      args = cons(cvref(ra1, i1, UNDEFINED), args);
      aset(ra0, apply(proc, args, EOL), MAKINUM(i*inc + base));
    }
  }
  return 1;
}
static int ramap_cxr(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  SCM ra1 = CAR(ras);
  SCM e1 = UNDEFINED;
  sizet i0 = ARRAY_BASE(ra0), i1 = ARRAY_BASE(ra1);
  long inc0 = ARRAY_DIMS(ra0)->inc, inc1 = ARRAY_DIMS(ra1)->inc;
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra1)->lbnd + 1;
  ra0 = ARRAY_V(ra0);
  ra1 = ARRAY_V(ra1);
  switch TYP7(ra0) {
  default: gencase:
    for (; n-- > 0; i0 += inc0, i1 += inc1)
      aset(ra0, apply(proc, RVREF(ra1, i1, e1), listofnull), MAKINUM(i0));
    break;
# ifdef FLOATS
#  ifdef SINGLES
  case tc7_fvect: {
    float *dst = (float *)VELTS(ra0);
    switch TYP7(ra1) {
    default: goto gencase;
    case tc7_fvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	dst[i0] = DSUBRF(proc)((double)((float *)VELTS(ra1))[i1]);
      break;
    case tc7_uvect:
    case tc7_ivect:
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	dst[i0] = DSUBRF(proc)((double)VELTS(ra1)[i1]);
      break;
    }
    break;
  }
#  endif /* SINGLES */
  case tc7_dvect: {
    double *dst = (double *)VELTS(ra0);
    switch TYP7(ra1) {
    default: goto gencase;
    case tc7_dvect:
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	dst[i0] = DSUBRF(proc)(((double *)VELTS(ra1))[i1]);
      break;
    case tc7_uvect:
    case tc7_ivect:
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	dst[i0] = DSUBRF(proc)((double)VELTS(ra1)[i1]);
      break;
    }
    break;
  }
# endif /* FLOATS */
  }
  return 1;
}
static int ramap_rp(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  SCM ra1 = CAR(ras), ra2 = CAR(CDR(ras));
  SCM e1 = UNDEFINED, e2 = UNDEFINED;
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0), i1 = ARRAY_BASE(ra1), i2 = ARRAY_BASE(ra2);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  long inc1 = ARRAY_DIMS(ra1)->inc;
  long inc2 = ARRAY_DIMS(ra2)->inc;
  ra0 = ARRAY_V(ra0);
  ra1 = ARRAY_V(ra1);
  ra2 = ARRAY_V(ra2);
  for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
    if BVE_REF(ra0, i0)
      if FALSEP(SUBRF(proc)(RVREF(ra1, i1, e1), RVREF(ra2, i2, e2)))
	BVE_CLR(ra0, i0);
  return 1;
}
static int ramap_1(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  SCM ra1 = CAR(ras);
  SCM e1 = UNDEFINED;
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0), i1 = ARRAY_BASE(ra1);
  long inc0 = ARRAY_DIMS(ra0)->inc, inc1 = ARRAY_DIMS(ra1)->inc;
  ra0 = ARRAY_V(ra0);
  ra1 = ARRAY_V(ra1);
  if (tc7_vector==TYP7(ra0))
    for (; n-- > 0; i0 += inc0, i1 += inc1)
      VELTS(ra0)[i0] = SUBRF(proc)(cvref(ra1, i1, UNDEFINED));
  else
    for (; n-- > 0; i0 += inc0, i1 += inc1)
      aset(ra0, SUBRF(proc)(RVREF(ra1, i1, e1)), MAKINUM(i0));
  return 1;
}
static int ramap_2o(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  SCM ra1 = CAR(ras);
  SCM e1 = UNDEFINED;
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0), i1 = ARRAY_BASE(ra1);
  long inc0 = ARRAY_DIMS(ra0)->inc, inc1 = ARRAY_DIMS(ra1)->inc;
  ra0 = ARRAY_V(ra0);
  ra1 = ARRAY_V(ra1);
  ras = CDR(ras);
  if NULLP(ras) {
    if (tc7_vector==TYP7(ra0))
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	VELTS(ra0)[i0] = SUBRF(proc)(cvref(ra1, i1, UNDEFINED), UNDEFINED);
    else
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	aset(ra0, SUBRF(proc)(RVREF(ra1, i1, e1), UNDEFINED),
	     MAKINUM(i0));
  }
  else {
    SCM ra2 = CAR(ras);
    SCM e2 = UNDEFINED;
    sizet i2 = ARRAY_BASE(ra2);
    long inc2 = ARRAY_DIMS(ra2)->inc;
    ra2 = ARRAY_V(ra2);
    if (tc7_vector==TYP7(ra0))
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	VELTS(ra0)[i0] =
	  SUBRF(proc)(cvref(ra1, i1, UNDEFINED), cvref(ra2, i2, UNDEFINED));
    else
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	aset(ra0,
	     SUBRF(proc)(RVREF(ra1, i1, e1), RVREF(ra2, i2, e2)),
	     MAKINUM(i0));
  }
  return 1;
}
static int ramap_a(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  SCM e0 = UNDEFINED, e1 = UNDEFINED;
  long n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  sizet i0 = ARRAY_BASE(ra0);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  ra0 = ARRAY_V(ra0);
  if NULLP(ras)
    for (; n-- > 0; i0 += inc0)
      aset(ra0, SUBRF(proc)(RVREF(ra0, i0, e0), UNDEFINED), MAKINUM(i0));
  else {
    SCM ra1 = CAR(ras);
    sizet i1 = ARRAY_BASE(ra1);
    long inc1 = ARRAY_DIMS(ra1)->inc;
    ra1 = ARRAY_V(ra1);
    for (; n-- > 0; i0 += inc0, i1 += inc1)
      aset(ra0, SUBRF(proc)(RVREF(ra0, i0, e0), RVREF(ra1, i1, e1)),
	   MAKINUM(i0));
  }
  return 1;
}

/* These tables are a kluge that will not scale well when more
 vectorized subrs are added.  It is tempting to steal some bits from
 the CAR of all subrs (like those selected by SMOBNUM) to hold an
 offset into a table of vectorized subrs.  */

static ra_iproc ra_rpsubrs[] = {
  {"=", UNDEFINED, ra_eqp},
  {"<", UNDEFINED, ra_lessp},
  {"<=", UNDEFINED, ra_leqp},
  {">", UNDEFINED, ra_grp},
  {">=", UNDEFINED, ra_greqp},
  {0, 0, 0}};
static ra_iproc ra_asubrs[] = {
  {"+", UNDEFINED, ra_sum},
  {"-", UNDEFINED, ra_difference},
  {"*", UNDEFINED, ra_product},
  {"/", UNDEFINED, ra_divide},
  {0, 0, 0}};

static char s_sarray_map[] = "serial-array-map!";
# define s_array_map  (s_sarray_map + 7)
SCM array_map(ra0, proc, lra)
     SCM ra0, proc, lra;
{
  int narg = ilength(lra);
  ASSERT(BOOL_T==procedurep(proc), proc, ARG2, s_array_map);
 tail:
  switch TYP7(proc) {
  wna: wta(UNDEFINED, (char *)WNA, s_array_map);
  default: gencase:
    ramapc(ramap, proc, ra0, lra, s_array_map);
    return UNSPECIFIED;
  case tc7_subr_1: ASRTGO(1==narg, wna);
    ramapc(ramap_1, proc, ra0, lra, s_array_map);
    return UNSPECIFIED;
  case tc7_subr_2: ASRTGO(2==narg, wna);
  case tc7_subr_2o: ASRTGO(2>=narg, wna);
    ramapc(ramap_2o, proc, ra0, lra, s_array_map);
    return UNSPECIFIED;
  case tc7_cxr: if (! SUBRF(proc)) goto gencase;
    ASRTGO(1==narg, wna);
    ramapc(ramap_cxr, proc, ra0, lra, s_array_map);
    return UNSPECIFIED;
  case tc7_rpsubr: {
    ra_iproc *p;
    if (FALSEP(arrayp(ra0, BOOL_T))) goto gencase;
    array_fill(ra0, BOOL_T);
    for (p = ra_rpsubrs; p->name; p++)
      if (proc==p->sproc) {
	while (NNULLP(lra) && NNULLP(CDR(lra))) {
	  ramapc(p->vproc, UNDEFINED, ra0, lra, s_array_map);
	  lra = CDR(lra);
	}
	return UNSPECIFIED;
      }
    while (NNULLP(lra) && NNULLP(CDR(lra))) {
      ramapc(ramap_rp, proc, ra0, lra, s_array_map);
      lra = CDR(lra);
    }
    return UNSPECIFIED;
  }
  case tc7_asubr:
    if NULLP(lra) {
      SCM prot, fill = SUBRF(proc)(UNDEFINED, UNDEFINED);
      if INUMP(fill) {
	prot = array_prot(ra0);
# ifdef FLOATS
	if (NIMP(prot) && INEXP(prot))
	  fill = makdbl((double)INUM(fill), 0.0);
# endif
      }
      array_fill(ra0, fill);
    }
    else {
      SCM tail, ra1 = CAR(lra);
      SCM v0 = (NIMP(ra0) && ARRAYP(ra0) ? ARRAY_V(ra0) : ra0);
      ra_iproc *p;
      /* Check to see if order might matter.
	 This might be an argument for a separate
	 SERIAL-ARRAY-MAP! */
      if (v0==ra1 || (NIMP(ra1) && ARRAYP(ra1) && v0==ARRAY_V(ra1)))
	if (ra0 != ra1 || (ARRAYP(ra0) && !ARRAY_CONTP(ra0)))
	  goto gencase;
      for (tail = CDR(lra); NNULLP(tail); tail = CDR(tail)) {
	ra1 = CAR(tail);
	if (v0==ra1 || (NIMP(ra1) && ARRAYP(ra1) && v0==ARRAY_V(ra1)))
	  goto gencase;
      }
      for (p = ra_asubrs; p->name; p++)
	if (proc==p->sproc) {
	  if (ra0 != CAR(lra))
	    ramapc(ra_identity, UNDEFINED, ra0, cons(CAR(lra), EOL), s_array_map);
	  lra = CDR(lra);
	  while (1) {
	    ramapc(p->vproc, UNDEFINED, ra0, lra, s_array_map);
	    if (IMP(lra) || IMP(CDR(lra))) return UNSPECIFIED;
	    lra = CDR(lra);
	  }
	}
      ramapc(ramap_2o, proc, ra0, lra, s_array_map);
      lra = CDR(lra);
      if NIMP(lra)
	for (lra = CDR(lra); NIMP(lra); lra = CDR(lra))
	  ramapc(ramap_a, proc, ra0, lra, s_array_map);
    }
    return UNSPECIFIED;
#ifdef CCLO
  case tc7_cclo:
    lra = cons(sc2array(proc,ra0,EOL), lra);
    proc = CCLO_SUBR(proc);
    goto tail;
#endif
  }
}

static int rafe(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  long i = ARRAY_DIMS(ra0)->lbnd;
  sizet i0 = ARRAY_BASE(ra0);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  long n = ARRAY_DIMS(ra0)->ubnd;
  ra0 = ARRAY_V(ra0);
  if NULLP(ras)
    for (; i <= n; i++, i0 += inc0)
      apply(proc, cvref(ra0, i0, UNDEFINED), listofnull);
  else {
    SCM ra1 = CAR(ras);
    SCM args, *ve = &ras;
    sizet k, i1 = ARRAY_BASE(ra1);
    long inc1 = ARRAY_DIMS(ra1)->inc;
    ra1 = ARRAY_V(ra1);
    ras = CDR(ras);
    if NULLP(ras)
      ras = nullvect;
    else {
      ras = vector(ras);
      ve = VELTS(ras);
    }
    for (; i <= n; i++, i0 += inc0, i1 += inc1) {
      args = EOL;
      for (k = LENGTH(ras); k--;)
	args = cons(aref(ve[k], MAKINUM(i)), args);
      args = cons2(cvref(ra0, i0, UNDEFINED), cvref(ra1, i1, UNDEFINED), args);
      apply(proc, args, EOL);
    }
  }
  return 1;
}
static char s_array_for_each[] = "array-for-each";
SCM array_for_each(proc, ra0, lra)
     SCM proc, ra0, lra;
{
  ASSERT(BOOL_T==procedurep(proc), proc, ARG1, s_array_for_each);
 tail:
  switch TYP7(proc) {
  default:
    ramapc(rafe, proc, ra0, lra, s_array_for_each);
    return UNSPECIFIED;
#ifdef CCLO
  case tc7_cclo:
    lra = cons(ra0, lra);
    ra0 = sc2array(proc, ra0, EOL);
    proc = CCLO_SUBR(proc);
    goto tail;
#endif
  }
}

static char s_array_imap[] = "array-index-map!";
SCM array_imap(ra, proc)
     SCM ra, proc;
{
  sizet i;
  ASSERT(NIMP(ra), ra, ARG1, s_array_imap);
  ASSERT(BOOL_T==procedurep(proc), proc, ARG2, s_array_imap);
  switch TYP7(ra) {
  default: badarg: wta(ra, (char *)ARG1, s_array_imap);
  case tc7_vector:
    {
      SCM *ve = VELTS(ra);
      for (i = 0; i < LENGTH(ra); i++)
	ve[i] = apply(proc, MAKINUM(i), listofnull);
      return UNSPECIFIED;
    }
  case tc7_string: case tc7_bvect: case tc7_uvect: case tc7_ivect:
  case tc7_fvect: case tc7_dvect: case tc7_cvect:
    for (i = 0; i < LENGTH(ra); i++)
      aset(ra, apply(proc, MAKINUM(i), listofnull), MAKINUM(i));
    return UNSPECIFIED;
  case tc7_smob: ASRTGO(ARRAYP(ra), badarg);
    {
      SCM args = EOL;
      SCM inds = make_uve(ARRAY_NDIM(ra)+0L, MAKINUM(-1L));
      long *vinds = VELTS(inds);
      int j, k, kmax = ARRAY_NDIM(ra) - 1;
      for (k = 0; k <= kmax; k++)
	vinds[k] = ARRAY_DIMS(ra)[k].lbnd;
      k = kmax;
      do {
	if (k==kmax) {
	  vinds[k] = ARRAY_DIMS(ra)[k].lbnd;
	  i = cind(ra, inds);
	  for (; vinds[k] <= ARRAY_DIMS(ra)[k].ubnd; vinds[k]++) {
	    for (j = kmax+1, args = EOL; j--;)
	      args = cons(MAKINUM(vinds[j]), args);
	    aset(ARRAY_V(ra), apply(proc, args, EOL), MAKINUM(i));
	    i += ARRAY_DIMS(ra)[k].inc;
	  }
	  k--;
	  continue;
	}
	if (vinds[k] < ARRAY_DIMS(ra)[k].ubnd) {
	  vinds[k]++;
	  k++;
	  continue;
	}
	vinds[k] = ARRAY_DIMS(ra)[k].lbnd - 1;
	k--;
      } while (k >= 0);
      return UNSPECIFIED;
    }
  }
}

SCM array_equal P((SCM ra0, SCM ra1));
static int raeql_1(ra0, as_equal, ra1)
     SCM ra0, as_equal, ra1;
{
  SCM e0 = UNDEFINED, e1 = UNDEFINED;
  sizet i0 = 0, i1 = 0;
  long inc0 = 1, inc1 = 1;
  sizet n = LENGTH(ra0);
  ra1 = CAR(ra1);
  if ARRAYP(ra0) {
    n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
    i0 = ARRAY_BASE(ra0);
    inc0 = ARRAY_DIMS(ra0)->inc;
    ra0 = ARRAY_V(ra0);
  }
  if ARRAYP(ra1) {
    i1 = ARRAY_BASE(ra1);
    inc1 = ARRAY_DIMS(ra1)->inc;
    ra1 = ARRAY_V(ra1);
  }
  switch TYP7(ra0) {
  case tc7_vector: default:
    for (; n--; i0+=inc0, i1+=inc1) {
      if FALSEP(as_equal) {
	if FALSEP(array_equal(RVREF(ra0, i0, e0), RVREF(ra1, i1, e1)))
	  return 0;
      }
      else
	if FALSEP(equal(RVREF(ra0, i0, e0), RVREF(ra1, i1, e1)))
	  return 0;
    }
    return 1;
  case tc7_string: {
    char *v0 = CHARS(ra0) + i0;
    char *v1 = CHARS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1)
      if (*v0 != *v1) return 0;
    return 1;
  }
  case tc7_bvect:
    for (; n--; i0 += inc0, i1 += inc1)
      if (BVE_REF(ra0, i0) != BVE_REF(ra1, i1)) return 0;
    return 1;
  case tc7_uvect: case tc7_ivect: {
    long *v0 = (long *)VELTS(ra0) + i0;
    long *v1 = (long *)VELTS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1)
      if (*v0 != *v1) return 0;
    return 1;
  }
# ifdef FLOATS
#  ifdef SINGLES
  case tc7_fvect: {
    float *v0 = (float *)VELTS(ra0) + i0;
    float *v1 = (float *)VELTS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1)
      if (*v0 != *v1) return 0;
    return 1;
  }
#  endif /* SINGLES */
  case tc7_dvect: {
    double *v0 = (double *)VELTS(ra0) + i0;
    double *v1 = (double *)VELTS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1)
      if (*v0 != *v1) return 0;
    return 1;
  }
  case tc7_cvect: {
    double (*v0)[2]= (double (*)[2])VELTS(ra0) + i0;
    double (*v1)[2] = (double (*)[2])VELTS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1) {
      if ((*v0)[0] != (*v1)[0]) return 0;
      if ((*v0)[1] != (*v1)[1]) return 0;
    }
    return 1;
  }
# endif /* FLOATS */
  }
}
static int raeql(ra0, as_equal, ra1)
     SCM ra0, as_equal, ra1;
{
  SCM v0 = ra0, v1 = ra1;
  array_dim dim0, dim1;
  array_dim *s0 = &dim0, *s1 = &dim1;
  sizet bas0 = 0, bas1 = 0;
  int k, unroll = 1, ndim = 1;
  if ARRAYP(ra0) {
    ndim = ARRAY_NDIM(ra0);
    s0 = ARRAY_DIMS(ra0);
    bas0 = ARRAY_BASE(ra0);
    v0 = ARRAY_V(ra0);
  }
  else {
    s0->inc = 1; s0->lbnd = 0; s0->ubnd = LENGTH(v0) - 1;
  }
  if ARRAYP(ra1) {
    if (ndim != ARRAY_NDIM(ra1)) return 0;
    s1 = ARRAY_DIMS(ra1);
    bas1 = ARRAY_BASE(ra1);
    v1 = ARRAY_V(ra1);
  }
  else {
    if (1 != ndim) return BOOL_F;
    s1->inc = 1; s1->lbnd = 0; s1->ubnd = LENGTH(v1) - 1;
  }
  if (TYP7(v0) != TYP7(v1)) return 0;
  unroll = (bas0==bas1);
  for (k = ndim; k--;) {
    if (s0[k].lbnd != s1[k].lbnd || s0[k].ubnd != s1[k].ubnd) return 0;
    if (unroll) unroll = (s0[k].inc==s1[k].inc);
  }
  if (unroll && v0==v1) return BOOL_T;
  return ramapc(raeql_1, as_equal, ra0, cons(ra1, EOL), "");
}

SCM raequal(ra0, ra1)
     SCM ra0, ra1;
{
  return (raeql(ra0, BOOL_T, ra1) ? BOOL_T : BOOL_F);
}
static char s_array_equalp[] = "array-equal?";
SCM array_equal(ra0, ra1)
     SCM ra0, ra1;
{
  if (IMP(ra0) || IMP(ra1))
  callequal: return equal(ra0, ra1);
  switch TYP7(ra0) {
  default: goto callequal;
  case tc7_bvect: case tc7_string: case tc7_uvect: case tc7_ivect:
  case tc7_fvect: case tc7_dvect: case tc7_cvect: case tc7_vector:
    break;
  case tc7_smob: if (!ARRAYP(ra0)) goto callequal;
  }
  switch TYP7(ra1) {
  default: goto callequal;
  case tc7_bvect: case tc7_string: case tc7_uvect: case tc7_ivect:
  case tc7_fvect: case tc7_dvect: case tc7_cvect: case tc7_vector:
    break;
  case tc7_smob: if (!ARRAYP(ra1)) goto callequal;
  }
  return (raeql(ra0, BOOL_F, ra1) ? BOOL_T : BOOL_F);
}

static iproc subr2os[] = {
  {s_ura_rd, ura_read},
  {s_ura_wr, ura_write},
  {0, 0}};

static iproc subr2s[] = {
	{s_array_fill, array_fill},
	{s_array_copy, array_copy},
	{s_sarray_copy, array_copy},
	{0, 0}};

static iproc lsubr2s[] = {
  {s_sc2array, sc2array},
  {s_array_map, array_map},
  {s_sarray_map, array_map},
  {s_array_for_each, array_for_each},
  {s_array_imap, array_imap},
  {0, 0}};

static void init_raprocs(subra)
     ra_iproc *subra;
{
  for(; subra->name; subra++)
    subra->sproc = CDR(intern(subra->name, strlen(subra->name)));
}

void init_ramap()
{
  init_raprocs(ra_rpsubrs);
  init_raprocs(ra_asubrs);
  init_iprocs(subr2os, tc7_subr_2o);
  init_iprocs(subr2s, tc7_subr_2);
  init_iprocs(lsubr2s, tc7_lsubr_2);
  make_subr(s_array_equalp, tc7_rpsubr, array_equal);
  smobs[0x0ff & (tc16_array>>8)].equalp = raequal;
  add_feature(s_array_for_each);
}
