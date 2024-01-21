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

/* "record.c" code for (R5RS) proposed "Record" user definable datatypes.
   Author: Radey Shouman */

#include "scm.h"

typedef struct {
  SCM rtd;
  SCM name;
  SCM fields;
} rtd_type;

typedef union {
  struct {
    SCM proc;
    SCM rtd;
  } pred;
  struct {
    SCM proc;
    SCM rtd;
    SCM index;
  } acc;
  struct {
    SCM proc;
    SCM rtd;
    SCM recsize;
    SCM indices;
  } constr;
} rec_cclo;

long tc16_record;

/* Record-type-descriptor for record-type-descriptors */
static SCM the_rtd_rtd;

/* Record <= [rtd, ... elts ... ] */
#define REC_RTD(x) (VELTS(x)[0])
#define RECP(x) (tc16_record==TYP16(x))
#define RTDP(x) (RECP(x) && the_rtd_rtd==REC_RTD(x))
#define RTD_NAME(x) (((rtd_type *)CDR(x))->name)
#define RTD_FIELDS(x) (((rtd_type *)CDR(x))->fields)
#define RCLO_RTD(x) (((rec_cclo *)CDR(x))->pred.rtd)

#ifdef ARRAYS
# define MAKE_REC_INDS(n) make_uve((long)n, MAKINUM(1))
# define REC_IND_REF(x, i) VELTS(x)[(i)]
# define REC_IND_SET(x, i, val) VELTS(x)[(i)] = (val)
#else
# define MAKE_REC_INDS(n) make_vector(MAKINUM(n), INUM0)
# define REC_IND_REF(x, i) INUM(VELTS(x)[(i)])
# define REC_IND_SET(x, i, val) VELTS(x)[(i)] = MAKINUM(val)
#endif

static char s_record[] = "record";
static char s_recordp[] = "record?";
SCM recordp(obj)
     SCM obj;
{
  return (NIMP(obj) && RECP(obj) ? BOOL_T : BOOL_F);
}
static char s_rec_pred1[] = " record-predicate-procedure";
SCM rec_pred1(cclo, obj)
     SCM cclo, obj;
{
  if (NIMP(obj) && RECP(obj) && (REC_RTD(obj)==RCLO_RTD(cclo)))
    return BOOL_T;
  return BOOL_F;
}
static SCM f_rec_pred1;
static char s_rec_pred[] = "record-predicate";
SCM rec_pred(rtd)
     SCM rtd;
{
  SCM cclo = makcclo(f_rec_pred1, 2L);
  ASSERT(NIMP(rtd) && RTDP(rtd), rtd, ARG1, s_rec_pred);
  RCLO_RTD(cclo) = rtd;
  return cclo;
}

static char s_rec_rtd[] = "record-type-descriptor";
SCM rec_rtd(rec)
     SCM rec;
{
  if (IMP(rec) || !RECP(rec)) return BOOL_F;
  return REC_RTD(rec);
}

static SCM f_rec_constr1;
static char s_rec_constr[] = "record-constructor";
SCM rec_constr(rtd, flds)
     SCM rtd, flds;
{
  SCM flst, fld;
  SCM cclo = makcclo(f_rec_constr1, (long)sizeof(rec_cclo)/sizeof(SCM));
  rec_cclo *ptr = (rec_cclo *)CDR(cclo);
  sizet i, j;
  ASSERT(NIMP(rtd) && RTDP(rtd), rtd, ARG1, s_rec_constr);
  ptr->constr.rtd = rtd;
  i = ilength(RTD_FIELDS(rtd));
  ptr->constr.recsize = MAKINUM(i);
  if UNBNDP(flds) {
    ptr->constr.indices = MAKE_REC_INDS(i);
    while (i--)
      REC_IND_SET(ptr->constr.indices, i, i+1);
  }
  else {
    ASSERT(NIMP(flds) && CONSP(flds), flds, ARG2, s_rec_constr);
    ptr->constr.indices = MAKE_REC_INDS(ilength(flds));
    for(i = 0; NIMP(flds); i++, flds = CDR(flds)) {
      fld = CAR(flds);
      ASSERT(NIMP(fld) && SYMBOLP(fld), fld, ARG2, s_rec_constr);
      flst = RTD_FIELDS(rtd);
      for (j = 0; ; j++, flst = CDR(flst)) {
	if (fld==CAR(flst)) {
	  REC_IND_SET(ptr->constr.indices, i, j+1);
	  break;
	}
	ASSERT(NNULLP(flst), fld, ARG2, s_rec_constr);
      }
    }
  }
  return cclo;
}
static char s_rec_constr1[] = " record-constructor-procedure";
SCM rec_constr1(args)
     SCM args;
{
  SCM cclo = CAR(args);
  SCM rec, inds = (((rec_cclo *)CDR(cclo))->constr.indices);
  sizet i = INUM(((rec_cclo *)CDR(cclo))->constr.recsize);
  args = CDR(args);
  NEWCELL(rec);
  DEFER_INTS;
  SETCHARS(rec, must_malloc((i+1L)*sizeof(SCM), s_record));
  SETNUMDIGS(rec, i+1L, tc16_record);
  ALLOW_INTS;
  while (i--)
    VELTS(rec)[i+1] = UNSPECIFIED;
  REC_RTD(rec) = RCLO_RTD(cclo);
  for (i = 0; i < LENGTH(inds); i++, args = CDR(args)) {
    ASSERT(NNULLP(args), UNDEFINED, WNA, s_rec_constr1);
    VELTS(rec)[ REC_IND_REF(inds, i) ] = CAR(args);
  }
  ASSERT(NULLP(args), UNDEFINED, WNA, s_rec_constr1);
  return rec;

}

/* Makes an accessor or modifier.
   A cclo with 2 env elts -- rtd and field-number. */
static SCM makrecclo(proc, rtd, field, what)
     SCM proc, rtd, field;
     char *what;
{
  SCM flst;
  SCM cclo = makcclo(proc, 3L);
  int i;
  ASSERT(RTDP(rtd), rtd, ARG1, what);
  ASSERT(NIMP(field) && SYMBOLP(field), field, ARG2, what);
  RCLO_RTD(cclo) = rtd;
  flst = RTD_FIELDS(rtd);
  for (i = 1; ; i++) {
    ASSERT(NNULLP(flst), field, ARG2, what);
    if (CAR(flst)==field) break;
    flst = CDR(flst);
  }
  (((rec_cclo *)CDR(cclo))->acc.index) = MAKINUM(i);
  return cclo;
}
static char s_rec_accessor1[] = " record-accessor-procedure";
SCM rec_accessor1(cclo, rec)
     SCM cclo, rec;
{
  ASSERT(NIMP(rec) && RECP(rec), rec, ARG1, s_rec_accessor1);
  ASSERT(RCLO_RTD(cclo)==REC_RTD(rec), rec, ARG1, s_rec_accessor1);
  return VELTS(rec)[ INUM(((rec_cclo *)CDR(cclo))->acc.index) ];
}
static char s_rec_modifier1[] = " record-modifier-procedure";
SCM rec_modifier1(cclo, rec, val)
     SCM cclo, rec, val;
{
  ASSERT(NIMP(rec) && RECP(rec), rec, ARG1, s_rec_modifier1);
  ASSERT(RCLO_RTD(cclo)==REC_RTD(rec), rec, ARG1, s_rec_modifier1);
  VELTS(rec)[ INUM(((rec_cclo *)CDR(cclo))->acc.index) ] = val;
  return UNSPECIFIED;
}
static SCM f_rec_accessor1;
static char s_rec_accessor[] = "record-accessor";
SCM rec_accessor(rtd, field)
     SCM rtd, field;
{
 return makrecclo(f_rec_accessor1, rtd, field, s_rec_accessor);
}
static SCM f_rec_modifier1;
static char s_rec_modifier[] = "record-modifier";
SCM rec_modifier(rtd, field)
     SCM rtd, field;
{
 return makrecclo(f_rec_modifier1, rtd, field, s_rec_accessor);
}

static char s_makrectyp[] = "make-record-type";
SCM *loc_makrtd;
SCM makrectyp(name, fields)
     SCM name, fields;
{
  SCM n;
#ifndef RECKLESS
  if(ilength(fields) < 0)
  errout: wta(fields, (char *)ARG2, s_makrectyp);
  for (n=fields; NIMP(n); n = CDR(n))
    if (!SYMBOLP(CAR(n))) goto errout;
#endif
  return apply(*loc_makrtd, name, cons(fields, listofnull));
}

static SCM markrec(ptr)
     SCM ptr;
{
  sizet i;
  if GC8MARKP(ptr) return BOOL_F;
  SETGC8MARK(ptr);
  for (i = NUMDIGS(ptr); --i;)
    if NIMP(VELTS(ptr)[i]) gc_mark(VELTS(ptr)[i]);
  return REC_RTD(ptr);
}
static sizet freerec(ptr)
     CELLPTR ptr;
{
  must_free(CHARS(ptr));
  return sizeof(SCM)*NUMDIGS(ptr);
}
static int recprin1(exp, port, writing)
     SCM exp, port;
     int writing;
{
  SCM names = RTD_FIELDS(REC_RTD(exp));
  sizet i;
  lputs("#s(", port);
  iprin1(RTD_NAME(REC_RTD(exp)), port, 0);
  for (i = 1; i < NUMDIGS(exp); i++) {
    lputc(' ', port);
    iprin1(CAR(names), port, 0);
    names = CDR(names);
    lputc(' ', port);
    iprin1(VELTS(exp)[i], port, writing);
  }
  lputc(')', port);
/*
  lputs("#<record <", port);
  iprin1(RTD_NAME(REC_RTD(exp)), port, 0);
  lputc('>', port);
  for(i = 1; i < NUMDIGS(exp); i++) {
      lputc(' ', port);
      iprin1(VELTS(exp)[i], port, writing);
    }
  lputc('>', port);
*/
  return 1;
}
SCM recequal(rec0, rec1)
     SCM rec0, rec1;
{
  sizet i = NUMDIGS(rec0);
  if (i != NUMDIGS(rec1)) return BOOL_F;
  if (REC_RTD(rec0) != REC_RTD(rec1)) return BOOL_F;
  while(--i)
    if FALSEP(equal(VELTS(rec0)[i], VELTS(rec1)[i]))
      return BOOL_F;
  return BOOL_T;
}
static smobfuns recsmob = {markrec, freerec, recprin1, recequal};
static iproc subr1s[] = {
  {s_recordp, recordp},
  {s_rec_pred, rec_pred},
  {s_rec_rtd, rec_rtd},
  {0, 0}};
static iproc subr2s[] = {
  {s_rec_accessor, rec_accessor},
  {s_rec_modifier, rec_modifier},
  {s_makrectyp, makrectyp},
  {0, 0}};
static char s_name[] = "name";
static char s_fields[] = "fields";
void init_record()
{
  SCM i_name = CAR(intern(s_name, (sizeof s_name)-1));
  SCM i_fields = CAR(intern(s_fields, (sizeof s_fields)-1));
  tc16_record = newsmob(&recsmob);
  NEWCELL(the_rtd_rtd);
  SETCHARS(the_rtd_rtd, must_malloc((long)sizeof(rtd_type), s_record));
  SETNUMDIGS(the_rtd_rtd, (long)sizeof(rtd_type)/sizeof(SCM), tc16_record);
  REC_RTD(the_rtd_rtd) = the_rtd_rtd;
  RTD_NAME(the_rtd_rtd) = makfromstr(s_record, (sizeof s_record)-1);
  RTD_FIELDS(the_rtd_rtd) = cons2(i_name, i_fields, EOL);
  sysintern("record:rtd", the_rtd_rtd);
  f_rec_pred1 = make_subr(s_rec_pred1, tc7_subr_2, rec_pred1);
  f_rec_constr1 = make_subr(s_rec_constr1, tc7_lsubr, rec_constr1);
  f_rec_accessor1 = make_subr(s_rec_accessor1, tc7_subr_2, rec_accessor1);
  f_rec_modifier1 = make_subr(s_rec_modifier1, tc7_subr_3, rec_modifier1);
  make_subr(s_rec_constr, tc7_subr_2o, rec_constr);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2s, tc7_subr_2);
  sysintern("record-type-descriptor?", rec_pred(the_rtd_rtd));
  sysintern("record-type-name", rec_accessor(the_rtd_rtd, i_name));
  sysintern("record-type-field-names", rec_accessor(the_rtd_rtd, i_fields));
  loc_makrtd = &CDR(sysintern("RTD:make", rec_constr(the_rtd_rtd, UNDEFINED)));
  add_feature(s_record);
}
