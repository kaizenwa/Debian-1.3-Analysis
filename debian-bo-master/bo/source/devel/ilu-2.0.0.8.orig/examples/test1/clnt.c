/* $Id: clnt.c,v 1.23 1996/07/17 16:00:49 larner Exp $ */
/* Last edited by Mike Spreitzer June 20, 1996 10:14 am PDT */

#include <stdio.h>
#include <stdlib.h>

#include "Test1.h"
#include "Test2.h"
#include "Test3.h"

#include "clnt.h"

#ifdef SECURE_TRANSPORT
#include <ilugssmech_nil.h>
#include <ilugssns_rfc822.h>
#endif /* SECURITY */

static void Print_CSS(Test1_CSS css)
{
  int             i;
  OUTPUT("<");
  for (i = 0; i < css._length; i++)
    OUTPUT("%s\"%s\"", (i ? ", " : ""), css._buffer[i]);
  OUTPUT(">");
}

static void Print_R(Test1_R r)
{
  OUTPUT("[%lu, ", (long unsigned) r.i);
  Print_CSS(r.css);
  OUTPUT(", <\"%s\", \"%s\", \"%s\">]",
	 r.a[0], r.a[1], r.a[2]);
}

static void Print_RO (Test1_RO ro)
{
  if (ro != ILU_NIL)
    Print_R(*ro);
  else
    OUTPUT("nil");
}

static void Print_U(Test1_U u)
{
  switch (u._d) {
  case Test1_TheU__R:
    OUTPUT("R");
    Print_R(u._u.R);
    break;
  case Test1_TheU__RO:
    OUTPUT("RO ");
    Print_RO(u._u.RO);
    break;
  case Test1_TheU__CSS:
    OUTPUT("CSS");
    Print_CSS(u._u.CSS);
    break;
  case Test1_TheU__O1:
    OUTPUT("O1[%s]", ILU_C_SBHOfObject(u._u.O1));
    break;
  case Test1_TheU__OO:
    if (u._u.OO == ILU_NIL)
      OUTPUT("OO[NIL]");
    else
      OUTPUT("OO[%p]", ILU_C_SBHOfObject(u._u.OO));
    break;
  case Test1_TheU__boolean:
    OUTPUT("BOOLEAN[%s]", u._u.boolean ? "TRUE" : "FALSE");
    break;
  default:
    OUTPUT("invalid tag %d", u._d);
  }
}

int doit()
{
  Test1_O1        handle;
  Test1_O2        o2;
  Test1_O3        o3;
  float           f;
  ILU_C_ENVIRONMENT s = {0};
  Test1_U         u, *pu;
  Test1_CSS       *css;
  Test1_ScS       scs[3];
  Test1_RO        ro;
  Test1_R         r;
  ilu_byte       *ap;

#ifdef SECURE_TRANSPORT
  /* we need to call these initializers to make sure any incoming
     secured SBH is parsed properly */
  ilugssmech_nil_initialize();
  ilugssns_rfc822_initialize();
#endif /* SECURITY */

  Test1__Initialize();
  Test3__Initialize();

  if ((handle = ILU_C_LookupObject("Test1-Server", "Test1_Initial_Object",
				   Test1_O1__MSType)) == NULL) {
    fprintf(stderr, "Unable to import object!\n");
    exit(1);
  }
#if 0
  if (ac < 2)
    exit(1);
  if ((handle = Test1_O1__CreateFromSBH(av[1], NULL)) == NULL)
    exit(1);
#endif
  u._d = Test1_U__boolean;
  u._u.boolean = 1;
  scs[0] = "hello world";
  scs[1] = "hello mars";
  css = Test1_CSS_Create(2, scs);
  pu = Test1_O1_U_CSS_to_U(handle, &u, css, &s);
  OUTPUT("Test1.O1.U-CSS-to-U() => ");
  if (!ILU_C_SUCCESSFUL(&s))
    OUTPUT("Exception \"%s\"", s.returnCode);
  else if (!pu)
    OUTPUT("NIL!");
  else
    Print_U(*pu);
  OUTPUT("\n");
  ro = Test1_O1_f_CSS_to_RO(handle, css, &s);
  OUTPUT("Test1.O1.f-CSS-to-RO() => ");
  if (!ILU_C_SUCCESSFUL(&s))
    OUTPUT("Exception \"%s\"", s.returnCode);
  else
    Print_RO(ro);
  OUTPUT("\n");

  f = Test1_O1_R_ScS_to_F(handle, ro, scs[0], &s);
  if (ILU_C_SUCCESSFUL(&s))
    OUTPUT("f=%f\n", f);
  else
    OUTPUT("Test1.O1.R-ScS-to-F() raises \"%s\"\n", s.returnCode);

  Test1_O1_a_RO(handle, ILU_NIL, &s);

  o2 = Test1_O1_get_O2(handle, &s);
  if (ILU_C_SUCCESSFUL(&s)) {
    Test1_A0        a = {9, 0, 2, 1, 0, 255, 16};
    Test1_A1        a1;
    Test1_I         i = 47;
    Test1_CSS       *css2;

    OUTPUT("got O2, sbh = %s\n", ILU_C_SBHOfObject(o2));

    css2 = Test1_O2_OO_A0_to_CSS(o2, handle, a, &s);
    if (!ILU_C_SUCCESSFUL(&s)) {
      OUTPUT("Test1.O2.OO-A0-to-CSS() raises \"%s\"\n",
	     s.returnCode);
    } else if (css2 == NULL)
      OUTPUT("Test1.O2.OO-A0-to-CSS() => NIL!\n");
    else {
      OUTPUT("Test1.O2.OO-A0-to-CSS() => ");
      Print_CSS(*css2);
      OUTPUT("\n");
    }

    r.css._length = 0;
    r.css._buffer = NULL;
    r.css._maximum = 0;
    r.i = 12;
    r.a[0] = "this is";
    r.a[1] = "data";
    r.a[2] = "initialization";
    a1[0] = "but this";
    a1[1] = "is";
    a1[2] = "fun";
    ap = Test1_O2_R_I_A1_to_I_A0(o2, &r, &i, a1, &s);
    if (ILU_C_SUCCESSFUL(&s)) {
      int             j;
      OUTPUT("O2.R-I-A1-to-I-A0() => i=%lu, res=<",
	     (long unsigned) i);
      for (j = 0; j < 8; j++)
	OUTPUT(" %d", ap[j]);
      OUTPUT(">\n");
    } else {
      OUTPUT("O2.R-I-A1-to-I-A0() raises \"%s\"\n", s.returnCode);
    }
  } else {
    OUTPUT("couldn't get an instance of O2.  Exception is \"%s\".\n",
	   s.returnCode);
  }

  o3 = Test1_O1_get_O3(handle, ilu_FALSE, &s);
  if (ILU_C_SUCCESSFUL(&s)) {
    Test1_RS       *rs = Test1_RS_Create(0, NULL);
    Test1_IS       *i2;
    Test1_R        *pr;

    OUTPUT("got O3, sbh = %s, type = %s\n", ILU_C_SBHOfObject(o3),
	   ILU_C_ClassName(o3));

    if (ILU_C_ClassRecordOfInstance(o3)
	!= ILU_C_FindILUClassByTypeName("Test1.O3")) {
      OUTPUT("instance of class %s received!\n", ILU_C_ClassName(o3));
    } else {
      i2 = Test1_O3_RS_R_to_R_IS(o3, rs, &pr, &s);
      if (ILU_C_SUCCESSFUL(&s))
	OUTPUT("Test1.O3.RS-R-to-R-IS succeeds.\n");
      else
	OUTPUT("Test1.O3.RS-R-to-R-IS raises %s\n", s.returnCode);
      Test1_O3_O1_U_to_U(o3, handle, &u, &s);
      if (ILU_C_SUCCESSFUL(&s)) {
	OUTPUT("Test1.O3.O1-U-to-U => ");
	Print_U(u);
	OUTPUT("\n");
      } else {
	OUTPUT("Test1.O3.O1-U-to-U raises %s\n", s.returnCode);
      }
    }
  } else {
    OUTPUT("couldn't get an instance of O3.  Exception is \"%s\".\n",
	   s.returnCode);
  }

  /* this next call should return an instance of Test3.O */
  o3 = Test1_O1_get_O3(handle, ilu_TRUE, &s);
  if (ILU_C_SUCCESSFUL(&s)) {
    Test1_RS       *rs = Test1_RS_Create(0, NULL);
    Test1_IS       *i2;
    Test1_R        *pr;

    OUTPUT("got O3, sbh = %s, type = %s\n", ILU_C_SBHOfObject(o3),
	   ILU_C_ClassName(o3));

    i2 = Test1_O3_RS_R_to_R_IS(o3, rs, &pr, &s);
    if (ILU_C_SUCCESSFUL(&s))
	OUTPUT("Test1.O3.RS-R-to-R-IS succeeds.\n");
      else
	OUTPUT("Test1.O3.RS-R-to-R-IS raises %s\n", s.returnCode);
    Test1_O3_O1_U_to_U(o3, handle, &u, &s);
      if (ILU_C_SUCCESSFUL(&s)) {
	OUTPUT("Test1.O3.O1-U-to-U => ");
	Print_U(u);
	OUTPUT("\n");
      } else {
	OUTPUT("Test1.O3.O1-U-to-U raises %s\n", s.returnCode);
      }

    if (ILU_C_ClassRecordOfInstance(o3)
	== ILU_C_FindILUClassByTypeName("Test3.O")) {
      Test1_U         *u2;

      u2 = Test3_O_I_to_Test1U(o3, 397, &s);
      if (!ILU_C_SUCCESSFUL(&s)) {
	OUTPUT("Test3.O.I-to-Test1U raises \"%s\"\n",
	       s.returnCode);
      } else if (u2 == NULL)
	OUTPUT("Test3.O.I-to-Test1U() => NIL!\n");
      else {
	OUTPUT("Test3.O.I-to-Test1U() => ");
	Print_U(*u2);
	OUTPUT("\n");
      }
    }
  } else {
    OUTPUT("couldn't get an instance of O3.  Exception is \"%s\".\n",
	   s.returnCode);
  }

  /* this next call should return an instance of Test1.O4 */
  o3 = Test1_O1_get_O3(handle, ilu_FALSE, &s);
  if (ILU_C_SUCCESSFUL(&s)) {
    OUTPUT("got O3, sbh = %s, type = %s\n", ILU_C_SBHOfObject(o3),
	   ILU_C_ClassName(o3));

    if (ILU_C_ClassRecordOfInstance(o3)
	== ILU_C_FindILUClassByTypeName("Test1.O4")) {
      ilu_real        r1, r2;

      r2 = Test1_O4_R_to_R(o3, r1 = 12345.6789, &s);
      if (!ILU_C_SUCCESSFUL(&s)) {
	OUTPUT("Test1_O2_OO_A0_to_CSS() raises \"%s\"\n",
	       s.returnCode);
      } else
	OUTPUT("doubles:  r1 is %.10f, r2 is %.10f\n", r1, r2);
    }
  } else {
    OUTPUT("couldn't get an instance of O3.  Exception is \"%s\".\n",
	   s.returnCode);
  }

  return (0);
}
