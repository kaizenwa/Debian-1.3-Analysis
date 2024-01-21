/* $Id: cppclnt.cpp,v 1.6 1995/12/19 07:04:08 janssen Exp $ */
/* Last edited by Mike Spreitzer December 7, 1995 1:43 pm PST */

#include <stdio.h>

#include "Test1.hh"
#include "Test2.hh"
#include "Test3.hh" 

static void printU(char *prefix, char *varName, Test1_T_U u)
{
    printf("%s%s.discriminator=%d ", prefix, varName, u.discriminator);
    switch (u.discriminator)
    {
    case 3:
	printf("%s.value.O1 = 0x%lx", varName, (unsigned long) u.value.O1);
	break;

    case 5:
	printf("%s.value.boolean = %u", varName, u.value.boolean);
	break;

    default:
	printf("(unexpected value)");
	break;
    }
    printf("\n");
}

int main(int, char **)
{
  Test1Status     s1;
  Test1_T_O1     *handle;
  Test1_T_O2     *o2;
  Test1_T_O3     *o3;
  float           f;
  Test1_T_U       u;
  Test1_T_CSS     css;
  Test1_T_ScS     scs[3];
  Test1_T_RO      ro;
  Test1_T_R       r;

  handle = (Test1_T_O1 *) iluObject::Lookup("Test1-Server",
					    "Test1_Initial_Object",
				       Test1_T_O1::ILUClassRecord);
  if (handle == NULL) {
    fprintf(stderr, "Unable to import server!\n");
    exit(1);
  }
  u.discriminator = 5;
  u.value.boolean = 1;
  scs[0] = "hello world";
  scs[1] = "hello mars";
  css = _Test1_T_CSS_sequence::Create(2, scs);
  u = *handle->U_CSS_to_U(&s1, &u, css);
  if (s1.returnCode != NULL) {
    fprintf(stderr, "U_CSS_to_U => exn %s!\n", s1.returnCode);
    exit(1);
  }
  printU("", "u", u);
  ro = handle->f_CSS_to_RO(&s1, css);
  if (s1.returnCode != NULL) {
    fprintf(stderr, "f_CSS_to_RO => exn %s!\n", s1.returnCode);
    exit(1);
  }
  printf("ro->i=%ld\n", ro->i);

  f = handle->R_ScS_to_F(&s1, ro, scs[0]);
  if (s1.returnCode != NULL) {
    fprintf(stderr, "R_ScS_to_F => exn %s!\n", s1.returnCode);
    exit(1);
  }
  printf("f=%f\n", f);

  handle->a_RO(&s1, ro);
  if (s1.returnCode != NULL) {
    fprintf(stderr, "a_RO => exn %s!\n", s1.returnCode);
    exit(1);
  }
  o2 = handle->get_O2(&s1);
  if (s1.returnCode == Test1Reply_Success) {
    Test1_T_A0      a;
    ilu_Byte       *ap;
    Test1_T_A1      a1;
    Test1_T_I       i;
    Test1_T_CSS     css2;

    printf("got O2, sbh = %s\n", o2->ILUStringBindingHandle());

    css2 = o2->OO_A0_to_CSS(&s1, handle, a);
    if (s1.returnCode == Test2Reply_Success) {
    } else {
      printf("exception on Test1_T_O2::OO_A0_to_CSS, exception is \"%s\"\n",
	     s1.returnCode);
    }

    r.css = new _Test1_T_CSS_sequence;
    r.i = 12;
    r.a[0] = "this is";
    r.a[1] = "data";
    r.a[2] = "initialization";
    a1[0] = "but this";
    a1[1] = "is";
    a1[2] = "fun";
    ap = (ilu_Byte *) o2->R_I_A1_to_I_A0(&s1, &r, &i, a1);
  } else {
    printf("couldn't get an instance of O2.  Exception is \"%s\".\n", s1.returnCode);
  }

  o3 = handle->get_O3(&s1, ilu_FALSE);
  if (s1.returnCode == Test1Reply_Success) {
    Test1_T_RS      rs = _Test1_T_TheRS_sequence::Create(0, NULL);
    Test1_T_IS      i2;

    printf("got O3, sbh = %s, type = %s\n", o3->ILUStringBindingHandle(),
	   o3->ILUClassName());

    if (o3->ILUInstanceClassRecord
	!= ilu::FindClassFromTypeName("Test1.O3")) {
      printf("instance of class %s received!\n", o3->ILUClassName());
    } else {
      i2 = o3->RS_R_to_R_IS(&s1, rs, &r);
      o3->O1_U_to_U(&s1, handle, &u);
      printU("", "u", u);
    }
  } else {
    printf("couldn't get an instance of O3.  Exception is \"%s\".\n",
	   s1.returnCode);
  }

  /* this next call should return an instance of Test3.O */
  o3 = handle->get_O3(&s1, ilu_TRUE);
  if (s1.returnCode == Test1Reply_Success) {
    Test1_T_RS      rs = _Test1_T_TheRS_sequence::Create(0, NULL);
    Test1_T_IS      i2;

    printf("got O3, sbh = %s, type = %s\n",
	   o3->ILUStringBindingHandle(), o3->ILUClassName());

    i2 = o3->RS_R_to_R_IS(&s1, rs, &r);
    o3->O1_U_to_U(&s1, handle, &u);
    printU("", "u", u);

    if (o3->ILUInstanceClassRecord
	== ilu::FindClassFromTypeName("Test3.O")) {
      Test3_T_O      *o;
      Test3Status     s3;
      Test1_T_U       u2;

      o = Test3_T_O::ILUQuaT(o3);
      u2 = *o->I_to_Test1U(&s3, 397);
      if (s3.returnCode != Test3Reply_Success) {
	printf("exception on Test3_O::I_to_Test1U, exception is \"%s\"\n",
	       s3.returnCode);
      } else
	printU("Test3_O::I_to_Test1U:  ", "u2", u2);
    }
  } else {
    printf("couldn't get an instance of O3.  Exception is \"%s\".\n",
	   s1.returnCode);
  }

  /* this next call should return an instance of Test1.O4 */
  o3 = handle->get_O3(&s1, ilu_FALSE);
  if (s1.returnCode == Test1Reply_Success) {
    printf("got O3, sbh = %s, type = %s\n",
	   o3->ILUStringBindingHandle(), o3->ILUClassName());

    if (o3->ILUInstanceClassRecord
	== ilu::FindClassFromTypeName("Test1.O4")) {
      Test1_T_O4     *o4;
      ilu_real        r1, r2;

      o4 = Test1_T_O4::ILUQuaT(o3);
      r2 = o4->R_to_R(&s1, r1 = 12345.6789);
      if (s1.returnCode != Test1Reply_Success) {
	printf("exception on R_to_R, exception is \"%s\"\n",
	       s1.returnCode);
      } else
	printf("doubles:  r1 is %.10f, r2 is %.10f\n", r1, r2);
    }
  } else {
    printf("couldn't get an instance of O3.  Exception is \"%s\".\n",
	   s1.returnCode);
  }

  return 0;
}
