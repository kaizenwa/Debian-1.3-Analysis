/* This file runs a full set of tests on the Macintosh version 
   of yorick, although it should be also useful for any other version.
   */

/* run all tests in testp.i */
skip_testb= 0;
skip_test1= 0;
skip_test2= 0;
skip_test3= 0;

/* use the same pass count for all tests */
npass= 20;

/* run the parser test */
#include "testp.i"

/* Run the tests of the math library routines */
#include "testm.i"
testm;

/* Run the linpack benchmark */
#include "testlp.i"
testlp;

/* Run the tests of the graphics package */
include, "testg.i"
lissajous,0;
lissajous,1;
testg;

#include "demo2.i"
demo2;

write, "Do you want to step through all graphics commands interactively?";
if (strtok(rdline(prompt="[y/N]"))(1)=="y") {
  grtest;
  txtest;
}
