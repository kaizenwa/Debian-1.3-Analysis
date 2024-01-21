#include <stdio.h>
#include <malloc.h>
#include <errno.h>

#define SIZE 1000000
#define PASSES 5

/* Test patterns */
#define ALL_ONES 037777777777
#define ALL_ZEROS 0
#define ONE_ZERO 025252525252
#define ZERO_ONE 012525252525

/* Increase as new tests are added. */
#define TESTS 8

main(argc,argv)
int argc;
int argv[];
{

   int *a; 
   int i,j;
   int error,pass_error,passes,pass,tpass;
   int words,success,size;

     switch (argc) {
	case 1:  size=SIZE; tpass=PASSES; break;
	case 2:  size=atoi(argv[1]); tpass=1; break;
	default: size=atoi(argv[1]); tpass=atoi(argv[2]);
     }  
     printf ("Using a memory size of %d 32 bit words (%d Bytes) and %d passes.\n",size,size*sizeof(int),tpass);

    if ((a = (int *)calloc(size, sizeof(int))) == NULL) {
                printf ("calloc() failed errno %d\n", errno);
		exit(1);
    }
    error=pass=0;
    printf (" Array a starts at %d\n",&a[0]);

    for (passes=0; passes<tpass; passes++) {
/* Write forwards */
    	for (i=0;i<size;i++) 
        	a[i]=ALL_ZEROS;
	if (error=error+check(a,ALL_ZEROS,pass++,size)) pass_error++;

    	for (i=0;i<size;i++) 
        	a[i]=ALL_ONES;
	if (error=error+check(a,ALL_ONES,pass++,size)) pass_error++;

    	for (i=0;i<size;i++) 
        	a[i]=ONE_ZERO;
	if (error=error+check(a,ONE_ZERO,pass++,size)) pass_error++;

    	for (i=0;i<size;i++) 
        	a[i]=ZERO_ONE;
	if (error=error+check(a,ZERO_ONE,pass++,size)) pass_error++;

/* Now write backwards */
        for (i=size-1;i>(-1);i--) 
                a[i]=ALL_ZEROS;
        if (error=error+check(a,ALL_ZEROS,pass++,size)) pass_error++;

        for (i=size-1;i>(-1);i--)
                a[i]=ALL_ONES;
        if (error=error+check(a,ALL_ONES,pass++,size)) pass_error++;

        for (i=size-1;i>(-1);i--)
                a[i]=ONE_ZERO;
        if (error=error+check(a,ONE_ZERO,pass++,size)) pass_error++;

        for (i=size-1;i>(-1);i--)
                a[i]=ZERO_ONE;
        if (error=error+check(a,ZERO_ONE,pass++,size)) pass_error++;

    }

    printf ("Found a total of %d errors.\n",error);
    if (error)
       printf ("** Errors occured in %d tests out of %d.\n",pass_error,tpass*TESTS);
    return(0);
}

int check(b,pattern,pass,s)
int *b;
long pattern;
int pass,s;
{
   int z , errors;

/* Put any test corruption here */

/* 		        b[100]=3632862387326;
			b[178281]=97326;
			b[72880]=36326;
			b[62718]=0;
*/


   errors=0;
   for (z=0;z<s;z++)
        if (b[z]!=pattern) {
          printf ("Error at address %d subscript %d, actual %d expected %d\n",&b[z],z,b[z],pattern);
          errors++;
        }
   if (errors) printf ("***^ End of pass %d ^*********\n",pass);
   return (errors);
}

