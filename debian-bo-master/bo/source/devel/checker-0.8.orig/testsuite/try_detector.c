#include "malloc.h"
#include <stdio.h>
#ifndef __ptr_t
#define __ptr_t void *
#endif

#define MESSAGE(t) fprintf(stderr,t) 
/*#define MESSAGE(t) write(2,t,strlen(t))*/
#define PR_ALLOC() MESSAGE("Alloc... ")
#define PR_FREE()  MESSAGE("Free... ");
#define PR_FREEN() MESSAGE("Free...\n")
#define PR_FORGET() MESSAGE("forget this zone...\n")
#define PR_GARABGE() MESSAGE("Call the garbage detector\n");
#define PR_EGARABGE() MESSAGE("Call the evol garbage detector\n");

void wait_key()
{
 fprintf(stderr,"Press return to continue...");
 getchar();
 fputc('\n',stderr);
}

extern char end, etext;
__ptr_t st_zone;

void multiple_alloc(int n, int size)
{
 for(; n>0; n--)
  malloc(size);
}

void main()
{
 __ptr_t au_zone;
 
 MESSAGE("Some information for you:\n");
 printf("&end: 0x%p, &etext: 0x%p, &au_zone: 0x%p\n\n",&end, &etext, &au_zone);
 
 MESSAGE("About static pointer...\n");
 PR_ALLOC();
 st_zone=malloc(100);
 PR_GARABGE();
 __chkr_garbage_detector();
 PR_FORGET();
 st_zone=(__ptr_t)0;
 PR_EGARABGE();
 __chkr_evol_detector();
 wait_key();

 MESSAGE("About automatic pointer...\n"); 
 PR_ALLOC();
 au_zone=malloc(5);
 PR_EGARABGE();
 __chkr_evol_detector();
 PR_FORGET();
 au_zone=(__ptr_t)0;
 PR_EGARABGE();
 __chkr_evol_detector();
 wait_key();
 
 MESSAGE("About heap pointer...\n");
 PR_ALLOC();
 au_zone=malloc(20);
 *((__ptr_t *)au_zone)=malloc(500);
 PR_EGARABGE();
 __chkr_evol_detector();
 PR_FORGET();
 *((__ptr_t *)au_zone)=(__ptr_t)0;
 PR_EGARABGE();
 __chkr_evol_detector();
 wait_key();
 
 MESSAGE("About multiple malloc...\n");
 PR_ALLOC();
 multiple_alloc(20, 128);
 multiple_alloc(5, 6000);
 PR_EGARABGE();
 __chkr_evol_detector();
 wait_key();

/*  __chkr_dump_frag(); */
 exit(0);
}
  