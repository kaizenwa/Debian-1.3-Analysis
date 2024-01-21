#include <stdio.h>
#include <locale.h>

main () {
printf("**********************No environs set***************************\n");
checkit();
printf("\n");

/* Set up LC_CTYPE to KOI-8 and try again */
printf("******************LC_CTYPE environ set to KOI-8*************\n");
setenv ("LC_CTYPE","koi8-r");
checkit();
printf("\n");

/* Set up LC_CTYPE to KOI-8 and try again */
printf("********************LANG environ set to KOI-8***************\n");
unsetenv ("LC_CTYPE");
setenv ("LANG","koi8-r");
checkit();
printf("\n");

/* Now Switch locales */
printf("********************Switching locales back to C******************\n");
unsetenv ("LANG");
setenv ("LC_CTYPE","C");
checkit();
printf("\n");

catclose(catopen("foo",1));
}

checkit() {

/* Set up according to environment vars */
printf("setlocale(LC_ALL,\"\") returned %s\n",setlocale(LC_ALL,""));

/* Set up al to C locale */
printf("setlocale(LC_ALL,\"C\") returned %s\n",setlocale(LC_ALL,"C"));

/* Error no such locale */
printf("setlocale(LC_ALL,\"ISO\") returned %s\n",setlocale(LC_ALL,"ISO"));

/* Set up LC_CTYPE according to environment var */
printf("setlocale(LC_CTYPE,\"\") returned  %s\n",setlocale(LC_CTYPE,""));

/* Get current Locale */
printf("setlocale(LC_MESSAGES,(char *)0) returned %s\n",setlocale(LC_MESSAGES,(char *)NULL));

}
