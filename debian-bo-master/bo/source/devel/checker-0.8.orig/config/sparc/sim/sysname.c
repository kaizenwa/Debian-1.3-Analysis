#include <stdio.h>

#define MAX 210
char *names[MAX];

main()
{
 int i,r;
 char n[40];
 for (i = 0; i < MAX; i++)
   names[i] = 0;
 while (!feof (stdin))
   {
     r = scanf("#define %s%d\n", n, &i);
/*     printf ("%d %s %d\n", r, n, i); */
     names[i] = strdup (n);
   }
 printf ("char *sysnames[] = {\n");
 for (i = 0; i < 210; i++)
   if (names[i])
     printf ("\"%s\",\n", names[i]);
   else
     printf ("\"(unknown)\",\n");
 printf ("};\n");
}
