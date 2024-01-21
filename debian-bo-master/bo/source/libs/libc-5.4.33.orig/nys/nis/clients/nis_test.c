#include <stdio.h>
#include <rpcsvc/nis.h>

int main(int argc, char *argv[])
{
     nis_result *nres;
     int verbose = 0;
     char *name;


     if (argc < 2)
     {
	 fprintf(stderr, "Usage: nistest [-v] <object>\n");
	 exit(1);
     }

     if (strcmp(argv[1], "-v") == 0)
     {
	 verbose = 1;
	 name = argv[2];
     }
     else
	 name = argv[1];
     
     nres = nis_list(name, EXPAND_NAME+FOLLOW_LINKS+FOLLOW_PATH,
		     NULL, NULL);

     if (nres == NULL)
     {
	 perror("nis_list: System Error");
	 exit(1);
     }

     if (nres->status == NIS_SUCCESS ||
	 nres->status == NIS_S_SUCCESS ||
	 nres->status == NIS_PARTIAL)
     {
	 if (verbose)
	 {
	     if (nres->status == NIS_PARTIAL)
		 printf("************ Only partial result **************\n");
	     nis_print_result(nres);
	 }
	 else
	 {
	     if (nres->status == NIS_PARTIAL)
		 printf("Partial result: %d matches\n",
			nres->objects.objects_len);
	     else
		 printf("Full result: %d matches\n",
			nres->objects.objects_len);
	 }
     }
     else
	nis_perror(nres->status, name);

     exit(0);
}
