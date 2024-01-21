/* This is a simple demo that just loads a file and exits. */

#include "config.h"

#include <stdio.h>
#include <slang.h>

/* Here are two intrinsics that the S-lang code can call. */

/* An intrinsic function to set error */
static void c_error (char *s)
{
   if (SLang_Error == 0) SLang_Error = INTRINSIC_ERROR;
   fprintf (stderr, "Error: %s\n", s);
}

/* Function to quit */
static void c_quit (void)
{
   exit (0);
}

/* Create the Table that S-Lang requires */
SLang_Name_Type Demo_Intrinsics[] =
{
   MAKE_INTRINSIC(".error", c_error, VOID_TYPE, 1),
   MAKE_INTRINSIC(".quit", c_quit, VOID_TYPE, 0),
   SLANG_END_TABLE
};


int main (int argc, char **argv)
{
   
   char *file;

   /* parse command line arguments */
   if (argc != 2) 
     {
	fprintf (stderr, "Usage: %s FILENAME\n", argv[0]);
	exit (-1);
     }
   
   file = argv[1];
   
   /* Initialize the library.  This is always needed. */
   
      if (!init_SLang()		       /* basic interpreter functions */
	  || !init_SLmath() 	       /* sin, cos, etc... */
#ifdef unix
	  || !init_SLunix()	       /* unix system calls */
#endif
	  || !init_SLfiles()	       /* file i/o */
	  
	  /* Now add intrinsics for this application */
	  || !SLang_add_table(Demo_Intrinsics, "Demo"))   
     {
	fprintf(stderr, "Unable to initialize S-Lang.\n");
	exit(-1);
     }

   /* Turn on debugging */
   SLang_Traceback = 1;
   
   /* Now load an initialization file and exit */
   SLang_load_file (file);
   
   return (SLang_Error);
}

   

   
