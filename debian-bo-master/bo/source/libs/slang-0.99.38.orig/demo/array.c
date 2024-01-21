/* Demonstrates how to create C arrays  that are callable from S-Lang */
#include "config.h"
#include <stdio.h>
#include <slang.h>

/* Here is a global C array that will be accessed from S-Lang */
float64 C_Array[100];

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

   /* Now declare the array to S-Lang.  Lets let it be known 2 ways:
    *   1. As a read/write array called: Vector
    *   2. As a read-only array called: Vector_Read_Only
    */
   
   if ((NULL == SLang_add_array ("Vector",   /* slang name */
				 (long *) C_Array,   /* location of the array */
				 1,      /* number of dimensions */
				 100,    /* number of elements in X direction */
				 0, 0,   /* Number in Y and Z directions */
				 'f',    /* FLoating point array */
				 SLANG_IVARIABLE))   /* Read/Write array */
       
       || 
       (NULL == SLang_add_array ("Vector_Read_Only",   /* slang name */
				 (long *) C_Array,/* location of the array */
				 1,      /* number of dimensions */
				 100,    /* number of elements in X direction */
				 0, 0,   /* Number in Y and Z directions */
				 'f',    /* FLoating point array */
				 SLANG_RVARIABLE)))   /* Readonly array */
     {
	fprintf(stderr, "Failed to add arrays\n");
	exit (-1);
     }
       
   
   /* Code here to do something in C to the arrays, e.g., initialize them */
   
   
   /* Turn on debugging */
   SLang_Traceback = 1;
   
   /* Now load an initialization file and exit */
   SLang_load_file (file);
   
   return (SLang_Error);
}

   

   
