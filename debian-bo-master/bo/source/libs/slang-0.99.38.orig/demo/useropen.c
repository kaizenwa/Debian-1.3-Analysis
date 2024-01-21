/* This demo indicates how to read and parse a S-Lang file by bypassing the 
 * built-in routines.
 */
#include "config.h"
#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#include <slang.h>

/* Suppose that you want to read input using a read line package 
 * such as one provided by S-Lang.  For generality, lets assume that this
 * function is called 'readline' and it is prototyped as:
 *   int readline (char *prompt, char *buffer);
 * where it returns the number of characters read and -1 if end of file.  The
 * first parameter is a prompt and the second represents the buffer where the
 * characters is read.  Also assume that this routine requires that the 
 * function 'init_readline' be called first before it can be used and 
 * 'reset_readline' must be called after using it.
 * 
 * The goal here is to get S-Lang to call the readline function.
 */

/* For the purposes of this demo, we will use just fgets */

static int readline (char *prompt, char *buf)
{
   fputs (prompt, stdout);  fflush (stdout);
   if (NULL == fgets (buf, 255, stdin)) return -1;
   return strlen (buf);
}

static int init_readline (void)
{
   puts ("Initializing readline."); fflush (stdout);
   return 1;
}

static void reset_readline (void)
{
   puts ("Resetting readline."); fflush (stdout);
}
 
/* The secret is to point the two global variables:
 * 
 *   SLang_User_Open_Slang_Object
 *   SLang_User_Close_Slang_Object
 * 
 * to your own routines.  Then use the S-Lang function 'SLang_load_object'
 * with the appropriate argument to call your routines.
 */

/* First of all, create the user open routine.  We will point 
 * SLang_User_Open_Slang_Object to it.  This routine must return 0 if 
 * sucessful, SL_OBJ_NOPEN if it tried to open it but failed or 
 * SL_OBJ_UNKNOWN if it does not handle the object.
 */

static int open_readline (SLang_Load_Type *x)
{
   switch (x->type)
     {
	/* look for the readline type.  We will use 'r' to represent our 
	 * readline routines.  S-Lang reserves numbers less than or equal to 
	 * 'Z' for itself.
	 */
      case 'r':
	if ((-1 == init_readline ())
	    || (NULL == (x->buf = (char *) SLMALLOC (256))))
	  return SL_OBJ_NOPEN;   /* failure */
	
	x->ptr = "Readline> ";	       /* use one of the allowed fields
					* of the structure for our prompt.
					*/
	/* success */
	return 0;		 
	
      default:
	/* unrecognized-- let S-lang handle it */
	return SL_OBJ_UNKNOWN;
     }
}

/* Now we have to write a routine to call 'reset_readline'.  It looks like 
 * the one above.
 */
static int close_readline (SLang_Load_Type *x)
{
   switch (x->type)
     {
      case 'r':
	reset_readline ();
	SLFREE ((char *) x->buf);
	return 0;
	
      default:
	/* unrecognized-- let S-lang handle it */
	return SL_OBJ_UNKNOWN;
     }
}


/* Now lets define the function that S-Lang will use to actually read the data.
 * It calls readline.  S-Lang will call this function and the function must 
 * return a pointer to the buffer containg the characters of the line or NULL
 * upon end of file.  In many ways, it is like fgets except that it is passed
 * a pointer to SLang_Load_Type in stead of FILE.
 */

static char *read_using_readline (SLang_Load_Type *x)
{
   int n;
   
   /* by construction (see below), tha ptr field is a pointer to our
    * prompt.
    */
   n = readline (x->ptr, x->buf);

   if (n <= 0) return NULL;
   return x->buf;
}

/* Now, we all of this is tied together in this routine which will be called
 * from main below.
 */

static void read_input (void)
{
   SLang_Load_Type x;
   
   x.type = 'r';		       /* readline type */
   x.read = read_using_readline;       /* function to call to perform the read */
   SLang_load_object (&x);
}

/* Now here is are some intrinsic functions */

int main (int argc, char **argv)
{
   /* usual stuff */
   
   (void) argc; (void) argv;
   
   if (!init_SLang()		       /* basic interpreter functions */
       || !init_SLmath() 	       /* sin, cos, etc... */
#ifdef unix
       || !init_SLunix()	       /* unix system calls */
#endif
       || !init_SLfiles())	       /* file i/o */
     {
	fprintf(stderr, "Unable to initialize S-Lang.\n");
	exit(-1);
     }
   
   /* Make the slang function pointers point to our routines */
   SLang_User_Open_Slang_Object = open_readline;
   SLang_User_Close_Slang_Object = close_readline;
   
   /* Define a print function */
   SLang_load_string ("define print (x) {() = fputs(string(x), stdout); () = fflush (stdout);}");
   
   read_input ();
   return SLang_Error;
}



 
