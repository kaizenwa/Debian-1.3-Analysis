/* This demo illustrates how to add a new type to S-lang's built-in types.
 * In particular, a complex type will be added.
 */

/* 
 * Coding Note:
 * ------------
 * The binary and unary operations that are defined below are coded 
 * for clarity.  A real implementation of complex numbers would want to 
 * exercise care to avoid overflow.  For example, abs(z) is coded as simply
 * sqrt(z.r^2 + z.i^2).  This should be coded in a way that depends upon
 * the relative magnitudes of z.r and z.i, e.g., z.r * sqrt(1 + (z.i/z.r)^2)
 * if |z.r| > |z.i|.
 */

/* The usual include files */
#include "config.h"

#include <stdio.h>
#include <math.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <slang.h>

/* Here is the complex variable type structure.  There is nothing mysterious
 * here. 
 */
typedef struct
{
   double real_part;
   double imag_part;
}
Complex_Type;


/* Since we are creating an application defined type, we must assign it a 
 * type number.  S-Lang reserves numbers less than 128 for itself.  Here, 
 * we will just use 128.  This number will be used when interacting with
 * the S-Lang API.
 */

#define COMPLEX_NUMBER 128

 
/* Here is a creation function for the Complex_Type objects.  Note that I am
 * using the S-Lang macros for malloc and free. 
 */
static SLuser_Object_Type *create_complex_number (double rx, double ix)
{
   Complex_Type *z;
   SLuser_Object_Type *u;
   
   z = (Complex_Type *) SLMALLOC (sizeof (Complex_Type));
   if (z != NULL)
     {
	z->real_part = rx;
	z->imag_part = ix;
	
	/* Now we have to create a user defined object to pass this back to the
	 * interpreter.
	 */
   
	u = SLang_create_user_object (COMPLEX_NUMBER);
	if (u != NULL)
	  {
	     u->obj = (long *) z;
	     return u;
	  }
	
	/* failure */
	SLFREE (z);
     }
   
   SLang_Error = SL_MALLOC_ERROR;
   return NULL;
}

/* This function will be called from S-lang to delete the complex number.  
 * Since we only malloced it, we just free what was malloced.
 */
static void destroy_complex_number (Complex_Type *z)
{
   SLFREE (z);
}

/* Now the following function will be called from S-Lang (see below) to 
 * create a complex number.  The number might be created from 2 integers, 2
 * floats, or an int and a float.  Slang will call this function when the 
 * user uses something like: variable z = complex (3.1, 4.2);
 */
static void complex (void)
{
   double xr, xi;
   int int_x, convert_flag;
   SLuser_Object_Type *z;
   
   /* The imaginary part will be on the top of the stack so lets pop it 
    * first.
    */
   
   if (SLang_pop_float (&xi, &int_x, &convert_flag)) return;
   /* Here convert_flag will be non-zero if an integer has been converted to
    * a float by the above routine.  Here we do not care so we will just 
    * use x and not ix.
    */
   
   if (SLang_pop_float (&xr, &int_x, &convert_flag)) return;
   
   z = create_complex_number (xr, xi);
   if (z == NULL) return;
   
   SLang_push_user_object (z);
}
   

/* The following function is a little helper routine for the one following 
 * it */
static void perform_conversion (unsigned char type, VOID_STAR ap,
				double *r, double *i)
{
   Complex_Type *z;
   
   if (type == COMPLEX_NUMBER)
     {
	z = (Complex_Type *) ap;
	*r = z->real_part;
	*i = z->imag_part;
     }
   else if (type == FLOAT_TYPE)
     {
	*r = *(double *) ap;
	*i = 0.0;
     }
   else /* INT_TYPE */
     {
	*r = (double) *(int *) ap;
	*i = 0.0;
     }
}

/* This function will be called by S-Lang to perform binary operations on the 
 * complex number.  We only have to worry about complex, integers, and floats.
 * The last function 'perform_conversion' takes care of the typecasts.  Note,
 * we could define 3 functions to do the work of this one:
 *   
 *  complex = complex op integer  (or: integer op complex)
 *  complex = complex op double   (or: double op complex)
 *  complex = complex op complex
 * 
 * However, it is easy to handle it all in a single function.
 */
static int complex_binary (int op, unsigned char a_type, unsigned char b_type,
			   VOID_STAR ap, VOID_STAR bp)
{
   double a_real, a_imag, b_real, b_imag, z_real, z_imag;
   double hypotenuse;
   SLuser_Object_Type *z;
   
   perform_conversion (a_type, ap, &a_real, &a_imag);
   perform_conversion (b_type, bp, &b_real, &b_imag);

   switch (op)
     {
      case SLANG_PLUS:		       /* + */
	z_real = a_real + b_real;
	z_imag = a_imag + b_imag;
	break;
	
      case SLANG_MINUS:		       /* - */
	z_real = a_real - b_real;
	z_imag = a_imag - b_imag;
	break;
	
      case SLANG_TIMES:		       /* * */
	z_real = a_real * b_real - a_imag * b_imag;
	z_imag = a_imag * b_real + a_real * b_imag;
	break;
	
      case SLANG_DIVIDE:	       /* / */
	if ((b_real == 0.0) && (b_imag == 0.0))
	  {
	     SLang_Error = DIVIDE_ERROR;
	     return 1;
	  }
	
	hypotenuse = b_real * b_real + b_imag * b_imag;
	
	z_real = (a_real * b_real + a_imag * b_imag) / hypotenuse;
	z_imag = (a_imag * b_real - a_real * b_imag) / hypotenuse;
	break;
	
      case SLANG_EQ: 		       /* == */
	SLang_push_integer ((a_real == b_real) && (a_imag == b_imag));
	return 1;
	
      case SLANG_NE:		       /* != */
	SLang_push_integer ((a_real != b_real) || (a_imag != b_imag));
	return 1;
	
      case SLANG_GT:		       /* > */
      case SLANG_GE:		       /* >= */
      case SLANG_LT:		       /* < */
      case SLANG_LE:		       /* <= */
      default:
	/* Operations not defined on these numbers */
	return 0;
     }
   
   z = create_complex_number (z_real, z_imag);
   if (z != NULL) SLang_push_user_object (z);
   return 1;
}

/* We also want our complex numbers to participate in some unary operations
 * as well.  This function takes care of that.  This function also extends 
 * some of the usual unary functions to complex numbers in a natural way.
 */
static int complex_unary (int op, unsigned char type, Complex_Type *z)
{
   double x, y;
   int i;
   SLuser_Object_Type *uz;
   
   /* Note: here type is not used since this function only applies to complex
    * numbers.
    */
   (void) type;
   
   x = z->real_part;
   y = z->imag_part;
   switch (op)
     {
      case SLANG_CHS: 		       /* z = -z */
	x = -x;	y = -y;
	break;
	
      case SLANG_SQR:		       
	/* Normally sqr(x) produces x * x. Lets be creative and return 
	 * the norm of z squared (|z|^2).
	 */
	SLang_push_float (x * x + y * y);
	return 1;
	
      case SLANG_MUL2:		       /* multiply by 2: mul2(z) */
	x = x * 2.0;  y = y * 2.0;
	break;
	
      case SLANG_ABS:		       /* norm of z */
	SLang_push_float (sqrt (x * x + y * y));
	return 1;
	
	/* Another creative extension.  Lets return an integer which indicates
	 * whether the complex number is in the upperhalf plane or not.
	 */
      case SLANG_SIGN:
	if (y > 0.0) i = 1; else if (y < 0.0) i = -1; else i = 0;
	SLang_push_integer (i);
	return 1;
	
      default:
	/* Undefined */
	return 0;
     }
   
   uz = create_complex_number (x, y);
   if (uz != NULL) SLang_push_user_object (uz);
   return 1;
}

/* We need some way of displaying the complex number in tracebacks, etc...
 * For this, S-Lang requires us to define a function that returns a 
 * MALLOCED string.  It will FREE it after use.   Note:  This function is 
 * not required but it is nice.
 */
static char *complex_string (SLuser_Object_Type *u)
{
   Complex_Type *z;
   char buf[128];
   char *s;
   
   z = (Complex_Type *) (u->obj);
   sprintf (buf, "(%f,%f)", z->real_part, z->imag_part);
   s = (char *) SLMALLOC (strlen (buf) + 1);
   if (s != NULL) strcpy (s, buf);
   return s;
}


/* The necessary routines have been implemented above.  Now, we call S-Lang 
 * to register the new type.
 */
static int register_complex_type (void)
{
   
   /* The first function call registers the new type and its function to 
    * delete the type.  Creating the type is handled differently.
    */
   if (!SLang_register_class (COMPLEX_NUMBER, 
			      (FVOID_STAR) destroy_complex_number, 
			      (FVOID_STAR) complex_string))
     return 0;
   
   /* Now define binary operators for this type. The binary operations
    * are between complex, float, and integer types.  Note the comment 
    * preceeding the complex_binary function.
    */
   if (!SLang_add_binary_op (COMPLEX_NUMBER, COMPLEX_NUMBER, 
			     (FVOID_STAR) complex_binary)
       ||!SLang_add_binary_op (COMPLEX_NUMBER, FLOAT_TYPE,
			       (FVOID_STAR) complex_binary)
       ||!SLang_add_binary_op (COMPLEX_NUMBER, INT_TYPE,
			       (FVOID_STAR) complex_binary))
     return 0;
   
   /* Finally, add the unary operator definition */
   if (!SLang_add_unary_op (COMPLEX_NUMBER, (FVOID_STAR) complex_unary))
     return 0;
   
   return 1;
}

/* Now lets define two functions to return the real and imaginary parts
 * of a complex number.  This will illustrate how get objects from the 
 * SLang stack.  Note that you must call a S-Lang routine to free them 
 * when you are finished with them.  The reason that you have to free them 
 * is because S-Lang did not pass them as arguments.
 */
static double real_part (void)
{
   SLuser_Object_Type *u;
   Complex_Type *z;
   double x;
   
   if (NULL == (u = SLang_pop_user_object (COMPLEX_NUMBER))) return 0.0;
   z = (Complex_Type *) (u->obj);
   x = z->real_part;
   SLang_free_user_object (u);
   return x;
}

static double imag_part (void)
{
   SLuser_Object_Type *u;
   Complex_Type *z;
   double y;
   
   if (NULL == (u = SLang_pop_user_object (COMPLEX_NUMBER))) return 0.0;
   z = (Complex_Type *) (u->obj);
   y = z->imag_part;
   SLang_free_user_object (u);
   return y;
}

/* Everthing below here is standard stuff.  It consists of the intrinsic
 * table and main.
 */

/* Function to quit */
static void c_quit (void)
{
   exit (0);
}

static void c_error (char *s)
{
   if (SLang_Error == 0) SLang_Error = INTRINSIC_ERROR;
   fprintf (stderr, "Error: %s\n", s);
}

/* Create the Table that S-Lang requires */
SLang_Name_Type Demo_Intrinsics[] =
{
   MAKE_INTRINSIC(".error", c_error, VOID_TYPE, 1),
   MAKE_INTRINSIC(".quit", c_quit, VOID_TYPE, 0),
   MAKE_INTRINSIC(".Real", real_part, FLOAT_TYPE, 0),
   MAKE_INTRINSIC(".Imag", imag_part, FLOAT_TYPE, 0),
   MAKE_INTRINSIC(".Complex", complex, VOID_TYPE, 0),
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
	  || !SLang_add_table(Demo_Intrinsics, "Demo")
	  /* register the complex type */
	  || !register_complex_type ())
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
