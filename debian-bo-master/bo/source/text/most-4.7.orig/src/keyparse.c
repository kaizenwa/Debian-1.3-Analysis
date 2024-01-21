#include "config.h"

#include <stdio.h>
#include <string.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

/* parses user key definition file */


/* The file is assumed to have a simple format:
 * 
 *   set "keyname"  function
 *   unset "keyname"
 * 
 * For example:
 * 
 *   unsetkey "^K"
 *   setkey exit "^Kx"
 * 
 * Comments extend from the first '%' character to the end of the line.
 */

# include <slang.h>
# include "jdmacros.h"
# include "most.h"
# include "keyparse.h"


static void parse_error (char *s, int n)
{
   fprintf(stderr, "Most: Error reading keymap file on line %d.\n%s\n", n, s);
   most_exit_error (NULL);
}

/* returns 0 upon success, 
 */
static int parse_file (FILE *fp)
{
   char buf[256];
   char tok[256], *tokp;
   int set = 0, n;
   FVOID_STAR f = NULL;

   n = 0;
   while (NULL != fgets (buf, 255, fp))
     {
	n++;
	tokp = buf;
	
	if (SLang_extract_token (&tokp, tok, 0))
	  {
	     if (*tok == '%') continue;
	     if (!strcmp(tok, "setkey")) set = 1;
	     else if (!strcmp(tok, "unsetkey")) set = 0;
	     else 
	       {
		 parse_error ("Expecting 'setkey' or 'unsetkey'", n); 
	       }
	  }
	else continue;
	
	if (set == 1)
	  {
	     if ((0 == SLang_extract_token (&tokp, tok, 0))
		 || SLang_Error || (*tok == '\"'))
	       parse_error ("Expecting function name.", n);
	     
	     if (NULL == (f = SLang_find_key_function(tok, Most_Keymap)))
	       {
		  parse_error ("Undefined function.", n);
	       }
	  }
	
	if ((0 == SLang_extract_token (&tokp, tok, 0))
	    || SLang_Error
	    || (*tok != '"')) 
	  {
	     parse_error ("Expecting a keyname.", n);
	  }
	
	/* knock off double quotes attached to keystring*/
	     *(tok + strlen(tok) - 1) = 0;
	
	if (set == 1)
	  {
	     SLkm_define_key (tok + 1, f, Most_Keymap);
	     if (SLang_Error) parse_error ("Error defining key.", n);
	  }
	else
	  {
	     SLang_undefine_key (tok + 1, Most_Keymap);
	     if (SLang_Error) parse_error ("Error undefining key.", n);
	  }
     }
   return 0;
}

int most_load_user_keymaps (void)
{
#ifndef VMS
   char filebuf[MAX_PATHLEN];
   int len;
#endif
   char *file;
   FILE *fp;
   
   if (NULL == (file = getenv ("MOST_INITFILE")))
     {
#ifdef VMS
	file = "SYS$LOGIN:MOST.RC";
#else
	*filebuf = 0;
	file = getenv ("HOME");
	if (file != NULL) strcpy (filebuf, file);
	file = filebuf;
	
	len = strlen (file);
	if (len && (file[len - 1] == '/')) file[len - 1] = 0;
	strcat (file, "/.mostrc");
#endif
     }
   
   if (NULL == (fp = fopen (file, "r"))) return 1;
   parse_file (fp);
   fclose (fp);
   if (SLang_Error) 
     {
	SLang_Error = 0;
	return -1;
     }
   return 0;
}

