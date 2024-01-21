#include "config.h"

/* editor functions */
#include <stdio.h>
#include <string.h>

#ifdef VMS
# include <ssdef.h>
# include <rmsdef.h>
# include <dvidef.h>
# include <jpidef.h>
/* #include <libdef.h> */
# include <descrip.h>
# include <iodef.h>
# include <ttdef.h>
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "most.h"
#include "edit.h"
#include "window.h"
#include "file.h"
#include "sysdep.h"

static int create_edit_command (char *edit, char *cmd)
{
   int d, s;
   char ch, *p = edit;
   /* Look for %d and %s */
   
   d = s = 0;
   
   while (0 != (ch = *p++))
     {
	if (ch != '%') continue;
	ch = *p++;
	if (!d && (ch == 'd')) 
	  {
	     if (s == 0) d = 1; else d = 2;
	  }
	else if (!s && (ch == 's'))
	  {
	     if (d == 0) s = 1; else s = 2;
	  }
	else 
	  {
	     most_message ("Invalid Editor definition.", 1);
	     return 0;
	  }
     }
   
   
   /* No %d, %s */
   
   if ((d == 0) && (s == 0))
     {
	sprintf (cmd, "%s %s", edit, Most_Buf->file);
     }
   else if (d == 0)
     {
	sprintf (cmd, edit, Most_Buf->file);
     }
   else if (s == 0)
     {
	sprintf (cmd, edit, Most_C_Line);
	strcat (edit, " ");
	strcat (edit, Most_Buf->file);
     }
   else /* d and s */
     {
	if (d == 1)
	  sprintf (cmd, edit, Most_C_Line, Most_Buf->file);
	else sprintf (cmd, edit, Most_Buf->file, Most_C_Line);
     }
   return 1;
}

   
#ifdef VMS
int call_edt_tpu(int tpu)
{
   char the_file[MAX_PATHLEN], *strp;
   extern void edt$edit();
   extern void tpu$tpu();
   struct dsc$descriptor_s  file_desc;
   
   if (tpu == 1)  /* tpu */
      sprintf(the_file,"TPU /START=%d ", Most_C_Line);
   else
      the_file[0] = '\0';
    
    strcat(the_file, Most_Buf->file);
   
   /*  lose the version number */
   strp = the_file;
   while((*strp != '\0') && (*strp != ';')) strp++;
   *strp = '\0';

   file_desc.dsc$w_length = strlen(the_file);
   file_desc.dsc$a_pointer = the_file;
   file_desc.dsc$b_class = DSC$K_CLASS_S; /* scalar, string type */
   file_desc.dsc$b_dtype = DSC$K_DTYPE_T; /* ascii string */

    if (tpu == 1)
      tpu$tpu(&file_desc);
    else
      edt$edit(&file_desc);
}
#endif    

void most_edit_cmd(void)
{
   char *editor;
   char cmd[MAX_PATHLEN + 30];
#ifdef VMS
   int tpu = -1;
#endif

   if ((0 == *Most_Buf->file) || ('*' == *Most_Buf->file)) return;
   
   if (Most_Secure_Mode)
     {
	most_message ("Editing not permitted in secure mode.", 1);
	return;
     }
   
   
   if ((NULL == (editor = getenv("MOST_EDITOR")))
       && (NULL == (editor = getenv("SLANG_EDITOR")))
       && (NULL == (editor = getenv("EDITOR"))))
#ifdef VMS
     editor = "EDT";
#else
     editor = "vi";
#endif
   
#ifdef VMS
   if (!strcmp(editor,"EDT")) tpu = 0;
   else if (!strcmp(editor,"TPU")) tpu = 1;
   else
#endif
   if (0 == create_edit_command(editor, cmd)) return;

   most_reset_tty ();
   most_reset_display ();
   
#ifdef VMS
   if (tpu != -1) call_edt_tpu(tpu);
   else
#endif
   posix_system (cmd);
   
   most_init_tty ();
   most_init_display ();
   most_reread_file ();
   most_redraw_display ();
}



