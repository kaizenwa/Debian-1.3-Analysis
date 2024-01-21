/********************************************************************\
**                             _________________________________    **
**   A n t h o n y            |________    __    __    _________|   **
**                                     |  |o_|  |o_|  |             **
**           T h y s s e n           __|   __    __   |__           **
**                                __|   __|  |  |  |__   |__        **
**  `` Dragon Computing! ''    __|   __|     |  |     |__   |__     **
**                            |_____|        |__|        |_____|    **
**                                                                  **
\********************************************************************/
/*
** user_functs.c
**
**   Some of the Application proceedures to perform the actions required
** to control this application.  These routines are called by button presses
** and other user input functions in the ``callbacks.c'' module, and by
** a `user defined menu' calling functions declared in ``user-menu.c'' 
** and implemented below.
**
** NOTE: the busywait routines MUST not be called by any of these routines.
** This is handled by the callback routines and the user menu handler.
*/
#include "xbmbrowser.h"
#include <pwd.h>

void
quit_browser()
/* Quit the application...
** The server de-allocates all pixmaps, colors, and widgets so just exit
** NOTE: This routine could be called directly, as a callback or as an event!
*/
{
  exit(0);
}


void expand_tilder(text)                /* expand in-situ in twiddle */
/*
** Expand Tilder
** This was provided by   Chris McDonald   chris@budgie.cs.uwa.edu.au
** and has been modified to use fast string library functions.
**
** NOTE: home_dir was set to   (char *) getenv("HOME")  in main()
*/
  char *text;
{
  static char buf[MAXNAMLEN];
  char *s, *t, *t1;
  struct passwd *p, *getpwnam();

  s = text;
  while ( *s == ' ' || *s == '\t' ) s++;  /* skip any leading space */
  if ( *s != '~' ) return;                /* if not a tilde -- return */

  /* expand the tilde */
  s++;                                    /* skip leading twiddle */
  t = buf;                                /* copy ~.../ into buf */
  while (*s && *s != '/') *t++ = *s++;
  *t = '\0';
  if(*buf && (p = getpwnam(buf)) == '\0') /* find correct home */
     return;                                 /* error -- return */
  t1 = *buf ? p->pw_dir : home_dir;  
  t = buf;
  strcpy(t, t1);                          /* buf <- home_dir */
  strcat(t, s);                           /* copy rest of text into buf */

  strcpy(text, buf);                      /* copy it back and return it */
}


void
change_dir(dir)
/* Change the current directory to the directory given
** Return false if we failed to enter sub-directory.
*/
  char *dir;
{
  char     newdir[1028];

  strcpy(newdir, dir);     /* save the given variable into a buffer */
  expand_tilder(newdir);   /* if a ~ string expand the tilde */

  /* change the current directory to the new directory */
  if( chdir(newdir) == 0 ) {  /* if success */

    /* get the full path of the new directory */
    (void) getcwd(newdir, MAXNAMLEN);
    if( strcmp(newdir,"/") != 0 )
      (void) strncat(newdir, "/", MAXNAMLEN);

    /* did we actually change directory */
    if( strcmp(dir_name, newdir) != 0 ) {
      strcpy(dir_name, newdir);          /* set the dir_name */
      if( app_data.recursive )           /* Reset resursive option */
        toggle_option(recur_opt, &app_data.recursive, NULL); /* fake it */
      scan_images();                     /* complete scan of icon images */
    }
  }

  /* Set or Reset the dialog widget to the correct directory path */
  XtVaSetValues(dirwidget, XtNvalue, (XtArgVal)dir_name, NULL );
  XtVaSetValues(XtNameToWidget(dirwidget, "value"),
                   XtNinsertPosition, (XtArgVal)strlen(dir_name), NULL);
}


void
exec_string(command)
/* Given a command to execute -- execute it! */
  char *command;
{
  system(command);
}


/* ------------------------------------------------------------------------- */
/* The following is the code to provide user dialog control 
** For this to work the  input()  and  confirm()  routines
** must be declared as input routines to the user-menu module.
*/
#define PARENT  mainpw  /* the main (composite) widget of application */
#define AT_X    50      /* relative position to this window for popup */
#define AT_Y    110

/* the dialog widget is created if and when needed */
static Widget   user_popup  = (Widget) NULL;
static Widget   user_dialog = (Widget) NULL;
static Boolean  user_input  = FALSE;  /* user input field in dialog? */

static void     user_ok();
static XtActionsRec  ok_actions[] = {
/* action_name    routine */
  { "Ok",         user_ok }     /* OK button on dialogs */
};

static char ok_trans[] =
  "<Key>Return:  Ok() \n\
   Ctrl<Key>M:   Ok() ";


static void
user_ok(widget, client_data, call_data )
/* OK button pressed on the user input dialog.
** If any input set copy it into the input string and
** tell the user_menu module to continue the function sequence
*/
  Widget    widget;
  XtPointer client_data, call_data;
{
  if ( user_input ) {
    strncpy(input, XawDialogGetValueString(user_dialog), MAXNAMLEN);
    input[MAXNAMLEN-1] = '\0';  /* just in case */
  }

  /* expand string is first non-space char is a ~ */
  expand_tilder(input);   /* if a ~ string expand the tilde */

  XtPopdown(user_popup);

  /* Now Continue the function sequence of this user menu item (if any) */
  menu_item_continue();
}


static void
user_cancel(widget, client_data, call_data )
/* Cancel button pressed on the user input dialog.
** Tell user-menu module to abort the current function sequence.
*/
  Widget    widget;
  XtPointer client_data, call_data;
{
  XtPopdown(user_popup);
  menu_item_abort();
}


static void
setup_dialog(prompt, value)
/* Create and position the user input/confirm dialog popup
** as required for the current user function.
*/
  char *prompt, *value;
{
  Position x, y;
  Dimension  w;

  /* where to place the widget,  AT_X/Y are constants in xbmbrowser.h */
  XtTranslateCoords(PARENT, AT_X, AT_Y, &x, &y);

  /* If dialog not created -- build it */
  if ( user_popup == NULL ) {
    /* figure out its start width */
    XtVaGetValues(PARENT, XtNwidth, &w, NULL);
    w -= 2 * AT_X;              /* its correct start width */
    w = ( w < 150 ) ? 150 : w;  /* minimum width */
    w = ( w > 600 ) ? 600 : w;  /* maximum width */

    user_popup = XtVaCreatePopupShell(
		   "user_popup", transientShellWidgetClass, PARENT,
		   XtNx,     (XtArgVal)x,
                   XtNy,     (XtArgVal)y,
                   XtNwidth, (XtArgVal)w, NULL);
    user_dialog = XtVaCreateManagedWidget(
		   "user_dialog", dialogWidgetClass, user_popup,
		   XtNvalue, (XtArgVal)"", /* insure text widget created */
                   XtNwidth, (XtArgVal)w, NULL);
    XawDialogAddButton(user_dialog, "Ok",     user_ok,     NULL);
    XawDialogAddButton(user_dialog, "Cancel", user_cancel, NULL);
    XtAppAddActions(XtWidgetToApplicationContext(PARENT),
                   ok_actions, XtNumber(ok_actions) );
  }
  else {
    /* dialog already defined so just position it - again */
    XtVaSetValues(user_popup,  XtNx,     (XtArgVal)x,
                               XtNy,     (XtArgVal)y, NULL);
  }

  /* initialize the initial value of widget */
  XtVaSetValues(user_dialog,
             XtNlabel, (XtArgVal)prompt,   /* set the dialog label */
             XtNvalue, (XtArgVal)value,    /* set the text widget */
             NULL);

  /* reset the width of the input text widget */
  if ( value != NULL ) {
    XtVaGetValues(user_dialog, XtNwidth, (XtArgVal *)&w, NULL);
    XtVaSetValues(XtNameToWidget(user_dialog, "value"),
                               XtNwidth, (XtArgVal)w, NULL);

    /* KLUDGE :- this widget seems to lose its translations if 
    ** the dialog is used for a confirm action since we last used it
    */ 
    XtOverrideTranslations(XtNameToWidget(user_dialog, "value"),
    				   XtParseTranslationTable(ok_trans));
  }
}


void
user_confirm(prompt)
/* Ask the user to confirm the action given.
** This routine is called from the user-menu module as a input
** function. user-menu is expected to stop the current function
** sequence until the user confirms or aborts the sequence.
*/
  char *prompt;
{
  user_input = FALSE;                   /* no input substition on return */
  setup_dialog(prompt, NULL);           /* setup the dialog widget */
  XtPopup(user_popup,XtGrabExclusive);  /* pop up the confirm dialog */

  /* return (user-menu moudle will return to application loop) */
}


void
input_string(prompt, initial)
/* Prompt the user for some input and store it into the input string
** This routine is called from the user-menu module as a input
** function. user-menu is expected to stop the current function
** sequence until the user confirms or aborts the sequence.
*/
  char *prompt, *initial;
{
  user_input = TRUE;                    /* do set input substition string */
  setup_dialog(prompt, initial);        /* setup the dialog widget */
  XtPopup(user_popup,XtGrabExclusive);  /* pop up the confirm dialog */

  /* return (user-menu moudle will return to application loop) */
}


void
file_selected()
/* Check that a file was selected when this menu itme was called.
** If no item selected (IE: the filename %f item is an empty string)
** Then abort the sequence and popup a dialog. It doesn't matter
** if the user selects the OK button as the current menu item sequence
** will have already been aborted.
*/
{
  /* Was a file selected when calling this function sequence */
  if ( file_name[0] == '\0' ) {

    /* Notify the user of abort */
    user_input = FALSE;                   /* no input substition on return */
    setup_dialog( "ABORT -- No File Selected -- ABORT", NULL);
    XtPopup(user_popup,XtGrabExclusive);  /* pop up the confirm dialog */

    /* Abort the sequence in user_menu */
    menu_item_abort();
  }  

  /* return (user-menu moudle will continue or abort as required) */
}


