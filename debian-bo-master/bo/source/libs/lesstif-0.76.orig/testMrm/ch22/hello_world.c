/* Written by Dave Brennan.
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* hello_world.c --
 * Initialize X Toolkit creating ApplicationShell widget, then create
 * the user interface described in the hello_world.uid file.
 */

#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>
#include <stdio.h>

/* Global declarations. */
static void quit();

/* Global definitions. */
/* Callback list looks like an action list: */
static MrmRegisterArg callback_list[] = {
    { "quit", (XtPointer) quit },
};

/* error - Print an error message and exit. */
static void
error (message)
char *message;
{
    fprintf (stderr, "hello_world: %s\n", message);
    exit (1);
}

/* quit - The quit callback procedure.  Exits the program. */
static void
quit (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    puts ((char *) client_data);
    exit (0);
}

main (argc, argv)
int argc;
char *argv[];
{
    XtAppContext app_context;
    Widget toplevel, hello_main;
    Cardinal status;
    static String uid_file_list[] = { "hello_world.uid" };
    MrmHierarchy hierarchy;
    MrmType class_code;
    
    MrmInitialize();
    
    toplevel =
        XtVaAppInitialize (&app_context,    /* application context    */
                           "Demos",         /* application class name */
                           NULL, 0,         /* command line options   */
                           &argc, argv,     /* argc and argv          */
                           NULL,            /* fallback resources     */
                           NULL);           /* arg list               */

    status =
        MrmOpenHierarchyPerDisplay ( XtDisplay(toplevel),
			XtNumber (uid_file_list), /* num files */
                           uid_file_list,            /* file list */
                           NULL,                     /* OS data   */
                           &hierarchy);              /* hierarchy */

    if (status != MrmSUCCESS)
        error ("Unable to open hello_world.uid file.");

    status = MrmRegisterNames (callback_list, XtNumber (callback_list));

    if (status != MrmSUCCESS)
        error ("Unable to register callback functions with Mrm.");

    status = MrmFetchWidget (hierarchy,          /* hierarchy to search */
                             "hello_main",       /* object name         */
                             toplevel,           /* parent              */
                             &hello_main,        /* widget created      */
                             &class_code);       /* widget's class code */

    if (status != MrmSUCCESS)
        error ("Unable to create interface from UID file");

    MrmCloseHierarchy (hierarchy);

    XtManageChild (hello_main);
    XtRealizeWidget (toplevel);

    XtAppMainLoop (app_context);
}
