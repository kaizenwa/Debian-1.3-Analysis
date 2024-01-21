/* location detection of Motif routines */

#include <Xm/BaseClassP.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  /* Motif will dump core if this is after the initialize,
   * but if you put this first, you're ok */
  /* MLM so will lesstif, now:) */
  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  _XmInitializeExtensions();

  printf("XmQmotif: %s\n", XrmQuarkToString(XmQmotif));

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
