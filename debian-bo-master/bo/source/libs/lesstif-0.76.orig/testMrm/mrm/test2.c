/**
 *
 * LESSTIF/testMrm/mrm/test2.c
 * $Id: test2.c,v 1.2 1996/11/07 23:02:55 miers Exp $
 *
 **/

#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>
#include <stdio.h>

String uid_file_list[] = { "test2a.uid", "test2b.uid", "test2c.uid" };

int
main(int argc, 
     char **argv)
{
    XtAppContext app;
    Widget toplevel, w;
    MrmHierarchy hierarchy;
    Cardinal status;
    MrmCode	cl;
    Pixel	pix;
    XtPointer	value;
    MrmCode	t;
    XColor	c;

    MrmInitialize();
    toplevel = XtVaAppInitialize(&app, "open", NULL, 0,
			         &argc, argv, NULL, NULL);

    status = MrmOpenHierarchyPerDisplay( XtDisplay(toplevel),
					XtNumber (uid_file_list),
					uid_file_list,
					NULL, &hierarchy);
    if (status != MrmSUCCESS) {
	fprintf(stderr, "Unable to open UID files.\n");
	exit(1);
    }

    if (MrmFetchColorLiteral(hierarchy, "green", XtDisplay(toplevel), NULL, &pix) != MrmSUCCESS) {
	fprintf(stderr, "MrmFetchColorLiteral failed\n");
	exit(1);
    }
    fprintf(stderr, "Fetched color 0x%x\n", pix);

    c.pixel = pix;
    XQueryColor(XtDisplay(toplevel), DefaultColormap(XtDisplay(toplevel), 0), &c);
    fprintf(stderr, "Color pixel 0x%x -> r 0x%x g 0x%x b 0x%x\n",
		pix, c.red, c.green, c.blue);

    if (MrmFetchLiteral(hierarchy, "n", XtDisplay(toplevel), &value, &t) != MrmSUCCESS)
	fprintf(stderr, "MrmFetchLiteral failed\n");
    else
	fprintf(stderr, "Fetched value 0x%x, type 0x%x\n", (int)*(int *)value, t);

    MrmCloseHierarchy(hierarchy);
}
