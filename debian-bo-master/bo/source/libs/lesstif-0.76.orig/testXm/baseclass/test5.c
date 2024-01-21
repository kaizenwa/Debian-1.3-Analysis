/*
 * test5.c -- test XmGetSecondaryResourceData().  Taken from the OSF/Motif
 * programmers manual (page 1-539).
 */
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/BulletinBP.h>

int
main(int argc, char **argv)
{
    XtAppContext AppContext;
    Widget       TopLevel, w, bb;
    XmSecondaryResourceData *block_array;
    Cardinal num_blocks, i, j;
    
    TopLevel = XtAppInitialize(&AppContext, "secres",
                               NULL, 0,
                               &argc, argv,
                               NULL,
                               NULL, 0);

#if 0
    w = XtCreateWidget("text", xmTextWidgetClass, TopLevel, NULL, 0);

    if ((num_blocks = XmGetSecondaryResourceData(xmTextWidgetClass,
#else
    bb = XtCreateWidget("bb", xmBulletinBoardWidgetClass, TopLevel, NULL, 0);
    w = XtCreateWidget("sep", xmSeparatorGadgetClass, bb, NULL, 0);

    if ((num_blocks = XmGetSecondaryResourceData(xmSeparatorGadgetClass,
#endif
						&block_array)) != 0) {
	for (i = 0; i < num_blocks; i++) {
	    for (j = 0; j < block_array[i]->num_resources; j++) {
		printf("%s\n", block_array[i]->resources[j].resource_name);
	    }
	    XtFree((char *)block_array[i]->resources);
	    XtFree((char *)block_array[i]);
	}
	XtFree((char *)block_array);
    }
    exit(0);
}
