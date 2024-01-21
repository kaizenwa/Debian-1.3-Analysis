/* test for multi line labels */

#include <Xm/XmP.h>
#include <XmI/TraversalI.h>
#include <Xm/PushB.h>
#include <stdio.h>

void
cb(Widget w, XtPointer data, XtPointer cbs)
{
    printf("Activated\n");
}

void
DumpNode(int which, XmTravTreeNode node)
{
    printf("  %d: addr: %p type: %d nav_type: %d tab_parent: %p\n",
	   which, node, node->type, node->nav_type, node->tab_parent.link);
    printf("  %d: widget: %s rect: %d %d %d %d\n",
	   which, XtName(node->widget), node->rect.x, node->rect.y,
	   node->rect.width, node->rect.height);
    printf("  %d: next: %p prev: %p up: %p down: %p\n",
	   which, node->next, node->prev, node->up, node->down);
    printf("\n");
}

void
DumpTree(XmTravTree tree) {
    int i;

    printf("Tree: Widget: %s current: %p num_entries: %d\n",
	   XtName(tree->shell), tree->current, tree->num_entries);
    printf("      num_alloc: %d next_alloc: %d num_excls: %d\n",
	   tree->num_alloc, tree->next_alloc, tree->num_excls);
    printf("      num_tab_alloc: %d num_tab_entries: %d\n",
	   tree->num_tab_alloc, tree->num_tab_entries);

    printf("Exclusive/tabs\n");
    for (i = 0; i < tree->num_tab_entries; i++)
	printf("  %d: %s\n", i, XtName(tree->excl_tabs[i]));

    printf("Nodes:\n");
    for (i = 0; i < tree->num_entries; i++)
	DumpNode(i, &tree->head[i]);
}

void
DumpFocusData(XmFocusData fd) {
    printf("FocusData: active_tab %p focus_item %p old_focus %p\n",
	   fd->active_tab_group, fd->focus_item, fd->old_focus_item);
    printf("           pointer_item: %p old_pointer: %p\n",
	   fd->pointer_item, fd->old_pointer_item);
    printf("           flush: %d focal_point: %d first_focus: %p\n",
	   fd->flush, fd->focal_point, fd->first_focus);
    printf("           focus_policy: %d\n", fd->focus_policy);
    DumpTree(&fd->tree);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nLabel", "MY_FONT");

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel, XmNfontList, fontlist, 
				XmNlabelString, xmstr1,
				XtNborderWidth, 20,
				NULL);

  XtAddCallback(one, XmNactivateCallback, cb, NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
