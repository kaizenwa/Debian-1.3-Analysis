#include <Xm/PushB.h>
#include <Xm/Frame.h>

Widget toplevel,
       canvas, 
       canvas_frame;


int main(
    int argc, 
    char *argv[])
{
    Arg wargs[100];   
    int n = 0;        

    toplevel = XtInitialize(argv[0], "test3", NULL, 0, &argc, argv); 

    n = 0;
    XtSetArg(wargs[n], XtNwidth, 500); n++;
    XtSetArg(wargs[n], XtNheight, 500); n++;
    XtSetArg(wargs[n], XtNy, 100); n++;
    XtSetArg(wargs[n], XtNx, 200); n++;

    canvas_frame = XtCreateManagedWidget("canvas_frame", 
					 xmFrameWidgetClass, toplevel, 
					 wargs, n);

    canvas = XtCreateManagedWidget("canvas", xmPushButtonWidgetClass,
				   canvas_frame, NULL, 0);

    XtRealizeWidget(toplevel);
    XtMainLoop();
    exit(0);
}


