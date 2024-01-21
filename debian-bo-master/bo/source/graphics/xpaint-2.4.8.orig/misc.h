/*
 * Miscellaneous definitions and prototypes, including those from misc.c.
 */

/* $Id: misc.h,v 1.18 1996/06/06 10:45:55 torsten Exp $ */

#if defined(HAVE_PARAM_H)
#include <sys/param.h>
#endif

/*
**  By default everything uses drand48(),
**    I was making more exceptions than inclusions.
 */
#define USE_DRAND

#if defined(SVR4) || defined(__osf__)
#define SHORT_RANGE
#else
#ifndef random
long random(void);
#endif

#ifndef linux
#if defined(BSD4_4) || defined(HPArchitecture) || defined(SGIArchitecture) || defined(_AIX)
void srandom(unsigned int);
#else
int srandom(unsigned int);
#endif	/* BSD4_4 */
#endif	/* linux */
#endif

#ifdef USE_DRAND
#ifdef DECLARE_DRAND48
extern double drand48();
extern long lrand48();
#endif
#define RANDOMI()	lrand48()
#define RANDOMI2(s, f)	(drand48() * ((f) - (s)) + (s))
#define SRANDOM(seed)	srand48((long) (seed))
#else
#ifdef SHORT_RANGE
#define RANGE		0x00000fff
#else
#define RANGE		0x0fffffff
#endif
#define RANDOMI()	random()
#define RANDOMI2(s, f)	(((double)(random() % RANGE) / \
			  (double)RANGE) * ((f) - (s)) + (s))
#define SRANDOM(seed)	srandom((unsigned) (seed))
#endif

/* dialog.c */
void AlertBox(Widget parent, char *msg, XtCallbackProc okProc,
	      XtCallbackProc nokProc, void *data);
void Notice(Widget w,...);

/* fatBitsEdit.c */
void FatCursorSet(Widget w, Pixmap cursor);
void FatCursorAddZoom(int zoom, Widget winwid);
void FatCursorRemoveZoom(Widget winwid);
void FatCursorDestroyCallback(Widget w, XtPointer arg, XtPointer junk);
void FatCursorOff(Widget w);
void FatbitsUpdate(Widget w, int zoom);
void FatbitsEdit(Widget paint);

/* fileName.c */
void *GetFileNameGetLastId(void);
void StdSaveRegionFile(Widget w, XtPointer paintArg, XtPointer junk);
void StdSaveAsFile(Widget w, XtPointer paintArg, XtPointer junk);
void StdSaveFile(Widget w, XtPointer paintArg, XtPointer junk);
void *ReadMagic(char *file);
void *getArgType(Widget w);
void GetFileName(Widget w, int type, char *def,
		 XtCallbackProc okFunc, XtPointer data);

/* fontSelect.c */
void FontSelect(Widget w, Widget paint);

/* grab.c */
#ifdef __IMAGE_H__
Image *DoGrabImage(Widget w, int width, int height);
#endif
void DoGrabPixel(Widget w, Pixel * p, Colormap * cmap);
XColor *DoGrabColor(Widget w);

/* help.c */
char *matchGet(char *line, char *pat);
void HelpDialog(Widget parent, String name);
#ifdef _STDIO_H
void HelpTextOutput(FILE * fd, String name);
#endif
void HelpInit(Widget top);

/* main.c */
void GetDefaultWH(int *w, int *h);
char *GetDefaultRC(void);
void SetIconImage(Widget w);
void main(int argc, char *argv[]);

/* misc.c */
Widget GetToplevel(Widget w);
Widget GetShell(Widget w);
void SetIBeamCursor(Widget w);
void SetCrossHairCursor(Widget w);
void SetPencilCursor(Widget w);
void EnlargePixmap(Display * dpy, Pixmap cursor, int zoom,
		   Pixmap * data, Pixmap * mask);
XRectangle *RectUnion(XRectangle * a, XRectangle * b);
XRectangle *RectIntersect(XRectangle * a, XRectangle * b);
void GetPixmapWHD(Display * dpy, Drawable d, int *wth, int *hth, int *dth);
Pixmap GetBackgroundPixmap(Widget w);
GC GetGCX(Widget w);
void StrToArgv(char *str, int *argc, char **argv);
XImage *NewXImage(Display * dpy, Visual * visual,
		  int depth, int width, int height);
double gauss(void);
int gaussclamp(int range);
void *xmalloc(size_t n);
void AutoCrop(Widget paint);

/* pattern.c */
void PatternEdit(Widget w, Pixmap pix, Widget button);

/* size.c */
void SizeSelect(Widget w, Widget paint, void (*func) (Widget, int, int, int));

/* typeConvert.c */
void InitTypeConverters(void);
