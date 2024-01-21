#ifndef Compat_h
#define Compat_h

#if XtSpecificationRelease <= 4

#  define XrmPermStringToQuark(string)	XrmStringToQuark(string)
#  define XtScreenDatabase(screen)	XtDatabase(DisplayOfScreen(screen))

typedef char *XPointer;

#endif

#endif /* Compat_h */
