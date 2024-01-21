/*
    MAIN.C
    A simple but adequate main for a Yorick-based code.

    $Id: main.c,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

typedef void BuiltIn(int);

#ifndef __cplusplus
extern int ym_argc;
extern char **ym_argv;
extern void YMainLoop(void);      /* task.c */

extern void YorickInit(BuiltIn **, void **, char **, char **);
/* Codger-generated arrays from ycode.c used in call to YorickInit */
extern BuiltIn *yBuiltIns[];
extern void *yGlobals[];
extern char *yLinkNames[];
extern char *initialIncludes[];

extern int matherr(int *);
void YError(const char *);        /* ydata.h */
void YWarning(const char *);      /* ydata.h */
#else
/* Note __cplusplus is for version 2.0, c_plusplus for version 1.2 */
extern "C" {
extern int ym_argc;
extern char **ym_argv;
extern void YMainLoop(void);      /* task.c */

extern void YorickInit(BuiltIn **, void **, char **, char **);
/* Codger-generated arrays from ycode.c used in call to YorickInit */
extern BuiltIn *yBuiltIns[];
extern void *yGlobals[];
extern char *yLinkNames[];
extern char *initialIncludes[];

extern int matherr(int *);
void YError(const char *);        /* ydata.h */
void YWarning(const char *);      /* ydata.h */
}
#endif

int main(int argc, char *argv[])
{
  ym_argc= argc;
  ym_argv= argv;
  YorickInit(yBuiltIns, yGlobals, yLinkNames, initialIncludes);
  YMainLoop();
  return 0;
}

/* On UNIX systems, the math library routines defined in math.h call
   matherr to signal many errors -- for Yorick, want this to halt
   the virtual machine immediately.  This is not a part of the
   ANSI C standard.  The function is placed here rather than in
   libyor.a to be sure that it will in fact replace the default
   version in libm.a.  */
#ifndef UNDERFLOW
/* matherr may be called for silly reasons such as underflow or
   loss of precision, which must NOT cause Yorick to abort.
   On SunOS, HPUX, and UNICOS, at least, the type numbers less
   than 4 are serious, while those greater or equal to 4 are not.  */
#define UNDERFLOW 4
#endif
/* ARGSUSED */
int matherr(int *type)
{
  extern int y_catch_category;
  y_catch_category= 0x01;
  if (*type<UNDERFLOW) YError("math library exception handler called");
  return 1;
}
