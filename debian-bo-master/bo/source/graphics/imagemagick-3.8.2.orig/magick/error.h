/*
  Error typedef declarations.
*/
typedef void
  (*ErrorHandler)(const char *,const char *);

/*
  Error declarations.
*/
extern ErrorHandler
  SetErrorHandler(ErrorHandler),
  SetWarningHandler(ErrorHandler);

extern void
  Error(const char *,const char *),
  Warning(const char *,const char *);
