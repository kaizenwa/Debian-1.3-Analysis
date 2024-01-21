@setlocal
@set emxopt=-t %emxopt%
nmake LANGUAGES=c CC=\emx\bin\gcc XCFLAGS=-B/emx/bin/ CFLAGS=-g >dostage1.out 2>&1
@endlocal
