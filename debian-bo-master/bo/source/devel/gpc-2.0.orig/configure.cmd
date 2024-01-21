@echo off
echo Configuring GNU Pascal for EMX (OS/2)
echo GCC sources and object code are assumed to be in ..\gcc-2.7
echo.
copy config\emx\make.cmd
copy config\emx\Makefile
copy config\emx\gpc-vers.c
copy config\emx\Makefile.rts rts\Makefile
copy config\emx\rts-vers.c rts
copy config\emx\rts-conf.h rts
copy config\emx\gcc_bcmp.c rts
copy config\emx\MakeTest test\Makefile
echo.
echo Now, type `make' to build the compiler and runtime system.
echo.
goto END

:END
