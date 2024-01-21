@echo off

if %1.==go32. goto config_go32
if %1.==GO32. goto config_go32
if %1.==emx. goto config_emx
if %1.==EMX. goto config_emx

rem Guess GO32/EMX from environment variables
rem (too bad: DOS does not even allow "if %FOO%.!=." !)

if %EMX%.==. goto no_emx
if %DJGPP%.==. goto config_emx
goto ambiguous

:no_emx
if %DJGPP%.==. goto ambiguous
goto config_go32

:ambiguous
echo Usage: configure GO32 or configure EMX
goto END

:config_go32
echo Configuring GNU Pascal for GO32
echo GCC sources and object code are assumed to be in ..\gcc-272

update config\msdos\Makefile Makefile
update config\msdos\Makefile.doc doc\Makefile
update config\msdos\Makefile.rts rts\Makefile
update config\msdos\rts-version.c rts\rts-version.c
update config\msdos\rts-config.h rts\rts-config.h
echo.
echo Now, type `make' to build the compiler and the runtime system.
echo.
goto END

:config_emx
echo GNU Pascal for EMX
echo.
echo Due to the 128-character limitation of command lines it is
echo NOT POSSIBLE to compile GNU Pascal (or GNU C) for EMX with DOS.
echo.
echo If you are running OS/2, please switch to an OS/2 prompt and
echo type `configur' again.
echo.
echo However, you can get GNU Pascal executables for EMX from the same
echo place you got the source.  They run both with DOS and OS/2.
echo.
goto END

:END
