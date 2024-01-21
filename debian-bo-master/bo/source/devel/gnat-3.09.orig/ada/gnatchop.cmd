@echo off
if %1.==. goto usage
setlocal
set parms=
set k8option=
:GET_OPTS
REM Scan through the options, checking for validity and especially looking for
REM the 'k' option since this will be used for the call to gcc to get the offset
REM information.
if %1.==-k. set k8option=-k8 & shift & goto get_opts
if %1.==-s. set parms=%parms% %1 & shift & goto get_opts
if %1.==-w. set parms=%parms% %1 & shift & goto get_opts
if %1.==-r. set parms=%parms% %1 & shift & goto get_opts
if %1.==/k. set k8option=-k8 & shift & goto get_opts
if %1.==/s. set parms=%parms% -s & shift & goto get_opts
if %1.==/w. set parms=%parms% -w & shift & goto get_opts
if %1.==/r. set parms=%parms% -r & shift & goto get_opts
REM Check that there is a filename argument after the option list, and that the
REM file actually exists.
if %1.==. echo missing filename & goto usage
if not exist %1 echo %1 not found & goto end
REM Call gnatf on the source filename argument with special options to generate
REM offset information. If this special compilation completes succesfully call
REM the gnatchp program to actuall split the source file in one file per
REM compilation unit in the optional directory if given otherwise in the current
REM directory.
gnatf -s -u %k8option% %1 >tmpfile
if errorlevel 1 echo Warning: parse errors detected, chop may not be successful
gnatchp %parms% %1 %2 <tmpfile
erase tmpfile
goto end
:USAGE
echo Usage : gnatchop [-k] [-r] [-s] [-w] filename [directory]
echo.
echo   k         limit filenames to 8 characters
echo   r         generate Source_Reference pragmas
echo   s         generate a compilation script
echo   w         overwrite existing filenames
echo   filename  source file
echo   directory directory to place split files (default is current directory)
:END
