@echo off
echo running dmake ...
set EMXOPT=-t %EMXOPT%
dmake %1
if errorlevel 1 goto error
cd rts
dmake %1
if errorlevel 1 goto error
cd ..
if X%1X == XinstallX goto end
echo.
echo Compilation complete.  Now, type `make install' to install.
goto end
:error
echo.
echo An error has occured.  Please read README.EMX and the Info 
echo documentation for hints.  Good luck!
:end
