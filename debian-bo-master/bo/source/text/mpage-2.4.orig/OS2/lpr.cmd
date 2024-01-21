@echo off
rem *** lpr.cmd - simulate Unix-like print command for a PS printer
rem *** pipes stdin through GhostScript to LPT1
E:\Apps\GS\gsos2 -sDEVICE=cdjmono -r300 -sOutputFile=LPT1 -dNOPAUSE -
