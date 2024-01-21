echo off
echo Batch file to use the NT windows based c++ stubber to create named stubs
echo for Win3.1 test1c example Note that this file won't work from a DOS box 
echo on a win3.1 machine since you can't normally launch window apps from 
echo there. There are some public domain (e.g. 'run' by Frits Wiarda 
echo (run18.zip)) and shareware (e.g. 'unixcorn' by Randall Spangler 
echo (unixcn20.zip) $10) utilities that will let you launch windows apps from
echo a win3.1 DOS box though. Unixcorn is probably the better bet since it 
echo provides an option to wait till the windows app actually returns.
echo on
cpstb32w -batch -sname t1stubs -hname test1 -cname t1clicom -hdrmap test1hh.map Test1.isl
cpstb32w -batch -sname t2stubs -hname test2 -cname t2clicom -hdrmap test1hh.map Test2.isl
cpstb32w -batch -sname t3stubs -hname test3 -cname t3clicom -hdrmap test1hh.map Test3.isl

