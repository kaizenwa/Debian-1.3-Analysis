echo off
echo Batch file to use the NT windows based c stubber to create named stubs
echo for Win3.1 test1c example Note that this file won't work from a DOS box 
echo on a win3.1 machine since you can't normally launch window apps from 
echo there. There are some public domain (e.g. 'run' by Frits Wiarda 
echo (run18.zip)) and shareware (e.g. 'unixcorn' by Randall Spangler 
echo (unixcn20.zip) $10) utilities that will let you launch windows apps from
echo a win3.1 DOS box though. Unixcorn is probably the better bet since it 
echo provides an option to wait till the windows app actually returns.
echo on
cstub32w -batch -tname t1true -sname t1surrgt -hname test1 -cname t1comm -hdrmap test1h.map Test1.isl
cstub32w -batch -tname t2true -sname t2surrgt -hname test2 -cname t2comm -hdrmap test1h.map Test2.isl
cstub32w -batch -tname t3true -sname t3surrgt -hname test3 -cname t3comm -hdrmap test1h.map Test3.isl

