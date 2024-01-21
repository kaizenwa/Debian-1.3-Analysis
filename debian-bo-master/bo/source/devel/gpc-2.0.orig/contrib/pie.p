
#include "xbgi.ph"

Program PieChart (Input,Output);
{ uses
    graph,crt; }

import BgiGraphics;

const
  maxinx = 10;

var
  data : array [ 1..maxinx ] of record
				  d : integer;	
				  n : string(30);
				end;

function filldata : integer;
begin
  data[1].d := 55;  
  data[2].d := 20;  
  data[3].d := 10;  
  data[4].d := 4;  
  data[5].d := 3;  
  data[6].d := 3;  
  data[7].d := 2;  
  data[8].d := 2;  
  data[9].d := 2;  
  data[10].d := 5;  

  data[1].n := 'Tekn. Yo.';  
  data[2].n := 'Yo';  
  data[3].n := 'Ins';  
  data[4].n := 'Asent';  
  data[5].n := 'Fil. Yo.';  
  data[6].n := 'Kaupt.Yo';  
  data[7].n := 'Valt.Yo.';  
  data[8].n := 'Arkkit.Yo';  
  data[9].n := 'Ei mitaan';  
  data[10].n := 'Muut';  

  filldata := maxinx;
end;

type
  stype = packed array [ 1..255 ] of char;

var
  Driver : integer;
  mode   : integer;
  foo    : stype;
  cptr   : ^char;

var
  total : real value 0.0;
  y,x,r : integer value 0;
  i,lkm : integer;
  j     : real value 0.0;
  ch    : char;

Begin
  detectgraph(driver,mode);

  lkm := filldata;
  for i := 1 to lkm do
    total := total + data[i].d;

  initgraph(driver,mode,foo);
  y := trunc(getmaxy/2);
  x := y;
  r := y;

  settextstyle (0, 0, 4);

  for i := 1 to lkm do
  begin
   setlinestyle (0, 0, 3);
   setfillstyle(1,i);
   pieslice(trunc(x+x/10),
	    y,
            trunc(j), 
	    trunc(j+(data[i].d*360)/total),
	    r);
   j := j + ((data[i].d*360)/total);
   bar3d(trunc(x*2+3*x/10),
	 trunc(r*2/lkm*(i-1)),
	 trunc(x*2+4*x/10),
	 trunc(r*2/lkm*i),
	 0,
	 0);

  writestr (foo, data[i].n,' ',trunc((data[i].d*100)/total):2);
  outtextxy(trunc(2*x+5*x/10+5),trunc(r*2/lkm*(i-1)+3),foo);
  end;
  read(ch);
  closegraph;
end.
