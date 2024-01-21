program log1;

var
  x,y: real;
  
begin
  repeat
    write('x = ');
    readln(x);
  
    y := ln(x);
  
    writeln('ln(x) = ', y:15:7);
  until (x=0);
end.
