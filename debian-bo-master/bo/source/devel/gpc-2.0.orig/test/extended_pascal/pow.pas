program PowDemo;

const
  E: real =  2.7182818284590452354;

var
  x: real;
  z: complex;

begin
  x := 2 pow 4;
  writeln('real:    2 pow 4     = ', x);

  x := 2 ** 4;
  writeln('real:    2 ** 4      = ', x);
  writeln;

  x := E pow 10;
  writeln('real:    E pow 10    = ', x);
  
  x := exp(10);
  writeln('real:    exp(10)     = ', x);
  writeln;

  z := cmplx(2,1) pow 2;
  writeln('complex: (2,1) pow 2 = (',re(z),',',im(z),')');
end.
