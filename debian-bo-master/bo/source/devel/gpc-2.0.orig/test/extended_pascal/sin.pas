(*
 * declaration: function sin(angle: real): real;
 * languages:   ISO-pascal, extended pascal, turbo pascal.
 *
 * declaration: function sin(z: complex): complex;
 * languages:   extended pascal
 *
 * remarks:     requires linking to math lib.
 *
 * example:     sin(Pi) = 0
 *
 *)

program SinDemo;

var
  x: real;
  z: complex;

const
  Pi: real = 3.14159265358979323846;

begin
  x := sin(Pi);
  writeln('real:    sin(Pi)  = ', x);

  z := sin(cmplx(2,1));
  writeln('complex: sin(2,1) = (',re(z),',',im(z),')');
end.
