(*
 * declaration: function arctan(angle: real): real;
 * languages:   ISO-pascal, extended pascal, turbo pascal.
 *
 * declaration: function arctan(z: complex): complex;
 * languages:   extended pascal.
 *
 * remarks:     requires linking to math lib.
 *
 * example:     arctan(Pi) =  1.2626273e+00 [rad].
 *
 *)

program ArcTanDemo;

var
  x: real;
  z: complex;

const
  Pi: real = 3.14159265358979323846;

begin
  x := arctan(Pi);
  writeln('real:    arctan(Pi)  = ', x);

  z := arctan(cmplx(2,1));
  writeln('complex: arctan(2,1) = (',re(z),',',im(z),')');
end.
