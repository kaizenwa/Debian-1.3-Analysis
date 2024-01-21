(*
 * declaration: function function cos(angle: real): real;
 * languages:   ISO-pascal, extended pascal, turbo pascal.
 *
 * declaration: function function cos(angle: complex): complex;
 * languages:   extended pascal
 *
 * remarks:     requires linking to math lib.
 *
 * example:     cos(Pi) = -1
 *
 *)

program CosDemo;

var
  x: real;
  z: complex;

const
  Pi: real = 3.14159265358979323846;

begin
  x := cos(Pi);
  writeln('real   : cos(Pi)  = ', x);

  z := cos(cmplx(2,1));
  writeln('complex: cos(2,1) = (',re(z),',',im(z),')');
end.
