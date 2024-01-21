(*
 * declaration: function ln(r: real): real;
 * languages:   ISO-pascal, extended pascal, turbo pascal.
 *
 * declaration: function ln(z: complex): complex;
 * languages:   extended pascal
 *
 * remarks:     requires linking to math lib.
 *
 * example:     ln(E) = 1
 *
 *)

program LnDemo;

var
  x: real;
  z: complex;

const
  E: real = 2.7182818284590452354;

begin
  x := ln(E);
  writeln('real:    ln(E)   = ', x);

  z := ln(cmplx(2,1));
  writeln('complex: ln(2,1) = (',re(z),',',im(z),')');
end.
