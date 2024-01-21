(*
 * declaration: function exp(r: real): real;
 * languages:   ISO-pascal, extended pascal, turbo pascal.
 *
 * declaration: function exp(z: complex): complex;
 * languages:   extended pascal.
 *
 * remarks:     requires linking to math lib.
 *
 * example:     exp(10) = 2.2026466e+04
 *
 *)

program ExpDemo;

var
  x: real;
  z: complex;

begin
  x := exp(10);
  writeln('real:    exp(10)  = ', x);

  z := exp(cmplx(2,1));
  writeln('complex: exp(2,1) = (',re(z),',',im(z),')');
end.
