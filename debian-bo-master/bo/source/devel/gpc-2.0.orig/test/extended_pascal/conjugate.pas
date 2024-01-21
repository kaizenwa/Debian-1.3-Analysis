(*
 * declaration: function conjugate(z: complex): complex;
 * languages:   GPC extension
 *
 * remarks:     requires linking to math lib.
 *
 * example:     conjugate(2,1) = (2,-1)
 *
 *)

program ConjDemo;

var
  z: complex;

begin
  z := conjugate(cmplx(2,1));
  writeln('complex conjugate of (2,1) = (',re(z),',',im(z),')');
end.
