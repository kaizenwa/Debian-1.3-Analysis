(*
 * declaration: function sqrt(r: real): real;
 * languages:   ISO-pascal, extended pascal, turbo pascal.
 *
 * declaration: function sqrt(z: complex): complex;
 * languages:   extended pascal
 *
 * remarks:     requires linking to math lib.
 *
 * example:     sqrt(2) = 1.4142136
 *
 *)

program SqrtDemo;

var
  x: real;
  z: complex;

begin
  x := sqrt(2);
  writeln('real:    sqrt(2)  = ', x);

  z := sqrt(cmplx(2,1));
  writeln('complex: sqrt(2,1) = (',re(z),',',im(z),')');
end.
