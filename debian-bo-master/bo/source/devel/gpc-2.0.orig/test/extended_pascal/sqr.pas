(*
 * declaration: function sqr(r: real): real;
 * languages:   ISO-pascal, extended pascal, turbo pascal.
 *
 * declaration: function sqr(z: complex): complex;
 * languages:   extended pascal.
 *
 * remarks:     requires linking to math lib.
 *
 * example:     sqr(2.5) = 6.25
 *
 *)

program SqrDemo;

var
  x: real;
  z: complex;

begin
  x := sqr(2.5);
  writeln('real:    sqr(2.5) = ', x);

  z := sqr(cmplx(2,1));

  writeln('complex: sqr(2,1) = (',re(z),',',im(z),')');
end.
