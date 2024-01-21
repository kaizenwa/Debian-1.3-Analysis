(*
 * declaration: function abs(r: real): real;
 * languages:   ISO-pascal, extended pascal, turbo pascal.
 *
 * declaration: function abs(z: complex): real;
 * languages:   extended pascal
 *
 * remarks:     requires linking to math lib.
 *
 * example:     abs(-123.45) = 123.45
 *
 *)

program AbsDemo;

var
  i: integer;
  r: real;

begin
  i := abs(-1234);
  writeln('integer: abs(-1234)    = ', i:1);

  r := abs(-123.456);
  writeln('real:    abs(-123.456) = ', r);

  r := abs(cmplx(2,1));
  writeln('complex: abs(2,1)      = ', r);
end.
