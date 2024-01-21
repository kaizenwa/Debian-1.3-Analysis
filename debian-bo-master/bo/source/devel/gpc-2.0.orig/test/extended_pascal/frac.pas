(*
 * declaration: frac(r: real): real;
 *
 * languages:   turbo pascal.
 *
 * remarks:     UNIMPLEMENTED
 *
 * example:     frac(Pi) = 0.14159265358979323846
 *
 *)

program FracDemo;

var
  x: real;

const
  Pi: real = 3.14159265358979323846;

begin
  x := frac(Pi);
  writeln('frac(Pi) = ', x);
end.
