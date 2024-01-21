(*
 * declaration: function polar(length, angle: real): complex;
 * languages:   extended pascal
 *
 * remarks:     requires linking to math lib.
 *
 * example:     
 *
 *)

program PolarDemo;

var
  length, angle: real;
  z: complex;

begin
  length:=2;
  angle:=1;

  writeln('length =', length);
  writeln('angle  =', angle);

  z:=polar(length, angle);

  writeln('RE:IM representation : (',re(z),',',im(z),')');
end.
