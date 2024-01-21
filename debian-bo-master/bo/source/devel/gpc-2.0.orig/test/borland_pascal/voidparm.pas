Program VoidParm;


Type
  TwoBytes = packed array [ 1..2 ] of Char;


Var
  KO, OK: TwoBytes;


Procedure move ( Const Source; Var Dest );

Var
  S: TwoBytes absolute Source;
  D: TwoBytes absolute Dest;

begin (* move *)
  D:= S;
end (* move *);


begin
  KO [ 2 ]:= 'K';
  KO [ 1 ]:= 'O';
  move ( KO, OK );
  writeln ( OK );
end.
