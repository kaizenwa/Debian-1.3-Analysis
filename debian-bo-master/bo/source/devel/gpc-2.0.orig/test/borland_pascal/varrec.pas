Program VarRec;

Var
  OK: record
    case 1..2 of
      1: ( x: Integer );
      2: ( O, K: Char );
  end (* OK *);

begin
  OK.x:= $4F4B4B4F;
  writeln ( OK.O, OK.K );
end.
