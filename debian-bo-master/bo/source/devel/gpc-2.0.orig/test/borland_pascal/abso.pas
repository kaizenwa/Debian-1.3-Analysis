Program Abso;

Var
  x: Integer;

  OK: record
    O, K: Char;
  end (* OK *) absolute x;

  KO: array [ 1..7 ] of Integer absolute x;

begin
  Var KO2: Integer absolute x;
  (* BUG: absolute does not work with structured types here. *)
  (* (enable_keyword ("Absolute") is called too late.)       *)
  x:= $4F4B4B4F;
  with OK do
    writeln ( O, K );
end.
