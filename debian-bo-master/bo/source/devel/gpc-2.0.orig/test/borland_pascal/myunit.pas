Unit MyUnit;

Interface

Const
  OKlength = 2;

Type
  OKarray = array [ 1..OKlength ] of Char;

Var
  OKvar: OKarray;

Procedure OK;

Implementation

(*$ifdef BUG *)

Const
  readln = 7;  (* This should be a private declaration of the Unit *)
               (* but actually is public!                          *)

(*$endif *)

Procedure OK;

Var
  i: Integer;

begin (* OK *)
  for i:= 1 to OKlength do
    write ( OKvar [ i ] );
  writeln;
end (* OK *);

end.
