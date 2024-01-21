Program OpenArr;


Var
  OK: array [ 1..2 ] of Char value ( 'O', 'K' );


Procedure WriteArray ( Var A: array of Char; Count: Integer );

(* BUG: should allow open arrays only as Var parameters. *)

Var
  i: Integer;

begin (* WriteArray *)
  i:= 0;
  while i < Count do
    begin
      write ( A [ i ] );
      inc ( i );
    end (* while *);
  writeln;
end (* WriteArray *);


begin
  WriteArray ( OK, 2 );
end.
