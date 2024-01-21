Program CstParam;


Const
  y: Char = 'Y';


(*$M *** *)  (* Messages to stderr during compilation (GNU extension). *)
(*$M Expect the warnings: "assignment of read-only location" and *)
(*$M "typed constants misused as initial variables" (plus a suggestion). *)
(*$M *** *)


(* "protected" can be redefined -- even in this context *)

Procedure Ignore ( Const protected: Char );

begin (* Ignore *)
  protected:= 'O';
end (* Ignore *);


begin
  Ignore ( y );
  write ( y );
  y:= 'K';
  writeln ( y );
end.
