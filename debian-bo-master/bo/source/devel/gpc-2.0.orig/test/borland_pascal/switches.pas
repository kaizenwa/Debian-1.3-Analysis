(*$M *** *)
(*$M Expect some warnings: --pedantic is gonna switched on. *)
(*$M *** *)

(*$P+*)

Program Switches;

(*$N-*)

(* Nested comments are switched off. }

(*$N+*)

(* Nested {comments} are switched {on}.*)


(*$ifdef BUG *)

  GPC does not see this text unless you #define BUG.  But why should you?

(*$endif *)


(*$define CMULB chr *)


begin
  (*$E+,C+*)  (* char-escapes and c-numbers on *)
  write ( '\117' );
  (*$E-*)  (* char-escapes off *)
  write ( '\n' + chr ( 010 ) + CMULB ( 0x08 ) + 'KX' );
  (*$E+*)
  writeln ( '\b \b' );
end.
