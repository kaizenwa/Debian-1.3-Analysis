(* Fibber.isl *)
(* $Id: Fibber.isl,v 1.1 1995/11/22 15:36:51 spreitze Exp $ *)
(* Last edited by Mike Spreitzer November 14, 1995 3:14 pm PST *)

INTERFACE Fibber;

TYPE String = SEQUENCE OF SHORT CHARACTER;
TYPE StringSeq = SEQUENCE OF String;

EXCEPTION Failed: StringSeq;

TYPE T = OBJECT
	METHODS
	Calc(d: CARDINAL, n: CARDINAL, ask: T): CARDINAL
		RAISES Failed END
	(* Return fib(n), using /ask/ for recursive calls. *)
	END;

