(* Dialog.isl *)
(* $Id: Dialog.isl,v 1.2 1995/07/28 23:53:24 spreitze Exp $ *)
(* Last edited by Mike Spreitzer July 28, 1995 3:56 pm PDT *)

INTERFACE Dialog;

TYPE String = SEQUENCE OF SHORT CHARACTER;

TYPE T = OBJECT METHODS
	M(arg: String): String
	END;
