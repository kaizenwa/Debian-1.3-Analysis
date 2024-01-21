(* $Id: ChgUp.isl,v 1.1 1995/02/17 23:54:06 spreitze Exp $ *)
(* Last edited by Mike Spreitzer February 17, 1995 11:10 am PST *)

INTERFACE ChgUp;

TYPE T = OBJECT
  METHODS
    GetBrother(): T,
    GetGeneration(): CARDINAL
  END;

