(* $Id: Test2.isl,v 1.3 1994/11/11 23:43:50 spreitze Exp $ *)
(* Last edited by Mike Spreitzer November 11, 1994 3:43 pm PST *)

INTERFACE Test2 IMPORTS Test1 END;

TYPE F = Test1.E;

TYPE U2 = F UNION
	ilu.CString = ev1 END,
	Test1.O1 = ev3 END,
	P = ev7 END
	END;

TYPE T1U = Test1.U;
TYPE T1U2 = Test1.U2;
TYPE T1U3 = Test1.U3;

TYPE T1O3 = Test1.O3;

TYPE P = OBJECT METHODS 
	SR-to-I (i: SHORT REAL): INTEGER
	END;

EXCEPTION E1: T1U;
