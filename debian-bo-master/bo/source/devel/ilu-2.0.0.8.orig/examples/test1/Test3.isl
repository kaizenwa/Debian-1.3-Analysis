(* $Id: Test3.isl,v 1.3 1994/11/11 23:43:50 spreitze Exp $ *)
(* Last edited by Mike Spreitzer November 11, 1994 3:43 pm PST *)

INTERFACE Test3 IMPORTS Test1, Test2 END;

EXCEPTION E1: ilu.CString;

TYPE O = OBJECT SUPERTYPES Test2.T1O3, Test2.P END
  METHODS
    I-to-Test1U (i: INTEGER): Test2.T1U RAISES E1, Test1.E1 END
  END;

TYPE T2T1U3 = Test2.T1U3;
TYPE T2T1O3 = Test2.T1O3;

TYPE FU = Test2.F UNION O = ev1 END, INTEGER = ev3 END END;
