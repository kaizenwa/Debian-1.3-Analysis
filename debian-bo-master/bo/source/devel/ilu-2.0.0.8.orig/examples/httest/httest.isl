(* $Id: httest.isl,v 1.1 1996/06/04 19:51:31 larner Exp $ *)

(* 
   Sample interface that imports http.isl and adds a method to 
   the resource object as defined in http.isl

	-- Dan Larner, 
*)

INTERFACE httest IMPORTS http END;

EXCEPTION FLIPEXCEP: INTEGER;

TYPE DerivedResource = OBJECT
  DOCUMENTATION "Derived object of Resource to test http subtypes"
  SUPERTYPES http.Resource END
  METHODS
	flipcase (strtoflipcase : ilu.CString ) : ilu.CString RAISES FLIPEXCEP END
  END;
