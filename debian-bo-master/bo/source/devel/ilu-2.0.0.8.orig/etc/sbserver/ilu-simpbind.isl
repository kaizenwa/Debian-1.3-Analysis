(* Simple binding service interface *)

INTERFACE ilu-simpbind;

TYPE Server = OBJECT TYPEID "ilu:ilu-simple-binding-version-1"
  DOCUMENTATION "Simple binding name service"
  METHODS
    (* add exceptions *)
    Publish (sbh : StringBindingHandle) : CookieType
	RAISES BadSBH, AlreadyPublished, MallocFailure END
	"returns a cookie if successful or NIL if failed",
    Withdraw (sbh : StringBindingHandle, cookie : CookieType) : BOOLEAN
	RAISES NoTable, NotPublished, BadProof, BadSBH END
	"returns true if successfully withdrawn",
    Lookup (sid : ilu.CString, ih : ilu.CString) : StringBindingHandle
	RAISES NoTable, NotPublished, MallocFailure END
	"returns object's sbh and mst if successful or NIL if failed",
    (* for debugging only *)
    Enumerate (pattern : ilu.CString) : StringBindingHandleList
	"returns list of objects in table, filtered according to pattern"
  END;

TYPE StringBindingHandle = ilu.CString;
TYPE StringBindingHandleList = SEQUENCE OF StringBindingHandle;
TYPE CookieType = ilu.CString;

EXCEPTION BadSBH "Couldn't parse the sbh" ;
EXCEPTION AlreadyPublished "Object's oid is already a key" ;
EXCEPTION NoTable "The SimpleBinding object's internal htable is NIL" ;
EXCEPTION NotPublished "Object's oid is not in htable" ;
EXCEPTION BadProof "Proof does not match htable proof" ;
EXCEPTION MallocFailure "Could not allocate space for published data" ;


