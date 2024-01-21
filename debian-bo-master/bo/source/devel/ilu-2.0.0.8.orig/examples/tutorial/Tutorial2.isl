INTERFACE Tutorial2 IMPORTS Tutorial END;

TYPE OpType = ENUMERATION
    SetValue, Add, Subtract, Multiply, Divide END;

TYPE Operation = RECORD
    op : OpType,
    value : REAL,
    accumulator : REAL
  END;

TYPE RegisterTape = SEQUENCE OF Operation;

TYPE TapeCalculator = OBJECT COLLECTIBLE
  SUPERTYPES Tutorial.Calculator END
  DOCUMENTATION "4 function calculator with register tape"
  METHODS
    GetTape () : RegisterTape
  END;

TYPE Factory = OBJECT SUPERTYPES Tutorial.Factory END
  METHODS
    CreateTapeCalculator () : TapeCalculator
  END;
