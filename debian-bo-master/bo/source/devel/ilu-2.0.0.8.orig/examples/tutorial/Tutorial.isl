INTERFACE Tutorial;

EXCEPTION DivideByZero
  "this error is signalled if the client of the Calculator calls
the Divide method with a value of 0";

TYPE Calculator = OBJECT COLLECTIBLE
  DOCUMENTATION "4-function calculator"
  METHODS
    SetValue (v : REAL) "Set the value of the calculator to `v'",
    GetValue () : REAL  "Return the value of the calculator",
    Add (v : REAL)      "Adds `v' to the calculator's value",
    Subtract (v : REAL) "Subtracts `v' from the calculator's value",
    Multiply (v : REAL) "Multiplies the calculator's value by `v'",
    Divide (v : REAL) RAISES DivideByZero END
      "Divides the calculator's value by `v'"
  END;

TYPE Factory = OBJECT
  METHODS
    CreateCalculator () : Calculator
  END;
