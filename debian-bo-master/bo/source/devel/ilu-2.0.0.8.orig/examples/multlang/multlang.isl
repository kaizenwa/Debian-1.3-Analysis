INTERFACE multlang BRAND "multlang.example.parc.xerox.com";

EXCEPTION TooBig;

TYPE Squarer = OBJECT
  METHODS
    ObtainSquare (val : CARDINAL) : CARDINAL RAISES TooBig END
  END;

TYPE Multiplier = OBJECT
  METHODS
    Multiply (val1 : CARDINAL, val2 : CARDINAL) : CARDINAL RAISES TooBig END
  END;
