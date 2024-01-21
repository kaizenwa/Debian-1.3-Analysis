INTERFACE Bank;

TYPE Account = OBJECT TYPEID "IDL:Bank/Account:1.0"
  METHODS
    balance () : SHORT REAL
  END;

TYPE AccountManager = OBJECT TYPEID "IDL:Bank/AccountManager:1.0"
  METHODS
    open (name : ilu.CString) : Account
  END;
