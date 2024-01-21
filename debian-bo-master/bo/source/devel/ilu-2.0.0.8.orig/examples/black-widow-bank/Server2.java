// Server.java

import java.util.*;

class Account extends Bank._sk_Account {
  Account(float balance) {
    _balance = balance;
  }
  public float balance() throws CORBA.SystemException {
    return _balance;
  }
  private float _balance;
}

class AccountManager extends Bank._sk_AccountManager {
  AccountManager(String name) {
    super(name);
  }
  public Bank.Account open(String name) throws CORBA.SystemException {
    // Lookup the account in the account dictionary.
    Bank.Account account = (Bank.Account) _accounts.get(name);
    if(account == null) {
      // Create a new account with between 0 and 1000 dollars.
      float balance = Math.abs(_random.nextInt()) % 100000 / 100f;
      account = new Account(balance);
      System.out.println("Created " + name + "'s account: " + account);
      // Export the new object reference.
      CORBA.ORB.init().BOA_init().obj_is_ready(account);
      // Save the account in the account dictionary.
      _accounts.put(name, account);
    }
    // Return the account.
    return account;
  }
  private Dictionary _accounts = new Hashtable();
  private Random _random = new Random();
}

public class Server2 {
  public static void main(String[] args) {
    try {
      // Initialize the ORB.
      CORBA.ORB orb = CORBA.ORB.init();
      // Initialize the BOA.
      CORBA.BOA boa = orb.BOA_init();
      // Create the account manager object.
      AccountManager manager = 
	new AccountManager("Post-Modern Bank");
      // Export the newly create object.
      boa.obj_is_ready(manager);
      System.out.println(orb.object_to_string(manager));
      // Wait for incoming requests
      boa.impl_is_ready();
    }
    catch(CORBA.SystemException e) {
      System.err.println(e);
    }
  }
}

