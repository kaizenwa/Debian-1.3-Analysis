// Client2.java

public class Client2 {

  public static void main(String args[]) {
    try {
      // Initialize the ORB.
      CORBA.ORB orb = CORBA.ORB.init();
      // Locate an account manager.
      CORBA.Object obj = orb.string_to_object(args[0]);
      Bank.AccountManager manager =
	Bank.AccountManager_var.narrow(obj);
      // use args[0] as the account name, or a default.
      String name = args.length > 1 ? args[1] : "Jack B. Quick";
      // Request the account manager to open a named account.
      Bank.Account account = manager.open(name);
      // Get the balance of the account.
      float balance = account.balance();
      // Print out the balance.
      System.out.println
	("The balance in " + name + "'s account is $" + balance);
    }
    catch(CORBA.SystemException e) {
      System.err.println(e);
    }
  }
}
