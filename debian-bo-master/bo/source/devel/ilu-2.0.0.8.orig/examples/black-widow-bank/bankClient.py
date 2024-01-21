import Bank, ilu, sys

def do_test (manager_sbh, account_name):
	manager = ilu.ObjectOfSBH(Bank.AccountManager, manager_sbh)
	account = manager.open (account_name)
	balance = account.balance()
	print "The balance for %s is $%.2f.\n" % (account, balance)

def main (argv):
	if not (len(argv) == 3):
		print 'Usage:  %s ACCOUNT-MANAGER-SBH ACCOUNT-NAME'
		sys.exit(1)
	do_test(argv[1], argv[2])

if __name__ == '__main__':
	main(sys.argv)
		
