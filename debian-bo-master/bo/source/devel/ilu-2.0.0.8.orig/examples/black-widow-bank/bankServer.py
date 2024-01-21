import Bank, ilu, Bank__skel, rand

mainloopvar = ilu.CreateLoopHandle()

class realAccount (Bank__skel.Account):

	def __init__(self, manager, name, balance):
		self.manager = manager
		self.IluServer = manager.IluServer
		self.IluInstHandle = manager.IluInstHandle + ' - ' + name
		self.name = name
		self.bank_balance = balance

	def balance (self):
		return self.bank_balance

class realAccountManager (Bank__skel.AccountManager):

	def __init__(self, name, srvr=None):
		self.IluInstHandle = name
		self.IluServer = srvr
		self.accounts = {}

	def open (self, name):
		if not self.accounts.has_key(name):
			initial_balance = rand.rand() / 32.768
			account = realAccount (self, name, initial_balance)
			self.accounts[name] = account
		else:
			account = self.accounts[name]
		return account

def main():

  instHandle = "Post-Modern Bank"
  serverID = "bankserver.somedept.somecompany.com"

  s = ilu.CreateServer(serverID, ("tcp_0_0",), "iiop_1_0_1")
  uc = realAccountManager(instHandle, s)

  uc.IluPublish()

  print ilu.IOROfObject(uc)

  ilu.RunMainLoop(mainloopvar)

if __name__ == '__main__':
	main()
