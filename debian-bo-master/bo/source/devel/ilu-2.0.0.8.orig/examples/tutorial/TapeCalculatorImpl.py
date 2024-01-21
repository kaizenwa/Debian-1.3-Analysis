import Tutorial, Tutorial2, Tutorial2__skel

class TapeCalculator (Tutorial2__skel.TapeCalculator):

	def __init__ (self):
		self.value = 0.0
		self.tape = []

	def SetValue (self, v):
		global value
		self.value = v
		self.tape.append({'op' : Tutorial2.OpType.SetValue, 'value' : v, 'accumulator' : self.value})

	def GetValue (self):
		return self.value

	def Add (self, v):
		self.value = self.value + v
		self.tape.append({'op' : Tutorial2.OpType.Add, 'value' : v, 'accumulator' : self.value})


	def Subtract (self, v):
		self.value = self.value - v
		self.tape.append({'op' : Tutorial2.OpType.Subtract, 'value' : v, 'accumulator' : self.value})


	def Multiply (self, v):
		self.value = self.value * v
		self.tape.append({'op' : Tutorial2.OpType.Multiply, 'value' : v, 'accumulator' : self.value})


	def Divide (self, v):
		try:
			self.value = self.value / v
		except ZeroDivisionError:
			raise Tutorial.DivideByZero
		self.tape.append({'op' : Tutorial2.OpType.Divide, 'value' : v, 'accumulator' : self.value})

	def GetTape (self):
		return (self.tape)
