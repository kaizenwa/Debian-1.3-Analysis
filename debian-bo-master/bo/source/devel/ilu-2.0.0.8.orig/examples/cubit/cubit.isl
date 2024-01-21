INTERFACE cubit;

TYPE Cubit-Many = RECORD
	o : BYTE,
	l : INTEGER,
	s : SHORT INTEGER
	END;
TYPE Cubit = OBJECT OPTIONAL TYPEID "IDL:Eng.SUN.COM/Cubit:1.0"
	METHODS
		cube-octet (o : BYTE) : BYTE,
		cube-short (s : SHORT INTEGER) : SHORT INTEGER,
		cube-long (l : INTEGER) : INTEGER,
		cube-struct (values : Cubit-Many) : Cubit-Many,
		ASYNCHRONOUS please-exit ()
	END;
