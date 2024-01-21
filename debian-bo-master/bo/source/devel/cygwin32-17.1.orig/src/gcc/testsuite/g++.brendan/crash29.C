
union Value
{
	Value(){}
};

struct GlobalAddress
{
	GlobalAddress(Value *nvar){}
};

main()
{
	new GlobalAddress(Value());		// internal error occured here
	//new GlobalAddress(new Value());	// This line is correct code
}
