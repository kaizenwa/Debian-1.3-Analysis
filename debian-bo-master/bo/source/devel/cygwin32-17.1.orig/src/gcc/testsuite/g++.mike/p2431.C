class A
{
	public:
	A(A &);
};

class B
{
	public:
	operator A ();
};

class C
{
	public :
	C()
	{
		B	b;
		A a = b;
	}
};
