extern "C" void printf (char *, ...);

class A {
	int	i;
	int	j;
    public:
	int	h;
	A() { i=10; j=20; }
	virtual void f1() { printf("i=%d j=%d\n",i,j); }
	friend virtual void f2() { printf("i=%d j=%d\n",i,j); }
};

class B : public A {
    public:
	virtual void f1() { printf("i=%d j=%d\n",i,j); }
	friend virtual void f2() { printf("i=%d j=%d\n",i,j); }
};

main() {
	A * a = new A;
}
