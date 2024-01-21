class A {
public:
        void aMethod(void) {};
};

class AA : A { };

class B {
public:
        void thisMethod() {
                AA ana;
                ana.aMethod();
        }
};
