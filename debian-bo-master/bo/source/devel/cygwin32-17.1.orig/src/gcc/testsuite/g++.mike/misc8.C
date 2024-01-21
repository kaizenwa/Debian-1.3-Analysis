class T { public: virtual ~T() {} };
template<class P> class X : public virtual T {};
main() { X<int> x; }  // ERROR - Unknown opcode!
