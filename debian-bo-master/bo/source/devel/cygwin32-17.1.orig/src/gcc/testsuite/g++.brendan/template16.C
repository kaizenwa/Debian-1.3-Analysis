template<class T> class TList {
   typedef void (T::*TVOIDFUNT)();
   typedef T*   (T::*TTPFUNT)(T*);
};

class A;
class B : TList<A> { };

