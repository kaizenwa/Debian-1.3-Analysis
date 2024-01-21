// visibility file
// From: jamshid@ses.com (Jamshid Afshar)
// Date:     Sun, 12 Dec 93 03:09:15 CST
// Subject:  Missed access declaration error
// Message-ID: <9312120909.AA22135@ses.com>

class X {
  public:
    void f();
};

class Y : private X {
  public:
    void f(int);
    X::f;  // g++ 2.5.5 doesn't flag this misuse
};
