// ctors file
// Subject: bug in handling static const object of the enclosing class
// Date: Tue, 1 Sep 92 10:38:44 EDT

class X
{
  private:
    int x;
  public:
    static const X x0;
    X( int );
};

class Y
{
  private:
    X xx;
  public:
    Y();
}
X::X( int xi )
{
    x = xi;
}

const X X::x0( 0 );

Y::Y()
{
    xx = X::x0;
}
