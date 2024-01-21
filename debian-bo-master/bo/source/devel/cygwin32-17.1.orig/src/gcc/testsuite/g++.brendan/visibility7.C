class X
{
public:
  void fn ();
};
class Y : private X
{};

class Unrelated
{
public:
  void foo () { Y y; y.fn (); }
};
