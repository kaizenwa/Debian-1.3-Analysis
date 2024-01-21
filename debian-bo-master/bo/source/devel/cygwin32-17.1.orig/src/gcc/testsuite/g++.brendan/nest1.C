int x;
class enclose {
public:
  int x;

  class inner {
  public:
    void f (int i) {
      x = i;
    }
  };
};

