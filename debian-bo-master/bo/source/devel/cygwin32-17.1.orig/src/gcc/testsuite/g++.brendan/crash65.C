class X {
public:  
  virtual const char* 	XY(const void* val) const = 0;
};


class Y : public X {
public:
  using X::xy;

  using X::z;
};
