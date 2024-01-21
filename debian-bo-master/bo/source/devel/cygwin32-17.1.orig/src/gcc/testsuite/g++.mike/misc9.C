class bee {
 public:
  int bee::bar;		// WARNING - there is an extra bee:: here
};

class foo {
 public:
  int bee::bar;		// ERROR - you cannot do this
    int me();
};
