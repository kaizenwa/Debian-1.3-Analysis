class A {
    public:
      A() {
      }
};

static class A *A_new_() {
    return new class A;
}

