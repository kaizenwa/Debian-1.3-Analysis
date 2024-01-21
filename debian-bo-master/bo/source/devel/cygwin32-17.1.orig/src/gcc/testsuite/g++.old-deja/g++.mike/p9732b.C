int count;

struct base {
  base () { ++count; }
  ~base () { --count; }
  base(const base&o) { ++count; }
};

class D {
public:
  ~D() {
    if (count != 0)
      exit (1);
    exit (0);
  }
} d;

base base_object;

base base_returning_function ();

const base& base_ref = base_returning_function ();

int main () {
}

base base_returning_function () {
  base local_base_object;
  return local_base_object;
}
