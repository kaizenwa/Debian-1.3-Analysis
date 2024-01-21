struct A {
  A (int);
};

struct B {
  B (int);
};

void myfunc (const A& t0);
void myfunc (const B& t0);

int main ()
{
   myfunc(1);   
}
