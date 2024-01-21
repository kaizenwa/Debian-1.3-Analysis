// visibility file
// From: Sandeep Shroff <ss@caere.com>
// Date:     Thu, 05 Aug 1993 17:23:20 -0700
// Subject:  Access to private constructor.
// Message-ID: <9308060023.AA10283@neptune.caere.com>
#include <iostream.h>

class Base
{
public:
  char* getName() {return name_;}

private:
  Base();
  Base(char* str);

  char* name_;
};

class Derived : public Base
{
public:
  Derived(int n, char* str);
  Derived(int n);

  getNum() {return num_;}
private:
  int num_;
};

Base::Base()
{
  name_ = strcpy(new char[strlen(" ") + 1], " ");
}

Base::Base(char* str)
{
  if(str != NULL)
    name_ = strcpy(new char[strlen(str) + 1], str);
}

Derived::Derived(int n, char* str) : Base(str)
{
  num_ = n;
}

Derived::Derived(int n) : Base()
{
  num_ = n;
}



int main()
{
  // Derived* d = new Derived(10, "test");
  Derived* d = new Derived(10);

  cerr << d->getNum() << "\t" << d->getName() << endl;
}

