class A {
  public:
    const int &operator[]( int i );
  private:
    int k;
};

const int& A::operator[]( int i )
{
    return k;
}


void ff( A &anA )
{
    int &ani = anA[0];

    ani = 7;
}
