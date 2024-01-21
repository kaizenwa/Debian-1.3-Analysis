class X {
public:
    X();
};
void foo ()
{
X:  ::abort();
    goto X;
}
