// test that use of `inline' is forbidden when it should be
inline int i;
struct c { inline int i; };
int foo (inline int i);
inline class c;
inline typedef int t;
class d { inline friend class c; };
