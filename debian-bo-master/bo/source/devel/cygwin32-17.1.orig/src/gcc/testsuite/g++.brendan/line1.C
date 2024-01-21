typedef struct s S;
struct S { int member:1; };  // the lineno for this should be 2, not 0
