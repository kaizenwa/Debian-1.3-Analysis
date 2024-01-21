class foo{
public:
  static void  bar( int i ){ value = i; }
  static int  value;
};

const int  foo::value = 0; // should be an error.

int main(){
  foo::bar( 1 );
  return 0;
}

