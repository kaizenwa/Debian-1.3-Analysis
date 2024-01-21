// Special g++ Options:

class A4 {
public:
  operator char * () {
    return "";
  }
  operator unsigned long long int () {
    return true;
  }
} a4;

main() {
  if (!a4) return 1;		// ERROR - ambiguous bool conversion
}
