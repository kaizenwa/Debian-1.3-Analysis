class aClass 
{ 
public: 
  __declspec(dllimport) int f1(); 
  __declspec(dllexport) int f2(); 
}; 
 
__declspec(dllexport) int aClass::f2() 
{ 
  return f1(); 
} 
