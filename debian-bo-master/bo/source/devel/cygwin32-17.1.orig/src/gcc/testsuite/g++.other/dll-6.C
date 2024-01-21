// dll.h
class aClass 
    { 
public: 
    __declspec(dllimport) aClass(); 
    }; 

// dll.cpp

__declspec(dllexport) aClass::aClass() 
    { 
    } 
