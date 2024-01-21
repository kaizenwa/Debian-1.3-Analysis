// ctor file
// Message-Id: <9302052351.AA10789@harvey>
// From: greg@qualcomm.com (Greg Noel)
// Subject: bug019.cc
// Date: Fri, 5 Feb 93 15:51:42 -0800

#include <iostream.h>

class Class
{
        class Err : public ostream
        {
        public:
                Err(void) : ostream(cout) { }
                ~Err(void) { }
        };
public:
        //template<class T> Err& operator << (const T x) { return Err() << x; }
        Err& operator << (const char *x) { return Err() << x; }
private:
        char x;
};
