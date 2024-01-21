class CLogger
{
public:
        void operator() (int,const char *) {};
        void operator() (int,const char *, ...) {};
} Log;

class CGLogger : public CLogger
{
} GLog;

main()
{
        Log(1,"Test");
        Log(1,"Test %d",3);
        GLog(1,"Test");
        GLog(1,"Test %d",3);
}
