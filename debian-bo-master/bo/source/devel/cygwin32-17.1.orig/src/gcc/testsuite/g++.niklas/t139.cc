struct stat {};
stat gstat;
int stat (struct stat*);
void f () { struct stat* ps; stat (ps); }
