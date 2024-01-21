char *version_string = "2.7.2.1";
char *gpc_version_string = "2.0(2.7.2.1)";

#ifdef ALPHA_BUG
/* I do not know why the linker starts to output the following,
   but if it does, compiling this file and the RTS library fixes it...

Warning: Linking some objects which contain exception information sections
        and some which do not. This may cause fatal runtime exception handling
        problems (last obj encountered without exceptions was <OBJ/LIB>)

*/

extern int _fpdata_size;
null_routine()
{
  int a = _fpdata_size;
}

#endif /* ALPHA_BUG */
