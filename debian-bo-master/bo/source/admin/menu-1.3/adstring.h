//   -*- mode: c++; -*-
#include <String.h>
#include <Regex.h>
#include <iostream.h>
#include <fstream.h>

#define MAX_LINE 1024  //this isn't actually used by adstring 

class endoffile{};
class ident_expected{};
class char_expected{
public:
  char c;
  char_expected(char ch):c(ch) {};
};

class parsestream{
  int lineno;
  istream *i;
  String buffer;
  String fname;

  char get_next();
public:
  init(istream &in){
    i=&in; 
    lineno=1;
  }
  parsestream(istream &in){
    fname="(probably) stdin";
    init(in);
  };
  parsestream(const String &s){
    ifstream *f=new ifstream(s);
    fname=s;
    init(*f);
  };
  ~parsestream(){
    //delete i;
  }
  bool good(){return *i;};
  //  void menuentry_error(const String s);
  char get_char();
  char put_back(char);
  String get_name();
  String get_name(const Regex &);
  String get_stringconst();
  String get_eq_name();
  String get_eq_stringconst();
  String get_line();
  void skip_line();
  void skip_space();
  void skip_char(char expect);
  int linenumber(){return lineno;};
  String filename(){return fname;};
};



extern String escape_doublequotes(const String &s);
extern String escape_string(const String &s, const String &esc);
extern String escapewith_string(const String &s, 
				const String &esc, 
				const String &with);
extern String replacewith_string(const String &s, 
				 const String &replace, 
				 const String &with);
extern String cppesc_string(const String &s);
extern String replace(String s,char match, char replace);
extern int Stringtoi(const String &s);
extern String itoString(int i);
extern String itohexString(int i);
extern String sort_hotkey(String s);
extern String String_parent(String s);
