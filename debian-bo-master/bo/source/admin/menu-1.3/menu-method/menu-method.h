//   -*- mode: c++; -*-

/*
  
Grammar of "string"'s:
  cat    = value | (value cat)
  value  = const | func | var
  const  = "\"" .* "\""
  var    = "$"name
  func   = name "(" args ")"
  args   = cat | (args "," cat)
  name   = [a-z,A-Z,_][a-z,A-Z,_,0-9]*

Structure of container classes
  str         general container
    const     contains constant string
    cat       vector of str's
    var       contains a named variable (like section, title, needs, ...).
    func      a function (print, ifelse, iffile, ..). 
      func1   with 1 argument
      func2   with 2 arguments
      func3   with 3 arguments

Example "str":
  "title=" print($title) ", mess:" \
   ifelse($longtitle, print(title), $longtitle)
*/

#include <iostream.h>
#include <vector.h>
#include <map.h>
#include <String.h>
#include "../common.h"
#include "../adstring.h"

#define string String

//Exception classes


class narg_mismatch{ //number of args mismatch to func.
public:
  char name[MAX_LINE];
  narg_mismatch(String s){
    strcpy(name,&(s[0]));
  };
};  
class dir_createerror{ //number of args mismatch to func.
public:
  char name[MAX_LINE];
  dir_createerror(String s){
    strcpy(name,&(s[0]));
  };
};  

class char_unexpected{ //an unexpected char occured
public:
  char c;
  char_unexpected(char ch){
    c=ch;
  };
};  

class informed_fatal{};

//container classes:

class func;

class str{
public:
  virtual ostream &output(ostream &o, 
			  map<string, string, less<string> > &menuentry)=NULL;
  virtual ostream &debug(ostream &)=NULL;
};

class const_str:public str{
private:
  string data;
public:
  const_str(parsestream &);
  virtual ostream &output(ostream &o, 
				     map<string, string, less<string> > &menuentry);
  ostream &debug(ostream &o);
};

class cat_str:public str{
private:
  vector<str *> v;
public:
  cat_str(parsestream &);
  ostream &output(ostream &o, 
		  map<string, string, less<string> > &menuentry);
  void output(map<string, string, less<string> > &menuentry);
  String  soutput(map<string, string, less<string> > &menuentry);
  ostream &debug(ostream &o);
};

class var_str:public str{
private:
  string var_name;
public:
  ostream &output(ostream &o, 
		  map<string, string, less<string> > &menuentry);
  var_str(parsestream &i);
  ostream &debug(ostream &o);
};

class func_str:public str{
private:
  vector<cat_str *> args;
  func *f;
public:
  func_str(parsestream &);
  ostream &output(ostream &o, 
		  map<string, string, less<string> > &menuentry);
  ostream &debug(ostream &o);
};


/////////////////////////////////////////////////////
//  prototypes of install-menu functions:
//

class func{
public: 
  virtual int nargs()=NULL;
  virtual ostream &output(ostream &, vector<cat_str *> &,
			  map<string, string, less<string> > &)=NULL;
  virtual string name()=NULL;
};
class func0:public func{
public:
  int nargs(){return 0;};
};
class func1:public func{
public:
  int nargs(){return 1;};
};
class func2:public func{
public:
  int nargs(){return 2;};
};
class func3:public func{
public:
  int nargs(){return 3;};
};
class func4:public func{
public:
  int nargs(){return 4;};
};


/////////////////////////////////////////////////////
//  Function that can be used in install-menu (declarations)
//

class prefix_func:public func0{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "prefix";}
};
class ifroot_func: public func2{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "ifroot";}
};

class print_func:public func1{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "print";}
};
class ifempty_func:public func2{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "ifempty";}
};
class ifnempty_func:public func2{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "ifnempty";}
};
class iffile_func:public func2{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "iffile";}
};

class esc_func:public func2{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "esc";}
};
class ifelse_func: public func3{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "ifelse";}
};

class ifeq_func: public func3{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "ifeq";}
};
class ifneq_func: public func3{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "ifneq";}
};
class ifeqelse_func: public func4{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "ifeqelse";}
};

class cond_surr_func: public func3{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "cond_surr";}
};
class escwith_func:public func3{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "escwith";}
};
class escfirst_func:public func3{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "escfirst";}
};
class replacewith_func:public func3{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "replacewith";}
};
class cppesc_func:public func1{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "cppesc";}
};
class parent_func:public func1{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "parent";}
};
class basename_func:public func1{
public: 
  ostream &output(ostream &o, vector<cat_str *> &,
		  map<string, string, less<string> > &);
  string name(){return "basename";}
};

/////////////////////////////////////////////////////
//  install-menu Function declarations
//


class supportedinfo {
private:
  class supinf{
  public:
    cat_str *c;
    int prec;
  };
  map<string, supinf, less<string> > sup;
public:
  supportedinfo(parsestream &);
  int prec(string &);
  void subst(map<string, string, less<string> > );
  ostream &debuginfo(ostream &);
};

class configinfo{
private:
  string compt,rcf, exrcf, mainmt, roots,  rootpref, userpref, treew;
  string preout, postout;
public:
  configinfo(parsestream &);
  cat_str *startmenu, *endmenu, *submenutitle, *hkexclude,
          *genmenu, *postrun, *prerun;
  int hotkeycase; //0=insensitive, 1=sensitive
  ostream &debug(ostream &);
  
  string &compat(){return compt;};
  string &rcfile(){return rcf;};
  string &examplercfile(){return exrcf;};
  string &mainmenutitle(){return mainmt;};
  string &rootsection(){return roots;};
  string &rootprefix(){return rootpref;};
  string &userprefix(){return userpref;};
  string &treewalk(){return treew;};
  string &preoutput(){return preout;};
  string &postoutput(){return postout;};
  string prefix();
};

class menuentry {
private:
  char hotkeyconv(char);
  void generate_hotkeys();
public:
  map <string, menuentry *, less<string> > submenus;
  map <string, string, less<string> > vars;
  void add_entry(string, string, map <string, string, less<string> > &);
  void output();
  void postprocess();
};

