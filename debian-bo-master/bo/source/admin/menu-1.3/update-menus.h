//   -*- mode: c++; -*-
#include <map.h>
#include <multimap.h>
#include <vector.h>
#include "adstring.h"
#include "common.h"

//Exception classes:

class unknown_cond_package{};
class cond_inst_false{};//conditional install returns false

class ferror_read{
public:
  char name[MAX_LINE];
  ferror_read(String s){
    strcpy(name,&(s[0]));
  };
};  //file open for reading failed

class dir_error_read{
public:
  char name[MAX_LINE];
  dir_error_read(String s){
    strcpy(name,&(s[0]));
  };
};  //dir open for reading failed

class informed_fatal{}; //something BAD happened, we told user, now die.


#define DPKG_STATUSFILE "/var/lib/dpkg/status"
#define MENUMETHODS     "/etc/menu-methods/"
#define CONFIGMENUS     "/etc/menu/"
#define PACKAGEMENUS    "/usr/lib/menu/"
#define MENUMENUS       "/usr/lib/menu/default/"
#define USERMENUS       ".menu/"
#define USERMETHODS     ".menu-methods/"

#define MENU1_INSTALLER "#!/usr/sbin/install-menu"

#define UPMEN_LOCKFILE  "/var/run/update-menus.pid"
#define DPKG_LOCKFILE   "/var/lib/dpkg/lock"
#define TRANSLATE_FILE  "/etc/menu-methods/translate_menus"
#define USERTRANSLATE   ".menu-methods/translate_menus"

#define TRANSLATE_TRANS    "translate"
#define SUBSTITUTE_TRANS   "substitute"
#define SUBTRANSLATE_TRANS "subtranslate"
#define ENDTRANSLATE_TRANS "endtranslate"
class menuentry {
private:
  bool test_installed(String filename);
public:
  map <String, String, less<String> > data;
  menuentry(parsestream &i, const String &);
  void output(vector<String> &s);
  void output_compat(vector<String> &s);
  ostream &debugoutput(ostream &o);
};



class trans_class{
protected:
  String match, replace, replace_var;
public:
  bool check(String &s);
  trans_class(const String &m,
	      const String &r,
	      const String &rv);
  
  virtual void process(menuentry &m,
		       const String &v)=NULL;
  ostream &debuginfo(ostream &);
};
class translate: public trans_class{
public:
  translate(const String &m,
	    const String &r,
	    const String &rv):trans_class(m,r,rv){};
  
  void process(menuentry &m,
	       const String &v);
};
class substitute: public trans_class{
public:
  substitute(const String &m,
	    const String &r,
	    const String &rv):trans_class(m,r,rv){};

  void process(menuentry &m,
	       const String &v);
};
class subtranslate: public trans_class{
public:
  subtranslate( String &m,
	    const String &r,
	    const String &rv):trans_class(m,r,rv){};

  void process(menuentry &m,
	       const String &v);
};

class translateinfo{
  typedef multimap<String, trans_class*, less<String> > trans_map;
  map<String, trans_map, less<String> > trans;
  //  map<String, multimap<String, trans_class*, less<String> >, less<String> > trans;
  void init(parsestream &i);
public:
  translateinfo(parsestream &i);
  translateinfo(const String &filename);
  void process(menuentry &m);
  void debuginfo();
};

