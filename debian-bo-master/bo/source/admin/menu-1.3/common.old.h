//   -*- mode: c++; -*-
#ifndef COMMON_H
#define COMMON_H

#include <string>
#include <fstream.h>

#include <string.h>

#define STATUSFILE   "/var/lib/dpkg/status"
#define PACKAGE_LINE "Package: "
#define STATUS_OK    "Status: install ok installed"
#define STATUSLINE   "Status:"
#define INSTALLEDSTR "installed"
#define REMVEDSTR    "removed"
#define INSTALLOPTION "--install-files"
#define STDINOPTION   "--stdin"
#define REMOVEOPTION  "--remove"

#define MENUMETHODS  "/etc/menu-methods"
#define CONFIGMENUS  "/etc/menu"      //the config menufiles by the sysadmin
#define PACKAGEMENUS "/usr/lib/menu"  //menu files supplied individual packages
#define MENUMENUS    "/usr/lib/menu/default"//menu files supplied with this package
#define USERMENUS    ".menu"   //read when user execs prog; add $home in front.

#define UPDATEDMENUS ".updated-menus"
#define MANAGERSTR   "menu managers:"

const int maxline=2000;
class strsort{
public:
  bool operator()(const char *i, const char *j) const{
    return strcmp(i,j)<0;
  }
};


extern char *getline_nonempty(istream &i, char *s, int l);
extern string &getline_nonempty(istream &i, string &s);
extern char *getword(istream &i, char *s, int l, char *sep=" \t\v\f\n\r");
extern bool  getword(istream &i, string &s, char *sep=" \t\v\f\n\r");
extern string &space_strip(string &s);
extern char *space_strip(char *s);
extern string getlastword(string &s, char separate);
extern char *string2char(char *, string &);
extern void skip_spaces(istream &i);
extern char *skip_spaces(char *);
extern string escape_quotes(string &s);
extern string escape_singlequotes(string &s);
extern string escape_bothquotes(string &s);

extern void show_passedtime(ostream &o, char *);
extern bool show_time;
#endif

