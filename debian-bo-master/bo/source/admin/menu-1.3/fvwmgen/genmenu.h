//   -*- mode: c++; -*-
#ifndef FVWM2H
#define FVWM2H
#include <string>
#include <iostream.h> 
#include <fstream.h>
#include <map.h>
#include <vector.h>

class fvwm2menu{  //I Dont really use this any more, should remove it.
public:
  string need;
  string section,
    id,
    text,
    icon,
    command;
};


class menuentry{
public:
  map <string, menuentry *, less<string> > submenus;
  menuentry *parent;
  menuentry(menuentry *p){parent=p;};
  ~menuentry();
  void     remove_id(string &s);
  void     remove_id(char *s);
  void     add_entry(string &, string &, string &, 
		     string &, string &, string &, string &, string &);
  string   fullsection();
  void     simpleshow(string );
  ostream &menudefs_gen(ostream &);
  void     menudefs_gen(string );
  /*  ostream &menudefs_fvwm2(ostream &);
      void     menudefs_fvwm2(string );
      ostream &mainmenu_fvwm2(ostream &);
      void     mainmenu_fvwm2(string );
      ostream &menudefs_fvwm(ostream &);
      void     menudefs_fvwm(string );
      ostream &menudefs_twm(ostream &);
      void     menudefs_twm(string );
      ostream &menudefs_9wm(ostream &);
      void     menudefs_9wm(string );
      istream &readfile_gen(istream &);
      */
  void     readfile_gen(string);
  ostream &writefile_gen(ostream &);
  void     writefile_gen(string);
  istream &readfile(istream &);
  void     readfile(string);
  ostream &writefile(ostream &);
  void     writefile(string);
};

class subst{
public:
  virtual string get(menuentry *)=NULL;
};

class subst_string:public subst{
private:
  string data;
public:
  subst_string(string s);
  string get(menuentry *);
};

typedef string (menuentry::*menuentry_f)(void);

class subst_var:public subst{
protected:
  menuentry_f f;
public:
  subst_var(string v){
    f=str_to_menu_f(v);
  };
  menuentry_f str_to_menu_f(string f);
};

class subst_var_plain:public subst_var{
public:
  subst_var_plain(string v)
    :subst_var(v){};
  string get(menuentry *);
};
class subst_var_icon:public subst_var{
public:
  subst_var_icon(string v)
    :subst_var(v){};
  string get(menuentry *);
};
class subst_var_esc_sq:public subst_var{
public:
  subst_var_esc_sq(string v)
    :subst_var(v){};
  string get(menuentry *);
};
class subst_var_esc_dq:public subst_var{
public:
  subst_var_esc_dq(string v)
    :subst_var(v){};
  string get(menuentry *);
};
class subst_var_esc_bq:public subst_var{
public:
  subst_var_esc_bq(string v)
    :subst_var(v){};
  string get(menuentry *);
};

class substitutable{
private:
  vector<subst *> s;
public:
  substitutable(string &data);
  string get(menuentry *m);
};


class supportinfo{
public:
  string wrap;     //the contents of the wrap file.
  substitutable *wrap_gen; //also contents, but with $.{var} substituted.
  int preference;   //the lower the preference number, the more preferable
};

class configclass {
private:
  string comp; //compatibility
  string dbf;  //cach file
  string mainm,genm,rcf,exrcf;
  string rootpr, userpr, input2pr;
  string startm, endm;
  string submt;//submenutitle
  string mainmt;//title of mainmenu
  string treew; //how to walk through the tree of menu's

  map < string, supportinfo, less<string> > support;
  void read_support(istream &i);
  void readwrapfile(string filename, string &wrap);
  string addprefix(string &f);
  string addprefix_input(string &f);

  substitutable *startm_gen, *endm_gen, *submt_gen;

public:
  configclass(){
    treew="c(m)"; 
  };
  string &compat() {return comp;};
  string databasefile() {return addprefix(dbf);};
  string systemdatabasefile(){return rootpr+dbf;};
  string mainmenufile() {return addprefix(mainm);};
  string genmenufile()  {return addprefix(genm);};
  string rcfile()       {return addprefix(rcf);};
  string examplercfile(){return addprefix_input(exrcf);};
  string startmenu()    {return startm;};
  string endmenu()      {return endm;};  
  string submenutitle() {return submt;};  
  string mainmenutitle(){return mainmt;};  
  string treewalk()     {return treew;};
  void   read(istream &i);
  int    supportpref(string &s);
  string &supportwrap(string &s){
    return (*support.find(s)).second.wrap;};
  ostream &showoptions(ostream &);
  string startmenu_gen(menuentry *m)   {return startm_gen->get(m);};
  string endmenu_gen(menuentry *m)     {return endm_gen->get(m);};
  string submenutitle_gen(menuentry *m){return submt_gen->get(m);};
  string supportwrapgen(string &s, menuentry *m){
    return (*support.find(s)).second.wrap_gen->get(m);};
};



#endif
