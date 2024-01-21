#include <strstream.h>
#include <iostream.h>
#include <fstream.h>
#include <set.h>
#include <list.h>
#include <getopt.h>
#include <ctype.h>
#include <unistd.h>
#include <stdlib.h>
#include <values.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include "menu-method.h"
#include <sys/stat.h>

int show_time=0, verbose=0, dodebug=0;

prefix_func    prefix_obj;
ifroot_func    ifroot_obj;

print_func     print_obj;
ifempty_func   ifempty_obj;
ifnempty_func  ifnempty_obj;
iffile_func    iffile_obj;
ifelse_func    ifelse_obj;
ifeq_func      ifeq_obj;
ifneq_func     ifneq_obj;
ifeqelse_func  ifeqelse_obj;

cond_surr_func cond_surr_obj;
esc_func       esc_obj;
escwith_func   escwith_obj;
escfirst_func  escfirst_obj;
replacewith_func replacewith_obj;
cppesc_func    cppesc_obj;
parent_func    parent_obj;
basename_func  basename_obj;

menuentry menu;
configinfo *config;
supportedinfo *supported;
int linenumber=-123456;

ofstream *genoutputfile;

struct option long_options[] = { 
  { "showtime", no_argument, &show_time, 1 }, 
  { "verbose", no_argument, &verbose, 1 }, 
  { "debug", no_argument, &dodebug, 1 }, 
  { "help", no_argument, NULL, 'h' }, 
  { "stdin", no_argument, NULL, 'f' }, 
  { NULL, 0, NULL, 0 } };

void usage(){
  cerr<<"menu-method: generate window-manager 'rc' files (or html docs)"<<endl
      <<"  menu-method gets the menuentries from standard in."<<endl
      <<"  Options to menu-method: "<<endl
      <<"     -h --help : this message"<<endl
      <<"     -d --debug: show debug info"<<endl
      <<"     -showtime : show timeing information"<<endl;
  exit(1);
}
bool empty_string(const String &s){
  if(s.length()&&(s!="none"))
    return false;
  else
    return true;
}


cat_str *get_eq_cat_str(parsestream &i){
  i.skip_space();
  i.skip_char('=');
  return new cat_str(i);
}

int check_dir(String s){
  String t;
  int i;
  if(dodebug)
    cerr<<"CHECK_DIR: "<<s<<endl;
  while(s.length()){
    i=s.index('/');
    if(i>=0){
      t=s.before('/')+'/';
      for(; ((unsigned int)i<s.length()) && (s[i]=='/'); i++);
      s=s.after(i-1);
    } else {
      t=s;
      s="";
    }
    if(chdir(t)){
      if(dodebug)
	cerr<<"MKDIR "<<t<<endl;
      if(mkdir(t,0755))
	throw dir_createerror(t);
      if(chdir(t))
	throw dir_createerror(t);
    }
  }
  return !s.length();
}

void closegenoutput(){
  (*genoutputfile)<<config->postoutput();
  delete genoutputfile;
}
void genoutput(const String &s,
	       map<string, string, less<string> > &v){
  
  static String outputname="";
  const char *t;
  String name;
  String dir;

  name=config->prefix()+"/"+config->genmenu->soutput(v);
  if(dodebug)
    cerr<<"GENOUTPUT: name="<<name<<endl;
  if(!(name==outputname)){
    if(genoutputfile)
      closegenoutput();
    outputname=name;
    check_dir(String_parent(outputname));
    t=outputname;
    genoutputfile=new ofstream(t);
    (*genoutputfile)<<config->preoutput();    
  }
  (*genoutputfile)<<s;
}

/////////////////////////////////////////////////////
//  Construction:
//


cat_str::cat_str(parsestream &i){
  char c;
  c=i.get_char();
  i.put_back(c);
  try{
    while((c=i.get_char())){
      if(isspace(c))
	continue;
      i.put_back(c);
      if(isalpha(c))
	v.push_back(new func_str(i));
      else if(c=='\"') 
	v.push_back(new const_str(i));
      else if(c=='$') 
	v.push_back(new var_str(i));
      else if(c==',')
	break;
      else if(c==')')
	break;
      else if(c=='\0')
	break;
      else
	throw char_unexpected(c);
    }
  }catch(endoffile){};
}

const_str::const_str(parsestream &i){
  data=i.get_stringconst();
}

var_str::var_str(parsestream &i){
  i.skip_char('$');
  var_name=i.get_name();
}

func_str::func_str(parsestream &i){
  char c;
  string name;
  
  name=i.get_name();
  if(name=="prefix")          f=&prefix_obj;
  else if(name=="ifroot")     f=&ifroot_obj;
  else if(name=="print")      f=&print_obj;
  else if(name=="ifempty")    f=&ifempty_obj;
  else if(name=="ifnempty")   f=&ifnempty_obj;
  else if(name=="iffile")     f=&iffile_obj;
  else if(name=="esc")        f=&esc_obj;
  else if(name=="ifelse")     f=&ifelse_obj;
  else if(name=="ifeq")       f=&ifeq_obj;
  else if(name=="ifneq")      f=&ifneq_obj;
  else if(name=="ifeqelse")   f=&ifeqelse_obj;
  else if(name=="cond_surr")  f=&cond_surr_obj;
  else if(name=="escwith")    f=&escwith_obj;
  else if(name=="escfirst")   f=&escfirst_obj;
  else if(name=="replacewith")f=&replacewith_obj;
  else if(name=="cppesc")     f=&cppesc_obj;
  else if(name=="parent")     f=&parent_obj;
  else if(name=="basename")   f=&basename_obj;
  
  else{
    cerr<<"Unknown function "+name<<endl;
    throw informed_fatal();
  }
  i.skip_space();
  i.skip_char('(');
  do{
    c=i.get_char();
    if(c==')')
      break;
    i.put_back(c);
    args.push_back(new cat_str(i));
  } while((c=i.get_char())&&(c==','));
  i.put_back(c);
  i.skip_char(')');
  if(args.size()!=(unsigned int)f->nargs())
    throw narg_mismatch(&(name[0]));
}


/////////////////////////////////////////////////////
//  Output routines
//

ostream &const_str::output(ostream &o, 
			   map<string, string, less<string> > &/*menuentry*/){
  return o<<data;
}

String cat_str::soutput(map<string, string, less<string> > &menuentry){
  char buf[MAX_BUF];
  vector <str * >::iterator i;
  ostrstream s(buf,sizeof(buf));
  
  for(i=v.begin();i!=v.end();i++)
    (*i)->output(s,menuentry);
  s<<ends;
  
  return String(buf);
}
ostream &cat_str::output(ostream &o, 
			 map<string, string, less<string> > &menuentry){
  o<<soutput(menuentry);
  return o;
}
void cat_str::output(map<string, string, less<string> > &menuentry){
  genoutput(soutput(menuentry),menuentry);
}

ostream &var_str::output(ostream &o,
			 map<string, string, less<string> > &menuentry){
  return o<<menuentry[var_name];
}
ostream &func_str::output(ostream &o,
			 map<string, string, less<string> > &menuentry){
  return f->output(o,args,menuentry);
}

/////////////////////////////////////////////////////
//  Debug routines
//
ostream &const_str::debug(ostream &o){
  return o<<"CONST_STR: "<<data<<endl;
}
ostream &cat_str::debug(ostream &o){
  vector<str *>::iterator i;
  o<<"CAT_STR: "<<endl;
  for(i=v.begin();i!=v.end();i++){
    (*i)->debug(o);
  }
  return o;
}
ostream &var_str::debug(ostream &o){
  return o<<"VAR_STR: "<<var_name<<endl;
}
ostream &func_str::debug(ostream &o){
  o<<"FUNC_STR: "<<f->name()<<" ("<<endl;
  vector<cat_str *>::iterator i;  
  for(i=args.begin();i!=args.end();i++){
    o<<", ";
    (*i)->debug(o);
  }
  return o<<")"<<endl;
}


/////////////////////////////////////////////////////
//  Function definitions:
//

ostream &prefix_func::output(ostream &o, vector<cat_str *> &,
			     map<string, string, less<string> > &){

  return o<<config->prefix();
}
ostream &ifroot_func::output(ostream &o, vector<cat_str *> & args,
			     map<string, string, less<string> > &menuentry){
  if(getuid())
    args[1]->output(o,menuentry);
  else
    args[0]->output(o,menuentry);

  return o;
}

ostream &print_func::output(ostream &o, vector<cat_str *> &args,
			    map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);

  if(empty_string(s)){
    cerr<<"Zero-size argument to print function";
    throw informed_fatal();
  }
  return o<<s;
}
ostream &ifempty_func::output(ostream &o, vector<cat_str *> &args,
			      map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);

  if(empty_string(s))
    args[1]->output(o,menuentry);
  return o;
}
ostream &ifnempty_func::output(ostream &o, vector<cat_str *> &args,
			       map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);
  if(!empty_string(s))
    args[1]->output(o,menuentry);
  return o;
}
ostream &iffile_func::output(ostream &o, vector<cat_str *> &args,
			     map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);
  ifstream f(s);
  if(f)
    args[1]->output(o,menuentry);
  return o;
}

ostream &esc_func::output(ostream &o, vector<cat_str *> &args,
			  map<string, string, less<string> > &menuentry){

  return o<<escape_string(args[0]->soutput(menuentry),
			  args[1]->soutput(menuentry));
}
ostream &escwith_func::output(ostream &o, vector<cat_str *> &args,
			      map<string, string, less<string> > &menuentry){

  return o<<escapewith_string(args[0]->soutput(menuentry),
			      args[1]->soutput(menuentry),
			      args[2]->soutput(menuentry));
}
ostream &escfirst_func::output(ostream &o, vector<cat_str *> &args,
			      map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);
  String esc=args[1]->soutput(menuentry);
  String t;
  int  i;

  for(i=0;(unsigned int)i!=s.length();i++){
    if(esc.contains(s[i])){
      t+=args[2]->soutput(menuentry);
      t+=s.after(i-1);
      break;
    }
    t+=s[i];
  }
  return o<<t;
}
ostream &replacewith_func::output(ostream &o, vector<cat_str *> &args,
				  map<string, string, less<string> > &menuentry){

  return o<<replacewith_string(args[0]->soutput(menuentry),
			       args[1]->soutput(menuentry),
			       args[2]->soutput(menuentry));
}
ostream &cppesc_func::output(ostream &o, vector<cat_str *> &args,
			      map<string, string, less<string> > &menuentry){

  return o<<cppesc_string(args[0]->soutput(menuentry));
}

ostream &ifelse_func::output(ostream &o, vector<cat_str *> &args,
			     map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);

  (!empty_string(s)) ?
    args[1]->output(o,menuentry)
    :
    args[2]->output(o,menuentry);
  return o;
}
ostream &ifeq_func::output(ostream &o, vector<cat_str *> &args,
			   map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);
  String t=args[1]->soutput(menuentry);
  if(s==t)
    args[2]->output(o,menuentry);
  
  return o;
}
ostream &ifneq_func::output(ostream &o, vector<cat_str *> &args,
			   map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);
  String t=args[1]->soutput(menuentry);
  if(!(s==t))
    args[2]->output(o,menuentry);
  
  return o;
}
ostream &ifeqelse_func::output(ostream &o, vector<cat_str *> &args,
			   map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);
  String t=args[1]->soutput(menuentry);
  if(s==t)
    args[2]->output(o,menuentry);
  else
    args[3]->output(o,menuentry);

  return o;
}

ostream &cond_surr_func::output(ostream &o, vector<cat_str *> &args,
				map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);

  if(!empty_string(s)){
    args[1]->output(o,menuentry);
    args[0]->output(o,menuentry);
    args[2]->output(o,menuentry);
  }
  return o;
}

ostream &parent_func::output(ostream &o, vector<cat_str *> &args,
			     map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);

  return o<<String_parent(s);
}

ostream &basename_func::output(ostream &o, vector<cat_str *> &args,
			       map<string, string, less<string> > &menuentry){

  String s=args[0]->soutput(menuentry);
  String t;
  int  i,p,q;
  
  for(i=0,p=-1,q=0;(unsigned int)i!=s.length();i++)
    if(s[i]=='/'){
      q=p;
      p=i;
    }
  if(p<0)
    return o;
  else
    return o<<s.at(q+1,p-q-1);
}

/////////////////////////////////////////////////////
//  Supported stuff
//

supportedinfo::supportedinfo(parsestream &i){
  char c=-1;
  linenumber=1;
  int prec=0;
  try{
    while(1){
      string name;
      while(1){
	i.skip_space();
	c=i.get_char();
	if(!c)
	  break;
	if(c=='#'){
	  i.skip_line();
	  break;
	}
	i.put_back(c);
	name=i.get_name();
	if(name=="endsupported"){
	  i.skip_line();
	  return;
	}
	name=upcase(name);
	if(dodebug)
	  cerr<<"SUPPORTED_CONSTRUCT: name="<<name<<endl;
	i.skip_space();
	i.skip_char('=');
	
	supinf inf;
	inf.c=new cat_str(i);
	inf.prec=prec++;
	sup[name]=inf;
	if(dodebug)
	  sup[name].c->debug(cerr);
      }
      i.skip_line();   //read away the final newline
    }  
  }
  catch(endoffile){}
  catch(char_expected c){
    cerr<<"\""<<c.c<<" expected in line "<<i.linenumber()<<", while parsing supported stuff"<<endl;
    throw informed_fatal();
  }
  catch(char_unexpected c){
    cerr<<"unexpected character \""<<c.c<<"\" at line "<<i.linenumber()<<", while parsing supported stuff"<<endl;
    throw informed_fatal();
  }
}

void supportedinfo::subst(map<string, string, less<string> > vars){

  map<string, string, less<string> >::iterator i;
  map<string, supinf, less<string> >::iterator j;

  if((i=vars.find(NEEDS_VAR))==vars.end()){
    cerr<<"Undefined "NEEDS_VAR" variable in menuentries"<<endl;
    throw informed_fatal();
  }
  if((j=sup.find(upcase((*i).second)))==sup.end()){
    cerr<<"Unknown "NEEDS_VAR"=\""<<(*i).second<<"\""<<endl;
    throw informed_fatal();
  }
  genoutput((*j).second.c->soutput(vars), vars);
}
int supportedinfo::prec(string &name){
  map<string, supinf, less<string> >::iterator i;
  int p;
  if((i=sup.find(upcase(name)))==sup.end())
    p=MAXINT;
  else
    p=(*i).second.prec;
  if(dodebug)
    cerr<<"PREC: name="<<upcase(name)<<"="<<p<<endl;
  return p;
}

ostream &supportedinfo::debuginfo(ostream &o){
  map<string, supinf, less<string> >::iterator i;
  for(i=sup.begin();i!=sup.end();i++){
    o<<"SUPPORTED:** name="<<(*i).first<<", prec="
	<<(*i).second.prec<<" Def="<<endl;
    (*i).second.c->debug(o);
  }
  return o;
}
/////////////////////////////////////////////////////
//  configinfo
//
configinfo::configinfo(parsestream &i){
  char c;
  string errmsg("Parse error: String constant expected");
  linenumber=1;
  treew="c(m)";
  prerun=postrun=genmenu=hkexclude=startmenu=endmenu=submenutitle=NULL;
  preout="#Automatically generated file. Do not edit (see /usr/doc/menu/README)\n\n";
  roots="/Debian";
  try{
    while(1){
      while((c=i.get_char())){
	string name,arg;
	if(c=='#'){
	  i.skip_line();
	  continue;
	} else
	  i.put_back(c);
	name=i.get_name();
	i.skip_space();
	if(name=="supported"){
	  i.skip_line();
	  supported=new supportedinfo(i);
	}
	else if(name=="startmenu")
	  startmenu=get_eq_cat_str(i);
	else if(name=="endmenu")
	  endmenu=get_eq_cat_str(i);
	else if(name=="submenutitle")
	  submenutitle=get_eq_cat_str(i);
	else if(name=="hotkeyexclude")
	  hkexclude=get_eq_cat_str(i);
	else if(name=="genmenu")
	  genmenu=get_eq_cat_str(i);	
	else if(name=="postrun")
	  postrun=get_eq_cat_str(i);	
	else if(name=="prerun")
	  prerun=get_eq_cat_str(i);	

	else if(name=="compat"){
	  if((compt=i.get_eq_stringconst())!="menu-1"){
	    cerr<<"Unknown compat mode "<<compt<<" in config file (only know of \""COMPAT_MODE"\"";
	    throw informed_fatal();
	  }
	}
	else if(name=="rcfile")
	  rcf=i.get_eq_stringconst();
	else if(name=="examplercfile")
	  exrcf=i.get_eq_stringconst();
	else if(name=="mainmenutitle")
	  mainmt=i.get_eq_stringconst();
	else if(name=="rootsection")
	  roots=i.get_eq_stringconst();
	else if(name=="rootprefix")
	  rootpref=i.get_eq_stringconst();
	else if(name=="userprefix")
	  userpref=i.get_eq_stringconst();
	else if(name=="treewalk")
	  treew=i.get_eq_stringconst();
	else if(name=="postoutput")
	  postout=i.get_eq_stringconst();
	else if(name=="preoutput")
	  preout=i.get_eq_stringconst();
	else if(name=="command"){
	  system(i.get_eq_stringconst());
	  exit(0);
	}
	else if(name=="hotkeycase"){
	  String s=i.get_eq_stringconst();
	  if(s=="sensitive")
	    hotkeycase=1;
	  else if (s=="insensitive")
	    hotkeycase=0;
	  else {
	    cerr<<"install-menus hotkeycase can only be {,in}sensitive"<<endl;
	    throw informed_fatal();
	  }
	}
	else{
	  cerr<<"Unknown identifier \""<<name<<"\" in script"<<endl;
	  throw informed_fatal();
	}
      }
      i.skip_line();//read away final newline
    }
  }
  catch(endoffile){}
  catch(char_expected c){
    cerr<<"\""<<c.c<<" expected in line "<<i.linenumber()<<", while parsing startupscript"<<endl;
    throw informed_fatal();
  }
  catch(char_unexpected c){
    cerr<<"unexpected character \""<<c.c<<"\" at line "<<i.linenumber()<<", while parsing startupscript"<<endl;
    throw informed_fatal();
  }
  catch(ident_expected){
    cerr<<"Identifier expected in line "<<i.linenumber()<<", while parsing startupscript"<<endl;
    throw informed_fatal();
  }
  catch(narg_mismatch f){
    cerr<<"Number of arguments to function "<<f.name<<" doesn't match, at line "<<i.linenumber()<<endl;
    throw informed_fatal();
  }

}

String configinfo::prefix(){
  if(getuid())
    return String(getenv("HOME"))+"/"+userprefix();
  else
    return rootpref;
}

ostream &configinfo::debug(ostream &o){
    o<<"Using compatibility with:"<<compt<<endl
     <<"mainmenutitle   : "<<mainmt   <<endl
     <<"rootsection     : "<<roots    <<endl
     <<"rcfile          : "<<rcf      <<endl
     <<"examplercfile   : "<<exrcf    <<endl
     <<"root-prefix     : "<<rootpref <<endl
     <<"user-prefix     : "<<userpref <<endl
     <<"startmenu       : "<<endl;
    if(startmenu)
      startmenu->debug(o);
    o<<"endmenu         : "<<endl;
    if(endmenu)
      endmenu->debug(o);
    o<<"genmenu         : "<<endl;
    if(genmenu)
      genmenu->debug(o);
    o<<"submenutitle    : "<<endl;
    if(submenutitle)
      submenutitle->debug(o);
  o<<"mainmenutitle   : "<<mainmt   <<endl
   <<"treewalk        : "<<treew    <<endl;
  return o;
}

/////////////////////////////////////////////////////
//  Menuentry
//

void menuentry::add_entry(string sec, string fullsec, 
			  map<string, string, less<string> > &v){
  if(dodebug)
    cerr<<"ADDENTRY: sec="<<sec<<"needs="<<v["needs"]<<endl;
  if(sec.length()!=0){
    string name;
    map <string, menuentry *, less<string> >::iterator i;
    if(sec.contains("/")){
      name=sec.before("/");
      sec=sec.after("/");
    }
    else{
      name=sec;
      sec="";
      if(!v[COMMAND_VAR].length()){
	vars[TITLE_VAR]=name;
	vars=v;
	vars[SECTION_VAR]=fullsec;
	return;
      }
    }
    if(dodebug)
      cerr<<"ADD_ENTRY: name="<<name<<", sec="<<sec<<endl;
    if((i=submenus.find(name))!=submenus.end()){
      if (!sec.length()){
	if((supported->prec(((*i).second)->vars[NEEDS_VAR]) >
	    supported->prec(v[NEEDS_VAR]))){
	  delete submenus[name];
	  submenus[name]=new menuentry;
	} else
	  return;
      }
    }else
      submenus[name]=new menuentry;
    submenus[name]->add_entry(sec, fullsec+"/"+name , v);
    if(!submenus[name]->vars[TITLE_VAR].length())
      submenus[name]->vars[TITLE_VAR]=name;
  } else 
    vars=v;
  vars[SECTION_VAR]=fullsec;
}
void menuentry::output(){
  string treew=config->treewalk();
  map <string, menuentry *, less<string> >::iterator i;

  for(unsigned int j=0;j<treew.length(); j++){
    bool children_too=false;
    switch(treew[j]){
    case 'c':
      for(i=submenus.begin(); i!=submenus.end(); i++)
	if((*i).second->submenus.size())
	  (*i).second->output();
      break;
    case '(':
      if(submenus.size())
	if(config->startmenu)
	  config->startmenu->output(vars);
      break;
    case ')':
      if(submenus.size())
	if(config->endmenu)
	  config->endmenu->output(vars);
      break;
    case 'M':
      children_too=true;
    case 'm':;
      for(i=submenus.begin(); i!=submenus.end(); i++)
        if((*i).second->vars[COMMAND_VAR].length())
          supported->subst((*i).second->vars);
        else{
	  if(config->submenutitle)
	    config->submenutitle->output((*i).second->vars);
          if(children_too)
            (*i).second->output();
        }
    }
  }
}

void menuentry::postprocess(){
  map <string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if((*i).second->submenus.size())
      (*i).second->postprocess();
  generate_hotkeys();
}
char menuentry::hotkeyconv(char h){
  if (config->hotkeycase)
    return h;
  else
    return tolower(h);
}
void menuentry::generate_hotkeys(){
  unsigned int i,j;
  list<int>::iterator k, old_k;
  map <string, menuentry *, less<string> >::iterator subi;
  map <string, string, less<string> >::iterator l;
  vector<String> keys;
  list<int> todo;
  set<char, less<char> > used_chars;
  String s;
  char c;
  
  if(config->hkexclude)
    s=config->hkexclude->soutput(vars);
  for(i=0;i!=s.length();i++)
    used_chars.insert(hotkeyconv(s[i]));

  for(subi=submenus.begin(), i=0; subi!=submenus.end(); subi++, i++){
    todo.push_back(i);
    l=(*subi).second->vars.find(HOTKEY_VAR);
    if(l!=(*subi).second->vars.end())
      keys.push_back((*l).second);
    else
      keys.push_back('\0');
    keys[i]+=sort_hotkey((*subi).second->vars[TITLE_VAR]);
  }
  j=0;
  while(todo.size()){
    for(k=todo.begin();k!=todo.end();){
      i=*k; 
      old_k=k++;//k++ here, to be able to todo.erase(old_k) safely.
      if(j>=keys[i].length()){
	keys[i]='\0';
	todo.erase(old_k);  //no hotkey found -- give up on this entry.
	continue;
      }
      c=keys[i][j];
      if(c){
	if(used_chars.find(hotkeyconv(c))==used_chars.end()){
	  todo.erase(old_k); //found a hotkey for this entry.
	  keys[i]=c;
	  used_chars.insert(hotkeyconv(c));
	  continue;
	}
	else
	  keys[i][j]='\0';
      }
    }
    j++;
  }
  for(subi=submenus.begin(), i=0; subi!=submenus.end(); subi++, i++){
    c=keys[i][0];
    if(c){
      (*subi).second->vars[HOTKEY_VAR]=c;
    }
  }
}
/////////////////////////////////////////////////////
//  Misc
//

void read_input(parsestream &i){
  char c;
  String s;
  try{
    while(1){
      map <string, string, less<string> > m;
      string name, val;
      string sec;
      do{
	name=i.get_name();
	val=i.get_eq_stringconst();
	m[name]=val;
	try{
	  c=i.get_char();
	  i.put_back(c);
	}catch(endoffile){
	  c=0;
	}
      } while(c);
      sec=m[SECTION_VAR]+'/';
      if(m.find(SORT_VAR)!=m.end())
	sec=sec+m[SORT_VAR]+":";
      sec=sec+replace(m[TITLE_VAR],'/','_');
      if(supported->prec(m[NEEDS_VAR])!=MAXINT)
	menu.add_entry(sec,config->rootsection(),m);
      i.skip_line();   //read away the final newline
    }
  }
  catch(endoffile){}
  catch(char_expected c){
    cerr<<"\""<<c.c<<" expected in line "<<i.linenumber()<<", while parsing stdinput"<<endl;
    throw informed_fatal();
  }
  catch(char_unexpected c){
    cerr<<"unexpected character \""<<c.c<<"\" at line "<<i.linenumber()<<", while parsing stdinput"<<endl;
    throw informed_fatal();
  }
  catch(ident_expected){
    cerr<<"Identifier expected in line "<<i.linenumber()<<", while parsing stdinput"<<endl;
    throw informed_fatal();
  }

}

void includemenus(string o, string i,
                  string rep, string m){
  //copy filename i to filename o, replacing the line
  //rep with menu-file m
  char buf[MAX_BUF];
  char c;
  bool changed=false;
  ifstream fi(i);
  ifstream fm(m);
  ofstream fo(o);
  
  if(!fi){
    cerr<<"Cannot open file "<<i<<endl; throw informed_fatal();}
  if(!fm){
    cerr<<"Cannot open file "<<m<<endl; throw informed_fatal();}
  if(!fo){
    cerr<<"Cannot open file "<<o<<endl; throw informed_fatal();}

  try{
    while(fi.get(buf,sizeof(buf))){
      if(String(buf).contains(rep,0)){
	while(fm.get(buf,sizeof(buf))){
	  fo<<buf<<endl;
	  fm.get(c);
	}
	changed=true;
      }
      else
	fo<<buf<<endl;
      fi.get(c);
    }
  }
  if(!changed)
    cerr<<"Warning: String \""<<rep
	<<"\" didn't occur in example file "<<i<<endl;
}


int main(int argc, char **argv){
  int c, option_index;
  char script[MAX_BUF];
  //  ofstream *menudef;
  string menudefname;
  
  try{
    if(argv[1])
      strcpy(script, argv[1]);
    else{
      cerr<<"install-menu: First parameter must be name of script"<<endl;
      throw informed_fatal();
    }

    while(1){
      c = getopt_long (argc, argv, "fdvh", long_options, &option_index);
      if(c==-1) 
	break;
      switch(c){
      case '?': 
	cerr<<"Try  --help for more information.\n"<<endl;
	throw informed_fatal();
      case 'd': dodebug=1; break;
      case 'v': verbose=1;break;
      case 'h': usage(); break;
      case 'f':break;
      }
    } while (c!=-1);
    ifstream i(script);
    if(!i){
      cerr<<"Cannot open script "<<script<< " for reading"<<endl;
      throw informed_fatal();
    }
    parsestream ps(i);
    config=new configinfo(ps);
    if(dodebug)
      supported->debuginfo(cerr);
    if(config->prerun)
      system(config->prerun->soutput(menu.vars));
    /*    menudefname=userprefix+config->genmenu();
	  menudef=new ofstream(menudefname);
	  if(!(*menudef)){
	  cerr<<"Cannot open "<<menudefname<<" for writing";
	  throw informed_fatal();
	  }
    */
    parsestream psscript(cin);
    read_input(psscript);
    if(menu.vars[TITLE_VAR]=="")
      menu.vars[TITLE_VAR]=config->mainmenutitle();
    menu.postprocess();
    menu.output();
    //    delete menudef;
    closegenoutput();
    if(config->rcfile().length()&&config->examplercfile().length())
      includemenus(config->prefix()+"/"+config->rcfile(),
		   config->prefix()+"/"+config->examplercfile(),
		   "include-menu-defs",
		   config->prefix()+"/"+config->genmenu->soutput(menu.vars));
    if(config->postrun)
      system(config->postrun->soutput(menu.vars));
  }
  catch(informed_fatal){
    cerr<<argv[1]<<": Aborting."<<endl;
    exit(1);
  }
  catch(dir_createerror d){
    cerr<<argv[1]<<": Cannot create directory "<<d.name<<endl;
  }
}

