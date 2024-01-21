/*
  Little utility for fvmw-like window managers, to pick the right entries 
  in a menu file for installation and removal of menu entries.


  GPL, joost witteveen, joostje@debian.org.
  */
#include "fvwmgen.h"
#include "common.h"
#include <strstream.h>
#include <vector.h>
#include <algo.h>

#include <ctype.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/stat.h>

#define OLDINSTALLOPTION "--install"  //not supported any more!
#define GENCOMPAT        "gen"
#define FVWM2COMPAT      "fvwm2"
#define FVWMCOMPAT       "fvwm"
#define TWMCOMPAT        "twm"
#define NINEWMCOMPAT     "9wm"

//typedef enum {need_x11, need_text, need_other} need;

char *x11wrap;
char *textwrap;
char *wmwrap;
char *modulewrap;
char *module2wrap;
bool verbose=false;
bool debug=false;
bool force=false;
bool show_time=false; 

menuentry menudat(NULL);
configclass config;


void usage(){
  cerr<<"Usage: "<<endl
      <<"  install-fvwmgenmenu configfile --install-files  filenames"<<endl
      <<"         Install menu entries in files filenames"<<endl
      <<"  install-fvwmgenmenu configfile --remove  menuids"<<endl
      <<"         Remove menuid's from menu"<<endl
      <<"For more info, please read /usr/doc/menu/README"<<endl;
  exit(1);
}
void donotuseoldinstall(){
  cerr<<"This programme does not accept the old \"--install\" option,"<<endl
      <<"as this will not allow your menuentry to be recycled by the"<<endl
      <<"other menumanagers. Please use --install-file $filename,"<<endl
      <<"if you must use this programme, or, better, use update-menus,"<<endl
      <<"the wrapper that's supposed to execute this programme"<<endl
      <<"See also /usr/doc/menu/README"<<endl;
  exit(1);
}


////////////////////////////////////////////////////////////////////////
//
//    subst_ classes
//
////////////////////////////////////////////////////////////////////////

// subst_string
subst_string::subst_string(string s){
  data=s;
}
string subst_string::get(menuentry *){
  return data;
}

// subst_var
menuentry_f subst_var::str_to_menu_f(string f){
  if     (f=="type")    return menuentry::get_type;
  else if(f=="fullsection") return menuentry::fullsection;
  else if(f=="section") return menuentry::get_subsec;
  else if(f=="title")   return menuentry::get_title;
  else if(f=="command") return menuentry::get_command;//returns precomm
  else if(f=="id")      return menuentry::get_id;
  else if(f=="icon")    return menuentry::get_icon;
  else{ 
    cerr<<"In config file: Unknown variable \""<<f<<"\""<<endl;
    exit(1);
  }
}
string subst_var_plain::get(menuentry * m){
  string tmp=(m->*f)();
  return (m->*f)();
}
string subst_var_icon::get(menuentry * m){
  string tmp=(m->*f)();
  if((tmp=="none")||(tmp==""))
    return "";
  else{
    string proc="%";
    return proc+tmp+proc;
  }
}
string subst_var_esc_sq::get(menuentry * m){
  string tmp=(m->*f)();
  return escape_singlequotes(tmp);
}
string subst_var_esc_dq::get(menuentry * m){
  string tmp=(m->*f)();
  return escape_quotes(tmp);
}
string subst_var_esc_bq::get(menuentry * m){
  string tmp=(m->*f)();
  return escape_bothquotes(tmp);
}


// substitutable

string substitutable::get(menuentry *m){
  string t;
  unsigned int i;
  for(i=0; i<s.size(); i++)
    t+=s[i]->get(m);
  return t;
};

substitutable::substitutable(string &data){
  unsigned int j=0;
  char ch;
  string tmp;
  string var;
  for(j=0; j<data.size();j++){
    switch (data[j]){
    case '\\':
      j++;
      if(j>=data.size())
	break;
      switch (data[j]){
      case 'n':tmp+='\n';break;
      case 't':tmp+='\t';break;
      case 'r':tmp+='\r';break;
      case 'v':tmp+='\v';break;
      case 'b':tmp+='\b';break;
      case 'f':tmp+='\f';break;
      case 'a':tmp+='\a';break;
      case '$':tmp+='$';break;
      case '\\':tmp+='\\';break;
      };
      break;
    case '$':
      if(tmp.size()){
	s.push_back(new subst_string(tmp));
	tmp="";
      }
      j++;
      if((j+2)>=data.size())
	break;
      ch=data[j];
      if(ch!='{')
	j++;
      if(data[j]!='{'){
	cerr<<"In cofig file: $ isn't followed by { (like ${var}, $s{var}, $%{var}"<<endl;
	exit(1);
      }
      j++;
      var="";
      while((data[j]!='}')&&(j<data.size()))
	var+=data[j++];
      if(data[j]!='}'){
	cerr<<"in config file: string "<<data<<" has unmatched {}"<<endl;
	exit(1);
      }
      switch(ch){
      case '{':s.push_back(new subst_var_plain (var)); break;
      case '%':s.push_back(new subst_var_icon  (var)); break;
      case 's':s.push_back(new subst_var_esc_sq(var)); break;
      case 'd':; 
      case '@':s.push_back(new subst_var_esc_dq(var)); break;
      case 'b':s.push_back(new subst_var_esc_bq(var)); break;
      default:
	cerr<<"In config file: Unknown variable type: "<<ch<<"in string"<<data<<endl;
	exit(1);
      }
      break;
    default:
      tmp+=data[j];
    }
  }
  if(tmp.size()){
    s.push_back(new subst_string(tmp));
    tmp="";
  }
}

////////////////////////////////////////////////////////////////////////
//
//   configclass
//
////////////////////////////////////////////////////////////////////////

string configclass::addprefix(string &f){
  if(f.size()){
    if(getuid())
      return getenv("HOME")+userpr+f;
    else
      return rootpr+f;
  }
  return "";
}
string configclass::addprefix_input(string &f){
  string try1;
  string try2;
  char buf[maxline];
  if(f.size()){
    if(getuid())
      try1=getenv("HOME")+userpr+f;
    else
      try1=rootpr+f;
    try2=input2pr+f;
    ifstream i1(string2char(buf,try1));
    if(i1)
      return try1;
    else{
      ifstream i2(string2char(buf,try2));
      if(i2)
	return try2;
    }
  }
  return "";
}
int    configclass::supportpref(string &s){
  map < string, supportinfo, less<string> >::iterator i;
  i=support.find(s);
  if(i!=support.end())
    return (*i).second.preference;
  else
    return -1;
}
void configclass::readwrapfile(string filename, string &wrap){
  char buf[maxline];
  strncpy(buf,filename.data(),filename.size());
  buf[filename.size()]=0;
  ifstream f(buf);
  if(f){
    getline_nonempty(f,buf,sizeof(buf));
    wrap=buf;
  } 
  else
    wrap="";
}
void configclass::read_support(istream &i){
  char buf[maxline];
  int pref=0;
  char ch;

  while(getline_nonempty(i,buf,sizeof(buf))){
    string def,s;
    supportinfo sup;

    istrstream str(buf,strlen(buf));
    
    getword(str,s," \t:=");
    if(s=="endsupported")break;
    skip_spaces(str);
    str.get(ch);
    sup.preference=pref;
    switch(ch){
    case ':':
      getword(str,def," \t:");
      readwrapfile(def, sup.wrap);
      break;
    case '=':
      getword(str,sup.wrap);
      break;
    default:
      cerr<<"Unexpected delimmiter in config file"<<endl;
      exit(1);
    }
    sup.wrap_gen=new substitutable(sup.wrap);
    support[s]=sup;
    pref++;
  }
}

void configclass::read(istream &i){
  char buf[maxline];
  while(getline_nonempty(i,buf,sizeof(buf))){
    string s,def;
    istrstream str(buf,strlen(buf));
    
    getword(str,s," \t:");
    getword(str,def," \t:");
    if(s=="compat")        comp=def;
    else if(s=="database") dbf=def;
    else if(s=="mainmenu") mainm=def;
    else if(s=="genmenu")  genm=def;
    else if(s=="rcfile")   rcf=def;
    else if(s=="examplercfile") exrcf=def;
    else if(s=="supported") read_support(i);
    else if(s=="startmenu") {
      startm=def;
      startm_gen=new substitutable(def); }
    else if(s=="endmenu") {
      endm=def;
      endm_gen=new substitutable(def); }
    else if(s=="submenutitle") {
      submt=def;
      submt_gen=new substitutable(def); }
    else if(s=="mainmenutitle") mainmt=def;
    else if(s=="rootprefix") rootpr=def;
    else if(s=="userprefix") userpr=def;
    else if(s=="input2prefix") input2pr=def;
    else if(s=="treewalk") treew=def;
    else if(s=="endmanager") break;
    else {
      cerr<<"Unknown identifier \""<<s<<"\" in input script"<<endl;
      exit(1);
    }  
  }
}

ostream &configclass::showoptions(ostream &o){
  map < string, supportinfo, less<string> >::iterator i;  
  o<<"Using compatibility with:"<<compat()<<endl
   <<"databasefile    : "<<databasefile()<<endl
   <<"systemdatabasefile: "<<systemdatabasefile()<<endl
   <<"mainmenusfile   : "<<mainmenufile()<<endl
   <<"genmenufile     : "<<genmenufile()<<endl
   <<"rcfile          : "<<rcfile()<<endl
   <<"examplercfile   : "<<examplercfile()<<endl
   <<"root-prefix     : "<<rootpr<<endl
   <<"user-prefix     : "<<userpr<<endl
   <<"input2-prefix   : "<<input2pr<<endl
   <<"startmenu       : "<<startm<<endl
   <<"endmenu         : "<<endm<<endl
   <<"submenutitle    : "<<submt<<endl
   <<"mainmenutitle   : "<<mainmt<<endl
   <<"treewalk        : "<<treew<<endl
   <<"supported:"<<endl;
  for(i=support.begin();i!=support.end();i++)
    o<<"  "<<(*i).first<<", pref="<<(*i).second.preference
     <<", wrap=\""<<(*i).second.wrap<<"\""<<endl;
  o<<"endsupported"<<endl;
  return o;
}


////////////////////////////////////////////////////////////////////////
//
//   misc-functions
//
////////////////////////////////////////////////////////////////////////



string empty_mainmenu(string s){
  if(s.size())
    return s;
  else
    return "MainMenu";
}
string getlastsection(string s){
  return empty_mainmenu(getlastword(s,'/'));
}

void lowerstr(char *s){
  while((*s=tolower(*(s++))));
}

char * copystr(char **p, string &s){
  *p=new char[s.size()+2];
  strncpy(*p,s.data(),s.size());
  (*p)[s.size()]=0;
  return *p;
}

bool substitute(string &com,
		string search,
		string replacement){
  char buf[maxline];
  char sear[maxline];
  char *t,*u;
  bool changed=false;
  strncpy(buf,com.data(),com.size());
  buf[com.size()]=0;
  strncpy(sear,search.data(),search.size());
  sear[search.size()]=0;
  u=buf;
  com="";
  while((t=strstr(u,sear))){
    com+=string(u,t-u)+replacement;
    changed=true;
    u=t+search.size();
  } 
  com+=u;
  return changed;
}
bool substitutevar(string &com, string search, string &replacement){
  bool found=false;
  
  found=substitute(com,"${"+search+"}",replacement);
  found|= substitute(com,"$@{"+search+"}",escape_quotes(replacement));
  if((replacement=="none")||(replacement==""))
    found|=substitute(com,"$%{"+search+"}","");
  else
    found|=substitute(com,"$%{"+search+"}","%"+replacement+"%");
  return found;
}

string preprocessfvwm(map <string , fvwm2menu, less<string> >::iterator i,
		  string wrap){
  if(!substitutevar(wrap,"title",(*i).second.text)&&
     (config.compat()==FVWM2COMPAT))
    wrap="\""+(*i).second.text+"\" "+wrap;
  substitutevar(wrap,"section",(*i).second.section);
  substitutevar(wrap,"id",(*i).second.id);
  substitutevar(wrap,"icon",(*i).second.icon);
  if(!substitutevar(wrap,"command",(*i).second.command))
    wrap+=" "+(*i).second.command;
  return wrap;
}

string preprocess(string wrap, menuentry *i){
  string s;
  substitute(wrap,"\\n","\n");
  substitute(wrap,"\\t","\t");
  substitute(wrap,"\\r","\r");
  substitute(wrap,"\\v","\v");
  substitute(wrap,"\\b","\b");
  substitute(wrap,"\\f","\f");
  substitute(wrap,"\\a","\a");
  substitute(wrap,"\\\\","\\");
  s=(*i).fullsection();
  substitutevar(wrap,"command", (*i).precomm);
  substitutevar(wrap,"fullsection", s);
  substitutevar(wrap,"icon", (*i).icon);
  substitutevar(wrap,"id", (*i).id);
  if((*i).subsec.size())
    substitutevar(wrap,"section", (*i).subsec);
  else{ //bad hack, but I don't know how else to do it.
    s=config.mainmenutitle();
    substitutevar(wrap,"section",s);
  }
  substitutevar(wrap,"title", (*i).title);
  substitutevar(wrap,"type", (*i).type);
  space_strip(wrap);
  return wrap;
}

void run_install(map <string, fvwm2menu, less<string> > &id){
  //this whole routine can get rid of for "compat=gen".

  for(map<string, fvwm2menu, less<string> >::iterator i=id.begin();
      i!=id.end();
      i++){
    string command;
    if((*i).second.command.size()){
      if (config.compat()!="gen"){
	command=preprocessfvwm(i,config.supportwrap((*i).second.need));
	space_strip(command); //this is for an fvwm2 bug; maybe also in fvwm*?
      }
      else
	command="non-empty";
    }
    menudat.add_entry((*i).second.section,
		      (*i).second.section,
		      (*i).second.id,
		      (*i).second.icon,
		      (*i).second.text,
		      (*i).second.need,
		      (*i).second.command,
		      command);
  }
}

void install_file(const char *filename){
  istream *f;
  char buf[maxline];
  char s[maxline];
  int pref;
  map <string, fvwm2menu, less<string> > id;
  map <string, fvwm2menu, less<string> >::iterator i;
  if(!strcmp(filename,STDINOPTION))
    f=&cin;
  else
    f=new ifstream(filename);
  if(verbose)
    cerr<<"Installing file:"<<filename<<endl;
  if(*f){
    while(getline_nonempty(*f,buf,sizeof(buf))){
      fvwm2menu m;
      istrstream str(buf,strlen(buf));
      
      str>>s;
      lowerstr(s);
      m.need=s;
      pref=config.supportpref(m.need);
      getword(str,m.section); 
      getword(str,m.id);      
      getword(str,m.icon);    
      getword(str,m.text);    
      skip_spaces(str);
      getline_nonempty(str,m.command); 
      
      if(pref>=0){
	if((i=id.find(m.id))!=id.end()){
	  if(config.supportpref((*i).second.need) > pref){
	    if(verbose)
	      cerr<<"overriding old "<<id[m.id].id<<"-"<<id[m.id].need
		  <<" entry with "<<m.id<<"-"<<m.need
		  <<", as latter has lower preference (more prefrable)"<<endl;
	    id[m.id]=m;
	  } else if(verbose)
	    cerr<<"Keeping old "<<id[m.id].id<<"-"<<id[m.id].need
		<<" entry instead of using "<<m.id<<"-"<<m.need
		<<", as former has lower preference (more prefrable)"<<endl;
	} else 
	  id[m.id]=m;
      }
      else if(verbose)
	cerr<<"Ignoring "<<m.id<<", as it has an unsupported needs entry of "<<m.need<<endl;
    }
    run_install(id);
  }
  else
    cerr<<"Cannot open file "<<filename<<endl;
}


void readconfigfile(char *argv1){
  ifstream i(argv1);
  config.read(i);
  if(debug)
    config.showoptions(cerr);
}
void includemenus(string o, string i,
		  string rep, string m){
  //copy filename i to filename o, replacing the line
  //rep with menu-file m
   char buf[maxline];
   string s;
   bool changed=false;
   ifstream fi(string2char(buf,i));
   ifstream fm(string2char(buf,m));
   ofstream fo(string2char(buf,o));
   
   if(!fi){
     cerr<<"Cannot open file "<<i<<endl; exit(1);}
   if(!fm){
     cerr<<"Cannot open file "<<m<<endl; exit(1);}
   if(!fo){
     cerr<<"Cannot open file "<<o<<endl; exit(1);}
   while(getline(fi,s)){
     if(s==rep){
       while(getline(fm,s))
	 fo<<s<<endl;
       changed=true;
     }
     else
       fo<<s<<endl;
   }
   if(!changed)
     cerr<<"Warning: String \""<<rep<<"\" didn't occur in example file "<<i<<endl;
}



////////////////////////////////////////////////////////////////////////
//
//    menuentry
//
////////////////////////////////////////////////////////////////////////


void menuentry::remove_id(string &s){
  map < string, menuentry *, less<string> >::iterator i;
  vector< map < string, menuentry *, less <string > >::iterator > tobe_erased;
  
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if(!((*i).second->command.size()))
      (*i).second->remove_id(s);

  for(i=submenus.begin(); i!=submenus.end(); i++)
    if((*i).second->command.size()&&
       ((*i).second->id==s))
      tobe_erased.push_back(i);
  for(unsigned int j=0;j!=tobe_erased.size();j++)
    submenus.erase(tobe_erased[j]);
}

void menuentry::remove_id(char *s){
  string st(s);
  remove_id(st);
};


void menuentry::add_entry(string &sec, 
			  string &total_section, 
			  string &id,
			  string &icon,
			  string &tit, 
			  string &type,
			  string &precomm,
			  string &command){
  istrstream str(sec.data(),sec.size());
  char  s[maxline];
  char  c;
  char  sub[maxline];
  //section=total_section;
  if(sec.size())  //no, I don't understand why this test is needed.
    str.get(s,sizeof(s),'/');  
  else
    *s=0;
  if(debug)
    cerr<<"Adding "<<id<<" s=\""<<s<<"\""<<" sec="<<sec<<endl;
  if(strlen(s)&&str.good()){
    str.get(c);
    string entry(s);
    str.get(sub,sizeof(sub),'\0');
    string submen(sub);
    //cout<<"sumben="<<submen<<", sub="<<sub<<endl;
    if(submenus.find(entry)==submenus.end())
      submenus[entry]=new menuentry(this);  
    submenus[entry]->subsec=s;
    submenus[entry]->add_entry(submen,total_section,
			       id,icon,tit,type,precomm,command);
    //    submenus[entry]->section=s;
    //cout<<"upadded "<<id<<", submenus.size()="<<submenus.size()<<", entry="<<entry<<endl;        
  }else{
    if(submenus.find(tit)==submenus.end())
      submenus[tit]=new menuentry(this);
    submenus[tit]->section=total_section;
    submenus[tit]->title=tit;
    submenus[tit]->id=id;
    if(icon.size())
      submenus[tit]->icon=icon;
    submenus[tit]->command=command;
    submenus[tit]->precomm=precomm;
    submenus[tit]->type=type;    
    if(command=="")
      submenus[tit]->subsec=tit;
    else
      submenus[tit]->subsec=s;
    //cout<<"loaded "<<id<<", submenus.size()="<<submenus.size()<<endl;
  }
}
string menuentry::fullsection(){
  if(parent)
    return parent->fullsection()+"/"+subsec;
  else
    return "/Debian"+subsec;
}

void menuentry::simpleshow(string s){
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    (*i).second->simpleshow(section);
  cerr<<"UpSe=\""<<s<<"\" "
      <<"Secti=\""<<section<<"\" "
      <<"Title=\""<<title<<"\" "
      <<"Comma=\""<<command<<"\" "
      <<"id   =\""<<id<<"\" "
      <<"icon =\""<<icon<<"\" "
      <<"fulls=\""<<fullsection()<<"\" "
      <<endl;
}
ostream &menuentry::menudefs_gen(ostream &o){
  map < string, menuentry *, less<string> >::iterator i;
  string treew=config.treewalk();
  for(unsigned int j=0;j<treew.size();j++){
    bool children_too=false;
    switch (treew[j]){
    case 'c':
      for(i=submenus.begin(); i!=submenus.end(); i++)
	if(!((*i).second->command.size()))
	  (*i).second->menudefs_gen(o);
      break;
    case '(':
      if(submenus.size())
	o<<config.startmenu_gen(this);
      break;
    case ')':
      if(submenus.size())
	o<<config.endmenu_gen(this);
      break;
    case 'M':;
      children_too=true;
    case 'm':
      for(i=submenus.begin(); i!=submenus.end(); i++)
	if((*i).second->command.size())
	  o<<config.supportwrapgen(((*i).second->type),(*i).second);
	else{
	  o<<config.submenutitle_gen((*i).second);
	  if(children_too)
	    (*i).second->menudefs_gen(o);
	}
      break;
    }
  }
  return o;
}
void menuentry::menudefs_gen(string s){
  char buf[maxline];
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  ofstream o(buf);
  menudefs_gen(o);
}


/*ostream &menuentry::menudefs_fvwm2(ostream &o){
  bool startshown=0;
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if(!((*i).second->command.size()))
      (*i).second->menudefs_fvwm2(o);
  for(i=submenus.begin(); i!=submenus.end(); i++){
    if(!startshown){
      startshown=1;
      //o<<"DestroyMenu "<<(*i).second->section<<endl
      // <<"AddToMenu "<<(*i).second->section<<" "
      // <<(*i).second->section<<" Title"<<endl;
      o<<"DestroyMenu "<<empty_mainmenu(subsec)<<endl
       <<"AddToMenu "<<empty_mainmenu(subsec)<<" "
       <<empty_mainmenu(subsec)<<" Title"<<endl;

    }
    if((*i).second->command.size())
      o<<"+ " <<(*i).second->command<<" "<<endl;
    else
      o<<"+ \""<<(*i).second->subsec
       <<"\" PopUp "<<(*i).second->subsec<<endl;
  } 
  if(startshown)
    o<<endl;
  return o;
}
void menuentry::menudefs_fvwm2(string s){
  char buf[maxline];
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  ofstream o(buf);
  menudefs_fvwm2(o);
}

ostream &menuentry::menudefs_fvwm(ostream &o){
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if(!((*i).second->command.size()))
      (*i).second->menudefs_fvwm(o);
  if(submenus.size())
    o<<"Popup \""<<empty_mainmenu(subsec)<<"\""<<endl;
  
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if((*i).second->command.size())
      o<<"      "<<(*i).second->command<<endl;
    else
      o<<"     Popup \""
       <<getlastsection((*i).second->subsec)<<"\" "
       <<empty_mainmenu((*i).second->subsec)<<endl;
  if(submenus.size())
    o<<"EndPopup"<<endl<<endl;
  return o;
}
void menuentry::menudefs_fvwm(string s){
  char buf[maxline];
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  ofstream o(buf);
  menudefs_fvwm(o);
}
ostream &menuentry::menudefs_twm(ostream &o){
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if(!((*i).second->command.size()))
      (*i).second->menudefs_twm(o);
  if(submenus.size())
    o<<"menu  \""<<empty_mainmenu(subsec)<<"\""<<endl<<"{"<<endl;
  
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if((*i).second->command.size())
      o<<"  "<<(*i).second->command<<endl;
    else
      o<<" \""
       <<getlastsection((*i).second->subsec)<<"\" f.menu \""
       <<empty_mainmenu((*i).second->subsec)<<"\""<<endl;
  if(submenus.size())
    o<<"}"<<endl<<endl;
  return o;
}
void menuentry::menudefs_twm(string s){
  char buf[maxline];
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  ofstream o(buf);
  menudefs_twm(o);
}

ostream &menuentry::menudefs_9wm(ostream &o){
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if(!((*i).second->command.size()))
      (*i).second->menudefs_9wm(o);
  if(submenus.size())
    o<<"function "<<empty_mainmenu(subsec)<<" () {"<<endl;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if((*i).second->command.size())
      o<<" "<<(*i).second->command<<endl;
    else
      o<<"  echo \"\\\""
       <<getlastsection((*i).second->subsec)<<":$config --menu "
       <<empty_mainmenu((*i).second->subsec)<<"\\\"\""<<endl;
  if(submenus.size())
    o<<"}"<<endl<<endl;
  return o;
}
void menuentry::menudefs_9wm(string s){
  char buf[maxline];
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  ofstream o(buf);
  menudefs_9wm(o);
}

ostream &menuentry::mainmenu_fvwm2(ostream &o){
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    o<<"+ \""<<(*i).first
     <<"\" Popup "<<(*i).first<<endl;
  return o;
}
void menuentry::mainmenu_fvwm2(string s){
  char buf[maxline];
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  ofstream o(buf);
  mainmenu_fvwm2(o);
}
*/
istream &menuentry::readfile(istream &i){
  string type="";
  string precomm="";
  if(!force)
    while(i.good()&&!i.eof()){
      string section,id,icon,title,command;
      getword(i,section);
      getword(i,id);
      getword(i,icon);
      getword(i,title);
      getline_nonempty(i,command);
      if(id!="")
	menudat.add_entry(section,section,id,icon,title,type,precomm,command);
    }
  return i;
}
void menuentry::readfile(string s){
  char buf[maxline];
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  ifstream i(buf);
  readfile(i);
}
istream &menuentry::readfile_gen(istream &i){
  string command="";
  if(!force)
    while(i.good()&&!i.eof()){
      string type,section,id,icon,title,precomm;
      getword(i,type);
      getword(i,section);
      getword(i,id);
      getword(i,icon);
      getword(i,title);
      getline_nonempty(i,precomm);
      if(id!="")
	menudat.add_entry(section,section,id,icon,title,type,precomm,precomm);
    }
  return i;
}
void menuentry::readfile_gen(string s){
  char buf[maxline];
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  ifstream i(buf);
  readfile_gen(i);
}

ostream &menuentry::writefile(ostream &o){
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if(!((*i).second->command.size()))
      (*i).second->writefile(o);
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if((*i).second->command.size())
      o<<(*i).second->section<<" "
       <<(*i).second->id<<" "
       <<(*i).second->icon<<" "
       <<"\""<<(*i).second->title<<"\" "
       <<(*i).second->command
       <<endl;
  return o;
}
void menuentry::writefile(string s){
  char buf[maxline];
  if(s.size()){
    strncpy(buf,s.data(),s.size());
    buf[s.size()]=0;
    ofstream o(buf);
    if(o)
      writefile(o);
  }
}
ostream &menuentry::writefile_gen(ostream &o){
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if(!((*i).second->command.size()))
      (*i).second->writefile_gen(o);
  for(i=submenus.begin(); i!=submenus.end(); i++)
    if((*i).second->command.size()||(*i).second->icon.size())
      o<<(*i).second->type<<" "
       <<(*i).second->section<<" "
       <<(*i).second->id<<" "
       <<(*i).second->icon<<" "
       <<"\""<<(*i).second->title<<"\" "
       <<(*i).second->precomm
       <<endl;
  return o;
}
void menuentry::writefile_gen(string s){
  char buf[maxline];
  if(s.size()){
    strncpy(buf,s.data(),s.size());
    buf[s.size()]=0;
    ofstream o(buf);
    if(o)
      writefile_gen(o);
  }
}
menuentry::~menuentry(){
  map < string, menuentry *, less<string> >::iterator i;
  for(i=submenus.begin(); i!=submenus.end(); i++)
    delete (*i).second;
}


string menuentry::get_subsec(){
  if(subsec=="")
    return config.mainmenutitle();
  else
    return subsec;
};


////////////////////////////////////////////////////////////////////////
//
//   main
//
////////////////////////////////////////////////////////////////////////

int main(int argc, char **argv){
  bool install=true;
  int i;
  if(argc<2)
    usage();
  for(i=2;argv[i];i++){
    if(!strcmp(argv[i],"-h"))                 usage();
    else if(!strcmp(argv[i],INSTALLOPTION))   install=true;
    else if(!strcmp(argv[i],REMOVEOPTION))    install=false;
    else if(!strcmp(argv[i],"-v"))            verbose=true;
    else if(!strcmp(argv[i],"-d"))            verbose=debug=true;
    else if(!strcmp(argv[i],"-f"))            force=true;
    else if(!strcmp(argv[i],OLDINSTALLOPTION))donotuseoldinstall();
    else
      continue;
    argv[i]="";
  }
  readconfigfile(argv[1]);
  if(config.compat()==GENCOMPAT)
    menudat.readfile_gen (config.databasefile());
  else
    menudat.readfile (config.databasefile());
  argv++;
  while(*(++argv)){
    if(**argv)
      if(install)
	install_file(*argv);
      else 
	menudat.remove_id(*argv);
  }
  //the user's databasefile should only contain the user's entries,
  //so, write the (usually small) user databasefile now:
  if(config.compat()==GENCOMPAT)
    menudat.writefile_gen(config.databasefile());
  else
    menudat.writefile(config.databasefile());
  if (config.compat()==GENCOMPAT){
    //to create the user menus, use the full database.
    menudat.readfile_gen(config.systemdatabasefile());
    menudat.menudefs_gen(config.genmenufile());
  }
  /* else if(config.compat()==FVWM2COMPAT){
    //write the "menudefs" file
    menudat.menudefs_fvwm2(config.genmenufile());
    //and write the "mainmenu" file.
    menudat.mainmenu_fvwm2(config.mainmenufile());
  } else if(config.compat()==FVWMCOMPAT){
    //to create the user menus, use the full database.
    menudat.readfile (config.systemdatabasefile());
    menudat.menudefs_fvwm(config.genmenufile());
  } else if(config.compat()==TWMCOMPAT){
    //to create the user menus, use the full database.
    menudat.readfile (config.systemdatabasefile());
    menudat.menudefs_twm(config.genmenufile());
  } else if(config.compat()==NINEWMCOMPAT){
    //to create the user menus, use the full database.
    menudat.readfile (config.systemdatabasefile());
    menudat.menudefs_9wm(config.genmenufile());
  }
  */
  if(config.rcfile().size()&&config.examplercfile().size()){
    includemenus(config.rcfile(),config.examplercfile(),
		 "include-menu-defs",
		 config.genmenufile());
    /*    if(config.compat()==NINEWMCOMPAT){
      char buf[maxline];
      string s=config.rcfile();
      chmod(string2char(buf,s),0755);
    }
    */
  }
  if(debug)
    menudat.simpleshow("");

}

