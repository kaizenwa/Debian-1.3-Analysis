/*
  
  For info, see README file.
  
  Copyright: GPL
  
  joost witteveen, joostje@debian.org
*/

#include <string>
#include "common.h"
#include <iostream.h> 
#include <strstream.h>
#include <set.h>
#include <map.h>
#include <vector.h>
#include <algo.h>

#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <dirent.h>
#include <utime.h>
#include <string.h>
#include <signal.h>

#include "update-menus.h"


#define LOCALNAME "local"

//set < char *, strsort > status; //"installed", "removed"
//set < char *, strsort >::iterator status_installed, status_removed;
bool verbose=false, debug=false, show_time=false;

translations translate_type   (menuentry::get_type);    
translations translate_section(menuentry::get_section);
translations translate_id     (menuentry::get_id);     
translations translate_icon   (menuentry::get_icon);   
translations translate_title  (menuentry::get_title);  
translations translate_command(menuentry::get_command);

int min(int i, int j){
  if (i<j)
    return i;
  else
    return j;
}
int shortcmp(char *s, char *t){
  return strncmp(s,t,min(strlen(t),strlen(s)));
}
ostream &operator<<(ostream &o, set <char *, strsort> &m){
  set <char *, strsort>::iterator i;
  for(i=m.begin();i!=m.end();i++)
    o<<*i<<", ";
  return o;
}

ostream &operator<<(ostream &o, map <char *, char *, strsort> &m){
  map <char *, char *, strsort>::iterator i;
  for(i=m.begin();i!=m.end();i++)
    o<<(*i).first<<" = "<<(*i).second<<endl;
  return o;
}

ostream &operator<<(ostream &o, map <char *, menuclass, strsort> &m){
  map <char *, menuclass, strsort>::iterator i;
  for(i=m.begin();i!=m.end();i++){
    o<<(*i).first<<" = "<<(*i).second.file<<":"
     <<(*i).second.mod_time<<" ";
    o<<endl;
  }
  return o;
}

/////////////////////////////
//
//   menuentry
//
/////////////////////////////

menuentry::menuentry(char *p, istream &i){
  char buf[maxline];

  if(getline_nonempty(i,buf,sizeof(buf))){
    istrstream s(buf,strlen(buf));

    menu_ok=true;
    package=p;
    getword(s,type);
    getword(s,section);
    getword(s,id);
    getword(s,icon);
    getword(s,title);
    getline_nonempty(s,command);
  } else
     menu_ok=false;
}

ostream &operator<<(ostream &o, menuentry &m){
  o
    <<m.type<<" "
    <<m.section<<" "
    <<m.id<<" "
    <<m.icon<<" "
    <<"\""<<m.title<<"\"";
  if(m.command.size())
    o<<" "<<m.command;
  o<<endl;
  return o;
}

void menuentry::write(string &s){
  char buf[maxline];
  ostrstream str(buf,sizeof(buf));

  str<<*this<<"\n"<<ends;
  s.append(buf,strlen(buf));
}

/////////////////////////////
//
//   translate
//
/////////////////////////////
/* The name of the game:
If a translate file looks like:

  translate id->section
    gnuplot Apps
  endtranslate

and we've got a menufile with:
  x11 Apps/Emulators dosemuid none "Dosemu" xdos

Then the following variables are thus interpreted:

class menuentry{ 
  string package, ...  //they refer to the dosemu entry above
}
class translate{ //this is the translate rule above
  //becase the left-hand-side of the x->y stuff after the
  //translate line is "id", this instance of translate
  //will be put in translate_id.

  menuentry_f replace_attr;  //returns attr to be replaced (in this case, 
                             //replace_attr()=menuentry.id="dosemuid")
  string      match_string;  //what should be matched for this translate
                             //match_string="gnuplot"
  string    replace_string;  //what the matching string should be replaced with.
                             //replace_string="Apps"
                             //
}
  the match_attr that's still missing is in the translations_section
  class.
*/
bool translate_string(      string &menu_match,       string &menu_repl, 
		      const string &tran_match, const string &tran_repl){
  // old  =  current value that's in menuentry
  // match=  the lef-hand side in the translate file
  // replace=the right-hand side in the translate file

  if(menu_match==tran_match){
    menu_repl=tran_repl;
    return true;
  }else
    return false;
}
bool subtranslate_string(      string &menu_match,       string &menu_repl, 
  		         const string &tran_match, const string &tran_repl){
  if(!menu_match.compare(tran_match,0,tran_match.size())){
    menu_repl=tran_repl;
    return true;
  } else
    return false;
}
bool substitue_string(      string &menu_match,       string &menu_repl, 
		      const string &tran_match, const string &tran_repl){
  if(!menu_match.compare(tran_match,0,tran_match.size())){
    if(menu_match.size()<=tran_match.size()) //should never be <
      menu_repl=tran_repl;
    else{
      unsigned int i;
      string tmp;
      for(i=0; i<tran_repl.size(); i++)
	tmp+=tran_repl[i];
      for(i=tran_match.size(); i<menu_repl.size(); i++)
	tmp+=menu_repl[i];
      menu_repl=tmp;
    }
    return true;
  } else
    return false;
}

bool translate::replace(menuentry_f match_attr, menuentry &m) const{
  
  return (*replace_method)((m.*match_attr)(), 
			   (m.*replace_attr)(),
			   match_string, replace_string);
}

void do_translations(menuentry &m){
  translate_type.   process(m.type,    m);
  translate_section.process(m.section, m);
  translate_id.     process(m.id,      m);
  translate_icon.   process(m.icon,    m);
  translate_title.  process(m.title,   m);
  translate_command.process(m.command, m);
}

void read_translategroup(istream &i,
			 translations *tr_point,
			 menuentry_f get_repl,
			 str_replace tr_method){
  char buf[maxline];
  while(getline_nonempty(i,buf,sizeof(buf))){
    string match_s;
    string repl_s;
    
    istrstream is(buf,strlen(buf));
    getword(is,match_s);
    if(match_s=="endtranslate")
      break;
    getword(is,repl_s);
    if(debug)
      cerr<<"translgroup: match_s="<<match_s
	  <<", repl_s="<<repl_s<<endl;
    tr_point->insert(get_repl, tr_method, match_s, repl_s);
  }
}; 

			 
void read_translations(const char *dir){
  char path[maxline];
  char c;
  char buf[maxline];
  translations *tr_point;
  menuentry_f get_repl;
  str_replace tr_meth;
  
  string tmp;

  strcpy(path,dir);
  strcat(path,"/translate_menus");
  ifstream trans_file(path);
  if(verbose)
    cerr<<"Using translation file \""<<path<<"\""<<endl;
  while(getline_nonempty(trans_file,buf, sizeof(buf))){
    istrstream trans_str(buf,strlen(buf));
    string match_attr, repl_attr, t;
    
    getword(trans_str, t);
    getword(trans_str, match_attr," \t\v\f\n\r-");
    trans_str.get(c);
    if(c!='-'){
      cerr<<"missing - in translate file, should look like \"translate x->y\'"<<endl;
      exit(1);
    }
    trans_str.get(c);
    if(c!='>'){
      cerr<<"missing > in translate file, should look like \"translate x->y\'"<<endl;
      exit(1);
    }
    getword(trans_str, repl_attr," \t\v\f\n\r");
    if(debug)
      cerr<<"Transl: t="<<t<<", match_attr="<<match_attr
	  <<", repl_attr="<<repl_attr<<", trailing=\""<<tmp<<"\""<<endl;

         if(match_attr=="type")    tr_point=&translate_type;
    else if(match_attr=="section") tr_point=&translate_section;
    else if(match_attr=="id")      tr_point=&translate_id;
    else if(match_attr=="icon")    tr_point=&translate_icon;
    else if(match_attr=="title")   tr_point=&translate_title;
    else if(match_attr=="command") tr_point=&translate_command;
    else {
      cerr<<"Unknown match_attr ("<<match_attr<<"( in translate file"<<endl;
      exit(1);
    }
         if(repl_attr=="type")     get_repl=menuentry::get_type; 
    else if(repl_attr=="section")  get_repl=menuentry::get_section;
    else if(repl_attr=="id")       get_repl=menuentry::get_id;
    else if(repl_attr=="icon")     get_repl=menuentry::get_icon;
    else if(repl_attr=="title")    get_repl=menuentry::get_title;
    else if(repl_attr=="command")  get_repl=menuentry::get_command;
    else {
      cerr<<"Unknown replace_attr("<<repl_attr<<") in translate file"<<endl;
      exit(1);
    }
      
	 if(t=="translate")           tr_meth=translate_string;
    else if(t=="subtranslate")        tr_meth=subtranslate_string;
    else if(t=="substitute")          tr_meth=substitue_string;
    else {
      cerr<<"Unknown translate header ("<<t<<") in translate file"<<endl;
      cerr<<"Valid translate headers are: translate, substranslate, substitute"<<endl;
      exit(1);
    }

    read_translategroup(trans_file, tr_point, get_repl, tr_meth);
  }
}
/////////////////////////////
//
//   misc
//
/////////////////////////////



void read_installing_package(set <char *, strsort > &packages){
  //checks parent pid to see if it's a postinst script running
  //from dpkg, and if so, assumes that that package also is installed.
  //The trick is that the ppid will have a command line that looks like:
  // bash /var/lib/dpkg/info/menu.postinst configure

  int ppid=getppid();
  char buf[maxline]="";
  char post[maxline];
  char ch;
  char postinstdir[]="/var/lib/dpkg/info/";
  char *t;
  ostrstream s(buf, sizeof(buf));
  
  s<<"/proc/"<<ppid<<"/cmdline";
  ifstream f(buf);
  if(f){
    f.get(buf,sizeof(buf),'\0'); //this should be "bash"
    f.get(ch);                   //read away the \0
    f.get(buf,sizeof(buf),'.'); //this should be "/var/lib/dpkg/info/$package"
    f.get(ch);
    f.get(post, sizeof(buf),'\0');
    if(!shortcmp(postinstdir,buf)&&(!strcmp(post,"postinst"))){
      //OK, it is, this package is installed.
      t=buf+strlen(postinstdir);
      if(verbose)
	cout<<"This update-menus was apparently called from postinst of the "
	    <<t<<" package,"<<endl
	    <<"so assuming that package to be installed."<<endl;
      packages.insert(t);
    }
  }
}
void read_installed_packages(set <char *, strsort > &packages,
			     const char *statusfile){
  //reads STATUSFILE, stores all installed packagesin $packages.
  char buf[maxline];
  char tmp[maxline];
  char *s, *t;
  char ch;
  ifstream status(statusfile);

  while(getline_nonempty(status,buf,sizeof(buf))){
    if(!shortcmp(buf,PACKAGE_LINE)){
      //Package found, now first check it's status:
      while(status.get(tmp,sizeof(tmp),'\n')&&status.get(ch)){
	if(!strlen(tmp))
	  break;
	if(!shortcmp(tmp,STATUSLINE)){
	  if(!shortcmp(tmp,STATUS_OK)){
	    //yes, package installed, now store it.
	    s=buf+strlen(PACKAGE_LINE);
	    t=new char[strlen(s)+1];
	    strcpy(t,s);
	    packages.insert(t);
	  }else
	    break;
	}
      }
    }
  }
  read_installing_package(packages);
}

void read_installed_managers(set <char *, strsort > &managers,
			     const char *dir){
  DIR *d=opendir(dir);
  struct dirent *entry;
  struct stat st;
  char buf[maxline];
  char *s, *name;
  if(verbose)
    cerr<<"Using menu-methods :";
  while((entry=readdir(d))){
    name=entry->d_name;
    if(!strcmp(name, "README"))
      continue;
    for(s=name;*s;s++)
      if(!(isalnum(*s)||(*s=='_')||(*s=='-')))
	break;
    if(*s)
      continue;
    strcpy(buf,dir);
    strcat(buf,"/");
    strcat(buf,name);
    stat(buf,&st);
    //actually, should check if getuid() has execute permission...
    if((st.st_mode & (S_IXOTH|S_IXGRP|S_IXUSR))&&
       (S_ISREG(st.st_mode)||S_ISLNK(st.st_mode))){
      s=new char[strlen(name)+1];
      strcpy(s,name);
      managers.insert(s);
      if(verbose)
	cerr<<" "<<s;
    }
  }
  if(verbose)
    cerr<<endl;
}

void getmenu_ids(set <char *, strsort > &ids, const char *f){
  ifstream menufile(f);
  char s[maxline];
  char t[maxline];
  char *u;
  int i;
  while(getline_nonempty(menufile,s,sizeof(s))){
    istrstream str(s,strlen(s));
    for(i=0; i<3; i++)
      getword(str,t,sizeof(t));
    u=new char[strlen(t)+1];
    strcpy(u,t);
    ids.insert(u);
  }
}

void getmenu_files(map <char *, menuclass, strsort > &menulist, 
		   const char *dir){
  //reads the menufiles in directory $dir.
  DIR *d=opendir(dir);
  struct dirent *entry;
  struct stat st;
  char buf[maxline];
  char *s, *name;

  while((entry=readdir(d))){
    name=entry->d_name;
    if(strcmp(name, "README")&&
       shortcmp(name,".")){
      strcpy(buf,dir);
      strcat(buf,"/");
      strcat(buf,name);
      stat(buf,&st);
      if(S_ISREG(st.st_mode)||S_ISLNK(st.st_mode)){
	//Everything used to be much more complicated than
	//it's now. So, most that's done here isn't needed any more.
	char *n=new char[strlen(name)+1];
	menuclass m;
	strcpy(n,name);
	s=new char[strlen(dir)+strlen(n)+2];
	strcpy(s,buf);
	m.file=s;
	m.managers=NULL;
	m.mod_time=st.st_mtime;
	m.size=st.st_size;
	getmenu_ids(m.ids,s);
	menulist[n]=m;
      }
    }
  }
}
void ignore_brokenpipes(int){
}

void run_stdio_install(const char *manager,
		       string all,
		       const char  *dir){
  char path[maxline];
  FILE *p;
  void (*old_signal)(int);
  if(all.size()){
    strcpy(path, dir);
    strcat(path, "/");
    strcat(path,manager);
    strcat(path," -f " STDINOPTION);
    if(verbose)
      cerr<<"running: "<<path<<endl;
    if((p=popen(path, "w"))){
      signal(SIGPIPE,ignore_brokenpipes);
      write(fileno(p),&all[0],all.size());
      pclose(p);
      signal(SIGPIPE,old_signal);
    }
  }
}

void process_dir(set <char *, strsort > &packages,
		 set <char *, strsort > &already_ok,
		 string &all_menuentries,
		 const char *dir){

  map <char *, menuclass, strsort > filelist;
  //  map <char *, menuclass, strsort > updated;
    
  map <char *, menuclass, strsort >::iterator f;

  getmenu_files(filelist,dir);
  
  if(debug)
    cerr<<"Processing directory:"<<dir<<endl
	<<"File list: "<<endl
	<<filelist<<endl;
  
  for(f=filelist.begin();f!=filelist.end();f++){
    if(((packages.find((*f).first)!=packages.end())||
	!shortcmp((*f).first,LOCALNAME))&&
       (already_ok.find((*f).first)==already_ok.end())){
      //we need to install this menuentryfile:
      ifstream menufile((*f).second.file);
      while(menufile){
	menuentry m((*f).first,menufile);
	if(m.ok()){
	  do_translations(m);
	  m.write(all_menuentries);
	}
      };
    }
    if(debug)
      cerr<<"Want file "<<(*f).second.file<<" (package: "<<(*f).first
	  <<") to be <manually optimised out> (because: "
	  <<"Package installed:"<<(packages.find((*f).first)!=packages.end())
	  <<", Package local:"<<!shortcmp((*f).first,LOCALNAME)
	  <<", Package size="<<(*f).second.size
	  <<")"<<endl;
  }
  for(f=filelist.begin();f!=filelist.end();f++)
    already_ok.insert((*f).first);
}

void usage(){
  cerr<<"update-menus: check for new/removed menufiles, and run the"<<endl
      <<"    menu-managers in /etc/menu-methods on any changed files"<<endl
      <<"    Options:"<<endl
      <<"    -v  Verbose: show all arguments to the /etc/menu-methods programmes"<<endl
      <<"    -d  Debug (loads of unintelligible output)"<<endl
      <<"    --showtime show timings (why is it so slow?)"<<endl
      <<"    -h  This help"<<endl;
  exit(1);
}

void check_menumenthsdir(char *d){
  struct stat st;
  stat(d,&st);
  if(!S_ISDIR(st.st_mode))
    strcpy(d,MENUMETHODS);
  if(verbose)
    cerr<<"Using menu-methods dir: "<<d<<endl;
}

void check_arguments(char **argv){
  while(*argv){
    if(!strcmp(*argv,"-v"))
      verbose=true;
    if(!strcmp(*argv,"-d"))
      debug=verbose=true;
    if(!strcmp(*argv,"-h"))
      usage();
    if(!strcmp(*argv,"-f"))
      ;
    if(!strcmp(*argv,"--showtime"))
      show_time=true;
    if(!strcmp(*argv,STDINOPTION))
      ;
    argv++;
  }
}

int main (int /*argc*/, char **argv){
  set <char *, strsort > packages;
  set <char *, strsort > managers;
  set <char *, strsort > already_ok;
  string all_menuentries;
  char menumethoddir[maxline]="";
  char menusdir[maxline];

  check_arguments(argv);

  if(getuid()){
    strcpy(menusdir,getenv("HOME"));
    strcat(menusdir,"/");
    strcpy(menumethoddir,menusdir);
    strcat(menumethoddir,".menu-methods");
    strcat(menusdir,USERMENUS);
  }
  else
    strcpy(menusdir,MENUMETHODS);

  check_menumenthsdir(menumethoddir);
  read_translations(menumethoddir);

  show_passedtime(cerr,"Just before read_installed_packages");
  read_installed_packages(packages, STATUSFILE);
  show_passedtime(cerr,"Just before read_installed_managers");
  read_installed_managers(managers, menumethoddir);
  if(getuid()){
    show_passedtime(cerr,"Just before process_dir userdir" );
    process_dir(packages, already_ok, 
		all_menuentries, menusdir);
  }
  
  show_passedtime(cerr,"Just before process_dir " CONFIGMENUS);
  process_dir(packages, already_ok, 
	      all_menuentries, CONFIGMENUS);
  show_passedtime(cerr,"Just before process_dir " PACKAGEMENUS);
  process_dir(packages,  already_ok, 
	      all_menuentries, PACKAGEMENUS);
  show_passedtime(cerr,"Just before process_dir " MENUMENUS);
  process_dir(packages, already_ok, 
	      all_menuentries, MENUMENUS);
  for(set <char *, strsort >::iterator i=managers.begin();
      i!=managers.end();
      i++){
    show_passedtime(cerr,"Installing manager" );
    run_stdio_install(*i, all_menuentries, menumethoddir);
  }
  show_passedtime(cerr,"The end" );
  return 0;
}
