/* Copyright: GPL */
/*
  Witten by joost witteveen;  
  read_pkginfo function by Tom Lees, run_menumethods by both.
  
  */

#include "update-menus.h"
#include <fstream.h>
#include <set>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <ctype.h>
#include <dirent.h>
#include <signal.h>

int debug=0, verbose=0;
set <String, less<String> > installed_packages;
set <String, less<String> > menufiles_processed;
translateinfo *transinfo;

DIR *open_dir_check(String dirname){
  struct stat st;
  dirname+='\0';
  stat (&(dirname[0]), &st);
  if (!S_ISDIR (st.st_mode))
    throw dir_error_read(&(dirname[0]));

  return opendir (&(dirname[0]));
}


/////////////////////////////////////////////////////
//  menuentry:
//

bool menuentry::test_installed(String filename){
  return installed_packages.find(filename)!=installed_packages.end();
}
menuentry::menuentry(parsestream &i, const String &filename){
  char c;
  c=i.get_char();
  if(c=='?'){
    String name, val;
    //new format menuentry
    
    //read available info
    name=i.get_name();
    if(name!=COND_PACKAGE)
      throw unknown_cond_package();
    i.skip_char('(');
    val=i.get_name();
    if(!test_installed(val))
      throw cond_inst_false();
    i.skip_char(')');
    i.skip_space();
    i.skip_char(':');
    //read menuentry:
    try{
      do{
	name=i.get_name();
	val=i.get_eq_stringconst();
	data[name]=val;
	c=i.get_char();
	i.put_back(c);
      }while(c);
    }
    catch(endoffile){};
    i.skip_line();
  }else{
    //old format menuentry
    if((!test_installed(filename))
       &&(!filename.contains("local",0))){
      i.skip_line();
      throw cond_inst_false();
    }
    i.put_back(c);
    data[NEEDS_VAR]  =i.get_stringconst();
    data[SECTION_VAR]=i.get_stringconst();
    i.get_stringconst(); //id is unused
    data[ICON_VAR]   =i.get_stringconst();
    data[TITLE_VAR]  =i.get_stringconst();
    i.skip_space();
    data[COMMAND_VAR]=i.get_line();
  }
}

void menuentry::output(vector<String> &s){
  String t;
  map<String, String, less<String> >::iterator i;
  if((i=data.begin())!=data.end())
    while (true){
      t+=String((*i).first) + "=\"" + 
	 escape_string((*i).second,"\"\n") + "\"";
      i++;
      if(i==data.end()){
	break;
      } else
	t+=" ";
    }
  t+="\n";
  if(debug)
    cerr<<"ADDING:"<<t;
  s.push_back(t);
}
void menuentry::output_compat(vector<String> &s){
  String t,icon;
  static idcount=0;
  icon=data[ICON_VAR];
  if(icon=="")
    icon="none";
  t=data[NEEDS_VAR]  +" "
   +data[SECTION_VAR]+" id"
   +itoString(idcount++)+" "  
   +icon   +" \""
   +data[TITLE_VAR]  +"\" "
   +escape_string(data[COMMAND_VAR],"\n")+"\n";
  if(debug)
    cerr<<"ADDING:"<<t;

  s.push_back(t);
}
ostream &menuentry::debugoutput(ostream &o){
  map<String, String, less<String> >::iterator i;

  o<<"MENUENTRY:"<<endl;
  for(i=data.begin(); i!=data.end(); i++)
    o<<"  data["<<(*i).first<<"]="<<(*i).second<<endl;
  return o;
}

/////////////////////////////////////////////////////
//  translate stuff
//

trans_class::trans_class(const String &m,
			 const String &r,
			 const String &rv){
  match=m;
  replace=r;
  replace_var=rv;
}

bool  trans_class::check(String &s){
  if(debug)
    cerr<<"checking "<<match<<" < "<<s<<endl;
  return match.contains(s,0);
}

ostream &trans_class::debuginfo(ostream &o){
  o<<", match="<<match<<", replace="<<replace<<", replace_var="<<replace_var;
  return o;
}

void translate::process( menuentry &m,
			const String &v){
  if(v==match)
    m.data[replace_var]=replace;
}
void subtranslate::process( menuentry &m,
			 const String &v){
  if(v.contains(match,0)){
    m.data[replace_var]=replace;
  }
}

void substitute::process( menuentry &m,
		   const String &v){
  String s,*t;
  if(v.contains(match,0)){
    t=&(m.data[replace_var]);
    if(t->length()>=replace.length())
      *t=replace+t->after((int)match.length()-1);
  }
}

void translateinfo::init(parsestream &i){
  String name, match, replace, match_var, replace_var;
  Regex ident("[a-zA-Z_][a-zA-Z0-9_]*");

  if(verbose)
    cerr<<"reading translate info in "<<i.filename()<<endl;
  try{
    do{
      i.skip_space();
      name=i.get_name(ident);
      if(name=="")
	i.skip_line();
    } while (name=="");
    if(debug)
      cerr<<"name="<<name<<endl;
    i.skip_space();
    match_var=i.get_name(ident);
    if(debug)
      cerr<<"match_var="<<match_var<<endl;
    i.skip_space();
    i.skip_char('-');
    i.skip_char('>');
    i.skip_space();
    replace_var=i.get_name(ident);
    i.skip_line();
    while(true){
      pair<const String, trans_class *> p;

      i.skip_space();
      match=i.get_stringconst();
      if(match==ENDTRANSLATE_TRANS){
	i.skip_line();
	break;
      }
      if(match[0]=='#'){
	i.skip_line();
	continue;
      }
      i.skip_space();
      replace=i.get_stringconst();
      (String)p.first=match;
      if(name=TRANSLATE_TRANS)
	p.second=new translate(match,replace,replace_var);
      if(name=SUBTRANSLATE_TRANS)
	p.second=new subtranslate(match,replace,replace_var);
      if(name=SUBSTITUTE_TRANS)
	p.second=new substitute(match,replace,replace_var);
      trans[match_var].insert(p);
      i.skip_line();
    }
  }
  catch (endoffile){}
  catch (ident_expected){
    cerr<<"Identifier expected in translates file"<<endl;
    exit(1);
  }
  catch (char_expected c){
    cerr<<"\""<<c.c<<"\" expected in line "<<i.linenumber()
	<<" in file "<<i.filename()<<endl;
    exit(1);
  }
}

translateinfo::translateinfo(parsestream &i){
  init(i);
}
translateinfo::translateinfo(const String &filename){

  if(debug)
    cerr<<"attempting to open "<<filename<<".. ";
  parsestream ps(filename);

  if(!ps.good())
    throw ferror_read(filename);
  init(ps);
}

void translateinfo::process(menuentry &m){
  map<String, trans_map, less<String> >::iterator i;
  trans_map::iterator j;
  String *match;
  for(i=trans.begin(); i!=trans.end(); i++){
    match=&m.data[(*i).first];
    j=(*i).second.upper_bound(*match);
    if(debug)
      cerr<<"translate: var["<<*match<<"]"<<endl;
    do{
      j--;
      (*j).second->process(m,*match);
    } while((*j).second->check(*match)&&
	    (j!=(*i).second.begin()));
    
  }
}
void translateinfo::debuginfo(){
  map<String, trans_map, less<String> >::iterator i;
  trans_map::iterator j;
  for(i=trans.begin(); i!=trans.end(); i++){
    cerr<<"TRANS: ["<<(*i).first<<"]"<<endl;
    for(j=(*i).second.begin();
	j!=(*i).second.end();
	j++){
      cerr<<"key="<<(*j).first;
      (*j).second->debuginfo(cerr);
      cerr<<endl;
    }
  } 
}
/////////////////////////////////////////////////////
//  Installed Package Status:
//

void read_pkginfo (const char *statusfile)
  //this procedure written by Tom Lees, originally in pure C.
{
  FILE *status;
  char tmp[MAX_LINE], tmp2[MAX_LINE] = "", *s, *s2;
  int i;
  
  if(!(status = fopen (statusfile, "r")))
    throw ferror_read(statusfile);
  if (verbose)
    cerr<<"Reading installed packages...\n"<<endl;
  while (!feof (status)){
    fgets (tmp, MAX_LINE, status);
    if (strlen (tmp) == 0)
      continue;
    if (tmp[strlen (tmp) - 1] == '\n')
      tmp[strlen (tmp) - 1] = '\0';
    for (i = strlen (tmp); i >= 0 && tmp[i] == ' '; i--) ;
    tmp[i] = '\0';
    for (s = tmp; *s == ' '; s++) ;
    if (strlen (s) == 0)
      continue;
    
    if (!strncmp (s, "Package: ", 9))
	strcpy (tmp2, s + 9);
    else if (!strncmp (s, "Status: ", 8) && strlen (tmp2) != 0) {
      for (s2 = s + 8; *s2 == ' '; s2++) ;
      for (; *s2 != ' ' && *s2 != '\0'; s2++) ;
      for (; *s2 == ' '; s2++) ;
      if (strncmp (s2, "ok ", 3))
	continue;
      for (s2 += 3; *s2 == ' '; s2++) ;
      if (!strcmp (s2, "installed"))
	installed_packages.insert(tmp2);
    }
  }
  fclose (status);
}

void read_menufile(const char *filename,
		   const String &shortfilename,
		   vector<String> &menudata,
		   vector<String> &menudata_compat){
  ifstream f(filename);
  parsestream i(f);

  if(!f)
    throw ferror_read(filename);
  
  if(debug){
    cerr<<"Reading menuentryfile "<<filename<<endl;
  }
  try{
    while(1){
      try{
	menuentry m(i,shortfilename);
	if(transinfo)
	  transinfo->process(m);
	m.output(menudata);
	m.output_compat(menudata_compat);
      }
      catch(cond_inst_false){}
      catch (ident_expected){
	cerr<<"Identifier expected in line "<<i.linenumber()
	    <<" in file "<<filename<<endl;
	throw informed_fatal();
      }
      catch (char_expected c){
	cerr<<"\""<<c.c<<"\" expected in line "<<i.linenumber()
	    <<" in file "<<filename<<endl;
	throw informed_fatal();
      }
      catch (unknown_cond_package){
	cerr<<"unknown install condition (currently, only \"package\" is supported)"<<endl;
	throw informed_fatal();
      }
    }
  }
  catch(endoffile){};
}

void read_menufilesdir(const String &dirname,
		       vector<String> &menudata,
		       vector<String> &menudata_compat){
  
  struct stat st;
  DIR *dir;
  struct dirent *entry;
  String name;
  if(verbose)
    cerr<<"Reading menuentryfiles  in "<<dirname<<endl;
  dir=open_dir_check(dirname);
  while((entry=readdir(dir))){
    name=String(entry->d_name);
    if((name!="README")&&(name[0]!='.'))
      if(menufiles_processed.find(name)==menufiles_processed.end()){
	menufiles_processed.insert(name);
	name=dirname+name;
	stat(&(name[0]),&st);
	if(S_ISREG(st.st_mode)||S_ISLNK(st.st_mode))
	  read_menufile(&(name[0]),entry->d_name,
			menudata,menudata_compat);
      }
    
  }
}

void run_menumethod(char *methodname,
		    const vector<String> &menudata,
		    const vector<String> &menudata_compat){
  ifstream m(methodname);
  char s[MAX_LINE];
  const char *str;
  const vector<String> *md;
  FILE *p;
  unsigned int i;

  if(!m)
    throw ferror_read(methodname);

  m.get(s,sizeof(s));
  if(strncmp(s,MENU1_INSTALLER,strlen(MENU1_INSTALLER)))
    md=&menudata_compat;
  else
    md=&menudata;
  if(verbose){
    cerr<<"Udate-menus: Running method:"<<methodname<<endl;
  }
  strcat(methodname," -f --stdin ");
  if((p=popen(methodname, "w"))){
    signal(SIGPIPE,SIG_IGN);
    for(i=0;i!=md->size();i++){
      str=(*md)[i];
      write(fileno(p),str,strlen(str));
    }
    pclose(p);
    signal(SIGPIPE,SIG_DFL);
  }
}

void run_menumethoddir (const String &dirname,
			const vector<String> &menudata,
			const vector<String> &menudata_compat){
  struct stat st;
  DIR *dir;
  struct dirent *entry;
  char *s, tmp[MAX_LINE];

  if(verbose)
    cerr<<"running menu-methods in "<<dirname<<endl;
  dir=open_dir_check(dirname);
  while ((entry = readdir (dir)) != NULL){
    if (!strcmp (entry->d_name, "README"))
      continue;
    for (s = entry->d_name; *s != '\0'; s++)
      if (!(isalnum (*s) || (*s == '_') || (*s == '-')))
	break;
    if (*s != '\0')
      continue;
    
    sprintf (tmp, "%s/%s", &(dirname[0]), entry->d_name);
    stat (tmp, &st);
    
    // Do we have execute permissions? 
    if (((st.st_mode & S_IXOTH) || 
	 ((st.st_mode & S_IXGRP) && st.st_gid == getegid ()) || 
	 ((st.st_mode & S_IXUSR) && st.st_uid == geteuid ())) && 
	(S_ISREG(st.st_mode) || S_ISLNK(st.st_mode)))
      run_menumethod(tmp,menudata,menudata_compat);
  }
  closedir (dir);
}

int kill_other_updatemenus(){

  struct stat st_other;
  struct stat st_self;
  String s,t;
  
  if(!getuid()){
    {
      ifstream f(UPMEN_LOCKFILE);
      if(!f){
	if(verbose)
	  cerr<<"Update-menus: No other update-menus programme running. Good."<<endl;
	return 0;
      }
      parsestream p(f);
      t=p.get_stringconst();
    }
    s="/proc/"+t+"/exe";
    stat(&(s[0]),&st_other);
    s="/proc/"+itoString(getpid())+"/exe";
    stat(&(s[0]),&st_self);
    if((st_self.st_dev==st_other.st_dev)&&
       (st_self.st_ino==st_other.st_ino)){
      int other_pid=Stringtoi(t);
      if(verbose)
	cerr<<"Update-menus: another update-menus is running (pid="<<other_pid<<"), killing it."<<endl;
      kill(other_pid,SIGKILL);
      return 1;
    }else
      if(verbose)
	cout<<"Update-menus: A process with same pid as in "UPMEN_LOCKFILE" is running, but appears to have different executable image. Ignoring it."<<endl;
  }
  return 0;
}

void create_lock(){
  
  if(!getuid()){
    ofstream of(UPMEN_LOCKFILE);
    of<<getpid();
    if(!of.good()){
      cerr<<"Update-menu: cannot write to lockfile "UPMEN_LOCKFILE<<". Aborting."<<endl;
      exit(1);
    }
  }
}

void remove_lock(){
  if(!getuid()){
    if(unlink(UPMEN_LOCKFILE))
      cerr<<"Update-menus: Cannot remove lockfile "UPMEN_LOCKFILE<<endl;
  }
}

int check_dpkglock(){
  //return 1 if DPKG_LOCKFILE is locked
  int fd;
  struct flock fl;
  char buf[MAX_LINE];
  if(getuid()){
    if(verbose)
      cerr<<"update-menus run by user -- cannot determine if dpkg is locking"<<endl
	  <<DPKG_LOCKFILE<<": assuming there is no lock"<<endl;
    return 0;
  }
  fd=open(DPKG_LOCKFILE, O_RDWR|O_CREAT|O_TRUNC, 0660);
  if(fd==-1)
    return 1;
  fl.l_type= F_WRLCK;
  fl.l_whence= SEEK_SET;
  fl.l_start= 0;
  fl.l_len= 1;
  if (fcntl(fd,F_SETLK,&fl) == -1) {
    close(fd);
    if (errno == EWOULDBLOCK || errno == EAGAIN || errno == EACCES)
      return 1;
    cerr<<"update-menus: Thank you for installing this test version of the menu package."<<endl
	<<"update-menus: Unfortunately, I encountered an unknown errno (="
	<<errno<<")."<<endl
	<<"update-menus: Could you please be so kind as to email joostje@debian.org"<<endl
	<<"update-menus: the errno (="<<errno<<") with a discription of what you did to"<<endl
	<<"update-menus: trigger this. Thanks very much."<<endl
	<<"Press enter"<<endl;
    cin.get(buf,sizeof(buf));
    return 1;
  }
  fl.l_type= F_UNLCK;
  fl.l_whence= SEEK_SET;
  fl.l_start= 0;
  fl.l_len= 1;
  if (fcntl(fd,F_SETLK,&fl) == -1){
    cerr<<"update-menus: ?? Just locked the dpkg status database to see if another dpkg"<<endl
	<<"update-menus: Was running. Now I cannot unlock it! Aborting"<<endl;
    exit(1);
  }
  close(fd);
  return 0;
}

void wait_dpkg(){
  
  int child;
  int other;

  other=kill_other_updatemenus();

  if(check_dpkglock()){
    if((child=fork())){
      exit(0);
    }else{
      create_lock();
      if((!other)||verbose){
        cerr<<"Update-menus: waiting for dpkg to finish (forking to background)"<<endl
            <<"Update-menus: (checking "DPKG_LOCKFILE")"<<endl;
      }
      while(check_dpkglock())
	sleep(2);
      if(debug)
	cerr<<"Update-menus: Dpkg lock on lockfile disapeared, generate menufiles"<<endl;
    }
  } else{
    create_lock();
    if(verbose)
      cerr<<"Update-menus: Dpkg not locking dpkg status area. Good."<<endl;
  }
}

void parse_params(char **argv){
  while(*(++argv)){
    if(String("-d")==*argv)
      debug=verbose=1;
    if(String("-v")==*argv)
      verbose=1;
    if(String("-h")==*argv){
      cerr<<"update-menus: update the various window-manager config files (and"<<endl
	  <<"  dwww, and pdmenu) after menuentry files were changed."<<endl
	  <<"Usage: update-menus [options] "<<endl
	  <<"    -v  be verbose about what is going on"<<endl
	  <<"    -d  debugging (loads of unintelligible output)"<<endl;
	exit(1);
    }
  }
}
main (int, char **argv){
  vector<String> menudata, menudata_compat;
  String dummy;
  
  debug=0;
  parse_params(argv);
  wait_dpkg();
  read_pkginfo(DPKG_STATUSFILE);
  transinfo=NULL;
  try{
    if(getuid()){
      try{
	transinfo=new translateinfo(String(getenv("HOME"))+"/"+USERTRANSLATE);
      } catch(ferror_read d){dummy=d.name;}
    };
    if(!transinfo){
      try{
	transinfo=new translateinfo(TRANSLATE_FILE);
      }catch (ferror_read d){dummy=d.name;};
    }
    if(debug)
      transinfo->debuginfo();
    if(getuid()){
      try{
	read_menufilesdir(String(getenv("HOME"))+"/"+USERMENUS,
			  menudata, menudata_compat);
      }
      catch(dir_error_read d){dummy=d.name;};
    }
    
    read_menufilesdir(CONFIGMENUS, menudata, menudata_compat);
    read_menufilesdir(PACKAGEMENUS, menudata, menudata_compat);
    read_menufilesdir(MENUMENUS, menudata, menudata_compat);
    //menudata+='\0';
    //menudata_compat+='\0';

    if(getuid()){
      try{
	run_menumethoddir(String(getenv("HOME"))+"/"+USERMETHODS,
			  menudata,menudata_compat);
      }
      catch(dir_error_read d){
	dummy=d.name;
	run_menumethoddir(MENUMETHODS, menudata, menudata_compat);	
      }
    } else
      run_menumethoddir(MENUMETHODS, menudata, menudata_compat);
  }
  catch (ferror_read f){
    cerr<<"Cannot open file "<<f.name<<" for reading"<<endl;
  }
  catch (dir_error_read d){
    cerr<<"Cannot open directory "<<d.name<<" for reading"<<endl;
  }
  catch (informed_fatal){
    cerr<<"Aborting."<<endl;
  }

  remove_lock();
}

