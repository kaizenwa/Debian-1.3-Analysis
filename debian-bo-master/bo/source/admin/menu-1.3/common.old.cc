#include "common.old.h"
#include <ctype.h>
#include <sys/timeb.h>

char *getline_nonempty(istream &i, char *s, int l){
  char *t=0;
  char ch;
  while((i.get(s,l,'\n'))&&(!i.eof())){
    i.get(ch);
    if((t=strchr(s,'#')))
      if((t==s)||(isspace(*(t-1))))
	*t=0;
    skip_spaces(s);
    if(strlen(s)){
      t=s;
      break;
    }
    t=0;
  }
  return t;
}

string &getline_nonempty(istream &i, string &s){
  char buf[maxline];
  getline_nonempty(i,buf,sizeof(buf));
  s=buf;
  return s;
}
/*bool getline(istream &i, string &s){
  char buf[maxline];
  bool ok;
  ok=getline(i,buf,sizeof(buf));
  s=buf;
  return ok;
}
*/
char *getword(istream &i, char *s, int l, char *sep){
  char *old=s;
  bool quoted=false;
  char ch='\0',oldch='\0';
  while(i.get(ch)&&strchr(sep,ch));
  if(ch)
    i.putback(ch);
  for(;((s-old)<l)&&i.get(*s)&&(*s); s++){
    if((*s=='\"')&&(oldch!='\\')){
      quoted=!quoted;
      s--;
      continue;
    }
    if((ch=='\\')&&(oldch!='\\'))
      s--;
      
    if((!quoted)&&strchr(sep,*s)){
      i.putback(*s);
      *s=0;
      break;
    }
    oldch=*s;
  } 
  *s=0;
  if(s==old)
    return 0;
  else
    return s;
}

bool getword(istream &i, string &s, char *sep){
  int j;
  bool quoted=false;
  char ch, oldch='\0';
  while(i.get(ch)&&strchr(sep,ch));
  i.putback(ch);
  j=0;
  while(i.get(ch)&&ch){
    if((ch=='\"')&&(oldch!='\\')){
      quoted=!quoted;
      continue;
    }
    if((!quoted)&&strchr(sep,ch)){
      i.putback(ch);
      break;
    }
    if((ch!='\\')||(oldch=='\\'))
      s.append(1,ch);
    oldch=ch;
    j++;
  }    
  return !!j;
}

string &space_strip(string &s){
  //removes double spaces ("  ") from the input string
  string t;
  char c,o='\0';
  for(unsigned int i=0;i<s.size();i++){
    c=s[i];
    if(!(isspace(c)&&isspace(o)))
      t.append(1,c);
    o=c;
  }
  s=t;
  return s;
}

char * space_strip(char *s){
  //removes double spaces ("  ") from the input string
  char *old=s;
  char *t;
  for(t=s;(*t=*s);s++)
    t+=!(isspace(*s)&&isspace(*(s+1)));
  return old;
}

string getlastword(string &s, char separate){
  char buf[maxline];
  char *t,*u;
  strncpy(buf,s.data(),s.size());
  buf[s.size()]=0;
  t=buf; u=buf;
  while((t=strchr(t,separate))){
    t++;
    u=t;
  }
  return string(u);
};

char *string2char(char *t, string &s){
  strncpy(t,s.data(),s.size());
  t[s.size()]=0;
  return t;
}
char *skip_spaces(char *s){
  char *old=s;
  char *t=s;
  while(isspace(*t))
    t++;
  while((*s++=*t++));
  return old;
}
void skip_spaces(istream &i){
  char ch='\0';
  while(i.get(ch)&&isspace(ch));
  if(ch)
    i.putback(ch);
}
string escape_quotes(string &s){
  // the string: emacs "my long filename"
  // becomes   : emacs \"my long filename\"
  string e;
  unsigned int i;
  for(i=0;i<s.size();i++){
    if(s[i]=='"')
      e.append(1,'\\');
    e.append(1,s[i]);
  }
  return e;
}
string escape_singlequotes(string &s){
  // the string: emacs "my long filename"
  // becomes   : emacs \"my long filename\"
  string e;
  unsigned int i;
  for(i=0;i<s.size();i++){
    if(s[i]=='\'')
      e.append(1,'\\');
    e.append(1,s[i]);
  }
  return e;
}
string escape_bothquotes(string &s){
  // the string: emacs "my long filename"
  // becomes   : emacs \"my long filename\"
  string e;
  unsigned int i;
  for(i=0;i<s.size();i++){
    if((s[i]=='\'')||(s[i]=='"'))
      e.append(1,'\\');
    e.append(1,s[i]);
  }
  return e;
}

long double double_time(){
  struct timeb tm;
  ftime(&tm);
  return tm.time+1e-3*tm.millitm;
}
void show_passedtime(ostream &o, char *s){
  if(show_time){
    static long double init_time=0;
    static long double lasttime;
    long double t=double_time();
    if(init_time==0)
      lasttime=init_time=t;
    o<<s<<". Relative time: "<<t-init_time
     <<", since last call: "<<t-lasttime<<endl;
    lasttime=t;
  }
}
/*
int main(){
  string s("hoi, dag,  maar nu,   lasdkjf     jalkfds  ");
  cout<<"\""
      <<space_strip(s)<<"\""<<endl;
}
*/

