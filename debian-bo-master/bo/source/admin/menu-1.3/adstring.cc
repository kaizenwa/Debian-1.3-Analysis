#include <ctype.h>
#include "adstring.h"
#include <stdlib.h>
#include <strstream.h>
#include <iomanip.h>
#include <list.h>

char parsestream::get_next(){
  char c;
  if(buffer.length()){
    c=buffer[buffer.length()-1];
    buffer=buffer.at(0,buffer.length()-1);
  }else if(!(i->get(c))){
    if(i->eof())
      throw endoffile();
    return 0;
  }
  return c;
}

char parsestream::get_char(){
  char c=get_next();
  switch (c){
  case '\\':
    c=get_next();
    if(c=='\n'){
      c=' '; //escaped newline is space
      lineno++;
    }
    else {
      put_back(c);
      c='\\';
    }
    return c;
  case '\n':
    while((c=get_next())&& (c=='\n'));
    if(!i->eof())
      put_back(c);
    put_back('\n');
    return 0;
  default:
    return c;
  }
}

char parsestream::put_back(char c){
  if (c)
    buffer+=c;
  return c;
}

String parsestream::get_line(){
  String s;
  char c;
  try{
    while((c=get_char()))
      s+=c;
    skip_line();
  }
  catch(endoffile){};
  return s;
}

String parsestream::get_name(){
  char c;
  String s;
  skip_space();
  //c=get_char();
  //if(!isalpha(c))
  //  throw ident_expected();
  //s=c;
  try{
    while((c=get_char())&&
	  (isalnum(c)||(c=='_')||(c=='-')))
      s+=c;
    if(c)
      put_back(c);
  } catch(endoffile){};
  return s;
}

String parsestream::get_name(const Regex &r){
  char c;
  String s;
  skip_space();
  //c=get_char();
  //if(!isalpha(c))
  //  throw ident_expected();
  //s=c;
  try{
    while((c=get_char())&&(String(c).contains(r)))
      s+=c;
    if(c)
      put_back(c);
  } catch(endoffile){};
  return s;
}


String parsestream::get_stringconst(){
  char c;
  String s;
  skip_space();
  c=get_char();
  try{
    if(c=='\"'){
      while((c=get_char())&&(c!='\"')){
	if(c=='\\'){
	  c=get_char();
	  switch(c){
	  case 't': c='\t'; break;
	  case 'b': c='\b'; break;
	  case 'n': c='\n'; break;
	  default:; break;
	  }
	}
	s+=c;
      }
      if(c!='\"')
	throw char_expected('\"');
    } else{ //no " at begining
      s=c;
      while((c=get_char())&&!isspace(c))
	s+=c;
    }
  }catch(endoffile){};  
  return s;
}

String parsestream::get_eq_name(){
  char c;
  skip_space();
  c=get_char();
  if(c!='=')
    throw char_expected('=');
  return get_name();
}
String parsestream::get_eq_stringconst(){
  char c;
  skip_space();
  c=get_char();
  if(c!='=')
    throw char_expected('=');    
  return get_stringconst();
}
void parsestream::skip_line(){
  char c;
  try{
    while((c=get_char()));
    if(!c)
      get_next();
    lineno++;
  }catch(endoffile){};
}
void parsestream::skip_space(){
  char c;
  while(isspace(c=get_char()));  
  if(c)
    put_back(c);
}
void parsestream::skip_char(char expect){
  char c=get_char();
  if(c!=expect){
    put_back(c);
    throw char_expected(expect);
  }
}




String escape_doublequotes(const String &s){
  String t;
  unsigned int i;
  for(i=0;i!=s.length();i++){
    if(s[i]=='\"')
      t+='\\';
    t+=s[i];
  }
  return t;
}

String escapewith_string(const String &s, const String &esc,
			 const String &with){
  // call with: escape_string("hello $world, %dir", "$%", "\\")
  // returns:   "hello \$world, \%dir"
  String t;
  unsigned int i;
  for(i=0;i!=s.length();i++){
    if(esc.contains(s[i]))
      t+=with;
    t+=s[i];
  }
  return t;
}

String escape_string(const String &s, const String &esc){
  // call with: escape_string("hello $world, %dir", "$%")
  // returns:   "hello \$world, \%dir"
  return escapewith_string(s,esc,"\\");
}
String cppesc_string(const String &s){
  String t;
  unsigned int i;
  for(i=0;i!=s.length();i++){
    if(!(isalnum(s[i])||(s[i]=='_')))
      t+=String('$')+itohexString(int(s[i]));
    else
      t+=s[i];
  }
  return t;
}
String replacewith_string(const String &s, const String &replace,
			  const String &with){
  // call with: replacewith_string("hello $world, %dir", "$% ", "123")
  // returns:   "hello31world,32dir"
  String t;
  unsigned int i;
  int j;
  for(i=0;i!=s.length();i++){
    if((j=replace.index(s[i]))>=0)
      t+=with[j % with.length()];
    else
      t+=s[i];
  }
  return t;
}

String replace(String s,char match, char replace){
  unsigned int i;
  for(i=0;i!=s.length();i++)
    if(s[i]==match)
      s[i]=replace;
  return s;
}
String sort_hotkey(String s){
  String t;
  unsigned i;
  
  t=s[0];
  s[0]='\0';
  for(i=1;i!=s.length();i++)
    if((isspace(s[i-1])||(ispunct(s[i-1]))==' ')&&isupper(s[i])){
      t+=s[i];
      s[i]='\0';
    }
  for(i=1;i!=s.length();i++)
    if((isspace(s[i-1])||(ispunct(s[i-1]))==' ')&&isalnum(s[i])){
      t+=s[i];
      s[i]='\0';
    }
  for(i=1;i!=s.length();i++)
    if(isupper(s[i])){
      t+=s[i];
      s[i]='\0';
    }
  for(i=1;i!=s.length();i++)
    if(isalpha(s[i])){
      t+=s[i];
      s[i]='\0';
    }
  for(i=1;i!=s.length();i++)
    if(isalnum(s[i])){
      t+=s[i];
      s[i]='\0';
    }
  for(i=1;i!=s.length();i++)
    if(s[i]){
      t+=s[i];
      s[i]='\0';
    }
  return t;
}

int Stringtoi(const String &s){
  return atoi(s);
}

String itoString(int i){
  char s[MAX_LINE];
  ostrstream str(s,sizeof(s));
  str<<i<<ends;
  return s;
}

String itohexString(int i){
  char s[MAX_LINE];
  ostrstream str(s,sizeof(s));
  str<<setbase(16)<<i<<ends;
  return s;
}


String String_parent(String s){
  int  i,p;
  
  for(i=0,p=-1;(unsigned int)i!=s.length();i++)
    if(s[i]=='/')
      p=i;
  if(p<0)
    return "";
  else
    return s.at(0,p);
}
