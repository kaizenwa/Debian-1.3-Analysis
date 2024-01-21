#ifndef UPDATE_MENUS_H
#define UPDATE_MENUS_H

class menuclass{
public:
  char         *file;
  time_t        mod_time;
  off_t         size;
  set<char *, strsort >::iterator status;
  set<char *, strsort > *managers;
  set<char *, strsort > ids;
};

class menuentry;
typedef string &(menuentry::*menuentry_f)(void);
typedef bool (*str_replace)(string &menu_match, string &menu_repl, 
		      const string &tran_match, const string &tran_repl);

class menuentry{
private:
  bool menu_ok;
public:
  string package;
  string type;
  string section;
  string id;
  string icon;
  string title;
  string command;
  
  menuentry(char *package, istream &i);
  void write(string  &v);
  string &get_type   (){ return type;};
  string &get_section(){ return section;};
  string &get_id     (){ return id;};
  string &get_icon   (){ return icon;};
  string &get_title  (){ return title;};
  string &get_command(){ return command;};
  bool ok(){return menu_ok;};
};


class translate{
private:
  menuentry_f replace_attr;
  str_replace replace_method;

  string match_string;
  string replace_string;

  void match(string &s);
public:
  translate(str_replace repl_meth, menuentry_f repl_f,
	    string      match_s,  string      repl_s){
    match_string=match_s;
    replace_method=repl_meth;
    replace_attr=repl_f;
    replace_string=repl_s;
  };
  translate(string dummy){
    match_string=dummy;
  }
  bool replace(menuentry_f match_attr,  menuentry &m)const;
  friend bool operator <(const translate &i, const translate &j);
};

bool operator <(const translate &i, const translate &j){
  return i.match_string < j.match_string;
}

class translations{
private:
  set <translate, less<translate> > s;
  menuentry_f match_attr;
public:
  translations(menuentry_f attr){
    match_attr=attr;
  }
  void process(string &match_s, menuentry &m){
    set <translate, less<translate> >::iterator i;
    if(s.size()){
      translate dummy(match_s);
      if(((i=s.lower_bound(dummy))==s.end())||
	 ((dummy<(*i))&&(i!=s.begin())))
	i--;
      while((*i).replace(match_attr,  m)&&
	    (i!=s.begin()))
	i--;
    }
  };
  void insert(menuentry_f get_repl,
	 str_replace tr_method,
	 string &match_s,
	 string &repl_s){
    translate t(tr_method, get_repl, match_s, repl_s);
    s.insert(t);
  };
};

extern void translate_string(string &match, string &replace);
extern void subtranslate_string(string &match, string &replace);
extern void substitue_string(string &match, string &replace);

extern ostream &operator<<(ostream &o, menuentry &m);

#endif  /* UPDATE_MENUS_H */
