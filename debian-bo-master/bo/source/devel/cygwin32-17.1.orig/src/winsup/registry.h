/* registry data structures for winsup.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

class reg_key
{
  HKEY key_;

public:

  int fillone_string (char *dst, const char *name, const char *def, int max);
  int setone_string (const char *src, const char *name);
  int setone_int (const char *key, int val);
  int getone_string (char *dst, const char *name, int len);

  void init (const char *name, HKEY r);
  int open(const char *name, HKEY r, REGSAM access);

  void close();

  reg_key (reg_key &parent, const char *name);
  reg_key ();
  HKEY get_key ();

  int get_int (const char *,int def);
  int get_string (const char *, char *buf, size_t len, const char *def);
  int set_string (const char *,const char *);
  int set_int (const char *,int val);
  void kill (const char *child);

  ~reg_key ();
};

class reg_session
{
  reg_key majorkey_;
  reg_key minorkey_;
  reg_key versionkey_;
  reg_key vminorkey_;

public:
  reg_session ();
  ~reg_session ();
  reg_key &get_key ();
};
