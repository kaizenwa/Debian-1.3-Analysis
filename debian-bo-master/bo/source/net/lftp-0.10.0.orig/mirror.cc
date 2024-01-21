/*
 * lftp and utils
 *
 * Copyright (c) 1996-1997 by Alexander V. Lukyanov (lav@yars.free.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <ctype.h>
#include <dirent.h>
#include <utime.h>
#include <unistd.h>
#include "ftpclass.h"
#include "mirror.h"
#include "ProcWait.h"

void  FileInfo::Merge(const FileInfo& f)
{
   if(strcmp(name,f.name))
      return;
// int sim=defined&f.defined;
   int dif=(~defined)&f.defined;
   if(dif&MODE)
      SetMode(f.mode);
   if(dif&DATE)
      SetDate(f.date);
   if(dif&DATE_UNPREC && !(defined&DATE))
      SetDateUnprec(f.date);
   if(dif&TYPE)
      SetType(f.filetype);
   if(dif&SYMLINK)
      SetSymlink(f.symlink);
}

mode_t	 parse_perms(const char *s)
{
   mode_t   p=0;

   if(strlen(s)!=9)
      bad: return (mode_t)-1;

   switch(s[0])
   {
   case('r'): p|=S_IRUSR; break;
   case('-'): break;
   default: goto bad;
   }
   switch(s[1])
   {
   case('w'): p|=S_IWUSR; break;
   case('-'): break;
   default: goto bad;
   }
   switch(s[2])
   {
   case('S'): p|=S_ISUID; break;
   case('s'): p|=S_ISUID;
   case('x'): p|=S_IXUSR; break;
   case('-'): break;
   default: goto bad;
   }
   s+=3;
   switch(s[0])
   {
   case('r'): p|=S_IRGRP; break;
   case('-'): break;
   default: goto bad;
   }
   switch(s[1])
   {
   case('w'): p|=S_IWGRP; break;
   case('-'): break;
   default: goto bad;
   }
   switch(s[2])
   {
   case('S'): p|=S_ISGID; break;
   case('s'): p|=S_ISGID;
   case('x'): p|=S_IXGRP; break;
   case('-'): break;
   default: goto bad;
   }
   s+=3;
   switch(s[0])
   {
   case('r'): p|=S_IROTH; break;
   case('-'): break;
   default: goto bad;
   }
   switch(s[1])
   {
   case('w'): p|=S_IWOTH; break;
   case('-'): break;
   default: goto bad;
   }
   switch(s[2])
   {
   case('T'): case('t'): p|=S_ISVTX; break;
   case('l'): p|=S_ISGID; p&=~S_IXGRP; break;
   case('x'): p|=S_IXOTH; break;
   case('-'): break;
   default: goto bad;
   }

   return p;
}

int   parse_month(char *m)
{
   static const char *months[13]={
      "Jan","Feb","Mar","Apr","May","Jun",
      "Jul","Aug","Sep","Oct","Nov","Dec",0
   };
   for(int i=0; months[i]; i++)
      if(!strcasecmp(months[i],m))
	 return(i%12);
   return -1;
}

FileSet *parse_listing(char **lines)
{
#define FIRST_TOKEN strtok(line," \t")
#define NEXT_TOKEN  strtok(NULL," \t")
   char	 *line;
   char	 base_dir_len=-1;
   char	 *curr_dir=xstrdup("");
   int 	 len;

   FileSet *set=new FileSet;

   if(lines==0)
      return set;

   while((line=*lines++)!=0)
   {
      if(sscanf(line,"total %d",&len)==1)
	 continue;
      len=strlen(line);
      if(len==0)
	 continue;

      /* dir1/dir2/dir3: */
      if(line[len-1]==':' && strchr(line,' ')==0 && strchr(line,'\t')==0)
      {
	 // we got directory name
	 line[--len]=0;
	 if(base_dir_len>=0)
	 {
	    xfree(curr_dir);
	    if(len<=base_dir_len)
	       curr_dir=xstrdup("");   // unlikely case
	    else
	       curr_dir=xstrdup(line+base_dir_len);
	 }
   	 else
	 {
	    char *b=strrchr(line,'/');
	    if(b)
	       base_dir_len=b-line+1;
	    else
	       base_dir_len=0;
	 }
	 continue;
      }

      /* parse perms */
      char *t = FIRST_TOKEN;
      if(t==0)
	 continue;
      FileInfo fi;
      switch(t[0])
      {
      case('l'):  // symlink
	 fi.SetType(fi.SYMLINK);
      	 break;
      case('d'):  // directory
	 fi.SetType(fi.DIRECTORY);
      	 break;
      case('-'):  // plain file
	 fi.SetType(fi.NORMAL);
      	 break;
      default:
	 continue;   // unknown
      }
      mode_t mode=parse_perms(t+1);
      if(mode!=(mode_t)-1)
	 fi.SetMode(mode);

      // link count
      t = NEXT_TOKEN;
      if(!t)
	 continue;

      // user
      t = NEXT_TOKEN;
      if(!t)
	 continue;

      // group or size
      char *group_or_size = NEXT_TOKEN;

      // size or month
      t = NEXT_TOKEN;
      if(!t)
	 continue;
      if(isdigit(*t))
      {
	 // size
      	 fi.SetSize(atol(t));
	 t = NEXT_TOKEN;
	 if(!t)
	    continue;
      }
      else
      {
	 // it was month
	 fi.SetSize(atol(group_or_size));
      }

      struct tm date;
      date.tm_mon=parse_month(t);
      if(date.tm_mon==-1)
	 date.tm_mon=0;

      // day of month
      t = NEXT_TOKEN;
      if(!t)
	 continue;
      date.tm_mday=atoi(t);

      // time or year
      t = NEXT_TOKEN;
      if(!t)
	 continue;
      date.tm_hour=date.tm_min=0;
      if(strlen(t)==5)
      {
	 sscanf(t,"%2d:%2d",&date.tm_hour,&date.tm_min);
	 time_t curr=time(0);
      	 date.tm_year=localtime(&curr)->tm_year;
      }
      else
	 date.tm_year=atoi(t)-1900;

      date.tm_isdst=0;
      date.tm_sec=0;

      fi.SetDateUnprec(mktime(&date));

      char *name=strtok(NULL,"");
      if(!name)
	 continue;
      if(fi.filetype==fi.SYMLINK)
      {
	 char *arrow=strstr(name," -> ");
	 if(arrow)
	 {
	    *arrow=0;
	    fi.SetSymlink(arrow+4);
	 }
      }
      if(curr_dir[0])
      {
	 char *fullname=(char*)alloca(strlen(curr_dir)+1+strlen(name)+1);
	 sprintf(fullname,"%s/%s",curr_dir,name);
	 fi.SetName(fullname);
      }
      else
	 fi.SetName(name);

      set->Add(new FileInfo(fi));
   }
   return set;
}

FileSet *local_files(char *dir)
{
   FileSet *set=new FileSet;
   DIR *d=opendir(dir);
   struct dirent *f;

   if(d==0)
   {
      perror(dir);
      return set;
   }
   for(;;)
   {
      f=readdir(d);
      if(f==0)
	 break;
      char *name=dir_file(dir,f->d_name);
      struct stat st;
      int res=lstat(name,&st);
      if(res==-1)
      {
	 perror(name);
	 continue;
      }
      FileInfo::type t;
      if(S_ISDIR(st.st_mode))
	 t=FileInfo::DIRECTORY;
      else if(S_ISREG(st.st_mode))
	 t=FileInfo::NORMAL;
      else if(S_ISLNK(st.st_mode))
	 t=FileInfo::SYMLINK;
      else
	 continue;   // ignore other type files

      FileInfo *fi=new FileInfo;
      fi->SetName(f->d_name);
      fi->SetSize(st.st_size);
      fi->SetDate(st.st_mtime);
      fi->SetMode(st.st_mode&07777);
      fi->SetType(t);

#ifdef HAVE_LSTAT
      if(t==fi->SYMLINK)
      {
	 char *buf=(char*)alloca(st.st_size+1);
	 res=readlink(name,buf,st.st_size);
	 if(res==-1)
	 {
	    delete fi;
	    continue;
	 }
	 buf[res]=0;
	 fi->SetSymlink(buf);
      }
#endif /* HAVE_LSTAT */

      set->Add(fi);
   }
   closedir(d);
   return set;
}

void FileSet::Add(FileInfo *fi)
{
   if(!(fi->defined & fi->NAME))
   {
      delete fi;
      return;
   }
   files=(FileInfo**)xrealloc(files,(++fnum)*sizeof(*files));
   files[fnum-1]=fi;
}

void FileSet::Merge(const FileSet *set)
{
   int i,j;
   for(i=0; i<set->fnum; i++)
   {
      for(j=0; j<fnum; j++)
      {
      	 if(!strcmp(files[j]->name,set->files[i]->name))
	 {
	    files[j]->Merge(*(set->files[i]));
	    break;
	 }
      }
      if(j==fnum)
      {
	 Add(set->files[i]);
      }
   }
}

void FileSet::Merge(char **list)
{
   if(list==0)
      return;

   int j;
   for( ; *list; list++)
   {
      for(j=0; j<fnum; j++)
      {
      	 if(!strcmp(files[j]->name,*list))
	    break;
      }
      if(j==fnum)
      {
	 FileInfo *fi=new FileInfo();
	 fi->SetName(*list);
	 Add(fi);
      }
   }
}

FileSet::FileSet(FileSet const *set)
{
   ind=set->ind;
   fnum=set->fnum;
   if(fnum==0)
      files=0;
   else
      files=(FileInfo**)xmalloc(fnum*sizeof(*files));
   for(int i=0; i<fnum; i++)
      files[i]=new FileInfo(*(set->files[i]));
}

FileSet::~FileSet()
{
   for(int i=0; i<fnum; i++)
      delete files[i];
   xfree(files);
}

void FileSet::SubtractSame(const FileSet *set)
{
   for(int i=0; i<fnum; i++)
   {
      for(int j=0; j<set->fnum; j++)
      {
	 if(files[i]->SameAs(set->files[j]))
	 {
	    Sub(i);
	    i--;
	    break;
	 }
      }
   }
}

void FileSet::SubtractAny(const FileSet *set)
{
   for(int i=0; i<fnum; i++)
   {
      for(int j=0; j<set->fnum; j++)
      {
	 if(!strcmp(files[i]->name,set->files[j]->name))
	 {
	    Sub(i);
	    i--;
	    break;
	 }
      }
   }
}

bool  FileInfo::SameAs(const FileInfo *fi)
{
   if(defined&NAME && fi->defined&NAME)
      if(strcmp(name,fi->name))
	 return false;
   if(defined&SIZE && fi->defined&SIZE)
      if(size!=fi->size)
	 return false;
   if(defined&TYPE && fi->defined&TYPE)
      if(filetype!=fi->filetype)
	 return false;

   if((defined&TYPE && filetype==DIRECTORY)
   || (fi->defined&TYPE && fi->filetype==DIRECTORY))
      return false;  // can't guarantee directory is the same (recursively)

   if(defined&DATE && fi->defined&DATE)
      if(date!=fi->date)
	 return false;
   if(defined&SYMLINK_DEF && fi->defined&SYMLINK_DEF)
      if(strcmp(symlink,fi->symlink))
	 return false;
   return true;
}

void  truncate_file_tree(const char *dir)
{
   fflush(stderr);
   pid_t pid;
   switch(pid=fork())
   {
   case(0): // child
      execlp("rm","rm","-rf",dir,NULL);
      perror("execlp(rm)");
      fflush(stderr);
      _exit(1);
   case(-1):   // error
      perror("fork()");
      return;
   default: // parent
      (new ProcWait(pid))->Auto();  // don't wait for termination
   }
}

void FileSet::LocalRemove(const char *dir)
{
   FileInfo *file;
   for(int i=0; i<fnum; i++)
   {
      file=files[i];
      if(file->defined & (file->DATE|file->DATE_UNPREC))
      {
	 char *local_name=dir_file(dir,file->name);

	 if(!(file->defined & file->TYPE)
	 || file->filetype==file->DIRECTORY)
	 {
	    int res=rmdir(local_name);
	    if(res==0)
	       continue;
	    res=remove(local_name);
	    if(res==0)
	       continue;
	    truncate_file_tree(local_name);
	    continue;
	 }
	 remove(local_name);
      }
   }
}

void FileSet::LocalUtime(const char *dir)
{
   FileInfo *file;
   for(int i=0; i<fnum; i++)
   {
      file=files[i];
      if(file->defined & (file->DATE|file->DATE_UNPREC))
      {
	 if(file->defined & file->TYPE
	 && file->filetype==file->SYMLINK)
	    continue;

	 char *local_name=dir_file(dir,file->name);
	 struct utimbuf ut;
	 ut.actime=ut.modtime=file->date;
	 utime(local_name,&ut);
      }
   }
}
void FileSet::LocalChmod(const char *dir)
{
   FileInfo *file;
   for(int i=0; i<fnum; i++)
   {
      file=files[i];
      if(file->defined & file->MODE)
      {
	 if(file->defined & file->TYPE
	 && file->filetype==file->SYMLINK)
	    //lchmod
	    continue;

	 char *local_name=dir_file(dir,file->name);

	 chmod(local_name,file->mode);
      }
   }
}

char  *dir_file(const char *dir,const char *file)
{
   static char *buf=0;
   static int buf_size=0;
   int len=strlen(dir)+1+strlen(file)+1;
   if(buf_size<len)
      buf=(char*)xrealloc(buf,buf_size=len);
   len=strlen(dir);
   if(len==0)
      sprintf(buf,"%s",file);
   else if(dir[len-1]=='/')
      sprintf(buf,"%s%s",dir,file);
   else
      sprintf(buf,"%s/%s",dir,file);
   return buf;
}

void FileSet::Count(int *d,int *f,int *s,int *o)
{
   for(int i=0; i<fnum; i++)
   {
      if(!(files[i]->defined&FileInfo::TYPE))
	 (*o)++;
      else switch(files[i]->filetype)
      {
      case(FileInfo::DIRECTORY):
	 (*d)++; break;
      case(FileInfo::NORMAL):
	 (*f)++; break;
      case(FileInfo::SYMLINK):
	 (*s)++; break;
      }
   }
}

void  FileSet::SetSize(char *name,long size)
{
   for(int i=0; i<fnum; i++)
   {
      if(files[i]->name==name)
      {
	 files[i]->SetSize(size);
   	 return;
      }
   }
}
void  FileSet::SetDate(char *name,time_t date)
{
   for(int i=0; i<fnum; i++)
   {
      if(files[i]->name==name)
      {
	 files[i]->SetDate(date);
   	 return;
      }
   }
}
