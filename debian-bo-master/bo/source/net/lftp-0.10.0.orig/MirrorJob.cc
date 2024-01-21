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
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#include "MirrorJob.h"

void  MirrorJob::PrintStatus(int v)
{
   char *tab="\t";

   if(v!=-1)
      FtpJob::PrintStatus(v);
   else
      tab="";

   if(!Done())
      return;
   printf("%sTotal: %d director%s, %d file%s, %d symlink%s\n",tab,
      dirs,dirs==1?"y":"ies",
      tot_files,tot_files==1?"":"s",
      tot_symlinks,tot_symlinks==1?"":"s");
   if(new_files || new_symlinks)
      printf("%sNew: %d file%s, %d symlink%s\n",tab,
	 new_files,new_files==1?"":"s",
	 new_symlinks,new_symlinks==1?"":"s");
   if(mod_files || mod_symlinks)
      printf("%sModified: %d file%s, %d symlink%s\n",tab,
	 mod_files,mod_files==1?"":"s",
	 mod_symlinks,mod_symlinks==1?"":"s");
   if(del_dirs || del_files || del_symlinks)
      printf("%sRemoved: %d director%s, %d file%s, %d symlink%s\n",tab,
	 del_dirs,del_dirs==1?"y":"ies",
	 del_files,del_files==1?"":"s",
	 del_symlinks,del_symlinks==1?"":"s");
}

void  MirrorJob::ShowRunStatus(StatusLine *s)
{
   switch(state)
   {
   case(DONE):
      s->Show("");
      break;

   case(WAITING_FOR_SUBGET):
   case(WAITING_FOR_SUBMIRROR):
      if(waiting && !waiting->Done())
	 waiting->ShowRunStatus(s);
      break;

   case(CHANGING_REMOTE_DIR):
      s->Show("CWD  `%s' [%s]",remote_dir,session->CurrentStatus());
      break;

   case(GETTING_SHORT_LIST):
      s->Show("NLST `%s' [%s]",remote_dir,session->CurrentStatus());
      break;

   case(GETTING_LONG_LIST):
      s->Show("LIST `%s' [%s]",remote_dir,session->CurrentStatus());
      break;

   case(GETTING_INFO):
      if(session->IsOpen())
	 s->Show("Getting files info [%s]",session->CurrentStatus());
      break;
   }
}

void  MirrorJob::HandleFile(int how)
{
   ArgV *args;
   int	 res;
   mode_t mode;
   GetJob   *gj;
   struct stat st;

   char *local_name=dir_file(local_dir,file->name);
   local_name=strcpy((char*)alloca(strlen(local_name)+1),local_name);
   char *remote_name=dir_file(remote_dir,file->name);

   if(file->defined&file->TYPE)
   {
      switch(file->filetype)
      {
      case(file->NORMAL):
      try_get:
	 if(how!=0)
	 {
	    to_get->next();
	    break;
	 }
	 if(lstat(local_name,&st)!=-1)
	 {
	    mod_files++;
	    remove(local_name);
	 }
	 else
	 {
	    new_files++;
	 }
	 // launch get job
	 args=new ArgV;
	 args->Append("get");
	 if(file->name[0]=='-')
	    args->Append("--");
	 args->Append(file->name);
	 args->Append("-o");
	 args->Append(local_name);
	 gj=new GetJob(Clone(),0,args);
	 if(file->defined&(file->DATE|file->DATE_UNPREC))
	    gj->SetTime(file->date);
	 waiting=gj;
	 waiting->parent=this;
	 waiting->cmdline=args->Combine();
	 delete args;
	 break;

      case(file->DIRECTORY):
	 if(how!=1)
	 {
	    to_get->next();
	    break;
	 }
	 if(!strcmp(file->name,".") || !strcmp(file->name,".."))
	 {
	    to_get->next();
	    break;
	 }
	 mode=(file->defined&file->MODE)?file->mode:0755;
	 res=mkdir(local_name,mode);
	 // launch sub-mirror
	 waiting=new MirrorJob(Clone(),local_name,remote_name);
	 waiting->parent=this;
	 waiting->cmdline=(char*)xmalloc(strlen("mirror")+1+
			      strlen(local_name)+1+strlen(remote_name)+1);
	 sprintf(waiting->cmdline,"mirror %s %s",remote_name,local_name);
	 break;

      case(file->SYMLINK):
	 if(how!=0)
	 {
	    to_get->next();
	    break;
	 }
#ifdef HAVE_LSTAT
	 if(file->defined&file->SYMLINK)
	 {
	    struct stat st;
	    if(lstat(local_name,&st)!=-1)
	    {
	       mod_symlinks++;
	       remove(local_name);
	    }
	    else
	    {
	       new_symlinks++;
	    }
	    res=symlink(file->symlink,local_name);
	    if(res==-1)
	       perror(local_name);
	 }
#endif /* LSTAT */
	 to_get->next();
	 break;
      }
   }
   else
   {
      // no info on type -- try to get
      goto try_get;
   }
}

int   MirrorJob::Do()
{
   char	 **glob_res;
   int	 res;
   FileSet  *files;
   const char	 *rcwd;

   switch(state)
   {
   case(DONE):
      return STALL;

   case(CHANGING_REMOTE_DIR):
      res=session->Read(0,0);
      if(res==Ftp::SEE_ERRNO &&
	    (errno==EINTR || errno==EAGAIN || errno==EMFILE || errno==ENFILE))
	 return STALL;
      if(res<0)
      {
	 fprintf(stderr,"mirror: %s\n",session->StrError(res));
	 state=DONE;
	 session->Close();
	 return MOVED;
      }
      session->Close();

      rcwd=session->GetCwd();
      xfree(remote_dir);
      remote_dir=xstrdup(rcwd);

      GetBetter();
      glob=new RemoteGlob(session,"",Ftp::LONG_LIST);
      state=GETTING_LONG_LIST;
      return MOVED;

   case(GETTING_LONG_LIST):
      if(!glob->Done())
	 return STALL;
      glob_res=glob->GetResult();
      to_get=parse_listing(glob_res);
      delete glob;

      GetBetter();
      glob=new RemoteGlob(session,"",Ftp::LIST);
      state=GETTING_SHORT_LIST;
      return MOVED;

   case(GETTING_SHORT_LIST):
      if(!glob->Done())
	 return STALL;
      glob_res=glob->GetResult();
      to_get->Merge(glob_res);
      delete glob;
      glob=0;

      session->Close();
      state=GETTING_INFO;
      to_get->rewind();
      return MOVED;

   case(GETTING_INFO):
      if(session->IsClosed())
      {
	 get_info=(Ftp::fileinfo*)xmalloc(sizeof(*get_info)*to_get->get_fnum());
	 cnt=0;
	 for(;;)
	 {
	    file=to_get->curr();
	    if(file==0)
	       break;

	    if((file->defined & file->TYPE &&
	       (file->filetype==file->SYMLINK || file->filetype==file->DIRECTORY))
	    || ((file->defined&(file->DATE|file->SIZE))==(file->DATE|file->SIZE)))
	    {
	       to_get->next();
	       continue;
	    }
	    get_info[cnt].get_size=!(file->defined & file->SIZE);
	    if(!get_info[cnt].get_size)
	       get_info[cnt].size=-1;
	    get_info[cnt].get_time=!(file->defined & file->DATE);
	    if(!get_info[cnt].get_time)
	       get_info[cnt].time=(time_t)-1;
	    get_info[cnt].file=file->name;
	    cnt++;
	    to_get->next();
	 }
	 if(cnt)
	 {
	    GetBetter();
	    session->GetInfoArray(get_info,cnt);
      	 }
	 else
	    goto info_done;
      }
      res=session->Done();
      if(res==Ftp::SEE_ERRNO &&
	    (errno==EINTR || errno==EAGAIN || errno==EMFILE || errno==ENFILE))
      {
	 block+=TimeOut(1000);
	 return STALL;
      }
      if(res==Ftp::IN_PROGRESS)
	 return STALL;
      assert(res==Ftp::OK);
      session->Close();

      for(int i=0; i<cnt; i++)
      {
	 if(get_info[i].time!=(time_t)-1)
	    to_get->SetDate(get_info[i].file,get_info[i].time);
	 if(get_info[i].size!=-1)
	    to_get->SetSize(get_info[i].file,get_info[i].size);
      }

   info_done:
      xfree(get_info);
      get_info=0;

      to_get->Count(&dirs,&tot_files,&tot_symlinks,&tot_files);

      files=local_files(local_dir);

      to_rm=new FileSet(files);
      to_rm->SubtractAny(to_get);

      same=new FileSet(to_get);
      to_get->SubtractSame(files);
      same->SubtractAny(to_get);

      delete files;

      to_get->rewind();
      waiting=0;
      state=WAITING_FOR_SUBGET;
      return MOVED;

   case(WAITING_FOR_SUBGET):
      if(waiting && waiting->Done())
      {
	 to_get->next();
	 delete waiting;
	 waiting=0;
      }
      if(!waiting)
      {
	 file=to_get->curr();
      	 if(!file)
	 {
	    to_get->rewind();
	    state=WAITING_FOR_SUBMIRROR;
	    return MOVED;
	 }
	 HandleFile(0);
	 return MOVED;
      }
      return STALL;

   case(WAITING_FOR_SUBMIRROR):
      if(waiting && waiting->Done())
      {
	 MirrorJob &mj=*(MirrorJob*)waiting; // we are sure it is a MirrorJob
	 tot_files+=mj.tot_files;
	 new_files+=mj.new_files;
	 mod_files+=mj.mod_files;
	 del_files+=mj.del_files;
	 tot_symlinks+=mj.tot_symlinks;
	 new_symlinks+=mj.new_symlinks;
	 mod_symlinks+=mj.mod_symlinks;
	 del_symlinks+=mj.del_symlinks;
	 dirs+=mj.dirs;
	 del_dirs+=mj.del_dirs;

	 to_get->next();
	 delete waiting;
	 waiting=0;
      }
      if(!waiting)
      {
	 file=to_get->curr();
      	 if(!file)
	 {
	    to_rm->Count(&del_dirs,&del_files,&del_symlinks,&del_files);
	    //remove to_rm
	    to_rm->LocalRemove(local_dir);
	    // set mtime on gotten files
	    to_get->LocalUtime(local_dir);
	    // set modes on same
	    same->LocalChmod(local_dir);
	    state=DONE;
	    return MOVED;
	 }
	 HandleFile(1);
	 return MOVED;
      }
      return STALL;
   }
   /*NOTREACHED*/
   abort();
}

MirrorJob::MirrorJob(Ftp *f,const char *new_local_dir,const char *new_remote_dir)
   : FtpJob(f)
{
   local_dir=xstrdup(new_local_dir);
   remote_dir=xstrdup(new_remote_dir);

   GetBetter();
   session->Chdir(remote_dir);
   state=CHANGING_REMOTE_DIR;

   to_get=to_rm=same=0;
   file=0;
   glob=0;

   tot_files=new_files=mod_files=del_files=
   tot_symlinks=new_symlinks=mod_symlinks=del_symlinks=0;
   dirs=1; del_dirs=0;

   get_info=0;
}

MirrorJob::~MirrorJob()
{
   xfree(get_info);
   xfree(local_dir);
   xfree(remote_dir);
   if(to_get)
      delete to_get;
   if(to_rm)
      delete to_rm;
   if(same)
      delete same;
   // don't delete this->file -- it is a reference
   if(glob)
      delete glob;
}
