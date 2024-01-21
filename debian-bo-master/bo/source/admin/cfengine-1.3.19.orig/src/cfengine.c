/* 

        Copyright (C) 1995
        Free Software Foundation, Inc.

   This file is part of GNU cfengine - written and maintained 
   by Mark Burgess, Dept of Computing and Engineering, Oslo College,
   Dept. of Theoretical physics, University of Oslo
 
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version. 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

*/


/*******************************************************************/
/*                                                                 */
/*  Cfengine : a site configuration langugae                       */
/*                                                                 */
/*  Module: (main) cfengine.c                                      */
/*                                                                 */
/*  Mark Burgess 1994/96                                           */
/*                                                                 */
/*******************************************************************/

#include "cf.defs.h"
#include "cf.extern.h"
#include "../pub/getopt.h"

extern FILE *yyin;

/*******************************************************************/
/* Level 0 : Main                                                  */
/*******************************************************************/

main (argc,argv)

char *argv[];
int argc;

{ void HandleSignal();
  struct Item *action;

signal (SIGTERM,HandleSignal);                   /* Signal Handler */
signal (SIGHUP,HandleSignal);

PreNetConfig();

Initialize(argc,argv);

if (! NOHARDCLASSES)
    {
    GetNameInfo();
    }

if (Locked())                             /* Establish lock for root */
   {
   exit(0);
   }

ReadRCFile();

ParseInputFiles();

SetEnvironment();
EchoValues();

if (PARSEONLY)                                          /* -p option */
   {
   Unlock();
   exit(0);
   }

if (PRSYSADM)                                           /* -a option */
   {
   printf("%s\n",VSYSADM);
   Unlock();
   exit (0);
   }

CheckSystemVariables();

for (action = VACTIONSEQ; action !=NULL; action=action->next)
   {
   switch(EvaluateAction(action->name,&VADDCLASSES))
      {
      case mountinfo:
                        GetHomeInfo();
                        GetMountInfo();
                        break;
      case mkpaths:
                        MakePaths();
                        break;
      case lnks:
                        MakeChildLinks();
                        MakeLinks();
                        break;
      case simplelnks:
                        MakeLinks();
                        break;
      case childlnks:
                        MakeChildLinks();
                        break;
      case chkmail:
                        MailCheck();
                        break;
      case mountall:
                        if (! NOMOUNTS )
                           {
                           MountFileSystems();
                           }
                        break;
      case requir:
                        CheckRequired();
                        break;
      case tidyf:
                        if (! NOTIDY) 
                           {
                           TidyFiles();
                           }
                        break;
      case shellcom:
                        if (! NOSCRIPTS)
                           {
                           Scripts();
                           }
                        break;
      case chkfiles:
                        if (! NOFILECHECK)
                           {
                           GetSetuidLog();
                           CheckFiles();
                           SaveSetuidLog();
                           }
                        break;
      case disabl:
                        DisableFiles();
                        break;
      case mountresc:
                        if (! NOMOUNTS)
                           {
                           MountHomeBinServers();
                           MountMisc();
                           }
                        break;
      case edfil:
                        if (! NOEDITS)
                           {
                           EditFiles();
                           }
                        break;

      case umnt:
                        Unmount();
                        break;

      case resolv:
                        CheckResolv();
                        break;
      case imag:
                        if (! NOCOPY)
                           {
                           MakeImages();
                           }
                        break;
      case netconfig:
                        if (IFCONF)
                           {
                           IfConf(VSYSTEMHARDCLASS);
                           SetDefaultRoute(); 
                           }

                        break;

      case tzone:
                        CheckTimeZone();
                        break;

      case procs:
                        if (! NOPROCS)
			    {
			    CheckProcesses();
			    }
                        break;

      default:  
                        sprintf(VBUFF,"Undefined action %s in sequence\n",action->name);
                        FatalError(VBUFF);
                        break;
      }

   DeleteItemList(VADDCLASSES);
   VADDCLASSES = NULL;
   }


Unlock();
}

/*******************************************************************/
/* Level 1                                                         */
/*******************************************************************/

ReadRCFile()

{ char filename[bufsize], buffer[bufsize], *sp, *mp;
  char class[maxvarsize], variable[maxvarsize], value[maxvarsize];
  int c;
  FILE *fp;

filename[0] = buffer[0] = class[0] = variable[0] = value[0] = '\0';
LINENUMBER = 0;

if ((sp=getenv(CFINPUTSVAR)) != NULL)
   {
   strcpy(filename,sp);
   if (filename[strlen(filename)-1] != '/')
      {
      strcat(filename,"/");
      }
   }

strcat(filename,VRCFILE);

if ((fp = fopen(filename,"r")) == NULL)      /* Open root file */
   {
   return;
   }

while (!feof(fp))
   {
   buffer[0] = '\0';
   fgets(buffer,bufsize,fp);
   LINENUMBER++;
   class[0]='\0';
   variable[0]='\0';
   value[0]='\0';

   if (strlen(buffer) == 0 || buffer[0] == '#')
      {
      continue;
      }

   if (strstr(buffer,":"))
      {
      Debug("Malformed line (missing :) in resource file %s - skipping\n",VRCFILE);
      continue;
      }

   sscanf(buffer,"%[^.].%[^:]:%[^\n]",class,variable,value);

   if (class[0] == '\0' || variable[0] == '\0' || value[0] == '\0')
      {
      printf("cfengine: %s:%s - Bad resource\n",VRCFILE,buffer);
      printf("          class=%s,variable=%s,value=%s\n",class,variable,value);
      FatalError("Bad resource");
      }

   if (strcmp(CLASSTEXT[VSYSTEMHARDCLASS],class) != 0) 
      {
      continue;  /* No point if not equal*/
      }

   if ((mp = malloc(strlen(value)+1)) == NULL)
      {
      perror("malloc");
      FatalError("malloc in ReadRCFile");
      }

   strcpy(mp,value);
   Verbose("Redefining resource %s as %s (%s)\n",variable,value,class);

   c = VSYSTEMHARDCLASS;

   switch (GetResource(variable))
      {
      case rmountcom:
                        VMOUNTCOMM[c] = mp;
                        break;

      case runmountcom:
                        VUNMOUNTCOMM[c] = mp;
                        break;
      case rethernet:
                        VIFDEV[c] = mp;
                        break;
      case rmountopts:
                        VMOUNTOPTS[c] = mp;
                        break;
      case rfstab:
                        VFSTAB[c] = mp;
                        break;
      case rmaildir:
                        VMAILDIR[c] = mp;
                        break;
      case rnetstat:
                        VNETSTAT[c] = mp;
                        break;
      case rpscomm:
	                VPSCOMM[c] = mp;
	                break;
      case rpsopts:
	                VPSOPTS[c] = mp;
	                break;
      default:
                        sprintf(VBUFF,"Bad resource %s in %s\n",variable,VRCFILE);
                        FatalError(VBUFF);
                        break;
      }
   }

fclose(fp);
}

/*******************************************************************/

Initialize(argc,argv)

char *argv[];
int argc;

{ char *sp, *cfargv[maxargs];
  int i, cfargc;

umask(0);                 /* This make the DEFAULT modes absolute */

strcpy(VDOMAIN,"undefined.domain");

strcpy(VERSION,CFVERSION);

VFACULTY[0] = '\0';
VSYSADM[0] = '\0';
VNETMASK[0]= '\0';
VBROADCAST[0] = '\0';
VMAILSERVER[0] = '\0';
VREPOSITORY[0] = '\0';
VDEFAULTROUTE[0] = '\0';
VARCH[0] = '\0';

CURRENTITEM[0] = '\0';
GROUPBUFF[0] = '\0';
ACTIONBUFF[0] = '\0';
CURRENTPATH[0] = '\0';
CLASSBUFF[0] = '\0';
LINKFROM[0] = '\0';
TICKSPERDAY = 60*60*24;

re_syntax_options |= RE_INTERVALS;

strcpy(VINPUTFILE,"cfengine.conf");
strcpy(VNFSTYPE,"nfs");

for (i = 0; i < hashtablesize; i++)
   {
   HASH[i] = 0;
   }

AddClassToHeap("any");      /* This is a reserved word / wildcard */

 /* Note we need to fix the options since the argv mechanism doesn't */
 /* work when the shell #!/bla/cfengine -v -f notation is used.      */
 /* Everything ends up inside a single argument! Here's the fix      */

cfargc = 1;
cfargv[0]="cfengine";

for (i = 1; i < argc; i++)
   {
   sp = argv[i];

   while (*sp != '\0')
      {
      while (*sp++ == ' ' && *sp != '\0') /* Skip to arg */
         {
         }

      cfargv[cfargc++] = sp-1;
      *(sp-2) = '\0';           /* break the argv string */

      while (*sp++ != ' ' && *sp != '\0') /* Skip to white space */
         {
         }
      }
   }

VDEFAULTBINSERVER.name = "";

CheckOpts(cfargc,cfargv);
}

/*******************************************************************/

CheckSystemVariables()

{ struct stat statbuf;
  char id[maxvarsize];

Debug2("\n\n");

if (VACTIONSEQ == NULL)
   {
   Warning("actionsequence is empty");
   }

sprintf(id,"%d",geteuid());   /* get effective user id */

if (VACCESSLIST != NULL && !IsItemIn(VACCESSLIST,id))
   {
   FatalError("Access denied");
   }

Debug2("cfengine -d : Debugging output enabled.\n");

if (DONTDO && (VERBOSE || DEBUG || D2))
   {
   printf("cfengine -n: Running in ``All talk and no action'' mode\n");
   }

if (TRAVLINKS && (VERBOSE || DEBUG || D2))
   {
   printf("cfengine -l : will traverse symbolic links\n");
   }

if ( ! IFCONF && (VERBOSE || DEBUG || D2))
   {
   printf("cfengine -i : suppressing interface configuration\n");
   }

if ( NOFILECHECK && (VERBOSE || DEBUG || D2))
   {
   printf("cfengine -c : suppressing file checks\n");
   }

if ( NOSCRIPTS && (VERBOSE || DEBUG || D2))
   {
   printf("cfengine -s : suppressing script execution\n");
   }

if ( NOTIDY && (VERBOSE || DEBUG || D2))
   {
   printf("cfengine -t : suppressing tidy function\n");
   }

if ( NOMOUNTS && (VERBOSE || DEBUG || D2))
   {
   printf("cfengine -m : suppressing mount operations\n");
   }

if ( MOUNTCHECK && (VERBOSE || DEBUG || D2))
   {
   printf("cfengine -C : check mount points\n");
   }


if (ERRORCOUNT > 0)
   {
   FatalError("Execution terminated after parsing due to errors in program");
   }

}

/*******************************************************************/

CheckTimeZone()

{ time_t tloc;

if (VTIMEZONE[0] == '\0')
   {
   FatalError("Program does not define a timezone");
   }

#ifndef AOS
#ifndef SUN4

tzset();

if (strncmp(tzname[0],VTIMEZONE,3) != 0)
   {
   printf("cfengine: WARNING! The time zone was %s (should be %s)\n\n",tzname[0],VTIMEZONE);
   }
else
   {
   Verbose("\nThe timezone is %s\n\n",VTIMEZONE);
   }

#else

if ((tloc = time((time_t *)NULL)) == -1)
   {
   printf("Couldn't read system clock\n\n");
   }

if (strncmp(localtime(&tloc)->tm_zone ,VTIMEZONE,3) != 0)
   {
   if ( ! SILENT )
      {
      printf("cfengine: WARNING! The time zone was %s (should be %s)\n\n",localtime(&tloc)->tm_zone,VTIMEZONE);
      }
   }
else
   {
   Verbose("\nThe timezone is %s\n\n",VTIMEZONE);
   }

#endif /* SUN4 */
#endif /* AOS  */
}

/*******************************************************************/

CheckProcesses()

{ struct Process *pp;
  struct Item *procdata = NULL;
  char *psopts = VPSOPTS[VSYSTEMHARDCLASS];

Banner("cfengine: Checking Processes:");

if (!LoadProcessTable(&procdata,psopts))
   {
   printf("cfengine: was unable to read the process table\n");
   return;
   }

for (pp = VPROCLIST; pp != NULL; pp=pp->next)
   {
   if (strcmp(pp->expr,"SetOptionString") == 0)
      {
      psopts = pp->restart;
      DeleteItemList(procdata);
      procdata = NULL;
      if (!LoadProcessTable(&procdata,psopts))
	 {
	 printf("cfengine: unable to read the process table\n");
	 }
      }

   DoProcessCheck(pp,procdata);
   }
}

/*******************************************************************/

GetNameInfo()

{ int i, found = false;
  char *sp,*sp2;
  time_t tloc;

if (uname(&VSYSNAME) == -1)
   {
   perror("uname ");
   FatalError("cfengine: uname couldn't get kernel name info!!\n");
   }

for (sp = VSYSNAME.sysname; *sp != '\0'; sp++)
   {
   *sp = ToLower(*sp);
   }

for (sp = VSYSNAME.machine; *sp != '\0'; sp++)
   {
   *sp = ToLower(*sp);
   }


for (i = 0; CLASSATTRIBUTES[i][0] != '\0'; i++)
   {
   if (strcmp(CLASSATTRIBUTES[i][0],VSYSNAME.sysname) == 0)
      {
      if (WildMatch(CLASSATTRIBUTES[i][1],VSYSNAME.machine))
         {
         if (WildMatch(CLASSATTRIBUTES[i][2],VSYSNAME.release))
            {
	    if (UNDERSCORE_CLASSES)
	       {
	       sprintf(VBUFF,"_%s",CLASSTEXT[i]);
	       AddClassToHeap(VBUFF);
	       }
	    else
	       {
               AddClassToHeap(CLASSTEXT[i]);
	       }
            found = true;
            break;
            }
         }
      else
         {
         Debug2("Cfengine: I recognize %s but not %s\n",VSYSNAME.sysname,VSYSNAME.machine);
         continue;
         }
      }
   }

if ((sp = malloc(strlen(VSYSNAME.nodename)+1)) == NULL)
   {
   FatalError("malloc failure in initialize()");
   }

strcpy(sp,VSYSNAME.nodename);

for (sp2=sp; *sp2 != '\0'; sp2++)  /* Truncate fully qualified name */
   {
   if (*sp2 == '.')
      {
      *sp2 = '\0';
      Debug("cfengine: INFO! Truncating fully qualified hostname %s to %s\n",VSYSNAME.nodename,sp);
      break;
      }
   }

AddClassToHeap(sp);                 /* Unqualified host name comes first */
VDEFAULTBINSERVER.name = sp;

VSYSTEMHARDCLASS = (enum classes) i;


if ((tloc = time((time_t *)NULL)) == -1)
   {
   printf("Couldn't read system clock\n");
   }

AddDayClass(ctime(&tloc));

if (VERBOSE)
   {
   if (UNDERSCORE_CLASSES)
      {
      sprintf(VBUFF,"_%s",CLASSTEXT[i]);
      }
   else
      {
      sprintf(VBUFF,"%s",CLASSTEXT[i]);
      }
   
   printf ("GNU cfengine - GNU Configuration Engine - \n%s\n%s\n\n",VERSION,COPYRIGHT);
   printf ("------------------------------------------------------------------------\n\n");
   printf ("Host name is: %s\n",VSYSNAME.nodename);
   printf ("Operating System Type is %s\n",VSYSNAME.sysname);
   printf ("Operating System Release is %s\n",VSYSNAME.release);
   printf ("Architecture = %s\n\n\n",VSYSNAME.machine);
   printf ("Using internal soft-class %s for host %s\n\n",VBUFF,VSYSNAME.nodename);
   printf ("The time is now %s\n\n",ctime(&tloc));
   printf ("------------------------------------------------------------------------\n\n");

   }

sprintf(VBUFF,"%d_bit",sizeof(long)*8);
AddClassToHeap(VBUFF);
Verbose("Additional hard class defined as: %s\n",VBUFF);

sprintf(VBUFF,"%s_%s",VSYSNAME.sysname,VSYSNAME.release);
for (sp = VBUFF; *sp != '\0'; sp++)
   {
   if (*sp == '.')
      {
      *sp = '_';
      }
   }
AddClassToHeap(VBUFF);

Verbose("Additional hard class defined as: %s\n",VBUFF);

sprintf(VBUFF,"%s_%s",VSYSNAME.sysname,VSYSNAME.machine);
for (sp = VBUFF; *sp != '\0'; sp++)
   {
   if (*sp == '.')
      {
      *sp = '_';
      }
   }
AddClassToHeap(VBUFF);

Verbose("Additional hard class defined as: %s\n",VBUFF);

sprintf(VBUFF,"%s_%s_%s",VSYSNAME.sysname,VSYSNAME.machine,VSYSNAME.release);
for (sp = VBUFF; *sp != '\0'; sp++)
   {
   if (*sp == '.')
      {
      *sp = '_';
      }
   }

AddClassToHeap(VBUFF);
Verbose("Additional hard class defined as: %s\n",VBUFF);

sprintf(VBUFF,"%s_%s_%s_%s",VSYSNAME.sysname,VSYSNAME.machine,VSYSNAME.release,VSYSNAME.version);
for (sp = VBUFF; *sp != '\0'; sp++)
   {
   if (*sp == '.')
      {
      *sp = '_';
      }
   }


if (strlen(VBUFF) < maxvarsize-2)
   {
   strcpy(VARCH,VBUFF);
   }
else
   {
   Verbose("cfengine internal: $(arch) overflows maxvarsize! Truncating\n");
   strcpy(VARCH,VSYSNAME.sysname);
   }

for (sp = VARCH; *sp != '\0'; sp++)
   {
   if (*sp == ' ')
      {
      *sp = '\0';   /* Truncate at first space */
      break;
      }
   }

AddClassToHeap(VARCH);
Verbose("Additional hard class defined as: %s\n",VARCH);

if (! found)
   {
   FatalError("Cfengine: I don't understand what architecture this is!");
   }

sprintf(LOGFILE,"%s/cfengine.%s.log",LOGFILEDIR,VSYSNAME.nodename);
sprintf(LOCKFILE,"%s/cfengine.%s.pid",LOCKFILEDIR,VSYSNAME.nodename);
sprintf(LASTFILE,"%s/cfengine.%s.last",LOGFILEDIR,VSYSNAME.nodename);

strcpy(VSETUIDLOG,LOGFILE);
}

/*******************************************************************/

ParseInputFiles()

{ struct Item *ptr;
  char filename[bufsize], *sp;

filename[0] = '\0';

if ((sp=getenv(CFINPUTSVAR)) != NULL)
   {
   if (! IsAbsoluteFileName(VINPUTFILE))     /* Don't prepend to absolute names */
      { 
      strcpy(filename,sp);
      if (filename[strlen(filename)-1] != '/')
         {
         strcat(filename,"/");
         }
      }
   }

strcat(filename,VINPUTFILE);

if ((yyin = fopen(filename,"r")) == NULL)      /* Open root file */
   {
   if (sp == NULL)
      {
      printf("cfengine: (%s is set to <nothing>)\n",CFINPUTSVAR);
      }
   else
      {
      printf("cfengine: (%s is set to %s)\n",CFINPUTSVAR,sp);
      }
   printf("cfengine: Can't open file %s\n",VINPUTFILE);
   Unlock();
   exit (1);
   }

strcpy(VCURRENTFILE,VINPUTFILE);

Debug("BEGIN PARSING %s\n",VCURRENTFILE);

LINENUMBER=1;

while (!feof(yyin))
   { 
   yyparse();
   }

fclose (yyin);

InstallPending(ACTION);

for (ptr = VIMPORT; ptr != NULL; ptr=ptr->next)
   {
   filename[0] = '\0';

   if ((sp=getenv(CFINPUTSVAR)) != NULL)
      {
      if (! IsAbsoluteFileName(ptr->name))   
         { 
         strcpy(filename,sp);
         if (filename[strlen(filename)-1] != '/')
            {
            strcat(filename,"/");
            }
         }
      }

   strcat(filename,ptr->name);

   if ((yyin = fopen(filename,"r")) == NULL) 
      {
      if (sp == NULL)
         {
         printf("cfengine: (%s is set to <nothing>)\n",CFINPUTSVAR);
         }
      else
         {
         printf("cfengine: (%s is set to %s)\n",CFINPUTSVAR,sp);
         }
      sprintf(VBUFF,"Can't open file %s\n",filename);
      FatalError(VBUFF);
      }

   LINENUMBER = 1;
   strcpy(VCURRENTFILE,ptr->name);

   Debug("PARSING FILE (%s)\n\n",ptr->name);

   while (!feof(yyin))
      { 
      yyparse();
      }

   fclose (yyin);
   InstallPending(ACTION);
   }

VCURRENTFILE[0]='\0';       /* Zero filename for subsequent errors */

Debug("(END OF PARSING)\n");
}

/*******************************************************************/

SetEnvironment()

{
VBUFF[0] = '\0';

/* Define an environment variable here with all static classes ? */
}

/*******************************************************************/

EchoValues()

{ struct Item *ip;
  char *sp1,*sp2;

if (strstr(VSYSNAME.nodename,VDOMAIN))
   {
   strcpy(VFQNAME,VSYSNAME.nodename);
   sp2 = VUQNAME;
   
   for (sp1 = VSYSNAME.nodename; *sp1 != '.' && *sp1 != '\0'; sp1++) /* Note the unqualified name */
      {
      *sp2++ = *sp1;
      }
   *sp2 = '\0';
   }
else
   {
   sprintf(VFQNAME,"%s.%s",VSYSNAME.nodename,VDOMAIN);
   strcpy(VUQNAME,VSYSNAME.nodename);
   }

if (VERBOSE || DEBUG || D2)
   {
   ListDefinedClasses();
   }

if (DEBUG || D2)
   {
   printf("\nFully qualified hostname is: %s\n",VFQNAME);
   printf("Unqualified hostname is: %s\n",VUQNAME);
   printf("\nSystem administrator mail address is: %s\n",VSYSADM);
   printf("Sensible size = %d\n",SENSIBLEFSSIZE);
   printf("Sensible count = %d\n",SENSIBLEFILECOUNT);
   printf("Edit File (Max) Size = %d\n\n",EDITFILESIZE);
   printf("------------------------------------------------------------\n");
   ListDefinedBinservers();
   printf("------------------------------------------------------------\n");
   ListDefinedHomeservers();
   printf("------------------------------------------------------------\n");
   ListDefinedHomePatterns();
   printf("------------------------------------------------------------\n");
   ListActionSequence();
   printf("\nUsing mailserver %s\n",VMAILSERVER);
   printf("\nLocal mountpoints: ");
   for (ip = VMOUNTLIST; ip != NULL; ip=ip->next)
      {
      printf ("%s ",ip->name);
      }
   printf("\n");
   printf("\nDefault route for packets %s\n\n",VDEFAULTROUTE);
   printf("\nFile repository = %s\n\n",VREPOSITORY);
   printf("------------------------------------------------------------\n");
   ListDefinedLinks();
   printf("------------------------------------------------------------\n");
   ListDefinedLinkchs();
   printf("------------------------------------------------------------\n");
   ListDefinedResolvers();
   printf("------------------------------------------------------------\n");
   ListDefinedScripts();
   printf("------------------------------------------------------------\n");
   ListDefinedImages();
   printf("------------------------------------------------------------\n");
   ListDefinedTidy();
   printf("------------------------------------------------------------\n");
   ListDefinedRequired();
   printf("------------------------------------------------------------\n");
   ListDefinedMountables();
   printf("------------------------------------------------------------\n");
   ListMiscMounts();
   printf("------------------------------------------------------------\n");
   ListFiles();
   printf("------------------------------------------------------------\n");
   ListDefinedDisable();
   printf("------------------------------------------------------------\n");
   ListDefinedMakePaths();
   printf("------------------------------------------------------------\n");
   ListDefinedIgnore();
   printf("------------------------------------------------------------\n");
   ListDefinedImports();
   printf("------------------------------------------------------------\n");
   ListFileEdits();
   printf("------------------------------------------------------------\n");
   ListUnmounts();
   printf("------------------------------------------------------------\n");
   ListProcesses();
   printf("------------------------------------------------------------\n");
   }
}


/*******************************************************************/

GetHomeInfo()

{ DIR *dirh;
  struct dirent *dirp;
  struct Item *ip;

if (getuid() != 0)                            
   {
   if (VERBOSE || DEBUG || D2)
      { 
      printf("Not root, so skipping GetHomeInfo()\n");
      }
   return;
   }

if (!MountPathDefined())
   {
   return;
   }


for (ip = VMOUNTLIST; ip != NULL; ip=ip->next)
   {
   if (IsExcluded(ip->classes))
      {
      continue;
      }
   
   if ((dirh = opendir(ip->name)) == NULL)
      {
      Verbose("cfengine: INFO: Host %s seems to have no (additional) local disks except the OS\n",VDEFAULTBINSERVER.name); 
      Verbose("          mounted under %s\n\n",ip->name);
      return;
      }

   for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
      {
      if (strcmp(".",dirp->d_name) == 0 || strcmp("..",dirp->d_name) == 0)
         {
         continue;
         }

      if (strcmp("lost+found",dirp->d_name) == 0)
         {
         continue;
         }

      sprintf(VBUFF,"%s/%s",ip->name,dirp->d_name);

      if (IsHomeDir(VBUFF))
         {
         Verbose("Host defines a home directory %s\n",VBUFF);
         }
      else
         {
         Verbose("Host defines a potential mount point %s\n",VBUFF);
         }

      sprintf(CURRENTPATH,"%s%s",ip->name,dirp->d_name);
      sprintf(CURRENTITEM,"%s:%s",VDEFAULTBINSERVER.name,CURRENTPATH);

      if (! IsItemIn(VMOUNTED,CURRENTITEM))
         {
         if ( MOUNTCHECK && ! RequiredFileSystemOkay(CURRENTPATH) && VERBOSE)
            {
            printf("cfengine: found a mountpoint %s but there was\n",CURRENTPATH);
            printf("          nothing mounted on it.\n\n");
            }
         }
      }
   closedir(dirh);
   }
}

/*******************************************************************/

enum aseq EvaluateAction(action,classlist)

char *action;
struct Item **classlist;

{ int i,j = 0;
  char *sp,cbuff[bufsize],actiontxt[bufsize];
  struct Item *ip;

cbuff[0]='\0';
actiontxt[0]='\0';
sp = action;

while (*sp != '\0')
   {
   ++j;
   sscanf(sp,"%[^.]",cbuff);

   while ((*sp != '\0') && (*sp !='.'))
      {
      sp++;
      }
 
   if (*sp == '.')
      {
      sp++;
      }
 
   if (IsHardClass(cbuff))
      {
      printf("cfengine::Error in action sequence: %s\n",action);
      FatalError("You cannot add a reserved class!");
      }
 
   if (j == 1)
      {
      strcpy(actiontxt,cbuff);
      continue;
      }
   else
      {
      AppendItem(classlist,cbuff,NULL);
      }
   }


BuildClassEnvironment();

if ((VERBOSE || DEBUG || D2) && *classlist != NULL)
   {
   printf("\n                  New temporary class additions\n");
   printf("                  -----------------------------\n");
   for (ip = *classlist; ip != NULL; ip=ip->next)
      {
      printf("                             %s\n",ip->name);
      }
   }

for (i = 0; ACTIONSEQTEXT[i] != NULL; i++)
   {
   if (strcmp(ACTIONSEQTEXT[i],actiontxt) == 0)
      {
      return(i);
      }
   }

return(non);
}

/*******************************************************************/

MakePaths()

{ struct File *ip;
  char *sp;
  struct stat statbuf;

Banner("cfengine: checking directories:");

for (ip = VMAKEPATH; ip != NULL; ip=ip->next)
   {
   if (IsExcluded(ip->classes))
      {
      continue;
      }

   CURRENTPATH[0] = '\0';
   ExpandVarstring(ip->path,CURRENTPATH,"");

   if (CURRENTPATH[strlen(CURRENTPATH)-1] != '/')
      {
      if (CURRENTPATH[strlen(CURRENTPATH)-2] != '/' && CURRENTPATH[strlen(CURRENTPATH)-1] != '.')
      strcat(CURRENTPATH,"/.");
      }

   MakeDirectoriesFor(CURRENTPATH);

   if (stat(CURRENTPATH,&statbuf) == -1)
      {
      printf("cfengine: cannot stat %s\n",CURRENTPATH);
      perror("stat");
      continue;
      }

   CheckExistingFile(ip->path,ip->plus,ip->minus,ip->action,ip->uid,ip->gid,&statbuf);
   }
}

/*******************************************************************/

MakeLinks()     /* <binserver> should expand to a best fit filesys */

{ struct Link *lp;
  char *sp, *bp;
  struct Item *ip;
  int matched,varstring;
  struct stat statbuf;
  short saveenforce;
  short savesilent;
  int LinkFiles(), HardLinkFiles(), AbsoluteLink(), RelativeLink();
  int (*linkfiles)();

if (NOLINKS)
   {
   return;
   }

Banner("cfengine: Checking links:");

for (lp = VLINK; lp != NULL; lp = lp->next)
   {
   if (IsExcluded(lp->classes))
      {
      continue;
      }

   switch (lp->type)
      {
      case 's':
                linkfiles = LinkFiles;
                break;
      case 'r':
	        linkfiles = RelativeLink;
		break;
      case 'a':
	        linkfiles = AbsoluteLink;
                break;
      case 'h':
                linkfiles = HardLinkFiles;
                break;
      default:
                printf("cfengine: internal error, link type was [%c]\n",lp->type);
                continue;
      }

   saveenforce = ENFORCELINKS;
   ENFORCELINKS = ENFORCELINKS || lp->force;

   savesilent = SILENT;
   SILENT = SILENT || lp->silent;

   matched = varstring = false;

   for( ip = VBINSERVERS; ip != NULL && (!matched); ip = ip->next)
      {
      CURRENTPATH[0] = CURRENTITEM[0] = '\0';

      varstring = ExpandVarbinserv(lp->to,CURRENTPATH,ip->name);

      if ((*linkfiles)(lp->from,CURRENTPATH,lp->inclusions,lp->exclusions,lp->copy))
         {
         matched = true;
         }
      else if (! varstring)
         {
         printf("cfengine: Error while trying to link %s -> %s\n",lp->from,CURRENTPATH);
         printf("          The file %s does not exist. Can't link.\n\n",CURRENTPATH);
         }

      if (! varstring)                       /* don't iterate over binservers if not var */
         {
         break;
         }
      }

   ENFORCELINKS = saveenforce;
   SILENT = savesilent;

   if (matched == false && ip == NULL)
      {
      printf("MakeLinks() didn't find any file to match %s -> %s\n",lp->from,lp->to);
      }
   }
}

/*******************************************************************/

MakeChildLinks()     /* <binserver> should expand to a best fit filesys */

{ struct Link *lp;
  char *sp, *bp;
  struct Item *ip;
  int matched,varstring;
  struct stat statbuf;
  short saveenforce;
  short savesilent;

if (NOLINKS)
   {
   return;
   }

Banner("cfengine: Checking multiple childlinks:");

for (lp = VCHLINK; lp != NULL; lp = lp->next)
   {
   if (IsExcluded(lp->classes))
      {
      continue;
      }

   saveenforce = ENFORCELINKS;
   ENFORCELINKS = ENFORCELINKS || lp->force;

   savesilent = SILENT;
   SILENT = SILENT || lp->silent;

   matched = varstring = false;

   for( ip = VBINSERVERS; ip != NULL && (!matched); ip = ip->next)
      {
      CURRENTPATH[0] = CURRENTITEM[0] = '\0';

      if (strcmp(lp->to,"linkchildren") == 0)           /* linkchildren */
         {
         if (stat(lp->from,&statbuf) == -1)
            {
            printf("cfengine: Makechildlinks() can't stat %s\n",lp->from);
            continue;
            }
         LinkChildren(lp->from,lp->type,&statbuf,NULL,NULL,NULL);
         break;
         }

      varstring = ExpandVarbinserv(lp->to,CURRENTPATH,ip->name);

      if (lp->recurse != 0)
	 {
	 matched = RecursiveLink(lp,lp->from,CURRENTPATH,lp->recurse);
         }
      else if (LinkChildFiles(lp->from,CURRENTPATH,lp->type,lp->inclusions,lp->exclusions,lp->copy))
         {
         matched = true;
         }
      else if (! varstring)
         {
         printf("cfengine: Error while trying to link %s -> %s\n",lp->from,CURRENTPATH);
         printf("          The file %s does not exist. Can't link.\n\n",CURRENTPATH);
         }

      if (! varstring)                       /* don't iterate over binservers if not var */
         {
         break;
         }
      }

   ENFORCELINKS = saveenforce;
   SILENT = savesilent;

   if (matched == false && ip == NULL)
      {
      printf("MakeChildLinks() didn't find any server to match %s -> %s\n",lp->from,lp->to);
      }
   }
}

/*******************************************************************/

MountFileSystems()

{ FILE *pp;

if (! GOTMOUNTINFO || DONTDO)
   {
   return;
   }

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can mount filesystems.\n");
   return;
   }

Banner("cfengine: mounting filesystems");

if ((pp = popen(VMOUNTCOMM[VSYSTEMHARDCLASS],"r")) == NULL)
   {
   printf("cfengine: failed to open pipe from %s\n",VMOUNTCOMM[VSYSTEMHARDCLASS]);
   return;
   }

while (!feof(pp))
   {
   fgets(VBUFF,bufsize,pp);

   if (strstr(VBUFF,"already mounted") || strstr(VBUFF,"exceeded") || strstr(VBUFF,"determined"))
      {
      continue;
      }

   if (strstr(VBUFF,"not supported"))
      {
      continue;
      }

   if (strstr(VBUFF,"access denied") || strstr(VBUFF,"RPC") || strstr(VBUFF,"root"))
      {
      printf("cfengine: There was a mount error.\n");
      printf("%s\n",VBUFF);
      GOTMOUNTINFO = false;
      break;
      }

   if (strstr(VBUFF,"trying"))
      {
      printf("cfengine: Aborted because MountFileSystems() went into a retry loop.\n");
      FatalError("RPC hung. Filesystem probably not exported/shared by server.");
      }
   }

pclose(pp);
}

/*******************************************************************/

GetMountInfo ()  /* This is, in fact, the most portable way to read the mount info! */
                 /* Depressing, isn't it? */
{ FILE *pp;
  struct Item *mp;
  char buf1[bufsize],buf2[bufsize],buf3[bufsize];
  char host[maxvarsize], mounton[bufsize];
  int i;

Banner("cfengine: Building list of currently mounted filesystems");

/* sscanf(VMOUNTCOMM[VSYSTEMHARDCLASS],"%s",buf1); */

/* Old BSD scanf crashes here! Why!? workaround: */

for (i=0; VMOUNTCOMM[VSYSTEMHARDCLASS][i] != ' '; i++)
   {
   buf1[i] =  VMOUNTCOMM[VSYSTEMHARDCLASS][i];
   }
buf1[i] = '\0';


if ((pp = popen(buf1,"r")) == NULL)
   {
   printf ("cfengine: Can't open %s\n",buf1);
   exit (1);
   }

do
   {
   VBUFF[0] = buf1[0] = buf2[0] = buf3[0] = '\0';

   fgets(VBUFF,bufsize,pp);
   sscanf(VBUFF,"%s%s%s",buf1,buf2,buf3);

   if (VBUFF[0] == '\n')
      {
      break;
      }

   if (strstr(VBUFF,"be root"))
      {
      printf("cfengine: Mount access is denied. You must be root.\n");
      printf("          Use the -n option to run safely.");
      }

   if (strstr(VBUFF,"retrying") || strstr(VBUFF,"denied") || strstr(VBUFF,"backgrounding"))
      {
      continue;
      }

   if (strstr(VBUFF,"exceeded") || strstr(VBUFF,"busy"))
      {
      continue;
      }

   if (strstr(VBUFF,"RPC"))
      {
      if (! SILENT)
         {
         printf("cfengine: There was an RPC timeout. Aborting mount operations.\n");
         printf("          Session failed while trying to talk to remote host\n");
         printf("%s\n",VBUFF);
         }
      GOTMOUNTINFO = false;
      pclose(pp);
      return;
      }

   switch (VSYSTEMHARDCLASS)
      {
      case sun4:
      case sun3:
      case ultrx: 
      case irix:
      case irix4:
      case irix64:
      case linuxx:
      case freebsd:
      case netbsd:
      case bsd_i:
      case nextstep:
      case bsd4_3:
      case newsos:
      case aos:
      case osf:     
                    if (buf1[0] == '/')
                       {
                       strcpy(host,VDEFAULTBINSERVER.name);
                       strcpy(mounton,buf3);
                       }
                    else
                       {
                       sscanf(buf1,"%[^:]",host);
                       strcpy(mounton,buf3);
                       }

                    break;
      case solaris:
      case solarisx86:
      case hp10:
      case hp:      
                    if (buf3[0] == '/')
                       {
                       strcpy(host,VDEFAULTBINSERVER.name);
                       strcpy(mounton,buf1);
                       }
                    else
                       {
                       sscanf(buf3,"%[^:]",host);
                       strcpy(mounton,buf1);
                       }

                    break;
      case aix:
                   /* skip header */

                    if (buf1[0] == '/')
                       {
                       strcpy(host,VDEFAULTBINSERVER.name);
                       strcpy(mounton,buf2);
                       }
                    else
                       {
                       strcpy(host,buf1);
                       strcpy(mounton,buf3);
                       }
                    break;

      case unused1:
      case unused2:
      case unused3:
                    break;

      default:
                    printf("Error: case %d = %s\n",VSYSTEMHARDCLASS,CLASSTEXT[VSYSTEMHARDCLASS]);
                    FatalError("System error in GetMountInfo - no such class!");
      }

   if (DEBUG || D2)
      {
      printf("  %s:%s\n",host,mounton);
      }

   InstallMountedItem(host,mounton);
   }

while (!feof(pp));

pclose(pp);
}

/*******************************************************************/

MountHomeBinServers()

{ struct Item *mp;
  char host[maxvarsize];
  char mountdir[bufsize];
  char maketo[bufsize];

if (! GOTMOUNTINFO)
   {
   printf("cfengine: Incomplete mount info due to RPC failure.\n");
   printf("          %s will not be modified on this pass!\n\n",VFSTAB[VSYSTEMHARDCLASS]);
   return;
   }

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can mount filesystems.\n");
   return;
   }

Banner("cfengine: Checking home and binservers");

for (mp = VMOUNTABLES; mp != NULL; mp=mp->next)
   {
   sscanf(mp->name,"%[^:]:%s",host,mountdir);

   strcpy(maketo,mountdir);

   if (maketo[strlen(maketo)-1] == '/')
      {
      strcat(maketo,".");
      }
   else
      {
      strcat(maketo,"/.");
      }

   if (strcmp(host,VDEFAULTBINSERVER.name) == 0) /* A host never mounts itself nfs */
      {
      continue;
      }

   Debug2(">>checking to mount %s on %s\n",mp->name,host);

   if (IsHomeDir(mountdir))
      {
      if (!IsItemIn(VMOUNTED,mp->name) && IsItemIn(VHOMESERVERS,host))
         {
         MakeDirectoriesFor(maketo);
         AddToFstab(host,mountdir,mountdir,"rw");
         }
      }
   else
      {
      if (!IsItemIn(VMOUNTED,mp->name) && IsItemIn(VBINSERVERS,host))
         {
         MakeDirectoriesFor(maketo);
         AddToFstab(host,mountdir,mountdir,"rw");
         }
      else
         {
         Debug2("%s is not a home or bin item, skipping...\n",mp->name);
         }
      }
   }
}


/*********************************************************************/

MountMisc()

{ struct MiscMount *mp;
  char host[maxvarsize];
  char mountdir[bufsize];
  char maketo[bufsize];
  char mtpt[bufsize];

if (! GOTMOUNTINFO)
   {
   printf("cfengine: Incomplete mount info due to RPC failure.\n");
   printf("          %s will not be modified on this pass!\n\n",VFSTAB[VSYSTEMHARDCLASS]);
   return;
   }

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can mount filesystems.\n");
   return;
   }

Banner("cfengine: Checking miscellaneous mountables:");

for (mp = VMISCMOUNT; mp != NULL; mp=mp->next)
   {
   sscanf(mp->from,"%[^:]:%s",host,mountdir);

   strcpy(maketo,mp->onto);

   if (maketo[strlen(maketo)-1] == '/')
      {
      strcat(maketo,".");
      }
   else
      {
      strcat(maketo,"/.");
      }

   if (strcmp(host,VDEFAULTBINSERVER.name) == 0) /* A host never mounts itself nfs */
      {
      continue;
      }

   sprintf(mtpt,"%s:%s",host,mp->onto);

   if (!IsItemIn(VMOUNTED,mtpt))
      {
      MakeDirectoriesFor(maketo);
      AddToFstab(host,mountdir,mp->onto,mp->options);
      }
   }
}

/*********************************************************************/

Unmount()

{ struct Item *ptr;
  char comm[bufsize];
  char fs[bufsize];
  struct Item *filelist, *item, *search;
  struct stat statbuf;
  FILE *pp;

Banner("cfengine: checking unmounts");

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can unmount filesystems.\n");
   return;
   }

filelist = NULL;

if (! LoadItemList(&filelist,VFSTAB[VSYSTEMHARDCLASS]))
   {
   printf("cfengine: Error -- couldn't open %s!\n",VFSTAB[VSYSTEMHARDCLASS]);
   return;
   }

NUMBEROFEDITS = 0;

for (ptr=VUNMOUNT; ptr != NULL; ptr=ptr->next)
   {
   if (IsExcluded(ptr->classes))
      {
      continue;
      }

   sscanf(ptr->name,"%*[^:]:%s",fs);

   Verbose("Unmount filesystem %s on %s\n",fs,ptr->name);

   if (strcmp(fs,"/") == 0 || strcmp(fs,"/usr") == 0)
      {
      printf("cfengine: Request to unmount / or /usr is refused!\n");
      continue;
      }

   if (IsItemIn(VMOUNTED,ptr->name) && (! DONTDO))
      {
      sprintf(comm,"%s %s",VUNMOUNTCOMM[VSYSTEMHARDCLASS],fs);

      if ((pp = popen(comm,"r")) == NULL)
         {
         printf("cfengine: failed to open pipe from %s\n",VUNMOUNTCOMM[VSYSTEMHARDCLASS]);
         return;
         }

      fgets(VBUFF,bufsize,pp);

      if (strstr(VBUFF,"busy") || strstr(VBUFF,"Busy"))
         {
         if (!SILENT)
            {
            printf("cfengine: umount warned that the device under %s\n",ptr->name);
            printf("          was busy. Cannot unmount that device.\n");
            }
         }

      pclose(pp);
      }

   if (stat(fs,&statbuf) != -1)
      {
      if ( ! S_ISDIR(statbuf.st_mode))
         {
         if ( !SILENT )
            {
            printf("cfengine: Warning! %s was not a directory.\n",fs);
            printf("          (Unmount) will not delete this!\n");
            }
         KillOldLink(fs);
         }
      else if (! DONTDO)
         {
         if (rmdir(fs) == -1)
            {
            printf("cfengine: unable to remove the directory %s\n",fs);
            perror("        ");
            } 
         else
            {
            Verbose("cfengine: removing directory %s\n",ptr->name);
            }
         }
      }

   if (VSYSTEMHARDCLASS == aix)
      {
      strcpy (VBUFF,fs);
      strcat (VBUFF,":");

      item = LocateNextItemContaining(filelist,VBUFF);

      if (item->next == NULL)
         {
         printf("cfengine: bad format in %s\n",VFSTAB[aix]);
         }

      DeleteItem(&filelist,item->next);

      while (strstr(item->next->name,"="))
         {
         DeleteItem(&filelist,item->next);    /* DeleteItem(NULL) is harmless */
         }
      }
   else
      {
      strcpy (VBUFF,ptr->name);

      if (VSYSTEMHARDCLASS == ultrx)   /* ensure name is no just a substring */
         {
         strcat (VBUFF,":");
         }
      else
         {
         strcat (VBUFF," ");
         }

      DeleteItemContaining(&filelist,VBUFF);
      }
   }


if ((! DONTDO) && (NUMBEROFEDITS > 0))
   {
   SaveItemList(filelist,VFSTAB[VSYSTEMHARDCLASS]);
   }

DeleteItemList(filelist);
}

/*********************************************************************/

CheckRequired()

{ struct Item *rp;
  struct Item *ip;
  int matched,varstring,missing = 0;


Banner("cfengine: checking required filesystems");

for (rp = VREQUIRED; rp != NULL; rp = rp->next)
   {
   if (IsExcluded(rp->classes))
      {
      continue;
      }

   matched = varstring = false;

   for( ip = VBINSERVERS; ip != NULL && (!matched); ip = ip->next)
      {
      CURRENTPATH[0] = CURRENTITEM[0] = '\0';

      varstring = ExpandVarbinserv(rp->name,CURRENTPATH,ip->name);

      if (RequiredFileSystemOkay(CURRENTPATH))  /* simple or reduced item */
         {
         Verbose("Filesystem %s seems okay\n",CURRENTPATH);
         matched = true;
         }
      else if (! varstring)
         {
         printf("cfengine: The file %s does not exist.\n\n",CURRENTPATH);
         }

      if (! varstring)                       /* don't iterate over binservers if not var */
         {
         break;
         }
      }

   if (matched == false && ip == NULL)
      {
      printf("cfengine: didn't find any file to match the required filesystem %s\n",rp->name);
      missing++;
      }
   }

if (missing)

   { time_t tloc;;

   if ((tloc = time((time_t *)NULL)) == -1)
      {
      printf("Couldn't read system clock\n");
      }
   printf("\n!! MESSAGE !! from cfengine at %s\n\n", ctime(&tloc));
   printf("There are %d required file(system)s missing on host <%s>\n",missing,VDEFAULTBINSERVER.name);
   printf("even after all mounts have been attempted.\n");
   printf("This may be caused by a failure to mount a network filesystem (check exports)\n");
   printf("or because no valid server was specified in the program %s\n\n",VINPUTFILE);
   }
}

/*******************************************************************/

MailCheck()

{ char mailserver[bufsize];
  char mailhost[maxvarsize];
  char rmailpath[maxvarsize];
  char lmailpath[maxvarsize];


if (VMAILSERVER[0] == '\0')
   {
   FatalError("Program does not define a mailserver for this host");
   }

Banner("cfengine: checking mail spool directory");

if (getuid() != 0)                            
   {
   printf("cfengine: Only root alter the mail configuration.\n");
   return;
   }

sscanf (VMAILSERVER,"%[^:]:%s",mailhost,rmailpath);

if (VMAILSERVER[0] == '\0')
   {
   printf("\ncfengine: WARNING: Host has no defined mailserver!\n");
   return;
   }

if (strcmp(VDEFAULTBINSERVER.name,mailhost) == 0) /* Is this the mailserver ?*/
   {
   return;
   }

sprintf(lmailpath,"%s:%s",mailhost,VMAILDIR[VSYSTEMHARDCLASS]);


if (IsItemIn(VMOUNTED,lmailpath))                             /* Remote file system mounted on */
   {                                                          /* local mail dir - correct      */
   Verbose("Mail spool area looks ok\n");
   return;
   }

strcpy(mailserver,VMAILDIR[VSYSTEMHARDCLASS]);
AddSlash(mailserver);
strcat(mailserver,".");

MakeDirectoriesFor(mailserver);                                  /* Check directory is in place */

if (IsItemIn(VMOUNTED,VMAILSERVER))
   {
   if (!SILENT)
      {
      Verbose("cfengine: Warning - the mail directory seems to be mounted as on\n");
      Verbose("          the remote mailserver and not on the correct local directory\n");
      Verbose("          Should strictly mount on %s\n",VMAILDIR[VSYSTEMHARDCLASS]);
      }
   return;
   }

if (MatchStringInFstab("mail"))
   {
   if (!SILENT)
      {
      Verbose("cfengine: Warning - the mail directory seems to be mounted\n");
      Verbose("          in a funny way. I can find the string <mail> in %s\n",VFSTAB[VSYSTEMHARDCLASS]);
      Verbose("          but nothing is mounted on %s\n\n",VMAILDIR[VSYSTEMHARDCLASS]);
      }
   return;
   }

printf("\ncfengine: Trying to mount %s\n",VMAILSERVER);


if (! DONTDO)
   {
   AddToFstab(mailhost,rmailpath,VMAILDIR[VSYSTEMHARDCLASS],"rw");
   }
else
   {
   printf("cfengine: Need to mount %s:%s on %s\n",mailhost,rmailpath,mailserver);
   }
}

/*******************************************************************/

TidyFiles()

   /* Here we start by scanning for any absolute path wildcards */
   /* After that's finished, we go snooping around the homedirs */

{ char basename[bufsize],pathbuff[bufsize];
  struct TidyPattern *tlp;
  struct Tidy *tp;
  struct Item *ip1,*ip2;
  struct stat statbuf;
  int homesearch = 0;
  int TidyWrapper();
  int RecHomeTidyWrapper();

Banner("cfengine: tidying by path");

for (tp = VTIDY; tp != NULL; tp=tp->next)
   {
   if (strncmp(tp->path,"home",4)==0)
      {
      homesearch = 1;
      continue;
      }

   for (tlp = tp->tidylist; tlp != NULL; tlp=tlp->next)
      {
      strcpy(VBUFF,tp->path);
      AddSlash(VBUFF);
      strcat(VBUFF,tlp->pattern);

      if (stat(VBUFF,&statbuf) != -1)   /* not lstat - could confuse user */
         {
         if (S_ISDIR(statbuf.st_mode))
            {
	    if (! tlp->rmdirs)          /* Are we allowed to rm empty dirs? */
	       {
               Verbose("cfengine: will not delete directories (matching %s)!\n",tlp->pattern);
	       Verbose("          Applies to %s\n",VBUFF);
               DeleteTidyList(tp->tidylist);
	       tp->tidylist = NULL;
               continue;
	       }
            }
         }
      }

   basename[0] = '\0';

   ExpandWildCardsAndDo(tp->path,basename,TidyWrapper,tp);
   
   DeleteTidyList(tp->tidylist);
   tp->tidylist = NULL;
   }


Debug2("End PATHTIDY:\n");

if (!homesearch)                           /* If there are "home" wildcards */
   {                                 /* Don't go rummaging around the disks */
   return;
   }

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can delete others' files.\n");
   return;
   }

if (VERBOSE || DEBUG || D2) 
   {
   printf("cfengine: tidying home directories\n");                  /* homedirs */
   }

if (!MountPathDefined())
   {
   return;
   }

for (ip1 = VHOMEPATLIST; ip1 != NULL; ip1=ip1->next)
   {
   for (ip2 = VMOUNTLIST; ip2 != NULL; ip2=ip2->next)
      {
      if (IsExcluded(ip2->classes))
	 {
	 continue;
	 }
      pathbuff[0]='\0';
      basename[0]='\0';
      strcpy(pathbuff,ip2->name);
      AddSlash(pathbuff);
      strcat(pathbuff,ip1->name);

      ExpandWildCardsAndDo(pathbuff,basename,RecHomeTidyWrapper,NULL);
      }
   }

Verbose("cfengine: Done with home directories\n");
}

/*******************************************************************/

GetSetuidLog()

{ struct Item *filetop = NULL;
  struct Item *ip;
  FILE *fp;
  char *sp;

if (getuid() != 0)                     /* Ignore this if not root */
   {
   return;
   }

if ((fp = fopen(VSETUIDLOG,"r")) == NULL)
   {
   }
else
   {
   while (!feof(fp))
      {
      VBUFF[0] = '\0';
      fgets (VBUFF,bufsize,fp);
      VBUFF[strlen(VBUFF)-1] = '\0';        /* chop */

      if (strlen(VBUFF) == 0)
         {
         continue;
         }

      if ((ip = (struct Item *)malloc (sizeof(struct Item))) == NULL)
         {
         perror("malloc");
         FatalError("GetSetuidList() couldn't allocate memory #1");
         }

      if ((sp = malloc (strlen(VBUFF)+2)) == NULL)
         {
         perror("malloc");
         FatalError("GetSetuidList() couldn't allocate memory #2");
         }

      if (filetop == NULL)
         {
         VSETUIDLIST = filetop = ip;
         }
      else
         {
         filetop->next = ip;
         }

      if (DEBUG || D2)
         {
         printf("SETUID-LOG: %s\n",VBUFF);
         }

      strcpy(sp,VBUFF);
      ip->name = sp;
      ip->next = NULL;
      filetop = ip;
      }

   fclose(fp);
   }

}

/*******************************************************************/

CheckFiles()                         /* Check through file systems */

{ struct File *ptr;
  int CheckFileWrapper();
  char buffer[bufsize];
  short savetravlinks = TRAVLINKS;
  short savekilloldlinks = KILLOLDLINKS;

Banner("cfengine: checking files");

if (TRAVLINKS && (VERBOSE || DEBUG || D2))
   {
   printf("(Default in switched to purge stale links...)\n");
   }

for (ptr = VFILE; ptr != NULL; ptr=ptr->next)
   {
   if (IsExcluded(ptr->classes))
      {
      continue;
      }

   TRAVLINKS = savetravlinks;

   if (ptr->travlinks == 'T')
      {
      TRAVLINKS = true;
      }

   else if (ptr->travlinks == 'F')
      {
      TRAVLINKS = false;
      }

   else if (ptr->travlinks == 'K')
      {
      KILLOLDLINKS = true;
      }

   if (KILLOLDLINKS && (VERBOSE || DEBUG || D2))
      {
      printf("Link purging BEGIN: ");
      }
   else
      {
      Verbose("Link purging OFF: ");
      }

   if (strncmp(ptr->path,"home",4) == 0)
      {
      CheckHome(ptr);
      continue;
      }

   Verbose("Checking file(s) in %s\n",ptr->path);

   buffer[0] = '\0';
   ExpandWildCardsAndDo(ptr->path,buffer,CheckFileWrapper,ptr);

   TRAVLINKS = savetravlinks;
   KILLOLDLINKS = savekilloldlinks;
   }
}

/*******************************************************************/

SaveSetuidLog()

{ FILE *fp;
  struct Item *ip;


if (getuid() != 0)                     /* Ignore this if not root */
   {
   return;
   }

if (! DONTDO)
   {
   if ((fp = fopen(VSETUIDLOG,"w")) == NULL)
      {
      printf("Can't open %s for writing\n",VSETUIDLOG);
      perror("fopen");
      return;
      }

   Verbose("cfengine: saving the setuid log in %s\n",VSETUIDLOG);

   for (ip = VSETUIDLIST; ip != NULL; ip=ip->next)
      {
      if (!isspace(*(ip->name)) && strlen(ip->name) != 0)
         {                         
         fprintf(fp,"%s\n",ip->name);
         if (DEBUG || D2)
            {
            printf("SAVE-SETUID-LOG: %s\n",ip->name);
            }
         }
      }

   fclose(fp);
   chmod(VSETUIDLOG,0600);
   }
}

/*******************************************************************/

DisableFiles()

{ struct Disable *dp;
  struct stat statbuf;

Banner("cfengine: checking files to disable");

for (dp = VDISABLELIST; dp != NULL; dp=dp->next)
   {
   if (IsExcluded(dp->classes))
      {
      continue;
      }

   if (lstat(dp->name,&statbuf) == -1)
      {
      Verbose("Filetype %s, %s is not there - ok\n",dp->type,dp->name);
      continue;
      }

   Verbose("Disable checking %s\n",dp->name);

   if (S_ISDIR(statbuf.st_mode))
      {
      printf("cfengine: Warning %s is a directory.\n",dp->name);
      printf("          I refuse to rename/delete a directory!\n\n");
      continue;
      }

   if (S_ISLNK(statbuf.st_mode))
      {
      if (strcmp(dp->type,"file") == 0)
         {
         Verbose("cfengine: %s is a link, not disabling\n", dp->name);
         continue;
         }

      if (readlink(dp->name,VBUFF,bufsize) == -1)
         {
         printf("cfengine: DisableFiles() can't read link %s\n",dp->name);
         perror("Readlink in disableFiles(): ");
         continue;
         }

      printf("cfengine: Deleting link %s -> %s\n",dp->name,VBUFF);

      if (! DONTDO)
         {
         if (unlink(dp->name) == -1)
            {
            printf("cfengine: Error while unlinking %s\n",dp->name);
            perror("unlink ");
            continue;
            }
         }
      }
   else
      {
      if (! S_ISREG(statbuf.st_mode))
         {
         printf("cfengine: %s is not a plain file - won't disable\n",dp->name);
         continue;
         }

      if (strcmp(dp->type,"link") == 0)
         {
         Verbose("cfengine: %s is a file, not disabling\n", dp->name);
         continue;
         }

      if (dp->rotate == 0)
         {
         strcpy(CURRENTPATH,dp->name);
         strcat(CURRENTPATH,".cf-disabled");

         printf("Disabling file %s\n",dp->name);

	 chmod(dp->name, (mode_t)0400);

         if (! DONTDO)
            {
            if (rename(dp->name,CURRENTPATH) == -1)
               {
               printf("cfengine: Error occurred while renaming %s\n",dp->name);
               perror("rename ");
               continue;
	       }

	    if (Repository(CURRENTPATH))
	       {
	       unlink(CURRENTPATH);
	       }
            }
         }
      else if (dp->rotate == CF_TRUNCATE)
	 {
         Verbose("cfengine: truncating (emptying) %s\n",dp->name);
	 
	 if (! DONTDO)
	    {
            TruncateFile(dp->name);
	    }
	 }
      else
	 {
	 Verbose("cfengine: rotating files %s by %d\n",dp->name,dp->rotate);

	 if (!DONTDO)
	    {
	    RotateFiles(dp->name,dp->rotate);
	    }
	 }
      }
   }
}

/*******************************************************************/

Scripts()

{ struct Item *ptr;
  char line[bufsize];
  char comm[20], *sp;
  int print;
  FILE *pp;

Banner("cfengine: Running shell commands");

for (ptr = VSCRIPT; ptr != NULL; ptr=ptr->next)
   {
   if (IsExcluded(ptr->classes))
      {
      continue;
      }

   Verbose("\ncfengine: executing script %s...\n",ptr->name);

   if (DONTDO)
      {
      printf("cfengine: execute script %s\n",ptr->name);
      }
   else
      {
      for (sp = ptr->name; *sp != ' ' && *sp != '\0'; sp++)
	 {
	 }

      if (sp - 10 >= ptr->name)
	 {
	 sp -= 10;   /* copy 15 most relevant characters of command */
	 }
      else
	 {
	 sp = ptr->name;
	 }

      bzero(comm,20);

      strncpy(comm,sp,15);

      if ((pp = popen(ptr->name,"r")) == NULL)
         {
	 printf("cfengine (%s): Couldn't open pipe to command %s\n",VSYSNAME.nodename,ptr->name);
	 perror("popen");
	 continue;
	 }

      while (!feof(pp))
	 {
	 bzero(line,bufsize);
	 fgets(line,bufsize,pp);
         Chop(line);
	 print = false;
	 
	 for (sp = line; *sp != '\0'; sp++)
	    {
	    if (! isspace(*sp))
	       {
	       print = true;
	       break;
	       }
	    }
	 
	 if (print)
	    {
	    printf("cf:%10s:%s: %s\n",VUQNAME,comm,line);
	    }
	 }
      
      pclose(pp);
      }
   }
}

/*******************************************************************/

EditFiles()

{ struct Edit *ptr;

Banner("cfengine: editing files");

for (ptr=VEDITLIST; ptr!=NULL; ptr=ptr->next)
   {
   if (strncmp(ptr->fname,"home",4) == 0)
      {
      DoEditHomeFiles(ptr);
      }
   else
      {
      DoEditFile(ptr,ptr->fname);
      }
   }

EDITVERBOSE = false;
}

/*******************************************************************/

MakeImages()

{ struct Image *ip;
  struct stat statbuf;

Banner("cfengine: Checking file images:");

for (ip = VIMAGE; ip != NULL; ip=ip->next)
   {
   if (IsExcluded(ip->classes))
      {
      continue;
      }

   IMAGEBACKUP = ip->backup;

   if (stat(ip->path,&statbuf) == -1)
      {
      printf("cfengine: hey! can't stat %s in copy\n",ip->path);
      continue;
      }

   if (strncmp(ip->destination,"home",4) == 0)
      {
      HOMECOPY = true;          /* Don't send home backups to repository */
      CheckHomeImages(ip);
      HOMECOPY = false;
      }
   else
      {
      if (S_ISDIR(statbuf.st_mode))
	 {
         RecursiveImage(ip,ip->path,ip->destination,ip->recurse);
	 }
      else
	 {
	 if (! MakeDirectoriesFor(ip->destination))
            {
            return;
            }

	 CheckImage(ip->path,ip->destination,ip);
	 }
      }
   }
}

/*******************************************************************/

PreNetConfig()                           /* Execute a shell script */

{ struct stat buf;
  char comm[bufsize];
  char *sp;

if (stat(VPRECONFIG,&buf) == -1)
   {
   return;
   }

if (NOPRECONFIG)
   {
   Verbose("Ignoring the cf.preconf file: option set\n");
   return;
   }

if (S_ISDIR(buf.st_mode) || S_ISCHR(buf.st_mode) || S_ISBLK(buf.st_mode))
   {
   printf("cfengine: Error: %s was not a regular file\n",VPRECONFIG);
   FatalError("Aborting.");
   }

Verbose("\n\nExecuting Net Preconfiguration script...%s\n\n",VPRECONFIG);

if ((sp=getenv(CFINPUTSVAR)) != NULL)
   {
   sprintf(comm,"%s/%s %s",sp,VPRECONFIG,CLASSTEXT[VSYSTEMHARDCLASS]);
   }
else
   {
   sprintf(comm,"./%s %s",VPRECONFIG,CLASSTEXT[VSYSTEMHARDCLASS]);
   }

system (comm);
}

/*******************************************************************/

CheckResolv()

{ struct Item *filebase = NULL;
  struct Item *ip;
  FILE *fp;
  char ch;


if (VDOMAIN == NULL)
   {
   printf("cfengine: Domain name not specified. Can't configure resolver\n");
   return;
   }

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can configure the resolver.\n");
   return;
   }

if ((fp = fopen(VRESOLVCONF[VSYSTEMHARDCLASS],"r")) == NULL)
   {
   }
else
   {
   while (!feof(fp))
      {
      fgets (VBUFF,bufsize,fp);

      if(VBUFF[strlen(VBUFF)-1] == '\n')
         {
         VBUFF[strlen(VBUFF)-1] = '\0';        /* chop */
         }

      AppendItem(&filebase,VBUFF,NULL);

      VBUFF[0] = '\0';
      }

   fclose(fp);
   }

for (ip = filebase; ip != NULL; ip=ip->next)
   {
   if (strlen(ip->name) == 0)
      {
      continue;
      }

   ch = *(ip->name+strlen(ip->name)-2);

   if (isspace(ch))
      {
      printf("cfengine: deleted line %s ended with a space in %s.\n",ip->name,VRESOLVCONF[VSYSTEMHARDCLASS]);
      printf("          The resolver doesn't understand this.\n");
      DeleteItem(&filebase,ip);
      }
   else if (isspace(*(ip->name)))
      {
      printf("cfengine: deleted line %s started with a space in %s.\n",ip->name,VRESOLVCONF[VSYSTEMHARDCLASS]);
      printf("          The resolver doesn't understand this.\n");
      DeleteItem(&filebase,ip);
      }
   }

DeleteItemStarting(&filebase,"domain");
EditItemsInResolvConf(VRESOLVE,&filebase);
sprintf(VBUFF,"domain %s",VDOMAIN);
PrependItem(&filebase,VBUFF,NULL);

if (DONTDO)
   {
   if (VERBOSE || DEBUG || D2)
      {
      printf("cfengine: editing %s\n",VRESOLVCONF[VSYSTEMHARDCLASS]);
  
      for (ip = filebase; ip != NULL; ip=ip->next)
         {
         if (!isspace(*(ip->name)) && strlen(ip->name) != 0) /* Don't print lines with blanks */
            {         
            printf("cfengine: %s : %s\n",VRESOLVCONF[VSYSTEMHARDCLASS],ip->name);
            }
         }
      }
   }
else
   {
   if ((fp = fopen(VRESOLVCONF[VSYSTEMHARDCLASS],"w")) == NULL)
      {
      printf("Can't open %s for writing\n",VRESOLVCONF[VSYSTEMHARDCLASS]);
      perror("fopen");
      return;
      }

   Verbose("cfengine: editing %s\n",VRESOLVCONF[VSYSTEMHARDCLASS]);

   for (ip = filebase; ip != NULL; ip=ip->next)
      {
      if (!isspace(*(ip->name)) && strlen(ip->name) != 0) /* Don't print lines with blanks */
         {                                                /* The resolver ignores them anyway */
         fprintf(fp,"%s\n",ip->name);
         }
      }

   fclose(fp);
   chmod(VRESOLVCONF[VSYSTEMHARDCLASS],DEFAULTSYSTEMMODE);
   }
}


/*******************************************************************/
/* Level 2                                                         */
/*******************************************************************/

IsAbsoluteFileName(f)

char *f;

{
if (*f == '/' || *f == '.')
   {
   return true;
   }

return false;
}

/*******************************************************************/

GetResource(var)

char *var;

{ int i;

for (i = 0; VRESOURCES[i] != '\0'; i++)
   {
   if (strcmp(VRESOURCES[i],var)==0)
      {
      return i;
      }
   }

sprintf (VBUFF,"Unknown resource %s in %s",var,VRCFILE);

FatalError(VBUFF);
}


/*******************************************************************/

CheckOpts(argc,argv)

char **argv;
int argc;

{ extern char *optarg;
  int optindex = 0;
  int c;

while ((c=getopt_long(argc,argv,"khHd:vlnif:pPmcCtsSaeEVD:N:LwxXuU",OPTIONS,&optindex)) != EOF)
  {
  switch ((char) c)
      {
      case 'E': printf("cfengine: the enforce-links option can only be used in interactive mode.\n");
                printf("Do you really want to blindly enforce ALL links? (y/n)\n");
                if (getchar() != 'y')
                   {
                   printf("cfengine: aborting\n");
                   exit(1);
                   }

                ENFORCELINKS = true;
                break;

      case 'f': strcpy(VINPUTFILE,optarg);
                MINUSF = true;
                break;

      case 'd': 

                switch (*optarg)
                   {
                   case '1': D1 = true;
                             break;
                   case '2': D2 = true;
                             break;
                   default:  DEBUG = true;
                             break;
                   }
                break;

      case 'D': AddCompoundClass(optarg);
                break;

      case 'N': NegateCompoundClass(optarg,&VNEGHEAP);
                break;

      case 'e': NOEDITS = true;
                break;

      case 'i': IFCONF = false;
                break;

      case 'v': VERBOSE = true;
                break;

      case 'l': TRAVLINKS = true;
                break;

      case 'n': DONTDO = true;
                break;

      case 'p': PARSEONLY = true;
                IGNORELOCK = true;
                break;

      case 'm': NOMOUNTS = true;
                break;

      case 'c': NOFILECHECK = true;
                break;

      case 'C': MOUNTCHECK = true;
                break;

      case 't': NOTIDY = true;
                break;

      case 's': NOSCRIPTS = true;
                break;

      case 'a': PRSYSADM = true;
                IGNORELOCK = true;
                break;

      case 'L': KILLOLDLINKS = true;
                break;

      case 'V': printf("GNU %s\n%s\n",VERSION,COPYRIGHT);
	        printf("This program is covered by the GNU Public License and may be\n");
		printf("copied free of charge. No warrenty is implied.\n\n");
                exit(0);

      case 'h': Syntax();
                exit(0);

      case 'x': NOPRECONFIG = true;
                break;

      case 'w': WARNINGS = false;
                break;

      case 'X': NOLINKS = true;
                break;

      case 'k': NOCOPY = true;
                break;

      case 'S': SILENT = true;
                break;

      case 'u': USEENVIRON = true;
                break;

      case 'U': UNDERSCORE_CLASSES = true;
	        break;

      case 'H': NOHARDCLASSES = true;
	        break;

      case 'P': NOPROCS = true;
	        break;

      default:  Syntax();
                exit(1);

      }
   }
}

/*******************************************************************/

ListDefinedClasses()

{ struct Item *ptr;

printf ("\nDefined Classes = ( ");

for (ptr = VHEAP; ptr != NULL; ptr=ptr->next)
   {
   printf("%s ",ptr->name);
   }

printf (")\n");

printf ("\nNegated Classes = ( ");

for (ptr = VNEGHEAP; ptr != NULL; ptr=ptr->next)
   {
   printf("%s ",ptr->name);
   }

printf (")\n\n");

printf ("Installable classes = ( ");

for (ptr = VALLADDCLASSES; ptr != NULL; ptr=ptr->next)
   {
   printf("%s ",ptr->name);
   }

printf (")\n");

if (VEXCLUDECOPY != NULL)
   {
   printf("Patterns to exclude from copies: = (");
   
   for (ptr = VEXCLUDECOPY; ptr != NULL; ptr=ptr->next)
      {
      printf("%s ",ptr->name);
      }

   printf (")\n");
   }

if (VEXCLUDELINK != NULL)
   {
   printf("Patterns to exclude from links: = (");
   
   for (ptr = VEXCLUDELINK; ptr != NULL; ptr=ptr->next)
      {
      printf("%s ",ptr->name);
      }

   printf (")\n");
   }

if (VCOPYLINKS != NULL)
   {
   printf("Patterns to copy instead of link: = (");
   
   for (ptr = VCOPYLINKS; ptr != NULL; ptr=ptr->next)
      {
      printf("%s ",ptr->name);
      }

   printf (")\n");
   }

if (VCOPYLINKS != NULL)
   {
   printf("Patterns to link instead of copy: = (");
   
   for (ptr = VLINKCOPIES; ptr != NULL; ptr=ptr->next)
      {
      printf("%s ",ptr->name);
      }

   printf (")\n");
   }

}

/*********************************************************************/

ListDefinedHomePatterns()

{ struct Item *ptr;


printf ("\nDefined wildcards to match home directories = ( ");

for (ptr = VHOMEPATLIST; ptr != NULL; ptr=ptr->next)
   {
   printf("%s ",ptr->name);
   }

printf (")\n");
}

/*********************************************************************/

ListDefinedBinservers()

{ struct Item *ptr;

printf ("\nDefined Binservers = ( ");

for (ptr = VBINSERVERS; ptr != NULL; ptr=ptr->next)
   {
   printf("%s ",ptr->name);
   }

printf (")\n");
}

/*********************************************************************/

ListDefinedLinks()

{ struct Link *ptr;
  struct Item *ip;
  
printf ("\nDEFINED LINKS\n\n");

for (ptr = VLINK; ptr != NULL; ptr=ptr->next)
   {
   printf("From %s -> %s force=%d, attr=%d type=%c\n",ptr->from,ptr->to,ptr->force,ptr->silent,ptr->type);
   for (ip = ptr->copy; ip != NULL; ip = ip->next)
      {
      printf(" Copy %s\n",ip->name);
      }

   for (ip = ptr->exclusions; ip != NULL; ip = ip->next)
      {
      printf(" Exclude %s\n",ip->name);
      }

   for (ip = ptr->inclusions; ip != NULL; ip = ip->next)
      {
      printf(" Include %s\n",ip->name);
      }
   }
}

/*********************************************************************/

ListDefinedLinkchs()

{ struct Link *ptr;
  struct Item *ip;

printf ("\nDEFINED CHILD LINKS\n\n");

for (ptr = VCHLINK; ptr != NULL; ptr=ptr->next)
   {
   printf("[%s->%s] force=%d attr=%d, rec=%d\n",ptr->from,ptr->to,
	  ptr->force,ptr->silent,ptr->recurse);
   
   for (ip = ptr->copy; ip != NULL; ip = ip->next)
      {
      printf(" Copy %s\n",ip->name);
      }

   for (ip = ptr->exclusions; ip != NULL; ip = ip->next)
      {
      printf(" Exclude %s\n",ip->name);
      }
   
   for (ip = ptr->inclusions; ip != NULL; ip = ip->next)
      {
      printf(" Include %s\n",ip->name);
      }
   }
}

/*********************************************************************/

ListDefinedResolvers()

{ struct Item *ptr;

printf ("\nDEFINED NAMESERVERS\n\n");

for (ptr = VRESOLVE; ptr != NULL; ptr=ptr->next)
   {
   printf("%s\n",ptr->name);
   }

printf (")\n");
}

/*********************************************************************/

ListDefinedScripts()

{ struct Item *ptr;

printf ("\nDEFINED SHELLCOMMANDS\n\n");

for (ptr = VSCRIPT; ptr != NULL; ptr=ptr->next)
   {
   printf("%s\n",ptr->name);
   }
}

/*********************************************************************/

ListDefinedImages()

{ struct Image *ptr;
  struct UidList *up;
  struct GidList *gp;
  struct Item *iip;
  
printf ("\nDEFINED FILE IMAGES\n\n");

for (ptr = VIMAGE; ptr != NULL; ptr=ptr->next)
   {
   printf("\n%s\n +%o\n -%o\n dest: %s\n action: %s\n",ptr->path,ptr->plus,ptr->minus,
	  ptr->destination,ptr->action);
  
   if (ptr->recurse == INFINITERECURSE)
      {
      printf(" recurse=inf\n");
      }
   else
      {
      printf(" recurse=%d\n",ptr->recurse);
      }
   
   printf(" uids = ( ");

   for (up = ptr->uid; up != NULL; up=up->next)
      {
      printf("%d ",up->uid);
      }

   printf(")\n gids = ( ");

   for (gp = ptr->gid; gp != NULL; gp=gp->next)
      {
      printf("%d ",gp->gid);
      }

   printf(")\n exclude:");

   for (iip = ptr->exclusions; iip != NULL; iip = iip->next)
      {
      printf(" %s",iip->name);
      }
   
   printf("\n");
   printf(" symlink:");

   for (iip = ptr->symlink; iip != NULL; iip = iip->next)
      {
      printf(" %s",iip->name);
      }
   
   printf("\n include:");
   
   for (iip = ptr->inclusions; iip != NULL; iip = iip->next)
      {
      printf(" %s",iip->name);
      }
   printf("\n");
   
   printf(" classes = %s\n",ptr->classes);

   printf(" method = %c (time/checksum)\n",ptr->type);

   printf(" server = %s\n",ptr->server);

   if (! ptr->backup)
      {
      printf(" NOT BACKED UP\n");
      }
   }
}

/*********************************************************************/

ListDefinedTidy()

{ struct Tidy *ptr;
  struct TidyPattern *tp;

printf ("\nDEFINED TIDY MASKS\n\n");

for (ptr = VTIDY; ptr != NULL; ptr=ptr->next)
   {
   printf("  path=%s\n",ptr->path);
   for(tp = ptr->tidylist; tp != NULL; tp=tp->next)
      {
      printf("    pat=%s, %c-age=%d, size=%d, linkdirs=%c, rmdirs=%c ",
	     tp->pattern,tp->searchtype,tp->age,tp->size,tp->dirlinks,tp->rmdirs);

      if (tp->recurse == INFINITERECURSE)
	 {
	 printf("recurse=inf\n");
	 }
      else
	 {
	 printf("recurse=%d\n",tp->recurse);
	 }
      }
   }
}

/*********************************************************************/

ListDefinedMountables()

{ struct Item *ptr;

printf ("\nDEFINED MOUNTABLES\n\n");

for (ptr = VMOUNTABLES; ptr != NULL; ptr=ptr->next)
   {
   printf("%s\n",ptr->name);
   }
}

/*********************************************************************/

ListMiscMounts()

{ struct MiscMount *ptr;

printf ("\nDEFINED MISC MOUNTABLES\n\n");

for (ptr = VMISCMOUNT; ptr != NULL; ptr=ptr->next)
   {
   printf("%s on %s (%s)\n",ptr->from,ptr->onto,ptr->options);
   }

printf (")\n");
}

/*********************************************************************/

ListDefinedRequired()

{ struct Item *ptr;

printf ("\nDEFINED REQUIRE\n\n");

for (ptr = VREQUIRED; ptr != NULL; ptr=ptr->next)
   {
   printf("%s\n",ptr->name);
   }
}

/*********************************************************************/

ListDefinedHomeservers()

{ struct Item *ptr;

printf ("\nDefined home servers = ( ");

for (ptr = VHOMESERVERS; ptr != NULL; ptr=ptr->next)
   {
   printf("%s ",ptr->name);
   }

printf (")\n");
}

/*********************************************************************/

ListDefinedDisable()

{ struct Disable *ptr;

printf ("\nDEFINED DISABLE\n\n");

for (ptr = VDISABLELIST; ptr != NULL; ptr=ptr->next)
   {
   printf("%s: rotate=%d, type=%s\n",ptr->name,ptr->rotate,ptr->type);
   }
}

/*********************************************************************/

ListDefinedMakePaths()

{ struct File *ptr;
  struct UidList *up;
  struct GidList *gp;

printf ("\nDEFINED DIRECTORIES\n\n");

for (ptr = VMAKEPATH; ptr != NULL; ptr=ptr->next)
   {
   printf("%s\n +%o\n -%o\n %s\n",ptr->path,ptr->plus,ptr->minus,FILEACTIONTEXT[ptr->action]);

   if (ptr->recurse == INFINITERECURSE)
      {
      printf(" recurse=inf\n");
      }
   else
      {
      printf(" recurse=%d\n",ptr->recurse);
      }
   
   printf(" uids = ( ");

   for (up = ptr->uid; up != NULL; up=up->next)
      {
      printf("%d ",up->uid);
      }

   printf(")\n gids = ( ");

   for (gp = ptr->gid; gp != NULL; gp=gp->next)
      {
      printf("%d ",gp->gid);
      }
   printf(")\n");
   }
}

/*********************************************************************/

ListDefinedImports()

{ struct Item *ptr;

printf ("\nDEFINED IMPORTS\n\n");

for (ptr = VIMPORT; ptr != NULL; ptr=ptr->next)
   {
   printf("%s\n",ptr->name);
   }
}

/*********************************************************************/

ListDefinedIgnore()

{ struct Item *ptr;

printf ("\nDEFINED IGNORE\n\n");

for (ptr = VIGNORE; ptr != NULL; ptr=ptr->next)
   {
   printf("%s\n",ptr->name);
   }
}

/*********************************************************************/

ListFiles()

{ struct File *ptr;
  struct UidList *up;
  struct GidList *gp;

printf ("\nDEFINED FILES\n\n");

for (ptr = VFILE; ptr != NULL; ptr=ptr->next)
   {
   printf("\n%s\n +%o\n -%o\n %s\n travelinks=%c\n",
	  ptr->path,ptr->plus,ptr->minus,FILEACTIONTEXT[ptr->action],ptr->travlinks);

   if (ptr->recurse == INFINITERECURSE)
      {
      printf(" recurse=inf\n");
      }
   else
      {
      printf(" recurse=%d\n",ptr->recurse);
      }
   
   printf(" uids = ( ");

   for (up = ptr->uid; up != NULL; up=up->next)
      {
      printf("%d ",up->uid);
      }

   printf(")\n gids = ( ");

   for (gp = ptr->gid; gp != NULL; gp=gp->next)
      {
      printf("%d ",gp->gid);
      }
   printf(")\n");
   }
}

/*******************************************************************/

ListActionSequence()

{ struct Item *ptr;

printf("\nAction sequence = (");

for (ptr=VACTIONSEQ; ptr!=NULL; ptr=ptr->next)
   {
   printf("%s ",ptr->name);
   }

printf(")\n");
}

/*******************************************************************/

ListUnmounts()

{ struct Item *ptr;

printf("\nDEFINED UNMOUNTS\n\n");

for (ptr=VUNMOUNT; ptr!=NULL; ptr=ptr->next)
   {
   printf("%s ",ptr->name);
   }
}

/*******************************************************************/

ListProcesses()

{ struct Process *ptr;
  char *sp;

printf("\nDEFINED PROCESSES\n\n");

for (ptr = VPROCLIST; ptr != NULL; ptr=ptr->next)
   {
   if (ptr->restart == NULL)
      {
      sp = "";
      }
   else
      {
      sp = ptr->restart;
      }
   
   printf("%s\n Restart = %s\n matches: %c%d\n signal=%s\n action=%c\n\n",ptr->expr,sp,ptr->comp,ptr->matches,
	  SIGNALS[ptr->signal],ptr->action);
   }

printf(")\n");
}

/*******************************************************************/

ListFileEdits()

{ struct Edit *ptr;
  struct Edlist *ep;

printf("\nDEFINED FILE EDITS\n\n");

for (ptr=VEDITLIST; ptr!=NULL; ptr=ptr->next)
   {
   printf("%s\n",ptr->fname);
   for (ep = ptr->actions; ep != NULL; ep=ep->next)
      {
      if (ep->data == NULL)
         {
         printf(" %s [nodata]\n",VEDITNAMES[ep->code]);
         }
      else
         {
         printf(" %s [%s]\n",VEDITNAMES[ep->code],ep->data);
         }
      }
   printf("\n");
   }
}

/*******************************************************************/

BuildClassEnvironment()

{ struct Item *ip;

sprintf(ALLCLASSBUFFER,"%s=\0",CFALLCLASSESVAR);

for (ip = VHEAP; ip != NULL; ip=ip->next)
   {
   if (IsDefinedClass(ip->name))
      {
      if (BufferOverflow(ALLCLASSBUFFER,ip->name))
	 {
	 printf("cfengine: culprit BuildClassEnvironment()\n");
	 return;
	 }

      strcat(ALLCLASSBUFFER,ip->name);
      strcat(ALLCLASSBUFFER,":");
      }
   }

for (ip = VALLADDCLASSES; ip != NULL; ip=ip->next)
   {
   if (IsDefinedClass(ip->name))
      {
      if (BufferOverflow(ALLCLASSBUFFER,ip->name))
	 {
	 printf("cfengine: culprit BuildClassEnvironment()\n");
	 return;
	 }
      
      strcat(ALLCLASSBUFFER,ip->name);
      strcat(ALLCLASSBUFFER,":");
      }
   }

Debug2("---\nENVIRONMENT: %s\n---\n",ALLCLASSBUFFER);

if (USEENVIRON)
   {
   if (putenv(ALLCLASSBUFFER) == -1)
      {
      perror("putenv");
      }
   }
}

/*******************************************************************/

RequiredFileSystemOkay (name)

char *name;

{ struct stat statbuf, localstat;
  DIR *dirh;
  struct dirent *dirp;
  int sizeinbytes = 0, filecount = 0;
  char buff[bufsize];

Debug2("Checking required filesystem %s\n",name);

if (stat(name,&statbuf) == -1)
   {
   return(false);
   }

if (S_ISLNK(statbuf.st_mode))
   {
   KillOldLink(name);
   return(true);
   }

if (S_ISDIR(statbuf.st_mode))
   {
   if ((dirh = opendir(name)) == NULL)
      {
      printf("cfengine: RequiredFileSystemOkay(): Can't open directory %s\n",name);
      return;
      }

   for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
      {
      if (strcmp(".",dirp->d_name) == 0 || strcmp("..",dirp->d_name) == 0)
         {
         continue;
         }

      filecount++;

      strcpy(buff,name);

      if (buff[strlen(buff)] != '/')
         {
         strcat(buff,"/");
         }

      strcat(buff,dirp->d_name);

      if (lstat(buff,&localstat) == -1)
         {
         if (S_ISLNK(localstat.st_mode))
            {
            KillOldLink(buff);
            continue;
            }

         perror("cfengine: stat in RequiredFileSystemOkay()");
         printf("          refers to %s\n",buff);
         continue;
         }

      sizeinbytes += localstat.st_size;
      }

   closedir(dirh);

   if (sizeinbytes < SENSIBLEFSSIZE)
      {
      printf("cfengine: File system %s is suspiciously small! (%d bytes)\n",name,sizeinbytes);
      return(true);
      }

   if (filecount < SENSIBLEFILECOUNT)
      {
      printf("cfengine: Filesystem %s has only %d files/directories.\n",name,filecount);
      return(true);
      }
   }

return(true);
}


/*******************************************************************/

InstallMountedItem(host,mountdir)

char *host, *mountdir;

{
strcpy (VBUFF,host);
strcat (VBUFF,":");
strcat (VBUFF,mountdir);

if (IsItemIn(VMOUNTED,VBUFF))
   {
   if (! SILENT || !WARNINGS)
      {
      printf("cfengine: WARNING mount item %s\n",VBUFF);
      printf("          is mounted multiple times!!\n\n");
      printf("          [If this is freebsd/netbsd you can switch off this]\n");
      printf("          [message with the -w/--nowarn option, or better still]\n");
      printf("          [get bsd to fix this bug!]\n");
      }
   }

AppendItem(&VMOUNTED,VBUFF,NULL);
}

/*******************************************************************/

AddToFstab(host,rmountpt,mountpt,mode)

char *host, *mountpt, *rmountpt, *mode;

{ char fstab[bufsize];
  char *opts;
  FILE *fp;

Verbose("AddToFstab(%s)\n",mountpt);

opts = VMOUNTOPTS[VSYSTEMHARDCLASS];

switch (VSYSTEMHARDCLASS)
   {
   case osf:
   case bsd4_3:
   case irix:
   case irix4:
   case irix64:
   case sun3:
   case aos:
   case nextstep:
   case newsos:
   case sun4:    sprintf(fstab,"%s:%s \t %s %s\t%s,%s 0 0",host,rmountpt,mountpt,VNFSTYPE,mode,opts);
                 break;
   case ultrx:   sprintf(fstab,"%s@%s:%s:%s:0:0:%s:%s",rmountpt,host,mountpt,mode,VNFSTYPE,opts);
                 break;
   case hp10:
   case hp:      sprintf(fstab,"%s:%s %s \t %s \t %s,%s 0 0",host,rmountpt,mountpt,VNFSTYPE,mode,opts);
                 break;
   case aix:     sprintf(fstab,"%s:\n\tdev\t= %s\n\ttype\t= %s\n\tvfs\t= %s\n\tnodename\t= %s\n\tmount\t= true\n\toptions\t= %s,%s\n\taccount\t= false\n\n",mountpt,rmountpt,VNFSTYPE,VNFSTYPE,host,mode,opts);
                 break;
   case linuxx:  sprintf(fstab,"%s:%s \t %s \t %s \t %s",host,rmountpt,mountpt,VNFSTYPE,opts);
                 break;

   case netbsd:
   case bsd_i:
   case freebsd: sprintf(fstab,"%s:%s \t %s \t %s \t %s,%s 0 0",host,rmountpt,mountpt,VNFSTYPE,mode,opts);
                 break;

   case solarisx86:
   case solaris: sprintf(fstab,"%s:%s - %s %s - yes %s,%s",host,rmountpt,mountpt,VNFSTYPE,mode,opts);
                 break;
   case unused1:
   case unused2:
   case unused3:
   default:      FatalError("AddToFstab(): unknown hard class detected!\n");
                 break;
   }

if (MatchStringInFstab(mountpt))
   {
   if (!SILENT)
      {
      printf("cfengine: Warning the file system %s seems to be in %s\n",mountpt,VFSTAB[VSYSTEMHARDCLASS]);
      printf("          already, but I was not able to mount it.\n");
      printf("          ( Check the exports file on host %s? Check for file with same name as dir?)\n",host);
      }
   return;
   }

if (DONTDO)
   {
   printf("cfengine: add filesystem to %s\n",VFSTAB[VSYSTEMHARDCLASS]);
   printf("          %s\n",fstab);
   }
else
   {
   if ((fp = fopen(VFSTAB[VSYSTEMHARDCLASS],"a")) == NULL)
      {
      printf("cfengine: can't open %s for appending\n",VFSTAB[VSYSTEMHARDCLASS]);
      perror("fopen");
      return;
      }

   printf("cfengine: adding filesystem to %s\n",VFSTAB[VSYSTEMHARDCLASS]);
   printf("          %s\n",fstab);
   /* fprintf(fp,"# cfengine added next line\n"); */
   fprintf(fp,"%s\n",fstab);
   fclose(fp);
   chmod(VRESOLVCONF[VSYSTEMHARDCLASS],DEFAULTSYSTEMMODE);
   }
}


/*******************************************************************/

EditItemsInResolvConf(from,list)

struct Item *from, **list;

{ struct Item *ip;
  char buf[maxvarsize];

if (from == NULL)
   {
   return;
   }
else
   {
   EditItemsInResolvConf(from->next,list);
   if (isdigit(*(from->name)))
      {
      sprintf(buf,"nameserver %s",from->name);
      }
   else
      {
      strcpy(buf,from->name);
      }
   
   DeleteItemStarting(list,buf); /* del+prep = move to head of list */
   PrependItem(list,buf,NULL);
   return;
   }
}

/*******************************************************************/

CheckHome(ptr)                      /* iterate check over homedirs */

struct File *ptr;

{ int RecFileCheck();
  struct Item *ip1, *ip2;
  char basename[bufsize],pathbuff[bufsize];

Debug2("\nCHECK HOME DIRS: \n");

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can check others' files.\n");
   return;
   }

if (!MountPathDefined())
   {
   return;
   }

for (ip1 = VHOMEPATLIST; ip1 != NULL; ip1=ip1->next)
   {
   for (ip2 = VMOUNTLIST; ip2 != NULL; ip2=ip2->next)
      {
      if (IsExcluded(ip2->classes))
	 {
	 continue;
	 }
      pathbuff[0]='\0';
      basename[0]='\0';
      strcpy(pathbuff,ip2->name);
      AddSlash(pathbuff);
      strcat(pathbuff,ip1->name);

      if (strlen(ptr->path) > 4)                  /* home/subdir */
         {
         if (*(ptr->path+4) != '/')
            {
            printf("cfengine: illegal use of home in files: %s\n",ptr->path);
            return;
            }
         else
            {
            strcat(pathbuff,ptr->path+5);
            }
	 }

      ExpandWildCardsAndDo(pathbuff,basename,RecFileCheck,ptr);
      }
   }

}

/*******************************************************************/

 /* These functions are wrappers for the real functions so that
    we can abstract the task of parsing general wildcard paths
    like /a*b/bla?/xyz from a single function ExpandWildCardsAndDo() */

/*******************************************************************/

TidyWrapper(startpath,vp)

char *startpath;
void *vp;

{ struct Tidy *tp;

tp = (struct Tidy *) vp;

Debug2("TidyWrapper(%s)\n",startpath);

RecursiveTidySpecialArea(startpath,tp,tp->recurse);
}

/*******************************************************************/

RecHomeTidyWrapper(startpath,vp)

char *startpath;
void *vp;

{
Verbose("Tidying %s...\n",startpath);
RecursiveHomeTidy(startpath,1,vp);
}

/*******************************************************************/

CheckFileWrapper(startpath,ptr)

char *startpath;
struct File *ptr;

{  struct stat statbuf;
   mode_t filemode;

if (ptr->action == touch && IsWildCard(ptr->path))
   {
   printf("cfengine: Can't touch a wildcard! (%s)\n",ptr->path);
   return;
   }

if (stat(startpath,&statbuf) == -1)
   {
   Verbose("cfengine: File/directory <%s> does not exist!\n",ptr->path);

   if (TouchDirectory(ptr))                /* files ending in /. */
      {
      MakeDirectoriesFor(startpath);
      ptr->action = fixall;
      *(startpath+strlen(ptr->path)-2) = '\0';       /* trunc /. */
         CheckExistingFile(startpath,ptr->plus,ptr->minus,ptr->action,ptr->uid,ptr->gid,&statbuf);
      return;
      }

   filemode = DEFAULTMODE;      /* Decide the mode for filecreation */
   filemode |=   ptr->plus;
   filemode &= ~(ptr->minus);

   switch (ptr->action)
      {
      case touch:   if (! DONTDO)
                       {
                       MakeDirectoriesFor(startpath);
                       if (creat(ptr->path,filemode) == -1)
			 {
                         perror("creat");
                         return;
                         }
                       }

                    Verbose("CFENGINE: touching file %s, mode = %o\n",ptr->path,filemode);
                    break;

      case linkchildren: 
      case warnall:
      case warnplain:
      case warndirs:
      case fixplain:
      case fixdirs:
      case fixall:  printf("cfengine: File/Dir %s did not exist and was marked (%s)\n",ptr->path,FILEACTIONTEXT[ptr->action]);
                    break;

      default:      FatalError("cfengine: Internal sofware error: Checkfiles(), bad action\n");
      }
   }
else
   {
   if (TouchDirectory(ptr))  /* Don't check just touch */
      {
      return;
      }

   if (ptr->action == linkchildren)
      {
      LinkChildren(ptr->path,&statbuf,ptr->uid,ptr->gid);
      return;
      }

   if (S_ISDIR(statbuf.st_mode) && (ptr->recurse != 0))
      {
      CheckExistingFile(startpath,ptr->plus,ptr->minus,ptr->action,ptr->uid,ptr->gid,&statbuf);
      RecursiveCheck(startpath,ptr->plus,ptr->minus,ptr->action,ptr->uid,ptr->gid,ptr->recurse,0); 
      }
   else
      {
      CheckExistingFile(startpath,ptr->plus,ptr->minus,ptr->action,ptr->uid,ptr->gid,&statbuf);
      }
   }
}

/*******************************************************************/

RotateFiles(name,number)

 /* Rotates file extensions like in free bsd, messages, syslog */
 /* etc. Note that this doesn't check for failure, since the   */
 /* aim is to disable the files anyway                         */


char *name;
int number;

{ int i;
  struct stat statbuf;

stat(name,&statbuf);

for (i = number-1; i > 0; i--)
   {
   if (BufferOverflow(name,"1"))
      {
      printf("cfengine: culprit: RotateFiles in Disable:\n");
      return;
      }
   
   sprintf(CURRENTITEM,"%s.%d",name,i);
   sprintf(VBUFF,"%s.%d",name, i+1);
   
   if (rename(CURRENTITEM,VBUFF) == -1)
      {
      if (DEBUG || D2)
	 {
         printf("cfengine: rename failed in RotateFiles %s -> %s\n",name,CURRENTITEM);
	 }
      }
   }

sprintf(VBUFF,"%s.1",name);

if (rename(name,VBUFF) == -1)
   {
   Debug2("cfengine: rename failed in RotateFiles %s -> %s\n",name,CURRENTITEM);
   }

if (creat(name,statbuf.st_mode) == -1)
   {
   printf("cfengine: failed to create new %s in disable(rotate)\n",name);
   }
}

/*******************************************************************/

TruncateFile(name)

char *name;

{ struct stat statbuf;

if (stat(name,&statbuf) == -1)
   {
   Debug2("cfengine: didn't find %s to truncate\n",name);
   return;
   }
else
   {
   if (creat(name,000) == -1)      /* dummy mode ignored */
      {
      printf("cfengine: creat(%s) failed\n",name);
      }
   }
}

/*******************************************************************/
/* Level 3                                                         */
/*******************************************************************/

TouchDirectory(ptr)                     /* True if file path in /. */

struct File *ptr;

{ char *sp;

if (ptr->action == touch)
   {
   sp = ptr->path+strlen(ptr->path)-2;

   if (strcmp(sp,"/.") == 0)
      {
      Debug2("Checking for touchdirectory = %s - true\n",ptr->path);
      return(true);
      }
   else
      {
      Debug2("Checking for touchdirectory = %s - false\n",ptr->path);
      return false;
      }
   }
else
   {
   return false;
   }
}


/*******************************************************************/

RecFileCheck(startpath,ptr)

char *startpath;
struct File *ptr;

{
Verbose("Checking files in %s...\n",startpath);
RecursiveCheck(startpath,ptr->plus,ptr->minus,ptr->action,ptr->uid,ptr->gid,ptr->recurse,0);
}

/*******************************************************************/

Syntax()

{ int i;

printf("GNU cfengine: A system configuration tool \n%s\n%s\n",VERSION,COPYRIGHT);
printf("\n");
printf("Options:\n\n");

for (i=0; OPTIONS[i].name != NULL; i++)
   {
   printf("--%-20s    (-%c)\n",OPTIONS[i].name,(char)OPTIONS[i].val);
   }

printf("\nBug reports to bug-cfengine@gnu.ai.mit.edu (News: gnu.cfengine.bug)\n");
printf("General help to help-cfengine@gnu.ai.mit.edu (News: gnu.cfengine.help)\n");
printf("Info & fixes at http://www.iu.hioslo.no/~mark/cfengine.html\n");
}

/*******************************************************************/
/* Toolkit fstab                                                   */
/*******************************************************************/

MatchStringInFstab(str)

char *str;

{ FILE *fp;

if ((fp = fopen(VFSTAB[VSYSTEMHARDCLASS],"r")) == NULL)
   {
   printf("cfengine: can't open %s for reading\n",VFSTAB[VSYSTEMHARDCLASS]);
   perror("fopen");
   return true; /* write nothing */
   }

while (!feof(fp))
   {
   fgets (VBUFF,bufsize,fp);

   if (VBUFF[0] == '#')
      {
      continue;
      }

   if (strstr(VBUFF,str))
      {
      return true;
      }
   }

fclose(fp);
return(false);
}



/* EOF */
