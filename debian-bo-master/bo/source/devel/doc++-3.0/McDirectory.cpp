#include "McDirectory.h"
#include "McSorter.h"
#include "McString.h"
#include <stdio.h>

#ifdef WIN32
#include <windows.h>
#include <MAPIWIN.h>

int McDirectory::scan(const char *dirname, McDArray<char *> &list,
		      const char *pattern)
{
  WIN32_FIND_DATA  findFileData;
  char buf[1024];
  if (!pattern) {
    pattern="*";
  }
  sprintf(buf,"%s\\%s",dirname,pattern);
  
  HANDLE searchHandle = FindFirstFile(buf, &findFileData);
  if (searchHandle==INVALID_HANDLE_VALUE) {
    printf("No files matching %s found.\n",buf);
    return(0);
  }
  int next=1;
  while (searchHandle!=INVALID_HANDLE_VALUE && next) {
    if (strcmp(".",findFileData.cFileName)!=0 &&
	strcmp("..",findFileData.cFileName)!=0 )
      list.append(_strdup(findFileData.cFileName));
    next=FindNextFile(searchHandle,&findFileData);
  }
  FindClose(searchHandle);
  StringCompare comp;
  if (list.size())
    sort((char **)list,list.size(),comp);
  return(0);
}

int McDirectory::isDirectory(const char *dirname)
{
    McString d(dirname);
    
    if (!d.length())
	return(0);
    while (d.length() && d[d.length()-1]==pathDelimiter()) {
	d.remove(d.length()-1);
    }
    WIN32_FIND_DATA  findFileData;
    HANDLE searchHandle = FindFirstFile((const char *)d, &findFileData);
    if (searchHandle==INVALID_HANDLE_VALUE) {
      return(0);
    }
    int ret = ((findFileData.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY)!=0);
    FindClose(searchHandle);    
    return(ret);
}
#else
#include <dirent.h>
/* Great. We're doing UNIX, not Windows ! */

int McDirectory::scan(const char *dirname, McDArray<char *> &list,
		      const char *pattern)
{

    if (!pattern) {
	pattern="*";
    }
    DIR *dir = opendir(dirname);    
    struct dirent *entry;
    
    if (!dir) {
	fprintf(stderr,"Can't open dir %s\n",dirname);
	return(0);
    }

    while (entry = readdir(dir)) {
	if (entry->d_name[0]!='.')
	    if (mcWildMatch(entry->d_name,pattern))		
		list.append(strdup(entry->d_name));
    };
    closedir(dir);
    StringCompare comp;
    if (list.size())
       sort((char **)list,list.size(),comp);
    return(list.size());
}

int McDirectory::isDirectory(const char *dirname)
{
    McString d(dirname);
    
    if (!d.length())
	return(0);
    while (d.length() && d[d.length()-1]==pathDelimiter()) {
	d.remove(d.length()-1);
    }
    DIR *dir = opendir(dirname);    
    if (!dir)
	return(0);
    closedir(dir);
    return(1);
}
#endif


