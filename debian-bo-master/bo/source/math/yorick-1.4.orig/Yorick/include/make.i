/*
   MAKE.I
   Yorick automatic Makefile generator.

   $Id$
 */
/*    Copyright (c) 1996.  The Regents of the University of California.
                    All rights reserved.  */

func make(args)
/* DOCUMENT make, args
         or make

     makes a Makefile for ARGS(1) in the current working directory.
     args is a list of strings that looks like this:

       [code_name, new1, new2, ..., "+", old1, old2, ...]

     (Without ARGS, checks to be sure that existing Makefile has
     correct MAKE_TEMPLATE for this platform.)

     Each NEWi is a Yorick include file in the current working
     directory, which will become a startup file for the new custom
     version of Yorick you want to build.  Each OLDi file is a
     startup file for a previously built package; these must be in
     either the Y_SITE/startup or Y_SITE/contrib directories (or
     have softlinks in the current working directory).

     The matrix.i and fft.i packages are included by default; if you
     want to override that default, specify "-matrix" and/or "-fft"
     as part of the list of OLDi.

     All the startup files must contain a MAKE-INSTRUCTIONS comment.
     It should be placed near the top of each startup file.
     This comment consists of a line containing slash* followed by
     MAKE-INSTRUCTIONS then a number of lines with various keywords,
     then the closing *slash on its own line.  The recognized keywords
     are:

     SRCS = src1.c src2.c src3.f ...
     LIB = pkg
     DEPLIBS = dep1 dep2
     NO-WRAPPERS

     The SRCS keyword is mandatory; the others are all optional.

     The SRCS are the source files which produce the compiled functions
     referenced by the extern statements in the startup file which
     contains this MAKE-INSTRUCTIONS comment.  The extension ".f" or
     ".F" implies Fortran source code, which has some special configuration
     fiddles (see Y_HOME/Maketmpl).  You can continue the list by placing
     a backslash \ at the end of a line; the list of SRCS stops after the
     first line which does not end with \.

     The LIB keyword specifies the name of the library for this package.
     The library will actually be called libpkg.a and the load option will
     be -lpkg (don't try to include these decoration in the name pkg you
     supply).  If the LIB keyword is not present, you will get no library,
     and you won't be able to use this startup file as one of the OLDi in
     any new custom versions of Yorick you may build in the future.

     DEPLIBS is a list of any dependent libraries which the compiled
     functions in SRCS may need.  Do not include m (the libm math library)
     or any Fortran libraries or other libraries related to or used by
     Yorick itself here.  Hopefully, you won't need DEPLIBS at all -- if
     you do, you will probably need to tweek the Makefile to add correct
     -L options so that libdep1, libdep2, etc can be found.

     If none of the extern functions in this startup file has a
     PROTOTYPE comment, add the NO-WRAPPERS keyword, otherwise don't.

     See Y_HOME/Maketmpl for more information (start yorick and type
     Y_HOME to find the directory name at your site).

     To run this program in batch mode:

     yorick -batch make.i
        type this after you've moved to a new platform in order to
	set the MAKE_TEMPLATE in the Makefile to the correct value
	for this platform (Y_HOME is probably different)

     yorick -batch make.i code_name new1 new2 ...
     yorick -batch make.i code_name new1 new2 ... + old1 old2 ...
        type this to construct a new Makefile for code_name
	if a Makefile already exists, it simply performs the check as
	above; if you want to rebuild a new Makefile, you need to
	remove or change the name of the old one by hand
 */
{
  if (check_makefile()) return;
  if (numberof(args)<1)
    error, "Usage: code_name new1 new2 ... + old1 old2 ...";
  code_name= args(1);
  if (numberof(args)<2) args= [];
  else args= args(2:0);
  if (numberof(args)) {
    list= where(args=="+");
    if (numberof(list)) {
      list= list(1);
      if (list>1) new_pkgs= args(1:list-1);
      else new_pkgs= [];
      if (list<numberof(args)) old_pkgs= args(list+1:0);
      else old_pkgs= [];
    } else {
      new_pkgs= args;
      old_pkgs= [];
    }
  }
  dflt_pkgs= [];
  list= where(old_pkgs!="-matrix");
  if (numberof(list)<numberof(old_pkgs)) old_pkgs= old_pkgs(list);
  else grow, dflt_pkgs, "matrix.i";
  list= where(old_pkgs!="-fft");
  if (numberof(list)<numberof(old_pkgs)) old_pkgs= old_pkgs(list);
  else grow, dflt_pkgs, "fft.i";
  old_pkgs= undup_names(grow(old_pkgs, dflt_pkgs));
  new_makefile, code_name, new_pkgs, old_pkgs;
}

func check_makefile(void)
{
  /* check an existing makefile for MAKE_TEMPLATE
   * also, check for an outdated Makefile and try to update to
   * the modern form
   * returns 1 if (possibly modified) Makefile is usable,
   * returns 0 if new Makefile must be built from scratch */
  f= open("Makefile", "r", 1);
  if (is_void(f)) return 0;

  /* read the whole Makefile into memory as an array of strings */
  list= array(pointer, 200);
  len= array(0, 200);
  obsolete= [];
  i0= 0;
  for (i=1 ; i<=200 ; ++i) {
    lines= rdline(f, 100);
    if (is_void(obsolete)) {
      token= strtok(lines," \t=");
      test= where(token(1,)=="MAKE_TEMPLATE");
      if (numberof(test)) {
	obsolete= 0;
	test= test(1);  /* should there be a warning if more than one? */
	token= strtok(token(2,test)," \t=")(1);
	n= -strlen(token);
	for (j=-1 ; j>n ; --j) if (strpart(token,j:j)=="/") break;
	if (strpart(token,1:j)==Y_HOME) return 1;
	lines(test)= "MAKE_TEMPLATE = "+Y_HOME+strpart(token,j+1:0);
      } else {
	token= token(1,);
	test= where((token=="exec_prefix") | (token=="Y_HOME"));
	if (numberof(test)) {
	  obsolete= sum(len)+test(1);
	  lines(test)= "#"+lines(test);
	  i0= i;
	}
      }
    }
    if (obsolete) {
      if (i0!=i) {
	token= strtok(lines," \t=")(1,);
	test= where((token=="exec_prefix") | (token=="Y_HOME"));
	if (numberof(test)) lines(test)= "#"+lines(test);
      }
      test= where(strmatch(lines,"-END-CODE-SPECIFIC-SECTION-"));
      if (numberof(test)) {
	test= test(1);
	if (test<3) {
	  if (i>1) {
	    list(i-1)= (*list(i-1))(1:-1);
	    lines= [string(0)];
	  } else {
	    /* this is not really a Makefile at all */
	    close, f;
	    rename, "Makefile", "Makefile.old";
	    return 0;
	  }
	} else {
	  lines= lines(1:test-2);
	}
	/* since lines shortened, this will be final pass */
      }
    }
    lines= lines(where(lines));
    list(i)= &lines;
    len(i)= numberof(lines);
    if (numberof(lines)<100) break;
  }
  n= sum(len);
  lines= n? array(string, n) : [];
  for (i=1,j=0 ; j<n ; j+=len(i++)) {
    if (!len(i)) continue;
    lines(j+1:j+len(i))= *list(i);
  }
  list= len= [];

  /* Makefile usable, but needs to be modified */
  close, f;
  rename, "Makefile", "Makefile.old";

  if (obsolete) {
    lines2= lines(obsolete:0);
    if (obsolete>1) lines= lines(1:obsolete-1);
    else lines= [];
    grow, lines, ["MAKE_TEMPLATE = "+Y_HOME+"Maketmpl"],
      lines2, ["# to set MAKE_TEMPLATE properly, run   yorick -batch make.i",
	       "include $(MAKE_TEMPLATE)"];
  }

  write, create("Makefile"), format="%s\n", lines;
  return 1;
}

func new_makefile(code_name, new_pkgs, old_pkgs)
{
  n= numberof(new_pkgs);
  if (!n) {
    new_pkgs= code_name+".i";
    n= 1;
  }
  srcs= lib= deplibs= s= d= [];
  nowrap= 0;
  for (i=1 ; i<=n ; ++i) {
    f= open(new_pkgs(i), "r", 1);
    if (is_void(f)) { f=new_pkgs(i); goto oops; }
    instrucs= get_keys(new_pkgs(i), f, s, l, d);
    if (is_void(lib))
      lib= l;
    else if (!is_void(l) && anyof(lib!=l))
      error, "new pacakges must all specify same LIB in MAKE-INSTRUCTIONS";
    grow, srcs, s;
    grow, deplibs, d;
    list= where(instrucs(1,)=="NO-WRAPPERS");
    if (numberof(list)) nowrap++;
  }
  deplibs= grow(lib, deplibs);
  if (numberof(lib)>1)
    error, "new packages may specify at most one LIB in MAKE-INSTRUCTIONS";
  else if (numberof(lib)==1)
    lib= lib(1);
  code_library= lib;
  nsrcs= numberof(srcs);
  if (nowrap==n) nowrap= "";
  else nowrap= " ywrap.o";

  n= numberof(old_pkgs);
  for (i=1 ; i<=n ; ++i) {
    /* note: . is assumed to be Y_LAUNCH */
    f= find_file(old_pkgs(i),
		 ["./",Y_SITE+"startup/",Y_SITE+"contrib/"]);
    if (is_void(f)) { f=old_pkgs(i); goto oops; }
    get_keys, old_pkgs(i), f, s, l, d;
    if (is_void(l))
      error,"package "+old_pkgs(i)+" specifies no LIB in MAKE-INSTRUCTIONS";
    grow, srcs, s;
    grow, deplibs, l, d;
  }

  srcs= strtok(srcs,".");
  ext= srcs(2,);
  srcs= srcs(1,);
  list= where((ext=="f") | (ext=="F"));
  fortran= (numberof(list)!=0);
  objs= srcs(1:nsrcs)+".o";

  if (!is_void(deplibs))
    deplibs= "-l"+undup_names(deplibs);

  if (open("Makefile","r",1)) rename, "Makefile", "Makefile.old";

  f= create("Makefile");
  write,f,format="# Makefile for %s\n",code_name;
  write,f,format="# generated by make.i     %s\n\n",timestamp();

  write,f,format="%s\n",
    "######################################################################";
  write,f,format="# %s\n\n","First section is definitions for Maketmpl";

  write,f,format="MAKE_TEMPLATE = %s%s\n\n",Y_HOME,"Maketmpl";
  write,f,format="C_OPTIMIZE = %s\nLD_OPTIMIZE = $(C_OPTIMIZE)\n","-O";
  if (fortran) write,f,format="F_OPTIMIZE = %s\n","-O";

  if (!is_void(code_library)) {
    write,f,format="\nCODE_NAME = %s\n", code_name;
    write,f,format="CODE_LIBRARY = lib%s.a\n", code_library;
    write,f,format="NON_SHARABLE = %s\n", "unused";
  } else {
    write,f,format="\nNON_SHARABLE = %s\n", code_name;
    write,f,format="CODE_NAME = %s\n", "unused";
    write,f,format="CODE_LIBRARY = %s\n", "unused2";
  }
  write,f,format="YWRAP_O =%s\n\n",nowrap;

  objlist= ["OBJS ="];
  tot= strlen(objlist(1));
  for (i=i0=1 ; i<=numberof(objs) ; ++i) {
    obj= objs(i);
    len= strlen(obj);
    if (tot+len+1 > 70) {
      objlist(i0)+= " \\";
      grow, objlist, ["      "];
      tot= strlen(objlist(0));
      ++i0;
    }
    objlist(i0)+= " "+obj;
    tot+= len+1;
  }
  write,f,format="%s",objlist;

  lib= "";
  for (i=1 ; i<=numberof(deplibs) ; ++i) lib+= " "+deplibs(i);
  write,f,format="\nPKG_LIBS =%s\n",lib;

  if (is_void(code_library))
    write,f,format="PKG_OBJS = %s\n","$(OBJS) $(YWRAP_O)"

  pkg= "";
  for (i=1 ; i<=numberof(new_pkgs) ; ++i) pkg+= " "+new_pkgs(i);
  write,f,format="\nY_INCLUDE =%s\n",pkg;
  pkg= "";
  for (i=1 ; i<=numberof(old_pkgs) ; ++i) pkg+= " "+old_pkgs(i);
  write,f,format="Y_OTHERS =%s\n\n",pkg;

  lib= fortran? " $(FORTRAN_LIBS)" : "";
  write,f,format="SYS_LIBS =%s\n", lib;
  if (fortran) write,f,format="FORTRAN_STYLE =%s\n", " $(WKS_FORTRAN)";

  write,f,format="\nCLEAN_UP = %s\n\n",code_name;

  write,f,format="%s\n",
    "######################################################################";
  write,f,format="# %s\n\n","Second section is targets for new package/code";

  if (is_void(code_library)) lib= "$(NON_SHARABLE)";
  else lib= "$(CODE_LIBRARY) $(CODE_NAME)";
  write,f,format="all:: %s\n\n", lib;

  write,f,format="%s\n%s\n%s\n%s\n%s\n\n",
    "# Add header dependencies or special compile instructions here, e.g.-",
    "#my_code1.o: my_code1.h   my_code.h",
    "#my_code2.o: my_code2.c my_code.h",
    "#\t$(CC) $(CFLAGS) -DSPECIAL_SWITCH -c my_code2.c",
    "#my_code3.o: my_code3.h   my_code1.h my_code.h";

  write,f,format="%s\n",
    "######################################################################";
  write,f,format="\n%s\n","include $(MAKE_TEMPLATE)";

  close,f;
  return;

oops:
  write, "new packages must be in current directory";
  write, "old packages must be in "+Y_SITE+"{startup,contrib}";
  error, "unable to find package: "+f;
}

func undup_names(names)
{
  /* remove duplicates from a list of names (or numbers) */
  if (numberof(names)<2) return names;
  require, "msort.i";
  order= msort(names);
  sorted= names(order);
  list= where(sorted(1:-1)==sorted(2:0));
  if (numberof(list)) {
    names(order(list))= structof(names)(0);
    names= names(where(names));
  }
  return names;
}

func get_keys(name, f, &srcs, &lib, &deplibs)
{
  instrucs= get_instructions(f);
  if (is_void(instrucs))
    error, "no MAKE-INSTRUCTIONS comment found in "+name;
  srcs= get_list(instrucs, "SRCS");
  if (is_void(srcs))
    error, "no SRCS in MAKE-INSTRUCTIONS comment found in "+name;
  lib= get_list(instrucs, "LIB");
  deplibs= get_list(instrucs, "DEPLIBS");
  return instrucs;
}

func get_list(instrucs, keyword)
{
  /* get keyword= from instruction lines returned by get_instructions
   * return value is array of token strings
   * -- allow for \ continuation of long lines */
  list= where(instrucs(,1)==keyword);
  if (!numberof(list)) return [];
  i= list(1);
  instrucs= instrucs(i:0,2);
  list= array(pointer, numberof(instrucs));
  len= array(0, numberof(list));
  line= tokenize_line(instrucs(1), " \t=");
  if (numberof(line)<2) return [];
  line= line(2:0);
  for (i=1 ; numberof(line)>=1 ; ++i) {
    cont= (line(0)=="\\");
    len(i)= n= numberof(line)-cont;
    if (n) list(i)= &line(1:n);
    if (!cont || i+1>=numberof(instrucs)) break;
    line= tokenize_line(instrucs(i+1), " \t=");
  }
  n= sum(len);
  if (!n) return [];
  line= array(string, n);
  for (i=1,j=0 ; j<n ; j+=len(i++)) {
    if (!len(i)) continue;
    line(j+1:j+len(i))= *list(i);
  }
  return line;
}

func tokenize_line(line, delim)
{
  len= strlen(line);
  if (!len) return [];
  list= array(string, len);
  len= 0;
  do {
    token= strtok(line, delim);
    line= token(2);
    token= token(1);
    if (!token) break;
    list(++len)= token;
  } while (line);
  if (!len) return [];
  return list(1:len);
}

func get_instructions(f)
{
  /* locate MAKE-INSTRUCTIONS comment and return 2xnlines
   * array of [[first token, full line]] */
  if (is_void(f)) return [];
  do {
    lines= rdline(f, 30);
    token= strtok(lines);
    list= where(token(1,)=="/*");
    if (numberof(list)) {
      starts= token(2,list);
      list= list(where(strtok(starts)(1,)=="MAKE-INSTRUCTIONS"));
      if (numberof(list)) {
	list= list(1);
	if (list>20) {
	  if (lines(0)) grow, lines, rdline(f, 10);
	  else grow, lines, array(string, 10);
	}
	lines= lines(list+1:list+10);
	token= strtok(lines," \t=")(1,);
	list= where(token=="*/");
	if (!numberof(list) || list(1)<2) return [];
	list= list(1)-1;
	return [token(1:list), lines(1:list)];
      }
    }
  } while (lines(0));
  return [];
}

func find_file(name, path)
{
  /* open file, which may be anywhere on optional supplied path */
  if (is_void(path) ||
      anyof(strpart(name,1:1)==["/","~","$"])) return open(name, "r", 1);
  list= where(strpart(path,0:0)!="/");
  if (numberof(list)) {
    if (numberof(path)>1) path(list)+= "/";
    else path+= "/";
  }
  for (i=1 ; i<numberof(path) ; ++i) {
    f= open(path(i)+name, "r", 1);
    if (!is_void(f)) return f;
  }
  return [];
}

if (batch()) {
  command_line= get_argv();
  if (numberof(command_line)>1) command_line= command_line(2:0);
  else command_line= [];
  make, command_line;
  quit;
}
