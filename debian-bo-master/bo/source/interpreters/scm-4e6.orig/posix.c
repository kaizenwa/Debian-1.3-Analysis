/* Copyright (C) 1994, 1995 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

/* "posix.c" functions only in Posix (unix).
   Author: Aubrey Jaffer */

#include "scm.h"

#include <pwd.h>
#include <sys/types.h>
#include <sys/wait.h>

#ifndef STDC_HEADERS
	char *ttyname P((int fd));
	FILE *popen P((const char* command, const char* type));
	int pclose P((FILE* stream));
#endif

     /* Only the superuser can successfully execute this call */
static char s_chown[] = "chown";
SCM l_chown(path, owner, group)
     SCM path, owner, group;
{
  int val;
  ASSERT(NIMP(path) && STRINGP(path), path, ARG1, s_chown);
  ASSERT(INUMP(owner), owner, ARG2, s_chown);
  ASSERT(INUMP(group), group, ARG3, s_chown);
  SYSCALL(val = chown(CHARS(path), INUM(owner), INUM(group)););
  return val ? BOOL_F : BOOL_T;
}

static char s_link[] = "link";
SCM l_link(oldpath, newpath)
     SCM oldpath, newpath;
{
  int val;
  ASSERT(NIMP(oldpath) && STRINGP(oldpath), oldpath, ARG1, s_link);
  ASSERT(NIMP(newpath) && STRINGP(newpath), newpath, ARG2, s_link);
  SYSCALL(val = link(CHARS(oldpath), CHARS(newpath)););
  return val ? BOOL_F : BOOL_T;
}

SCM l_pipe()
{
  int fd[2], ret;
  FILE *f_rd, *f_wt;
  SCM p_rd, p_wt;
  NEWCELL(p_rd); NEWCELL(p_wt);
  SYSCALL(ret = pipe(fd););
  if (ret) {ALLOW_INTS; return BOOL_F;}
  SYSCALL(f_rd = fdopen(fd[0], "r"););
  if (!f_rd) {
    close(fd[0]);
    goto errout;
  }
  SYSCALL(f_wt = fdopen(fd[1], "w"););
  if (!f_wt) {
    fclose(f_rd);
  errout:
    close(fd[1]);
    wta(UNDEFINED, (char *)NALLOC, s_port_type);
  }
  CAR(p_rd) = tc16_fport | mode_bits("r");
  CAR(p_wt) = tc16_fport | mode_bits("w");
  SETSTREAM(p_rd, f_rd);
  SETSTREAM(p_wt, f_wt);
  ALLOW_INTS;
  return cons(p_rd, p_wt);
}

char	s_op_pipe[] = "open-pipe";
SCM open_pipe(pipestr, modes)
     SCM pipestr, modes;
{
	FILE *f;
	register SCM z;
	ASSERT(NIMP(pipestr) && STRINGP(pipestr), pipestr, ARG1, s_op_pipe);
	ASSERT(NIMP(modes) && STRINGP(modes), modes, ARG2, s_op_pipe);
	NEWCELL(z);
	/* DEFER_INTS, SYSCALL, and ALLOW_INTS are probably paranoid here*/
	DEFER_INTS;
	ignore_signals();
	SYSCALL(f = popen(CHARS(pipestr), CHARS(modes)););
	unignore_signals();
	if (!f) z = BOOL_F;
	else {
	  CAR(z) = tc16_pipe | OPN | (strchr(CHARS(modes), 'r') ? RDNG : WRTNG);
	  SETSTREAM(z, f);
	}
	ALLOW_INTS;
	return z;
}
SCM l_open_input_pipe(pipestr)
     SCM pipestr;
{
  return open_pipe(pipestr, makfromstr("r", (sizeof "r")-1));
}
SCM l_open_output_pipe(pipestr)
     SCM pipestr;
{
  return open_pipe(pipestr, makfromstr("w", (sizeof "w")-1));
}
static int prinpipe(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  prinport(exp, port, s_pipe);
  return !0;
}

static char scm_s_getgroups[] = "getgroups";
SCM scm_getgroups()
{
  SCM grps, ans;
  int ngroups = getgroups(NULL, 0);
  if (!ngroups) return BOOL_F;
  NEWCELL(grps);
  DEFER_INTS;
  {
    gid_t *groups = (gid_t *)must_malloc(ngroups * sizeof(gid_t),
					 scm_s_getgroups);
    int val = getgroups(ngroups, groups);
    if (val < 0) {
      must_free(groups);
      ALLOW_INTS;
      return BOOL_F;
    }
    SETCHARS(grps, groups);	/* set up grps as a GC protect */
    SETLENGTH(grps, 0L + ngroups * sizeof(gid_t), tc7_string);
    ALLOW_INTS;
    ans = make_vector(MAKINUM(ngroups), UNDEFINED);
    while (--ngroups >= 0) VELTS(ans)[ngroups] = MAKINUM(groups[ngroups]);
    SETCHARS(grps, groups);	/* to make sure grps stays around. */
    return ans;
  }
}  

/* These 2 routines are not protected against `entry' being reused
   before access to that structure is completed */

static char s_pwinfo[] = "getpw";
SCM l_pwinfo(user)
     SCM user;
{
  SCM ans = make_vector(MAKINUM(7), UNSPECIFIED);
  struct passwd *entry;
  SCM *ve = VELTS(ans);
  DEFER_INTS;
  if UNBNDP(user) SYSCALL(entry = getpwent(););
  else if INUMP(user) SYSCALL(entry = getpwuid(INUM(user)););
  else {
    ASSERT(NIMP(user) && STRINGP(user), user, ARG1, s_pwinfo);
    SYSCALL(entry = getpwnam(CHARS(user)););
  }
  ALLOW_INTS;
  if (!entry) return BOOL_F;
  ve[ 0] = makfrom0str(entry->pw_name);
  ve[ 1] = makfrom0str(entry->pw_passwd);
  ve[ 2] = ulong2num((unsigned long)entry->pw_uid);
  ve[ 3] = ulong2num((unsigned long)entry->pw_gid);
  ve[ 4] = makfrom0str(entry->pw_gecos);
  ve[ 5] = makfrom0str(entry->pw_dir);
  ve[ 6] = makfrom0str(entry->pw_shell);
  return ans;
}
#include <grp.h>
static char s_grinfo[] = "getgr";
SCM l_grinfo(name)
     SCM name;
{
  SCM ans = make_vector(MAKINUM(4), UNSPECIFIED);
  struct group *entry;
  SCM *ve = VELTS(ans);
  DEFER_INTS;
  if UNBNDP(name) SYSCALL(entry = getgrent(););
  else if INUMP(name) SYSCALL(entry = getgrgid(INUM(name)););
  else {
    ASSERT(NIMP(name) && STRINGP(name), name, ARG1, s_grinfo);
    SYSCALL(entry = getgrnam(CHARS(name)););
  }
  ALLOW_INTS;
  if (!entry) return BOOL_F;
  ve[ 0] = makfrom0str(entry->gr_name);
  ve[ 1] = makfrom0str(entry->gr_passwd);
  ve[ 2] = ulong2num((unsigned long)entry->gr_gid);
  ve[ 3] = makfromstrs(-1, entry->gr_mem);
  return ans;
}
SCM l_setgr(arg)
     SCM arg;
{
  if (UNBNDP(arg) || FALSEP(arg)) endgrent();
  else setgrent();
  return UNSPECIFIED;
}
SCM l_setpw(arg)
     SCM arg;
{
  if (UNBNDP(arg) || FALSEP(arg)) endpwent();
  else setpwent();
  return UNSPECIFIED;
}

static char s_kill[] = "kill";
SCM l_kill(pid, sig)
     SCM pid, sig;
{
  int i;
  ASSERT(INUMP(pid), pid, ARG1, s_kill);
  ASSERT(INUMP(sig), sig, ARG2, s_kill);
  SYSCALL(i = kill((int)INUM(pid), (int)INUM(sig)););
  return MAKINUM(0L+i);
}
static char s_waitpid[] = "waitpid";
SCM l_waitpid(pid, options)
     SCM pid, options;
{
  int i, status;
  ASSERT(INUMP(pid), pid, ARG1, s_waitpid);
  ASSERT(INUMP(options), options, ARG2, s_waitpid);
  SYSCALL(i = waitpid(INUM(pid), &status, INUM(options)););
  return i < 0 ? BOOL_F : MAKINUM(0L+status);
}

SCM l_getppid()
{
  return MAKINUM(0L+getppid());
}

SCM l_getuid()
{
  return MAKINUM(0L+getuid());
}
SCM l_getgid()
{
  return MAKINUM(0L+getgid());
}
#ifndef LACK_E_IDs
SCM l_geteuid()
{
  return MAKINUM(0L+geteuid());
}
SCM l_getegid()
{
  return MAKINUM(0L+getegid());
}
#endif

static char s_setuid[] = "setuid";
SCM l_setuid(id)
     SCM id;
{
  ASSERT(INUMP(id), id, ARG1, s_setuid);
  return setuid(INUM(id)) ? BOOL_F : BOOL_T;
}
static char s_setgid[] = "setgid";
SCM l_setgid(id)
     SCM id;
{
  ASSERT(INUMP(id), id, ARG1, s_setgid);
  return setgid(INUM(id)) ? BOOL_F : BOOL_T;
}

#ifndef LACK_E_IDs
static char s_seteuid[] = "seteuid";
SCM l_seteuid(id)
     SCM id;
{
  ASSERT(INUMP(id), id, ARG1, s_seteuid);
  return seteuid(INUM(id)) ? BOOL_F : BOOL_T;
}
static char s_setegid[] = "setegid";
SCM l_setegid(id)
     SCM id;
{
  ASSERT(INUMP(id), id, ARG1, s_setegid);
  return setegid(INUM(id)) ? BOOL_F : BOOL_T;
}
#endif

static char s_ttyname[] = "ttyname";
SCM l_ttyname(port)
     SCM port;
{
  char *ans;
  ASSERT(NIMP(port) && OPPORTP(port), port, ARG1, s_ttyname);
  if (tc16_fport != TYP16(port)) return BOOL_F;
  SYSCALL(ans = ttyname(fileno(STREAM(port))););
  /* ans could be overwritten by another call to ttyname */
  return ans ? makfrom0str(ans) : BOOL_F;
}

SCM l_fork()
{
  long pid = 0L + fork();
  return -1L==pid ? BOOL_F : MAKINUM(pid);
}

#include <sys/utsname.h>
SCM l_uname()
{
  struct utsname buf;
  SCM ans = make_vector(MAKINUM(5), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  if (uname(&buf)) return BOOL_F;
  ve[ 0] = makfrom0str(buf.sysname);
  ve[ 1] = makfrom0str(buf.nodename);
  ve[ 2] = makfrom0str(buf.release);
  ve[ 3] = makfrom0str(buf.version);
  ve[ 4] = makfrom0str(buf.machine);
  /* ve[ 5] = makfrom0str(buf.domainname); */
  return ans;
}

static iproc subr0s[] = {
	{"pipe", l_pipe},
	{scm_s_getgroups, scm_getgroups},
	{"getppid", l_getppid},
	{"getuid", l_getuid},
	{"getgid", l_getgid},
#ifndef LACK_E_IDs
	{"getegid", l_getegid},
	{"geteuid", l_geteuid},
#endif
	{"uname", l_uname},
	{"fork", l_fork},
	{0, 0}};

static iproc subr1os[] = {
	{s_pwinfo, l_pwinfo},
	{s_grinfo, l_grinfo},
	{"setpwent", l_setpw},
	{"setgrent", l_setgr},
	{0, 0}};

static iproc subr1s[] = {
	{"setuid", l_setuid},
	{"setgid", l_setgid},
#ifndef LACK_E_IDs
	{"setegid", l_setegid},
	{"seteuid", l_seteuid},
#endif
	{"open-input-pipe", l_open_input_pipe},
	{"open-output-pipe", l_open_output_pipe},
	{s_ttyname, l_ttyname},
	{0, 0}};

static iproc subr2s[] = {
	{s_link, l_link},
	{s_kill, l_kill},
	{s_waitpid, l_waitpid},
	{s_op_pipe, open_pipe},
	{0, 0}};

static iproc subr3s[] = {
	{s_chown, l_chown},
	{0, 0}};

void init_posix()
{
	init_iprocs(subr0s, tc7_subr_0);
	init_iprocs(subr1s, tc7_subr_1);
	init_iprocs(subr1os, tc7_subr_1o);
	init_iprocs(subr2s, tc7_subr_2);
	init_iprocs(subr3s, tc7_subr_3);
	add_feature("posix");
	ptobs[0x0ff & (tc16_pipe>>8)].fclose = pclose;
	ptobs[0x0ff & (tc16_pipe>>8)].free = pclose;
	ptobs[0x0ff & (tc16_pipe>>8)].print = prinpipe;
	add_feature(s_pipe);
}
