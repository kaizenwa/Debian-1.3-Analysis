/* ########################################################################
   
   SMAC FILE USED BY XCORAL EDITOR
   
   File: rcs.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/rcs.sc
   Description: 
   Created: Sun Aug  7 15:52:06 MET 1994
   Author: Thierry Emery
   Modified: Sun Aug  7 15:53:53 MET 1994
   Last maintained by: Lionel Fournigault
   
   RCS $Revision: 1.1 $ $State: Exp $
   
   
   ########################################################################
   
   Note: SMAC interface for RCS (ci, co, rlog, rcsdiff)
   
   Defines: rcs_initialize, rcs_check_in, rcs_check_out_locked,
    rcs_check_out_unlocked, rcs_check_in_and_out_locked,
    rcs_check_in_and_out_locked, rcs_lock_revision, rcs_unlock_revision,
    rcs_diff, rcs_log, rcs_repository.
   
   Suggested bindings:
   "^x^i" "rcs_check_in"
   "^x^o" "rcs_check_out_locked"
   "^x^a" "rcs_check_in_and_out_locked"
   "^x^l" "rcs_log"
   
   Procedure: From each file's window, CheckIn once, then alternate CheckOut
   and CheckIn, or repeat CheckInAndOut ... (see ci & co in man)
   
   If you modify a buffer whose file is not checked out, you will
   get a `Permission denied' message if you attempt to save it.
   You can then check it out (execute `CheckOut' from its window)
   and save it.
   
   To print the RCS log for the current file, execute `Rlog'
   
   To compare the current file to an existing revision, execute
   `RcsDiff'
   
   Warnings: - Initial description is not handled yet (and is even deleted
   by CheckIn and CheckInAndOut)
   - Executing CheckOut on a checked out file freezes Xcoral; you
   then have to kill the process executing the `co' command
   (should be tested in a future revision of this interface)
   
   ########################################################################
   
   Copyright (c) : Thierry Emery
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   
   ######################################################################## */

{
  if (! function("does_unix_command_exist"))
    load_file("utilities.sc");
  if (! function("cmd"))
    load_file("cmd.sc");
}

/* -------------------------------------------------------------------------
   Rcs commands : available.
   ------------------------------------------------------------------------- */
int rcs_available = 2;
int is_rcs_available()
{
    /* On a besoin de rcs, rlog, ci, co et rcsdiff */
    if (rcs_available != 2) {
	if (rcs_available == 0)
	  display_message("RCS not available", "RCS", 1);
	return rcs_available;
    }
    if (! does_unix_command_exist("rcs"))
      rcs_available = 0;
    else if (! does_unix_command_exist("rlog"))
      rcs_available = 0;
    else if (! does_unix_command_exist("ci"))
      rcs_available = 0;
    else if (! does_unix_command_exist("co"))
      rcs_available = 0;
    else if (! does_unix_command_exist("rcsdiff"))
      rcs_available = 0;
    else
      rcs_available = 1;
    
    if (rcs_available == 0)
      display_message("RCS not available", "RCS", 1);
    return rcs_available;
}

/* -------------------------------------------------------------------------
   Check current mode.
   ------------------------------------------------------------------------- */
int is_mode_ok()
{
  char *mode = current_mode();
  
  if ( (strcmp(mode,"Shell") == 0)  || (strcmp(mode,"Edir") == 0) ) {
    display_message("RCS not available in this mode", "RCS", 1);
    return 0;
  }
  
  return 1;
}

/* -------------------------------------------------------------------------
   Try to determine comment leader (redefinable)
   ------------------------------------------------------------------------- */

char* check_in_comment_leader() {
    
    if (the_char(0) == '/' && the_char(1) == '*')
      return "   ";
    else
      return "# ";
}

/* -----------------------------------------------------------------------------------
   Checks if a revision is locked. Return 0 if unlocked else username.
   Arguments filename and revision must be valid.
   The result, if not 0, must be free by the caller function.
   -----------------------------------------------------------------------------------*/

char *locked_by (char *filename, char *revision)
{
    char *result;
    int len;
    char *the_cmd = "rlog -r%s %s | grep revision | grep locked | tail -1 | awk '{print $5}'";
    char *tmp = (char *) malloc (strlen(filename) + strlen(revision)+ strlen(the_cmd) + 5 );
				 
    sprintf(tmp, the_cmd, revision, filename);
   
    result = cmd_shell_to_string (tmp);
    free(tmp);
    if (result)
      len = strlen(result);
    else
      return 0;
	
    if (len < 2)
      return 0;
     /* Pour virer le point virgule si il exsite */
    if (result[strlen(result)-1] == ';')
      result[strlen(result)-1]= 0;

    return (result);
}

/* -----------------------------------------------------------------------------------
   Checks how many time a file is locked
   Argument filename must be valid. Return true if the file is locked more than once
   else return false.
   -----------------------------------------------------------------------------------*/

int is_locked_more_once (char *filename)
{
    char *result;
    char *cmd = "rlog %s | grep -i 'locked by' | wc -l | awk '($1 > 1){print $1}'";
    char *tmp = (char *) malloc (strlen(filename) + strlen(cmd) + 5 );
    int val;
				 
    sprintf(tmp, cmd, filename);
    result = cmd_shell_to_string (tmp);
    if (result == 0)
	val = 0;
    else
       val = 1;

    if(result)
      free(result);
    free(tmp);
    return(val);
}

int is_check_in_once(char *file)
{
    char *result;
    char *cmd = "rlog %s 2>&1 | grep rlog | head -1 | awk -F: '{print $1}'";
    char *tmp = (char *) malloc(strlen(file) + strlen(cmd) + 5);
    
    sprintf(tmp, cmd, file );
    result = cmd_shell_to_string (tmp);
    if (result && strcmp(result,"rlog error")==0) {
	free(result);
	free(tmp);
	return 0;
    }
    return 1;
}

int rcs_check_in_perm(char *file)
{
    char *tmp = (char *) malloc (320);
    char *username=0, *last_revision, *lock, *result;
    int error;

    /* Le fichier existe-il */
    if ( ! does_file_exist (file)) {
	result = basename(file);
	sprintf(tmp,"%s : file not found\n", result );
	display_message(tmp,"Check In error", 1);
	free(result);
	return 0;
    }
    
    /* 
       Il faut verifier si le fichier a deja 
       subit un check in
    */
    if(! is_check_in_once(file)) {
	result = basename(file);
	sprintf(tmp,"rlog error: RCS/%s : No such file or directory",
		result);
	display_message(tmp, "Rcs initialize",1);
	free(tmp);
	if(result)
	  free(result);
	return 0;
    }
    
    username = cmd_shell_to_string ("whoami");
    if (is_locked_more_once(file)) {
	/* Several locks */
	if(username)
	  free(username);
	display_message("Warning : multiple revisions locked","Check_In",1);
	sprintf(tmp,"rlog -l %s | grep -i 'locked by'", file);
	cmd(tmp); 
	free(tmp);
	return 0;
    }

    sprintf(tmp,"rlog -l %s | grep head | tail -1 | awk '{print $2}'\n", file);
    last_revision = cmd_shell_to_string (tmp);
    
    if ((lock = locked_by (file, last_revision)) == 0) {
	sprintf(tmp,"Check In  error: no lock set by %s\nYou must check out locked before\n",
		username);
	error = 1;
	display_message(tmp,"Check In Error", 1);
    }
    else if (strcmp(username, lock) != 0) {
	sprintf(tmp,"Check In error: lock set by %s\n", lock);
	error = 1;
	display_message(tmp,"Check In Error", 1);
    }
    else
     ;
    
    free(tmp);
    if (username)
      free(username);
    if (lock)
      free(lock);
    if (last_revision)
      free(last_revision);
    
    if (error)
      return 0;
    else
      return 1;
}
    
/* -------------------------------------------------------------------------
   Checks in an RCS file. Return 1 if success else 0.
   ------------------------------------------------------------------------- */

int rcs_check_in()
{
    char *tmp;
    char *file = filename();
    char *result, *ci_command, *comment_leader;
    char *username, *lock, *last_revision, *log_msg;
    int error = 0;
 
    if (! is_mode_ok())
      return;

    if (! is_rcs_available())
	return;

    tmp = (char *) malloc (320);
    
    if ( file ) {
	watch_on();
	if (! rcs_check_in_perm(file)) {
	    free(file);
	    watch_off();
	    return 0;
	}
	    
	save_file();
	log_msg = gets("Log message : ");
	if (log_msg == 0) {
	    log_msg = (char *)malloc(32);
	    strcpy(log_msg,"No log message");
	}
	comment_leader = check_in_comment_leader();
	
	if (!gets_string_cancelled(log_msg)) {
	  ci_command = (char*) malloc(3*strlen(file)+strlen(comment_leader)
				      +strlen(log_msg)+60);
	  sprintf(ci_command,"ci -t-\"\" -m\"%s\" %s; rcs -c\"%s\" %s",
		  log_msg,file,comment_leader,file);

	  free(log_msg);
	  cmd(ci_command);
	  redisplay();
	  
	  /* Il faut verifier que le fichier n'est plus la */
	  wait_for_nofile(file);

	  raise_window();
	  kill_current_buffer();
	}
	else {
	  watch_off();
	  return 0;
	}
	free(file);
	watch_off();
    }
    else {
	display_message("No file to check in","Check In",1);
    }

    return 1;
}

/* -------------------------------------------------------------------------
   Check for a valid revision. Return 1 if success else 0
   ------------------------------------------------------------------------- */

int is_valid_revision(char *file, char *revision)
{
    char *tmp = (char *) malloc (320);
    char *result;
    int val = 0;

    if (file == 0 || revision == 0 || (! is_num(revision))) {
	free(tmp);
	return val;
    }
    
    sprintf(tmp,"rlog %s | grep -i revision | grep %s", file, revision);
    result = cmd_shell_to_string (tmp);
    if ( result != 0 ) {
	val = 1;
	free(result);
    }
    else {
	val = 0;
    }
    free(tmp);
    
    return(val);
}

/* -------------------------------------------------------------------------
   check out permissions
   ------------------------------------------------------------------------- */

int rcs_check_out_perm(char *file, int locked, char *revision)
{
  char *username, *tmp = 0,*lock = 0;
  int error = 0;
  char *tmp = (char *) malloc(64);
    
    /* La revision est-elle valid */
    if ( (revision != 0) && (! is_valid_revision(file,revision))) {
	if (! (*revision == 7)) {
	    sprintf(tmp,"Revision %s is not valid", revision);
	    display_message(tmp, "Revision", 1);
	}
	free(tmp);
	return 0;
    }

    username = cmd_shell_to_string ("whoami");
    if (revision == 0) {
	tmp = (char *) malloc(64);
	sprintf(tmp,"rlog -l %s | grep head | tail -1 | awk '{print $2}'\n", file);
	revision = cmd_shell_to_string (tmp);
	free(tmp);
    }
    lock = locked_by (file, revision);		
    
    if(lock && locked) {
	/* Check out sur un fichier deja locked */
	if (strcmp(username, lock) != 0) {
	    tmp = (char *) malloc(64);
	    sprintf(tmp,"Check Out error: already locked by %s\n", lock);
	    display_message(tmp,"Check Out Error", 1);
	    free(tmp);
	    error = 0;
	}
    }

    if(username)
      free(username);
    if(lock)
      free(lock);
    
    if(error)
      return 0;
    else
      return 1;
}
    
/* -------------------------------------------------------------------------
   check out a revision
   ------------------------------------------------------------------------- */

void rcs_check_out_internal (char *file, int lock, char *revision)
{
    char *co_command;
    char *tmp = (char *) malloc(64);
    int origin_win=current_window();

    if ( ! rcs_check_out_perm(file,lock,revision))
      return;
    
    co_command = (char*) malloc(strlen(file)+16);
    if (revision)
      lock ? sprintf(co_command,"co -l -r%s %s", revision, file) :
	sprintf(co_command,"co -r%s %s", revision, file);
    else 
      lock ? sprintf(co_command,"co -l %s", file) :
	sprintf(co_command,"co %s",file);
      
    cmd(co_command);
    wait_for_file(file);
    free(co_command);
    select_window(origin_win);
    raise_window();
    read_file(file);
}

/* -------------------------------------------------------------------------
   check file, revision then check out
   ------------------------------------------------------------------------- */

void rcs_check_out(int lock)
{
    char *file, *revision;
    char *file = filename();
    char *reply, *result;
    char *tmp;
    
    if (! is_mode_ok())
      return;

    if (! is_rcs_available())
	return;
    
    tmp =  (char *) malloc(128);
    if (!file) {
	file = gets("Check out file : ");
	if (gets_string_cancelled(file)) {
	    file = 0;
	}
	else {
	    revision = gets("Revision number [last] : ");
	}
    }
    else {
	reply = gets("Warning : erase current buffer [n/y] : ");
	if ( reply == 0 || reply == 7 || *reply == 'n' ) {
	    if (reply)
	      free(reply);
	    free(file);
	    return;
	}
	else {
	    if (*reply != 'y') {
		free(reply);
		free(file);
		return;
	    }
	}
    }
    if (file) {
	if(! is_check_in_once(file)){
	    result = basename(file);
	    sprintf(tmp,"rlog error: RCS/%s : No such file or directory",
		    result);
	    display_message(tmp, "Rcs check out",1);
	    free(tmp);
	    if(result)
	      free(result);
	    free(file);
	    return;
	}
	rcs_check_out_internal(file, lock, revision);
	free(file);
    }
}

/* -------------------------------------------------------------------------
   Check an RCS file out locked (read-write) for user
   ------------------------------------------------------------------------- */

void rcs_check_out_locked()
{
    watch_on();
    rcs_check_out(1);
    watch_off();
}

/* -------------------------------------------------------------------------
   Check an RCS file out unlocked (read-only) for user
   ------------------------------------------------------------------------- */

void rcs_check_out_unlocked()
{
    watch_on();
    rcs_check_out(0);
    watch_off();
}

/* -------------------------------------------------------------------------
   Check an RCS file in/out 
   ------------------------------------------------------------------------- */

void rcs_check_in_out_internal (int lock)
{
    char *file = filename();

    if (! is_mode_ok())
      return;

    if (! is_rcs_available())
      return;
    
    if (file) {
	if (! rcs_check_in()) {
	    free(file);
	    return 0;
	}
	rcs_check_out_internal(file, lock, 0);
    }
    free(file);
}

/* -------------------------------------------------------------------------
   Checks in an RCS file and checks it back out locked (read-write)
   ------------------------------------------------------------------------- */

void rcs_check_in_and_out_locked()
{
    watch_on();
    rcs_check_in_out_internal (1);
    watch_off();
}

/* -------------------------------------------------------------------------
   Checks in an RCS file and checks it back out unlocked (read-only)
   ------------------------------------------------------------------------- */

void rcs_check_in_and_out_unlocked()
{
    watch_on();
    rcs_check_in_out_internal (0);
    watch_off();
}


/* -------------------------------------------------------------------------
   print RCS log for current file
   ------------------------------------------------------------------------- */

void rcs_log() {
    
    char *log_command, *file;
    
    if (! is_mode_ok())
      return;

    if (! is_rcs_available())
      return;

    file = filename();
    
    watch_on();
    if (file) {
	log_command = (char*) malloc(strlen(file)+6);
	sprintf(log_command,"rlog %s",file);
	free(file);
	cmd(log_command);
	free(log_command);
    }
    watch_off();
}

/* -------------------------------------------------------------------------
   Does an RCS diff for current file
   ------------------------------------------------------------------------- */

void rcs_diff() {
    
    char *diff_command, *file, *revision;
    char *tmp = (char *) malloc(64);

    if (! is_mode_ok())
      return;

    if (! is_rcs_available())
      return;

    file = filename();
    
    watch_on();
    if (file) {
	revision = gets("rcs diff with revision : ");
	if (!gets_string_cancelled(revision)) {
	    if ( ! is_valid_revision(file,revision)) {
		sprintf(tmp,"Revision %s is not valid", revision);
		display_message(tmp, "Revision", 1);
		free(tmp);
		free(file);
		free(revision);
		watch_off();
		return;
	    }
	    diff_command = (char*) malloc(strlen(revision)+strlen(file)+13);
	    sprintf(diff_command,"rcsdiff -r%s %s",revision,file);
	    cmd(diff_command);
	    free(diff_command);
	}
	
	if (revision)
	  free(revision);
	free(file);
    }
    watch_off();
}

/* -------------------------------------------------------------------------
   Display RCS directorie contents
   ------------------------------------------------------------------------- */

void rcs_repository()
{
    if (! is_mode_ok())
      return;

    watch_on();
    cmd("ls -lt RCS/*");
    watch_off();
}

/*
**	Function name : rcs_lock_internal
**
**	Description :
**	Input :
**	Output :
*/
void rcs_lock_revision_internal(int lock)
{
    char *file, *revision, *userlock, *username, *result;
    char *tmp = (char *) malloc(128);
    
    file = gets("Enter file name : ");
    if (gets_string_cancelled(file))
      return;
    revision = gets("Enter revision number : ");
    if (gets_string_cancelled(revision))
      return;

    /* 
       Il faut verifier si le fichier a deja 
       subit un check in
    */
    if(! is_check_in_once(file)) {
	result = basename(file);
	sprintf(tmp,"rlog error: RCS/%s : No such file or directory", result);
	free(result);
	display_message(tmp,"Rcs lock error", 1);
	return;
    }
    /* 
       Il faut verifier que la revision est valide
    */
    if (! is_valid_revision(file,revision)) {
	if (! (*revision == 7)) {
	    sprintf(tmp,"Revision %s is not valid", revision);
	    display_message(tmp, "Revision", 1);
	    free(tmp);
	    return;
	}
    }
    userlock = locked_by(file,revision);
    username = cmd_shell_to_string ("whoami");
    
    if (lock) {
	/* On veut locker */
	if (userlock) {
	    sprintf(tmp,"Revision %s already locked", revision);
	    display_message(tmp, "Revision", 1);
	    free(tmp);
	    return;
	}
	else {
	    sprintf(tmp,"rcs -l%s %s 2>&1", revision, file);
	    result = cmd_shell_to_string (tmp);
	}
	free(tmp);
    }
    else {
	/* On veut unlocker */
	if (userlock) {
	    if (strcmp(username,userlock) != 0){
		sprintf(tmp,"Sorry revision %s is locked by %s",
			revision, username );
		display_message(tmp, "Revision", 1);
		return;
	    }
	    else {
		sprintf(tmp,"rcs -u%s %s 2>&1", revision, file);
		result = cmd_shell_to_string (tmp);
	    }
	    free(tmp);
	}
	else {
	    sprintf(tmp,"Revision %s already unlocked", revision);
	    display_message(tmp, "Revision", 1);
	    free(tmp);
	    return;
	}
    }
}

/*
**	Function name : rcs_lock_revision
**
**	Description :
**	Input :
**	Output :
*/
void rcs_lock_revision()
{
    if (! is_mode_ok())
      return;

    if (! is_rcs_available())
      return;

    rcs_lock_revision_internal(1);
}

/*
**	Function name : rcs_unlock_revision
**
**	Description :
**	Input :
**	Output :
*/
void rcs_unlock_revision()
{
    if (! is_mode_ok())
      return;

    if (! is_rcs_available())
      return;

    rcs_lock_revision_internal(0);
}

/*
**	Function name : rcs_initialize
**
**	Description :
**	Input :
**	Output :
*/
void rcs_initialize()
{
    char *tmp;
    char *result, *file, *username;
 
    if (! is_mode_ok())
      return;

    if (! is_rcs_available())
	return;

    file = filename();
    if (!file) {
	file = gets("Enter file name : ");
	if (gets_string_cancelled(file))
	  return;
    }
    
    tmp =  (char *) malloc(256);
    watch_on();
    /* Il faut verifier si le fichier a deja subit un check in */
    if(is_check_in_once(file)) {
	sprintf(tmp, "%s already initialysed", basename(file));
	display_message(tmp, "Rcs initialize",1);
	free(tmp);
	watch_off();
	return;
    }

    /* Create and initialize a new RCS file,
       Append the login name in access list */
    username = cmd_shell_to_string ("whoami");
    if (!username) {
	display_message("User name error", "Rcs initialize",1);
	free(tmp);
	watch_off();
	return;
    }

    sprintf(tmp,"rcs -i -t-\"noting\" -a%s %s > /dev/null 2>&1", 
	    username, file);
    result = cmd_shell_to_string(tmp);

    /* RCS dir exist ? */
    sprintf(tmp,"%s/RCS",dirname(file));
    if (is_dir(tmp))
      sprintf(tmp,"%s/RCS/%s,v", dirname(file), basename(file));
    else
      sprintf(tmp,"%s,v", file);

    wait_for_file (tmp);
    
    sprintf(tmp,"ci %s > /dev/null 2>&1", file);
    result = cmd_shell_to_string(tmp);
    wait_for_nofile(file);
    sprintf(tmp,"co %s > /dev/null 2>&1", file);
    result = cmd_shell_to_string(tmp);
    
    sprintf(tmp, "Done : %s 1.1 is now initialized", basename(file));
    display_message(tmp, "Rcs initialize",1);

    free(username);
    free(tmp);
    
    watch_off();
}

