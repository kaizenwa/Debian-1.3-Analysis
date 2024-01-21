/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "pipe.h"



/* Options specific to the pipe transport. They must be in alphabetic
order (note that "_" comes before the lower case letters). Those starting
with "*" are not settable by the user but are used by the option-reading
software for alternative value types. Some options are stored in the transport
instance block so as to be publicly visible; these are flagged with opt_public.
The "directory" option is obsolete, being replaced by "home_directory". */

optionlist pipe_transport_options[] = {
  { "*expand_group",     opt_stringptr | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, expand_gid)) },
  { "*expand_user",      opt_stringptr | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, expand_uid)) },
  { "*set_group",         opt_bool | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, gid_set)) },
  { "*set_user",          opt_bool | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, uid_set)) },
  { "batch",             opt_local_batch | opt_public,
      (void *)(offsetof(transport_instance, local_batch)) },
  { "batch_max",         opt_int | opt_public,
      (void *)(offsetof(transport_instance, batch_max)) },
  { "bsmtp",             opt_local_batch | opt_public,
      (void *)(offsetof(transport_instance, local_smtp)) },
  { "command",           opt_stringptr,
      (void *)(offsetof(pipe_transport_options_block, cmd)) },
  { "current_directory", opt_stringptr | opt_public,
      (void *)(offsetof(transport_instance, current_dir)) },
  { "delivery_date_add", opt_bool,
      (void *)(offsetof(pipe_transport_options_block, delivery_date_add)) },
  { "directory",         opt_stringptr | opt_public | opt_hidden,
      (void *)(offsetof(transport_instance, home_dir)) },
  { "envelope_to_add", opt_bool,
      (void *)(offsetof(pipe_transport_options_block, envelope_to_add)) },
  { "from_hack",         opt_bool,
      (void *)(offsetof(pipe_transport_options_block, from_hack)) },
  { "group",             opt_expand_gid | opt_public,
      (void *)(offsetof(transport_instance, gid)) },
  { "home_directory",    opt_stringptr | opt_public,
      (void *)(offsetof(transport_instance, home_dir)) },
  { "ignore_status",     opt_bool,
      (void *)(offsetof(pipe_transport_options_block, ignore_status)) },
  { "initgroups",        opt_bool | opt_public,
      (void *)(offsetof(transport_instance, initgroups)) },
  { "log_fail_output",   opt_bool | opt_public,
      (void *)(offsetof(transport_instance, log_fail_output)) },
  { "log_output",        opt_bool | opt_public,
      (void *)(offsetof(transport_instance, log_output)) },
  { "max_output",        opt_mkint,
      (void *)(offsetof(pipe_transport_options_block, max_output)) },
  { "path",              opt_stringptr,
      (void *)(offsetof(pipe_transport_options_block, path)) },
  { "pipe_as_creator",   opt_bool | opt_public,
      (void *)(offsetof(transport_instance, deliver_as_creator)) },
  { "prefix",            opt_stringptr,
      (void *)(offsetof(pipe_transport_options_block, prefix)) },
  { "restrict_to_path",  opt_bool,
      (void *)(offsetof(pipe_transport_options_block, restrict_to_path)) },
  { "retry_use_local_part", opt_bool | opt_public,
      (void *)offsetof(transport_instance, retry_use_local_part) },
  { "return_fail_output",opt_bool | opt_public,
      (void *)(offsetof(transport_instance, return_fail_output)) },
  { "return_output",     opt_bool | opt_public,
      (void *)(offsetof(transport_instance, return_output)) },
  { "return_path_add",   opt_bool,
      (void *)(offsetof(pipe_transport_options_block, return_path_add)) },
  { "suffix",            opt_stringptr,
      (void *)(offsetof(pipe_transport_options_block, suffix)) },
  { "timeout",           opt_time,
      (void *)(offsetof(pipe_transport_options_block, timeout)) },
  { "umask",             opt_octint,
      (void *)(offsetof(pipe_transport_options_block, umask)) },
  { "user",              opt_expand_uid | opt_public,
      (void *)(offsetof(transport_instance, uid)) },
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int pipe_transport_options_count =
  sizeof(pipe_transport_options)/sizeof(optionlist);

/* Default private options block for the pipe transport. */

pipe_transport_options_block pipe_transport_option_defaults = {
  NULL,           /* cmd */
  "/usr/bin",     /* path */
  "From ${if def:return_path{$return_path}{MAILER-DAEMON}} ${tod_bsdinbox}\n",
                  /* prefix */
  "\n",           /* suffix */
  022,            /* umask */
  20480,          /* max_output */
  60*60,          /* timeout */
  FALSE,          /* from_hack */
  FALSE,          /* ignore_status */
  FALSE,          /* return_path_add */
  FALSE,          /* delivery_date_add */
  FALSE,          /* envelope_to_add */
  FALSE           /* restrict_to_path */
};



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void
pipe_transport_init(transport_instance *tblock)
{
/*
pipe_transport_options_block *ob =
  (pipe_transport_options_block *)(tblock->options_block);
*/

/* Retry_use_local_part defaults TRUE if unset */

if (tblock->retry_use_local_part == 2) tblock->retry_use_local_part = TRUE;

/* If pipe_as_creator is set, then uid/gid should not be set. */

if (tblock->deliver_as_creator && (tblock->uid_set || tblock->gid_set ||
  tblock->expand_uid != NULL || tblock->expand_gid != NULL))
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "both pipe_as_creator and an explicit uid/gid are set for the %s "
        "transport", tblock->name);

/* If a fixed uid field is set, then a gid field must also be set. */

if (tblock->uid_set && !tblock->gid_set)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "user set without group for the %s transport", tblock->name);

/* Only one of return_output/return_fail_output or log_output/log_fail_output
should be set. */

if (tblock->return_output && tblock->return_fail_output)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "both return_output and return_fail_output set for %s transport",
    tblock->name);

if (tblock->log_output && tblock->log_fail_output)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "both log_output and log_fail_output set for %s transport",
    tblock->name);

/* If batch SMTP is set, ensure the generic local batch option matches */

if (tblock->local_smtp != local_batch_off)
  tblock->local_batch = tblock->local_smtp;
}




/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface details. */

void
pipe_transport_entry(
  transport_instance *tblock,      /* data for this instantiation */
  address_item *addr)              /* address(es) we are working on */
{
pid_t pid, outpid;
int i, fd, rc;
int pipefd[2];
int envcount = 0;
int argcount = 0;
int address_count = 0;
int max_args;
address_item *ad;
pipe_transport_options_block *ob =
  (pipe_transport_options_block *)(tblock->options_block);
BOOL smtp_dots = FALSE;
BOOL return_path_add = ob->return_path_add;
BOOL delivery_date_add = ob->delivery_date_add;
BOOL envelope_to_add = ob->envelope_to_add;
BOOL expand_arguments;
char **argv;
char *envp[20];
char *env_local_part;
char *s, *ss, *cmd;

DEBUG(2) debug_printf("%s transport entered\n", tblock->name);

/* Set up for the good case */

addr->transport_return = OK;
addr->basic_errno = 0;

/* Pipes are not accepted as general addresses, but they can be generated from
.forward files or alias files. In those cases, the command to be obeyed is
pointed to by addr->local_part; it starts with the pipe symbol. In other cases,
the command is supplied as one of the pipe transport's options. */

if (addr->local_part[0] == '|')
  {
  cmd = addr->local_part + 1;
  while (isspace(*cmd)) cmd++;
  expand_arguments = addr->expand_pipe;
  }
else
  {
  cmd = ob->cmd;
  expand_arguments = TRUE;
  }

/* Get store in which to build an argument list. Count the number of addresses
supplied, and allow for that many arguments, plus an additional 60, which
should be enough for anybody. Multiple addresses happen only when one of
the batch options is set. */

for (ad = addr; ad != NULL; ad = ad->next) address_count++;
max_args = address_count + 60;
argv = (char **)store_malloc((max_args+1)*sizeof(char *));

/* Split the command up into arguments terminated by white space. Lose
trailing space at the start and end. Double-quoted arguments can contain \\ and
\" escapes; single quotes may not. Copy each argument into a new string. The
total length can be no longer than the length of the original. */

s = cmd;
ss = store_malloc((int)strlen(s) + 1);
while (isspace(*s)) s++;

while (*s != 0 && argcount < max_args)
  {
  argv[argcount++] = ss;

  /* Handle single-quoted arguments */

  if (*s == '\'')
    {
    s++;
    while (*s != 0)
      {
      if (*s == '\'') break;
      *ss++ = *s++;
      }
    if (*s != 0) s++;
    }

  /* Handle double-quoted arguments */

  else if (*s == '\"')
    {
    s++;
    while (*s != 0)
      {
      if (*s == '\"') break;

      /* Handle escape sequences */

      else if (*s == '\\')
        {
        if (s[1] == 0) break;
        *ss++ = string_interpret_escape(&s);
        s++;
        }

      /* Not an escape sequence */

      else *ss++ = *s++;
      }

    /* Advance past terminator */

    if (*s != 0) s++;
    }

  /* Argument not in quotes is terminated by EOL or white space */

  else while (*s != 0 && !isspace(*s)) *ss++ = *s++;

  /* Terminate the current argument, and skip trailing spaces. */

  *ss++ = 0;
  while (isspace(*s)) s++;
  }

argv[argcount] = (char *)0;

/* If *s != 0 we have run out of argument slots. */

if (*s != 0)
  {
  addr->transport_return = FAIL;
  addr->message = string_sprintf("Too many arguments to command \"%s\" in "
    "%s transport", cmd, tblock->name);
  return;
  }

/* Expand each individual argument if required. Expansion happens for pipes set
up in filter files and with directly-supplied commands. It does not happen if
the pipe comes from a traditional .forward file. A failing expansion is a big
disaster if the command came from the transport's configuration; if it came
from a user it is just a normal failure.

An argument consisting just of the text "$pipe_addresses" is treated specially.
It is not passed to the general expansion function. Instead, it is replaced by
a number of arguments, one for each address. This avoids problems with shell
metacharacters and spaces in addresses. */

DEBUG(7)
  {
  debug_printf("pipe command:\n");
  for (i = 0; argv[i] != (char *)0; i++)
    debug_printf("  argv[%d] = %s\n", i, string_printing(argv[i], FALSE));
  }

if (expand_arguments)
  {
  for (i = 0; argv[i] != (char *)0; i++)
    {

    /* Handle special fudge for passing an address list */

    if (strcmp(argv[i], "$pipe_addresses") == 0 ||
        strcmp(argv[i], "${pipe_addresses}") == 0)
      {
      int additional;

      if (argcount + address_count - 1 > max_args)
        {
        addr->transport_return = FAIL;
        addr->message = string_sprintf("Too many arguments to command \"%s\" "
          "in %s transport", cmd, tblock->name);
        return;
        }

      additional = address_count - 1;
      if (additional > 0)
        memmove(argv + i + 1 + additional, argv + i + 1,
          (argcount - i)*sizeof(char *));

      for (ad = addr; ad != NULL; ad = ad->next) argv[i++] = ad->orig;
      i--;
      }

    /* Handle normal expansion string */

    else
      {
      char *expanded_arg = expand_string(argv[i]);
      if (expanded_arg == NULL)
        {
        addr->transport_return = (addr->local_part[0] == '|')? FAIL : PANIC;
        addr->message = string_sprintf("Expansion of \"%s\" "
          "from command \"%s\" in %s transport failed: %s",
          argv[i], cmd, tblock->name, expand_string_message);
        return;
        }
      argv[i] = expanded_arg;
      }
    }

  DEBUG(7)
    {
    debug_printf("pipe command after expansion:\n");
    for (i = 0; argv[i] != (char *)0; i++)
      debug_printf("  argv[%d] = %s\n", i, string_printing(argv[i], FALSE));
    }
  }


/* If restrict_to_path is set, check that the name of the command does not
contain any slashes. */

if (ob->restrict_to_path)
  {
  if (strchr(argv[0], '/') != NULL)
    {
    addr->transport_return = FAIL;
    addr->message = string_sprintf("\"/\" found in \"%s\" (command for %s "
      "transport) - failed for security reasons", cmd, tblock->name);
    return;
    }
  }

/* If the command is not an absolute path, search the PATH directories
for it. */

if (argv[0][0] != '/')
  {
  char *p;
  for (p = string_firstinlist(ob->path, ':'); p != NULL;
       p = string_nextinlist(':'))
    {
    struct stat statbuf;
    sprintf(big_buffer, "%s/%s", p, argv[0]);
    if (stat(big_buffer, &statbuf) == 0)
      {
      argv[0] = string_copy(big_buffer);
      break;
      }
    }
  if (p == NULL)
    {
    addr->transport_return = FAIL;
    addr->message = string_sprintf("\"%s\" command not found for %s transport",
      argv[0], tblock->name);
    return;
    }
  }


/* Set up the environment for the command. */

env_local_part = (deliver_localpart == NULL)? "" : deliver_localpart;

envp[envcount++] = string_sprintf("LOCAL_PART=%s", env_local_part);
envp[envcount++] = string_sprintf("LOGNAME=%s", env_local_part);
envp[envcount++] = string_sprintf("USER=%s", env_local_part);

envp[envcount++] = string_sprintf("DOMAIN=%s", (deliver_domain == NULL)?
  "" : deliver_domain);

envp[envcount++] = string_sprintf("HOME=%s", (deliver_home == NULL)?
  "" : deliver_home);

envp[envcount++] = string_sprintf("MESSAGE_ID=%s", message_id_external);
envp[envcount++] = string_sprintf("PATH=%s", ob->path);
envp[envcount++] = string_sprintf("QUALIFY_DOMAIN=%s", qualify_domain_sender);
envp[envcount++] = string_sprintf("SENDER=%s", sender_address);
envp[envcount++] = "SHELL=/bin/sh";

if (addr->host_list != NULL)
  envp[envcount++] = string_sprintf("HOST=%s", addr->host_list->name);

envp[envcount] = NULL;


/* If the -N option is set, can't do any more. */

if (dont_deliver)
  {
  debug_printf("*** delivery by %s transport bypassed by -N option",
    tblock->name);
  return;
  }


/* Handling the output from the pipe is tricky. If a file for catching this
output is provided, we could just hand that fd over to the pipe, but this isn't
very safe because the pipe might loop and carry on writing for ever (which is
exactly what happened in early versions of Exim). Therefore we must hand over a
pipe fd, read our end of the pipe and count the number of bytes that come
through, chopping the sub-process if it exceeds some limit.

However, this means we want to run a sub-process with both its input and output
attached to pipes. We can't handle that easily from a single parent process
using straightforward code such as the transport_write_message() function
because the subprocess might not be reading its input because it is trying to
write to a full output pipe. The complication of redesigning the world to
handle this is too great - simpler just to run another process to do the
reading of the output pipe. */


/* Make the pipe for handling the output - do this always, even if a
return_file is not provided. */

if (pipe(pipefd) < 0)
  {
  addr->transport_return = DEFER;
  addr->message = string_sprintf(
    "Failed to create pipe for handling output in %s transport",
      tblock->name);
  return;
  }

/* As this is a local transport, we are already running with the required
uid/gid, so pass NULL to child_open to indicate no change. */

if ((pid = child_open(argv, envp, ob->umask, NULL, NULL, &fd,
     pipefd[pipe_write])) < 0)
  {
  addr->transport_return = DEFER;
  addr->message = string_sprintf(
    "Failed to create child process for %s transport", tblock->name);
  return;
  }

/* Close off the end of the output pipe we are not using. */

close(pipefd[pipe_write]);

/* Now fork a process to handle the output that comes down the pipe. */

if ((outpid = fork()) < 0)
  {
  addr->basic_errno = errno;
  addr->transport_return = DEFER;
  addr->message = string_sprintf(
    "Failed to create process for handling output in %s transport",
      tblock->name);
  close(pipefd[pipe_read]);
  return;
  }

/* This is the code for the output-handling subprocess. Read from the pipe
in chunks, and write to the return file if one is provided. Keep track of
the number of bytes handled. If the limit is exceeded, try to kill the
subprocess, and in any case close the pipe and exit, which should cause the
subprocess to fail. */

if (outpid == 0)
  {
  int count = 0;
  close(fd);
  set_process_info("reading output from %s", cmd);
  while ((rc = read(pipefd[pipe_read], big_buffer, big_buffer_size)) > 0)
    {
    if (addr->return_file >= 0)
      write(addr->return_file, big_buffer, rc);
    count += rc;
    if (count > ob->max_output)
      {
      char *message = "\n\n*** Too much output - remainder discarded ***\n";
      DEBUG(2) debug_printf("Too much output from pipe - killed\n");
      if (addr->return_file >= 0)
        write(addr->return_file, message, (int)strlen(message));
      kill(pid, SIGKILL);
      break;
      }
    }
  close(pipefd[pipe_read]);
  _exit(0);
  }

close(pipefd[pipe_read]);  /* Not used in this process */


/* Carrying on now with the main parent process. Attempt to write the message
to it down the pipe. It is a fallacy to think that you can detect write errors
when the sub-process fails to read the pipe. The parent process may complete
writing and close the pipe before the sub-process completes. Sleeping for a bit
here lets the sub-process get going, but it may still not complete. So we
ignore all writing errors. */

DEBUG(2) debug_printf("Writing message to pipe\n");

/* If the local_smtp option is not unset, we need to write SMTP prefix
information. The various different values for batching are handled outside; if
there is more than one address available here, all must be included. Force
SMTP dot-handling. */

if (tblock->local_smtp != local_smtp_off)
  {
  address_item *a;
  char *sender = (addr->errors_address != NULL)?
    addr->errors_address : sender_address;
  smtp_dots = TRUE;
  return_path_add = delivery_date_add = envelope_to_add = FALSE;

  write(fd, "MAIL FROM: <", 12);
  write(fd, sender, (int)strlen(sender));
  write(fd, ">\n", 2);

  for (a = addr; a != NULL; a = a->next)
    {
    write(fd, "RCPT TO: <", 10);
    if (a->local_part[0] == ',' || a->local_part[0] == ':')
      {
      write(fd, "@", 1);
      write(fd, a->domain, (int)strlen(a->domain));
      write(fd, a->local_part, (int)strlen(a->local_part));
      }
    else
      {
      write(fd, a->local_part, (int)strlen(a->local_part));
      write(fd, "@", 1);
      write(fd, a->domain, (int)strlen(a->domain));
      }
    write(fd, ">\n", 2);
    }

  write(fd, "DATA\n", 5);
  }

/* Now any other configured prefix. */

if (ob->prefix != NULL)
  {
  char *prefix = expand_string(ob->prefix);
  if (prefix == NULL)
    {
    addr->transport_return = PANIC;
    addr->message = string_sprintf("Expansion of \"%s\" (prefix for %s "
      "transport) failed", ob->prefix, tblock->name);
    return;
    }
  write(fd, prefix, (int)strlen(prefix));
  }

(void) transport_write_message(addr, fd,
  (return_path_add? topt_add_return_path : 0) |
  (delivery_date_add? topt_add_delivery_date : 0) |
  (envelope_to_add? topt_add_envelope_to : 0) |
  (ob->from_hack? topt_from_hack : 0 ) |
  (smtp_dots? topt_smtp_dots : 0),
  addr->errors_address, 0);   /* no CRLF */

/* Now any configured suffix */

if (ob->suffix != NULL)
  {
  char *suffix = expand_string(ob->suffix);
  if (suffix == NULL)
    {
    addr->transport_return = PANIC;
    addr->message = string_sprintf("Expansion of \"%s\" (suffix for %s "
      "transport) failed", ob->suffix, tblock->name);
    return;
    }
  write(fd, suffix, (int)strlen(suffix));
  }

/* If local_smtp, write the terminating dot. */

if (tblock->local_smtp != local_smtp_off) write(fd, ".\n", 2);

/* OK, the writing is now all done. Close the pipe. */

(void) close(fd);

/* Wait for the child process to complete and take action if the returned
status is nonzero or if something went wrong. */

if ((rc = child_close(pid, ob->timeout)) != 0)
  {
  /* The process did not complete in time; kill it and fail the delivery. */

  if (rc == -256)
    {
    kill(pid, SIGKILL);
    addr->transport_return = FAIL;
    addr->message = string_sprintf("pipe delivery process timed out",
      tblock->name);
    }

  /* Wait() failed. */

  else if (rc == -257)
    {
    addr->transport_return = PANIC;
    addr->message = string_sprintf("Wait() failed for child process of %s "
      "transport: %s", tblock->name, strerror(errno));
    }

  /* Either the process completed, but yielded a non-zero (necessarily
  positive) status, or the process was terminated by a signal (rc will contain
  the negation of the signal number). Treat killing by signal as failure unless
  status is being ignored. */

  else if (rc < 0)
    {
    if (!ob->ignore_status)
      {
      addr->transport_return = FAIL;
      addr->message = string_sprintf("Child process of %s transport (running "
        "command \"%s\") was terminated by signal %d", tblock->name, cmd, -rc);
      }
    }

  /* For positive values (process terminated with non-zero status), we need a
  status code to request deferral. A number of systems contain the following
  line in sysexits.h:

      #define EX_TEMPFAIL 75 temp failure; user is invited to retry

  Based on this, use exit code EX_TEMPFAIL to mean "defer" when not ignoring
  the returned status. Otherwise fail. */

  else
    {
    if (!ob->ignore_status)
      {
      int size, ptr, i;
      addr->transport_return = (rc == EX_TEMPFAIL)? DEFER : FAIL;

      /* Ensure the message contains the expanded command and arguments. This
      doesn't have to be brilliantly efficient - it is an error situation. */

      addr->message = string_sprintf("Child process of %s transport returned "
        "%d from command:", tblock->name, rc);
      ptr = (int)strlen(addr->message);
      size = ptr + 1;
      for (i = 0; i < sizeof(argv)/sizeof(int *) && argv[i] != NULL; i++)
        {
        BOOL quote = FALSE;
        addr->message = string_cat(addr->message, &size, &ptr, " ", 1);
        if (strpbrk(argv[i], " \t") != NULL)
          {
          quote = TRUE;
          addr->message = string_cat(addr->message, &size, &ptr, "\"", 1);
          }
        addr->message = string_cat(addr->message, &size, &ptr, argv[i],
          (int)strlen(argv[i]));
        if (quote)
          addr->message = string_cat(addr->message, &size, &ptr, "\"", 1);
        }
      addr->message[ptr] = 0;  /* Ensure concatenated string terminated */
      }
    }
  }

/* The child_close() function just calls wait() until the required pid
is returned. Therefore it might already have waited for the output handling
process. In case it hasn't, do some more waiting here. All subprocesses
should be done before we pass this point. */

while (wait(&rc) >= 0);

DEBUG(2) debug_printf("%s transport yielded %d\n", tblock->name,
  addr->transport_return);
}

/* End of transport/pipe.c */
