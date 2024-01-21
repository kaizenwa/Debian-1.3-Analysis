/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* Code for mail filtering functions. */


#include "exim.h"


typedef struct filter_cmd {
  struct filter_cmd *next;
  int command;
  BOOL seen;
  BOOL noerror;
  void *args[1];
} filter_cmd;

typedef struct condition_block {
  struct condition_block *parent;
  int type;
  BOOL testfor;
  void *left;
  void *right;
} condition_block;

typedef struct alias {
  struct alias *next;
  char *name;
} alias;


static char **error_pointer;
static char *log_filename;
static int *special_action;
static int  line_number;
static int  expect_endif;
static int  had_else_endif;
static int  log_fd;
static int  log_mode;
static int  output_indent;
static BOOL as_user;
static BOOL no_log;
static BOOL seen_force;
static BOOL seen_value;
static BOOL noerror_force;

enum { had_neither, had_else, had_elif, had_endif };

static BOOL read_command_list(char **, filter_cmd ***, BOOL, BOOL);


/* Condition identities. */

enum { cond_and, cond_or, cond_personal, cond_is, cond_matches, cond_contains,
       cond_delivered, cond_above, cond_below, cond_errormsg };


/* Command identities: must be kept in step with the list
of command words which follows. */

enum { deliver_command, elif_command, else_command, endif_command,
       finish_command, fail_command, freeze_command, if_command,
       log_command, logfile_command,
       mail_command, noerror_command, pipe_command, save_command,
       seen_command, testprint_command, unseen_command, vacation_command };

static char *command_list[] = {
  "deliver",
  "elif",
  "else",
  "endif",
  "finish",
  "fail",
  "freeze",
  "if",
  "log",
  "logfile",
  "mail",
  "noerror",
  "pipe",
  "save",
  "seen",
  "testprint",
  "unseen",
  "vacation"
};

static int command_list_count = sizeof(command_list)/sizeof(char *);

/* The arguments for the mail command. All the text ones come first. */

static char *mailargs[] = {  /* "to" must be first, and */
  "to",                      /* "cc" and "bcc" must follow */
  "cc",
  "bcc",
  "subject",
  "text",
  "file",
  "log",
  "once"
};

/* This defines the offsets for the arguments; first the string ones, and
then the non-string ones. The order must be as above. */

enum { mailarg_index_to, mailarg_index_cc, mailarg_index_bcc,
       mailarg_index_subject, mailarg_index_text, mailarg_index_file,
       mailarg_index_log, mailarg_index_once,
       mailarg_index_expand,       /* first non-string argument */
       mailargs_total              /* total number of arguments */
       };

/* The count of string arguments */

#define mailargs_string_count mailarg_index_expand

/* Offsets in the data structure for the string arguments */

static int reply_offsets[] = {  /* must be in same order as above */
  offsetof(reply_item, to),
  offsetof(reply_item, cc),
  offsetof(reply_item, bcc),
  offsetof(reply_item, subject),
  offsetof(reply_item, text),
  offsetof(reply_item, file),
  offsetof(reply_item, logfile),
  offsetof(reply_item, oncelog)
};




/*************************************************
*          Find next significant char            *
*************************************************/

/* Function to skip over white space and, optionally, comments.

Arguments:
  ptr              pointer to next character
  comment_allowed  if TRUE, comments (# to \n) are skipped

Returns:           pointer to next non-whitespace character
*/

char *
nextsigchar(char *ptr, BOOL comment_allowed)
{
for (;;)
  {
  while (isspace(*ptr))
    {
    if (*ptr == '\n') line_number++;
    ptr++;
    }
  if (comment_allowed && *ptr == '#')
    {
    while (*(++ptr) != '\n' && *ptr != 0);
    continue;
    }
  else break;
  }
return ptr;
}



/*************************************************
*                Read one word                   *
*************************************************/

/* The terminator is white space unless bracket is TRUE, in which
case ( and ) terminate.

Arguments
  ptr       pointer to next character
  buffer    where to put the word
  size      size of buffer
  bracket   if TRUE, terminate on ( and ) as well as space

Returns:    pointer to the next significant character after the word
*/

char *
nextword(char *ptr, char *buffer, int size, BOOL bracket)
{
char *bp = buffer;
while (*ptr != 0 && !isspace(*ptr) &&
       (!bracket || (*ptr != '(' && *ptr != ')')))
  {
  if (bp - buffer < size - 1) *bp++ = *ptr++; else
    {
    *error_pointer = string_sprintf("word is too long in line %d of "
      "filter file", line_number);
    break;
    }
  }
*bp = 0;
return nextsigchar(ptr, TRUE);
}



/*************************************************
*                Read one item                   *
*************************************************/

/* Might be a word, or might be a quoted string; in the latter case
do the escape stuff.

Arguments:
  ptr        pointer to next character
  buffer     where to put the item
  size       size of buffer
  bracket    if TRUE, terminate non-quoted on ( and ) as well as space

Returns:     the next significant character after the item
*/

char *
nextitem(char *ptr, char *buffer, int size, BOOL bracket)
{
char *bp = buffer;
if (*ptr != '\"') return nextword(ptr, buffer, size, bracket);
while (*(++ptr) != 0 && *ptr != '\"' && *ptr != '\n')
  {
  if (bp - buffer >= size - 1)
    {
    *error_pointer = string_sprintf("string is too long in line %d of "
      "filter file", line_number);
    break;
    }

  if (*ptr != '\\') *bp++ = *ptr; else
    {
    if (isspace(ptr[1]))    /* \<whitespace>NL<whitespace> ignored */
      {
      char *p = ptr + 1;
      while (*p != '\n' && isspace(*p)) p++;
      if (*p == '\n')
        {
        line_number++;
        ptr = p;
        while (ptr[1] != '\n' && isspace(ptr[1])) ptr++;
        continue;
        }
      }

    *bp++ = string_interpret_escape(&ptr);
    }
  }

if (*ptr == '\"') ptr++;
  else *error_pointer = string_sprintf("quote missing at end of string "
    "in filter file line %d", line_number);

*bp = 0;
return nextsigchar(ptr, TRUE);
}




/*************************************************
*          Convert a string + K|M to a number    *
*************************************************/

/*
Arguments:
  s        points to text string
  OK       set TRUE if a valid number was read

Returns:   the number, or 0 on error (with *OK FALSE)
*/

static int
get_number(char *s, BOOL *OK)
{
int value, count;
*OK = FALSE;
if (sscanf(s, "%i%n", &value, &count) != 1) return 0;
if (tolower(s[count]) == 'k') { value *= 1024; count++; }
if (tolower(s[count]) == 'm') { value *= 1024*1024; count++; }
while (isspace(s[count])) count++;
if (s[count] != 0) return 0;
*OK = TRUE;
return value;
}



/*************************************************
*            Read one condition                  *
*************************************************/

/* A complete condition must be terminated by "then"; bracketed internal
conditions must be terminated by a closing bracket. They are read by calling
this function recursively.

Arguments:
  ptr             points to start of condition
  condition_block where to hang the created condition block
  toplevel        TRUE when called at the top level

Returns:          points to next character after "then"
*/

static char *
read_condition(char *ptr, condition_block **cond, BOOL toplevel)
{
char buffer[256];
BOOL testfor = TRUE;
condition_block *current_parent = NULL;
condition_block **current = cond;

*current = NULL;

/* Loop to read next condition */

for (;;)
  {
  condition_block *c;

  /* reaching the end of the input is an error. */

  if (*ptr == 0)
    {
    *error_pointer = string_sprintf("\"then\" missing at end of filter file");
    break;
    }

  /* Opening bracket at the start of a condition introduces a nested
  condition, which must be terminated by a closing bracket. */

  if (*ptr == '(')
    {
    ptr = read_condition(nextsigchar(ptr+1, TRUE), &c, FALSE);
    if (*error_pointer != NULL) break;
    if (*ptr != ')')
      {
      *error_pointer = string_sprintf("expected \")\" in line %d of "
        "filter file", line_number);
      break;
      }
    if (!testfor)
      {
      c->testfor = !c->testfor;
      testfor = TRUE;
      }
    ptr = nextsigchar(ptr+1, TRUE);
    }


  /* Closing bracket at the start of a condition is an error. Give an
  explicit message, as otherwise "unknown condition" would be confusing. */

  else if (*ptr == ')')
    {
    *error_pointer = string_sprintf("unexpected \")\" in line %d of "
      "filter file", line_number);
    break;
    }

  /* Otherwise we expect a word or a string. */

  else
    {
    ptr = nextitem(ptr, buffer, sizeof(buffer), TRUE);
    if (*error_pointer != NULL) break;

    /* "Then" at the start of a condition is an error */

    if (strcmp(buffer, "then") == 0)
      {
      *error_pointer = string_sprintf("unexpected \"then\" near line %d of "
        "filter file", line_number);
      break;
      }

    /* "Not" at the start of a condition negates the testing condition. */

    if (strcmp(buffer, "not") == 0)
      {
      testfor = !testfor;
      continue;
      }

    /* Build a condition block from the specific word. */

    c = store_malloc(sizeof(condition_block));
    c->left = c->right = NULL;
    c->testfor = testfor;
    testfor = TRUE;

    /* Check for conditions that start with a keyword */

    if (strcmp(buffer, "delivered") == 0) c->type = cond_delivered;
    else if (strcmp(buffer, "error_message") == 0) c->type = cond_errormsg;

    /* Personal can be followed by any number of aliases */

    else if (strcmp(buffer, "personal") == 0)
      {
      c->type = cond_personal;
      for (;;)
        {
        alias *a;
        char *saveptr = ptr;
        ptr = nextword(ptr, buffer, sizeof(buffer), TRUE);
        if (*error_pointer != NULL) break;
        if (strcmp(buffer, "alias") != 0)
          {
          ptr = saveptr;
          break;
          }
        ptr = nextitem(ptr, buffer, sizeof(buffer), TRUE);
        if (*error_pointer != NULL) break;
        a = store_malloc(sizeof(alias));
        a->name = string_copy(buffer);
        a->next = (alias *)(c->left);
        c->left = (void *)a;
        }
      }

    /* If it's not a word we recognize, then it must be the lefthand
    operand of one of the comparison words. */

    else
      {
      char *isptr = NULL;

      c->left = string_copy(buffer);
      ptr = nextword(ptr, buffer, sizeof(buffer), TRUE);
      if (*error_pointer != NULL) break;

      /* Handle "does|is [not]", preserving the pointer after "is" in
      case it isn't that, but the form "is <string>". */

      if (strcmp(buffer, "does") == 0 || strcmp(buffer, "is") == 0)
        {
        if (buffer[0] == 'i') isptr = ptr;
        ptr = nextword(ptr, buffer, sizeof(buffer), TRUE);
        if (*error_pointer != NULL) break;
        if (strcmp(buffer, "not") == 0)
          {
          c->testfor = !c->testfor;
          if (isptr != NULL) isptr = ptr;
          ptr = nextword(ptr, buffer, sizeof(buffer), TRUE);
          if (*error_pointer != NULL) break;
          }
        }

      if (strcmp(buffer, "matches") == 0 || strcmp(buffer, "match") == 0)
        c->type = cond_matches;
      else if (strcmp(buffer, "contains") == 0 || strcmp(buffer, "contain") == 0)
        c->type = cond_contains;
      else if (strcmp(buffer, "above") == 0)
        c->type = cond_above;
      else if (strcmp(buffer, "below") == 0)
        c->type = cond_below;

      /* Unknown word unless following "is" or "is not", in which case
      it's actually the argument. Reset to read it. */

      else if (isptr != NULL)
        {
        c->type = cond_is;
        ptr = isptr;
        }
      else
        {
        *error_pointer = string_sprintf("unrecognized condition word \"%s\" "
          "near line %d of filter file", buffer, line_number);
        break;
        }

      /* Get the RH argument. */

      ptr = nextitem(ptr, buffer, sizeof(buffer), TRUE);
      if (*error_pointer != NULL) break;
      c->right = string_copy(buffer);
      }
    }

  /* We have read some new condition and set it up in the condition block
  c; point the current pointer at it, and then deal with what follows. */

  *current = c;

  /* Closing bracket terminates if this is a lower-level condition. Otherwise
  it is unexpected. */

  if (*ptr == ')')
    {
    if (toplevel)
      *error_pointer = string_sprintf("unexpected \")\" in line %d of "
        "filter file", line_number);
    break;
    }

  /* Opening bracket following a condition is an error; give an explicit
  message to make it clearer what is wrong. */

  else if (*ptr == '(')
    {
    *error_pointer = string_sprintf("unexpected \"(\" in line %d of "
      "filter file", line_number);
    break;
    }

  /* Otherwise the next thing must be one of the words "and", "or" or "then" */

  else
    {
    char *saveptr = ptr;
    ptr = nextword(ptr, buffer, sizeof(buffer), FALSE);
    if (*error_pointer != NULL) break;

    /* "Then" terminates a toplevel condition; otherwise a closing bracket
    has been omitted. Put a string terminator at the start of "then" so
    that reflecting the condition can be done when testing. */

    if (strcmp(buffer, "then") == 0)
      {
      if (toplevel) *saveptr = 0;
        else *error_pointer = string_sprintf("missing \")\" at end of "
          "condition near line %d of filter file", line_number);
      break;
      }

    /* "And" causes a new condition block to replace the one we have
    just read, which becomes the left sub-condition. The current pointer
    is reset to the pointer for the right sub-condition. We have to keep
    track of the tree of sequential "ands", so as to traverse back up it
    if an "or" is met. */

    else if (strcmp(buffer, "and") == 0)
      {
      condition_block *andc = store_malloc(sizeof(condition_block));
      andc->parent = current_parent;
      andc->type = cond_and;
      andc->testfor = TRUE;
      andc->left = c;
      andc->right = NULL;    /* insurance */
      *current = andc;
      current = (condition_block **)(&(andc->right));
      current_parent = andc;
      }

    /* "Or" is similar, but has to be done a bit more carefully to
    ensure that "and" is more binding. If there's a parent set, we
    are following a sequence of "and"s and must track back to their
    start. */

    else if (strcmp(buffer, "or") == 0)
      {
      condition_block *orc = store_malloc(sizeof(condition_block));
      condition_block *or_parent = NULL;

      if (current_parent != NULL)
        {
        while (current_parent->parent != NULL &&
               current_parent->parent->type == cond_and)
          current_parent = current_parent->parent;

        /* If the parent has a parent, it must be an "or" parent. */

        if (current_parent->parent != NULL)
          or_parent = current_parent->parent;
        }

      orc->parent = or_parent;
      if (or_parent == NULL) *cond = orc; else
        or_parent->right = orc;
      orc->type = cond_or;
      orc->testfor = TRUE;
      orc->left = (current_parent == NULL)? c : current_parent;
      orc->right = NULL;   /* insurance */
      current = (condition_block **)(&(orc->right));
      current_parent = orc;
      }

    /* Otherwise there is a disaster */

    else
      {
      *error_pointer = string_sprintf("\"and\" or \"or\" or \"%s\" "
        "expected near line %d of filter file, but found \"%s\"",
          toplevel? "then" : ")", line_number, buffer);
      break;
      }
    }
  }

return nextsigchar(ptr, TRUE);
}



#ifdef never
/*************************************************
*          Condition printer: for debugging      *
*************************************************/

static void
print_condition(condition_block *c, int indent)
{
int i;
switch(c->type)
  {
  case cond_personal:
  for (i = 0; i < indent; i++) debug_printf(" ");
  debug_printf("personal\n");
  break;

  case cond_delivered:
  for (i = 0; i < indent; i++) debug_printf(" ");
  debug_printf("delivered\n");
  break;

  case cond_errormsg:
  for (i = 0; i < indent; i++) debug_printf(" ");
  debug_printf("errormessage\n");
  break;

  case cond_is:
  for (i = 0; i < indent; i++) debug_printf(" ");
  debug_printf("is\n");
  break;

  case cond_matches:
  for (i = 0; i < indent; i++) debug_printf(" ");
  debug_printf("matches\n");
  break;

  case cond_contains:
  for (i = 0; i < indent; i++) debug_printf(" ");
  debug_printf("contains\n");
  break;

  case cond_above:
  for (i = 0; i < indent; i++) debug_printf(" ");
  debug_printf("above\n");
  break;

  case cond_below:
  for (i = 0; i < indent; i++) debug_printf(" ");
  debug_printf("below\n");
  break;

  case cond_and:
  case cond_or:
  print_condition(c->left, indent+2);
  for (i = 0; i < indent + 2; i++) debug_printf(" ");
  debug_printf((c->type == cond_and)? "AND\n" : "OR\n");
  print_condition(c->right, indent+2);
  break;
  }
}
#endif




/*************************************************
*            Read one filtering command          *
*************************************************/

/*
Arguments:
   pptr        points to pointer to first character of command; the pointer
                 is updated to point after the last character read
   lastcmdptr  points to pointer to pointer to last command; used for hanging
                 on the newly read command
   rewrite     TRUE if rewrites are to be done on new addresses

Returns:       TRUE if command successfully read, else FALSE
*/

static BOOL
read_command(char **pptr, filter_cmd ***lastcmdptr, BOOL rewrite)
{
int command, i;
filter_cmd *new, **newlastcmdptr;
BOOL yield = TRUE;
BOOL was_seen_or_unseen = FALSE;
BOOL was_noerror = FALSE;
char buffer[256];
char *ptr = *pptr;

/* Read the next word and find which command it is. Command words are normally
terminated by white space, but there are two exceptions, which are the "if" and
"elif" commands. We must allow for them to be terminated by an opening bracket,
as brackets are allowed in conditions and users will expect not to require
white space here. */

if (strncmp(ptr, "if(", 3) == 0)
  {
  strcpy(buffer, "if");
  ptr += 2;
  }
else if (strncmp(ptr, "elif(", 5) == 0)
  {
  strcpy(buffer, "elif");
  ptr += 4;
  }
else
  {
  ptr = nextword(ptr, buffer, sizeof(buffer), FALSE);
  if (*error_pointer != NULL) return FALSE;
  }

for (command = 0; command < command_list_count; command++)
  if (strcmp(buffer, command_list[command]) == 0) break;

/* Handle the individual commands */

switch (command)
  {
  /* Deliver, log, logfile, pipe, and testprint all take a single argument,
  and save can have an option second argument for the mode. */

  case deliver_command:
  case log_command:
  case logfile_command:
  case pipe_command:
  case save_command:
  case testprint_command:

  ptr = nextitem(ptr, buffer, sizeof(buffer), FALSE);
  if (*buffer == 0)
    *error_pointer = string_sprintf("\"%s\" requires an argument "
      "near line %d of filter file", command_list[command], line_number);

  if (*error_pointer != NULL) yield = FALSE; else
    {
    int start, end, domain;
    char *error;
    char *argument;
    void *second_argument = (void *)(-1);

    /* A deliver command's argument must be a valid address */

    if (command == deliver_command)
      {
      argument = parse_extract_address(buffer, &error, &start, &end, &domain,
        FALSE);
      if (argument != NULL)
        argument = rewrite? rewrite_address(argument, TRUE, TRUE) :
          rewrite_address_qualify(argument, TRUE);
      else
        {
        *error_pointer = string_sprintf("malformed address \"%s\" in "
          "filter file: %s", buffer, error);
        yield = FALSE;
        break;
        }
      }

    /* The argument for the log command must end in a newline, and the save
    and logfile commands can have an optional mode argument. */

    else
      {
      if (command == log_command)
        {
        int len = (int)strlen(buffer);
        if (len == 0 || buffer[len-1] != '\n') strcat(buffer, "\n");
        }
      argument = string_copy(buffer);
      if ((command == save_command || command == logfile_command) &&
          isdigit(*ptr))
        {
        ptr = nextword(ptr, buffer, sizeof(buffer), FALSE);
        second_argument = (void *)strtol(buffer, NULL, 0);
        }
      }

    /* Set up the command block. Seen defaults TRUE for delivery commands,
    FALSE for logging commands, and it doesn't matter for testprint, as
    that doesn't change the "delivered" flag. */

    new = store_malloc(sizeof(filter_cmd) + sizeof(void *));
    new->next = NULL;
    **lastcmdptr = new;
    *lastcmdptr = &(new->next);
    new->command = command;
    new->seen = seen_force? seen_value :
      (command != log_command && command != logfile_command);
    new->noerror = noerror_force;
    new->args[0] = argument;
    new->args[1] = second_argument;
    }
  break;


  /* Elif, else and endif just set a flag if expected. */

  case elif_command:
  case else_command:
  case endif_command:
  if (expect_endif > 0)
    had_else_endif = (command == elif_command)? had_elif :
                     (command == else_command)? had_else : had_endif;
  else
    {
    *error_pointer = string_sprintf("unexpected \"%s\" command near "
      "line %d of filter file", buffer, line_number);
    yield = FALSE;
    }
  break;


  /* Freeze and fail are available only if there is a pointer to pass
  back special actions; these are available only for system filters. */

  case fail_command:
  case freeze_command:
  if (special_action == NULL)
    {
    *error_pointer = string_sprintf("unavailable filtering command \"%s\" "
      "near line %d of filter file", buffer, line_number);
    yield = FALSE;
    break;
    }

  /* Else drop through and treat as "finish" */

  /* Finish has no arguments */

  case finish_command:
  new = store_malloc(sizeof(filter_cmd));
  new->next = NULL;
  **lastcmdptr = new;
  *lastcmdptr = &(new->next);
  new->command = command;
  new->seen = seen_force? seen_value : FALSE;
  break;


  /* Seen, unseen, and noerror are not allowed before if, which takes a
  condition argument and then and else sub-commands. */

  case if_command:
  if (seen_force || noerror_force)
    {
    *error_pointer = string_sprintf("\"seen\", \"unseen\", or \"noerror\" "
      "found before an \"if\" command near line %d in filter file",
        line_number);
    yield = FALSE;
    }

  /* Set up the command block for if */

  new = store_malloc(sizeof(filter_cmd) + 4 * sizeof(void *));
  new->next = NULL;
  **lastcmdptr = new;
  *lastcmdptr = &(new->next);
  new->command = command;
  new->args[0] = NULL;
  new->args[1] = new->args[2] = NULL;
  new->args[3] = ptr;

  /* Read the condition */

  ptr = read_condition(ptr, (condition_block **)(&(new->args[0])), TRUE);
  if (*error_pointer != NULL) { yield = FALSE; break; }

  /* Read the commands to be obeyed if the condition is true */

  newlastcmdptr = (filter_cmd **)(&(new->args[1]));
  if (!read_command_list(&ptr, &newlastcmdptr, TRUE, rewrite)) yield = FALSE;

  /* If commands were successfully read, handle the various possible
  terminators. There may be a number of successive "elif" sections. */

  else
    {
    while (had_else_endif == had_elif)
      {
      filter_cmd *newnew =
        store_malloc(sizeof(filter_cmd) + 4 * sizeof(void *));
      new->args[2] = newnew;
      new = newnew;
      new->next = NULL;
      new->command = command;
      new->args[0] = NULL;
      new->args[1] = new->args[2] = NULL;
      new->args[3] = ptr;

      ptr = read_condition(ptr, (condition_block **)(&(new->args[0])), TRUE);
      if (*error_pointer != NULL) { yield = FALSE; break; }
      newlastcmdptr = (filter_cmd **)(&(new->args[1]));
      if (!read_command_list(&ptr, &newlastcmdptr, TRUE, rewrite))
        yield = FALSE;
      }

    if (yield == FALSE) break;

    /* Handle termination by "else", possibly following one or more
    "elsif" sections. */

    if (had_else_endif == had_else)
      {
      newlastcmdptr = (filter_cmd **)(&(new->args[2]));
      if (!read_command_list(&ptr, &newlastcmdptr, TRUE, rewrite))
        yield = FALSE;
      else if (had_else_endif != had_endif)
        {
        *error_pointer = string_sprintf("\"endif\" missing near line %d of "
          "filter file", line_number);
        yield = FALSE;
        }
      }

    /* Otherwise the terminator was "endif" - this is checked by
    read_command_list(). The pointer is already set to NULL. */
    }

  /* Reset the terminator flag. */

  had_else_endif = had_neither;
  break;


  /* The mail & vacation commands have a whole slew of keyworded arguments,
  each of which must be expanded. The final argument value is the file expand
  boolean, whose offset is defined in mailarg_index_expand. Although it's
  logically a boolean, because it is stored in a char * value, we use NULL
  and not NULL, to keep 64-bit compilers happy. */

  case mail_command:
  case vacation_command:
  new = store_malloc(sizeof(filter_cmd) + mailargs_total * sizeof(void *));
  new->next = NULL;
  new->command = command;
  new->seen = seen_force? seen_value : FALSE;
  new->noerror = noerror_force;
  for (i = 0; i < mailargs_string_count; i++) new->args[i] = NULL;
  new->args[mailarg_index_expand] = NULL;

  /* Read keyword/value pairs until we hit one that isn't. The data
  must contain only printing chars plus tab, though the "text" value
  can also contain newlines. The "file" keyword can be preceded by the
  word "expand". */

  for (;;)
    {
    char *saveptr = ptr;
    ptr = nextword(ptr, buffer, sizeof(buffer), FALSE);
    if (*error_pointer != NULL)
      {
      yield = FALSE;
      break;
      }

    /* Ensure "expand" is followed by "file" */

    if (strcmp(buffer, "expand") == 0)
      {
      new->args[mailarg_index_expand] = "";  /* not NULL => TRUE */
      ptr = nextword(ptr, buffer, sizeof(buffer), FALSE);
      if (strcmp(buffer, "file") != 0)
        {
        *error_pointer = string_sprintf("\"expand\" not followed by \"file\" "
          " near line %d of filter file", line_number);
        yield = FALSE;
        break;
        }
      }

    /* Scan for the keyword */

    for (i = 0; i < mailargs_string_count; i++)
      if (strcmp(buffer, mailargs[i]) == 0) break;
    if (i >= mailargs_string_count)
      {
      ptr = saveptr;
      break;
      }
    ptr = nextitem(ptr, buffer, sizeof(buffer), FALSE);
    if (*error_pointer != NULL)
      {
      yield = FALSE;
      break;
      }
    else
      {
      char *s = expand_string(buffer);
      char *p = s;
      if (s == NULL)
        {
        *error_pointer = string_sprintf("failed to expand \"%s\" near "
          "line %d of filter file", buffer, line_number);
        yield = FALSE;
        break;
        }
      for (p = buffer; *p != 0; p++)
        {
        if (!isprint(*p) && *p != '\t' && (*p != '\n' ||
            i != mailarg_index_text))
          {
          *error_pointer = string_sprintf("non-printing character (%d) "
            "near line %d of filter file", *p, line_number);
          yield = FALSE;
          break;
          }
        }
      new->args[i] = s;
      }
    }

  /* If this is the vacation command, apply some default settings to
  some of the arguments. */

  if (command == vacation_command)
    {
    if (new->args[mailarg_index_file] == NULL)
      {
      new->args[mailarg_index_file] = string_copy(".vacation.msg");
      new->args[mailarg_index_expand] = "";   /* not NULL => TRUE */
      }
    if (new->args[mailarg_index_log] == NULL)
      new->args[mailarg_index_log] = string_copy(".vacation.log");
    if (new->args[mailarg_index_once] == NULL)
      new->args[mailarg_index_once] = string_copy(".vacation");
    }

  /* We have now set up all the arguments. Check that the the current
  user is not one of the recipients; this is to prevent loops. */

  /**** This isn't so easy to do - what to do if it does match? Just remove
  the offending address? Then must check that there is at least one address
  left...

  sprintf(buffer, "%s@%s", deliver_localpart, deliver_domain);

  for (i = mailarg_index_to; i <= mailarg_index_bcc; i++)
    {
    if (new->args[i] != NULL && strstric(new->args[i], buffer, FALSE) != NULL)
      {

      }
    }
  ****** needs some thought ******/

  /* Join the address on to the chain of generated addresses */

  **lastcmdptr = new;
  *lastcmdptr = &(new->next);
  break;


  /* Seen and unseen just set flags */

  case seen_command:
  case unseen_command:
  if (seen_force)
    {
    *error_pointer = string_sprintf("\"seen\" or \"unseen\" repeated "
      "near line %d in filter file", line_number);
    yield = FALSE;
    }
  seen_value = (command == seen_command);
  seen_force = TRUE;
  was_seen_or_unseen = TRUE;
  break;


  /* So does noerror */

  case noerror_command:
  noerror_force = TRUE;
  was_noerror = TRUE;
  break;


  /* Oops */

  default:
  *error_pointer = string_sprintf("unknown filtering command \"%s\" "
    "near line %d of filter file", buffer, line_number);
  yield = FALSE;
  break;
  }

if (!was_seen_or_unseen && !was_noerror)
  {
  seen_force = FALSE;
  noerror_force = FALSE;
  }

*pptr = ptr;
return yield;
}



/*************************************************
*              Read a list of commands           *
*************************************************/

/* If condional is TRUE, the list must be terminated
by the words "else" or "endif".

Arguments:
  pptr        points to pointer to next character; the pointer is updated
  lastcmdptr  points to pointer to pointer to previously-read command; used
                for hanging on the new command
  conditional TRUE if this command is the subject of a condition
  rewrite     TRUE if new addresses are to be rewritten

Returns:      TRUE on success
*/

static BOOL
read_command_list(char **pptr, filter_cmd ***lastcmdptr, BOOL conditional,
  BOOL rewrite)
{
if (conditional) expect_endif++;
had_else_endif = had_neither;
while (**pptr != 0 && had_else_endif == had_neither)
  {
  if (!read_command(pptr, lastcmdptr, rewrite)) return FALSE;
  *pptr = nextsigchar(*pptr, TRUE);
  }
if (conditional)
  {
  expect_endif--;
  if (had_else_endif == had_neither)
    {
    *error_pointer = "\"endif\" missing at end of filter file\n";
    return FALSE;
    }
  }
return TRUE;
}




/*************************************************
*   Test a header for the "personal" condition   *
*************************************************/

/* A bit more efficient than doing a lot of explicit expansions. We pass
in a chain of alias blocks and a variable-length list of explicit text
strings.

Arguments:
  s          header name, including the trailing colon
  cond       value to return if the header contains any of the strings
               or aliases
  aliases    points to a chain of alias blocks
  count      number of strings
  ...        the strings

Returns:     cond if the header exists and contains one of the aliases or
               strings; otherwise !cond
*/

static BOOL
test_header(char *s, BOOL cond, alias *aliases, int count, ...)
{
header_line *h;
int slen = (int)strlen(s);

for (h = header_list; h != NULL; h = h->next)
  {
  int i;
  alias *a;
  va_list ap;

  if (h->type == htype_old || slen > h->slen ||
    strncmpic(s, h->text, slen) != 0)
      continue;

  for (a = aliases; a != NULL; a = a->next)
    if (strstric(h->text + slen, a->name, FALSE) != NULL) return cond;

  va_start(ap, count);
  for (i = 0; i < count; i++)
    {
    char *p = va_arg(ap, char *);
    if (strstric(h->text + slen, p, FALSE) != NULL) return cond;
    }
  va_end(ap);
  }

return !cond;
}




/*************************************************
*             Test a condition                   *
*************************************************/

/*
Arguments:
  c              points to the condition block; c->testfor indicated whether
                   it's a positive or negative condition
  delivered      points to the "delivered" state of the filtering

Returns:         TRUE if the condition is met
*/

static BOOL
test_condition(condition_block *c, BOOL *delivered)
{
BOOL yield;
regexp *re;
alias *aliases;
char *self, *self_from, *self_to;
char *regcomp_error = NULL;
char *exp[2], *p;
int val[2];
int i;

if (c == NULL) return TRUE;  /* does this ever occur? */

switch (c->type)
  {
  case cond_and:
  yield = test_condition(c->left, delivered) &&
          *error_pointer == NULL &&
          test_condition(c->right, delivered);
  break;

  case cond_or:
  yield = test_condition(c->left, delivered) ||
          (*error_pointer == NULL &&
          test_condition(c->right, delivered));
  break;

  case cond_personal:
  aliases = (alias *)(c->left);
  self = string_sprintf("%s@%s", deliver_localpart, deliver_domain);
  self_from = rewrite_address_for_filter(self, rewrite_from);
  self_to   = rewrite_address_for_filter(self, rewrite_to);
  if (self_from == NULL) self_from = self;
  if (self_to == NULL) self_to = self;
  yield = test_header("to:", TRUE, aliases, 2, self, self_to) &&
          test_header("from:", FALSE, aliases, 5, "server@", "daemon@",
            "root@", self, self_from) &&
          test_header("subject:", FALSE, NULL, 1, "circular") &&
          test_header("precedence:", FALSE, NULL, 1, "bulk") &&
          (sender_address == NULL || sender_address[0] != 0);
  if (self_from != self) store_free(self_from);
  if (self_to   != self) store_free(self_to);
  store_free(self);
  break;

  case cond_delivered:
  yield = *delivered;
  break;

  /* Only TRUE if a message is actually being processed; FALSE for address
  testing and verification. */

  case cond_errormsg:
  yield = message_id[0] != 0 &&
    (sender_address == NULL || sender_address[0] == 0 || user_null_sender);
  break;

  /* All other conditions have left and right values that need expanding;
  on error, it doesn't matter what value is returned. Free up the un-
  expanded strings, and save the expansions, because for regular expressions
  there may be pointers into them. They'll get freed when the whole
  command structure is freed at the end - as will any that never got
  expanded because never tested. */

  default:
  p = (char *)(c->left);
  for (i = 0; i < 2; i++)
    {
    exp[i] = expand_string(p);
    if (exp[i] == NULL)
      {
      *error_pointer = string_sprintf("failed to expand \"%s\" in "
        "filter file: %s", p, expand_string_message);
      return FALSE;
      }
    store_free(p);
    for (p = exp[i]; *p != 0; p++) *p = tolower(*p);
    p = (char *)(c->right);
    }

  c->left = exp[0];
  c->right = exp[1];

  /* Inner switch for the different cases */

  switch(c->type)
    {
    case cond_is:
    yield = strcmp(exp[0], exp[1]) == 0;
    break;

    case cond_contains:
    yield = strstr(exp[0], exp[1]) != NULL;
    break;

    case cond_matches:
    regcomp_error_pointer = &regcomp_error;
    re = regcomp(exp[1]);
    regcomp_error_pointer = NULL;
    if (re == NULL)
      {
      *error_pointer = string_sprintf("error while compiling "
        "regular expression \"%s\" in filter file: %s",
        exp[1], regcomp_error);
      return FALSE;
      }
    yield = regexec(re, exp[0]);
    if (yield)
      {
      expand_nmax = 0;
      for (; expand_nmax < NSUBEXP; expand_nmax++)
        {
        expand_nstring[expand_nmax] = re->startp[expand_nmax];
        expand_nlength[expand_nmax] = re->endp[expand_nmax] -
          expand_nstring[expand_nmax];
        }
      expand_nmax--;
      }
    free(re);   /* NB *not* store_free, because got by regcomp */
    break;

    /* For above and below, convert the strings to numbers */

    case cond_above:
    case cond_below:
    for (i = 0; i < 2; i++)
      {
      val[i] = get_number(exp[i], &yield);
      if (!yield)
        {
        *error_pointer = string_sprintf("malformed numerical string \"%s\" "
          "in filter file", exp[i]);
        return FALSE;
        }
      }
    yield = (c->type == cond_above)? (val[0] > val[1]) : (val[0] < val[1]);
    break;
    }
  break;
  }

return yield == c->testfor;
}



/*************************************************
*         Free a condition block                 *
*************************************************/

/*
Argument:  pointer to the condition block
Returns:   nothing
*/

static void
free_condition(condition_block *c)
{
alias *a, *aa;
if (c == NULL) return;
switch (c->type)
  {
  case cond_and:
  case cond_or:
  free_condition(c->left);
  free_condition(c->right);
  break;

  case cond_contains:
  case cond_matches:
  case cond_is:
  store_free(c->left);
  store_free(c->right);
  break;

  case cond_personal:
  for (a = (alias *)(c->left); a != NULL; a = aa)
    {
    aa = a->next;
    store_free(a->name);
    store_free(a);
    }
  break;

  case cond_delivered:
  case cond_errormsg:
  case cond_above:
  case cond_below:
  break;
  }
store_free(c);
}



/*************************************************
*             Ouput the current indent           *
*************************************************/

static void
indent(void)
{
int i;
for (i = 0; i < output_indent; i++) debug_printf(" ");
}



/*************************************************
*          Interpret chain of commands           *
*************************************************/

/* In testing state, just say what would be done rather than doing it. The
testprint command just expands and outputs its argument in testing state, and
does nothing otherwise.

Arguments:
  commands    points to chain of commands to interpret
  generated   where to hang newly-generated addresses
  delivered   points to the "delivered" state of the filtering

Returns:      FALSE if there was an error, or if "finish" was obeyed
*/

static BOOL
interpret_commands(filter_cmd *commands, address_item **generated,
  BOOL *delivered)
{
char *s;
int mode;
address_item *addr;
BOOL condition_value;

while (commands != NULL)
  {
  switch(commands->command)
    {
    case deliver_command:
    case save_command:
    if (commands->seen) *delivered = TRUE;
    s = expand_string((char *)(commands->args[0]));
    mode = (int)(commands->args[1]);
    if (s == NULL)
      {
      *error_pointer = string_sprintf("failed to expand \"%s\": %s",
        (char *)(commands->args[0]), expand_string_message);
      return FALSE;
      }

    /* Test case: report what would happen */

    if (filter_test != NULL)
      {
      indent();
      switch(commands->command)
        {
        case deliver_command:
        printf("%seliver message to: %s%s\n", (commands->seen)?
          "D" : "Unseen d", s, commands->noerror? " (noerror)" : "");
        break;
        case save_command:
        if (mode < 0)
          printf("%save message to: %s%s\n", (commands->seen)?
            "S" : "Unseen s", s, commands->noerror? " (noerror)" : "");
        else
          printf("%save message to: %s %04o%s\n", (commands->seen)?
            "S" : "Unseen s", s, mode, commands->noerror? " (noerror)" : "");
        break;
        }
      }

    /* Real case: Ensure save command starts with / and cause deliver
    argument to be rewritten. */

    else
      {
      BOOL pfr = FALSE;

      switch(commands->command)
        {
        case deliver_command:
        DEBUG(10) debug_printf("Filter: %sdeliver message to: %s%s\n",
          (commands->seen)? "" : "unseen ", s,
          commands->noerror? " (noerror)" : "");
        break;

        case save_command:
        DEBUG(10) debug_printf("Filter: %ssave message to: %s%s\n",
          (commands->seen)? "" : "unseen ", s,
          commands->noerror? " (noerror)" : "");
        if (s[0] != '/')
          {
          char *ss = string_sprintf("%s/%s", deliver_home, s);
          store_free(s);
          s = ss;
          }
        pfr = TRUE;
        break;
        }

      /* Create the new address and add it to the chain, setting the
      ignore_error flag if necessary, the mode value for save, and an
      appropriate pfr flag. */

      addr = deliver_make_addr(s);
      addr->pfr = pfr;
      addr->mode = mode;
      addr->ignore_error = commands->noerror;
      addr->next = *generated;
      *generated = addr;
      }
    break;

    case pipe_command:
    if (commands->seen) *delivered = TRUE;
    s = string_copy((char *)(commands->args[0]));
    if (filter_test != NULL)
      {
      indent();
      printf("%sipe message to: %s%s\n", (commands->seen)?
        "P" : "Unseen p", s, commands->noerror? " (noerror)" : "");
      }
    else /* Ensure pipe command starts with | */
      {
      DEBUG(10) debug_printf("Filter: %spipe message to: %s%s\n",
        (commands->seen)? "" : "unseen ", s,
        commands->noerror? " (noerror)" : "");
      if (s[0] != '|')
        {
        char *ss = string_sprintf("|%s", s);
        store_free(s);
        s = ss;
        }

      /* Create the new address and add it to the chain, setting the
      ignore_error flag if necessary. Set the expand_pipe flag so that
      each command argument is expanded in the transport after the command
      has been split up into separate arguments. */

      addr = deliver_make_addr(s);
      addr->pfr = TRUE;
      addr->ignore_error = commands->noerror;
      addr->next = *generated;
      addr->expand_pipe = TRUE;
      *generated = addr;
      }
    break;

    /* Set up the file name and mode, and close any previously open
    file. */

    case logfile_command:
    if (commands->seen) *delivered = TRUE;
    log_mode = (int)(commands->args[1]);
    if (log_mode == -1) log_mode = 0600;
    if (log_fd >= 0)
      {
      close(log_fd);
      log_fd = -1;
      }
    log_filename = expand_string((char *)(commands->args[0]));
    if (log_filename == NULL)
      {
      *error_pointer = string_sprintf("failed to expand \"%s\": %s",
        (char *)(commands->args[0]), expand_string_message);
      return FALSE;
      }
    if (filter_test != NULL)
      {
      indent();
      printf("%sogfile %s\n", (commands->seen)? "Seen l" : "L", log_filename);
      }
    break;

    case log_command:
    if (commands->seen) *delivered = TRUE;
    s = expand_string((char *)(commands->args[0]));
    if (s == NULL)
      {
      *error_pointer = string_sprintf("failed to expand \"%s\": %s",
        (char *)(commands->args[0]), expand_string_message);
      return FALSE;
      }

    if (filter_test != NULL)
      {
      indent();
      printf("%sog \"%s\"\n", (commands->seen)? "Seen l" : "L",
        string_printing(s, FALSE));
      }

    /* Attempt to write to a log file only if configured as permissible and
    running as a user, i.e. if seteuid has been used to give up a privileged
    uid. This will be the case for normal user filter files unless the security
    option set to 2 and seteuid is not set for the forwardfile director. */

    else
      {
      if (!as_user || no_log)
        {
        DEBUG(10)
          debug_printf("filter log command aborted: euid=%d no_log=%d\n",
          (int)geteuid(), no_log);
        *error_pointer = as_user? "log command not permitted" :
          "filter not running as user";
        return FALSE;
        }
      else
        {
        int len;
        DEBUG(10) debug_printf("writing filter log as euid %d\n",
          (int)geteuid());
        if (log_fd < 0)
          {
          if (log_filename == NULL)
            {
            *error_pointer = "attempt to obey \"log\" command in filter file "
              "without a previous \"logfile\"";
            return FALSE;
            }
          log_fd = open(log_filename, O_CREAT|O_APPEND|O_WRONLY, log_mode);
          if (log_fd < 0)
            {
            *error_pointer = string_sprintf("failed to open filter log file "
              "\"%s\": %s", log_filename, strerror(errno));
            return FALSE;
            }
          }
        len = (int)strlen(s);
        if (write(log_fd, s, len) != len)
          {
          *error_pointer = string_sprintf("write error on file \"%s\": %s",
            log_filename, strerror(errno));
          return FALSE;
          }
        }
      }
    break;

    /* Freeze and fail are available only when there is somewhere to pass
    back the action; this is the case only for system filtering. These
    commands are rejected at parse time otherwise. */

    case fail_command:
    *special_action = SPECIAL_FAIL;
    break;

    case freeze_command:
    *special_action = SPECIAL_FREEZE;
    break;

    case finish_command:
    if (commands->seen) *delivered = TRUE;
    if (filter_test != NULL)
      {
      indent();
      debug_printf("%sinish\n", (commands->seen)? "Seen f" : "F");
      }
    else
      DEBUG(10) debug_printf("Filter: %sfinish\n",(commands->seen)? "" : "f");
    return FALSE;
    break;

    case if_command:
    condition_value =
      test_condition((condition_block *)(commands->args[0]), delivered);
    if (*error_pointer != NULL) return FALSE;
    if (filter_test != NULL)
      {
      indent();
      debug_printf("Condition is %s: %s\n", condition_value? "true" : "false",
        commands->args[3]);
      }
    else
      DEBUG(10) debug_printf("Filter: condition is %s: %s\n",
        condition_value? "true" : "false", commands->args[3]);
    output_indent += 2;
    if (!interpret_commands((filter_cmd *)
      (commands->args[condition_value? 1:2]),
        generated, delivered)) return FALSE;
    output_indent -= 2;
    break;


    /* To try to catch runaway loops, do not generate mail if the
    return path is unset or if a non-trusted user supplied -f <>
    as the return path. */

    case mail_command:
    case vacation_command:
    if (return_path == NULL || return_path[0] == 0 || user_null_sender)
      {
      if (filter_test != NULL)
        printf("%s command ignored because return_path is empty\n",
          command_list[commands->command]);
      else DEBUG(10) debug_printf("%s command ignored because return_path "
        "is empty\n", command_list[commands->command]);
      break;
      }

    /* Proceed with mail or vacation command */

    if (commands->seen) *delivered = TRUE;
    if (filter_test != NULL)
      {
      int i;
      char *to = (char *)(commands->args[mailarg_index_to]);
      indent();
      printf("%sail to: %s%s%s\n", (commands->seen)? "Seen m" : "M",
        (to == NULL)? "<default>" : to,
        (commands->command == vacation_command)? " (vacation)" : "",
        (commands->noerror)? " (noerror)" : "");
      for (i = 1; i < mailargs_string_count; i++)
        {
        char *arg = (char *)(commands->args[i]);
        if (arg != NULL)
          {
          int len = (int)strlen(mailargs[i]);
          int indent = (debug_level > 0)? output_indent : 0;
          while (len++ < 7 + indent) printf(" ");
          printf("%s: %s%s\n", mailargs[i], string_printing(arg, FALSE),
            (commands->args[mailarg_index_expand] != NULL &&
              strcmp(mailargs[i], "file") == 0)? " (expanded)" : "");
          }
        }
      }
    else
      {
      int i;
      char *to = (char *)(commands->args[mailarg_index_to]);
      to = (to == NULL)? expand_string("$reply_address") :
         string_copy(to);
      while (isspace(*to)) to++;

      DEBUG(10)
        {
        debug_printf("Filter: %smail to: %s%s%s\n",
          (commands->seen)? "seen " : "",
          to,
          (commands->command == vacation_command)? " (vacation)" : "",
          (commands->noerror)? " (noerror)" : "");
        for (i = 1; i < mailargs_string_count; i++)
          {
          char *arg = (char *)(commands->args[i]);
          if (arg != NULL)
            {
            int len = (int)strlen(mailargs[i]);
            while (len++ < 15) debug_printf(" ");
            debug_printf("%s: %s%s\n", mailargs[i], string_printing(arg,FALSE),
              (commands->args[mailarg_index_expand] != NULL &&
                strcmp(mailargs[i], "file") == 0)? " (expanded)" : "");
            }
          }
        }

      addr = deliver_make_addr(string_sprintf(">%s", to));
      addr->pfr = TRUE;
      addr->ignore_error = commands->noerror;
      addr->next = *generated;
      *generated = addr;
      addr->reply = store_malloc(sizeof(reply_item));
      addr->reply->headers = NULL;    /* Can't set that from here (yet?) */
      addr->reply->from = NULL;
      addr->reply->to = to;
      addr->reply->file_expand = commands->args[mailarg_index_expand] != NULL;
      for (i = 1; i < mailargs_string_count; i++)
        {
        char *ss = (char *)(commands->args[i]);
        *((char **)(((char *)(addr->reply)) + reply_offsets[i])) =
          (ss == NULL)? NULL : string_copy(ss);
        }
      }
    break;

    case testprint_command:
    if (filter_test != NULL || debug_level >= 10)
      {
      char *s = expand_string((char *)(commands->args[0]));
      if (s == NULL)
        {
        *error_pointer =
          string_sprintf("Failed to expand \"%s\" in testprint command: %s\n",
            (char *)(commands->args[0]), expand_string_message);
        return FALSE;
        }

      if (filter_test == NULL)
        debug_printf("Filter: testprint: %s\n", string_printing(s, TRUE));
      else
        printf("Testprint: %s\n", string_printing(s, TRUE));
      }
    }

  commands = commands->next;
  }

return TRUE;
}



/*************************************************
*            Interpret a mail filter file        *
*************************************************/

/*
Arguments:
  filter      points to the entire file, read into store as a single string
  generated   where to hang newly-generated addresses
  delivered   points to the "delivered" state of the filtering
  action      if non-null, allow special actions and return here
  error       where to pass back an error text
  notroot     TRUE if running as an ordinary user; this enables the use
                of the "log" command
  forbid_log  if TRUE, locks out the "log" command regardless of the value
                of notroot
  rewrite     if TRUE, newly generated addresses are rewritten

Returns:      TRUE on success
*/

BOOL
filter_interpret(char *filter, address_item **generated, BOOL *delivered,
  int *action, char **error, BOOL notroot, BOOL forbid_log, BOOL rewrite)
{
char *ptr = filter;
filter_cmd *commands = NULL;
filter_cmd **lastcmdptr = &commands;

DEBUG(10) debug_printf("Filter: start of processing\n");

/* Initialize "not delivered" and outside an if command. */

*delivered = FALSE;
expect_endif = 0;
output_indent = 0;

/* To save having to pass this about all the time, make it static, and
initialize to no error. Also initialize the line number, for error messages and
the log file variables. */

special_action = action;
as_user = notroot;
no_log = forbid_log;
error_pointer = error;
*error_pointer = NULL;
line_number = 1;
log_fd = -1;
log_mode = 0600;
log_filename = NULL;

/* Scan filter file for syntax and build up an interpretation thereof,
and interpret the compiled commands, and if testing, say whether we ended
up delivered or not, unless something went wrong. */

seen_force = FALSE;
ptr = nextsigchar(ptr, TRUE);
if (read_command_list(&ptr, &lastcmdptr, FALSE, rewrite))
  {
  interpret_commands(commands, generated, delivered);
  if (*error_pointer == NULL)
    {
    if (filter_test != NULL)
      debug_printf("At the end of filtering \"delivered\" is %s\n",
        (*delivered)? "true" : "false");
    else DEBUG(10)
      debug_printf("Filter: at end \"delivered\" is %s\n",
        (*delivered)? "true" : "false");
    }
  }

/* Close the log file if it was opened. */

if (log_fd >= 0) close(log_fd);

/* Free up the store that was used in building data structures before
returning, and kill off any numerical variables. */

while (commands != NULL)
  {
  int i;
  filter_cmd *this = commands;
  commands = commands->next;
  switch(this->command)
    {
    case deliver_command:
    case log_command:
    case logfile_command:
    case pipe_command:
    case save_command:
    case testprint_command:
    if (this->args[0] != NULL) store_free(this->args[0]);
    break;

    case fail_command:
    case freeze_command:
    case finish_command:
    break;

    case if_command:
    free_condition((condition_block *)(this->args[0]));
    if (this->args[1] != NULL) store_free(this->args[1]);
    if (this->args[2] != NULL) store_free(this->args[2]);
    break;

    case mail_command:
    case vacation_command:
    for (i = 0; i < mailargs_string_count; i++)
      if (this->args[i] != NULL) store_free(this->args[i]);
    break;
    }
  store_free(this);
  }

DEBUG(10) debug_printf("Filter: end of processing\n");

expand_nmax = -1;
return *error_pointer == NULL;
}





/*************************************************
*      Interpret a system-wide filter            *
*************************************************/

/* This is called from deliver_message before the real process of delivery
begins. It is intended as a spam and mail-bomb filtering defence. It runs the
filtering code on message_filter, using message_filter_uid/gid.

Arguments:
  generated     where to hang generated addresses
  delivered     set TRUE if significant delivery happens
  action        if non-null, allow special actions and return here
  error         place to point error message

Returns:        TRUE if filtering succeeds
*/

BOOL
filter_system_interpret(address_item **generated, BOOL *delivered,
  int *action, char **error)
{
BOOL yield;
FILE *filter;
char *filebuf;
int saved_euid = -1, saved_egid = -1;   /* keep picky compiler happy */
struct stat statbuf;

/* Arrange to run under the correct uid/gid. If we are not root at this point,
we can become root by seteuid(), because that is the state in
deliver_message(). */

if (message_filter_uid_set)
  {
  saved_euid = geteuid();
  saved_egid = getegid();
  if (saved_euid != root_uid) mac_seteuid(root_uid);
  mac_setegid(message_filter_gid);
  mac_seteuid(message_filter_uid);
  DEBUG(9) debug_printf("running system filter as uid=%d gid=%d\n",
    geteuid(), getegid());
  }

/* Now open the filter file and read it into memory. */

filter = fopen(message_filter, "r");
if (filter == NULL)
  {
  *error = string_sprintf("failed to open \"%s\"", message_filter);
  yield = FALSE;
  goto RESTORE_UID;
  }

if (fstat(fileno(filter), &statbuf) != 0)
  {
  *error = string_sprintf("failed to stat \"%s\"", message_filter);
  yield = FALSE;
  goto RESTORE_UID;
  }

filebuf = store_malloc(statbuf.st_size + 1);
if (fread(filebuf, 1, statbuf.st_size, filter) != statbuf.st_size)
  {
  *error = string_sprintf("error while reading \"%s\": %s",
    message_filter, strerror(errno));
  yield = FALSE;
  goto RESTORE_UID;
  }
filebuf[statbuf.st_size] = 0;

/*Now we can call the standard filter interpreter. */

yield = filter_interpret(filebuf, generated, delivered, action, error,
  TRUE, FALSE, TRUE);
store_free(filebuf);

/* Restore the original effective uid/gid before returning. */

RESTORE_UID:

if (message_filter_uid_set)
  {
  mac_seteuid(root_uid);
  mac_setegid(saved_egid);
  mac_seteuid(saved_euid);
  }

return yield;
}




/*************************************************
*            Test a mail filter                  *
*************************************************/

/* This is called when exim is run with the -bf option. The name
of the filter file is in filter_test, and we are running under an
unprivileged uid/gid. A test message's headers have been read into
store, and the body of the message is still accessible on the
standard input.

Argument:   the standard input fd, containing the message body
Returns:    TRUE if no errors
*/

BOOL
filter_runtest(int fd)
{
int rc, body_len;
register int ch;
BOOL yield, delivered;
struct stat statbuf;
address_item *generated = NULL;
char *body, *error, *filebuf, *s, *tag;

/* Read the filter file into store as will be done by the director
in a real case. */

debug_printf("Testing Exim filter file %s\n", filter_test);

if (fstat(fd, &statbuf) != 0)
  {
  printf("exim: failed to get size of %s: %s\n", filter_test, strerror(errno));
  return FALSE;
  }

filebuf = store_malloc(statbuf.st_size + 1);
rc = read(fd, filebuf, statbuf.st_size);
close(fd);

if (rc != statbuf.st_size)
  {
  printf("exim: error while reading %s: %s\n", filter_test, strerror(errno));
  return FALSE;
  }

filebuf[statbuf.st_size] = 0;

/* Check that the file starts with # Exim filter, as the director does. If
it does not, treat it as an ordinary .forward file and check that. */

s = filebuf;
tag = "# exim filter";
while (isspace(*s)) s++;           /* Skips initial blank lines */
for (; *tag != 0; s++, tag++)
  {
  if (*tag == ' ')
    {
    while (*s == ' ' || *s == '\t') s++;
    s--;
    }
  else if (tolower(*s) != tolower(*tag))
    {
    yield = parse_extract_addresses(filebuf, &generated, &error,
      TRUE,                           /* no addresses => no error */
      FALSE,                          /* don't recognize :blackhole: */
      TRUE,                           /* do rewrite */
      NULL,                           /* no check on includes */
      NULL);                          /* fail on syntax errors */

    if (yield != OK)
      {
      printf("exim: error in forward file: %s\n", error);
      return FALSE;
      }

    if (generated == NULL)
      printf("exim: no addresses generated from forward file\n");

    else
      {
      printf("exim: forward file generated:\n");
      while (generated != NULL)
        {
        printf("  %s\n", generated->orig);
        generated = generated->next;
        }
      }

    return TRUE;
    }
  }

/* We have to read the remainder of the message in order to find its size, so
we might as well set up the message_body variable at the same time (when
*really* filtering this is not read unless needed). The reading code is written
out here rather than having options in read_message_data, in order to keep that
function as efficient as possible. */

message_body = body = store_malloc(message_body_visible + 1);
body_len = 0;

if (!feof(stdin))
  {
  if (!dot_ends)
    {
    while ((ch = getc(stdin)) != EOF)
      {
      if (body_len < message_body_visible) message_body[body_len++] = ch;
      message_size++;
      }
    }
  else
    {
    int ch_state = 1;
    while ((ch = getc(stdin)) != EOF)
      {
      switch (ch_state)
        {
        case 0:                         /* Normal state */
        if (ch == '\n') ch_state = 1;
        break;

        case 1:                         /* After "\n" */
        if (ch == '.')
          {
          ch_state = 2;
          continue;
          }
        if (ch != '\n') ch_state = 0;
        break;

        case 2:                         /* After "\n." */
        if (ch == '\n') goto READ_END;
        if (body_len < message_body_visible) message_body[body_len++] = '.';
        message_size++;
        ch_state = 0;
        break;
        }
      if (body_len < message_body_visible) message_body[body_len++] = ch;
      message_size++;
      }
    READ_END: ch = ch;  /* Some compilers don't like null statements */
    }
  }

message_body[body_len] = 0;

/* Convert newlines in the body to spaces */

while (*body != 0)
  {
  if (*body == '\n') *body = ' ';
  body++;
  }

/* Now pass the filter file to the function that interprets it. Because
filter_test is not NULL, the interpreter will output comments about what
it is doing, but an error message will have to be output here. No need to
clean up store. The final argument is TRUE because Exim has given up root
privilege when running a filter test, and in any case, as it is a test,
is isn't going to try writing any files. */

if (!(yield = filter_interpret(filebuf, &generated, &delivered, NULL, &error,
  TRUE, FALSE, TRUE))) printf("Filter error: %s\n", error);
return yield;
}

/* End of filter.c */
