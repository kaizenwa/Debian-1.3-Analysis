#include "FvwmIconMan.h"
#include "readconfig.h"

/************************************************************************
 *
 * Builtin functions:
 *
 ************************************************************************/

extern int builtin_selectbutton (int numargs, BuiltinArg *args);
extern int builtin_sendcommand (int numargs, BuiltinArg *args);
extern int builtin_warp (int numargs, BuiltinArg *args);
extern int builtin_quit (int numargs, BuiltinArg *args);
extern int builtin_printdebug (int numargs, BuiltinArg *args);
extern int builtin_raisemanager (int numargs, BuiltinArg *args);
extern int builtin_lowermanager (int numargs, BuiltinArg *args);

typedef struct {
  char *name;
  int (*func)(int numargs, BuiltinArg *args);
  int numargs;
  BuiltinArgType args[MAX_ARGS];
} FunctionType;

FunctionType builtin_functions[] = {
  { "lowermanager", builtin_lowermanager, 0 }, 
  { "printdebug", builtin_printdebug, 0 },
  { "raisemanager", builtin_raisemanager, 0 },
  { "selectbutton", builtin_selectbutton, 2, 
    { ManagerArg, ButtonArg } 
  },
  { "sendcommand", builtin_sendcommand, 3, 
    { ManagerArg, WindowArg, StringArg } 
  },
  { "quit", builtin_quit, 0 },
  { "warp", builtin_warp, 2,
    { ManagerArg, ButtonArg } 
  }
};

static int num_builtins = sizeof (builtin_functions) / sizeof (FunctionType);

/************************************************************************/


struct charstring 
{
  char key;
  int  value;
};
  
typedef struct {
  ButtonType buttontype;
  int offset; /* if buttontype is AbsoluteButton */
} TermStruct;

struct charstring key_modifiers[]=
{
  {'s',ShiftMask},
  {'c',ControlMask},
  {'m',Mod1Mask},
  {'1',Mod1Mask},
  {'2',Mod2Mask},
  {'3',Mod3Mask},
  {'4',Mod4Mask},
  {'5',Mod5Mask},
  {'a',AnyModifier},
  {'n',0},
  {0,0}
};

#if FVWM_VERSION == 1
static FILE *config_fp = NULL;
#endif


/* This is only used for printing out the .fvwmrc line if an error
   occured */

#define PRINT_LINE_LENGTH 80
static char current_line[PRINT_LINE_LENGTH];

static void save_current_line (char *s)
{
  char *p = current_line;

  while (*s && p < current_line + PRINT_LINE_LENGTH - 1) {
    if (*s == '\n') {
      *p = '\0';
      return;
    }
    else {
      *p++ = *s++;
    }
  }
  *p = '\0';
}

void print_args (int numargs, BuiltinArg *args)
{
#ifdef PRINT_DEBUG
  int i;

  for (i = 0; i < numargs; i++) {
    switch (args[i].type) {
    case NoArg:
      ConsoleDebug ("NoArg ");
      break;

    case StringArg:
      ConsoleDebug ("String: %s ", args[i].value.string_value);
      break;

    case ButtonArg:
      ConsoleDebug ("Button: %d %d ", args[i].value.button_value.offset,
		    args[i].value.button_value.base);
      break;

    case WindowArg:
      ConsoleDebug ("Window: %d %d ", args[i].value.button_value.offset,
		    args[i].value.button_value.base);
      break;

    case ManagerArg:
      ConsoleDebug ("Manager: %d %d ", args[i].value.button_value.offset,
		    args[i].value.button_value.base);
      break;

    default:
      ConsoleDebug ("bad ");
      break;
    }
  }
  ConsoleDebug ("\n");
#endif
}

static void print_binding (Binding *binding)
{
  int i;
  Function *func;

  if (binding->IsMouse) {
    ConsoleDebug ("\tMouse: %d\n", binding->Button_Key);
  }
  else {
    ConsoleDebug ("\tKey or action: %d %s\n", binding->Button_Key, 
		  binding->key_name);
  }

  ConsoleDebug ("\tModifiers: %d\n", binding->Modifier);
  ConsoleDebug ("\tAction: %s\n", binding->Action);
  ConsoleDebug ("\tFunction struct: 0x%x\n", binding->Function);
  func = binding->Function;
  while (func) {
    for (i = 0; i < num_builtins; i++) {
      if (func->func == builtin_functions[i].func) {
	ConsoleDebug ("\tFunction: %s 0x%x ", builtin_functions[i].name,
		      func->func);
	break;
      }
    }
    if (i > num_builtins) {
      ConsoleDebug ("\tFunction: not found 0x%x ", func->func);
    }
    print_args (func->numargs, func->args);
    func = func->next;
  }
}

void print_bindings (Binding *list)
{
#ifdef PRINT_DEBUG
  ConsoleDebug ("binding list:\n");
  while (list != NULL) {
    print_binding (list);
    ConsoleDebug ("\n");
    list = list->NextBinding;
  }
#endif
}

static int iswhite (char c)
{
  if (c == ' ' || c == '\t' || c == '\0')
    return 1;
  return 0;
}

static void skip_space (char **p)
{
  while (**p == ' ' || **p == '\t')
    (*p)++;
}

static char *stripcpy(char *source)
{
  char *tmp,*ptr;
  int len;

  if(source == NULL)
    return NULL;

  while(isspace(*source))
    source++;
  len = strlen(source);
  tmp = source + len -1;
  while(((isspace(*tmp))||(*tmp == '\n'))&&(tmp >=source))
    {
      tmp--;
      len--;
    }
  ptr = safemalloc(len+1);
  strncpy(ptr,source,len);
  ptr[len]=0;
  return ptr;
}

static void add_to_binding (Binding **list, Binding *binding)
{
  ConsoleDebug ("In add_to_binding:\n");

  if (*list == NULL) {
    *list = binding;
  }
  else {
    binding->LastBinding->NextBinding = *list;
    *list = binding;
  }
}

static int extract_int (char *p, int *n)
{
  char *s;
  int sign = 1;

  while (isspace (*p) && *p)
    p++;

  if (*p == '-') {
    sign = -1;
    p++;
  }
  else if (*p == '+') {
    sign = 1;
    p++;
  }

  if (*p == '\0') {
    return 0;
  }

  for (s = p; *s; s++) {
    if (*s < '0' || *s > '9') {
      return 0;
    }
  }

  *n = atoi (p) * sign;
  
  return 1;
}   

/****************************************************************************
 *
 * Gets the next "word" of input from char string indata.
 * "word" is a string with no spaces, or a qouted string.
 * Return value is ptr to indata,updated to point to text after the word
 * which is extracted.
 * token is the extracted word, which is copied into a malloced
 * space, and must be freed after use. 
 *
 **************************************************************************/
static char *GetNextToken(char *indata,char **token)
{ 
  char *t,*start, *end, *text;

  t = indata;
  if(t == NULL)
    {
      *token = NULL;
      return NULL;
    }
  while(isspace(*t)&&(*t != 0))t++;
  start = t;
  while(!isspace(*t)&&(*t != 0))
    {
      /* Check for qouted text */
      if(*t == '"')
	{
	  t++;
	  while((*t != '"')&&(*t != 0))
	    {
	      /* Skip over escaped text, ie \" or \space */
	      if((*t == '\\')&&(*(t+1) != 0))
		t++;
	      t++;
	    }
	  if(*t == '"')
	    t++;
	}
      else
	{
	  /* Skip over escaped text, ie \" or \space */
	  if((*t == '\\')&&(*(t+1) != 0))
	    t++;
	  t++;
	}
    }
  end = t;

  text = safemalloc(end-start+1);
  *token = text;

  while(start < end)
    {
      /* Check for qouted text */
      if(*start == '"')
	{	
	  start++;
	  while((*start != '"')&&(*start != 0))
	    {
	      /* Skip over escaped text, ie \" or \space */
	      if((*start == '\\')&&(*(start+1) != 0))
		start++;
	      *text++ = *start++;
	    }
	  if(*start == '"')
	    start++;
	}
      else
	{
	  /* Skip over escaped text, ie \" or \space */
	  if((*start == '\\')&&(*(start+1) != 0))
	    start++;
	  *text++ = *start++;
	}
    }
  *text = 0;
  if(*end != 0)
    end++;

  return end;
}

static void find_context(char *string, int *output, struct charstring *table,
			 char *tline)
{
  int i=0,j=0;
  Bool matched;
  char tmp1;

  *output=0;
  i=0;
  while(i<strlen(string))
    {
      j=0;
      matched = FALSE;
      while((!matched)&&(table[j].key != 0))
	{
	  /* in some BSD implementations, tolower(c) is not defined
	   * unless isupper(c) is true */
	  tmp1=string[i];
	  if(isupper(tmp1))
	    tmp1=tolower(tmp1);
	  /* end of ugly BSD patch */

	  if(tmp1 == table[j].key)
	    {
	      *output |= table[j].value;
	      matched = TRUE;
	    }
	  j++;
	}
      if(!matched)
	{
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("Bad context: %s\n", string);
	}
      i++;
    }
  return;
}

static int init_config_file (char *file)
{
#if FVWM_VERSION == 1
  if ((config_fp = fopen (file, "r")) == NULL)  {
    ConsoleMessage ("Couldn't open file: %s\n", file);
    return 0;
  }
#endif
  return 1;
}

static void close_config_file (void)
{
#if FVWM_VERSION == 1
  if (config_fp)
    fclose (config_fp);
#endif
}

static char *parse_term (char *string, TermStruct *term)
{
  char *rest, *token;
  int n;
  
  ConsoleDebug ("parse_term: %s\n", string);
  rest = GetNextToken (string, &token);
  if (token == NULL) {
    term->buttontype = NoButton;
    return NULL;
  }
  if (!strcasecmp (token, "focus")) {
    term->buttontype = FocusButton;
  }
  else if (!strcasecmp (token, "select")) {
    term->buttontype = SelectButton;
  }
  else if (extract_int (token, &n)) {
    term->buttontype = AbsoluteButton;
    term->offset = n;
  }
  else {
    ConsoleMessage ("Bad button: %s\n", token);
    term->buttontype = NoButton;
    Free (token);
    return NULL;
  }
  
  Free (token);
  return rest;
}

static char *parse_button (char *string, BuiltinArg *arg, int *flag)
{
  char *rest, *p, op = '\0';
  TermStruct term1, term2;

  *flag = 0;
  arg->value.button_value.offset = 0;
  arg->value.button_value.base = AbsoluteButton;

  rest = parse_term (string, &term1);
  if (term1.buttontype == NoButton) {
    return NULL;
  }
  
  p = rest;
  while (isspace (*p) && *p) 
    p++;
  if (*p == '+' && isspace (p[1])){
    op = '+';
    rest = p + 1;
  }
  else if (*p == '-' && isspace (p[1])) {
    op = '-';
    rest = p + 1;
  }
   
  if (op == '\0') {
    if (term1.buttontype == AbsoluteButton) {
      arg->value.button_value.offset = term1.offset;
    }
    else {
      arg->value.button_value.base = term1.buttontype;
    }
    *flag = 1;
    return rest;
  }

  rest = parse_term (rest, &term2);
  if (term2.buttontype != AbsoluteButton) {
    ConsoleMessage ("Second term of button expression must be an integer\n");
    return NULL;
  }
  
  if (op == '-')
    term2.offset *= -1;

  if (term1.buttontype == AbsoluteButton) {
    arg->value.button_value.offset = term1.offset + term2.offset;
  }
  else {
    arg->value.button_value.offset = term2.offset;
    arg->value.button_value.base = term1.buttontype;
  }

  *flag = 1;
  return rest;
}

static void free_function (Function *func)
{
  int i;
  
  for (i = 0; i < func->numargs; i++) {
    if (func->args[i].type == StringArg)
      Free (func->args[i].value.string_value);
  }
  Free (func);
}
  

static Function *parse_function (char *line)
{
  Function *ftype = (Function *)safemalloc (sizeof (Function));
  char *ptr, *name;
  int i, j, flag;

  ConsoleDebug ("in parse_function\n");

  ptr = GetNextToken (line, &name);
  if (name == NULL) {
    ConsoleMessage ("No function name\n");
    return NULL;
  }
  
  for (i = 0; i < num_builtins; i++) {
    if (!strcasecmp (name, builtin_functions[i].name)) {
      Free (name);
      ftype->func = builtin_functions[i].func;
      ftype->numargs = builtin_functions[i].numargs;
      ftype->next = NULL;

      for (j = 0; j < builtin_functions[i].numargs; j++) {
	ftype->args[j].type = builtin_functions[i].args[j];
	switch (builtin_functions[i].args[j]) {
	case StringArg:
	  ptr = GetNextToken (ptr, &ftype->args[j].value.string_value);
	  if (!ftype->args[j].value.string_value) {
	    ConsoleMessage ("%s: too few arguments\n",
			    builtin_functions[i].name);
	    return NULL;
	  }
	  ftype->args[j].type = builtin_functions[i].args[j];
	  break;

	case ButtonArg:
	case WindowArg:
	case ManagerArg:
	  ptr = parse_button (ptr, &ftype->args[j], &flag);
	  if (!flag) {
	    ConsoleMessage ("%s: too few arguments\n",
		     builtin_functions[i].name);
	    return NULL;
	  }
	  ftype->args[j].type = builtin_functions[i].args[j];
	  break;

	default:
	  ConsoleMessage ("internal error in parse_function\n");
	  return NULL;
	}
      }
      
      if (j != builtin_functions[i].numargs) {
	ConsoleMessage ("%s: too few arguments\n", builtin_functions[i].name);
	return NULL;
      }

      return ftype;
    }
  }

  
  ConsoleMessage ("Unknown function: %s\n", name);
  Free (name);

  return NULL;
}

Binding *ParseMouseEntry (char *tline)
{
  char modifiers[20],*ptr,*action,*token;
  Binding *new;
  int button;
  int n1=0,n2=0;
  int mods;

  /* tline points after the key word "key" */
  ptr = tline;
  ptr = GetNextToken(ptr,&token);  
  if(token != NULL) {
    n1 = sscanf(token,"%d",&button);
    Free(token);
  }

  action = GetNextToken(ptr,&token); 
  if(token != NULL) {
    n2 = sscanf(token,"%19s",modifiers);
    Free(token);
  }
  if((n1 != 1)||(n2 != 1))
    ConsoleMessage ("Mouse binding: Syntax error");
  
  find_context(modifiers,&mods,key_modifiers,tline);
  if((mods & AnyModifier)&&(mods&(~AnyModifier))) {
    ConsoleMessage ("Binding specified AnyModifier and other modifers too. Excess modifiers will be ignored.");
  }
  
  new  = (Binding *)safemalloc(sizeof(Binding));
  new->IsMouse = 1;
  new->Button_Key = button;
  new->key_name = NULL;
  new->Modifier = mods;
  new->Action = stripcpy(action);
  new->Function = parse_function (action);
  new->NextBinding = NULL;
  new->LastBinding = new;
  
  if (!new->Function) {
    ConsoleMessage ("Bad action: %s\n", action);
    Free (new->Action);
    Free (new);
    return NULL;
  }

  ConsoleDebug ("Mouse: %d %d %s\n", new->Button_Key,
		new->Modifier, new->Action);

  return new;
}

static Binding *ParseKeyEntry (char *tline)
{
  char *action,modifiers[20],key[20],*ptr, *token, *actionstring, *keystring;
  Binding *new = NULL, *temp, *last = NULL;
  Function *func = NULL;
  int i,min,max;
  int n1=0,n2=0;
  KeySym keysym;
  int mods;

  /* tline points after the key word "key" */
  ptr = tline;

  ptr = GetNextToken(ptr,&token);  
  if(token != NULL) {
    n1 = sscanf(token,"%19s",key);
    Free(token);
  }
  
  action = GetNextToken(ptr,&token);  
  if(token != NULL) {
    n2 = sscanf(token,"%19s",modifiers);
    Free(token);
  }

  if((n1 != 1)||(n2 != 1))
    ConsoleMessage ("Syntax error in line %s",tline);

  find_context(modifiers,&mods,key_modifiers,tline);
  if((mods & AnyModifier)&&(mods&(~AnyModifier))) {
    ConsoleMessage ("Binding specified AnyModifier and other modifers too. Excess modifiers will be ignored.");
  }

  /*
   * Don't let a 0 keycode go through, since that means AnyKey to the
   * XGrabKey call in GrabKeys().
   */
  if ((keysym = XStringToKeysym(key)) == NoSymbol || 
      XKeysymToKeycode(theDisplay, keysym) == 0) {
    ConsoleMessage ("Can't find keysym: %s\n", key);
    return NULL;
  }
  
 
  XDisplayKeycodes(theDisplay, &min, &max);
  for (i=min; i<=max; i++) {
    if (XKeycodeToKeysym(theDisplay, i, 0) == keysym) {
      if (!func) {
	func = parse_function (action);
	if (!func) {
	  ConsoleMessage ("Bad action: %s\n", action);
	  return NULL;
	}
	actionstring = stripcpy(action);
	keystring = stripcpy(key);
      }
      temp = new;
      new  = (Binding *)safemalloc(sizeof(Binding));
      new->IsMouse = 0;
      new->Button_Key = i;
      new->key_name = keystring;
      new->Modifier = mods;
      new->Action = actionstring;
      new->Function = func;
      new->NextBinding = temp;
      if (!last) {
	last = new;
      }
      new->LastBinding = last;

      ConsoleDebug ("Key: %d %s %d %s\n", i, new->key_name,
		    mods, new->Action);
    }
  }
  return new;
}

static Binding *ParseSimpleEntry (char *tline)
{
  Binding *new;
  Function *func;

  func = parse_function (tline);
  if (func == NULL)
    return NULL;

  new = (Binding *)safemalloc (sizeof (Binding));
  new->IsMouse = 0;
  new->Button_Key = 0;
  new->key_name = "select";
  new->Modifier = 0;
  new->Action = stripcpy (tline);
  new->Function = func;
  new->NextBinding = NULL;
  new->LastBinding = new;

  return new;
}

void run_function_list (Function *func)
{
  while (func) {
    func->func (func->numargs, func->args);
    func = func->next;
  }
}

void run_binding (WinManager *man, Action action)
{
  Binding *binding = man->bindings[action];
  ConsoleDebug ("run_binding:\n");
  print_bindings (binding);

  if (binding && binding->Function && binding->Function->func) {
    run_function_list (binding->Function);
  }
}

void execute_function (char *string)
{
  Function *func = parse_function (string);
  if (func == NULL) {
    return;
  }
  else {
    run_function_list (func);
    free_function (func);
  }
}

static int GetConfigLineWrapper (int *fd, char **tline)
{
#if FVWM_VERSION == 1

  static char buffer[1024];
  char *temp;

  if (fgets (buffer, 1024, config_fp)) {
    *tline = buffer;
    temp = strchr (*tline, '\n');
    if (temp) {
      *temp = '\0';
    }
    else {
      ConsoleMessage (stderr, "line too long\n");
      exit (1);
    }
    return 1;
  }

#else

  char *temp;

  GetConfigLine (fd, tline);
  if (*tline) {
    temp = strchr (*tline, '\n');
    if (temp) {
      *temp = '\0';
    }
    return 1;
  }

#endif

  return 0;
}

static char *read_next_cmd (ReadOption flag)
{
  static ReadOption status;
  static char *buffer;
  static char *retstring, displaced, *cur_pos;

  retstring = NULL;
  if (flag != READ_LINE && !(flag & status))
    return NULL;

  switch (flag) {
  case READ_LINE:
    while (GetConfigLineWrapper (Fvwm_fd, &buffer)) {
      cur_pos = buffer;
      skip_space (&cur_pos);
      if (!strncasecmp (Module, cur_pos, ModuleLen)) {
        retstring = cur_pos;
        cur_pos += ModuleLen;
        displaced = *cur_pos;
        if (displaced == '*') 
          status = READ_OPTION;
        else if (displaced == '\0')
          status = READ_LINE;
        else if (iswhite (displaced))
          status = READ_ARG;
        else 
          status = READ_LINE;
        break;
      }
    }
    break;

  case READ_OPTION:
    *cur_pos = displaced;
    retstring = ++cur_pos;
    while (*cur_pos != '*' && !iswhite (*cur_pos)) 
      cur_pos++;
    displaced = *cur_pos;
    *cur_pos = '\0';
    if (displaced == '*')
      status = READ_OPTION;
    else if (displaced == '\0')
      status = READ_LINE;
    else if (iswhite (displaced))
      status = READ_ARG;
    else 
      status = READ_LINE;
    break;

  case READ_ARG:
    *cur_pos = displaced;
    skip_space (&cur_pos);
    retstring = cur_pos;
    while (!iswhite (*cur_pos))
      cur_pos++;
    displaced = *cur_pos;
    *cur_pos = '\0';
    if (displaced == '\0')
      status = READ_LINE;
    else if (iswhite (displaced))
      status = READ_ARG;
    else 
      status = READ_LINE;
    break;

  case READ_REST_OF_LINE:
    status = READ_LINE;
    *cur_pos = displaced;
    skip_space (&cur_pos);
    retstring = cur_pos;
    break;
  }

  if (retstring && retstring[0] == '\0')
    retstring = NULL;

  return retstring;
}

static char *conditional_copy_string (char **s1, char *s2)
{
  if (*s1)
    return *s1;
  else
    return copy_string (s1, s2);
}

static NameType parse_format_dependencies (char *format)
{
  NameType flags = NO_NAME;

  ConsoleDebug ("Parsing format: %s\n", format);

  while (*format) {
    if (*format != '%') {
      format++;
    }
    else {
      format++;
      if (*format == 'i')
	flags |= ICON_NAME;
      else if (*format == 't')
	flags |= TITLE_NAME;
      else if (*format == 'c')
	flags |= CLASS_NAME;
      else if (*format == 'r')
	flags |= RESOURCE_NAME;
      else if (*format != '%')
	ConsoleMessage ("Bad format string: %s\n", format);
    }
  }
#ifdef PRINT_DEBUG
  ConsoleDebug ("Format depends on: ");
  if (flags & ICON_NAME)
    ConsoleDebug ("Icon ");
  if (flags & TITLE_NAME)
    ConsoleDebug ("Title ");
  if (flags & CLASS_NAME)
    ConsoleDebug ("Class ");
  if (flags & RESOURCE_NAME)
    ConsoleDebug ("Resource ");
  ConsoleDebug ("\n");
#endif

  return flags;
}

#define SET_MANAGER(manager,field,value)                           \
   do {                                                            \
     int id = manager;                                             \
     if (id == -1) {                                               \
       for (id = 0; id < globals.num_managers; id++) {             \
	 globals.managers[id].##field = value;                     \
       }                                                           \
     }                                                             \
     else if (id < globals.num_managers) {                         \
       globals.managers[id].##field = value;                       \
     }                                                             \
     else {                                                        \
       ConsoleMessage ("Internal error in SET_MANAGER: %d\n", id); \
     }                                                             \
   } while (0)

static void handle_button_config (int manager, int context, char *option)
{
  char *p;
  ButtonState state; 
	   
  p = read_next_cmd (READ_ARG);
  if (!p) {
    ConsoleMessage ("Bad line: %s\n", current_line);
    ConsoleMessage ("Need argument to %s\n", option);
    return;
  }
  else if (!strcasecmp (p, "flat")) {
    state = BUTTON_FLAT;
  }
  else if (!strcasecmp (p, "up")) {
    state = BUTTON_UP;
  }
  else if (!strcasecmp (p, "down")) {
    state = BUTTON_DOWN;
  }
  else {
    ConsoleMessage ("Bad line: %s\n", current_line);
    ConsoleMessage ("This isn't a valid button state: %s\n", p);
    return;
  }
  ConsoleDebug ("Setting buttonState[%s] to %s\n", 
		contextDefaults[context].name, p);
  SET_MANAGER (manager, buttonState[context], state);

  /* check for optional fore color */
  p = read_next_cmd (READ_ARG);
  if ( !p )
    return;
         
  ConsoleDebug ("Setting foreColorName[%s] to %s\n", 
		contextDefaults[context].name, p);
  SET_MANAGER (manager, foreColorName[context], 
	       copy_string (&globals.managers[id].foreColorName[context], p));

  /* check for optional back color */
  p = read_next_cmd (READ_ARG);
  if ( !p )
    return;
         
  ConsoleDebug ("Setting backColorName[%s] to %s\n",
		contextDefaults[context].name, p);
  SET_MANAGER (manager, backColorName[context], 
	       copy_string (&globals.managers[id].backColorName[context], p));
}

void read_in_resources (char *file)
{
  char *p, *q;
  int i, n, manager;
  char *option1;
  Binding *binding;
  Resolution r;

  if (!init_config_file (file))
    return;

  while ((p = read_next_cmd (READ_LINE))) {
    ConsoleDebug ("line: %s\n", p);
    save_current_line (p);

    option1 = read_next_cmd (READ_OPTION);
    if (option1 == NULL)
      continue;

    ConsoleDebug ("option1: %s\n", option1);
    if (!strcasecmp (option1, "nummanagers")) {
      /* If in transient mode, just use the default of 1 manager */
      if (!globals.transient) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	if (extract_int (p, &n) == 0) {
	  ConsoleMessage ("This is not a number: %s\n", p);
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	if (n > 0) {
	  allocate_managers (n);
	  ConsoleDebug ("num managers: %d\n", n);
	}
	else {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("You can't have zero managers. "
			  "I'll give you one.\n");
	  allocate_managers (1);
	}
      }
    }
    else {
      /* these all can specify a specific manager */

      if (globals.managers == NULL) {
	ConsoleDebug ("I'm assuming you only want one manager\n");
	allocate_managers (1);
      }

      manager = 0;

      if (option1[0] >= '0' && option1[0] <= '9') {
	if (globals.transient) {
	  ConsoleDebug ("In transient mode. Ignoring this line\n");
	  continue;
	}
	if (extract_int (option1, &manager) == 0 || 
	    manager <= 0 || manager > globals.num_managers) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("This is not a valid manager: %s.\n", option1);
	  manager = 0;
	}
	option1 = read_next_cmd (READ_OPTION);
	if (!option1) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
      }
      else if (!strcasecmp (option1, "transient")) {
	if (globals.transient) {
	  ConsoleDebug ("Transient manager config line\n");
	  manager = 1;
	  option1 = read_next_cmd (READ_OPTION);
	  if (!option1) {
	    ConsoleMessage ("Bad line: %s\n", current_line);
	    continue;
	  }
	}
	else {
	  ConsoleDebug ("Not in transient mode. Ignoring this line\n");
	  continue;
	}
      }

      manager--; /* -1 means global */

      ConsoleDebug ("Applying %s to manager %d\n", option1, manager);

      if (!strcasecmp (option1, "action")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}

	if (!strcasecmp (p, "mouse")) {
	  i = MOUSE;
	}
	else if (!strcasecmp (p, "key")) {
	  i = KEYPRESS;
	}
	else if (!strcasecmp (p, "select")) {
	  i = SELECT;
	}
	else {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("This isn't a valid action name: %s\n", p);
	  continue;
	}

	q = read_next_cmd (READ_REST_OF_LINE);
	if (!q) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("Need an action\n");
	  continue;
	}
	
	switch (i) {
	case MOUSE:
	  binding = ParseMouseEntry (q);
	  break;

	case KEYPRESS:
	  binding = ParseKeyEntry (q);
	  break;

	case SELECT:
	  binding = ParseSimpleEntry (q);
	  break;
	}

	if (binding == NULL) {
	  ConsoleMessage ("Offending line: %s\n", current_line);
	  ConsoleMessage ("Bad action\n");
	  continue;
	}

	if (manager == -1) {
	  int j;
	  for (j = 0; j < globals.num_managers; j++) {
	    add_to_binding (&globals.managers[j].bindings[i], binding);
	  }
	}
	else if (manager < globals.num_managers) {
	  add_to_binding (&globals.managers[manager].bindings[i], binding);
	}
	else {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("There's no manager %d\n", manager);
	}
      }
      else if (!strcasecmp (option1, "background")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	ConsoleDebug ("default background: %s\n", p);

        for ( i = 0; i < NUM_CONTEXTS; i++ )
	  SET_MANAGER (manager, backColorName[i], 
	    conditional_copy_string (&globals.managers[id].backColorName[i], 
				     p));
      }
      else if (!strcasecmp (option1, "dontshow")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	do {
	  ConsoleDebug ("dont show: %s\n", p);
	  if (manager == -1) {
	    int i;
	    for (i = 0; i < globals.num_managers; i++)
	      add_to_stringlist (&globals.managers[i].dontshow, p);
	  }
	  else {
	    add_to_stringlist (&globals.managers[manager].dontshow, p);
	  }
	  p = read_next_cmd (READ_ARG);
	} while (p);
      }
      else if (!strcasecmp (option1, "drawicons")) {
#ifdef MINI_ICONS
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("Need argument to drawicons\n");
	  continue;
	}
	if (!strcasecmp (p, "true")) {
	  i = 1;
	}
	else if (!strcasecmp (p, "false")) {
	  i = 0;
	}
	else {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("What is this: %s?\n", p);
	  continue;
	}
	ConsoleDebug ("Setting drawicons to: %d\n", i);
	SET_MANAGER (manager, draw_icons, i);
#else
	ConsoleMessage ("DrawIcons support not compiled in\n");
#endif
      }
      else if (!strcasecmp (option1, "followfocus")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("Need argument to followfocus\n");
	  continue;
	}
	if (!strcasecmp (p, "true")) {
	  i = 1;
	}
	else if (!strcasecmp (p, "false")) {
	  i = 0;
	}
	else {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("What is this: %s?\n", p);
	  continue;
	}
	ConsoleDebug ("Setting followfocus to: %d\n", i);
	SET_MANAGER (manager, followFocus, i);
      }
      else if (!strcasecmp (option1, "font")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	ConsoleDebug ("font: %s\n", p);

	SET_MANAGER (manager, fontname, 
		     copy_string (&globals.managers[id].fontname, p));
      }
      else if (!strcasecmp (option1, "foreground")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	ConsoleDebug ("default foreground: %s\n", p);

        for ( i = 0; i < NUM_CONTEXTS; i++ )
	SET_MANAGER (manager, foreColorName[i], 
           conditional_copy_string (&globals.managers[id].foreColorName[i], 
				    p));
      }
      else if (!strcasecmp (option1, "format")) {
	char *token;
	NameType flags;

	p = read_next_cmd (READ_REST_OF_LINE);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	GetNextToken (p, &token);
	
	SET_MANAGER (manager, formatstring,
		     copy_string (&globals.managers[id].formatstring, token));
	flags = parse_format_dependencies (token);
	SET_MANAGER (manager, format_depend, flags);
	Free (token);
      }
      else if (!strcasecmp (option1, "geometry")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}

	SET_MANAGER (manager, geometry, 
		     copy_string (&globals.managers[id].geometry, p));
      }
      else if (!strcasecmp (option1, "iconname")) {
	char *token;
	p = read_next_cmd (READ_REST_OF_LINE);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	GetNextToken (p, &token);
	
	SET_MANAGER (manager, iconname,
		     copy_string (&globals.managers[id].iconname, token));
	Free (token);
      }
      else if (!strcasecmp (option1, "resolution")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	ConsoleDebug ("resolution: %s\n", p);
	if (!strcasecmp (p, "global"))
	  r = SHOW_GLOBAL;
	else if (!strcasecmp (p, "desk"))
	  r = SHOW_DESKTOP;
	else if (!strcasecmp (p, "page"))
	  r = SHOW_PAGE;
	else {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("What kind of resolution is this?\n");
	  continue;
	}

	SET_MANAGER (manager, res, r);
      }
      else if (!strcasecmp (option1, "show")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	do {
	  ConsoleDebug ("show: %s\n", p);
	  if (manager == -1) {
	    int i;
	    for (i = 0; i < globals.num_managers; i++)
	      add_to_stringlist (&globals.managers[i].show, p);
	  }
	  else {
	    add_to_stringlist (&globals.managers[manager].show, p);
	  }
	  p = read_next_cmd (READ_ARG);
	} while (p);
      }
      else if (!strcasecmp (option1, "showtitle")) {
	ConsoleMessage ("Bad line: %s\n", current_line);
	ConsoleMessage ("showtitle is no longer an option. Use format\n");
	continue;
      }
      else if (!strcasecmp (option1, "sort")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("Need argument to sort\n");
	  continue;
	}
	if (!strcasecmp (p, "true")) {
	  i = 1;
	}
	else if (!strcasecmp (p, "false")) {
	  i = 0;
	}
	else {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("What is this: %s?\n", p);
	  continue;
	}
	ConsoleDebug ("Setting sort to: %d\n", i);
	SET_MANAGER (manager, sort, i);
      }
      else if (!strcasecmp (option1, "title")) {
	char *token;
	p = read_next_cmd (READ_REST_OF_LINE);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  continue;
	}
	GetNextToken (p, &token);
	
	SET_MANAGER (manager, titlename,
		     copy_string (&globals.managers[id].titlename, token));
	Free (token);
      }
      else if (!strcasecmp (option1, "plainButton")) {
	handle_button_config (manager, PLAIN_CONTEXT, option1);
      }
      else if (!strcasecmp (option1, "selectButton")) {
	handle_button_config (manager, SELECT_CONTEXT, option1);
      }
      else if (!strcasecmp (option1, "focusButton")) {
	handle_button_config (manager, FOCUS_CONTEXT, option1);
      }
      else if (!strcasecmp (option1, "focusandselectButton")) {
	handle_button_config (manager, FOCUS_SELECT_CONTEXT, option1);
      }
      else if (!strcasecmp (option1, "usewinlist")) {
	p = read_next_cmd (READ_ARG);
	if (!p) {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("Need argument to usewinlist\n");
	  continue;
	}
	if (!strcasecmp (p, "true")) {
	  i = 1;
	}
	else if (!strcasecmp (p, "false")) {
	  i = 0;
	}
	else {
	  ConsoleMessage ("Bad line: %s\n", current_line);
	  ConsoleMessage ("What is this: %s?\n", p);
	  continue;
	}
	ConsoleDebug ("Setting usewinlist to: %d\n", i);
	SET_MANAGER (manager, usewinlist, i);
      }
      else {
	ConsoleMessage ("Bad line: %s\n", current_line);
	ConsoleMessage ("Unknown option: %s\n", p);
      }
    }
  }

  if (globals.managers == NULL) {
    ConsoleDebug ("I'm assuming you only want one manager\n");
    allocate_managers (1);
  }
  print_managers();
  close_config_file();
}

