% mailalias.sl	-*- mode: slang; mode: fold -*-
%
% expand any aliases in the To, Cc, Bcc, From, Reply-To: lines
%
% - use Pine-style (TAB delimited) aliases
% - use UCB Mail (space delimited) aliases
%
% Pine (.addressbook) style:
% alias-name[TAB]Family Name(s), First Name(s)[TAB]e-mail-address
% alias-name[TAB]space for name[TAB]e-mail-address
%
% UCB Mail (.mailrc) style
% alias alias-name e-mail-address
% group alias-name address1 address2 ...
%
% The mail entry point: mailalias_expand ();
% relies on the X-Mailer: field to indicate the end of the header
%
% add to your mail_hook()
%
% define mail_hook ()
% {
%    local_setkey ("mailalias_expand", "^C^E");
%    ...
% }
%

%{{{ Public Variables
%!% where to find the [TAB] delimited mail alias list
!if (is_defined ("Mailaliases"))
{
   variable Mailaliases = dircat (getenv ("HOME"), ".addressbook");
}

%!% where to find the [SPACE] delimited mail alias list
!if (is_defined ("Mailrc"))
{
   variable Mailrc = getenv ("MAILRC");
   !if (strlen(Mailrc)) Mailrc = dircat (getenv ("HOME"), ".mailrc");
}
%}}}

%{{{ Private Variables
variable Mailalias_buf = " *mailalias*";
%}}}
%{{{ Private Functions
%{{{ forward reference
define mailalias_xAlias();
%}}}

% take a comma-delimited list of aliases and return a list of fully
% expanded names and addresses
define mailalias_xList (raw)	%{{{
{
   variable str, list = Null_String;

   if (strlen (Mailrc))
     {
	raw = str_replace_all (raw, " ", ",");
     }

   raw = strcompress (raw, ",");
   loop (strchopr (raw, ',', 0))
     {
	str = mailalias_xAlias (strtrim (()));
	if (strlen (str))
	  {
	     if (strlen (list))
	       list = strcat (list, ",\n\t");

	     list = strcat (list, str);
	  }
     }
   strtrim (list);		% return on the stack
}
%}}}

% expands alias => "Full Name" <user@machine.domain>
% quotes any names that contain '@' to <name@machine>
% for a [TAB] delimited field
define mailalias_xAlias (alias)	%{{{
{
   variable name, addr = Null_String;
   variable lt = char ('<'), gt = char ('>');

   !if (strlen (alias)) return addr;
   if (is_substr (alias, char ('@')))
     {
	% "<user@machine.domain>"
	if (is_substr (alias, lt)) alias; else strncat (lt, alias, gt, 3);
	return;
     }

   bob ();
   !if (right (bol_fsearch (strcat (alias, "\t")))) return alias;

   % hand-rolled functions, regular expressions are too inconsistent
   % and do not work across lines
   USER_BLOCK0
     {
	push_mark ();		% extract a field
	skip_chars ("^\t");	% next field
	bufsubstr ();		% return the field on the stack
	go_right (1);		% skip TAB char
     }

   name = X_USER_BLOCK0;	% fullname or surname
   name = strtrim (strncat (strtrim (extract_element (name, 1, ',')), " ",
			    strtrim (extract_element (name, 0, ',')), 3));

   % "quote" name in case it contains one of `,.@'
   name = strncat ("\"", name, "\" ", 3);

   if (looking_at_char ('('))	% distribution list (with parentheses)
     {
	name = Null_String;	% ignore name of distribution list?
	go_right (1);
	push_mark ();
	!if (fsearch_char (')')) skip_chars ("^\t ");	% messed-up entry?
	addr = mailalias_xList (bufsubstr ());
     }
   else
     {
	addr = X_USER_BLOCK0;
	addr = strncat (lt, addr, gt, 3);
     }
   strcat (name, addr);		% "Full Name" <user@machine.domain>
}
%}}}

% expands alias => user@machine.domain
% for a [SPACE] delimited field
define mailrc_xAlias (alias)	%{{{
{
   variable name, addr = Null_String;
   variable space = char (' ');
   variable n;

   alias = strtrim (alias);
   
   if (not(strlen(alias)) 
       or is_substr (alias, char ('@')))
     return alias;

   % look for "alias name"
   name = strncat ("alias ", alias, space, 3);
   
   bob ();

   % Convert all tabs to spaces
   replace ("\t", space);
   
   n = bol_fsearch (name);
   !if (n)
     {
	% look for "group name"
	name = strncat ("group ", alias, space, 3);
	n = bol_fsearch (name);
	!if (n) return alias;
     }
   
   go_right (n);
   
   % hand-rolled functions, regular expressions are too inconsistent
   forever
     {
	skip_white ();
	if (eolp ()) break;
	push_mark ();
	skip_chars ("^ ");	% next address
	name = bufsubstr ();	% address
	if (strlen (addr)) strncat (addr, ",\n\t", name, 3); else name;
	addr = ();
     }
   addr;
}
%}}}

% look for the addresses that follow FIELD and are before the next one.
define mailalias_xField (field)	%{{{
{
   variable n, addr, buf = "*mail*";

   bob ();
   !if (right (bol_fsearch (field))) return;	% not there - trouble!
   push_mark ();

   !if (re_fsearch ("^[-A-Za-z]+:"))	% search for next header line
     {
	pop_mark_1 ();
	return;
     }
   exchange_point_and_mark ();
   addr = bufsubstr ();
   n = strlen (addr);
   deln (n--, n);		% don't delete newline character
   setbuf (Mailalias_buf);
   addr = strcat ("\t", mailalias_xList (addr));
   sw2buf (buf);
   insert (addr);
}
%}}}
%}}}

%!% expands "To, Cc, Bcc" mail header fields using the aliases/addresses
%!% that are defined in Mailaliases
define mailalias_expand ()	%{{{
{
   variable buf = "*mail*";

   if (strcmp (buf, whatbuf)) error ("Mail mode only");
   push_spot_bob ();
   !if (bol_fsearch ("X-Mailer:"))
     {
	!if (bol_fsearch ("Subject:"))
	  {
	     pop_spot ();
	     error ("Failed to find text divider.");
	  }
     }

   push_mark ();
   bob ();
   narrow ();

   setbuf (Mailalias_buf);
   if (insert_file (Mailaliases) > 0)
     {
	variable header;
	set_buffer_modified_flag (0);   % mark unmodified
	sw2buf (buf);

	% order isn't important
	Null_String;
	"From:";
	"Reply-To:";
	"Bcc:";
	"Cc:";
	"To:";

	while (header = (), strlen (header))
	  mailalias_xField (header);
     }
   else
     {
	sw2buf (buf);
     }
   delbuf (Mailalias_buf);
   widen ();
   pop_spot ();
}
%}}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{{{ check that everything is okay, unload as appropriate

% ~/.addressbook is okay, un-load Mailrc stuff
if (1 == file_status (Mailaliases))
{
   eval (".()mailrc_xAlias");
   Mailrc = Null_String;
}
% ~/.mailrc is okay, then un-load addressbook stuff & over-load definition
else if (1 == file_status (Mailrc))
{
   eval (".(mailrc_xAlias) mailalias_xAlias");
   Mailaliases = Mailrc;
}
% nothing seems to have worked, so get rid of everything
else
{
   eval (".()mailrc_xAlias");
   eval (".()mailalias_xAlias");
   eval (".()mailalias_xList");
   eval (".()mailalias_xField");
   eval (".()mailalias_expand");
   Mailaliases = Null_String;
}
%}}}
