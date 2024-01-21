% mailalias.sl		-*- SLang -*-
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
%
% The mail entry point: mailalias_expand ();
%
% relies on the X-Mailer: field to indicate the end of the header
%

variable Mailalias_buf = " *mailalias*";
%variable Mailalias_buf = " *mailalias tmp*";

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

Mailaliases = "";
define mailalias_xAlias();	% forward reference

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% take a comma-delimited list of aliases and return a list of fully
% expanded names and addresses
define mailalias_xList (raw)
{
   variable argc = 0, argv, expanded = Null_String;

   while ((argv = mailalias_xAlias (extract_element (raw, argc, ','))),
	  strlen (argv))
     {
	if (argc) expanded = strcat (expanded, ",\n\t");
	expanded = strcat (expanded, argv);
	argc++;
     }
   strtrim (expanded);		% return on the stack
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expands alias => "Full Name" <user@machine.domain>
% quotes any names that contain '@' to <name@machine>
% for a [TAB] delimited field

define mailalias_xAlias (alias)
{
   variable name = Null_String, addr = Null_String;
   variable lt = char ('<'), gt = char ('>');

   USER_BLOCK0
     {
	push_mark ();		% extract a field
	skip_chars ("^\t");	% next field
	bufsubstr ();		% return the field on the stack
     }
   
   alias = strtrim (alias);
   !if (strlen (alias)) return addr;

   if (is_substr (alias, "@"))
     {
	% "<user@machine.domain>"
	if (is_substr (alias, lt))
	  alias;
	else
	  strncat (lt, alias, gt, 3);
	return;
     }

   bob ();
   !if ( bol_fsearch (strcat (alias, "\t"))) return alias;
   go_right (strlen (alias) + 1);

   % hand-rolled functions, regular expressions are too inconsistent
   % and do not work across lines
   X_USER_BLOCK0;
   =name;			% fullname or surname
   name = strtrim (strncat (strtrim (extract_element (name, 1, ',')), " ",
			    strtrim (extract_element (name, 0, ',')), 3));

   % "quote" name in case it contains one of `,.@'
   name = strncat ("\"", name, "\" ", 3);
   go_right (1);		% now looking at address

   if (looking_at_char ('('))	% distribution list (with parentheses)
     {
	name = Null_String;	% ignore name of distribution list?
	go_right (1);
	push_mark ();
	!if (fsearch (")")) skip_chars ("^\t ");	% messed-up entry?
	addr = mailalias_xList (bufsubstr ());
     }
   else
     {
	X_USER_BLOCK0;
	=addr;
	addr = strncat (lt, addr, gt, 3);
     }
   strcat (name, addr);		% "Full Name" <user@machine.domain>
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expands alias => user@machine.domain
% for a [SPACE] delimited field
define mailrc_xAlias (alias)
{
   variable name, addr = Null_String;
   variable space = char (' ');

   alias = strtrim (alias);
   !if (strlen (alias)) return addr;
   if (is_substr (alias, "@")) return alias;

   % look for "alias name"
   name = strcat ("alias ", alias);
   bob ();
   forever
     {
	!if (bol_fsearch (name))
	  {
	     % look for "group name"
	     name = strncat ("group ", alias, space, 3);
	     bob ();
	     !if (bol_fsearch (name)) return alias;
	  }
	go_right (strlen (name));
	POINT;
	skip_white ();
	if (POINT != ())
	  break;
     }
   
	  
   % hand-rolled functions, regular expressions are too inconsistent
   do
     {
	push_mark ();
	skip_chars ("^\t ");	% next address
	name = bufsubstr ();	% address
	skip_chars ("\t ");	% next address
	if (strlen (name))
	  {
	     if (strlen(addr))
	       addr = strncat (addr, ",\n\t", name, 3);
	     else
	       addr = name;
	  }
     } while (not (eolp ()));
   addr;
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% look for the addresses that follow FIELD and are before the next one.
%
define mailalias_xField (field)
{
   variable n, addr, buf = "*mail*";

   bob ();
   !if (bol_fsearch (field)) return;	% not there - trouble!
   go_right (strlen (field));
   push_mark ();

   !if (re_fsearch ("^[-A-Za-z]+:"))	% search for next header line
     {
	pop_mark ();
	return;
     }
   call ("exchange");
   addr = bufsubstr ();
   n = strlen (addr);
   deln (n--, n);		% don't delete newline character
   setbuf (Mailalias_buf);
   addr = strcat ("\t", mailalias_xList (addr));
   sw2buf (buf);
   insert (addr);
}

%!% expands "To, Cc, Bcc" mail header fields using the aliases/addresses
%!% that are defined in Mailaliases
define mailalias_expand ()
{
   variable buf = "*mail*";
   variable header;

   if ( strcmp (buf, whatbuf) ) error ("Mail mode only");
   push_spot ();
   bob ();
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
   if ( insert_file (Mailaliases) > 0 )
     {
	set_buffer_modified_flag (0);   % mark unmodified
	sw2buf (buf);

	% order isn't important
	Null_String;
	"From:";
	"Reply-To:";
	"Bcc:";
	"Cc:";
	"To:";

	while ( =header, strlen (header))
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check that everything is okay

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

% ~/.addressbook is okay, un-load Mailrc stuff
if (strlen(Mailaliases) and (1 == file_status (Mailaliases)))
{
   eval (".()mailrc_xAlias");
   Mailrc = Null_String;
}
% ~/.mailrc is okay, then un-load addressbook stuff & over-load definition
else if (strlen (Mailrc) and (1 == file_status (Mailrc)))
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
