.\" BEGINPART D
.SH CUSTOMIZED ARTICLE HEADER PRESENTATION
Normally, \fInn\fP will just print a (high-lighted) single line header
containing the author, subject, and date (optional) of the article
when it is read.
.LP
By setting the
.B header-lines
variable as described below, it is possible to get a more informative
multi line header with optional high-lighting and underlining.
.LP
The
.B header-lines
variable is set to a list of header line identifiers, and the
customized headers will then contain exactly these header lines
\fIin the specified order\fP.
.LP
The same specifications are also used by the \fB:print\fP and
\fBsave-short\fP commands via the \fBprint-header-lines\fP and
\fBsave-header-lines\fP variables.
.LP
The following header line identifiers are recognized in the
\fBheader-lines\fP,
\fBprint-header-lines\fP, 
and \fBsave-header-lines\fP variables:
.LP
.in +8n
.ta 5m
.\"ta 4 9
.br
\fBA\fP	Approved:
.br
\fBa\fP	Spool-File:	(path of spool file containing the article)
.br
\fBB\fP	Distribution:
.br
\fBC\fP	Control:
.br
\fBD\fP	Date:
.br
\fBd\fP	Date-Received:
.br
\fBF\fP	From:
.br
\fBf\fP	Sender:
.br
\fBG\fP	Newsgroup:	(current group)
.br
\fBg\fP	Newsgroup:	(current group if cross-posted or merged)
.br
\fBI\fP	Message-Id:
.br
\fBK\fP	Keywords:
.br
\fBL\fP	Lines:
.br
\fBN\fP	Newsgroups:
.br
\fBn\fP	Newsgroups:   (but only if cross posted)
.br
\fBO\fP	Organization:
.br
\fBP\fP	Path:
.br
\fBR\fP	Reply-To:
.br
\fBS\fP	Subject:
.br
\fBv\fP	Save-File:	(the default save file for this article)
.br
\fBW\fP	Followup-To:
.br
\fBX\fP	References:
.br
\fBx\fP	Back-References:
.br
\fBY\fP	Summary:
.in -8n
.DT
.LP
The 'G' and 'g' fields will include the local article number if it is
known, e.g.
.nf
	Newsgroup: news.software.nn/754
.fi
.LP
The following special symbols are recognized in the \fBheader-lines\fP
variable (and ignored otherwise):
.LP
Preceding the identifier with an equal sign "=" or an underscore "_"
will cause the header field contents to be high-lighted or underlined.
.LP
A plus sign "+" will use the shading attribute defined by
\fBshading-on\fP and \fBshading-off\fP to high-light the field
contents.  If no shading attribute is defined it will underline the
field instead.
.LP
Including an asterisk "*" in the list will produce the standard one
line header at that point.
.LP
Example:  The following setting of the
.B header-lines
variable will show the author (underlined), organization, posting
date, and subject (high-lighted) when articles are read:
.sp 0.5v
.nf
	set header-lines _FOD=S
.fi
.SH COMMAND LINE OPTIONS
Some of the command line options have already been described, but
below we provide a complete list of the effect of each option by
showing the equivalent
.BR set ,
.BR unset ,
or
.B toggle
command.
.LP
Besides the options described below, you can set \fIany\fP of
\fInn\fP's variables directly on the command line via an argument of
the following format:
.sp 0.5v
.nf
	variable=value
.fi
.sp 0.5v
To set or unset a boolean variable, the value can be specified as
\fIon\fP or \fIoff\fP (\fIt\fP and \fIf\fP will also work).
.LP
Notice that the init files are read \fIbefore\fP the options are
parsed (unless you use the \-\fBI\fP option).  Therefore, the options
which are related to boolean variables set in the init file will
toggle the value set there, rather than the default value.
Consequently, the meaning of the options are also user-defined.
.LP
The explanations below describe the effect related to the default
setting of the variables, with the `reverse' effect in square
brackets.
.TP
\-\fBa\fP\fIN\fP	{\fBset limit\fP \fIN\fP}
.I Limit
the maximum number of articles presented in each group to
.I N.
This is useful to get up-to-date quickly if you have not
read news for a longer period.
.TP
\-\fBa0\fP
Mark
.I all
unread articles as read.  See the full explanation at the beginning of
this manual.
.TP
\-\fBB\fP	{\fBtoggle backup\fP}
Do not [do] backup the rc file.
.TP
\-\fBd\fP	{\fBtoggle split\fP}
Do not [do] split digests into separate articles.
.TP
\-\fBf\fP	{\fBtoggle fsort\fP}
Do not [do] sort folders according to the subject (present the
articles in a folder in the sequence in which they were saved).
.TP
\-\fBg\fP
Prompt for the name of a news group or folder to be entered
.TP
\-\fBi\fP	{\fBtoggle case-fold-search\fP}
Normally searches with \-\fBn\fP and \-\fBs\fP are case independent.
Using this option, the case becomes significant.
.TP
\-\fBI\fP
Do not read the init file.  This must be the first option!!
The global \fIsetup\fP file is still read.
.TP
\-\fBI\fP\fIfile-list\fP
Specifies an alternate list of init files to be loaded instead of the
standard global and private init files.  The list is a comma-separated
list of file names.  Names which does not contain a `/' are looked for
in the ~/.nn directory.  An empty element in the list is interpreted
as the global init file.  The list of init files must
\fInot\fP be separated from the \fB\-I\fP option by blanks, and it
must be the first option.  Example:  The default behaviour corresponds
to using -I,init (first the global file, then the file ~/.nn/init).
The global \fIsetup\fP file is still read as the first init file
independently of the -I option used.
.TP
\-\fBk\fP	{\fBtoggle kill\fP}
Do not [do] perform automatic kill and selection of articles.
.TP
\-\fBl\fP\fIN\fP	{\fBset stop\fP \fIN\fP}
Stop after printing the first \fIN\fP lines of each article.
This is useful on slow terminals.
.TP
\-\fBL\fP[\fIf\fP]	{\fBset layout\fP \fIf\fP}
Select alternative menu layout
.IR f
(0 to 4).
If
.I f
is omitted, menu layout 3 is selected.
.TP
\-\fBm\fP	{\fIno corresponding variable\fP}
Merge all articles into one `meta group' instead of showing
them one group at a time.  When -m is used, no articles will be marked
as read.
.TP
\-\fBn\fP\fIWORD\fP
Collect only articles which contain the string \fIWORD\fP in the
sender's name (case is ignored).  If \fIWORD\fP
starts with a slash `/', the rest of the argument is used as a
\fIregular expression\fP instead of a fixed string.
.TP
\-\fBN\fP	{\fIno corresponding variable\fP}
Disable updating of the rc file.  This includes not recording that
groups have been read or unsubscribed to (although \fInn\fP will think
so until you quit).
.TP
\-\fBq\fP	{\fBtoggle sort\fP}
Do not [do] sort the articles (q means quick, but it isn't
any quicker in practice!)
.TP
\-\fBQ\fP	{\fBtoggle silent\fP}
Quiet mode - don't [do] print the logo or "No News" messages.
.TP
\-\fBr\fP	{\fBtoggle repeat-group-query\fP}
Make \-\fBg\fP repeat query for a group to enter.
.TP
\-\fBs\fP\fIWORD\fP
Collect only articles which contain the string
.I WORD
in their subject (case is ignored).  If
.I WORD
starts with a slash `/', the rest of the argument is used as a
\fIregular expression\fP instead of a fixed string.
.TP
\-\fBS\fP	{\fBtoggle repeat\fP}
Do not [do] eliminate duplicated subject lines on menus.
.TP
\-\fBT\fP	{\fBtoggle time\fP}
Do not [do] show the current time in the prompt line.
.TP
\-\fBw\fP[\fIN\fP]	{\fBset window\fP \fIN\fP}
Reserve \fIN\fP lines of the menu screen for a preview window.  If
\fIN\fP is omitted, the preview window is set to 5 lines.
.TP
\-\fBW\fP	{\fBtoggle confirm-messages\fP}
[Don't] Wait for confirmation on all messages.
.TP
\-\fBx\fP[\fIN\fP]	{\fBset old N\fP}
Present (or scan) all (or the last \fIN\fP) unread as well as
read articles.  This will
.I never
mark unread articles as read.
.TP
\-\fBX\fP	{\fIno corresponding variable\fP}
Read/scan unsubscribed groups also.  Most useful when looking for
a specific subject in all groups, e.g.
.br
   nn -mxX -sSubject all
.br
.SH MACRO DEFINITIONS
Practically any combination of commands and key strokes can be defined
as a macro which can be bound to a single key in menu and/or reading mode.
.LP
The macro definition must specify a sequence of commands and key
strokes as if they were typed directly from the keyboard.  For
example, a string specifying a file name must follow a save command.
This manual does not give a complete specification of all the input
required by the various commands; it is recommended to execute the
desired command sequence from the keyboard prior to defining the macro
to get the exact requirements of each command.
.LP
Although it is possible to define temporary macros interactively using the
.B :define
command, macro definitions are normally placed in the
.I init
file.  Macros are numbered from 0 to 100, i.e. it is possible to define
a total of 101 different macros (implicit macros defined with the
\fBmap\fP command uses internal numbers from 101 to 200).
.LP
To define macro number \fIM\fP, the following construction is used
(the line breaks are mandatory):
.nf
	\fBdefine\fP \fIM\fP
		\fIbody\fP
	\fBend\fP
.fi
.LP
The \fIbody\fP consists of a sequence of \fItokens\fP separated by
white space (blanks or newlines).  However, certain \fItokens\fP
continue to the end of the current line.
.LP
The following \fItokens\fP may occur in the macro \fIbody\fP:
.TP
.I Comments
Empty lines and text following a # character (preceded by white space)
is ignored.
.TP
.I Command Names
Any command name listed in the key mapping section can be included in
a macro causing that command to be invoked when the macro is executed.
.TP
.I Extended Commands
All the extended commands which can be executed through the
.B command
command (normally bound to the : key) can also be executed in a macro.
An extended command starts with a colon (:) and continues to the
end of the current line.  Example:
.nf
	:show groups total
.fi
.TP
.I Key Strokes
A key stroke (which is normally mapped into a command depending on the
current mode) is specified as a key name enclosed in single quotes.
Examples (A-key, left arrow key, RETURN key):
.nf
	'A'  'left'  '^M'
.fi
.TP
.I Shell Commands
External commands can be invoked as part of a macro execution.  There
are two forms of shell command invocations available depending on
whether a command \fImay\fP produce output or require user input, or
it is \fIguaranteed\fP to complete without input or output to the
terminal.  The difference is that in the latter case, \fInn\fP does
not prepare the terminal to be used by another program.  When the
command completes, the screen is \fInot\fP redrawn
automatically; you should use the \fBredraw\fP command to do that.
The tho forms are:
.sp 0.5v
.nf
	:!echo this command uses the terminal
	:!!echo this command does not > /tmp/file
.fi
.TP
.I Strings
Input to commands prompting for a string, e.g. a file name, can be
specified in a macro as a double quoted string.  Example (save without
prompting for a file name):
.nf
	\fBsave-short\fP "+$G"
.fi
.TP
.I Conditionals
Conditionals may occur anywhere in a macro; a conditional is evaluated
when the macro is executed, and if the condition is false \fIthe rest
of the current line is ignored\fP.  The following conditionals are available:
.nf
	\fB?menu\fP	True in menu mode
	\fB?show\fP	True in reading mode
	\fB?folder\fP	True when looking at a folder
	\fB?group\fP	True when looking at a news group
	\fB?yes\fP	Query user, true if answer is yes
	\fB?no\fP	Query user, true if answer is no
.fi
Example (stop macro execution if user rejects to continue):
.nf
	\fBprompt\fP "continue? " \fB?no break\fP
.fi
.sp 0.5v
In addition to these conditionals, it is possible to test the current
value of boolean and integer variables using the following form:
.nf
	\fB?\fP\fIvariable\fP\fB=\fP\fIvalue\fP
.fi
This conditional will be true (1) if the variable is an integer variable
whose current value is the one specified, or (2) if the variable is a
boolean variable which is either \fBon\fP or \fBoff\fP.  Examples:
.nf
	?layout=3 :set layout 1
	?monitor=on  break
	?sort=off :sort age
.fi
.TP
.B break
Terminate macro execution completely.  This includes nested macros.
Example (stop if looking at a folder):
.nf
	\fB?folder\fP \fBbreak\fP
.fi
.TP
.B return
Terminate execution of current macro.  If the current macro is called
from another macro, execution of that macro continues immediately.
.TP
.B input
Query the user for a key stroke or a string, for example a file name.
Example (prompt the user for a file name in the usual way):
.nf
	\fBsave-short\fP \fBinput\fP
.fi
.TP
.B yes
Confirm unconditionally \fIif\fP a command requires confirmation.  It
is ignored if the command does not require confirmation.  Example
(confirm creation of new files):
.nf
	\fBsave-short\fP "+$G" \fByes\fP
.fi
.TP
.B no
Terminate execution of current macro \fIif\fP a command requires
confirmation; otherwise ignore it.  If neither \fByes\fP nor \fBno\fP
is specified when a command requires confirmation, the user must
answer the question as usual \- if the user confirms the action
execution continues normally; otherwise the execution of the current
macro is terminated.  Example (do not create new files):
.nf
	\fBsave-short\fP "+$L/misc" \fBno\fP
.fi
.TP
\fBprompt\fP \fIstring\fP
Print the \fIstring\fP in the prompt line (highlighted).  The string
must be enclosed in double quotes.  Example:
.nf
	\fBprompt\fP "Enter recipient name"
.fi
When the macro terminates, the original prompt shown on entry to the
macro will automatically be redrawn.  If this is not desirable (e.g.
if the macro goes from selection to reading mode), the redrawing of
the prompt can be disabled by using a \fBprompt\fP command with an
empty \fIstring\fP ("").  Example:
.nf
	\fBprompt\fP "Enter reading mode?" # old prompt is saved
	?no return # and old prompt is restored
	read-skip       # changes the prompt
	\fBprompt\fP "" # so forget old prompt
.fi
.TP
\fBecho\fP \fIstring\fP
Display the \fIstring\fP in the prompt line for a short period.  Example:
.nf
	?show \fBecho\fP "Cannot be used in reading mode" break
.fi
.TP
\fBputs\fP \fIstring-to-end-of-line\fP
The rest of the line is output directly to the terminal without
interpretation.
.TP
\fBmacro\fP \fIM\fP
Invoke macro number \fIM\fP.  The maximum macro nesting level is five
(also catches macro loops).
.LP
I use the following macro to quickly save all the selected files in a
file whose name is entered as usual.  It also works in reading mode
(saving just the current article).
.nf
	\fBdefine\fP 1
		:unset save-report
		save-short input yes
		?menu '+'
		:set save-report
	\fBend\fP
.fi
.SH KEY MAPPINGS
The descriptions of the keys and commands provided in this manual
reflects the default key mappings in \fInn\fP.  However, you can
easily change these mappings to match your personal demands, and it is
also possible to remap keys depending on the terminal in use.
Permanent remapping of keys must be done through the
.I init
file, while temporary changes (for the duration of the current
invocation of \fInn\fP) can be made with the
.B :map
command.
.LP
The binding and mapping of keys are controlled by four tables:
.TP
.B The multikey definition table
This table is used for mapping multicharacter key sequences into
single characters.  By default the table contains the mappings for the
four cursor keys, and there is room for 10 user-defined multikeys.
The fourteen multikeys are named:
.BR up ,
.BR down ,
.BR right ,
.BR left
(the four arrow keys), and
.BR #0
through
.BR #9
for the user-defined keys.
.sp 0.5v
Multikey #\fIi\fP (where \fIi\fP is a digit or an arrow key name) is
defined using the following command:
.sp 0.5v
.nf
	\fBmap #\fP\fIi\fP \fIkey-sequence\fP
.fi
.sp 0.5v
where the
.I sequence
is a list of 7-bit character names (see below) separated by spaces.
For example, if the HOME key sends the sequence ESC [ H, you can
define multikey #0 to be the home key using the command:
.sp 0.5v
.nf
	map #0 ^[ [ H
.fi
.TP
.B The input key mapping table
All characters that are read from the keyboard will be mapped through
the input mapping table.  Consequently, you can globally remap one key
to produce any other key value.  By default all keys are mapped into
themselves.
.sp 0.5v
An entry in the input key mapping table to map \fIinput-key\fP into
\fInew-key\fP is made with the command
.sp 0.5v
.nf
	\fBmap key\fP \fIinput-key\fP \fInew-key\fP
.fi
.sp 0.5v
For example, to make your ESC key function as
.B interrupt
you can use the command
.sp 0.5v
.nf
	map key ^[ ^G
.fi
.TP
.B The selection mode key binding table
This table defines for each key which command should be invoked when
that key is pressed in selection mode, i.e. when the article menu is
shown.  The command to bind a
.I key
to a
.I command
in selection mode is:
.sp 0.5v
.nf
	\fBmap menu\fP \fIkey command\fP
.fi
.sp 0.5v
For example, to have the HOME key defined as multikey #0 above bound
to the
.B select
command, the following command is used:
.sp 0.5v
.nf
	map menu #0 select
.fi
.sp 0.5v
To remap a key to select a specific article on the menu (which the `a'
through `z' keys do by default), the \fIcommand\fP must be specified
as `\fBarticle\fP \fIN\fP' where \fIN\fP is the entry number on the
menu counted from zero (i.e. a=0, b=1, ..., z=25, 0=26, ..., 9=35).
For example, to map `J' to select article `j', the following
command is used:
.sp 0.5v
.nf
	map menu J article 9
.fi
.TP
.B The reading mode key binding table
This table defines for each key which command should be invoked when
that key is pressed in reading mode, i.e. when the article text is
shown.  The command to bind a
.I key
to a
.I command
in reading mode is:
.sp 0.5v
.nf
	\fBmap show\fP \fIkey command\fP
.fi
.LP
In addition to the direct mappings described above, the following
variations of the \fBmap\fP command are available:
.TP
.B User defined keymaps
Additional keymaps can be defined using the command
.sp 0.5v
.nf
	\fBmake map\fP \fInewmap\fP
.fi
.sp 0.5v
This will create a new keymap which can initialized using normal
\fBmap\fP commands, e.g.
.sp 0.5v
.nf
	\fBmap\fP \fInewmap key command\fP 
.fi
.sp 0.5v
To activate a user-defined keymap, it must be bound to a \fIprefix key\fP:
.sp 0.5v
.nf
	\fBmap\fP \fIbase-map prefix-key\fP \fBprefix\fP \fInewmap\fP
.fi
.sp 0.5v
When used, the prefix key itself does not activate a command, but
instead it require another key to be entered and then execute the
command bound to that key in the keymap which is bound to the prefix key.
  For example, to let the key sequence "^X i" execute macro number 10 in
both modes, the following commands can be used:
.sp 0.5v
.nf
	make map ctl-x
	map ctl-x i macro 10
	map both ^X prefix ctl-x
.fi
.TP
.B Mapping keys in both modes
Using the pseudo-keymap `both', it is possible to map a key to a
command in both selection and reading mode at once.  For example, to
map the home key to macro number 5 in both modes, the following
command can be used:
.sp 0.5v
.nf
	map both #0 macro 5
.fi
.TP
.B Aliasing
A key can also be mapped directly to the command currently bound to
another key.  Later remapping of the other key will not change the
mapping of the `aliased' key.  This is done using the following command:
.sp 0.5v
.nf
	map \fIkeymap new-key\fP \fBas\fP \fIold-key\fP
.fi
.TP
.B Binding macros to keys
A previously defined macro can be bound to a key using the command:
.sp 0.5v
.nf
	map \fIkeymap key\fP \fBmacro\fP \fImacro-number\fP
.fi
.TP
.B Implicit macro definitions
An implicit macro can also be defined directly in connection with the
\fBmap\fP command:
.sp 0.5v
.nf
	map \fIkeymap key\fP \fB(\fP
	\fIbody\fP...
	\fB)\fP
.fi
.LP
Keys and character names are specified using the following notation:
.TP
.I C
A single printable character represents the key or character itself.
.TP
\fB^\fP\fIC\fP
This notation represents a control key or character.
DEL is written as \fB^?\fP
.TP
\fI125\fP, \fI0175\fP, \fI0x7D\fP
Characters and keys can be specified by their ordinal value in
decimal, octal, and hexadecimal notation.
.TP
\fBup\fP, \fBdown\fP, \fBright\fP, \fBleft\fP
These names represent the cursor keys.
.TP
\fB#0\fP  through  \fB#9\fP
These symbols represent the ten user-defined multikeys.
.LP
If the variable \fBdata-bits\fP is 7, key maps can specify binding of
all keys in the range 0x00 to 0x7F, and the 8th bit will be stripped
in all keyboard input.
If the variable \fBdata-bits\fP is 8, the 8th bit is not cleared, and
key maps are extended to allow binding of keys in the range 0xA0 to
0xFE (corresponding to the national characters defined by the ISO 8859
character sets).
Binding commands to these keys can be done either by using their
numeric value, or directly specifying the 8 bit character in the map
command, e.g.
.sp 0.5v
.nf
	map menu 0xC8 macro 72
	map key \o'\(aae' %
.fi
.LP
To show the current contents of the four tables, the following
versions of the \fB:map\fP command are available:
.TP
.B :map
Show the current mode's key bindings.
.TP
.B :map menu
Show the selection mode key bindings.
.TP
.B :map show
Show the reading mode key bindings.
.TP
.B :map #
Show the multikey definition table.
.TP
.B :map key
Show the input key mapping table.
.SH STANDARD KEY BINDINGS
Below is a list of all the commands that can be bound to keys, either
in selection mode, in reading mode, or both.  For each command the
default command key bindings in both modes are shown.
If the key is not bound in one of the modes, but it can be bound, the
corresponding part will just be empty.  If the command cannot be bound
in one of the modes, that mode will contain the word \fBnix\fP.
.LP
.in +8n
.ta \w'continue-no-mark'u+5m +\w'Selection_mode'u+3m
.\"ta 4 26 42
.br
\fIFunction	Selection mode	Reading mode\fP
.br
\fBadvance-article\fP	\fBnix\fP 	a
.br
\fBadvance-group\fP	A 	A
.br
\fBarticle\fP \fIN\fP	a-z0-9 	\fBnix\fP
.br
\fBback-article\fP	\fBnix\fP 	b
.br
\fBback-group\fP	B 	B
.br
\fBcancel\fP	C 	C
.br
\fBcommand\fP	:  	:
.br
\fBcompress\fP	\fBnix\fP 	c
.br
\fBcontinue\fP	\fBspace\fP 	\fBspace\fP
.br
\fBcontinue-no-mark\fP	\fBreturn\fP 	\fBnix\fP
.br
\fBdecode\fP
.br
\fBfind\fP	= 	/
.br
\fBfind-next\fP	\fBnix\fP 	.
.br
\fBfollow\fP	F 	fF
.br
\fBfull-digest\fP	\fBnix\fP 	H
.br
\fBgoto-group\fP	G 	G
.br
\fBgoto-menu\fP	\fBnix\fP 	= Z
.br
\fBhelp\fP	? 	?
.br
\fBjunk-articles\fP	J 	\fBnix\fP
.br
\fBkill-select\fP	K 	K
.br
\fBlayout\fP	" 	\fBnix\fP
.br
\fBleave-article\fP	\fBnix\fP 	l
.br
\fBleave-next\fP	L 	L
.br
\fBline+1\fP	,  \fBdown\fP 	\fBreturn\fP
.br
\fBline-1\fP	/ 	\fBnix\fP
.br
\fBline=@\fP	\fBnix\fP 	g
.br
\fBmacro\fP \fIM\fP
.br
\fBmail\fP	M 	m M
.br
\fBmessage\fP	^P 	^P
.br
\fBnext-article\fP	\fBnix\fP 	n
.br
\fBnext-group\fP	N 	N
.br
\fBnext-subject\fP	\fBnix\fP 	k
.br
\fBnil\fP
.br
\fBoverview\fP	Y 	Y
.br
\fBpage+1\fP	> 	\fBnix\fP
.br
\fBpage+1/2\fP	\fBnix\fP 	d
.br
\fBpage-1\fP	< 	\fBdelete  backspace\fP
.br
\fBpage-1/2\fP	\fBnix\fP 	u
.br
\fBpage=0\fP	\fBnix\fP 	h
.br
\fBpage=1\fP	^ 	^
.br
\fBpage=$\fP	$ 	$
.br
\fBpatch\fP
.br
\fBpost\fP
.br
\fBpreview\fP	% 	%
.br
\fBprevious\fP	P 	p
.br
\fBprint\fP		P
.br
\fBquit\fP	Q 	Q
.br
\fBread-return\fP	Z 	\fBnix\fP
.br
\fBread-skip\fP	X 	X
.br
\fBredraw\fP	^L ^R 	^L ^R
.br
\fBreply\fP	R 	r R
.br
\fBrot13\fP	\fBnix\fP 	D
.br
\fBsave-body\fP	W 	w W
.br
\fBsave-full\fP	S 	s S
.br
\fBsave-short\fP	O 	o O
.br
\fBselect\fP	. 	\fBnix\fP
.br
\fBselect-auto\fP	+ 	\fBnix\fP
.br
\fBselect-invert\fP	@ 	\fBnix\fP
.br
\fBselect-range\fP	- 	\fBnix\fP
.br
\fBselect-subject\fP	* 	*
.br
\fBshell\fP	! 	!
.br
\fBskip-lines\fP	\fBnix\fP 	\fBtab\fP
.br
\fBunselect-all\fP	~ 	\fBnix\fP
.br
\fBunshar\fP
.br
\fBunsub\fP	U 	U
.br
\fBversion\fP	V 	V
.in -8n
.DT
.LP
See the descriptions of the default bindings for a description of the
commands.  The pseudo command
.B nil
is used to
.I unbind
a key.
.SH THE INIT FILES
The
.I init
files are used to customize \fInn\fP's behaviour to local conventions
and restrictions and to satisfy each user's personal taste.
.br
Normally, \fInn\fP reads upto three init files on start-up if they
exist (all init files are optional):
.TP
$LIB/\fBsetup\fP
A system-wide file located in the library directory.  This file is
\fIalways\fP loaded before any other init file (even when the
\-\fBI\fP option is specified).  It cannot contain a group
presentation sequence.
.TP
$LIB/\fBinit\fP
Another system-wide (global) init file located in the library
directory.  This file may be ignored via the \-\fBI\fP option.
.TP
~/.nn/\fBinit\fP
The private init file located in the user's \&\fI.nn\fP directory.
It is read after the global init file to allow the user to change the
default setup.
.LP
The init file is parsed one line at a time.  If a line ends with a
backslash `\e', the backslash is ignored, and the following line is
appended to the current line.
.LP
The init file may contain the following types of commands (and data):
.TP
.B Comments
Empty lines and lines with a # character as the first non-blank
character are ignored.  Except where # has another meaning defined by
the command syntax (e.g. multi-keys are named #\fIn\fP), trailing
comments on input lines are ignored.
.TP
.B Variable settings
You can
.B set
(or
.BR unset )
all the variables described earlier to change
nn's behaviour permanently.  The
.B set
and
.B unset
commands you can use in the init file have exactly the same format as
the
.B :set
and
.B :unset
commands described earlier (except that the : prefix is omitted.)
.sp 0.5v
Variables can also be locked via the \fBlock\fP command; this is
typically done in the \fIsetup\fP file to enforce local policies.
.TP
.B Key mappings
You can use all the versions of the
.B map
command in the init file.
.TP
.B Macro Definitions
You can define sequences of commands and key strokes using the
\fBdefine\fP...\fBend\fP construction,
which can then be
bound to single keys with the
.B map
command.
.TP
.B Load terminal specific files
You can load a terminal specific file using the
.sp 0.5v
.nf
	\fBload\fP \fIfile\fP
.fi
.sp 0.5v
The character
.B @
in the
.I file
will be replaced by the terminal type defined in the TERM environment
variable.  \fInn\fP silently ignores the
.B load
command if the file does not exist (so you don't have to have a
specific init file for terminals which does not require remapping).
If the file is not specified by an absolute pathname, it must reside
in your ~/.nn directory.  Examples:
.nf
	# load local customizations
	load /usr/lib/nninit
	# load personal terminal specific customizations
	load init.@
.fi
.TP
.B Switch to loading a different init file
You can skip the rest of the current init file and start loading a
different init file with the following command:
.sp 0.5v
.nf
	\fBchain\fP \fIfile\fP
.fi
.sp 0.5v
If this occur in the private or global init file, the chained init
file may contain a sequence part which will replace the private or
global presentation sequence respectively.
.TP
.B Stop loading current init file
You can skip the rest of the current init file with the following
command:
.sp 0.5v
.nf
	\fBstop\fP
.fi
.TP
.B Give error messages and/or terminate
If an error is detected in the init file, the following commands can be
used to print an error message and/or terminate execution:
.sp 0.5v
.nf
\fBerror\fP \fIfatal error message\fP...
.fi
	Print the message and terminate execution.
.sp 0.5v
.nf
\fBecho\fP \fIwarning message\fP...
.fi
	Print the message and continue.
.sp 0.5v
.nf
\fBexit\fP [ \fIstatus\fP ]
.fi
	Terminate \fInn\fP with the specified exit status or 0 if omitted.
.TP
.B Change working directory of nn
You can use the
.B cd
command to change the working directory whenever you enter \fInn\fP.
Example:
.nf
	# Use folder directory as working directory inside \fInn\fP
	cd ~/News
.fi
.TP
.B Command groups
The init file can contain groups of commands which are executed under
special conditions.  The command groups are described in the section
on command groups below.
.TP
.B One or more save-files sections
A \fBsave-files\fP section is used to assign default save files to
specific groups:
.sp 0.5v
.nf
	\fBsave-files\fP
	  \fIgroup-name (pattern)\fP \fIfile-name\fP
	  ...
	\fBend\fP
.fi
.sp 0.5v
The group name (patterns) and save file names are specified in the
same way as in the presentation sequence (see below).  Example:
.nf
	\fBsave-files\fP
	  news*  +news/$L
	  comp.sources*  /u/src/$L/
	\fBend\fP
.fi
.TP
.B The news group presentation sequence
The
.I last
part of the init file may specify the sequence in which you want the
news groups to be presented.  This part starts with the command
.B sequence
and continues to the end of the init file.
.LP
Both init files may contain a presentation sequence.  In this case,
the global sequence is \fIappended\fP to the private sequence.
.SH COMMAND GROUPS
Command groups may only occur in the init file, and they provide a way
to have series of commands executed at certain points during news reading.
.LP
In release 6.4 onwards, these possibilities are still rather
rudimentary, and a mixture of normal init file syntax and macro syntax
is used depending on whether the command group is only executed on
start-up or several times during the \fInn\fP session.
.LP
A command group begins with the word \fBon\fP and
ends with the word \fBend\fP.  The following command groups are
conditionally executed during the parsing of the init file if the
specified \fIcondition\fP is true.  They may also have an optional
\fBelse\fP part which is executed if the \fIcondition\fP is false:
.sp 0.5v
.nf
	\fBon\fP \fIcondition\fP
		commands
	[ \fBelse\fP
		commands ]
	\fBend\fP
.fi
.LP
The following conditional command groups may be used in the init file
to be executed at start-up:
.TP
\fBon [\fP \fItest\fP \fB]\fP
The commands (init file syntax) in the group are executed only if the
specified \fItest\fP is true.
A shell is spawned to execute the command "[ \fItest\fP ]", so all the
options of the \fBtest\fP(1) command is available.  For example, to
unset the flow-control variable if the tty is a pseudo-tty, the
following conditional can be used:
.nf
	on [ -n "`tty | grep ttyp`" ]
		unset flow-control
	end
.fi
.TP
\fBon !\fP\fIshell command\fP
The command group is executed if the given \fIshell command\fP exits
with 0 status (success).
Care should be taken that the command does not produce any
output, e.g. by redirecting its output to /dev/null.  For example, to
prevent people from reading news if load is above a specific level,
the following conditional might be placed in the global setup file.
.nf
	on !load-above 5
		error load is too high, try again later.
	end
.fi
.TP
\fBon `\fP\fIshell command\fP\|\fB`\fP \fIstring\fP...
The command group is executed if the \fIfirst output line\fP from
executing the specified \fIshell command\fP is listed among the
specified \fIstring\fP values.  The \fIshell command\fP can be omitted
on subsequent occurrences of this conditional, in which case
the output from the last \fBshell command\fP is used.
For example, the following conditional
can be used to switch to an init file which has a limited sequence for
news reading during working hours, evenings, and nights:
.nf
	on `date +%H` 9 10 11 12 13 14 15 16
		chain init.work
	end
	on `` 17 18 19 20 21
		chain init.evening
	else
		chain init.night
	end
.fi
.TP
\fBon ``\fP \fIstring\fP...
This is equivalent to the previous form except that instead of
executing a shell command, the output from the previous 
.TP
\fBon $\fP\fIvariable\fP [ \fIvalue\fP ]
If no \fIvalue\fP strings are specified, the command group is executed
if the given \fIvariable\fP is defined in the environment.  Otherwise,
the command group is executed only if the value of the \fIvariable\fP
occur in the \fIvalue\fP list.  For example, if you want \fInn\fP to
look for mail in whatever $MAIL is set to - if it is set - you can use
the following code:
.nf
	on $MAIL
		set mail $(MAIL)
	end
.fi
.TP
\fBon slow\fP
.br
The commands (init file syntax) in the group are executed only if the
current terminal output speed is less than or equal to the baud rate
set in the \fBslow-speed\fP variable.  This can be used to optimize
the user-interface for slow terminals by setting suitable variables:
.sp 0.5v
.nf
	\fBon slow\fP
		set confirm-entry
		set slow-mode
		set delay-redraw
		unset visible-bell
		set compress
		unset header-lines
		set stop 5
		set window 10
	\fBend\fP
.fi
.TP
\fBon fast\fP
.br
Same as \fBon slow\fP except that the commands are only executed when
the terminal is running at a speed above the \fBslow-speed\fP value.
.TP
\fBon term\fP \fIterm-type\fP...
.br
The commands are executed if one of the \fIterm-type\fP names is
identical to value of the TERM environment variable.
.TP
\fBon host\fP \fIhost-name\fP...
.br
The commands are executed if the local host's name occur in the
\fIhost-name\fP list.
.TP
\fBon program\fP \fIprogram-name\fP...
.br
The commands are executed if the current program (\fInn\fP,
\fInncheck\fP, etc) in the \fIprogram-name\fP list.
.LP
The following \fBon\fP command groups are really macros which may be
executed during \fInn\fP's normal processing, and as such they cannot
have an \fBelse\fP part.
.TP
\fBon entry\fP [ \fIgroup list\fP ]
.br
These commands (macro format!) are executed every time \fInn\fP enters a
news group.  If a group list is not specified, the commands are
associated with all groups which don't have its own entry macro
specified in the group sequence.  Otherwise, the entry macro will be
associated with the groups in the list.  The group list is specified
using the meta-notations described in the presentation sequence section.
.sp 0.5v
\fIAll\fP `:' commands at the beginning of the
command group are executed \fIbefore\fP \fInn\fP collects the articles
in the group, so it is possible to set or unset variables like
\fBcross-post\fP and \fBauto-read-mode\fP before any articles are
collected and the menu is (not) shown.
  The non-`:' commands, and `:' commands that follows a command of
another type will be executed immediately \fIafter\fP the first menu
page is presented.  The execution of a `:' command can be postponed by
using a double `::' as the command prefix.
.sp 0.5v
.nf
	\fBon entry\fP comp.sources* alt.sources
		:set cross-post on   # set before collection
		:local auto-read-mode on   # set before showing menu
		::unset cross-post   # set after collection
	\fBend\fP
.fi
.TP
\fBon start-up\fP
.br
These `:' commands (macro format!) are executed on start-up just
before \fInn\fP enters the first news group.  However, postponed
commands (i.e. non-`:' commands) will not be executed until the first
group is shown (it works like an entry macro).
.SH GROUP PRESENTATION SEQUENCE
News groups are normally presented in the sequence defined in the
system-wide
.B init
file in \fInn\fP's library directory.
.LP
You can personalize the presentation sequence
by specifying an alternative sequence in the private
.I init
file.
The sequence in the private init file is used
.I before
the global presentation sequence, and need only
describe the deviations from the default presentation sequence.
.LP
The presentation sequence must start with the word
.nf
	\fBsequence\fP
.fi
followed by a list of the news group names in the order you want them
to be presented.
The group names must be separated by white space.
The sequence list must be the \fIlast\fP part of the
init file (the parsing of commands from the init file stops when the
word \fBsequence\fP is encountered).
.LP
You may use a full group name like "comp.unix.questions", or just the
name of a main group or subgroup, e.g. "comp" or "comp.unix".
However, if "comp" precedes "comp.unix.questions" in the list, this
subgroup will be placed in the normal alphabetic sequence during the
collection of all the "comp" groups.
.LP
Groups which are not explicitly mentioned in any of the sequence files
will be placed after the mentioned groups, unless `!!' is used and it
has not been disabled (as described below).
.LP
Each group name may be followed by a file or folder name (must start
with either of `/' `~' or `+') which will specify the default save file
for that group (and its subgroups).  A single `+' following the group
name is an abbreviation for the last save file name used.
For example, the following two sequences are equivalent:
.nf
	group1 +file group2 +file group3 +file
	group1 +file group2 + group3 +
.fi
.LP
When an article is saved, the default save name will be used as the
initial contents of the file name prompt for further editing.  It
therefore does not need to be be a complete file name (unless you use
the quick save mode).
.LP
Each group name may also be associated
with a so-called \fBentry action\fP.  This is basically an (unnamed)
macro which is invoked on entry to the group (following the same rules
as the `on entry' command group related to :set and :unset commands).
.LP
The entry action begins with a left parenthesis `\fB(\fP' and ends
with a right parenthesis `\fB)\fP' on an otherwise empty line:
.sp 0.5v
.nf
	comp.sources. +src/$L/ (
		:set cross-post
	)
.fi
.sp 0.5v
The last entry action can be repeated by specifying an empty set of
parenthesis, e.g.
.sp 0.5v
.nf
	comp.unix. +unix ()
.fi
.sp 0.5v
The entry action of a preceding group in the sequence can be
associated with the current group(s) by specifying the name of the
group in the parentheses instead of the commands, e.g.
.sp 0.5v
.nf
	comp.unix. +unix (comp.sources.unix)
.fi
.sp 0.5v
A macro can also be associated with the entry action by specifying its
number in the same way as the group name above, e.g.
.sp 0.5v
.nf
	rec.music. +music (30)
.fi
.sp 0.5v
Notice that it is the
\fIcurrent\fP definition of the macro which is associated with the
group, so if the macro is later redefined with the `:define' command,
it will not have any effect on the entry action.
.LP
Group names can be specified using the following notations:
.TP
group.name
Append the group (if it exists) to the presentation sequence list.  If
\fBalso-subgroups\fP is set (default), all subscribed subgroups of the
group will be included as well (if there are any).  Examples: "comp",
"comp.unix", "comp.unix.questions".  If the group does not exits (e.g.
"comp"), the subgroups will be included even when \fBalso-subgroups\fP
is not set, i.e. "comp" is equivalent to "comp.".
.TP
group.name.
Append the subgroups of the specified group to the presentation
sequence.  The group itself (if it exists) is not included.
Examples: "comp.", "comp.unix.".
.TP
.group.name
Append the groups whose name ends with the specified name to the
sequence.  Example: ".test".
.TP
group.name*
Append the group and its subgroups to the presentation sequence list
(even when \fBalso-subgroups\fP is not set).  Example: "comp.unix*".
.LP
The following meta notation can be used in a sequence file.  The
group.name can be specified using any of the forms described above:
.TP
\&! groups
Completely ignore the group or groups specified
unless they are already in the presentation sequence (i.e. has been
explicitly mentioned earlier in the sequence).
.TP
\&!:\fIcode\fP groups
Ignore a selection of groups based on the given \fIcode\fP letter (see
below), unless they are already included in the sequence.  Notice that
these forms \fIonly\fP excludes groups from the
presentation sequence, i.e. they \fIdo not\fP include the remaining
groups at this point; that must be done explicitly elsewhere.
.TP
\&!:U groups
Ignore unsubscribed groups, i.e. if they are neither new, nor present
and subscribed in \&.newsrc.
This is useful to ignore a whole hierarchy except for a
few groups which are explicitly mentioned in \&.newsrc and still see
new groups as they are created.
.TP
\&!:X groups
Ignore unsubscribed \fIand\fP new groups, i.e. if they are not
currently present and subscribed in \&.newsrc.
This is useful to ignore a whole hierarchy except for a
few groups which are explicitly mentioned in \&.newsrc.  New groups in
the hierarchy are ignored unless `NEW' occurs earlier in the sequence.
.TP
\&!:O groups
Ignore old groups, i.e. \fIunless\fP they are new.  This is useful to
ignore a whole hierarchy but still see new groups which are created in
the hierarchy (it might become interesting some day).  Individual
groups can still be included in the sequence if they are specified
before the `!:O' entry.
.TP
\&!:N groups
Ignore new groups in the hierarchy.
.TP
\&!!
Stop building the presentation sequence.  This eliminates all groups
that are not already in the presentation sequence.
.TP
\fBNEW\fP
This is a pseudo group name which matches all \fInew\fP groups; you
could place this symbol early in your presentation sequence to see new
groups `out of sequence' (to attract your attention to them).
.TP
\fBRC\fP
This is a pseudo group name which matches all groups occurring in the
\&.newsrc file.  It will cause the groups in .newsrc to be appended to
the presentation sequence in the sequence in which they are listed in
\&.newsrc.
.TP
\fBRC:\fP\fInumber\fP
Similar to the \fBRC\fP entry, but limited to the first \fInumber\fP
lines of the .newsrc file.  Example: RC:10 (use 10 lines of .newsrc).
.TP
\fBRC:\fP\fIstring\fP
Similar to the \fBRC\fP entry, but limited to the lines up to (and
including) the first line (i.e. group) starting with the given
\fIstring\fP.  For example:  RC:alt.sources
.TP
\&< group.name
Place the group (and its subgroups) at the beginning of the
presentation sequence.  Notice that each `<' entry will place the
group(s) at the beginning of the current sequence, i.e. < A < B < C
will generate the sequence C B A.
.TP
\&> group.name
Place the group (and its subgroups) after all other groups that are
and will be entered into the presentation sequence.
.TP
\&@
Disable the `!!' command.  This can be included in the personal
presentation sequence if the global
.B sequence
file contains a !! entry (see example 1 below).
.TP
\&% .... %
Starts and ends a region of the sequence where it is possible to
include groups which has been eliminated earlier.  This may be useful
to alter the sequence of some groups, e.g. to place comp.sources.bugs
after all other source groups, the following sequence can be used:
.sp 0.5v
! comp.sources.bugs comp.sources* % comp.sources.bugs %
.LP
.sp 0.5v
.B Example 1:
In a company where ordinary users only should read the local
news groups, and ignore the rest (including new news groups which are
otherwise always subscribed to initially), can use the following
global presentation sequence:
.sp 0.5v
.nf
	general
	follow
	! local.test
	local
	!!
.fi
.sp 0.5v
The "expert" users in the company must put the
.B @
command somewhere
in their private sequence to avoid losing news groups which they have
not explicitly mentioned in their init file.
.sp
.B Example 2:
This is the global sequence for systems with
heavy news addicts who setup their own sequences anyway.
.sp 0.5v
.nf
	# all must read the general news first
	< general
.sp 0.5v
	# test is test, and junk is junk,
	# so it is placed at the very end
	> test
	> .test
	> junk
.sp 0.5v
	# this is the standard sequence which everybody may
	# change to their own liking
	local	# our local groups
	dk	# the Danish groups
	eunet.general # to present it before eunet.followup
	eunet	# the other European groups
	comp	# the serious groups
	news	# news on news
	sci	# other serious groups
	rec	# not really that important (don't quote me)
	misc	# well, it must be somewhere
.sp 0.5v
	# the groups that are not listed above goes here
.fi
.sp 0.5v
Notice the use of comments in the sequence where they are allowed at
the end of non-empty lines as well.
.sp
.B Example 3:
My own presentation sequence (in the init file) simply
lists my favourite groups and the corresponding default save files:
.sp 0.5v
.nf
   \fBsequence\fP
	!:U alt*  # ignore unsubscribed alt groups
	news.software.nn +nn
	comp.sys.ti* +ti/$L
	NEW  # show new groups here
	news*
	rec.music.synth +synth/
	comp.emacs*,gnu.emacs +emacs/misc
	comp.risks +risks
	eunet.sources +src/unix/
	comp.sources* +src/$L/
.fi
.sp 0.5v
The presentation sequence is not used when \fInn\fP is called with one or
more news group names on the command line; it is thus possible to read
ignored groups (on explicit request) wihtout changing the init file.
(Of course, you can also use the
.B G
command to read ignored groups).
.SH MERGING NEWS GROUPS
The third example above contains the following line:
.sp 0.5v
.nf
	comp.emacs*,gnu.emacs +emacs/misc
.fi
.sp 0.5v
This is the syntax used to \fImerge\fP groups.  When two or more
groups are merged, all new articles in these groups are presented
together as if they were one group.  To merge groups, their names must
be listed together in the sequence, and only separated by a single
comma.  To merge the groups resulting from a single group pattern
(e.g. comp.emacs*), the group pattern must be followed by a comma and
a blank (e.g. comp.emacs*, ...).
.LP
Merged groups are presented as the first group in the "list", and the
word "MERGED" will be shown after the group name.  The \fBY\fP
{\fBoverview\fP} command will still show merged groups as individual
groups, but they will be annotated with the symbol `&' on the first of
the groups, and a `+' on the rest of the groups.
.LP
In the current version, the concept of the \fIcurrent group\fP in
connection with merged groups is a bit fuzzy.  This should only be
noticeable with the \fBG\fP command, which will take the most recently
used group among the merged groups as the current group.  So things
like \fBG = ...\fP may not always work as expected.
.SH ENVIRONMENT
The following environment variables are used by \fInn\fP:
.LP
.BR EDITOR .
The editor invoked when editing replies, follow-ups, and composing
mail.  \fInn\fP knows about the following editors:
\fIvi\fP, \fIded\fP, \fIGNU emacs\fP, and \fImicro-emacs\fP,
and will try to position the cursor on the first line following the
header, i.e. after the blank line which must not be deleted!  If an
article has been included, the cursor is placed on the first line of
the included text (to allow you to delete sections easily).
.LP
.BR LOGNAME .
This is taken as the login name of the current user.  It is used by
\fInn\fP to return failed mail.  If it is not defined, \fInn\fP will
use the value of USER, or if that is not defined either, \fInn\fP will
use the call `who am i' to get this information.  If all attempts
fail, the failed mail is dropped in the bit bucket.
.LP
.BR PAGER .
This is used as the initial value of the \fBpager\fP variable.
.LP
.BR SHELL .
This is the shell which is spawned if the system cannot suspend
\fInn\fP, and it will be used to execute the shell escapes.
.LP
.BR TERM .
The terminal type.
.SH FILES
.DT
.nr tW \w'~/.nn/KILL.COMP'
.nr tX \w'/usr/lib/nntp-server'
.if \n(tWu>\n(tXu .nr tX \n(tWu
.ta \n(tWu+3m
.\"ta 0 22
~/.newsrc	The record of read articles.
.br
~/.nn/select	The record of selected and seen articles.
.br
~/.nn/init	Personal configuration and presentation sequence.
.br
~/.nn/kill	The automatic kills and selections.
.br
~/.nn/KILL.COMP	The compiled kill file.
.br
~/.nn/LAST	The time stamp of the last news group we have seen.
.br
~/.nn/NEXTG	Active group last time \fInn\fP was quit.
.br
~/.nn/.param	Parameter file for the aux script
.br
$lib/setup	System-wide setup - always read first.
.br
$lib/init	System-wide setup and presentation sequence.
.br
$lib/aux	The response edit and send script.
.br
$lib/routes	Mapping rules for mail addresses (on non-domain systems).
.br
$db/*	The news data base.
.br
/etc/termcap	Terminal data base [BSD].
.br
/usr/lib/terminfo/*	Terminal data base [SysV].
.br
/usr/lib/nntp-server	Name of remote nntp server.
.sp 0.5v
.DT
The name $lib and $db are the directories used for the auxiliary files
and the news data base respectively.  Their name and location is
defined at compile time.  Common choices are /usr/local/lib/nn or
/usr/lib/news/nn for $lib and /usr/spool/nn or /usr/spool/news/.nn for
$db.
.SH SEE ALSO
Other netnews documentation.
.br
RFC 1341, MIME (Multipurpose Internet Mail Extensions)
.br
nncheck(1), nngoback(1), nngrab(1), nngrep(1), nnpost(1), nntidy(1)
.br
nnadmin(1M), nnusage(1M), nnmaster(8), nnspew(8)
.SH AUTHOR
Kim F. Storm, Texas Instruments A/S, Denmark
.br
E-mail: storm@texas.dk  (but see the addresses below)
.LP
The NNTP support was designed and implemented by Ren\o'\(aae' Seindal,
Institute of Datalogy, University of Copenhagen, Denmark.
.LP
Bugs and fixes, suggestions, ideas, critique, etc. can be sent to
the following address:
.nf
	nn-bugs@dkuug.dk
.fi
.LP
The news.software.nn group is used for discussion on all subjects
related to the nn news reader.  This includes, but is not limited to,
questions, answers, ideas, hints, information from the development
group, patches, etc.
.\" ENDPART D
