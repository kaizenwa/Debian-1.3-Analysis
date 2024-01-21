.\" BEGINPART C
.SH CATCH UP
If you have not read news for some time, there are probably more news
than you can cope with.  Using the option \-\fBa0\fP \fInn\fP will put
you into \fBcatch-up mode\fP.
.LP
The first question you will get is whether to catch up interactively
or automatically.  If you instruct \fInn\fP to catch up automatically,
it will simply mark all articles in all groups as read, thus bringing
you \fIcompletely up-to-date\fP.
.LP
If you choose the interactive mode, \fInn\fP will locate all groups
with unread articles, and for each group it will prompt you for an
action to take on the group.  An action is selected using a single
letter followed by \fBreturn\fP.  The following actions are available:
.TP
.B y
Mark all articles as read in current group.
.TP
.B n
Do not update group (this is the default action if you just hit
\fBreturn\fP).
.TP
.B r
Enter reading mode to read the group.
.TP
.B U
Unsubscribe to the group.
.TP
.B ?
Give a list of actions.
.TP
.B q
Quit.
When you quit, \fInn\fP will ask whether the
rest of the groups should be updated unconditionally or whether they
should remain unread.
.SH VARIABLES AND OPTIONS
It is possible to control the behaviour of \fInn\fP through the
setting (and unsetting) of the variables described below.  There are
several ways of setting variables:
.br
\- Through command line options when \fInn\fP is invoked.
.br
\- Through \fIassignments\fP on the command line when \fInn\fP is invoked.
.br
\- Through global \fBset\fP commands in the init file.
.br
\- Through \fBset\fP or \fBlocal\fP commands executed from entry macros.
.br
\- Through the \fB:set\fP extended command when you run \fInn\fP.
.LP
There are four types of variables:
.br
\- Boolean variables
.br
\- Integer variables
.br
\- String variables
.br
\- Key variables
.LP
Boolean variables control a specific function in \fInn\fP, e.g.
whether the current time is shown in the prompt line.  A boolean
variable is set to
.B true
with the command
.nf
	\fBset\fP \fIvariable\fP
.fi
and it is set to
.B false
with either of the following (equivalent) commands:
.nf
	\fBunset\fP \fIvariable\fP
	\fBset no\fP\fIvariable\fP
.fi
.LP
You can also toggle the value of a boolean variable using the command:
.nf
	\fBtoggle\fP \fIvariable\fP
.fi
.LP
For example:
.nf
	\fBset\fP time
	\fBunset\fP time
	\fBset\fP notime
	\fBtoggle\fP time
.fi
.LP
Integer variables control an amount e.g. the size of the preview
window, or the maximum number of articles to read in each group.  They
are set with the following command:
.nf
	\fBset\fP \fIvariable value\fP
.fi
In some cases, not setting an integer value has a special meaning,
for example, not having a minimal preview window or reading all
articles in the groups no matter how many there are.  The special
meaning can be re-established by the following command:
.nf
	\fBunset\fP \fIvariable\fP
.fi
For example:
.nf
	\fBset\fP window 7
	\fBunset\fP limit
.fi
.LP
String variables may specify directory names, default values for
prompts, etc.  They are set using the command
.nf
	\fBset\fP \fIvariable string\fP
.fi
Normally, the \fIstring\fP value assigned to the \fIvariable\fP
value starts at the first non-blank character after the variable name
and ends with the last non-blank character (excluding comments) on the
line.  To include leading or trailing blanks, or the comment start
symbol, #, in the string they must be escaped using a backslash `\e',
e.g. to set \fBincluded-mark\fP to the string " # ", the following
assignment can be used:
.sp 0.5v
.nf
	set included-mark  \e\ \e#\e\ \ \ # blank-#-blank
.fi
.sp 0.5v
To include a backslash in the string, it must be duplicated `\e\e'.
A backslash may also be used to include the following special
characters in the string: \ea=alarm, \eb=backspace, \ee=escape,
\ef=form-feed, \en=new-line, \er=return, \et=tab.
.LP
Key variables control the keys used to control special functions
during user input such as line editing and completion.  They are set
using the command
.nf
	\fBset\fP \fIvariable key-name\fP
.fi
.LP
A variable can be \fIlocked\fP which makes further modification of the
variable impossible:
.nf
	\fBlock\fP \fIvariable\fP
.fi
This can be used in the \fIsetup\fP init file which is loaded
unconditionally to enforce local conventions or restrictions.  For
example, to fix the \fBincluded-mark\fP variable to the string ">",
the following commands can be placed in the setup file:
.nf
	\fBset\fP included-mark >
	\fBlock\fP included-mark
.fi
.LP
The current variable settings can be shown with the
.B :set
command:
.TP
\fB:set\fP (without arguments)
This will give a listing of the variables which have been set in
either the init file or interactively.
.TP
\fB:set all\fP
This will give a listing of all variables.  Modified variables will be
marked with a `*' and \fIlocal\fP variables will be marked with a `>'.
A locked variable is marked with a `!'.
.TP
\fB:set /\fP\fIregexp\fP
This will give a listing of all variables whose name matches the given
regular expression.
.TP
\fB:set\fP \fIpartial-name\fP \fBspace\fP
The \fBspace\fP (\fBcomp1-key\fP) key will complete the variable name
as usual, but as a side effect it will display the variable's current
value in the message line.
.LP
Variables are global by default, but a local instantiation of the
variable can be created using the \fB:local\fP command.  The local
variable will overlay the global variable as long as the current group
is active, i.e. the global variable will be used again when you exit
the current group.  The initial value of the local variable will be
the same as the global variable, unless a new value is specified in
the \fB:local\fP command:
.sp 0.5v
.nf
	\fB:local\fP \fIvariable\fP [ \fIvalue\fP ]
.fi
.sp 0.5v
.LP
The following variables are available:
.TP
\fBalso-full-digest\fP	(boolean, default false)
When a digest is split, the digest itself is not normally included on
the menu, and as such the initial adminstrative information is not
available.  Setting \fBalso-full-digest\fP will cause the (unsplit)
digest to be included on the menu.  These articles are marked with a @
at the beginning of the subject.
.TP
\fBalso-subgroups\fP	(boolean, default true)
When set, a group name in the presentation sequence will also cause
all the subgroups of the group to be included, for example, comp.unix
will also include comp.unix.questions, etc.  When \fBalso-subgroups\fP
is not set, subgroups are only included if the group name is followed
by a `.' in which case the main group is \fInot\fP included, i.e.
`comp.unix' is not included when `comp.unix.' is specified in the
presentation sequence, and vice-versa.  Following a group name by an
asterisk `*', e.g. comp.unix*, will include the group as well as all
subgroups independently of the setting of \fBalso-subgroups\fP.
.TP
\fBappend-signature-mail\fP	(boolean, default false)
When false, it is assumed that the .signature file is automatically
appended to responses sent via E-mail.  If true, .signature will be
appended to the letter (see query-signature).
.TP
\fBappend-signature-post\fP	(boolean, default false)
When false, it is assumed that the .signature file is automatically
appended to posted articles.  If true, .signature will explicitly be
appended to posted articles (see query-signature).
.TP
\fBattributes\fP \fIsymbols\fP	(string, default ....)
Each element in this string represents a symbol used to represent an
article attribute when displayed on the screen.  See the section on
Marking Articles and Attributes.
.TP
\fBauto-junk-seen\fI	(boolean, default true)
When set, articles which have the \fIseen attribute\fP (,) will be marked
read when the current group is left.  If not set, these articles will
still be either unread or marked seen the next time the group is
entered (see also \fBconfirm-junk-seen\fP and \fBretain-seen-status\fI).
.TP
\fBauto-preview-mode\fP		(boolean, default false)
Enables \fIAuto Preview Mode\fP.  In this mode, selecting an article
on the menu using its article id (letter a-z) will enter preview mode
on that article immediately.  Furthermore, the `n' {\fBnext-article\fP}
command will preview the next article on the menu only if it has the
same subject as the current article; otherwise, it will return to the
menu with the cursor placed on the next article.  The \fBcontinue\fP
command at the end of the article and the `=' {\fBgoto-menu\fP}
returns to the menu immediately as usual.
.TP
\fBauto-read-mode-limit\fP \fIN\fP	(integer, default 0)
When operating in \fIauto reading mode\fP, \fInn\fP will
\fIauto-select\fP all unread articles in the group, skip the
article selection phase, and enter reading mode directly after
entry to the group.
  Auto reading mode is disabled when \fBauto-read-mode-limit\fP is
zero; it is activated unconditionally if the value is negative, and
conditionally if the value is greater than zero and the number of
unread articles in the current group does not exceed the given value.
.TP
\fBauto-select-closed\fP \fImode\fP	(integer, default 1)
Normally, selecting a \fIclosed subject\fP (usually in consolidated
menu mode) will select (or deselect) all \fIunread\fP articles with
the given subject (or all articles if they are all read).  This
behaviour can be changed via the value of this variable as follows:
.nf
0: select only the first article with the subject (shown on menu).
1: select only the unread articles with the subject.
2: select all available articles with the subject.
.fi
.TP
\fBauto-select-rw\fP	(boolean, default false)
If set, a subject of an article read or posted is automatically 
used for subsequent auto-selecting (if not already selected).
This is the most efficient way to see your own posts automatically.
.TP
\fBauto-select-subject\fP	(boolean, default false)
When set, selecting an article from the menu using the article id
(a-z), all articles on the menu with the same subject will
automatically be selected as well.
.TP
\fBbackup\fP	(boolean, default true)
When set, a copy of the initial .newsrc and select files will save be
the first time they are changed.  \fInn\fP remembers the initial
contents of these files internally, so the backup variable can be set
any time if not set on start-up.
.TP
\fBbackup-folder-path\fP \fIfile\fP	(string, default "BackupFolder~")
When removing deleted articles from a folder, this variable defines
the name of the file where a (temporary) copy of the original folder
is saved.  If the \fIfile\fP name doesn't contain a `/', the file will
be located in the .nn directory.  Otherwise the file name is used
directly as the relative or full path name of the backup file.
If possible, the old folder will be renamed to the backup folder name;
otherwise the old folder is copied to the backup folder.
.TP
\fBbackup-suffix\fP \fIsuffix\fP	(string, default ".bak")
The suffix appended to file names to make the corresponding backup
file name (see \fBbackup\fP).
.TP
\fBbug-report-address\fP \fIaddress\fP	(string, default nn-bugs@dkuug.dk)
The mail address to which bug reports created with the \fB:bug\fP
command are sent.
.TP
\fBcase-fold-search\fP		(boolean, default true)
When set, string and regular expression matching will be case
independent.  This is related to all commands matching on names or
subjects, except in connection with auto-kill and auto-select where
the individual kill file entries specifies this property.
.TP
\fBcharset\fP \fIcharset\fP	(string, default "us-ascii")
The character set in use on your terminal. Legal values are "us-ascii",
"iso-8859-\fIX\fP", where \fIX\fP is a nonzero digit, and "unknown".
Setting this variable also sets the \fBdata-bits\fP variable to the default
bit width of the character set (7 for "us-ascii" and "unknown", 8 for the
"iso-8859-\fIX\fP" sets).
.sp 0.5v
The value of this variable also determines wether \fInn\fP allows
8-bit characters in the body of articles being posted and letters
being mailed (unless the value is "unknown", in which case this is
determined by the value of the \fBdata-bits\fP variable).
If necessary, \fInn\fP will add extra headers to the 
message indicating its the character set.
.TP
\fBcheck-db-update-time\fP \fIH\fP	(integer, default 12)
When non-zero, \fInn\fP will issue a warning if the database has not
been updated in the last \fIH\fP hours.  The warning will tell you
whether no news has arrived (feed broken?), or whether it is just
\fInnmaster\fP which has not updated the database (dead?).
.TP
\fBcheck-group-access\fP	(boolean, default false)
When set, \fInn\fP will perform a check on the readability of a
group's readability before showing the menu for that group.  Normally,
this is not necessary since all users traditionally have access to all
news groups.  Setting (and locking) this variable may be used to limit
access to a news group via the permissions and ownership of the
group's spool directory (this will only work for non-NNTP sites).
.TP
\fBcollapse-subject\fP \fIoffset\fP	(integer, default 25)
When set (non-negative), subject lines which are too long to be
presented in full on the menus will be "collapsed" by removing a
sufficient number of characters from the subject starting at the given
\fIoffset\fP in the subject.  This is useful in source groups where
the "Part (01/10)" string sometimes disappears from the menu.  When
not set (or negative), the subjects are truncated.
.TP
\fBcolumns\fP \fIcol\fP	(integer, default screen width)
This variable contains the screen width i.e. character positions per
line.
.TP
\fBcomp1-key\fP \fIkey\fP	(key, default \fBspace\fP)
The key which gives the first/next completion, and the default value
when \fInn\fP is prompting for a string, e.g. a file name.
.TP
\fBcomp2-key\fP \fIkey\fP	(key, default \fBtab\fP)
The key which ends the current completion and gives the first
completion for the next component
when \fInn\fP is prompting for a string, e.g. a file name.
.TP
\fBcompress\fP		(boolean, default false)
This variable controls whether text compression (see the
\fBcompress\fP command) is turned on or off when an article is
shown.  The compression is still toggled for the current article with
the \fBcompress\fP command key.
.TP
\fBconfirm-append\fP		(boolean, default false)
When set, \fInn\fP will ask for confirmation before appending an
article to an existing file (see also \fBconfirm-create\fP).
.TP
\fBconfirm-auto-quit\fP		(boolean, default false)
When set, \fInn\fP will ask for confirmation before quitting after
having read the last group.  If not confirmed, \fInn\fP will recycle
the presentation sequence looking for groups that were skipped with
the `N' {\fBnext-group\fP} command.  But it will not look for new
articles arrived since the invocation of \fInn\fP.
.TP
\fBconfirm-create\fP		(boolean, default true)
When set, \fInn\fP will ask for confirmation before creating a new
file or directory when saving or unpacking an article (see also
\fBconfirm-append\fP).
.TP
\fBconfirm-entry\fP		(boolean, default false)
When set, \fInn\fP will ask for confirmation before entering a group
with more than \fBconfirm-entry-limit\fP unread articles (on the first
menu level).  It is useful on slow terminals if you don't want to wait
until \fInn\fP has drawn the first menu to be able to skip the group.
  Answering no to the "Enter?" prompt will cause \fInn\fP to skip to
the next group without marking the current group as read.  If you
answer by hitting \fBinterrupt\fP, \fInn\fP will ask the question
"Mark as read?" which allows you to mark the current group as read
before going to the next group.  If this second question is also
answered by hitting \fBinterrupt\fP, \fInn\fP will quit immediately.
.TP
\fBconfirm-entry-limit\fP \fIarticles\fP	(integer, default 0)
Specifies the minimum number of unread articles in a group for which
the \fBconfirm-entry\fP functionality is activated.
.TP
\fBconfirm-junk-seen\fP		(boolean, default false)
When set, \fInn\fP will require confirmation before marking seen
articles as read when \fBauto-junk-seen\fP is set.
.TP
\fBconfirm-messages\fP		(boolean, default false)
In some cases, \fInn\fP will sleep one second (or more) when it has shown a
message to the user, e.g. in connection with macro debugging.  Setting
.B confirm-messages
will cause \fInn\fP to
\fIwait\fP for you to confirm all messages by hitting any
key.  (It will show the symbol <> to indicate that it is awaiting
confirmation.)
.TP
\fBconsolidated-manual\fP	(boolean, default false)
When set, the \fIonline manual\fP will be presented with one
menu line for each \fIprogram\fP in the \fInn\fP package.
.TP
\fBconsolidated-menu\fP		(boolean, default false)
When set, \fInn\fP will automatically \fIclose\fP all multi-article
subjects on entry to a group, so that each subject only occur once on
the menu page.
.TP
\fBcounter-delim-left\fP	(string, default "[")
The delimiter string output to the left of the article counter in a
closed subject's menu line.
.TP
\fBcounter-delim-right\fP	(string, default "] ")
The delimiter string output to the right of the article counter in a
closed subject's menu line.
.TP
\fBcounter-padding\fP \fIpad\fP		(integer, default 5)
On a consolidated menu, the subjects may not be very well aligned
because the added [...] counters have varying length.  To (partially)
remedy this, all counters (and subjects without counters) are prefixed
by up to \fIpad\fP spaces to get better alignment.  Increasing it
further may yield practially perfect alignment at the cost of less
space for the subject itself.
.TP
\fBcross-filter-seq\fP		(boolean, default true)
When set, cross posted articles will be presented in the first
possible group, i.e. according to the current presentation sequence
(\fIcross\fP-post \fIfilter\fPing on \fIseq\fPuence).  The article is
automatically marked read in the other cross posted groups unless you
unsubscribe to the first group in which it was shown before reading
the other groups.  Likewise, it is sufficient to leave the article
unread in the first group to keep it for later handling.
  If not set, cross-postings are shown in the first group occurring on
the Newsgroups: line which the user subscribes to (i.e. you let the
poster decide which group is most appropriate to read his posting).
.TP
\fBcross-post\fP		(boolean, default false)
Normally, \fInn\fP will only show cross-posted articles in the first
subscribed group on the Newsgroups: line.  When
.B cross-post
is set, \fInn\fP will show cross-posted articles in all subscribed
groups to which they are posted.
.TP
\fBdata-bits\fP \fIbits\fP	(integer, default 7)
When set to 7, \fInn\fP will display characters with the 8th bit set
using a meta-notation \fBM-\fP\fI7bit-char\fP.  If set to 8, these
characters are sent directly to the screen (unless \fBmonitor\fP is
set). Setting the \fBcharset\fP variable also sets this variable to the
default bit width of character set.
.sp 0.5v
It also controls whether keyboard input is 7 or 8 bits, and thus
whether key maps contain 127 or 255 entries.  See the key mapping
section for more details.
.sp 0.5v
If the \fBcharset\fP has value "unknown", the value of \fBdata-bits\fP
also determines wether \fInn\fP allows 8-bit characters in the body of
articles being posted and letters being mailed (this is normally
determined directly by the \fBcharset\fP variable).
.TP
\fBdate\fP		(boolean, default true)
If set \fInn\fP will show the article posting date when articles are
read.
.TP
\fBdebug\fP \fImask\fP	(integer, default 0)
Look in the source if you are going to use this.
.TP
\fBdecode-header-file\fP \fIfile\fP	(string, default "Decode.Headers")
The name of the file in which the header and initial text of articles
decoded with the \fB:decode\fP command is saved.  Unless the file name
starts with a `/', the file will be created in the same directory as
the decoded files.  The information is not saved if this variable is
not set.
.TP
\fBdecode-skip-prefix\fP \fIN\fP	(integer, default 2)
When non-null, the \fB:decode\fP command will automatically skip
\fIupto\fP \fIN\fP characters at the beginning of each line to find
valid uuencoded data.  This allows \fInn\fP to automatically decode
(multi-part) postings which are both uuencoded and packed with shar.
.TP
\fBdefault-distribution\fP \fIdistr\fP	(string, default "world")
The distribution to use as the default suggestion when posting
articles using the \fBfollow\fP and \fBpost\fP commands if the
corresponding \fBfollow-distribution\fP or \fBpost-distribution\fP
variable contains the \fBdefault\fP option.
.TP
\fBdefault-kill-select\fP \fI[1]days\fP	(number, default 30)
Specifies the default action for the \fBK\fP {\fBkill-select\fP}
command if the first prompt is answered by \fBreturn\fP.  It contains
the number of days to keep the kill or select entry in the kill file
(1-99 days).  If it has the value \fIdays\fP+100 (e.g. 130), it
denotes that the default action is to \fIselect\fP rather than kill on
the subject for the specified period.
.TP
\fBdefault-save-file\fP \fIfile\fP	(string, default +$F)
The default save file used when saving articles in news groups where
no save file has been specified in the init file (either in a
\fBsave-files\fP section or in the presentation sequence).
It can also be specified using the abbreviation "+" as the file name
when prompted for a file name even in groups with their own save file.
.TP
\fBdelay-redraw\fP		(boolean, default false)
Normally, \fInn\fP will redraw the screen after extended
commands (:cmd) that clear the screen.  When \fBdelay-redraw\fP is set
\fInn\fP will prompt for another extended command instead of redrawing
the screen (hit \fBreturn\fP to redraw).
.TP
\fBecho-prefix-key\fP		(boolean, default true)
When true, hitting a prefix key (see the section on key mapping below)
will cause the prefix key to be echoed in the message line to indicate
that another key is expected.
.TP
\fBedit-patch-command\fP	(boolean, default true)
When true, the \fB:patch\fP command will show the current
\fBpatch-command\fP and give you a chance to edit it before applying
it to the articles.
.TP
\fBedit-print-command\fP	(boolean, default true)
When true, the \fBprint\fP command will show the current \fBprinter\fP
command and give you a chance to edit it before printing the articles.
Otherwise the articles are just printed using the current \fBprinter\fP
command.
.TP
\fBedit-response-check\fP	(boolean, default true)
When editing a response to an article, it normally does not have any
meaning to send the initial file prepared by \fInn\fP unaltered, since
it is either empty or only contains included material.  When this
variable is set, exiting the editor without having changed the file
will automatically abort the response action without confirmation.
.TP
\fBedit-unshar-command\fP	(boolean, default false)
When true, the \fB:unshar\fP command will show the current
\fBunshar-command\fP and give you a chance to edit it before applying
it to the articles.
.TP
\fBeditor\fP \fIcommand\fP	(string, default not set)
When set, it will override the current EDITOR environment variable
when editing responses and new articles.
.TP
\fBembedded-header-escape\fP \fIstring\fP	(string, default '~')
When saving an article to a file, header lines embedded in the body of
the article are escaped using this string to make it possible for
\fInn\fP to split the folder correctly afterwards.
Header lines are not escaped if this variable is not set.
.TP
\fBenter-last-read-mode\fP \fImode\fP	(integer, default 1)
Normally, \fInn\fP will remember which group is active when you quit,
and offer to jump directly to this group when you start \fInn\fP the
next time.  This variable is used to control this behaviour.  The
following \fImode\fP values are recognized:
.nf
0: Ignore the remembered group (r.g.).
1: Enter r.g. if the group is unread (with user confirmation)
2: Enter r.g. or first unread group after it in the sequence (w/conf).
3: Enter r.g. if the group is unread (no confirmation)
4: Enter r.g. or first unread group after it in the sequence (no conf).
.fi
.TP
\fBentry-report-limit\fP \fIarticles\fP	(integer, default 300)
Normally, \fInn\fP will just move the cursor to the upper left corner
of the screen while it is reading articles from the database on
entry to a group.  For large groups this may take more than a fraction
of a second, and \fInn\fP can then report what it is doing.  If
it must read more articles than the number specified by this variable,
\fInn\fP will report which group and how many articles it is reading.
.TP
\fBerase-key\fP \fIkey\fP	(key, default tty erase key)
The key which erases the last input character
when \fInn\fP is prompting for a string, e.g. a file name.
.TP
\fBexpert\fP		(boolean, default false)
If set \fInn\fP will use slightly shorter prompts (e.g. not tell you
that ? will give you help), and be a bit less verbose in a few other
cases (e.g. not remind you that posted articles are not available
instantly).
.TP
\fBexpired-message-delay\fP \fIpause\fP	(integer, default 1)
If a selected article is found to have been expired, \fInn\fP will
normally give a message about this and sleep for a number of seconds
specified by this variable.  Setting this variable to zero will still
make \fInn\fP give the message without sleeping afterwards.  Setting
it to -1 will cause the message not to be shown at all.
.TP
\fBflow-control\fP	(boolean, default true)
When set, \fInn\fP will turn on xon/xoff flow-control before writing
large amounts of text to the screen.  This should guard against
lossage of output, but in some network configurations it has had the
opposite effect, losing several lines of the output.  This variable
is always true on systems with CBREAK capabilities which can do single
character reads without disabling flow control.
.TP
\fBflush-typeahead\fP	(boolean, default false)
When true, \fInn\fP will flush typeahead prior to reading commands
from the keyboard.  It will not flush typeahead while reading
parameters for a command, e.g. file names etc.
.TP
\fBfolder\fP \fIdirectory\fP	(string, default ~/News)
The full pathname of the
.I folder directory
which will replace the + in folder names.  It will be initialized from
the FOLDER environment variable if it is not set in the
.I init
file.
.TP
\fBfolder-format-check\fP	(boolean, default true)
When saving an article with a full or partial header in an existing
folder, \fInn\fP will check the format of the folder to be able to
append the article in the proper format.  If this variable is not set,
folders are assumed to be in the format specified via the
\fBmmdf-format\fP and \fBmail-format\fP variables, and articles are
saved in that format without checking.  Otherwise, the \fB*-format\fP
variables are only used to determine the format for \fInew\fP folders.
.TP
\fBfolder-save-file\fP \fIfile\fP	(string, default not set)
The default save file used when saving articles \fIfrom\fP a folder.
.TP
\fBfollow-distribution\fP \fIwords\fP	(string, default see below)
This variable controls how the Distribution: header is constructed for
a follow-up to an original article.  Its value is a list of
\fIwords\fP selected from the following list:
.sp 0.5v
[ [ \fBalways\fP ] \fBsame\fP ] [ \fBask\fP ]
[ \fBdefault\fP | \fIdistribution\fP ]
.sp 0.5v
This is interpreted in two steps:
.br
- First the default distribution is determined.  If \fBsame\fP is
specified and the original article has a Distribution: header, that
header is used.  Else if \fBdefault\fP is specified (or
\fIdistribution\fP is omitted), the value of
\fBdefault-distribution\fP is used.  And finally, if only a
\fIdistribution\fP (any word) is specified that is used as the default.
.br
- Then if \fBask\fP is specified, the user will be asked to confirm
the default distribution or provide another distribution.  However, if
\fBalways\fP (and \fBsame\fP) is specified, and the default was taken
from the original article's distribution, the original distribution is
used \fIwithout\fP confirmation.
.br
The default value of \fBfollow-distribution\fP is \fBalways\fP
\fBsame\fP \fBdefault\fP, i.e. use either the original distribution or
the \fBdefault-distribution\fP without confirmation in either case.
.TP
\fBfrom-line-parsing\fP \fIstrictness\fP	(integer, default 2)
Specifies how strict \fInn\fP must parse a "From " line in a folder to
recognize it as a mail format message separator line.  The following
strictness values determine whether a line starting with "From " will
be recognized as a separator line:
.nf
	0: Always.
	1: Line must have at least 8 fields.
	2: Line must contain a valid date and time (ctime style).
.fi
.TP
\fBfsort\fP		(boolean, default true)
When set, folders are sorted alphabetically according to the subject
(and age).
Otherwise, the articles in
a folder will be presented in the sequence in which they were saved.
.TP
\fBguard-double-slash\fP	(boolean, default false)
Normally, when entering a file name, entering two slashes `//' in a
row (or following a slash by a plus `/+') will cause \fInn\fP to
erase the entire line and replace it with the `/' (or `+').  On some
systems, two slashes are used in network file names, and on those
systems \fBguard-double-slash\fP can be set; that will cause \fInn\fP
to require \fIthree\fP slashes in a row to clear the input.
.TP
\fBheader-lines\fP \fIlist\fP	(string, no default)
When set, it determines the list of header fields that are shown when
an article is read instead of the normal one line header showing the
author and subject.  See the full description in the section on
Customized Article Headers below.
.TP
\fBhelp-key\fP \fIkey\fP	(key, default \fB?\fP)
The key which ends the current completion and gives a list of possible
completions for the next component
when \fInn\fP is prompting for a string, e.g. a file name.
.TP
\fBignore-re\fP		(boolean, default false)
If set, articles with subjects already seen in a previous
invocation of nn or another newsreader - and not auto-selected -
are automatically killed.  A great way to read even less news!
.TP
\fBignore-xon-xoff\fP		(boolean, default false)
Normally, \fInn\fP will ignore ^S and ^Q in the input from the
terminal (if they are not handled in the tty driver).  Setting this
variable will treat these characters as normal input.
.TP
\fBinclude-art-id\fP		(boolean, default false)
The first line in a response with included material normally reads
\&"...somebody... writes:" without a reference to the specific article
from which the quotation was taken (this is found in the References:
line).  When this variable is set, the line will also include the
article id of the referenced article: "In ...article... ... writes:".
.TP
\fBinclude-full-header\fP	(boolean, default false)
When set, the \fBmail\fP (M) command will always include the full
header of the original article.  If it is not set, it only includes
the header when the article is forwarded without being edited.
.TP
\fBinclude-mark-blank-lines\fP	(boolean, default false)
When set, the \fBincluded-mark\fP is placed on blank lines in included
articles.  Otherwise, blank lines are left blank (to make it easy to
delete whole paragraphs with `d}' in vi and `C-@ M-] C-W' in emacs).
.TP
\fBincluded-mark\fP \fIstring\fP	(string, default ">")
This string is prefixed to all lines in the original article that are
included in a reply or a follow-up.  (Now you have the possibility to
change it, but please don't.  Lines with a mixture of prefixes like
.br
   : orig-> <> } ] #- etc.
.br
are very difficult to comprehend.  Let's all use the standard folks!
(And hack inews if it is the 50% rule that bothers you.)
.TP
\fBinews\fP \fIshell-command\fP	(string, default "INEWS_PATH -h")
The program which is invoked by \fInn\fP to deliver an article to the
news transport.  The program will be given a complete article
including a header containing the newsgroups to which the article is
to be posted.  See also \fBinews-pipe-input\fP.  It is \fInot\fP used
when cancelling an article!
.TP
\fBinews-pipe-input\fP		(boolean, default true)
When set, the article to be posted will be piped into the \fBinews\fP
program.  Otherwise, the file containing the article will be given as
the first (and only) argument to the \fBinews\fP command.
.TP
\fBinitial-newsrc-file\fP \fIfile\fP	(string, default '.defaultnewsrc')
Defines the name of a file which is used as the initial .newsrc file
for new users.  The name may be a full path name, or as the default a
file name which will be looked for in a number of places:
in the standard news lib directory (where it can be shared with other
news readers),
in nn's lib directory,
and in the database directory.
Groups which are not present in the initial .newsrc file will be
automatically unsubscribed provided \fBnew-group-action\fP is set to a
value allowing unsubscribed groups to be omitted from .newsrc.
.TP
\fBkeep-backup-folder\fP	(boolean, default false)
When set, the backup folder (see \fBbackup-folder-path\fP) created
when removing deleted articles from a folder is not removed.
Notice that a backup folder is not created if all articles are removed
from a folder!
.TP
\fBkeep-unsubscribed\fP		(boolean, default true)
When set, unsubscribed groups are kept in .newsrc.  If not set,
\fInn\fP will automatically remove all unsubscribed from .newsrc if
\fBtidy-newsrc\fP is set.  See also \fBunsubscribe-mark-read\fP.
.TP
\fBkill\fP		(boolean, default true)
If set, \fInn\fP performs automatic kill and selection based on the
.I kill
file.
.TP
\fBkill-debug\fP		(boolean, default false)
When set, \fInn\fP will display a trace of the auto-kill/select
process on entry to a group.
It is automatically turned off if `q' is entered as the answer to a
"hit any key" prompt during the debug output.
.TP
\fBkill-key\fP \fIkey\fP	(key, default tty kill key)
The key which deletes the current line
when \fInn\fP is prompting for a string, e.g. a file name.
.TP
\fBkill-reference-count\fP \fIN\fP	(integer, default 0)
When this variable is non-zero, all articles which have \fIN\fP or
more references on the References: line (corresponding to the number
of >>'s on the menu line) will be auto-killed if they are not
auto-selected (or preserved) via an entry in the kill file.  It should
probably not be used globally for all groups, but can be set on a
per-group via the entry macros.
.TP
\fBlayout\fP \fInumber\fP	(integer, default 1)
Set the menu layout.  The argument must be a number between 0 and 4.
.TP
\fBlimit\fP \fImax-articles\fP	(integer, default infinite)
.I Limit
the maximum number of articles presented in each group to
.I max-articles.
The default is to present
.I all
unread articles no matter how many there are.  Setting this variable,
only the most recent
.I max-articles
articles will be presented, but all the articles will still be marked
as read.  This is useful to get up-to-date quickly if you have not
read news for a longer period.
.TP
\fBlines\fP \fIlin\fP	(integer, default screen height)
This variable contains the screen height i.e. number of lines.
.TP
\fBlong-menu\fP		(boolean, default false)
If set \fInn\fP will not put an empty line after the header line and
an empty line before the prompt line; this gives you two extra menu
lines.
.TP
\fBmacro-debug\fP	(boolean, default false)
If set \fInn\fP will trace the execution of all macros.  Prior to the
execution of each command or operation in a macro, it will show the
name of the command or the input string or key stroke at the bottom of
the screen.
.TP
\fBmail\fP \fIfile\fP	(string, default not set)
\fIfile\fP must be a full path name of a file.  If defined, \fInn\fP will
check for arrival of new mail every minute or so by looking at the
specified file.
.TP
\fBmail-alias-expander\fP \fIprogram\fP	(string, default not set)
When set, aliases used in mail responses may be expanded by the
specified \fIprogram\fP.  The program will be given the completed
response in a file as its only argument, and the aliases should be
expanded directly in this file (of course the \fIprogram\fP may use
temporary files and other means to expand the aliases as long the the
result is stored in the provided file).
.br
Notice: currently there are no alias expanders delivered with \fInn\fP.
.br
Warning: Errors in the expansion process may lead to the response
not being sent.
.TP
\fBmail-format\fP	(boolean, default false)
When set, \fInn\fP will save articles in a format that is compatible
with normal mail folders.
Unless \fBfolder-format-check\fP is false, it is only used to specify
the format used when new folders are created.
This variable is ignored if \fBmmdf-format\fP is set.
.TP
\fBmail-header\fP \fIheaders\fP	(string, default not set)
The \fIheaders\fP string specifies one or more extra header lines
(separated by semi-colons `;') which are added to the header of mail
sent from \fInn\fP using the \fBreply\fP and \fBmail\fP commands.  For
example:
.nf
	set mail-header Reply-To: storm@texas.dk;Organization: TI - DK
.fi
To include a semicolon `;' in a header, precede it by a backslash (which
must be doubled because of the conventions for entering strings).
.TP
\fBmail-record\fP \fIfile\fP	(string, default not set)
\fIfile\fP must be a full path name of a file.  If defined, all replies and
mail will be saved in this file in standard
.I mailbox
format, i.e. you can use you favourite mailer (and \fInn\fP) to look at
the file.
.TP
\fBmail-script\fP \fIfile\fP	(string, default not set)
When set, \fInn\fP will use the specified file instead of the standard
\fIaux\fP script when executing the \fBreply\fP and \fBmail\fP
commands.
.TP
\fBmailer\fP \fIshell-command\fP	(string, default REC_MAIL)
The program which is invoked by \fInn\fP to deliver a message to the
mail transport.  The program will be given a complete mail message
including a header containing the recipient's address.  See also
\fBmailer-pipe-input\fP.
.TP
\fBmailer-pipe-input\fP		(boolean, default true)
When set, the message to be sent will be piped into the \fBmailer\fP
program.  Otherwise, the file containing the message will be given as
the first (and only) argument to the \fBmailer\fP command.
.TP
\fBmarked-by-next-group\fP \fIN\fP	(integer, default 0)
Specifies the amount of (unmarked) articles on the menu marked
\fIseen\fP by the \fBN\fP {\fBnext-group\fP} command in selection
mode.  See \fBmarked-by-read-skip\fP for possible values of \fIN\fP.
.TP
\fBmarked-by-read-return\fP \fIN\fP	(integer, default 0)
Specifies the amount of (unmarked) articles on the menu marked
\fIseen\fP by the \fBZ\fP {\fBread-return\fP} command in selection
mode.  See \fBmarked-by-read-skip\fP for possible values of \fIN\fP.
.TP
\fBmarked-by-read-skip\fP \fIN\fP	(integer, default 4)
Specifies the amount of (unmarked) articles on the menu marked
\fIseen\fP by the \fBX\fP {\fBread-skip\fP} command in selection mode.
The following values of \fIN\fP are recognized:
.nf
	0:  No articles are marked seen
	1:  Current page is marked seen
	2:  Previous pages are marked seen
	3:  Previous and current pages are marked seen
	4:  All pages are marked seen
.fi
.TP
\fBmark-overlap\fP	(boolean, default false)
When set, \fInn\fP will draw a line (using the underline capabilities
of the terminal if possible) to indicate the end of the overlap (see the
\fBoverlap\fP variable).
.TP
\fBmark-overlap-shading\fP	(boolean, default false)
When set, \fInn\fP will \fIshade\fP overlapping lines (see the
\fBoverlap\fP variable) using the attributes defined by the
\fBshading-on\fP and \fBshading-off\fP variables (of if not set, with
the underline attribute).  This is typically used to give overlapping
lines a different colour on terminals which have this capability.
.TP
\fBmenu-spacing\fP \fImode\fP	(integer, default 0)
When \fImode\fP is a non-zero number as described below, \fInn\fP will
add blank lines between the lines on the menu to increase readability
at the cost of presenting fewer articles on each page.  The following
values of \fImode\fP are recognized:
.nf
0: Don't add blank lines between menu lines.
1: Add a blank line between articles with \fIdifferent\fP subjects.
2: Add a blank line between \fIall\fP articles.
.fi
.TP
\fBmerge-report-rate\fP \fIrate\fP	(integer, default 1)
When \fInn\fP is invoked with the -m option (directly or via
\fInngrap\fP), a status report of the merging process is displayed and
updated on the screen every \fIrate\fP seconds.  The report contains
the time used so far and an estimate of the time needed to complete
the merge.
.TP
\fBmessage-history\fP \fIN\fP	(integer, default 15)
Specifies the maximum number, \fIN\fP, of older messages which can be
recalled with the \fB^P\fP {\fBmessage\fP} command.
.TP
\fBmin-window\fP \fIsize\fP	(integer, default 7)
When the \fBwindow\fP variable is not set, \fInn\fP will clear the
screen to preview an article if there are less than \fIsize\fP unused
lines at the bottom of the menu screen.
.TP
\fBmmdf-format\fP	(boolean, default false)
When set, \fInn\fP will save articles in MMDF format.
Unless \fBfolder-format-check\fP is false, it is only used to specify
the format used when new folders are created.
.TP
\fBmonitor\fP		(boolean, default false)
When set, \fInn\fP will show
.I all
characters in the received messages using a "cat -v" like format.
Otherwise, only the printable characters are shown (default).
.TP
\fBmotd\fP		(boolean, default true)
When set, \fInn\fP will display the \fImessage of the day\fP on
start-up if it has changed since it was last shown.  The message is
taken from the file "motd" in the lib directory.  It can also be shown
(again) using the \fB:motd\fP command.
.TP
\fBmulti-key-guard-time\fP \fItimeout\fP	(integer, default 2)
When reading a multi-key sequence from the keyboard, \fInn\fP will
expect the characters constituting the multi-key to arrive "quickly"
after each other.  When a partial multi-key sequence is read,
\fInn\fP will wait (at least) \fItimeout\fP tenths of a second for
each of the following characters to arrive to complete the multi-key
sequence.  If the multi-key sequence is \fInot\fP completed within
this period, \fInn\fP will read the partial multi-key sequence as
individual characters instead.  This way it is still possible to use
for example the ESC key on a terminal with vt100 like arrow keys.
When \fInn\fP is used via an rlogin connection, you may have to
increase the timeout to get reliable recognition of multi-keys.
.TP
\fBnew-group-action\fP \fIaction\fP	(integer, default 3)
This variable controls how new groups are treated by \fInn\fP.  It is
an integer variable, and the following values can be used.  Some of
these actions (marked with an *) will only work when
\fBkeep-unsubscribed\fP is set, since the presence of a group in
\&.newsrc is the only way to recognize it as an old group:
.sp 0.5v
\fB0\fP)  Ignore groups which are not in \&.newsrc.  This will obviously
include new groups.
.sp 0.5v
\fB1\fP*)  Groups not in \&.newsrc are considered to be new, and are
inserted at the beginning of the \&.newsrc file.
.sp 0.5v
\fB2\fP*)  Groups not in \&.newsrc are considered to be new, and are
appended to the end of the \&.newsrc file.
.sp 0.5v
\fB3\fP)  New groups are recognized via a time-stamp saved in the
file \&.nn/LAST and in the database, i.e. it is not dependent on the
groups currently in \&.newsrc.  The new groups are automatically
appended to \&.newsrc with subscription.  Old groups not present in
\&.newsrc will be considered to be unsubscribed.
.sp 0.5v
\fB4\fP)  As \fB3\fP, but the user is asked to confirm that the new
group should be appended to \&.newsrc.  If rejected, the group will not
be appended to \&.newsrc, and thus be regarded as unsubscribed.
.sp 0.5v
\fB5\fP)  As \fB4\fP, except that the information is stored in a
format compatible with the \fIrn\fP news reader (\&.rnlast).  This needs
to be tested!
.TP
\fBnew-style-read-prompt\fP	(boolean, default true)
When set, the reading mode prompt line includes the group name and the
number of selected articles in the group.
.TP
\fBnews-header\fP \fIheaders\fP	(string, default not set)
The \fIheaders\fP string specifies one or more extra header lines
(separated by semi-colons `;') which are added to the header of
articles posted from \fInn\fP using the \fBfollow\fP and \fBpost\fP
commands.  See \fBmail-header\fP for an example.
.TP
\fBnews-record\fP \fIfile\fP	(string, default not set)
Save file for follow-ups and postings.  Same rules and format as the
\fBmail-record\fP variable.
.TP
\fBnews-script\fP \fIfile\fP	(string, default not set)
When set, \fInn\fP will use the specified file instead of the standard
\fIaux\fP script when executing the \fBfollow\fP and \fBpost\fP
commands.
.TP
\fBnewsrc\fP \fIfile\fP (string, default "~/.newsrc") Specifies the
file used by \fInn\fP to register which groups and articles have been
read.  The default setting corresponds to the \&.newsrc file used by
other news readers.  Notice that \fInn\fP release 6.4 onwards
\fIdoes allow\fP individual articles to be marked unread, and some
articles marked unread, and thus no longer messes up \&.newsrc for
other news readers!
.TP
\fBnntp-cache-dir\fP \fIdirectory\fP	(string, default "~/.nn")
When NNTP is used, \fInn\fP needs to store articles temporarily on
disk.  This variable specifies which directory \fInn\fP will use to
hold these files.  The default value may be changed during
configuration.  This variable can only be set in the init file.
.TP
\fBnntp-cache-size\fP \fIsize\fP	(integer, default 10, maximum 10)
Specifies the number of temporary files in the nntp cache.  The
default and maximum values may be changed during configuration.
.TP
\fBnntp-debug\fP	(boolean, default false)
When set, a trace of the nntp related traffic is displayed in the
message line on the screen.
.TP
\fBold\fP [\fImax-articles\fP]	(integer, default not set)
When
.B old
is set, \fInn\fP will present (or scan) all (or the last
\fImax-articles\fP) unread as well as
read articles.  While
.B old
is set, \fInn\fP will
.I never
mark any unread articles as read.
.TP
\fBorig-to-include-mask\fP \fIN\fP	(integer, default 3)
When replying to an article, \fInn\fP will include some of the header
lines which may be used to construct a proper mail address for the
poster of the original article.  These addresses are placed on
\fIOrig-To:\fP lines in the reply header and will automatically be
removed before the letter is sent.  This variable specifies which
headers from the article are included; its value \fIN\fP is the sum of
the following values:
.nf
	1: \fIReply-To:\fP
	2: \fIFrom:\fP
	4: \fIPath:\fP
.fi
.TP
\fBoverlap\fP \fIlines\fP	(integer, default 2)
Specifies the number of overlapping lines from one page to the next
when paging through an article in reading mode.
The last line from the previous page
will be underlined if the terminal has that capability.
.TP
\fBpager\fP \fIshell-command\fP		(string, default $PAGER)
This is the pager used by the \fB:admin\fP command (and \fInnadmin\fP)
when it executes certain commands, e.g. grepping in the Log file.
.TP
\fBpatch-command\fP \fIshell-command\fP	(string, default "patch -p0")
This is the command which is invoked by the \fB:patch\fP command.
.TP
\fBpost-distribution\fP \fIwords\fP	(string, default see below)
This variable controls how the Distribution: header is constructed
when posting an original article.  Its value is a list of
\fIwords\fP selected from the following list:
.sp 0.5v
[ \fBask\fP ] [ \fBdefault\fP | \fIdistribution\fP ]
.sp 0.5v
This is interpreted in two steps:
.br
- First the default distribution is determined.  If \fBdefault\fP is
specified (or \fIdistribution\fP is omitted), the value of
\fBdefault-distribution\fP is used.  Otherwise, the specified
\fIdistribution\fP (any word) is used as the default.
.br
- Then if \fBask\fP is specified, the user will be asked to confirm
the default distribution or provide another distribution.
.br
The default value of \fBpost-distribution\fP is \fBask\fP
\fBdefault\fP, i.e. use the \fBdefault-distribution\fP with
confirmation from the user.
.TP
\fBpreview-continuation\fP \fIcond\fP	(integer, default 12)
This variable determines on what terms the following article should be
automatically shown when previewing an article, and the
\fBnext-article\fP command is used, or \fBcontinue\fP is used at the
end of the article.  The following values
can be used:
.br
\fB0\fP \- never show the next article (return to the menu).
.br
\fB1\fP \- always show the next article (use 'q' to return to the menu).
.br
\fB2\fP \- show the next article if it has the same subject as the
current article, else return to the menu.
.br
The value should be the \fIsum\fP of two values: one for the action
after using \fBcontinue\fP on the last page of the article, and one
for the action performed when the \fBnext-article\fP command is used
\fImultiplied by 10\fP.
.TP
\fBpreview-mark-read\fP		(boolean, default true)
When set, previewing an article will mark the article as read.
.TP
\fBprevious-also-read\fP	(boolean, default true)
When set, going back to the previously read group with \fBP\fP
{\fBprevious\fP} will include articles read in the current invocation
of \fInn\fP even if there are still unread articles in the group.
.TP
\fBprint-header-lines\fP \fIfields\fP	(string, default "FDGS")
Specifies the list of header fields that are output when
an article is printed via the \fB:print\fP command and
\fBprint-header-type\fP is 1 (short header).  The \fIfields\fP 
specification is desctribed
in the section on Customized Article Headers below.
.TP
\fBprint-header-type\fP \fIN\fP	(integer, default 1)
Specifies what kind of header is printed by the \fB:print\fP command,
corresponding to the three \fBsave-*\fP commands: \fI0\fP prints only
the article body (no header), \fI1\fP prints a short header,
and \fI2\fP prints the full article header.
.TP
\fBprinter\fP \fIshell-command\fP	(string, default is system dep.)
This is the default value for the
.B print
command.  It should include an option which prevents the spooler from
echoing a job-id or similar to the terminal to avoid problems with
screen handling (e.g. lp -s on System V).
.TP
\fBquery-signature\fP		(boolean, default ...)
Will cause \fInn\fP to require confirmation before appending
the \&.signature file to out-going mail or news if the corresponding
\fBappend-sig-\fP... variable is set.
.TP
\fBquick-count\fP	(boolean, default true)
When set, calculating the total number of unread articles at start-up
is done by simple subtracting the first unread article number from the
total number of articles in each group.  This is very fast, and fairly
accurate but it may be a bit too large.  If not set, each line in
\&.newsrc will be interpreted to count every unread article, thus giving
a very accurate number.  This variable is also used by \fInncheck\fP.
.TP
\fBquick-save\fP	(boolean, default false)
When set, \fInn\fP will not prompt for a file name when an article is
saved (unless it belongs to a folder).
Instead it uses the save file specified for the current group in the
init file or the default save file.
.TP
\fBre-layout\fP \fIN\fP		(integer, default 0)
Normally on the menu, \fInn\fP will prefix the subject a number of
`>'s corresponding to the number of references on the References:
line.  The \fBre-layout\fP variable may be set to use a different
prefix on the subjects:
.nf
	0:  One `>' per reference is shown (default).
	1:  A single `>' is shown if the Subject contains Re:.
	2:  The number of references is shown as `n>'
	3:  A single Re: is shown.
	4:  If any references use layout 0, else layout 1.
.fi
.TP
\fBre-layout-read\fP \fIN\fP	(integer, default -1)
When the \fBheader-lines\fP variable is not set, or contains the "*"
field specifier, a line similar to the menu line will be used as the
header of the article in reading mode, including the sender's name and
the article's subject.  When this variable is negative, the subject
on this header line will be prefixed according to the \fBre-layout\fP
variable.  Otherwise, it will define the format of the "Re:" prefix to
be used instead of the \fBre-layout\fP used on the menu.
.TP
\fBread-return-next-page\fP	(boolean, default false)
When set, the \fBZ\fP {\fBread-return\fP} command will return to the
\fInext\fP menu page rather than the current menu page.
.TP
\fBrecord\fP \fIfile\fP	(string, no default)
Setting this
.I pseudo
variable will set both the \fBmail-record\fP and the
\fBnews-record\fP variables to the specified pathname.
.TP
\fBrepeat\fP		(boolean, default false)
When set, \fInn\fP will not eliminate duplicated subject lines on
menus (I cannot imagine why anyone should want that, but....)
.TP
\fBrepeat-group-query\fP	(boolean, default false)
When set, invoking \fInn\fP with the \fB\-g\fP option will always
repeat the query for a group to enter until you quit explicitly.
(Same as setting the \fB\-r\fP option permanently).
.TP
\fBreport-cost\fP		(boolean, default true)
This variable is ignored unless \fInn\fP is running with accounting
enabled (see \fInnacct\fP).  When set, \fInn\fP will report the cost
of the current session and the total on exit.
.TP
\fBresponse-check-pause\fP \fIpause\fP	(integer, default 2)
Specifies the number of seconds to wait after posting an article to
see whether the action *might* have failed.  Some commands run in the
background and may thus not have completed during this period, so even
when \fInn\fP says "Article posted", it may still fail (in which case
you are informed via mail).
.TP
\fBresponse-default-answer\fP \fIaction\fP	(string, default "send")
The default action to be taken when hitting \fBreturn\fP to the
"response action" prompt  (abort, edit, send, view, write).  If it is
unset, no default action is defined.
.TP
\fBretain-seen-status\fP	(boolean, default false)
Normally, seen articles will just be unread the next time the group is
entered (unless they were marked read by \fBauto-junk-seen\fP).  If
\fBretain-seen-status\fP is set, the seen attribute on the articles
will survive to the next time the group is entered.  (This is not
recommended because it may result in very large select files).
.TP
\fBretry-on-error\fP \fItimes\fP	(integer, default 0)
When set, \fInn\fP will try the specified number of \fItimes\fP to
open an article before reporting that the article does not exist
any more.  This may be necessary in some network environments.
.TP
\fBsave-closed-mode\fP \fImode\fP	(integer, default 13)
When saving an article in selection mode (i.e. by selecting it from
the menu), \fInn\fP will simply save the specified article if the
article's subject is \fIopen\fP.  When the selected menu entry is a
closed subject, the \fBsave-closed-mode\fP variable determines how
many articles among the closed articles should be saved:
.nf
0: save root article (the one on the menu) only
1: save selected articles within subject
2: save unread (excl selected) articles within subject
3: save selected+unread articles within subject
4: save all articles within subject
.fi
If `10' is added to the above values, \fInn\fP will not save the
selected subject immediately; instead it will ask which articles
to save using the above value as the default answer.
.TP
\fBsave-counter\fP \fIformat\fP	(string, default "%d")
This is the printf-format which \fInn\fP uses to create substitution
string for the trailing * in save file names.  You can set this to
more complex formats if you like, but be sure that it will produce
different strings for different numbers.  An alternative format which
seems to be popular is ".%02d" .
.TP
\fBsave-counter-offset\fP \fIN\fP	(integer, default 0)
Normally, file names created with the \fIpart.*\fP form will
substitute the \fI*\fP with successive numbers starting from one.
Setting this variable will cause these numbers to start from \fIN\fP+1.
.TP
\fBsave-header-lines\fP \fIfields\fP	(string, default "FDNS")
Specifies the list of header fields that are saved when
an article is saved via the \fBO\fP {\fBsave-short\fP} command.
The \fIfields\fP specification is desctribed
in the section on Customized Article Headers below.
.TP
\fBsave-report\fP	(boolean, default true)
When set, a message reporting the number of lines written is shown
after saving an article.  Since messages are shown for a few seconds,
this may slow down the saving of many articles (e.g. using the
.B S*
command).
.TP
\fBscroll-clear-page\fP		(boolean, default true)
Determines whether \fInn\fP clears the screen before showing each new
page of an article.
.TP
\fBscroll-last-lines\fP \fIN\fP		(integer, default 0)
Normally, \fInn\fP will show each new page of an article from the top
of the screen (with proper marking of the overlap).  When this
variable is set to a negative value, \fInn\fP will scroll the text of
the new pages from the bottom of the screen instead.  If it is set to a
positive value, \fInn\fP will show pages from the top as usual, but
switch to scrolling when there are \fIless than\fP the specified
number of lines left in the article.
.TP
\fBselect-leave-next\fP		(boolean, default false)
When set, you will be asked whether to select articles with the
\fBleave-next\fP attribute on entry to a group with left over
articles.
.TP
\fBselect-on-sender\fP		(boolean, default false)
Specifies whether the \fBfind\fP (=) command in article selection mode
will match on the subject or the sender.
.TP
\fBshading-on\fP \fIcode\fP...	(control string, default not set)
Specifies the escape code to be sent to the terminal to cause
"shading" of the following output to the screen.  This is used if the
\fBmark-overlap-shading\fP is set, and by the `+' attribute in the
\fBheader-lines\fP variable.
.TP
\fBshading-off\fP \fIcode\fP...	(control string, default not set)
Specifies the escape code to be sent to the terminal to turn off the
shading defined by \fBshading-on\fP.  Shading will typically
be done by changing the foreground colour to change, e.g.
.sp 0.5v
.nf
	on term ti924-colour
		set shading-on  ^[ [ 3 2 m
		set shading-off ^[ [ 3 7 m
		set mark-overlap-shading
		unset mark-overlap
	end
.fi
.TP
\fBshell\fP \fIprogram\fP	(string, default $SHELL)
The shell program used to execute shell escapes.
.TP
\fBshell-restrictions\fP	(boolean, default false)
When set (in the init file), \fInn\fP will not allow the user to
invoke the shell in any way, including saving on pipes.  It also
prevents the user from changing certain variables containing commands.
.TP
\fBshow-purpose-mode\fP \fIN\fP		(integer, default 1)
Normally, \fInn\fP will show the purpose of a group the first time it
is read, provided a purpose is known.  Setting this variable, this
behaviour can be changed as follows:
.nf
	0:  Never show the purpose.
	1:  Show the purpose for new groups only.
	2:  Show the purpose for all groups.
.fi
.TP
\fBsilent\fP		(boolean, default false)
When set, \fInn\fP wont print the logo or "No News" if there are no
unread articles.  Only useful to set in the init file or with the
.B \-Q
option.
.TP
\fBslow-mode\fP		(boolean, default false)
When set, \fInn\fP will cut down on the screen output to give better
response time at low speed.
Normally, \fInn\fP will use standout mode (if possible) to mark
selected articles on the menu, but when \fBslow-mode\fP is set, \fInn\fP will
just put an asterisk `*' next to the article identifier on selected
articles.  Also when \fBslow-mode\fP is set \fInn\fP will avoid
redrawing the screen in the following cases:  After a \fBgoto-group\fP
command an empty menu is shown (hit \fBspace\fP to make it appear),
and after responding to an article, only the prompt line is shown (use
^L to redraw the screen).  To avoid redrawing the screen after an
extended command, set the \fBdelay-redraw\fP variable as well.
.TP
\fBslow-speed\fP \fIspeed\fP	(integer, default 1200)
If the terminal is running at this baud rate or lower, the \fBon
slow\fP (see the section on init files) condition will be true, and
the \fBon fast\fP will be false (and vice-versa).
.TP
\fBsort\fP		(boolean, default true)
When set, \fInn\fP will sort articles according to the current
\fBsort-mode\fP on entry to a group.  Otherwise, articles will be
presented in order of arrival.
If not set on entry to a menu for merged groups, the articles from
each group will be kept together on the menu.  If \fBsort\fP is unset
while merged groups are presented on the menu, the articles will be
reordered by local article number (which may not keep articles from
the same group together).
.TP
\fBsort-mode\fP \fImode\fP	(integer, default 1)
The default sort algorithm used to sort the articles on entry to a
news group.  It is a numeric value corresponding to one of the sorting
methods described in connection with the :sort command:
.nf
	0 \- arrival (ordered by article number)
	1 \- subject (subjects ordered after age of first article)
	2 \- lexical (subjects in lexicographical order)
	3 \- age (articles ordered after posting date only)
	4 \- sender (articles ordered after sender's name)
.fi
.TP
\fBspell-checker\fP \fIshell-command\fP	(string, default not set)
When set, responses can be checked for spelling mistakes via the
(i)spell action.  The command to perform the spelling is given the
file containing the full article including header as its only
argument.  If the spell checker can fix spelling mistakes, it must
apply the changes directly to this file.
.TP
\fBsplit\fP		(boolean, default true)
When set, digests will automatically and silently be split into
sub-articles which are then handled transparently as normal articles.
Otherwise, digests are presented as one article (which you can split
on demand with the
.B G
command).
.TP
\fBstop\fP \fIlines\fP	(integer, default not set)
When
.B stop
is set, \fInn\fP will only show the first \fIlines\fP lines of the
of each article
before prompting you to continue.  This is useful on slow terminals and
modem lines to be able to see the first few lines of longer articles
(and skipping the rest with the
.B n
command).
.TP
\fBsubject-match-limit\fP \fIlength\fP	(integer, default 256)
Subjects will be considered identical if their first \fIlength\fP
characters match.  Setting this uncritically to a low value may
cause unexpected results!
.TP
\fBsubject-match-offset\fP \fIoffset\fP	(integer, default 0)
When set to a positive number, that many characters at the beginning
of the subject will be ignored when comparing subjects for ordering
and equality purposes.
.TP
\fBsubject-match-parts\fP	(boolean, default false)
When set, two subjects will be considered equal if they are identical
up to the first (differing) digit.  Together with the
\fBsubject-match-offset\fP variable, this can be used in source groups
where the subject often has a format like:
.sp 0.5v
.nf
	vXXXXXX: Name of the package (Part 01/04)
.fi
.sp 0.5v
Setting \fBsubject-match-offset\fP to 8 and \fBsubject-match-parts\fP
to true will make \fInn\fP consider all four parts of the package
having the same subject (and thus be selectable with `*').
.sp 0.5
Notice that changing the \fBsubject-match-\fP... variables manually
will not have an immediate effect.  To reorder the menu, an explicit
\fB:sort\fP command must be performed.  These variables are mainly
intended to be set using the \fB:local\fP command in \fBon entry\fP
macros for source and binary groups (entry macros are evaluated before
the menu is collected and sorted).
.TP
\fBsubject-match-minimum\fP \fIcharacters\fP	(integer, default 4)
When set to a positive number, that many characters at the beginning
of the subject must match before the subject-match-parts option comes
into affect.  This is important, because the part matching causes the
rest of the line to be ignored after the first digit pair is
discovered.  This begins after any subject-match-offset has been
applied. 
.TP
\fBsuggest-default-save\fP	(boolean, default true)
When set, \fInn\fP will present the \fBdefault-save-file\fP when
prompting for a save file name in a group without a specific save
file, or \fBfolder-save-file\fP when saving from a folder.  When not
set, no file name is presented, and to use the default
save file, a single + must be specified.
.TP
\fBtidy-newsrc\fP		(boolean, default false)
When set, \fInn\fP will automatically remove lines from .newsrc which
represent groups not found in the active file or unsubscribed groups
if \fBkeep-unsubscribed\fP is not set.
.TP
\fBtime\fP		(boolean, default true)
When set, \fInn\fP will show the current time in the prompt line.
This is useful on systems without a
.I sysline (1)
utility.
.TP
\fBtrace-folder-packing\fP	(boolean, default true)
When set, a trace of the retained and deleted messages is printed when
a folder is rewritten.
.TP
\fBtrusted-escape-codes\fP \fIcodes\fP	(string, default none)
When set to a list of one or more characters, \fInn\fP will trust and
output \fIescape\fP characters in an article if it is followed by one
of the characters in the list.  For example, to switch to or from
kanji mode, control codes like "\fIesc\fP\ $" and "\fIesc\fP\ (\ J"
may be present in the text.  To allow these codes, use the following
command:
.sp 0.5v
.nf
	set trusted-escape-codes ($
.fi
.sp 0.5v
You can also set it to \fBall\fP to pass all espace codes through to
the screen.  Notice that \fInn\fP thinks all characters (including
\fIesc\fP) output to the screen as occupy one column.
.TP
\fBunshar-command\fP \fIshell-command\fP	(string, default "/bin/sh")
This is the command which is invoked by the \fBunshar\fP command.
.TP
\fBunshar-header-file\fP \fIfile\fP	(string, default "Unshar.Headers")
The name of the file in which the header and initial text of articles
unpacked with the \fB:unshar\fP command is saved.  Unless the file name
starts with a `/', the file will be created in the same directory as
the unpacked files.  The information is not saved if this variable is
not set.  Setting it to "Unshar.Result" will cause the headers and the
results from the unpacking process to be merged in a meaningful way
(unless \fBmmdf-format\fP is set).
.TP
\fBunsubscribe-mark-read\fP	(boolean, default true)
When set, unsubscribing to a group will automatically mark all current
articles read; this is recommended to keep the size of .newsrc down.
Otherwise, unread articles in the unsubscribe groups are kept in
\&.newsrc.  If \fBkeep-unsubscribed\fP is false, this variable has no
effect.
.TP
\fBupdate-frequency\fP		(integer, default 1)
Specifies how many changes need to be done to the .newsrc or select
files before they are written back to disk.  The default setting
causes .newsrc to be updated every time a group has been read.
.TP
\fBuse-path-in-from\fP		(boolean, default false)
When \fBmail-format\fP is set, saved articles will be preceded by a
specially formatted \&"From\ " line:
.nf
	From origin date
.fi
Normally, the origin will be the name of the news group where the
article appeared, but if \fBuse-path-in-from\fP is set, the contents
of the "Path:" header will be used as the origin.
.TP
\fBuse-selections\fP		(boolean, default true)
When set, \fInn\fP uses the selections and other article attributes
saved last time \fInn\fP was used.  If not set, \fInn\fP ignores the
select file.
.TP
\fBvisible-bell\fP	(boolean, default true)
When set, \fInn\fP will flash the screen instead of "ringing the
bell" if the visible bell (flash) capability is defined in the
termcap/terminfo database.
.TP
\fBwindow\fP \fIsize\fP	(integer, default not set)
When set, \fInn\fP will reserve the last \fIsize\fP lines of the menu
screen for a preview window.  If not set, \fInn\fP will clear the
screen to preview an article if there are less than \fBmin-window\fP
lines at the
bottom of the screen.  As a side effect, it can also be used to reduce
the size of the menus, which may be useful on slow terminals.
.TP
\fBword-key\fP \fIkey\fP	(key, default ^W)
The key which erases the last input component or word
when \fInn\fP is prompting for a string, e.g. the last name in a path
name.
.TP
\fBwrap-header-margin\fP \fIsize\fP	(integer, default 6)
When set (non-negative), the customized header fields specified in
\fBheader-lines\fP will be split across several lines if they don't
fit on one line.  When \fIsize\fP is greater than zero, lines will be
split at the first space occurring in the last \fIsize\fP columns of
the line.  If not set (or negative), long header lines will be
truncated if they don't fit on a single line.
.\" ENDPART C
