.\" BEGINPART B
.SH FILE NAME EXPANSION
When the save commands prompts for a file name, the following file
name expansions are performed on the file name you enter:
.TP
\fB+\fP\fIfolder\fP
The
.B +
is replaced by the contents of the
.B folder
variable (default value "~/News/") resulting in the name of a file in the
.I folder
.IR directory .
Examples:
.nf
	+emacs, +nn, +sources/shar/nn
.fi
.TP
\fB+\fP
A single plus is replaced by the expansion of the file name contained in the
.B default-save-file
variable (or by \fBfolder-save-file\fP when saving from a folder).
.TP
\fB~/\fP\fIfile\fP
The
.B ~
is replaced by the contents of the environment variable HOME, i.e. the
path name of your home directory.
Examples:
.nf
	~/News/emacs, ~/News/nn, ~/src/shar/nn
.fi
.TP
\fB~\fP\fIuser\fP\fB/\fP\fIfile\fP
The \fB~\fP\fIuser\fP part is replaced by the \fIuser\fP's home
directory as defined in the /etc/passwd file.
.TP
\fB|\fP\fIcommand-line\fP
Instead of writing to a file, the articles are piped to the given
shell (/bin/sh) command-line.  Each save or write command will create a
separate pipe, but all articles saved or written in one command (in
selection mode) are given
as input to the same shell command.  Example:
.nf
	| pr | lp
.fi
This will print the articles on the printer after they have been piped
through pr.
    It is possible to create separate pipes for each saved article by
using a double pipe symbol in the beginning of the command, e.g.
.nf
	|| cd ~/src/nn ; patch
.fi
.LP
The following symbols are expanded in a file name or command:
.TP
.B $F
will be expanded to the name of the current group with the periods
replaced by slashes, e.g. rec/music/synth.
.TP
.B $G
will be expanded to the name of the current group.
.TP
.B $L
will be expanded to the \fIlast component\fP of the name of the
current group.  You may use this to create default save file names
like +src/$L in the comp.sources groups.
.TP
.B $N
will be expanded to the (local) article number, e.g. 1099.  In
selection mode it is only allowed at the end of the file name!
.TP
.B $(VAR)
is replaced by the string value of the environment variable \fIVAR\fP.
.LP
Using these symbols, a simple naming scheme for `default folder name' is
.B +$G
which will use the group name as folder name.  Another possibility is
.BR +$F/$N .
.LP
As mentioned above, you can also instruct \fInn\fP to save a series of
files in separate, unique files.  All that is required is that the
file name contains an asterisk, e.g.
.nf
	+src/hype/part*.shar
.fi
This will cause each of the articles to be saved in separate, unique
files named part1.shar, part2.shar, and so on, always choosing a part
number that results in a unique file name (i.e. if part1.shar did
already exist, the first article would be saved in part2.shar, the
next in part3.shar, and so on).
.LP
\fBRelated variables\fP:
default-save-file, folder, folder-save-file, save-counter, save-counter-offset.
.SH FILE AND GROUP NAME COMPLETION
When entering a file name or a news group name, a simple
.B completion
feature is available using the \fBspace\fP, \fBtab\fP, and \fB?\fP keys.
.LP
Hitting \fBspace\fP anywhere during input will complete the
.I current
.I component
of the file name or group name with the
.I first
available possibility.
.LP
If this possibility is not the one you want, keep on hitting
.B space
until it appears.
.LP
When the right completion has appeared, you can just continue typing
the file or group name, or you can hit
.B tab
to fix the current component, and get the
.I first
possibility for the next component, and then use
.B space
to go through the other possible completions.
.LP
The
.B ?
key will produce a list of the possible
.I completions
of the current component.  If the list is too long for the available
space on screen, the key can be repeated to get the next part of the
list.
.LP
The current completion can be deleted with the
.B erase
key.
.LP
The default value for a file name is the last file name you have
entered, so if you enter a
.B space
as the first character after the prompt, the last file name will be
repeated (and you can edit it if you like).  In some cases, a string
will already be written for you in the prompt line, and to get the
default value in these cases, use the \fBkill\fP key.  This also means
that if you neither want the initial value, nor the default value, you
will have to hit the \fBkill\fP twice to get a clean prompt line.
.LP
\fBRelated variables\fP:
comp1-key, comp2-key, help-key, suggest-default-save.
.SH POSTING AND RESPONDING TO ARTICLES
In both selection mode and reading mode you can post new articles,
post follow-ups to articles, send replies to the author of an article,
and you can send mail to another user with the option of including an
article in the letter.  In reading mode, a response is made to the
current article, while in selection mode you will be prompted for an
article to respond to.
.LP
The following commands are available (the lower-case equivalents are
also available in reading mode):
.TP
\&\fBR\fP	{\fBreply\fP}
Reply through mail to the author of the article.  This is the preferred
way to respond to an article unless you think your reply is of general
interest.
.TP
\&\fBF\fP	{\fBfollow\fP}
Follow-up with an article in the same newsgroup (unless an alternative
group is specified in the article header).
The distribution of the follow-up is normally the same as the original
article, but this can be modified via the \fBfollow-distribution\fP
variable.
.TP
\&\fBM\fP	{\fBmail\fP}
Mail a letter or
.I forward
an article to a single recipient.
In selection mode, you will be prompted for an article to include
in your letter, and in reading mode you will be asked if the current
article should be included in the letter.
You will then be prompted for the recipient of the letter (default
recipient is yourself)
and the subject of the letter (if an article is included, you may hit
.B space
to get the default subject which is the subject of the included article).
  The header of the article is only included in the posted letter if
it is forwarded (i.e. not edited), or if the variable
\fBinclude-full-header\fP is set.
.TP
\&\fB:post\fP	{\fBpost\fP}
Post a new article to any newsgroup.  This command will prompt you for
a
.I comma-separated
list of newsgroups to post to (you cannot enter a space because
.B space
is used for group name completion as described below).
  If you enter \fB?\fP {\fBhelp-key\fP} as the first key, \fInn\fP
will show you a list of all available news groups and their purpose.
While paging through this list, you can enter \fBq\fP to quit looking
at the list.  You can also enter \fB/\fP followed by a regular
expression (typically a single word) which will cause \fInn\fP to show
a (much shorter) list containing only the lines matching the regular
expression.
   Normally, you will be prompted for the distribution of the article
with the default take from \fBdefault-distribution\fP, but this can be
changed via the \fBpost-distribution\fP variable.
.LP
Generally, \fInn\fP will construct a file with a suitable header, optionally
include a copy of the article in the file with each non-empty line
prefixed by a `>' character (except in mail mode), and invoke an
editor of your choice (using the EDITOR environment variable) on this
file, positioning you on the first line of the body of the article (if
it knows the editor).
.PP
When you have completed editing the message, it will compare it to the
unedited file, and if they are identical (i.e. you did not make any
changes to the file), or it is empty, the operation is cancelled.
Otherwise you will be prompted for an action to take on the
constructed article (enter first letter followed by \fBreturn\fP, or
just \fBreturn\fP to take the default action):
.sp 0.5v
.nf
    a)bort c)c e)dit h)old i)spell m)ail p)ost r)eedit s)end v)iew w)rite 7)bit
    Action: (post article)
.fi
.sp 0.5v
You now have the opportunity to perform one of the following actions:
.LP
.in +2m
.ta 5m
.\"ta 4 9
\fBa\fP	throw the response away (will ask for confirmation),
.br
\fBc\fP	mail a copy of a \fIfollow-up\fP to the poster of the article,
.br
\fBe\fP	edit the file again,
.br
\fBh\fP	hold response for later completion,
.br
\fBi\fP	run an (interactive) \fBspell-checker\fP on the text,
.br
\fBm\fP	mail a (blind) copy to a specified recipient,
.br
\fBn\fP	same as \fBa\fPbort (\fIno\fP don't post),
.br
\fBp\fP	post article (same as \fBs\fPend),
.br
\fBr\fP	throw away the edited text and edit the original text,
.br
\fBs\fP	send the article or letter,
.br
\fBv\fP	view the article (through the \fBpager\fP),
.br
\fBw\fP	append it to a file (before you send it),
.br
\fBy\fP	confirm \fIdefault answer\fP (e.g. \fIyes\fP post it), or
.br
\fB7\fP	strip the high-order bit from all characters in the message
.in -2m
.DT
.LP
If you have selected a 7-bit character set (this is determined by the
values of the \fBcharset\fP and \fBdata-bits\fP variables), \fInn\fP
will not allow you to post an article or send a letter whose body
contains characters with the high-order bit set. It will warn you after
you have first edited the message and disable the c)c, m)ail,
p)ost, s)end and y)es actions. You can then either e)dit the message
to delete those characters, use 7)bit to strip the high-order bits,
a)bort the message, or h)old it and select an 8-bit character set from
\fInn\fP.
.LP
To complete an unfinished response saved by the h)old command, simply
enter any response action, e.g. \fBR\fP {\fBreply\fP}.  This will
notice the unfinished response and ask you whether you want to
complete it now.  Only one unfinished response can exist at a time.
Notice that the $A environment variable may no longer be valid as a
path to the original article when the response is completed.
.LP
If your message contains 8-bit characters, the \fBcharset\fP variable
is not set to "unknown" and the message does not already have a
\fBMIME-Version\fP or \fBContent-\fP\fIXXX\fP header, \fInn\fP will add
the following headers to your message before sending it:
.sp 0.5v
.nf
	MIME-Version: 1.0
	Content-Type: text/plain; charset=\fIcharset\fP
	Content-Transfer-Encoding: 8bit
.fi
.sp 0.5v
It must be noted that sending 8-bit characters over the current news
and mail networks is risky at best; although large parts of the network
will pass through such characters unchanged, high-order bits may occasionally
be stripped. Although the MIME standard provides solutions for this by
encoding the characters, this is not yet supported by \fInn\fP.
Adding the above headers is an interim solution that is compatible with
current practice and is much better than just sending the message without
any hints about the character set used.
.LP
\fBRelated variables\fP:
append-signature-mail, append-signature-post, charset, data-bits,
default-distribution, follow-distribution, post-distribution,
edit-response-check, editor, include-art-id, include-full-header,
included-mark, mail-header, mail-record, mail-script, mailer,
mailer-pipe-input, news-header, news-record, news-script,
orig-to-include-mask, pager, query-signature,
record, response-check-pause, response-default-answer,
save-counter, save-counter-offset, save-report, spell-checker.
.SH JUMPING TO OTHER GROUPS
By default \fInn\fP will present the news groups in a predefined
sequence (see the section on Presentation Sequence later on).
To override this sequence and have a look at any other group the
.B G
{\fBgoto-group\fP} command available in both selection and reading
mode enables you to move freely between all the newsgroups.
.LP
Furthermore, the
.B G
command enables you to open folders and other files, to read old
articles you have read before, and to grep for a specific subject in a
group.
.PP
It is important to notice that normally the goto command is recursive,
i.e. a new \fImenu level\fP is created when the specified group or
folder is presented, and when it has been read, \fInn\fP will continue
the activity in the group that was presented before the goto command
was executed.  However, if there are unread articles in the target
group you can avoid entering a new menu level by using the
.B j
reply described below.  The current menu level (i.e. number of nested
goto commands) will be shown in the prompt line as "<N>" (in reverse
video).
.PP
The goto command is very powerful, but unfortunately also a little bit
tricky at first sight, because the facilities it provides depend on
the context in which the command is used.
.PP
When executed, the goto command will prompt you for the name of the
newsgroup, folder, or file to open.  It will use the first letter
you enter to distinguish these three possibilities:
.TP
.B return
An empty answer is equivalent to the current newsgroup.
.TP
\fIletter\fP
The answer is taken to be the name of a newsgroup.
If a news group with the given name does not exist, \fInn\fP will
treat the answer as a regular expression and locate the first group
in the presentation sequence (or among all groups) whose name matches
the expression.
.TP
.I +
.br
The answer is taken to be the name of a folder.  If only `+' is
entered, it is equivalent to the default save file for the current
group.
.TP
\fI\&/ or ./ or ~/\fP
The answer is taken to be the name of a file, either relative to the
current directory, relative to your home directory, or an absolute
path name for the file.
.TP
.B %
In reading mode, this reply corresponds to reading the current article
(and splitting it as a digest).  In selection mode, it will prompt for
an article on the menu to read.
.TP
.B @
This choice is equivalent to the archive file for the current group.
\fInnmaster\fP maintains archive files with all old and current
articles for the groups which have the auto-archive option set in the
GROUPS file (see \fInnmaster\fP(8)).
.TP
\fB=\fP and \fInumber\fP
These answers are equivalent to the same answers described below
applied to the current group (e.g. \fBG return =\fP and \fBG =\fP are
equivalent).
.LP
Specifying a folder, a file, or an article (with \fB%\fP) will cause
\fInn\fP to treat the file like a digest and split it into separate
articles (not physically!)  which are then presented on a menu in the
usual way, allowing you to read or save individual subarticles from
the folder.
.LP
When you enter a group name, \fInn\fP will ask you how many articles
in the group you want to see on the menu.  You can give the following
answers:
.TP
.I a number N
In this case you will get the newest N articles in the group, or if
you specified the current group (by hitting \fBreturn\fP to the group
name prompt or entering the number directly), you will get that many
\fIextra\fP articles included on the same menu (without creating a new
menu level).
.TP
.B j
This answer can only be given if there are unread articles in the
group.  It will instruct nn to jump directly to the specified group in
the presentation sequence \fIwithout\fP creating a new menu level.
.TP
.B u
This instructs \fInn\fP to present the \fIunread\fP articles in the
group (if there are any).  If you have already read the group (in the
current invocation of \fInn\fP), the \fBu\fP answer will instruct
\fInn\fP to present the articles that were unread when you entered
\fInn\fP.
.TP
.B a
This instruct \fInn\fP to present \fBall\fP articles in the group.
.TP
\fBs\fP\fIword\fP or \fB=\fP\fIword\fP
This instructs \fInn\fP to search \fIall\fP articles in the groups,
but only present the articles containing the word \fIword\fP in the
subject.  Notice that case is ignored when searching for the word in
the subject lines.
.TP
\fBn\fP\fIword\fP
Same as the \fBs\fP form except that it searched for articles where
the sender \fIname\fP matches \fIword\fP.
.TP
\fBe\fP\fIword\fP
Same as the \fBs\fP form except that it Psearched for articles where
\fIeither\fP the subject or the sender name matches \fIword\fP.
.TP
\fIword\fP = \fB/\fP\fIregexp\fP
When the first character of the \fIword\fP specified with the \fBs\fP,
\fBn\fP, and \fBe\fP forms is a slash `/', the rest of the input is
interpreted as a regular expression to search for.  Notice that
regular expression matching is case insensitive when
\fBcase-fold-search\fP is set (default).
.TP
.B return
The meaning of an empty answer depends on the context: if there are
unread articles in the specified group the unread articles will be
presented, otherwise \fIall\fP articles in the group will be included
in the menu.
.LP
If you specified the current group, and the menu already contains all
the available articles, \fInn\fP will directly prompt for a word to
search for in the subject of all articles (the prompt will be an equal
sign.)
.LP
When the goto command creates a new menu level, \fInn\fP will not
perform auto kill or selection in the group.  You can use the \fB+\fP
command in menu mode to perform the auto-selections.
.LP
There are three commands in the goto family:
.TP
\&\fBG\fP	{\fBgoto-group\fP}
This is the general goto command described above.
.TP
\&\fBB\fP	{\fBback-group\fP}
Backup one or more groups.  You can hit this key one or more times to
go back in the groups already presented (including those without new
articles); when you have found the group you are looking for, hit
\fBspace\fP to enter it.
.TP
\&\fBA\fP	{\fBadvance-group\fP}
Advance one or more groups.  This command is similar to the \fBB\fP
command, but operates in the opposite direction.
.TP
\&\fBN\fP	{\fBnext-group\fP}
When used within an \fBA\fP or \fBB\fP command, it skips forward to
the next group in the sequence with unread articles or which has
previously been visited.
.TP
\&\fBP\fP	{\fBprevious\fP}
When used within an \fBA\fP or \fBB\fP command, it skips backwards to
the preceding group in the sequence with unread articles or which has
previously been visited.
.LP
Once you have entered an \fBA\fP or \fBB\fPcommand, you can freely mix
the \fBA\fP, \fBB\fP, \fBP\fP, and \fBN\fP commands to find the group
you want, and you can also use the \fBG\fP command to be prompted for
a group name.
.LP
To show the use of the goto command some typical examples on
its use are given below:
.sp
.nf
.I "Present the unread articles in the dk.general group"
.sp 0.5v
 	\fBG\fP dk.general \fBreturn\fP \fBu\fP
.sp
.I "Jump directly to the gnu.emacs group and continue from there"
.sp 0.5v
 	\fBG\fP gnu.emacs \fBreturn\fP \fBj\fP
.sp
.I "Include the last 10 READ articles in the current group menu"
.sp 0.5v
 	\fBG\fP 10 \fBreturn\fP
.sp
.I "Find all articles in rec.music.misc on the subject Floyd"
.sp 0.5v
 	\fBG\fP rec.music.misc \fBreturn\fP
 	\fB=\fP floyd \fBreturn\fP
.sp 0.5v
.sp
.I "Open the folder +nn"
.sp 0.5v
 	\fBG\fP +nn \fBreturn\fP
.sp
.I "Split current article as a digest (in reading mode)"
.sp 0.5v
 	\fBG\fP \fB%\fP

.fi
.LP
\fBRelated variables\fP:
case-fold-search, default-save-file, folder-save-file
.SH AUTOMATIC KILL AND SELECTION
When there is a subject or an author which you are either very
interested in, or find completely uninteresting, you can easily
instruct \fInn\fP to \fIauto-select\fP or \fIauto-kill\fP articles
with specific subjects or from specific authors.  These instructions
are stored in a \fIkill file\fP, and the most common types of entries
can be created using the following command:
.TP
\&\fBK\fP	{\fBkill-select\fP}
Create an entry in your personal kill file.  The contents of the entry
is specified during a short dialog that is described in details below.
This command is available in both selection and reading mode.
.LP
Entries in the kill file may apply to a single newsgroup or to all
newsgroups.  Furthermore, entries may be permanent or they may be
expired a given number of days after their entry.
.LP
To increase performance, \fInn\fP uses a compiled version of the kill
file which is read in when \fInn\fP is invoked.  The compiled kill
file will automatically be updated if the normal kill file has been
modified.
.LP
The following dialog is used to build the kill file entry:
.TP
\fIAUTO (k)ill or (s)elect (CR => Kill subject 30 days)\fP
If you simply want \fInn\fP to kill all articles with the subject of
the current article (in reading mode) or a specific article (which
\fInn\fP will prompt for in selection mode), just hit \fBreturn\fP.
This will cause \fInn\fP to create an entry in the kill file to kill
the current (or specified) subject in the current group for a period
of 30 days (which should be enough for the discussion to die out).
.sp 0.5v
You can control the default kill period, or change it into a "select"
period via the \fBdefault-kill-select\fP variable.
.sp 0.5v
If this "default behaviour" is not what you want, just answer either
\fIk\fP or \fIs\fP to kill or select articles, respectively, which
will bring you on to the rest of the questions.
.TP
\fIAUTO SELECT on (s)ubject or (n)ame  (s)\fP
(The \fISELECT\fP will be substituted with \fIKILL\fP depending on the
previous answer).  Here you specify whether you want the kill or
select to depend on the subject of the article (\fBs\fP or
\fBspace\fP), or on the name of the author (\fBn\fP).
.TP
\fISELECT NAME:\fP
(Again \fISELECT\fP may be substituted with \fIKILL\fP and
\fISUBJECT\fP may replace \fINAME\fP).  You must now enter a name (or
subject) to select (or kill).  In reading mode, you may just hit
\fBreturn\fP (or \fB%\fP) to use the name (or subject) of the current
article.  In selection mode, you can use the name (or subject) from an
article on the menu by answering with \fB%\fP followed by the
corresponding article identifier.
.sp 0.5v
When the name or subject is taken from an article (the current or one
from the menu), \fInn\fP will only select or kill articles where the
name or subject matches the original name or subject exactly including
case.
.sp 0.5v
If the first character typed at the prompt is a slash `/', the rest of
the line is used as a \fIregular expression\fP which is used to match
the name or subject (case \fIin\fPsensitive).
.sp 0.5v
Otherwise, \fInn\fP will select or kill articles which \fIcontain\fP
the specified string anywhere in the name or subject (ignoring case).
.TP
\fISELECT in (g)roup `dk.general' or in (a)ll groups  (g)\fP
You must now specify whether the selection or kill should apply to the
current group only (\fBg\fP or \fBspace\fP) or to all groups (\fBa\fP).
.TP
\fILifetime of entry in days (p)ermanent  (30)\fP
You can now specify the lifetime of the entry, either by entering a
number specifying the number of days the entry should be active, or
\fBp\fP to specify the entry as a permanent entry.  An empty reply is
equivalent to 30 days.
.TP
\fICONFIRM SELECT ....\fP
Finally, you will be asked to confirm the entry, and you should
especially note the presence or absence of the word \fBexact\fP which
specify whether an exact match applies for the entry.
.LP
\fBRelated variables\fP:
default-kill-select, kill.
.SH THE FORMAT OF THE KILL FILE
The kill file consists of one line for each entry.  Empty lines and
lines starting with a # character are ignored.  \fInn\fP automatically
places a # character in the first position of expired entries when it
compiles the kill file.  You can then edit the kill file manually from
time to time to clean out these entries.
.LP
Each line has the following format
.nf
	[\fIexpire time\fP :] [\fIgroup name\fP] : \fIflags\fP : \fIstring\fP [: \fIstring\fP]...
.fi
.LP
Permanent entries have no \fIexpire time\fP (in which case the colon
is omitted as well!).  Otherwise, the \fIexpire time\fP defines the
time (as a time_t value) when the entry should be expired.
.LP
The \fIgroup name\fP field can have three forms:
.TP
\fInews.group.name\fP
If it is the name of a single news group (e.g. comp.unix), 
the entry applies to that group only.
.TP
\fB/\fP\fIregular expression\fP
If it starts with a slash `/' followed by a \fIregular expression\fP
(e.g. /^news\e..*), the entry applies to all groups whose name are
matched by the regular expression.
.TP
\fIempty\fP
An empty group field will apply the entry to \fIall\fP groups.
.LP
The \fIflags\fP field consists of a list of characters which
identifies the type of entry, and the interpretation of each
\fIstring\fP field.  When used, the flag characters must be used in
the order in which they are described below:
.TP
\fB~\fP	(optional)
.br
When this flag is present on any of the entries for a specific group,
it causes all entires which \fIare not auto-selected\fP to be killed.
This is a simple way to say: I'm interested in this and that, but
nothing else.
.TP
\fB+\fP	or \fB!\fP (optional)
.br
Specify an auto-select \fB+\fP or an auto-kill \fB!\fP entry,
respectively.  If neither are used, the article is neither selected
nor killed which is useful in combination with the `\fB~\fP' flag.
.TP
\fB>\fP (optional)
When used with a subject (flag \fBs\fP), the kill entry only matches
follow-ups to that subject (i.e. where the Subject: line starts with
Re:).  For example, to kill all "Re:"'s in rec.humor use the following
kill entry: rec.humor:!>s/:.
.TP
\fB<\fP (optional)
When used with a subject (flag \fBs\fP), the kill entry only matches
base articles with that subject (i.e. where the Subject: line does not
start with Re:).  For example, to kill all articles asking for help
(but not follow-ups) in the tex group, add this to your kill file:
.nf
	comp.text.tex:!s</:^HELP
.fi
.TP
\fBn\fP or \fBs\fP or \fBa\fP (mandatory)
.br
Specify whether the corresponding string applies to the name \fBn\fP
or to the subject \fBs\fP of an article.  If flag \fBa\fP is used, the
corresponding string is ignored (but must be present), and the entry
applies to articles with a non-empty References: line.
.TP
\fB/\fP (optional)
.br
Specifies that the corresponding \fIstring\fP is a \fBregular expression\fP
which the sender or subject is matched against.  If not specified, a simple
string match is performed using the given \fIstring\fP.
.TP
\fB=\fP (optional)
.br
Specifies that the match against the name or subject is \fIcase
sensitive\fP.  Furthermore, when regular expression matching
is \fInot\fP used, the name or subject must be of the same length
of the \fIstring\fP to match.
Otherwise, the match will be case insensitive, and a \fIstring\fP may
occur anywhere in the name or subject to match.
.TP
\fB|\fP or \fB&\fP (mandatory if multiple strings)
.br
If more than one string is specified, the set of \fIflags\fP
corresponding to each \fIstring\fP must be separated by either an
\fIor operator\fP `\fB|\fP' or an \fIand operator\fP `\fB&\fP'.  The
and operator has a higher precedence than the or operator, e.g.  a
complex match expression \fIa|b&c|d\fP will succeed if either of
\fIa\fP, \fIb&c\fP, or \fId\fP matches.
.LP
The \fIstring\fP field in the entry is the name, subject or regular
expression that will be matched against the name or subject of each
article in the group (or all groups).  Colons and backslashes must be
escaped with a backslash in the string.
.LP
Example 1:  Auto-select articles from `Tom Collins' (exact) on subject
`News' in all groups:
.sp 0.5v
    :+n=&s:Tom Collins:News
.sp
Example 2:  Kill all articles which are neither from `Tom' or `Eve' in
some.group.  Select only articles from Eve:
.sp 0.5v
    some.group:~n:Tom
.br
    some.group:+n:Eve
.sp
The second example can also be written as a single entry with an or
operator (in this case, the select/kill attribute only applies to the succeeding strings):
.br
    some.group:~n|+n:Tom:Eve
.LP
To remove expired entries, to "undo" a \fBK\fP command, and to make
the more advanced entries with more than one string, you will have to
edit the kill file manually.  To recompile the file, you can use the
\fB:compile\fP command.  When you invoke \fInn\fP, it will also
recompile the kill file if the compiled version is out of dat.
.SH SHELL ESCAPES
The
.B !
commands available in selection and reading mode are identical in
operation (with one exception).  When you enter the shell escape
command, you will be prompted for a shell command.  This command will
be fed to the shell specified in the \fBshell\fP variable (default
loaded from the SHELL environment variable or /bin/sh) after the
following substitutions have been performed on the command:
.TP
\fIFile name expansion\fP
The earlier described file name expansions will be performed on all
arguments.
.TP
.B $G
will be substituted with the name of the current news group.
.TP
.B $L
will be substituted with the \fIlast component\fP of the name of the
current news group.
.TP
.B $F
will be substituted with the name of the current news group with the
periods replaced by slashes.
.TP
.B $N
will be substituted with the (local) article number (only defined in
reading mode).
.TP
.B $A
is replaced by the full path name of the file containing the current article
(only defined in reading mode).
.TP
.B %
Same as $A.
.TP
.B $(VAR)
is replaced by the string value of the environment variable \fIVAR\fP.
.LP
When the shell command is completed, you will be asked to hit any key
to continue.  If you hit the
.B !
key again, you will be prompted for a new shell command.  Any other
key will redraw the screen and return you to the mode you came from.
.LP
\fBRelated variables\fP:
shell, shell-restrictions.
.SH MISCELLANEOUS COMMANDS
Below are more useful commands which are available in both
selection and reading modes.
.TP
\&\fBU\fP	{\fBunsub\fP}
Unsubscribe to the current group.  You will not see this group
any more unless you explicitly request it.  If the variable
\fBunsubscribe-mark-read\fP is set, all articles in the group will be
marked read when you unsubscribe.
  If the variable \fBkeep-unsubscribed\fP is not set, the group will
be removed from .newsrc.  If you are not subscribing to the group, you
will be given the possibility to \fIresubscribe\fP to the group!  This
may be used in connection with the \fBG\fP command to resubscribe a
group.
.TP
\&\fBC\fP	{\fBcancel\fP}
Cancel (delete) an article in the current group or folder.  Cancelling
articles in a folder will cause the folder to be rewritten when it is
closed.  In selection mode, you will be prompted for the identifier of
the article to cancel.  Normal users can only cancel their own
articles.
See also the section on folder maintenance.
.TP
\&\fBY\fP	{\fBoverview\fP}
Provide an overview of the groups with unread articles.
.TP
\&\fB"\fP	{\fBlayout\fP}
Change menu layout in selection mode.  The menu will be redrawn using
the next layout (cycling through ..., 2, 3, 4, 0, 1, ...)
.LP
Most of the commands in \fInn\fP are bound to a key and can be activated
by a single keystroke.  However, there are a few commands that
cannot be bound to a key directly.
.LP
As shown in the keystroke command descriptions, all commands have a
name, and it is possible to activate a command by name with the
\fIextended command\fP key (\fB:\fP).  Hitting this key will prompt
you for the name of a command (and parameters).  For example, an
alternative to hitting the \fBR\fP key to reply to an article is to
enter the extended command \fB:reply\fP followed by \fBreturn\fP.  The
\fB:post\fP and \fB:unshar\fP commands described earlier can also be
bound to a key.  The complete list of commands which can be bound to
keys is provided in the section on Key Mappings below.
.LP
The following extended commands \fIcannot\fP be bound to a key, mainly
because they require additional parameters on the prompt line, or
because it should not be possible to activate them too easily.
.TP
\fB:admin\fP
Enter administrative mode.  This is identical in operation to the
.IR nnadmin (1M)
program.
.TP
\fB:bug\fP
Prepare and send a bug report to the nn-bugs mailing address.
.TP
\fB:cd\fP [ \fIdirectory\fP ]
Change current working directory.  If the directory argument is not provided,
\fInn\fP will prompt for it.
.TP
\fB:clear\fP
Clear the screen (without redraw).  This may be useful at the
beginning of the init file (possibly guarded by "on program nn"), or
in some macros.
.TP
\fB:compile\fP
Recompile the \fIkill\fP file.  This is not necessary under normal
operation since \fInn\fP automatically compiles the file on start-up
if it has changed, but it can be used if you modify the kill file
while \fInn\fP is suspended.
.TP
\fB:coredump\fP
Abort with a core dump.  For debugging purposes only.
.TP
\fB:define\fP \fImacro\fP
Define macro number \fImacro\fP as described in the Macro Definition
section below.  If \fImacro\fP is omitted, the next free macro number
will be chosen.
.TP
\fB:dump\fP \fItable\fP
Same as the \fB:show\fP command described below.
.TP
\fB:help\fP [ \fIsubject\fP ]
Provide online help on the specified subject.  If you omit the
subject, a list of the available topics will be given.
.TP
\fB:load\fP [ \fIfile\fP ]
Load the specified \fIfile\fP.  If the \fIfile\fP argument is omitted,
the init file is reloaded.
The \fBsequence\fP part (if present) is ignored.
.TP
\fB:local\fP \fIvariable\fP [ \fIvalue\fP ]
Make the variable local to the current group.  Subsequent changes to
the variable will only be effective until the current group is left.
If a value is specified, it will be assigned to the local variable.
To assign a new value to a boolean variable, the values \fBon\fP and
\fBoff\fP must be used.
.TP
\fB:lock\fP \fIvariable\fP
Lock the specified \fIvariable\fP so it cannot be modified.
.TP
\fB:man\fP
Call up the online manual.  The manual is presented as a normal folder
with the program name in the `From' field and the section title in the
\&`subject' field.  All the normal commands related to a folder works
for the online manual as well, e.g. you can save and print sections of
the manual.
.TP
\fB:map\fP \fIarguments\fP
This is the command used for binding commands to the keys.  It is
fully described in the Key Mapping section below.
.TP
\fB:mkdir\fP [ \fIdirectory\fP ]
Create the directory (and the directories in its path).  It will
prompt for at directory name if the argument is omitted.
.TP
\fB:motd\fP
Show the \fImessage of the day\fP (maintained by the news
administrator in the file "motd" in the lib directory.  This file is
automatically displayed on start-up whenever it changes if the
\fBmotd\fP variable is set.
.TP
\fB:pwd\fP
Print path name of current working directory on message line.
.TP
\fB:q\fP
Has no effect besides redrawing the screen if necessary.  If an
extended command (one which is prefixed by a :) produces any output
requirering the screen to be redrawn, the screen will not be redrawn
immediately if the variable \fBdelay-redraw\fP is set (useful on
slow terminals).  Instead another \fB:\fP prompt is shown to allow you
to enter a new extended command immediately.  It is sufficient to hit
.B return
to redraw the screen, but it has been my experience that entering
.B q return
in this situation happens quite often, so it was made a no-op.
.TP
\fB:q!\fP
Quit \fInn\fP without updating the \fB.newsrc\fP file.
.TP
\fB:Q\fP
Quit \fInn\fP.  This is equivalent to the normal
.B Q
command.
.TP
\fB:rmail\fP
Open your mailbox (see the \fBmail\fP variable) as a folder to
read the incoming messages.  This is \fInot\fP a full mail interface
(depending on the nn configuration, you may not be able to delete
messages, add cc: on replies, etc), but it can give
you a quick glance at new mail without leaving \fInn\fP.
.TP
\fB:set\fP \fIvariable\fP [ \fIvalue\fP ]
Set a boolean variable to true or assign the value to a string or
integer variable.  The
.B :set
command is described in details in the section on VARIABLES.
.TP
\fB:sh\fP
Suspend \fInn\fP, or if that is not possible, spawn an interactive shell.
.TP
\fB:show groups\fP \fImode\fP
Show the total number or the number of unread articles in the current
group, depending on \fImode\fP: \fBall\fP (list the number of unread
articles in all groups including groups which you have unsubscribed
to), \fBtotal\fP (list the total number of articles in all existing
groups), \fBsequence\fP (list unread groups in presentation sequence
order), \fBsubscr\fP (list all subscribed
groups), \fBunsub\fP (list unsubscribed groups only).  Any other
\fImode\fP results in a listing of the number of unread articles in
all subscribed groups including those you have suppressed with the `!'
symbol in the group presentation sequence.  To get just the currently
unread groups in the presentation sequence, use the `Y'
{\fBoverview\fP} command.
.TP
\fB:show kill\fP
Show the kill entries that applies to the current group and to all groups.
.TP
\fB:show rc\fP [ \fIgroup\fP ]
Show the .newsrc and select file entries for the current or the
specified group.
.TP
\fB:show map\fP [ \fImode\fP ]
Show the key bindings in the current or specified mode.
.TP
\fB:sort\fP [ \fImode\fP ]
Reorder the articles on the menu according to \fImode\fP or if omitted
to the default \fBsort-mode\fP.  The following sorting modes are
available:
.br
\fBarrival\fP: list articles by local article number which
will be the same as the order in which they
arrived on the system (unless groups are merged),
.br
\fBsubject\fP: articles with identical
subjects are grouped and ordered after age of the oldest article in
the group,
.br
\fBlexical\fP: subjects in lexicographical order,
.br
\fBage\fP: articles ordered after posting date only,
.br
\fBsender\fP: articles ordered after sender's name.
.TP
\fB:toggle\fP \fIvariable\fP
Toggle a boolean variable.
.TP
\fB:unread\fP [ \fIgroup\fP ] [ \fIarticles\fP ]
Mark the current (or specified) group as unread.  If the
\fIarticles\fP argument is omitted, the number of unread articles in
the group will be set to the number of unread articles when \fInn\fP
was invoked.  Otherwise, the argument specifies the number of unread
articles.
.TP
\fB:unset\fP \fIvariable\fP
Set a boolean variable to false or clear an integer variable.
.TP
\fB:x\fP
Quit \fInn\fP and \fBmark\fP all articles in the current group as
\fIread\fP!
.LP
\fBRelated variables\fP:
backup, bug-report-address, delay-redraw, keep-unsubscribed,
unsubscribe-mark-read, mail, pager, sort-mode.
.\" ENDPART B
