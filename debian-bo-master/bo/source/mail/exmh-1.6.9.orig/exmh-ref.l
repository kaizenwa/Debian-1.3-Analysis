'	# DS - begin display
.de DS
.RS
.nf
.sp
..
'	# DE - end display
.de DE
.fi
.RE
.sp .5
..
.TH "EXMH REFERENCE" 1 "August 22, 1996" "Exmh 1.6.9" "EXMH REFERENCE"
.SH NAME
exmh-ref \- A detailed reference to exmh functions.
.SH "EXMH REFERENCE CHAPTER"
.PP
This man page
gives complete, but terse, information about
\fIExmh\fP.
.SH "COMMAND LINE OPTIONS"
.PP
There are just a handful of command line options for exmh.
Note that Xt-like command line options are not supported because
\fIexmh\fP is based on Tk, not Xt.
.IP "\fB\-bgAction \fIaction\fR" 15
Override background processing setting.
Valid actions are: off, count, msgchk, flist, and inc.
.IP "\fB\-bgPeriod \fIminutes\fR" 15
Override background period setting.
.IP "\fB\-geometry \fIgeometry\fR" 15
Initial geometry to use for the window.  Exmh remembers the geometry
when you change it interactively, so you do not really need to
specify it each time on the command line.
.IP "\fB\-iconposition \fIgeometry\fR" 15
Initial position to use for the icon in the form +\fIxoff\fP+\fIyoff\fP.
You can use minus signs instead of + to position relative to the right
and bottom of the screen.
.IP "\fB\-iconic\fR" 15
Begin execution iconic.
.IP "\fB\-sedit\fR" 15
Override the editor command defined in Preferences and use
the built-in editor (sedit) instead.
.IP \fIfolder\fP
If the argument is not one of the flags given above, it is assumed to
be the name of a folder.  In this case \fIexmh\fP begins in that folder.
.BE
.SH EXMH MAIN BUTTONS
.PP
There are three sets of buttons in exmh, the
main, or top row, folder operations, and message operations.
The main set has global commands: Log, Help..., Bindings..., Aliases, Preferences, and Quit.
.IP "Help..."
Help is a menu with several choices:
.IP "Quick Intro"
This provides a useful sub-set of this man page.
.IP "Color Legend"
This provides a highlighting key for color or monochrome screens.
.IP "Frequently Asked Questions"
This displays a Frequently-Asked-Questions file.
It has the same info as the help.FAQ file.
.IP "Exmh Mailing Lists..."
This is a sub-menu that lets you administer your membership in the
exmh-related mailing lists.  See the About menu entry for an
explanation of the various mailing lists.  The add/remove operations
result in mail messages.  You'll need to Send the draft messages.
.IP "Submit Bug Report"
A mail message is started so you can report a problem with exmh.
.IP "Register as an EXMH User"
A mail message is started to you can register as an exmh user.
There is an opportunity to sign up for the mailing lists and
provide positive and negative feedback about exmh.
.IP "Pgp Setup"
Get started with PGP.  There is some help text and an interface
to create your public key / private key pair.
.IP "Bindings..."
Exmh has two sets of key bindings.  One for simple editting,
and another to invoke commands.  The editting bindings are used in
the simple editor and in the various text entry widgets.
The command bindings are in effect otherwise, and they allow you
to invoke various Exmh commands without using buttons or menus.
This is a menu with three choices:
.IP Commands
This brings up a dialog that lets you edit the keystroke bindings.
All of the defined bindings are displayed, and you can edit their
associated keystroke in place.  Use the entry to the right of each
command to alter or add key bindings.  There are further directions
in the dialog box.
.IP "Simple Edit"
These bindings result in class definitions for the
Entry and Text widget classes.
The
default bindings are Emacs-like.
See the Tk man page on the bind command for details about the syntax,
if you need to.  In Tk versions 3.3 and later, "Meta" is an ok
synonym for "Mod1". To emulate Emacs, any keystroke that is defined
to be <Meta-\fIx\fP> will also result in a binding for <Escape>\fIx\fP.
.IP "Compose Key"
The compose key is used to insert 8-bit characters.
This dialog shows you the key sequences used to get special characters.
You can define which key maps to Compose in the Simple Edit bindings dialog.
.IP Log
If you have enabled the debug log under the Hacking Support preferences
section, then this button appears.  It displays the debug log.
.IP Aliases
This displays an interface to the MH alias file.  You will be prompted
to set up the MH profile entry that names the alias file, if needed.
There are a pair of lists in the interface.  To the left is a list
of your aliases.  Selecting one will display the address(s) of the
alias in the right-hand list.  Under each list are three buttons:
Insert, Change, and Delete, that let you operate on the aliases
or the addresses associated with an alias.  As you view messages,
the From address of the current message is displayed in the dialog.
There is a Help button in the dialog that gives more tips about using it.
.IP Preferences
This displays a dialog that lets you customize \fIexmh\fP.
There is also a different Help button in the Preference dialog that
explains how to make selections and get more info.  In particular,
you can click on the descriptive label for an item to get
a popup with more detailed information on the preference item.
.LP
The Fonts item under Preferences is a special case.  It lets you define
the fonts used for different widgets.  Consult the help text associated
with its interface for more details.
.IP Quit
This quits exmh.  If there are pending changes to draft messages,
they are saved first.
.SH FOLDER BUTTONS
.PP
The middle set of buttons has folder commands:
New, Flist, Inc, Commit, Search... and More...
.IP New
This pops up a dialog box from which you can create new folders.
When entering a folder name, the '@' character is short for the
current folder name.  Note that the Delete folder operation is
under the More menu.
.IP Flist
The Flist button refreshes the color feedback of folder buttons
so you can tell which folders have unseen messages in them.
If you use presort inc style, then this is done automatically.
However, if you read all the unseen messages in a folder from
outside \fIexmh\fP, then it will miss the transition out of the unseen state,
so the Flist button will help reset its view of the folder state.
.IP Inc
The Inc button will incorporate new mail from your spool file
into your mail folders.
This command is also bound to the keystroke 'i'.
.IP Commit
The Commit button causes pending delete and move operations to occur.
This command is also bound to the keystroke <Control-Return>.
Know that messages marked for deletion are removed by
means of the MH \fBrmm\fP command.
In most systems, \fBrmm\fP will rename the message to a name
beginning with either a comma (,) or pound sign (#).
Unlike \fBxmh\fP, \fPexmh\fP requires that you commit changes before
changing folders and doing other operations that modify the scan listing
(e.g., Pack, Sort, Rescan).
However, you can set Auto Commit under the Scan Listing preferences
to have exmh automatically commit changes when needed.
See also the description of the "Purge Folder" operation.
.IP Search...
This is a menu of different search methods.
The first two operations use a simple Find dialog.
The Pick and Glimpse interfaces have their own dialogs.
The menu entries under Search... are:
.IP "Search help"
This help text describes the search methods in more detail.
.IP "Current message"
Enter a string to find in the current message.
<Return> causes the search to
begin.  <Control-s> and <Control-r> cause subsequent forward
and reverse searches, respectively.
.IP "Folder table of contents"
This search is done by scanning the folder table of contents display,
so it only hits on the current folder scan output.
Select the All button to select all matching messages.
Use Pick for more general searching.
.IP "All messages in current folder (pick)"
This provides an interface to the MH \fBpick\fP command.
Begin by choosing from the "pick attribute" menu in the dialog.
These include headers like Subject, From, To, and Cc,
as well as date criteria like Before, and After.
You can search for a string in the body of messages with
the "Search" field.
You can enter a pattern for any of these, and \fBpick\fP selects messages that
satisfy the logical AND of these pattern specifications.
Note that the Before and After patterns are date-specific, and \fBpick\fP
understands many forms of date patterns.  Read the MH man page on \fBpick\fP
for the complete story.  You can build up more complex logical search
expressions by clicking the Or button.
.IP "Pick from Seq/msgs(s)"
This entry lets you limit
\fBpick\fP to a specific set of messages. In particular, you can specify
a sequence name (e.g., "unseen"), and have pick find all messages with that
sequence identifier.  After the pick completes, a range of messages is
selected that satisfies the search, and you can operate on these messages
as described below.
.IP "Add to Sequence(s)"
This entry lets you apply a sequence name to the
set of messages that are selected by the search.  You can select these
later by using the "Pick from Seq" field.
.IP "Add to Sel"
This button is part of the \fIpick\fP interface.
Set this before the pick to add the results of the pick to
whatever message range is already selected in the table of contents.
Otherwise a new selection is made.
.IP "New FTOC"
This button is part of the \fIpick\fP interface.
It causes the table of contents to be replaced with the set
of currently selected messages.  These can be the result of a pick,
or you can just make a selection with the mouse.
\fIWarning\fP: after projecting a new table of contents,
a subsequent \fIpick\fP will only find things in this limited
table of contents.
.IP "Mark Seen"
This button is part of the \fIpick\fP interface.
Use this to remove the selected messages from the unseen sequence.
.IP "All messages in all folders (glimpse)"
This brings up an interface to the Glimpse text index tool.
You must first index your mail,
so to get started click "index" to build your indexes.
This process runs in the background, and the status of the
index process is displayed in the Glimpse log.
The storage overhead of the indexes is about 10% to 15%,
which is quite good for a full text index system.
To search, enter a word in the search area and press "Search".
The button changes to a "Stop" button, although the stop may
take a moment to take effect.  The search results are displayed in
the log.  Click on the message in the log and exmh will visit
that message.  For more details, see the exmh-use man page.
.IP "Display related messages"
This selects messages that have the same subject as the current
message, or that are "in-reply-to" the current message.
The folder table of contents is replaced by one that only lists
the related messages.
.IP More...
This is a menu with more folder operations in it:
.IP "Rescan folder"
Rescan forces a rescan of the folder contents.  \fIExmh\fP maintains a
cache of the scan output that could get confused.  The Rescan
button forces a full scan.  If you just want to scan the new
messages that have arrived in the folder you are currently
reading, it is more efficient to click on the folder's label
in the top pane.  This will invoke a scan merging procedure
that attempts to preserve the cached results of previous scans.
You should only need Rescan when you have reason to believe
that \fIexmh\fP is out of sync.
.IP "List only selected messages"
This changes the table of contents to list only the selected messages.
This is useful after a pick or table of contents search that has
highlighted several messages throughout the folder.
You must use Rescan folder to get the full listing back.
.IP "Pack folder"
This will renumber messages in the current folder
to eliminate any gaps in the number sequence.
.IP "Sort folder"
This is a cascade menu that provides different ways to
sort your folder: by date, by subject, and by the sender.
The subject and sender sorts also do some grouping by date
so that messages from one person or about one subject may
be grouped into several subgroups based on their age.
See the MH sortf command manual page for details.
.IP "Find all folders"
This refreshes the display of folder buttons,
and it will pick up any new folders you have created.
This currently uses the MH command "folders -fast -recurse",
which can be slow.
The results are saved, so you should not
need to wait for this scanning unless you change your folder
structure.  (You can also edit the .folders file in your
MH mail directory.)  The \fIexmh\fP distribution comes with a two-line patch
for the MH folders program that speeds it up considerably.
This is found in the misc/folder.patch file.
.IP "Delete folder"
This lets you delete a folder.  The folder must be empty, except
for .xmhcache, .mh_sequences, and any .nfs junk files.
.IP "Catch-up Unseen"
This clears the unseen sequence from the current folder.
.IP "Update scan caches"
This runs another process in the background to update any
scan caches that are out of date.
.IP "Update all scan caches"
This runs another process in the background to update all the
scan caches.
.IP "Purge folder"
When MH deletes a message it really only renames it to have a name like
#N or ,N if it was message N.  The Purge operation will clean up these
junk files that are older than 7 days.  (This age parameter can be
adjusted by a Preference item under the MH Tweaks section.)
.IP "Purge all folders"
This runs through all your folders deleting old junk files.
This is done in the background so you can continue to use the
user interface during the process.
.IP "Import mailbox folders"
If you use a program like Elm or Sun's Mailtool, you can import their
message folder contents into the format used by MH.  This menu entry
brings up a dialog that lets you do this.  After you specify the
directory that contains the mail folders, a display with a checkbutton
for each folder is created.  You can select which folders to import.
The originals are left alone, so you'll have to delete them yourself
after you import them into MH.
.IP "Audit"
This is a cascade menu entry that lets you either view
the audit summary statistics or the current audit log.
Note that the audit log is appended into ~/Mail/.exmhaudit
when you quit exmh.
.IP "Re-read MH Profile"
If you change something important in your MH profile,
you can get exmh to reprocess it with this operation.
.SH MESSAGE BUTTONS
.PP
The last set of commands are message operations.
Several of these work in a batch fashion on all the
messages in a selected set.
These include Delete, Move, Link, Forward, Unmark, and Print.
Some of these buttons and menu entries
are disabled if there is no current message.
Disabled actions have their text greyed out.
.IP Forward
Forward the current message(s).
Also keystroke 'f'.
.IP Send
Compose a mail message.  Chose the editor to use via the Preferences dialog. Also keystroke 'c' (for "compose").
.IP Prev
Move to the previous message.
Also keystroke 'p'.  Keystroke 'P' changes the current message
without displaying it.  Use 's' to display such a message.
.IP Next
Move to the next message.  This will chain to the next folder
with unread messages if you are at the end of the current folder.
The "Next" folder is defined by the Folder-Order .mh_profile entry,
and it is always the first folder in the order that has unseen
messages.  (This keeps you in the interesting folders and away
from the dregs if you organize your Folder-Order properly.)
Also keystroke 'n'.  Keystroke 'N' changes the current message
without displaying it.  Keystroke 'F' changes to the next folder
with unseen messages.
.IP Delete
Mark the current message(s) for deletion and go to the next message.
Also keystroke 'd'.  If you change your mind, select the message
again and "Unmark" it via the entry under the message More... menu.
.IP Move
Mark the selected message(s) to be moved to the target folder and go to the next message.  Also keystroke 'm'.
Note that selecting a target folder also moves the current message(s).
So, you use the Move button if the correct target folder is
already selected.
If you make a mistake and want to move the
message to a different folder, just select it again and right click on
the proper destination folder.  If you don't want to move it at all,
then "Unmark" the message.
.IP Link
Similar to move, except that the message is linked
into the other folder so that it appears in both places.
While this appears like a copy, the same message is shared
between the folders to save disk space.
.IP "Reply..."
This is a menu of reply options.  By default there are two choices,
but you can easily define more variations on reply.  The default choices
are:
.IP "Reply to sender <Key-r>"
Reply to the current message, only to the person that send the original.
Also keystroke 'r'.
.IP "Reply all <Key-R>"
Reply to the current message, including on the cc line everyone that
got the original message. (It invokes MH \fBrepl\fP with:
-cc cc -cc to).
Also keystroke 'R'.
.IP "Reply help"
This pops up a display that tells you how to program new reply buttons.
You'll need to be familiar with the MH repl command and its filters.
Check out the \fBrepl\fP and \fBmh-format\fP manual pages.
.IP More...
This is a menu of more message operations.
.IP Print
Print the current message, or the selected set of messages.  The print
command is defined via the Preferences dialog.  The whole message
is printed, headers and all.
.IP "Unmark (Undo)"
Cancel a move or delete mark on the selected message(s).  Also keystroke 'u'.
.IP Clip
Display a new toplevel window containing the current message.
.IP "First Unseen"
Display the first unseen message, if any.  Also keystroke 'U'.
.IP "Burst Digest"
Use the MH \fBburst\fP command to split up a digest message into its individual
messages.  In addition, the new messages are added to the unseen sequence.
You can control whether or not the original message is replaced with just
the digest header by specifying burst options in your .mh_profile. (E.g., "burst: -inplace").
.IP "Redistribute"
This uses the MH \fBdist\fP command to resend a message to a different recipient.  Your editor is invoked with a small buffer containing only
Resent-To: and Resent-Cc: lines.
.IP "Save to file"
This simply copies the current message(s) to a file selected via the file
selector dialog.
.IP "Use message as draft"
This copies the current message into the drafts folder and opens
your editor on the message.
.IP "Edit message"
This opens your editor on the message so that you can
annotate it.
.IP "uudecode message"
This simple runs uudecode with the current message as input.
A file selection dialog lets you choose the name of the resulting file.
.IP "Print postscript content"
This is useful for non-MIME messages that contain a postscript file.
The message body (everything from the initial %!PS onwards)
is put into a temp file and then the Preferences
Postscript Print Command is used to print that file.
.IP "Apply command to message"
This runs a UNIX command over the current message (all of it).  A dialog lets
you set the command, with occurrences of $file replaced with the
pathname of the message.  The default value of the UNIX command is
hiding under the Printing preferences entry.
.IP "Apply command to body"
This runs a UNIX command over a temporary file containing the body of the
current message.
.IP "Mark Unseen"
This puts the current message(s) back into the unseen sequence.
.IP "PGP Encrypt"
Replace the current message with an encrypted version of it.
.IP "old PGP..."
This is a menu of operations that apply to PGP messages that
are not wrapped up in a MIME format.  Those are handled automatically.
This menu is used to tell exmh explicitly what to do.
You can Decrypt, View, or Extract Keys.
.IP "Scan for URLs"
Scan through the message body and look for URL and URN references.
If any are found, they are highlighted and made into active text.
Click on the text and Mosaic will be started on that URI.  You can
set the command to use via the URI preferences section.
Also keystroke 'z'.
.IP "View selected URL"
If you have manually selected an embedded URL, then use this menu
entry to get exmh to ask your web browser to display the selected URL.
If the automatic scanning doesn't work perfectly, you can do a manual
selection on top of an incorrect active text button.
.IP "Concatenate partial messages"
If you get a message in parts, as a MIME type message/partial,
then select all the messages that have the parts and
invoke this operation.
.SH SEDIT
The built-in editor, \fBsedit\fP, has a dozen or so commonly used editing
operations bound to keystrokes.  These are Emacs-like, but
you can change them from within the editor through the "Simple Edit" dialog
available under the main "Bindings" menu.
In addition, the editor has a number of buttons and menu entries
used to manipulate your draft messages.
.IP Abort
This deletes the draft message and removes the editor window.
.IP Save&Quit
This will save the message and quit the editor window.
The idea is that you will come back to the draft via
the drafts folder later.
.IP Help
This button brings up a window with a short explanation of the editor.
.IP "Insert @"
The is only active while you are replying to a message.
When you are replying to a message, the file named "@" is a symbolic link
to the message to which you are replying.  When this file is inserted,
it is formatted a bit, unlike the Insert File... operation.  The mail
headers are stripped off, quoted-printable text is decoded so
you get 8-bit characters in the edit buffer,
and each line is prefixed with a short string
to distinguish the inserted lines.  You can set this prefix via
an Xresource, replPrefix. The default prefix is "> ".  If you want to
change this and still have trailing blanks, it turns out this works ok.
The resource value seems to begin with the first non-blank character, but
then extends all the way to the newline character.
.IP Sign
This appends your ~/.signature file to the draft.
If this file is instead an executable script or program, it is
executed in order to generate the signature.
.IP Sign...
If you have multiple files that match the ~/.signature* pattern,
the Sign button is replaced with a menu of possibilities.
If you choose the autoSign Simple Editor Preference, then this menu
is used to choose which signature file (or program) is used when
automatically signing the message as it gets posted.
.IP Send
When you are done with the message, click Send.  This is also bound
to the keystroke <Control-c><Control-c>.
.IP Text...
This menu is used to add bold, italic, underline, etc. to your messages.
This results in a MIME message of type text/enriched.  The formatting
commands are supposed to be palatable to a non-MIME reader, but
you may want to take this into consideration.  If you disable the
"Format mail" setting (described below), the looks are discarded
before sending the message.
.IP More...
This menu contains a few entries for less common operations.  They are:
.IP "Keep on send"
This is a checkbox menu item that is off (un-highlighted) by default.
If selected it prevents the editor window from disappearing after you
send or abort a message.  This is useful if you'd like to compose
a few similar messages.  (I also use it to debug the enriched text formatter.)
.IP "Format mail"
This is a checkbox menu item that is enabled (highlighted) by default.
Formatting means to break long lines (> 80 characters).  This
is done as the file is saved, not as you type.  So, while the Tk text
widget does line wrapping while you enter text, \fBsedit\fP does not
break these lines until you go to send the message.  \fBIf you insert a file
into a message that has critical formatting, you may want to disable the
Format option.\fP
.IP "Attempt mhn"
If this is selected, your message is run through \fBmhn\fP in order to expand
any # directives that format a MIME message. See also the
"Run mhn now" menu entry below, and see the \fBmhn\fP man page for details.
.IP "Quote Printable"
If this is selected, then any text MIME parts are run through
the quoted-printable encoder to wrap up 8-bit characters in
a 7-bit safe encoding.  The Quoted Printable default
preference setting under Simple Editor can be set to
automatic, never, or always.  In automatic mode, quoting is enabled
when you enter a Compose key sequence to generate an 8-bit character.
.IP "Cite Selection"
This formats and inserts the current selection.
It is assumed to be in the currently displayed mail message,
and the sender of that message is identified before the formatted text.
Hint: this is the only useful way to include text from PGP encrypted
messages and text/enriched messages.  The formatting of text/enriched
is lost, however.
.IP "Insert File..."
This brings up a dialog box that lets you insert a file into the message.
.IP "Insert Part..."
This is similar to Insert File, except that it creates a MIME multipart message
structure to wrap up the inserted file.
It tries to determine the type of the file based on
\fBnametemplate\fP entries in the metamail mailcap description file.
It presents a dialog that lets you override the type,
add a Content-Description, and choose an encoding
for the file.  When you insert the first Part of a message
you are asked whether or not to save the existing message
body.  If you save it, it becomes the text/plain first part of the multipart
message.
.IP "Insert External..."
This sets up an Message/External-Body MIME message part.  This is useful
for sending around pointers to files available via FTP or on your
local file system.
.IP "Insert PGP Keys..."
Insert your PGP keys into the mail message.  You can use the key
selector to choose any key in your public keyring.  Just type in
a pattern that matches part of the key ID.  Matching keys are
displayed in the listbox.
.IP Whom
This entry displays a small window with the network addresses of
the recipients of the message.  Clicking the button a second time
makes the window go away.
.IP "Spell..."
This runs either the UNIX spell or ispell command.
Choose between these two options in the Simple Editor preferences.
(Don't confuse that with the setting under Editor Support preferences.)
If you use ispell then the draft is reloaded after ispell terminates.
If you use spell then the output of spell is put
into a second pane inside the editor window.  That's all - nothing fancy.
.IP "Find SEL"
This searches the draft for the current X selection.  The intent is that
you select misspelled words in the output of the spell command and use
this menu entry to locate them in the draft.  (The find and keybinding
mechanisms were stressed by this addition - expect a cleanup someday.
There is no reasonable way to define keystroke accelerators for
the find and still be able to change them in the Bind dialog. - don't ask...).
.IP "Save Buffer"
This saves the editor buffer into a file in your drafts folder.
Most likely you really want the "Keep on Send" function instead
of Save, because that function preserves the draft after you send
it, while ordinarily (in spite of "Save") MH will rename the draft
after you send it.
.IP "Run mhn now"
This processes the draft with \fBmhn\fR
and puts the result back into the buffer.
After this you can use "Insert Part..." to add more parts,
but you should not use any new \fBmhn\fR directives.
.IP "Mime Preview"
This displays the message as it will appear when an exmh user
receives it.  A detached top-level window is used, as with
message Clip.
.IP "Alternate Editor"
Save the draft and invoke your alternate editor on the message.
Define the alternate under the Editor Support preferences..
.IP "PGP..."
This is a menu of options that use the Pretty Good Privacy package to
to encryption and authentication.  An explanation of pgp is beyond
the scope of this manual.  This works by inserting a header into your
message that indicates what action to do.  The actual encryption or whatever
is done after you hit the Send button.  If you use an external
editor you can insert the headers in that, and exmh will run
your message through PGP for you when you send it.  The four
supported headers are:
.DS
pgp-action: signclear
pgp-action: signbinary
pgp-action: encrypt
pgp-action: encryptsign
.DE
.SH SEE ALSO
exmh-use, exmh, exmh-custom, mh
.SH AUTHOR
Brent.Welch@Sun.COM
.SH THANKS
To Xerox PARC/CSL, for supporting this work initially,
to Sun Microsystems Laboratories for continuing the support,
and to all the exmh users who contributed ideas and code.

