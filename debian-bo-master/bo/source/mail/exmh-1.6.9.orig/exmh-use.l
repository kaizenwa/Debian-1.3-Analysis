.\"
.\" 
.\" $Header: /home/bwelch/src/exmh/RCS/exmh-use.l,v 1.8 1996/08/22 22:51:22 bwelch Exp $ SPRITE (Berkeley)
.\" .DS
.\"	Begin an indented unfilled display.
.\"
.\" .DE
.\"	End of indented unfilled display.
.\"
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
.TH "EXMH" 1 "August 22, 1996" "Exmh 1.6.9" "EXMH USER GUIDE"
.SH NAME
exmh-use \- A more advanced user guide to the exmh mail user interface.
.SH INTRODUCTION
.PP
This man page provides more detailed information
about using \fIexmh\fP,
such as managing nested folders,
filtering mail,
and using an external editor.
The \fBexmh\fP man page is an introduction.
The \fBexmh-custom\fP man page describes customization of \fIexmh\fP,
and the \fBexmh-ref\fP man page describes all the buttons and
menus of \fIexmh\fP.
.PP
\fIExmh\fP uses the
regular MH programs to manipulate your mail folders
and messages.
This means it is compatible with command-line use of MH programs,
and its actions should be familar if you are an experienced MH user.
If you are a new MH user, then the details of running MH programs
is hidden behind the graphical interface.
.PP
This man page will occasionally make
references to advanced MH features like scan formats
and reply filters.  If you do not know about these you can
ignore the comments.  If you do know about them, however, you will
see that \fIexmh\fP can be affected by settings for these features
in your MH profile.
.SH OVERVIEW OF FEATURES
.PP
As well as providing basic MH functionality, \fIexmh\fP
has a number of other features:
.PP
\fBMIME support\fP.  \fIExmh\fP can display
MIME (Multipurpose Internet Mail Extensions)
messages, either directly or with the
help of the \fImetamail\fP package.
The built-in editor lets you compose enriched text messages
and insert files as parts of a multipart message.
.PP
\fBSun Attachments\fP.  These attachments are coerced into
MIME format.  Special treatment is given to certain common
sun attachments like Calendar Appointments.  You cannot create
a Sun format attachment within \fIexmh\fP, however.
.PP
\fBNested Folder Display\fP.
A display of your folders appears as a set of labels,
one for each folder.
This is similar to \fIxmh\fP, except that
the folder labels are highlighted to
indicate the current folder, the target folder for moves,
folders with unread mail in them,
and folders with nested folders under them.
\fIExmh\fP supports arbitrarily nested folder structures, unlike \fIxmh\fP,
which only supports a single level of nesting.
.PP
\fBScan Listing Highlights\fP.
The scan listing (folder table-of-contents)
is also highlighted to indicate the current message,
unread messages, and messages marked for move or delete.
Either color or monochrome highlighting is used, depending on your display.
.PP
\fBFacesaver bitmap display\fP.  If you have a facesaver database
on your system, \fIexmh\fP displays the bitmap face of the person
that sent the current message (or their organization).
.PP
\fBBackground processing\fP.  You can set \fIexmh\fP to run
\fIinc\fP periodically,
check for new messages arriving asynchronously in folders,
run the MH \fImsgchk\fP program,
or count up the messages in your mail spool file.
.PP
\fBMail filtering\fP.  \fIExmh\fP is designed to work with
external agents that filter arriving mail into different folders.
.PP
\fBUnseen folder display\fP.  If you use a system that delivers mail into
various folders, not just inbox, then you have the problem
of finding what folders have unseen messages in them.  \fIexmh\fP
provides feedback in the folder display so you can tell
which folders have new messages.
Also, the Next command will chain to the next folder with
unread messages if you are at the end of the current folder.
.PP
\fBPick\fP.
An interface to MH \fIpick\fP lets you select messages by patterns in the
header fields, by date, or by sequence name.
The interface is similar, but not identical, to the \fIxmh\fP pick interface.
.PP
\fBFast Search\fP.
Because \fIpick\fP can be slow, \fIexmh\fP also implements
fast searching over the current folder listing and current message body.
.PP
\fBGlimpse interface\fP.
Glimpse is a full text search tool from the University of Arizona.
It has a low overhead indexing scheme that costs about 10% to 12% of your
mail storage space.  It is quite useful to find messages in any of
your many folders.
.PP
\fBEditor interface\fP.  You can hook \fIexmh\fP to your favorite editor
using the \fBexmh-async\fP script.  Or, Tcl-based editors such
as \fImxedit\fP can interact with \fIexmh\fP directly.
As the default, a simple built-in editor named \fIsedit\fP is provided.
.PP
\fBKeybinding User Interface\fP.  Bindings for common editting and cursor
motion commands are exposed in a UI that lets you choose keystroke
sequences to match your expectations.  You can also define key bindings for
Tcl commands that are part of the implementation.
.PP
\fBAliases User Interface\fP.  A browser for your MH aliases lets you
define new aliases and insert aliases into mail messages.
.PP
\fBPretty Good Privacy (PGP)\fP.  If you have PGP, you can use it from
\fIexmh\fP to digitally sign, encrypt, and decrypt messages.
.PP
\fBPreferences User Interface\fP.  There are many knobs and dials you
can adjust to control the behavior of \fIexmh\fP.
.PP
\fBURL Smarts\fP.  Exmh can scan for embedded URLs in your messages,
and it can look for X-URI: mail headers.  These are turned into
active text buttons.  Click on them and exmh asks your favorite
Web browser to display the URL.
.PP
\fBUser Programming\fP.  If the preference settings are not enough for you,
you can program \fIexmh\fP more directly.  You can define new buttons
and menus and add new Tcl code to its implementation.
This is described in the \fBexmh-custom\fP man page.
.SH "GETTING STARTED"
.PP
Mostly likely you are already an MH user.  If not, go through
the tutorial in the basic \fIexmh\fP man page.
.PP
If you previously used a mail reader like Elm or Sun's mailtool that
manages mail folders in mbox-like files, then you can use the
\fIImport mailbox folders\fP menu entry to copy your old folder contents
into the folders managed by MH and \fIexmh\fP.   This menu entry
is under the folder More... menu.
.PP
There is an extensive Preferences system for \fIexmh\fP
that lets you tune it in many ways. 
Throughout the man page, references to Preference settings will be
made when there are tunable parameters associated with a feature.
A more complete description of the Preference facility is given
in the \fBexmh-custom\fP man page.
.SH KEYBOARD COMMANDS
.PP
Several of the folder and message operations have keystroke bindings
so you can do much of your browsing with hands on the keyboard.
The default bindings are given
here using the Tk syntax that you'll need to use in the
Bind dialogs.  Capitalization is important.  For more information,
see the \fBexmh-custom\fP man page.
.DS
<Control-Return> - Commit
<Key-i> - Inc
<Key-f> - Forward
<Key-r> - Reply
<Key-c> - Compose
<Key-p> - Previous
<Key-P> - Previous message, but do not display it.
<minus> - Previous message, even if it is marked for delete or move.
<Key-n> - Next
<Key-N> - Next message, but do not display it.
<Key-F> - Change to next folder with unseen messages.
<Key-U> - Next unseen message, if any.
<Key-s> - Display the current message.
<Key-d> - Delete and advance
<Key-D> - Delete and advance, but do not display.
<Key-m> - Move and advance
<Key-M> - Move and advance, but do not display.
<Key-u> - Unmark a pending delete or refile from the current message(s).
<Key-z> - Highlight embedded URLs
<Key-A> - Added selected Sun Calendar Manager appointment to your calendar.
<space> - page forward the message display.
<BackSpace> - page back the message display.
<dollar> - '$' Go to last message, but do not display.
<asciicircum> - '^' Go to first message, but do not display.
<question> - '?' Bring up the Command binding dialog.
.DE
You can browse, change, and define new command keystrokes via the
Bind user interfaces available under the Bindings menu.  There are
two dialogs: one for editing commands in the built-in editor, and another
for the command bindings described above.
.SH "THE EXMH DISPLAY"
.PP
At the top of the display is a subwindow
that has a label for each of your top-level folders.
In the middle there is a table of contents for the current folder.
At the bottom the current message is displayed.
Both the folder display and table of contents
have some highlights that are described below.
.PP
The command buttons are grouped into three sets.
The buttons along the top apply to \fIexmh\fP itself, such
as Help, Preferences, and Quit.
The buttons above the scan listing are operations on folders, like
Inc and Commit.  There is a menu there labeled More... that has
several more folder-related operations like Pack and Sort.
The row of buttons above the message display are for operations on
messages, such as Send, Next, and Delete.  There is another menu here
labeled More... that has several more operations on messages, such
as Print, Edit, Burst Digest, and Edit Message.
.PP
The version number of \fIexmh\fP is displayed next to the top row
of buttons.  The release cycle for \fIexmh\fP goes something like
1.5alpha, 1.5beta, 1.5gamma, ... 1.5, 1.6alpha, and so on.
The unqualified versions are deemed "stable" enough for naive
users.
.PP
Summary information for the current folder appears next to the
folder buttons.  This indicates the current folder and how many
messages are in it.
Just below the scan listing is a status line that provides some
feedback about what \fIexmh\fP is doing.  When a new message is
displayed, the subject or Content-Description component is displayed there.
Just to the left of
this the folder and number of the current message is displayed.
.PP
To the left of the message buttons the EXMH logo appears.
This is replaced with the facesaver image of the person, or their
organization, that sent the current message.
.PP
The black diamond to the right of the status message area is a grip
that you use to change the boundaries between subwindows.
The resize unfortunately depends on heuristics based on the size of
text being displayed in the window, so you have to have a message
displayed before the resize will work.
.PP
Press the first mouse button on the black diamond and a horizontal line appears.
As you drag the line up and down, the status message indicates
what boundary you are changing.
Initially you are adjusting the scan listing (middle subwindow).
If you drag the line off the top of the scan listing,
the mode changes and are now adjusting the folder cache
size, if it exists.
If you drag the line above the folder cache,
the mode changes again you are changing the main folder display area.
As you drag the line down, the mode changes again.  The main
drawback of the automatic mode sensing 
is that you cannot grow the main folder display area by
more than the size of the folder cache, if it exists.
.PP
There are also preference settings for each of the subwindow sizes.
Look under Scan Listing, Folder Cache, and Folder Display preferences.
.SH "FOLDER DISPLAY"
.PP
The top subwindow has a label for each of your MH folders.
The folder display is highlighted to reflect the state of folders.
The \fIColor Legend\fP menu item under the main Help menu will
display a key for the highlights.
.DS
black - the current folder.
bold outline - a folder with unseen messages.
stippled background - the target folder for move and link operations.
.DE
.PP
On color displays:
.DS
red - the current folder.
blue - a folder with unseen messages.
yellow - the target folder for move and link operations.
.DE
Folders that have nested folders under them are
highlighted by a shadow box.
.PP
The mouse bindings for the folders labels are:
.DS
Left - Change to folder.
Middle - View nested folders.
Right - Refile current messages to the folder.
Shift-Right - Link current messages to the folder.
Shift-Middle - Drag a folder label to some drop target.
Control-Right - Clear the current target folder.
.DE
.PP
Note that a right click does two things at once.
It selects the target folder for refile, and it
marks the currently selected message(s) for pending refile to that folder.
This might seem over-eager at first, but it is quite useful
once you get used to it.  The currently selected target
folder remains highlighted, and you can move a message to
this folder by clicking the Move button or typing 'm'.
.PP
A middle click on the folder label displays a popup window with the
folder labels for nested folders,
and you can operate on these labels just like the top-level ones.
The popups work recursively for nested folders.
When the mouse leaves the pop-up it is withdrawn.
The Folder Display preferences lets you adjust how the
pop-up behaves.
.PP
\fIHint:\fP  If you change the popup-related settings, 
you'll have to resize the \fIexmh\fP window a bit to force it
to redisplay the folder labels.  Only then will the new bindings
take effect.
.PP
You can make the folder display into a separate top-level window
with a setting under the Folder Display Preferences.  This means you
can close the main window and still view the folder display to see what
folders have new mail in them.  When you click on a folder label the
main window will be opened automatically.
.SH FOLDER CACHE
.PP
Below the main folder display is a cache line of recently visited folders.
If you have nested folders, their complete pathname is given in the cache
window.  Using the Folder Cache preferences settings you can choose how
many lines are reserved for the folder cache.
You can also put some folders into the
cache permanently with the Sticky Folders preference item.
.PP
If you don't have many folders, the folder cache line is really just
a waste of screen real estate.  In this case, set the number of folder cache
lines to 0 in the Folder Cache Preferences section
to eliminate this part of the display altogether.
.PP
You can drag folders in and out of the folder cache.
Use <Shift-Button-2> to drag a folder label.
Drag a label from the main display into the cache to add it,
or from the cache to the main display to remove it from the cache.
.SH "FOLDER TABLE OF CONTENTS"
.PP
The middle subwindow lists the messages in the current folder.
.PP
The lines of the scan listing are highlighted to reflect the
state of each message.
On color displays:
.DS
    Red - the current message.
    Blue - unread messages.
    Grey background - messages marked for deletion.
    Yellow background - message marked for refile to another folder.
.DE
.PP
On monochrome displays:
.DS
    Reverse video - the current message.
    Underlined - unread messages.
    Cross-hatched background - messages marked for deletion.
    Stippled background - message marked for refile to another folder.
.DE
.PP
The mouse bindings for the scan listing are listed below.
"Drag" refers to holding the mouse button down while moving the mouse.
.DS
Left - select and display a message.
Left-Drag - select a range of messages.
Shift-Left - modify the selected range.
Middle-Drag - scroll the display.
Right - select but do not display the message.
Shift-Right - Drag folder/msgID to drop target.
.DE
.PP
Several of the operations on messages can be applied to a set
of selected messages.  You can create a discontinuous selection
by using the Shift-Left (and Shift-Left-Drag) mouse actions.
.PP
\fIHint\fP:
The scan listing comes from \fIboth\fP the MH \fIscan\fP and \fIinc\fP
programs, depending on circumstance.
If you define a custom scan format, set the same filter in your
profile for both \fIinc\fP and \fIscan\fP.
.SH MESSAGE DISPLAY
.PP
The current message, if any, is displayed in the bottom subwindow.
The uninteresting mail headers are scrolled off the top of this display
initially.  You can control this with the
Header-Display and Header-Suppress profile entries
as described in the \fBexmh-custom\fP man page.
That man page also describes how to colorize different mail headers.
.PP
Pressing the space bar will page you through the message,
or take you to the next message if you are at the end of the current message.
Backspace will go back a page.  You can also scroll the message with
the mouse by using the scrollbar or drag-scrolling.
.PP
MIME messages are indicated by displaying their content-description in the
status line.  The mouse cursor is changed to a watch while the
MIME message is parsed and displayed.
On a color display, a MIME message body is displayed in a slightly
darker background.  This shading is used to indicate the depth of
nesting in a multipart MIME structure.
.PP
A popup menu is available to process MIME messages, such as saving
their contents or processing the contents with an external viewer.
Press the right mouse button in the message display area to obtain this menu.
Make sure you press the mouse button over the message body,
not the headers.
For multipart messages,
the menu changes depending on what part of the message you are
looking at.  See also the Preferences entry for MIME for some
parameters you can adjust.
.PP
Every content type can be saved or printed, and other types can have
additional options depending on entries in your site's mailcap file.
The print function just prints the item as text using your
Print preferences command.
The print-with-mailcap entry, if available, uses the print command
as defined for the content-type in the mailcap file.
.PP
You can display a message in a new top-level window in order to keep
it around on your display.  Use the Clip operation under the message More...
menu.  This will display the current message in a new, larger window.
.PP
Exmh can cooperate with a WWW browser to display web pages.
If you get a URL in a mail message, then use the 'z' keystroke to
highlight the URLs in the message.  (This is adhoc and not %100 accurate.)
The URLs are turned into active text buttons.
Click on one and exmh asks your web browser to display the page.
You can have exmh scan every message automatically for URLs, although
this can be slow.  Tune this with the settings under the WWW preferences.
.PP
If exmh sees special X-URL (or X-URI) headers it will turn the
facesaver area into an active button.  It takes on a raised relief
and when you click on the face (or exmh logo) then the web browser is
asked to display the URL in the X-URL header.  More and more folks are
putting X-URL headers into their mail messages so you can find their
home page.

.SH "MANAGING MESSAGES"
.PP
There are three basic actions you can take with a mail message:
leave it alone, delete it, or refile it to another folder.
You automatically advance to the next message after you delete or
refile the current message.  This makes it easy to go through
your mail messages, deleting and refiling as you go.
There are several settings under the Scan Listing preferences
section that control the behavior of \fIexmh\fP when reading mail,
and some of these will be mentioned below.
.PP
Like \fIxmh\fP, \fIexmh\fP does deletes and refiles in two steps.
In the first step you \fImark\fP a message as needing some action.
Later on you \fIcommit\fP these changes by pressing the
Commit button, or by pressing <Control-Return>.
Unlike \fIxmh\fP, however, \fIexmh\fP requires that you commit
changes before you view a different folder.
Marked messages are highlighted in the scan listing,
as described above.  If you reselect a message marked for refile,
the destination folder for that message is shown in the Status line.
.PP
\fBDeleting Messages\fP.
To delete a message, use the Delete button or press 'd'.
Delete results in a call to \fIrmm\fP.  If you want deleted messages
to be refiled into a special folder (e.g. +wastebasket or +deleted),
you'll have to define a shell script and register that as your
rmmproc in your .mh_profile.
.PP
\fITrick\fP: If you press 'D' to delete a message,
then you advance to the next message
without displaying it, so you can quickly go through a folder
to clean it up.
.PP
\fBRefiling Messages\fP.
To refile a message, right click on the destination folder.
This marks the current message(s) for refile to that folder,
and leaves the folder selected as the current target folder.
If the right target is already selected, then you can use the
Move button or type 'm' to refile the message
and advance to the next message.
.PP
\fITrick\fP:
If you press 'M' to refile, you advance but do not display the next message.
.PP
\fBUndo\fP.
If you make a mistake, you can unmark a message
with the \fIUnmark (Undo)\fP message More... menu entry.  This operation
applies to the currently selected message(s), not necessarily to the
last message you marked for delete or refile.
If you want to change the disposition of a message, like refile
it instead of delete it, you do not need to unmark it first.
Just select it and take the new action.
.PP
\fITrick\fP: use '-' (minus) to back up to a message you just marked,
and 'u' to unmark the message.
.PP
\fBLinking Messages\fP.
To link the current message(s) into a folder, hold the shift key down as
you right click the destination folder label.  If the correct folder happens to
be selected as the target already, you can also use the Link button.
If you use Link frequently, you should adjust the Scan Listing
preferences item for \fIAdvance after Link\fP.  If you turn this off,
then the current message remains selected after a Link, which makes it
easier to link a message into multiple folders.
.PP
\fBAuto Commit\fP.
The \fIAuto Commit\fP Scan Listing preferences item
will let \fIexmh\fP commit your changes
when you change folders, sort or pack a folder,
or quit the program.
Without auto commit, you will be prompted to commit when
you try to take one of these actions
and have messages still marked for delete or refile.
In addition, auto commit will commit changes when you close
the main window.
.PP
\fBImplied Direction\fP.
The next message viewed after a Delete or Move is usually
the next message.  However, if you set the
\fIImplied Direction\fP preference item, then \fIexmh\fP will remember your
last Next or Prev action and move in that same direction after
a Delete or Move.  This means you can just as conveniently go
through a folder backwards as forwards, although it might catch
you by surprise.
.PP
\fBSkipping Marked Messages\fP.
The \fIskip marked msgs\fP preference item controls
whether Next and Prev take you to a message marked for delete or refile,
or whether you skip over these.  Remember the handy '-' key binding,
which takes you to the previous message even if it is marked.
.PP
\fBChanging Folders Automatically\fP.
When you are at the end of a folder, \fIexmh\fP will be ready to take
you to the next folder that has unseen messages in it.
You can just press Next, and it will automatically change folders
for you.  If you have marked messages, however, you will be reminded of
that.  At this point you can press the Commit button, or if you have auto-commit
enabled, then you can press the Next button again to trigger the commit and
folder change.  If you always want to be warned before an automatic
folder change, enable the \fINext Guard\fP preference item.  With this
enabled you will have to click Next twice (or type 'n' twice) to
get the automatic folder change.  It will also tell you to which folder
it is about to change.
.PP
The final twist on automatic folder changing is that,
by default, \fIexmh\fP will automatically
change back to your "first" folder if there are no more folders with
unseen messages in them.  
You can disable this feature with the 
\fICycle back to first\fP preference item.
The default Folder-Order profile component defines inbox to be the first folder,
so ordinarily you will change back there.
The Folder-Order profile component is described in the
\fBexmh-custom\fP man page.
.PP
.SH "SENDING MAIL"
This section describes how to start a mail message.
The messages are created as templates in your drafts folder
and then your editor is opened on the draft.
The next two sections describe how to actually compose the message
when using the built-in editor or an external editor.
.PP
\fBSending a New Message\fP.
Use the Send message button to create a new draft message.
This runs the MH \fIcomp\fP command to set up the template for the
draft, so it is affected by your components file, if any,
and your comp profile entry, if any.
.PP
\fBReplying to a Message\fP.
The Reply... menu has two entries: \fIReply to sender\fP and \fIReply all\fP.
These use the MH \fIrepl\fP command, so they are affected by your replcomps
file, if any, and your repl profile entry, if any.
The first entry uses \fI-nocc to -nocc cc\fP
so that the reply just goes to the sender and you,
while the other entry uses \fI-cc all\fP so that the reply goes to everyone
in the To and Cc headers of the original message.
There is also an entry that displays instructions for adding more
entries to the Reply... menu.  This is described in more detail
in the \fBexmh-custom\fP man page..
.PP
\fBForwarding a Message\fP.
Use the Forward button to forward the current message, or messages.
This uses the MH \fIforw\fP command to set up the message, so it is affected
by your forwcomps file and your forw profile entry, if these exist.
.PP
\fBUsing an Existing Message as a Template\fP.
You can use an existing message as a template for a new message by
using the \fIUse message as draft\fP menu entry under the message More...
menu.  In this case the message is copied into the draft folder first.
You will also use this menu entry if you have to go back into your
drafts folder to continue work on a message draft.
.SH "THE BUILT-IN EDITOR"
.PP
The built-in editor provides a handful of text editting commands
and several operations that affect the formatting of a mail message.
.PP
\fBSending a Message\fP.
Usually all you need to do is fill in the
headers of your message, type in the message, and press the Send button.
When you start,
the cursor will be positioned at the end of the first empty header line,
or at the beginning of the message body if, as with reply, all the headers
are already initialized.  When entering the headers, <Tab> will take
you to the next header line, and <Double-Tab> will position the cursor
at the start of the message body.
.PP
The only important rule to remember about mail messages is that
the header and body are separated by a blank line.  In addition,
MH supports a header separator line of all dashes, ------.
.PP
\fBSaving a Draft\fP.
If you want to save a message draft and return to it later,
use the Save&Quit button.
If you want to send a message but save it for use as another
draft, select the \fIKeep on Send\fP checkbox item under the
More... menu.  In this case, when you click Send the editor will
remain open and the message will remain in your drafts folder.
Otherwise the draft message is removed from the drafts folder
after it has been successfully sent.
(Actually, it is renamed in the same fashion that \fIrmm\fP
renames deleted messages.)
If you quit \fIexmh\fP and you have drafts in progress, they
are automatically saved and remain in your drafts folder.
.PP
\fIWarning!\fP the Save button under the
More... just writes the editor's buffer to the file system.
It does not imply that the draft will remain after it is sent.
You probably want Save&Quit or Keep on Send instead.
.PP
\fBAborting a Draft\fP.
If you decide you do not want to send a message, you can delete it
by pressing the Abort button.  If you have not changed the initial
template it will be aborted immediately.  Otherwise it will
prompt you for confirmation.
.PP
\fITrick\fP: a second click on the
Abort button is the same as clicking the OK button in the
confirmation dialog.
.PP
\fBSignatures\fP.
The editor supports signatures in two ways.  If you select Auto-sign
from the Simple Editor Preferences section, then your .signature
file will be appended to the message when you press the Send button.
Otherwise, you can use the Sign button to insert your .signature file
at the end of the message.
.PP
If you have several files that match the pattern ~/.signature*,
then you will have a menu of signature options available under the
Sign... menu, which replaces the Sign button.
You can define the default choice with a Preference setting under
the Simple Editor section.
With auto-sign, the Sign... menu has a set of check box items that determine
which signature file will be used.  Otherwise, the menu simply selects
which signature you want to insert at the end of the message, and
the signature is inserted when you select the menu entry.
If you are composing a multipart MIME message, your signature is
added as another MIME part.
.PP
\fITrick\fP: if your signature file has the execute permission set, then
exmh will try and execute it and use its output for the signature.
If you accidentally have the execute bit set, the error that occurs
may be very unclear.
.PP
.SH MAIL FORMATTING
\fBLine Breaks\fP.
By default, you can just type in long lines and they will wrap at
word boundaries when the line gets too long.  This actually happens
twice, the first time by the Tk text widget when you enter text.
Unfortunately,  this doesn't actually insert newline characters, 
and it is done
based on the size of the window.  Therefore, the line breaking is done a second
time by \fIexmh\fP after you press the Send button.
The line length limit defaults
to 79, although you can change that in the Simple Edit Preferences section.
.PP
\fIHint\fP:
If you change the line length,
you should also resize the editor window to match the
width (e.g. 72) so your message will look right as you type it in.
Set the following resource in your ~/.exmh-defaults
.DS
*Sedit*Text.width: 72
.DE
.PP
\fBChanging Fonts\fP.
You can use the Text... menu to change the font of the selected text.
This promotes your message to a MIME content-type of text/enriched.
The encoding of these text looks is also done when you press the Send
button.  You may see the encoding after clicking Send
as a side-effect of the way this is implemented.
.PP
The \fIInsert File...\fP menu entry is used to insert a file as-is,
with no MIME structure.  The file is inserted at the current insert point
in the editor.
.PP
The \fIInsert @\fP button is used to insert the body of the message being
replied to.  This button is only enabled when you are replying to a message.
The body is formatted a little, just by prefixing each line of the message
body with a string.  The default is "> ", and you can change this via
the Simple Editor Preferences section.
.PP
\fINote\fP: it is apparently not
possible to define a repl prefix with leading spaces.  This is a side
effect of storing the prefix in the X resources database.
.PP
The \fICite Selection\fP menu entry formats and inserts selected text.
It is assumed that the text is from the current message, and the
sender of that message is identified just before the inserted text.
You can use this feature to insert text from several different messages.
.PP
The \fBPGP\fP menu provides access to the Pretty Good Privacy public
key system.  With this you can send messages with a digital signature that
certifies that you sent the message.  You can also encrypt and decrypt
messages with PGP.
A complete discussion of PGP is far beyond the scope of this
man page.  However, if you know PGP, you will find the PGP features of
\fIexmh\fP useful.
.PP
\fIHint:\fP.  The \fIInsert PGP Keys\fP menu entry is hiding over in
the Sedit More... menu.  Use this to insert your public key or other
public keys into a mail message.  By default, only your keys are listed
in the key chooser.  Just type in part of the key ID of any key you want
to choose and matching keys are displayed.  Try '*' to list all keys.
.SH MIME FORMATTING
.PP
\fIWarning!\fP  The line-break formatting described above can
interact poorly with the formatting described next.
This is one of the roughest
spots in the \fIexmh\fP implementation.
For example, if
you include a file in your message that has important formatting,
like a Postscript file or some program code, you will want to disable
the line-break formatting.  You can do this by turning off
the \fIFormat mail\fP
checkbox item under the More... menu.
.PP
If you are familiar with MHN directives,
you can enter them into your
message.  In this case, you will want to select the 
\fIAttempt MHN\fP checkbox menu item under the More... menu.
Again, you will probably want to disable \fIFormat mail\fP to make
sure that long MHN directives are not chopped into multiple lines
at the wrong place.
.PP
You can create a multipart MIME message by using
the \fIInsert Part...\fP menu item.  This adds another part to your
message, promoting it to MIME content-type multipart/mixed if
necessary.
The first time you add a part,
you have the option of preserving the existing message
body or completely replacing it.
When the file is inserted you are asked to choose
its MIME content-type and content-transfer-encoding.
.PP
The \fIInsert External...\fP menu entry is used to create a
MIME part of Content-Type message/external-body.  This is an indirect
reference to a file, which is either a local file or a file available
via anonymous FTP.  A dialog lets you
fill in the various parameters to the indirect reference.
For an anonymous FTP pointer, the \fISite\fP is the internet host name
and the \fIDirectory\fP and \fIFile name\fP determine what file it is.
The \fITransfer mode\fP is either "text" or "image".
Use "image" to ensure that all 8 bits of each byte get transferred.
The \fIDescription\fP is for the benefit of the receiver of the mail
message and should describe what the pointer references.
.PP
\fITrick\fP: A file section dialog appears first, but this is not always
appropriate if you are making an external reference to a non-local FTP site.
Just hit cancel on the file selection dialog.  Then, when you enter the
file name, the content-type will be inferred from the name.  You can
always override the content-type if \fIexmh\fP guesses wrong.
.SH "8 BIT CHARACTERS"
.PP
The built-in editor supports 8-bit characters found in European
character sets.  Insert an 8-bit character by pressing the
Compose key and then two other keys to get the special character.
For example, Compose, then \fBe\fP and \fB'\fP
creates an accented e character.
All the vowels can be composed with ' ` and " to get accents,
and there are several more bindings.
The complete table of compose key sequences is given by the
\fICompose Key\fP entry in the Bindings... menu.
.PP
When you compose a mail message that contains 8-bit characters you
need to be careful when sending it.  Some mail gateways do not
like 8-bit characters and will bounce the message or corrupt it.
The Quoted-Printable encoding will protect 8-bit characters.
The built-in editor will do this quoting for you if it detects
input of an 8-bit character.  The quoting is done when you press Send.
You can control whether or not this happens with a combination of the
Quote Printable menu entry (for per-message control) and the
Quoted Printable default preference item for the Simple Editor.
.PP
The built-in editor handles the special coding required when
including 8-bit characters in mail headers.
.PP
When you are replying to a message that contains 8-bit characters,
it may be encoded, too.
The Insert @ operation will first decode the message so that the
true 8-bit characters are inserted into the editor buffer.
This works better than the repl filters than include the message body.
If you use one of those, then you'll see the quoted-printable encoding
instead of the 8-bit character.
.SH "USING ANOTHER EDITOR"
.PP
You can use your favorite editor with exmh by specifying an
\fIEditor command\fP in the Editor Support Preferences section.
This command has the complete pathname of the draft message
appended to it before it is executed.
Several examples of editor commands include:
.DS
sedit
mxedit
emacsclient &
gnuclient &
exmh-async emacs
exmh-async emacsclient
exmh-async gnuclient
exmh-async xterm -e vi
.DE
The built-in editor is called \fIsedit\fP.
\fIMxedit\fP and \fItkvi\fP are Tcl-based editors that
know how to communicate with \fIexmh\fP about draft messages.
\fIemacsclient\fP is a program that is used to communicate with
a running \fIemacs\fP in order to get it to edit a specified file.
You will need the "server.el" package for emacs in order to set up
your emacs as a server for this arrangement.  There is more information
about this in the misc directory of the \fIexmh\fP distribution.
\fIemacsclient\fP can used in two ways.
If you can post the message directly from
emacs, then just use "emacsclient &" and \fIexmh\fP forgets about the
draft and assumes \fIemacs\fP will take care of it.   Otherwise,
use the exmh-async wrapper that is described below.
.PP
After the editor exits you are presented with a
\fIWhat Now\fP dialog that gives you several options:
Send, Abort, Save, Re-edit, and More...
Under the More.. menu you can apply MHN, run a spell checker,
or run the MH whom program to verify the addresses you have specified.
You can also invoke the built-in editor, \fIsedit\fP, in case you
want to use its MIME composition features.
.PP
There is an "Alternate Editor" preference setting.
The built-in editor has a menu entry that saves the draft
and starts the alternate editor.  The What Now dialog also
has a menu entry to do this.
.PP
In most cases you will use the \fBexmh-async\fP wrapper script
to run an external editor.  This is a short Tcl/Tk script that runs your
editor and then communicates with \fIexmh\fP when your editor exits.
This means the \fIexmh\fP user interface can remain active while you compose
your message.  You can even compose several messages at once.  You
will get a different \fIWhat Now\fP dialog for each message draft.
.PP
The \fBexmh-async\fP wrapper tries to pass through all the arguments to
the editor command you specify.  However, a couple arguments are picked
up by the Tcl/Tk shell (wish).  Some tricks are played, however, so you can
protect arguments.  For example, the following works:
.DS
exmh-async xterm {-geo 80x40+0+0} -e vi {+c /: *$}
.DE
This protects the -geo argument from exmh-async,
and it protects the complex format of the \fIvi\fP search command from
interpretation by the Tcl parser.
.PP
As a final note, if you try to test exmh-async from the command line,
you need to execute it like this:
.DS
exmh-async exmh xterm -e vi &
.DE
The first argument is the name of the exmh application, which is needed
for the Tk-based communication between exmh-async and exmh.  This argument
does not appear when you define the command in Preferences.
.SH "FILTERING MAIL"
.PP
One of the strong points for \fIexmh\fP is its support for mail filtering
systems that file messages into different folders as they are delivered.
The filtering is done by looking for patterns in the mail headers.
For example, you can put all the mail from your boss into one folder,
and all the mail from a mailing list or digest into another folder.
\fIExmh\fP visually highlights the folders that have new mail.
This depends on the definition of
the Unseen-Sequence profile component, and the support of this
sequence by the mail filtering system.  Both the \fIslocal\fP and
\fIprocmail\fP filtering systems use the \fIrcvstore\fP program
that takes care of these details.
The \fIinc\fP program also adds
messages to the unseen sequence, so even if you do not filter mail
the new messages in inbox will be highlighted by \fIexmh\fP.
.PP
The standard filter used with MH is described in the mhook (or slocal)
man page.  Below is a quick summary of how it works.
You must maintain a file named ~/.maildelivery that has your
mail filter specifications.  The contents of the file are explained below.
If you can forward mail into programs at your site, then put this
into your .forward file to get filtering:
.DS
| /usr/local/mh/lib/slocal -user yourname
.DE
.PP
Alternatively, if you select the "presort" inc-style described below,
then exmh will run slocal for you each time you say Inc.
.PP
If you use the \fIprocmail\fP system, or perhaps supply your own,
use the MH rcvstore program to put messages into folders.
This program updates the unseen sequence so exmh can find the new mail.
Unfortunately, the rcvstore program is in the MH library, so you
must reference it with a long pathname, (e.g.,
/usr/local/mh/lib/rcvstore.)
.PP
A sample .maildelivery filter file is shown below.
Note: this file is ignored if it has group or world write permission.
It uses the
\fIrcvstore\fP program to refile messages into different folders.
The first field is the header to match, and the second is
the value to look for.  This is a case-insensitive string.
The third column specifies an action.  In this case | says to
pipe the message into the program in the last column.
The fourth column provides limited decision making.  In this case
the ? means to try to match this filter if the message has not
already matched a previous filter.  The complete syntax of the
filters is given in the mhook man page.
.DS
subject exmh            |  ?  "/usr/local/lib/mh/rcvstore +exmh"
subject mxedit          |  ?  "/usr/local/lib/mh/rcvstore +mxedit"
subject book            |  ?  "/usr/local/lib/mh/rcvstore +book"
to      journalclub     |  ?  "/usr/local/lib/mh/rcvstore +journal"
from    footbag-digest" |  ?  "/usr/local/lib/mh/rcvstore +footlist"
default -               |  ?  "/usr/local/lib/mh/rcvstore +inbox"
.DE
.PP
\fIWarning!\fP If you use the .maildelivery file to filter messages,
you should always have a default action in the file that refiles the
messages into your inbox (or some catch-all folder).
Depending on your local configuration, there might not be
a good pre-default default action.
If there is no default action, mail that does not match a filter
will be discarded!
.PP
A patch for the slocal program is distributed with exmh.
It adds an additional syntax to the maildelivery file that makes it
simpler to use rcvstore.  Use the + operation instead of | (pipe)
to specify a refile action.  The above example looks like this:
.DS
subject exmh            +  ?  "exmh"
subject mxedit          +  ?  "mxedit"
subject book            +  ?  "book"
to      journalclub     +  ?  "journal"
from    footbag-digest" +  ?  "footlist"
default -               +  ?  "inbox"
.DE
.PP
\fBIncorporating Mail\fP.
The filtering support affects the way you incorporate mail from
your system spool file.  \fIExmh\fP supports several different styles:
\fIinbox\fP, \fImultidrop\fP, \fIpresort\fP, \fIpresortmulti\fP and \fInone\fP.
Use the \fIIncorporate Mail\fP preference section to choose one.
You may also want to enable periodic inc under the \fIBackground Processing\fP
preferences section.
.IP inbox
Inc moves new messages into your inbox folder.  This is the default.
No filtering is done, but the inbox folder label will highlight when
new mail is incorporated into it, and unread messages will be
highlighted.
.IP none
If you use your .forward file to process mail through an
external program in order to deliver messages into various folders,
then you don't need Inc in the user interface.  Selecting "none"
will eliminate the Inc button altogether and disable the internal
Inc procedure.
A background "flist" task is a nice complement to this setting so
that you can see what folders are getting mail.
.IP presort
Inc moves new messages directly into various folders.
This is similar to what you may be doing already with
an external program.  This is built into exmh in order
to eliminate the need for an external program.
(I cannot use a .forward file at my site.)
The way presort works is to inc from your spool file
into a temporary folder (MyIncTmp).
Then, the MH filtering hook \fIslocal\fP is run
on each message.  In this mode, it is expected that
the filtering action is to put the message into
a folder.  You do this by specifying an action that
is to pipe the message into the MH \fIrcvstore\fP program.
See the MH man page for \fImhook\fP for details.
.IP multidrop
This assumes that you use POP or the 
MH maildelivery facilities (see \fImhook\fP) to
put new messages into a set of dropboxes (inbox-like files).
You set up the correspondence between POP hosts and folders,
or between inbox files and folders with your ~/.xmhcheck file.
Inc moves messages from the POP server or dropboxes into folders.
The format of each line of ~/.xmhcheck file is shown below.
In the first case, the file name must be an absolute pathname
(i.e., has leading /).  In the second case (POP),
the third field is an optional user ID.
.IP presortmulti
This is a combination of multidrop and presort.
Any dropbox that is associated with the MyIncTmp folder gets filtered
just as with presort.
.PP
The \fB.xmhcheck\fP file.
The following shows the format
of the .xmhcheck file.  If the folder_name is MyIncTmp and you
use presortmulti inc style, then messages are filtered after they
are fetched from the dropbox.
.DS
folder_name	/filename/of/dropbox
or
folder_name	POP_hostname [POP_user_name]
.DE
.SH FACES
.PP
One of the more fun features of exmh is its ability to display the facesaver
bitmap of the person that sent you mail.
There are two sources of the bitmap images, either
the facesaver database or inline X-Face mail header fields.
These two sources can be used independently, but the main trick
is getting \fIexmh\fP configured properly to use them.
.PP
\fBFacesaver Database\fP.
The exmh installer asks for two pieces of facesaver-related information:
the root directory of your faces installation and a search
path of faces directories.  This assumes that you have retrieved the
faces software and associated databases and installed them under
one directory, which is referred to as its root directory.
The default is /usr/local/faces.
Underneath this directory there is assumed to be one or more
directories that contain facesaver databases.
It is these directories that are named in the search path;
the names are relative to the root of the faces installation.
Typically there is facesaver, logos, and a local database,
which is called "parc" at my site.
The first two databases come from the facesaver.tar.Z and logos.tar.Z
files, respectively.  The default values are:
.DS
Faces Root Directory:	/usr/local/faces
Faces Search Path:	parc logos news facesaver
.DE
.PP
If you are setting up your own database, you need to understand how it
is organized.  It is easiest to demonstrate by example.  For myself,
welch@parc.xerox.com, my facesaver image is found as
com/xerox/parc/welch/face.xbm.
\fIExmh\fP will look for this file under each of the directories named
by the faces search path.  The complete pathname might be
/usr/local/faces/parc/com/xerox/parc/welch/face.xbm.
If a bitmap is not found, then
the search algorithm trims off trailing components in an
effort to find a more general bitmap, typically a company or
organizational logo.  At your site, for example, you might only have
/usr/local/faces/logos/com/xerox/face.xbm, which contains the
Xerox corporate logo.
.PP
If your \fIexmh\fP installation is not correct, or if you have a personal
faces database that the \fIexmh\fP maintainer doesn't know about,
then you can also override the install-time settings with the FACEPATH
environment variable.  This is a more traditional search path of
colon-separated directory names.
.PP
\fBX-Face headers\fP.
The X-Face header contains a compressed version of a facesaver bitmap,
which is a 48x48x1 (monochrome) bitmap.  The faces software comes with
some filters and scripts to generate these and decompress them.
The X-Face header was original intended as a way of distributing
your facesaver image so that folks can update their database.
However, at the moment \fIexmh\fP just decompresses the bitmap and
displays it.  It requires a Preference setting for the X-Face pipeline
for this to work.  If the pipeline is blank (the default), no processing
is done.  To decompress and display the header, use a pipeline setting
like this:
.DS
uncompface | ikon2xbm
.DE
The uncompface program comes with the faces software.
It also has a version of \fIikon2xbm\fP, but a faster version of this
program (a C program instead of some scripts) is distributed with
\fIexmh\fP in its misc directory.
Future versions of \fIexmh\fP will cache the results of decompressing
the X-Face line in a personal faces database, but in the current
version it just writes the file to /tmp/FACES.[pid].
If you are inspired you could add a Hook_MsgDisplay to stash this
file into a real facesaver database.
The \fBexmh-custom\fP man page describes hook procedures.
.SH SEARCHING IN EXMH
There are several ways to search for things in exmh:
.IP "Find in message body"
Searching uses regular expression syntax, and case is ignored.
.IP "Find in table of contents"
Searching uses regular expression syntax, and case is ignored.
This just searches over what you can see in the display.
If you select All, then all matching messages are selected.
.sp 1
Hint: Use the folder-More... menu entry entitled "List only selected messages"
to get a listing that only contains the selected messages.
.IP "Pick by attributes"
A general search (i.e, MH pick) over the messages in the folder.
This can run slowly because each message in the folder must be
processed.  You can match on various header fields by using the
"Choose pick option" menu in the Pick dialog.
If you choose more than one option, the messages must match all of them,
unless you use the Not and Or buttons to build up more complex criteria.
This interface is explained in more detail in the \fIexmh\fP man page.
.sp 1
The "Add to Sequence(s)" option will add the matching messages to
an MH sequence.   Just pick a meaningful name for the sequence.
(Sequences are not supported well by exmh, yet.)
The "Pick from Seq/msg(s)" lets you narrow the search to a sequence
(e.g., "unseen" or something you defined yourself) or a set of messages
(e.g., last:100 for the last 100 messages in the folder).
.sp 1
The "Add to Sel" option determines if the matching messages are
added to the currently selected set, or if a new set is created.
.IP "Glimpse full text"
Glimpse is a full text searching tool from the
University of Arizona.  Invoke the "Glimpse full text"
menu entry under the Search... menu
to bring up an interface to this tool.
You must first index your mail,
so to get started click "index" to build your indexes.
This process runs in the background, and the status of the
index process is displayed in the Glimpse log.
The storage overhead of the indexes is about 10% to 15%,
which is quite good for a full text index system.
.sp 1
To search, enter a word in the search area and press "Search".
The button changes to a "Stop" button, although the stop may
take a moment to take effect.  The search results are displayed in
the log.  Click on the message in the log and exmh will visit
that message.
.sp 1
Glimpse supports approximate pattern matching, so under the Opts...
menu you can choose how many errors are allowed in a word match.
You can also control if the search is case sensitive or whole word.
.sp 1
Exmh creates one glimpse index per mail folder.  These are stored
under the ~/Mail/.glimpse directory.  This lets you limit the
search to the current folder, or to the current folder and its
subfolders.
.sp 1
To update the index, just press index again.  If a folder has not changed
then the index is not rebuilt.  While Glimpse has an incremental
reindex feature, exmh does not use it.  There appear to be bugs in it.
.sp 1
For more information about Glimpse, see this URL:
http://glimpse.cs.arizona.edu:1994/
.SH "TIPS"
.PP
\fBBackground Processing\fP.
You can set up \fIexmh\fP to periodically
incorporate mail (i.e., \fIinc\fP) for you.
If you already do this via an external agent, then you should
have \fIexmh\fP periodically  check for
new mail in your folders, which is the \fIflist\fP action.
The folder highlighting and icon feedback
work best with \fIinc\fP and \fIflist\fP.
You can also have it periodically
run \fImsgchk\fP or count messages in your spool file, but the
disadvantage of doing this is that
there is no audible or icon feedback when it sees mail waiting in
your spool file.  Instead, there is just a status message that you might
not notice.  Choose what background action is taken and how
frequently it is done with the Background Processing preferences section.
.PP
\fBScan Caches\fP.
\fIExmh\fP maintains a cache of the scan output for each folder.
If you run MH programs from the command line it can invalidate the
cache.  You can manually update the cache with the \fIRescan Folder\fP
operation under the Folder More... menu.  There are also menu entries that
update scan caches for all your folders.  These run in the background
so the user interface can remain active.
The scan cache is compatible with \fIxmh\fP.
.PP
\fBPerformance\fP.
If you want message display to go as fast as possible, do three things.
1) Disable the X-Face pipeline command so you do not try to decompress
X-Face headers.
2) Disable the facesaver database, which is distinct from the X-Face pipeline.
Actually, this is only slow on the first message you receive from 
a new email address.  The pathname of the bitmap image that corresponds
to that address is cached to avoid the expensive lookup the next time
you view a message from that address.
3) Disable the Graphic Separator in the MIME preferences.  That uses
a 3D line of text instead of a blank line, and it takes a bit longer
to display.
.PP
\fIexmh\fP does not like really big folders.  If you have more than
several hundred messages in a folder you will notice that folder
change times get pretty slow.  Take this as a nudge to reorganize
things into subfolders.  Every three months I move saved messages
into subfolders with names like 94Q2, 94Q3, etc.
I understand the performance problems involved, and plan to
overhaul some of the exmh internals to improve this situation.
In the meantime...
.PP
Really big folders with lots of messages in the unseen sequence are
really slow.  Use the "Catch-up unseen" folder More... operation to clear
the unseen sequence.  You probably want to do this when you import mail
from other systems mailboxes.
.PP
\fBMore Keyboard Stuff\fP.
If you really like using the keyboard instead of the mouse,
you can change folders, set the target folder, and select messages
by number with keyboard commands.
When you
type plus ('+'), focus warps to the status line so you can
type in the name of a folder.  In this mode,
a plus cycles between choosing the target folder for moves or
a folder to change into.  <space> does folder name completion,
<Return> accepts the folder name, and <Control-c> cancels the folder
selection.  If you start by typing a number (not plus), that message
is selected, but it is not displayed until you press <Return>.
.SH INSTALLATION
Installation is done via the exmh.install script.  Invoke it as:
.DS
    wish -f ./exmh.install
.DE
This puts up a dialog box that lets you patch various file system
specific pathnames and enable or disable features that your
system cannot support (e.g., facesaver, sound).
Read the info in the dialog box and enter the appropriate file system
pathnames.
Click \fBPatch\fP to sed the exmh.MASTER script and create
the main exmh file, then click \fBTclIndex\fP to generate the library index,
and click \fBTest\fP to try it out.
If you are satisfied,
click \fBInstall\fP to copy the main script and the supporting
libraries into their destination directories.
.PP
Note: the "Script Library" and the "Install lib directory"
are usually the same place.  The script library is the run-time value
of the library directory, while the install lib directory is the
install-time value.  During the Test run, the Script
library is automatically set to "./lib" so you can run without installing.
When you hit
the Patch button, some consistency checks are done and a relative
pathname will trigger a warning.  You can ignore this when you are
testing, but you should install a working copy that only references
absolute pathnames.
.PP
NOTE: Currently you cannot leave the script library where it gets
unpacked.  The installation procedure really assumes that
the Install Directory is a different place than ./lib.
You'll get an error during the install, and in fact,
background.tcl will get removed as it tries to copy it
onto itself.
.PP
The original motivation for install-directory vs script-directory was
AFS installations in which writable volumes have different names than
their read-only, replicated volumes.  In this case you want to install
to the writable volume, but run from the read-only replicas.
.PP
If you get the Script library wrong, it will be manifest as
a Tcl error about an undefined procedure named Exmh.
.PP
Upon startup, the install will look for ../*/.exmhinstall so you
can retrieve configuration information from previous versions
installed in peer directories.
You can also maintain configuration information via the "Conf" button,
which looks for ../*/.exmhinstall*.
.PP
A patch to the MH \fBslocal\fP program is included as "misc/slocal.patch".
The patch adds a "+" syntax to your .maildelivery file that
results in messages being filed into a folder by means
of the MH \fBrcvstore\fP library program.  You can keep your patched copy of
\fBslocal\fP distinct from the installed version because \fIexmh\fP
runs \fBslocal\fP directly.  The installation dialog lets you set
up the pathname for your custom copy.
.PP
There is also a patch for the MH folder.c program.
The patch makes "folders -recurse" run much faster because
it avoids directories that do not have subfolders.
The patches apply to MH 6.7.
.SH "UPGRADING WITH PATCHES"
.PP
There are usually patch files to go between releases (e.g. from 1.6.4 to 1.6.5).
The best way to use these patches is to patch your 1.6.4
.B source directory
and then reinstall using the procedure described above.
This has the benefit of fixing the version number in the main script.
Finally, this is how the patches are generated, so if you apply them
to your installed directories they might not work properly.
.SH "TK SEND AND XAUTHORITY"
.PP
The thing that causes the most trouble with new exmh users is the
Tk send facility and the Xauthority mechanism.  This matters if
you use the detached background process or the \fBexmh-async\fP
editor wrapper.
.PP
The send facility is used for communication between Tk interpreters.
The most recent releases of Tk require that you have your X environment
set up to use Xauthority.  The details for doing this vary from
X server to X server.  The basic idea is that, \fIbefore\fP
you start the X server you create a file, ~/.Xauthority, that contains
a random bit string.  The file is only readable by your user account.
You pass the name of this file to the X server when you start it up.
Then, each time a window is created, the client programs read this
file and pass the random bit string to the server.  The idea is that
only programs run from your account can read the file to get the
right bit string.  For the details, read about the -auth or -xauth
argument to your server.
.PP
Even if you get this right, you must also make sure that the old
\fIxhost\fP security system is not used.  That is, the xhost list must
be empty.  To do this, run the following command:
.DS
xhost -
.DE
.PP
With release 1.6.4 and later you can have exmh police your xhost list.
It can clear out any hosts that creep onto the list.  This typically
happens with wrapper scripts used to run other programs.  If you use
Xauthority properly, the additional use of xhost is completely bogus
and insecure.  Not to mention that it breaks Tk send.  Under
Background Processing preferences, select "Keep xhost list clear".
.PP
The other alternative is to recompile the Tk library with the
-DTK_NO_SECURITY compile flag.  This is appropriate if your X server
just cannot do the Xauthority protocol, or you think your environment
is safe enough.  That is, you don't have to worry about other users
popping up rogue windows on your display or directing your tk applications
to delete all your files.
.SH "MORE INFORMATION"
You are welcome to send bug reports and comments about exmh
to the following email address: Brent.Welch@Sun.COM.
.PP
Exmh is available via FTP on parcftp.xerox.com in the pub/exmh directory.
.PP
MH is available as pub/mh/mh-6.8.tar.Z from ftp.ics.uci.edu
.PP
Tcl and Tk are available in the ucb/tcl directory on ftp.cs.berkeley.edu
.PP
Expect is available as pub/expect/expect.tar.Z from ftp.cme.nist.gov
.PP
Metamail (for MIME support) is available in thumper.bellcore.com:pub/nsb
.PP
Faces are available from cs.indiana.edu:/pub/faces.
The individual databases are located under /pub/faces/picons/db,
and they include .xbm, .xpm, and .gif representations.
I recommend getting domains.tar.Z, misc.tar.Z, and unknown.tar.Z.
If you have room, also get usenix.tar.Z and users.tar.Z.
.DS
Database           Date            Bytes       Description
domains.tar.Z      95.12.06        1356923     Company, Country and Univ Logos
misc.tar.Z         95.11.12        58991       Things like root and postmaster
news.tar.Z         95.12.06        372765      For newsgroup (not mail)
unknown.tar.Z      95.11.12        19074       Default images
usenix.tar.Z       95.04.13        19472394    The USENIX database
users.tar.Z        95.12.08        2012905     Other personal faces
weather.tar.Z      95.11.12        146329      For weather (not mail)
.DE
Glimpse is available from the University of Arizona:
http://glimpse.cs.arizona.edu:1994/
or ftp://cs.arizona.edu/glimpse
.PP
To get PGP, send mail to pgp-bugs@mit.edu.
.SH FILES
.PP
Your mail is collected under a directory usually called Mail.
If you are using another mail system that uses that directory,
set your Path MH profile entry to something else.
The current folder and other
information is stored in a file named "context" in your Mail directory.  
Your mail is stored as one message per file in directories that
correspond to your mail folders.  Nested folders are supported, up to
the maximum depth supported for directories in your file system.
Information about message sequences is kept in a file named .mh_sequences
in each mail folder directory.  The current message
is in the sequence named "cur", for example.  New mail is in
the sequence named "unseen".  You can define more sequences,
up to 10 per folder,
but exmh doesn't support them very well.
.PP
Exmh adds a few more files for its own purposes.
It uses an alternate context stored in .exmhcontext so
that command line MH programs and exmh do not interfere
with each other.
The .folders file lists all the mail folders you have.
Each folder has a .xmhcache file that has a cache of
the scan output.

.SH SEE ALSO
exmh, exmh-ref, exmh-custom, mh
.SH AUTHOR
Brent.Welch@Sun.COM
.SH THANKS
To Xerox PARC/CSL, for supporting this work initially,
to Sun Microsystems Laboratories for continuing the support,
and to all the exmh users who contributed ideas and code.

