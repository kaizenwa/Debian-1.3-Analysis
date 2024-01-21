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
.TH "EXMH CUSTOM" 1 "August 22, 1996" "Exmh 1.6.9" "CUSTOMIZING EXMH"
.SH NAME
exmh-custom \- A guide to customizing the exmh mail user interface.
.SH "CUSTOMIZING EXMH"
.PP
This man page describes the four mechanisms used to customize \fIExmh\fP:
Preferences, the MH profile, X resources, and custom Tcl code.
.PP
\fIExmh\fP is built with the assumption that you will want to
customize it to some degree.
The simplest way is by the Preferences user interface,
which exposes numerous knobs and dials that you can adjust to
control a lot of the behavior of \fIexmh\fP.
The second way is by defining X resources.  You will need to do this
if you want to control fonts and colors.  It would be great if there
was a user interface for this, but this is something that has not
been crossed off the TODO list, yet.
You also use X resources to define new buttons and menus.
The third way is by adding custom Tcl code to the implementation
of \fIexmh\fP.  A personal library of Tcl routines is supported.  You 
can either add new buttons or menus to invoke your functionality,
or take advantage of some hook points inside \fIexmh\fP to slip
in your new feature.  It is also possible to completely replace
any module of the \fIexmh\fP implementation.
Finally, there are a few MH profile components introduced by
\fIexmh\fP, although these may eventually migrate out of
the profile and into the Preferences package.
.SH PREFERENCES
.PP
After you have used \fIexmh\fP a little, you should explore all
its capabilities by clicking on the Preferences button.
There is a two-level preferences scheme, mainly because
there are too many knobs and dials.  At the top-level you
see a menu that corresponds to different modules of
the implementation.  Clicking on one of the items brings up
the preferences items for that module.
Use the Help button to display more detailed information
about the preference items.
If you click on the label of an item, the help text
is scrolled to the information for that item.
.PP
There are three types of options you can set through the Preferences dialog:
choices, booleans, and general items.
Each of these are tied to a Tcl variable and an X resource name.
Changes in the Preference user interface change the value of the Tcl
variable, which affects the \fIexmh\fP runtime behavior.
Then, when you click \fBSave\fP in the dialog, the values are saved as
X resource settings in your ~/.exmh-defaults file.
.PP
Choices are represented by radio-style buttons where only one button
in the set can be enabled at once.  Changes take effect immediately.
Booleans are represented by check-style buttons.  If the checkbox is
dark, then the option is turned on.  Changes take effect immediately.
Numeric and filename settings have entry widgets in which you can
type in a new value.  Press <Return> for the change to take effect
immediately (or choose \fBSave\fP).
.PP
You can cycle through all the preference dialogs by using the \fBNext\fP
button, which takes you to the next preference section.
There is also a Prev button to go back.  You should take time at least
once to go through all the Preference sections to get an idea of
what sort of options are available.
.PP
If you decide you like your settings, click \fBSave\fP in the main
Preferences dialog to save
them in a .exmh-defaults file in your home directory.
Click \fBReset All\fP in the main dialog to restore all the settings
to those of your last \fBSave\fP.  Within each module's preference dialog
there is a \fBReset\fP button that resets only those module's settings.
.PP
\fIWarning!\fP
If you click \fBDismiss\fP in the main dialog, some preferences may have
been set for the current session, but they will not have been saved to
your ~/.exmh-defaults file.
.SH PREFERENCE SECTIONS
.PP
Here is a short summary of the preference sections and what
features they control.  The Tcl module that the section
corresponds to is listed so you can dig into the code if you want to.
.IP "Fonts"
This is a custom dialog for picking font resources for different
widget classes.
.IP "Hacking Support"
A debug log can be enabled, and you can define the directory
for personal Tcl code. (main.tcl)
.IP "MH Tweaks" 
Background sending can be enabled. The naming convention for
deleted files (leading , or #) is set.
The age of files to purge can be defined. (mh.tcl)
.IP "Simple Editor" 
Formatting parameters can be adjusted
and automatic signatures can be enabled. (sedit.tcl)
.IP "Editor Support" 
An external editor, spell program, and MHN command can be defined. (editor.tcl)
.IP "Printing" 
The print command can be defined.  You can also enter an
arbitrary UNIX command to apply to a message. (print.tcl)
.IP "Scan Listing"
You set the number of lines in the scan listing here.
There are several tweaks on message viewing: Implied Direction,
Next Guard, Auto Commit, Advance After Link, Show
New Messages, Skip Marked.  You can also choose the
scan format width here. (ftoc.tcl)
.IP "MIME" 
There are several adjustments you can make to the MIME message display.
Note that the font sizes chosen here do not affect non-MIME
messages. (mime.tcl)
.IP "WWW"
Exmh can communicate with an external HTML viewer (e.g., Mosaic or netscape)
in order to display pages from the World Wide Web.
It can scan the current message for embedded URL.
The URLs are changed into active text buttons.  Click on one
and the web browser is asked to display the page.
Use the preferences to choose the external viewer and to
control if the URL scanning is done automatically. (uri.tcl)
.IP "Incorporate Mail" 
The method you use to Inc can be set.
You can set inc to run when you start exmh and when
you open the exmh window. (inc.tcl)
.IP "Windows & Scrolling"
Scrolling speed and parameters related to constrained text
scrolling can be defined.
Constrained scrolling keeps the last line of text
stuck to the bottom of the text display. (exwin.tcl and widgetText.tcl)
.IP "Folder Cache" 
Set how many lines are in your folder cache display, and
what folders are permanently in your folder cache. (fcache.tcl)
.IP "Folder Display" 
Set the number of rows of labels in the folder display.
The style of nested folder display is controlled here. (fdisp.tcl)
.IP "Sound" 
The sound effects can be controlled. (sound.tcl)
.IP "Faces" 
The use of the facesaver database and the decompression
of X-Face components can be controlled. (faces.tcl)
.IP "PGP interface" 
You choose whether \fIexmh\fP can remember your pass phrase or
if all PGP programs are run under an xterm instead. (pgp.tcl)
.IP "Background Processing" 
What actions occur in the background, and how frequently. (background.tcl)
.IP "FS Box"
The file selector has a few parameters, including the threshold size
for large directories and whether or not you want to see files
whose names begin with a period.
.IP "Busy Indicator" 
What method of signaling that exmh is busy. (busy.tcl)
.SH BINDING UI
.PP
There are a number of keystroke bindings already defined by \fIexmh\fP
that invoke different Tcl commands.  You can change the bindings
and add bindings for new commands via the Bind dialog.
Open the dialog from the \fICommands\fP menu entry under the Bindings menu.
The dialog presents a scrollable column of commands and their bindings,
plus an area at the top to define a new binding.
.PP
\fBBinding Syntax\fP.
The following is a very brief summary of the Tk bind syntax.
For the complete story,
consult the Tk man page for the \fIbind\fP command.
The Tk syntax for the bindings events looks like this:
.DS
<\fImodifier\fP-\fItype\fP-\fIdetail\fP>
.DE
A \fImodifier\fP is a key that you hold down while pressing another key.
The modifiers you are likely to use are listed below.
Capitalization is important.
.DS
Control
Shift
Meta
.DE
The \fItype\fP is the event type, and it can left out if the detail part
implies the type.  The types you will usually use are:
.DS
Key
Button
.DE
The \fIdetail\fP specifies the key or button number for the event.
Keys are named by their X keysym.  For the letters and digits, the
keysym is just the letter or digit, e.g., <Key-a>, which can
be shortened to <a>.  For punctuation, however, the keysyms are words.
Here are some examples, and again the Key type is left out.
.DS
<comma>
<period>
<dollar>
<asciicircum>
<question>
<exclam>
.DE
The \fIBindings... Simple Edit\fP dialog has a little event watcher that can help
you determine the keysyms used on your system.  You can also experiment
yourself.  Run \fIwish\fP (the Tcl/Tk shell) and enter the \fIbind\fP command shown below.
Then type with the mouse over the little window it displays.
.DS
bind . <Any-Key> {puts stdout "keysym = %K letter = %A"}
.DE


.SH MH PROFILE
.PP
\fIExmh\fP uses a couple of things from your .mh_profile file, including
several components that are new.
.IP "Header-Suppress and Header-Display"
You control what headers are displayed in a message with a combination
of the Header-Suppress and Header-Display profile components.
Hidden headers are just scrolled off the top of the message display window.
Each of these profile components is a list of \fIregular expression patterns\fP
that are used to
match against the header.  Case is \fInot\fP significant in the patterns.
Its easiest to explain by giving the algorithm that uses these patterns.
.DS
By default, show all headers.
If a header is in the Header-Suppress list, do not show it.
If a header is in the Header-Display list, show it.

The default values for these profile components are:

Header-Suppress: .*
Header-Display: Subject To From Date Cc
.DE
.PP
If you are a mail junkie, you may want to use Header-Suppress to
explicitly suppress the boring header componets you already
know about.  The new, interesting
components inserted by random mailers will be displayed for
you to check out.  In contrast, the default for Header-Suppress will
hide everything, and you explicitly
choose what headers you want to see by setting Header-Display.
.IP Folder-Order
The Folder-Order component defines a sort ordering for your folder labels
in the folder display area.  Each item in the order can be the
name of a folder, or a \fIstring match\fP pattern to match on the folder names.
All folder names that match the same pattern are sorted alphabetically.
Longer pattern matches have priority over shorter patterns.
The patterns use the syntax of Tcl's string match function, which is
similar to that used in many shells.
.DS
* matches a sequence of any characters.
? matches any character.

The default Folder-Order puts your inbox first.

Folder-Order: inbox *

My Folder-Order looks like:

Folder-Order: personal exmh mxedit * mail* sun m3 mach background

.DE
.PP
The other effect of Folder-Order is to define a traversal order for
visiting folders with unread mail in them.  When you do a Next and are
at the end of a folder, \fIexmh\fP will automatically change folders
to the next one in the Folder-Order that has unseen messages, if any.
When there are no more folders with unseen mail, then you will change
back to the first folder in your Folder-Order, unless you disable
this by turning off the \fICycle back to first\fP preference setting
under the Unseen Folders section.
.IP Folder-Unseen
The Folder-Unseen component lets you constrain
the search through folders for the
ones with messages in the unseen message sequence.  Its value is a set
of \fIstring match\fP patterns that are matched against folder names.
If a folder name matches the pattern, it is searched for unseen messages.
If the pattern begins with a !, however, then a folder that matches
the pattern (not including the !) is excluded from the search.
The default is *, which matches everything.  Put the negated patterns
first in your list of patterns (e.g., !junk* inbox*).
.IP Folder-Ignore
The Folder-Ignore component specifies a set of \fIstring match\fP
patterns for folder names
you want to ignore.  These folders are not even displayed by exmh.
It defaults to ".* */.* */*/.* */*/*/.*" , which causes \fIexmh\fP to
ignore directories whose name begins with a period.
.IP Draft-Folder
The Draft-Folder component is used to determine where to put messages being composed.
\fIExmh\fP will ask you if it is ok to create a Draft-Folder entry
if you do not already have one.
.IP ExmhShowProc
The ExmhShowProc component lets you define a program that pre-filters
a message before displaying it.  If an ExmhShowProc is defined,
then \fIexmh\fP runs that program with the current message as the
standard input and displays what is generated on the program's standard
output.  Note that the Header-Suppress and
Header-Display mechanism is still used even if you have a special
show proc.
.IP Scan-Proc
The Scan-Proc component can be used to define an alternative scan program.
If you change the scan format, you need to make sure that the first
item on each scan line is the message number.
\fIExmh\fP depends on this.  (It makes no attempt to decipher scan formats.)
.IP MailDrop
The MailDrop component is required if you do not have your system
mail spool file in the "standard" location, which is typically
/usr/spool/mail/\fIusername\fP.  If you do not define MailDrop
correctly then Inc will not do anything because it will not find
your new messages in the system spool file.
.IP Path
The Path component is used to find your mail folders.  \fIExmh\fP will
abort if this entry is not there on the presumption that
you have not set yourself up to use MH properly.
.SH X RESOURCES
.PP
The X resource database is used as a repository of Preference settings,
window positions, and definitions of fonts, colors, buttons, and menus.
The information in the database can come from a variety of sources,
which can be confusing.
The default values come from the \fBapp-defaults\fP file that is kept
in the script library directory for \fIexmh\fP.
Color-specific resources are contained in the
app-defaults-color or app-defaults-mono file.
One of these two is used depending on the display.
.PP
A site administrator can add local resource specifications in the
local-app-defaults file.  Put this into the exmh script library
directory (the same place as app-defaults).
To handle site-specific color-specific resources,
\fIexmh\fP will also read the local.app-defaults-color
or local.app-defaults-mono if those files exist.
.PP
Each user has a ~/.exmh-defaults file in their
home directory.  To handle personal color-specific resources,
\fIexmh\fP will also read your ~/.exmh-defaults-color
or ~/.exmh-defaults-mono if those files exist.
.PP
I do not recommend putting exmh-related resource settings in your
~/.Xdefaults, although you can do that.
If you do, be warned that values from the ~/.Xdefaults file and
the RESOURCE_MANAGER property on the root window will be
overridden by things in your ~/.exmh-defaults file.
.PP
The ~/.exmh-defaults file is divided into sections.
The first section is for things you add by hand.
The remaining sections are automatically managed by \fIexmh\fP.
If you manually add settings to your ~/.exmh-defaults file, 
add entries to the \fIbeginning\fP of this file.
Add them  before the
comment about the rest of the file being automatically
generated so that you will not lose your changes.
.PP
If a resource has a multiword
value, you *should not quote* the value in the resource file.
The right way to specify these in your ~/.exmh-defaults file is shown below.
The leading "*" gets around quirks in the way Tk names its applications;
different instances of the application have different names.
.DS
*scrollbarSide:	left
*c_current: violet red
.DE
.PP
Finally, if you are really serious about fiddling with resources, you
should look through the app-defaults file.  For one thing, there is
no guarantee that the resource names used in this man page,
which correspond to version 1.5, will be exactly the same in
later versions of \fIexmh\fP.
Furthermore, there might be new goodies
that appear in future versions that are not described here.
Only by reading the app-defaults file of the current
version will you be sure you are setting things correctly in
your ~/.exmh-defaults file.
(\fIHint:\fP read through the main exmh script for the definition
of the exmh(library) Tcl variable, which is the script library
where app-defaults lives.  The script is short, and the definition
is near the beginning.)
.SH WIDGET CLASS HIERARCHY
.PP
If you want to dive into the widget tree and fiddle with fonts and colors
and such, here are the class descriptions.  I also highly recommend
the \fItkinspect\fP program, which you can find in the Tcl archives.
.IP Main
Top row of buttons and title label
.IP Fdisp
Folder label display
.IP Fltop
Folder label display when it is in a detached toplevel.
.IP Fops
Folder operation buttons and folder label
.IP Ftoc
Folder table of contents display
.IP Mid
Frame around Face, Msgid, Status, Mops
.IP Mid.Face
Bitmap display
.IP Mid.Right.Status.label
Message label
.IP Mid.Right.Status.msg
Status line
.IP Mid.Right.Mops
Message buttons
.IP Msg
Message display
.IP Clip
Detached message display
.IP Sedit
Simple editor top-levels
.IP Help
Help window
.IP Key
Color key window
.IP Pref
Preferences dialogs
.IP Log
Error/debug log
.IP Pick
The pick dialog
.IP Glimpse
The glimpse dialog
.IP NewFolder
The new folder dialog.
.IP DeleteFolder
The delete folder dialog.
.IP WhatNow
The What Now? dialog.
.IP Error
Error popups
.IP Dialog
General popups
.SH RESOURCES FOR BUTTONS
.PP
\fIexmh\fP uses X resources to specify its
buttons and menus on the main display, the editor window,
and the What Now dialog.
You can add a button to one of these areas of the user interface
by listing it in a \fBubuttonlist\fP resource
and then adding
some more resources that describe the button.
X resource names are hierarchical, and these are the button list
resources used by \fIexmh\fP.
.DS
*Main.ubuttonlist
*Fops.ubuttonlist
*Mops.ubuttonlist
*Sedit.Menubar.ubuttonlist
*WhatNow.ubuttonlist
.DE
\fBFops\fP is the set of folder operation buttons.
\fPMops\fP is the set of message operations buttons.
\fBSedit.Menubar\fP is the set of buttons in the built-in editor.
\fBWhatNow\fP is the What Now dialog used with external editors.
The \fBubuttonlist\fP
resource is necessary because there is no easy way to enumerate the
contents of the resource database.
.PP
There are actually several resources associated with each set of
buttons in order to provide maximum flexibility.  
There are three sources of button definitions:
system buttons are defined by the base release ("at the factory");
local buttons are defined by your site administrator;
user buttons are defined by each user.
In addition, the site and the user can delete buttons with other resources.
The resources are:
.DS
buttonlist	The list of system defined buttons
lbuttonlist	The list of local (site) defined buttons
ubuttonlist	The list of user defined buttons
l-buttonlist	The list of buttons deleted at the local level.
u-buttonlist	The list of buttons deleted at the user level.
.DE
When \fIexmh\fP creates a set of buttons, (e.g., the *Main buttons),
it asks for the definition of all these
resources to determine what buttons are being defined
(e.g., *Main.buttonlist, *Main.ubuttonlist, and so on.)
For each of these buttons, additional resources specify the
text label and command for each button.
This is best explained
by an example.  Here are the definitions for the main buttons:
.DS
*Main.buttonlist:       quit pref alias
*Main.quit.text:        Quit
*Main.quit.command:     Exmh_Done
*Main.pref.text:        Preferences
*Main.pref.command:     Preferences_Dialog
*Main.alias.text:       Aliases
*Main.alias.command:    Aliases_Pref
.DE
The \fB*Main.buttonlist\fP resource names the buttons that
appear in the top row of buttons.  Its value, in turn causes
\fIexmh\fP to look around for the other resources that define the
text and command attributes for each button.  The command is a Tcl command,
and most are simple commands of one or two words.
If you are really inspired you can set many different attributes of
a Tk button via resources, but you'll have to consult the Tk man page
on \fIbutton\fP for the details.
.PP
As another example, here is how you would add a Repl button to the
message buttons.  By default, there are a few variations on Reply under
the Reply... menu.  You might like a Repl button that does your most
common form of reply.  The \fBMsg_Reply\fP Tcl command takes regular
arguments for the MH \fIrepl\fP program.
.DS
*Mops.ubuttonlist: myrepl
*Mops.myrepl.text: Repl
*Mops.myrepl.command: Msg_Reply -filter myrepl.filter -cc all
.DE
.PP
If you hate the Reply... menu altogether, you can remove it by
adding it to the u-buttonlist resource.
You'll have to look at the master app-defaults file to find out
the internal name of each button.
.DS
*Mops.u-buttonlist:  reply
.DE
.SH "RESOURCES FOR MENUS"
.PP
The menus in \fIexmh\fP are defined in a similar way.
It is a little more complex because there is more to a menu
than a button, but the general idea is the same.  There are parallel
sets of resources for the system-defined and user-defined parts.
Each section has a list of menus defined
with the following resources:
.DS
menulist	The list of system defined menus
lmenulist	The list of local (site) defined menus
umenulist	The list of user defined menus
l-menulist	The list of menus deleted at the local level.
u-menulist	The list of menus deleted at the user level.
.DE
.PP
Each menu, in turn, has a \fBtext\fP resource that defines
the label on the menubutton.
The \fBentrylist\fP resource lists
the entries that are found under the menu.
Again, the system-defined entries are listed under \fBentrylist\fP,
the administrator defines \fBlentrylist\fP,
and users are meant to add new entries to \fBuentrylist\fP.
System (or local) defined entries can be removed by adding them to
the \fBl-entrylist\fP and \fBu-entrylist\fP resources.
.PP
For each menu entry there are resources with the following naming
convention (this is not standard Tk):
if the entrylist item is \fIfoo\fP, then:
.DS
l_\fIfoo\fP defines the label (text) for the entry.
c_\fIfoo\fP defines the command.
t_\fIfoo\fP defines the type: "command", "check", "radio", "cascade", or "separator".
v_\fIfoo\fP defines the variable associated with check and radio entries.
m_\fIfoo\fP defines the menu associated with cascade entries.
.DE
For more information, it might be helpful to consult the Tk
man page for \fImenu\fP.
For example, here is how the main menus for exmh are defined:
.DS
*Main.menulist:		bind help

*Main.bind.text:	Bindings
*Main.bind.m.entrylist: command sedit
*Main.bind.m.l_command: Commands
*Main.bind.m.c_command: Bind_Pref
*Main.bind.m.l_sedit:	Simple Edit
*Main.bind.m.c_sedit:	Sedit_Pref

*Main.help.text:	Help
*Main.help.m.entrylist:	help colorkey faq
*Main.help.m.l_colorkey:	Color Legend
*Main.help.m.c_colorkey:	Help_KeyDisplay
*Main.help.m.l_help:	Quick Intro
*Main.help.m.c_help:	Help
*Main.help.m.l_faq:	Frequently Asked Questions
*Main.help.m.c_faq:	Help FAQ
.DE
.PP
Note the additional \fB.m\fP component in the \fB*Main.help.m.entrylist\fP
resource name.  The \fB*Main.help\fP resource corresponds to the menubutton,
and \fB*Main.help.m\fP corresponds to the menu associated with that button.
.PP
For another example, we can use
the \fBuentrylist\fP resource to add a new menu entry to
the message More... menu.  It will be a check-button type entry that will set
the Tcl variable that controls the "skip marked" behavior of
Next and Prev.  In addition,
we will separate the user-defined entries from the system entries
with a special separator menu entry.
The resources
in your ~/.exmh-defaults would look something like this:
.DS
*Mops.more.m.uentrylist: sep skip
*Mops.more.m.t_sep: separator
*Mops.more.m.t_skip: check
*Mops.more.m.l_skip: Skip marked messages
*Mops.more.m.v_skip: ftoc(skipMarked)
.DE
.PP
In this case the Tcl variable is \fBftoc(skipMarked)\fP,
which is an element of an associative Tcl array.
Menu entries that use Tcl variables defined by \fIexmh\fP
might stop working in a future release.  However, you can easily
get an idea of what the important variables are by searching through
the code for Preferences_Add calls.  These calls set up the relationship
between the internal Tcl variables and the Preference items you see
in the interface.  In most cases the variables are elements of
an associative array.  In this example, \fBftoc\fP is the array that
holds the state variables for ftoc.tcl, which implements the
scan listing.
.SH BUTTON GROUPS
.PP
When you use \fIexmh\fP, you will notice that some of the message
buttons are disabled when there is no current message.
This is implemented by putting the Mops buttons and menu entries into
groups.  The group membership is defined via resources.
The groups are \fIcurrent\fP, \fIrange\fP, and \fPnodraft\fP.
The current group contains
all the buttons and menu entries that are enabled when
there is a current message.
The range group contains buttons and menu entries that can
be applied to multiple messages.
The nodraft group is for those buttons and menu entries that
ought to be disabled when you are in the drafts folder.
.PP
For each group there are several corresponding resources that
list the buttons, (system, local, and user), and menu entries,
(system, local, and user), in the group.
These are the group-defining resources and (part of) their
default values.
.DS
*Mops.g_current:	link move delete reply forward
*Mops.gm_current:	Print {Unmark (Undo)} Clip Redistribute {Burst Digest}
*Mops.lg_current:
*Mops.lgm_current:
*Mops.ug_current:
*Mops.ugm_current:
*Mops.l-g_current:
*Mops.l-gm_current:
*Mops.u-g_current:
*Mops.u-gm_current:

*Mops.g_range:		link move delete forward
*Mops.gm_range:		Print Unmark {Mark Unseen}
*Mops.lg_range:
*Mops.lgm_range:
*Mops.ug_range:
*Mops.ugm_range:
*Mops.l-g_range:
*Mops.l-gm_range:
*Mops.u-g_range:
*Mops.u-gm_range:

*Mops.g_nodraft:	reply forward
*Mops.gm_nodraft:	Redistribute
*Mops.lg_nodraft:
*Mops.lgm_nodraft:
*Mops.ug_nodraft:
*Mops.ugm_nodraft:
*Mops.l-g_nodraft:
*Mops.l-gm_nodraft:
*Mops.u-g_nodraft:
*Mops.u-gm_nodraft:
.DE
The naming convention for buttons and menu entries is different.
The buttons are named the same way they appear in the \fBbuttonlist\fP
resource specification.  The menu entries are named by their
textual label.  If a menu entry label includes spaces, then the
label must be grouped.  Braces are used for compatibility with the
Tcl grouping syntax.
.PP
\fIWarning!\fP If you 
move things between the *Mops.buttonlist and the
*Mops.more.m.entrylist, then you will have to adjust your group settings
because the naming convention for buttons and menus is
different.
.SH COLOR RESOURCES
.PP
The basic TK widget color attributes are listed below.
Most of these are defined in app-defaults-color to obtain
a family of gray levels.
.IP background
The main background of a widget.  Default is a light gray (#efefef)
.IP foreground
The foreground, (e.g., for text).  Default is black.
.IP activeBackground
The background of a button or menu when the mouse is over it.  Default is white.
.IP activeForeground
The foreground of a button or menu when the mouse is over it.  Default is black.
.IP disabledForeground
The foreground color of a button or menu that has been disabled.  Default is grey50.
.IP selectBackground
The background of text when it is selected.  Default is canary blue.
.IP highlightColor
The color of the focus highlight rectangle when a widget has focus.  Default is black.
.IP highlightBackground
The color of the focus highlight rectangle when a widget does not have focus.  Default is the same gray as background.
.IP selector
The color of the checkbutton and radiobutton glyph when the button is selected.  Default is black.
.IP troughColor
The "other background" for scrollbars and scales.  Default is gray (#dfdfdf)
.PP
You can specify colors for particular classes of widgets.  All the labels
are blue in exmh, by default, because of the following in app-defaults:
.DS
*Label.foreground: blue
.DE
.PP
The following resources are used to control the looks in the scan listing and
folder label display.  The fact that some have a trailing Bg or Fg is
purely historical accident.  Originally you could only specify the
foreground (for current and unseen) or the background (for moved and
deleted) but now, for the benefit of greyscale users, you can specify
all of them.  You might also look at the fdispColor.tcl and ftocColor.tcl
files that use these resources.
.IP c_current
The color for the current message and current folder.  Default is violet red.
.IP c_currentBg
The background color for the current message.  Default is white.
.IP c_unseen
The color for unseen messages and folders that contain unseen messages.
Default is blue.
.IP c_unseenBg
The background color for unseen messages.  Default is normal background
(same as text widget default background).
.IP c_moved
The background color for messages that are marked for refile,
and the background label color for the target folder for refile.
Default is yellow.
.IP c_movedFg
The foreground color for messages that are marked for refile.
The default is normal foreground.
.IP c_deletedFg
The foreground color for messages marked for delete.  Default is normal foreground.
.IP c_foreground
The foreground color for labels in the folder display.  Default is black.
.IP c_background
The background color for labels in the folder display.  Default is white.
.IP c_popup
The color for the popups that display nested folders.  Default is grey.
.IP c_st_normal
The color for normal status messages.  Default is blue.
.IP c_st_error
The color for error messages. Default is purple.
.IP c_st_warn
The color for warning messages.  Default is red.
.IP c_st_background
The color for messages from the background process.  Default is medium sea green.
.IP c_uri
The color used to highlight URL and URN text by the Highlight URI function.
The default is thistle.
.SH COLORIZING HEADERS
.PP
A set of resources are used to specify colors and other font attributes of
message headers.
.IP m_tagnames
This defines a list of mail headers for which you want to define
special looks when they are displayed in the message area.
For each tagname, you should define another resource with name
m_\fItagname\fP
(e.g, m_subject) that has the text tag configuration options
for that header line.  Consult the Tk man page on the text widget
to see what sort of tag configuration options there are.
In addition, two special tagnames are used for defaults if there
are no more specific matching tag name.  "default" applies to
displayed headers, while "hidden" applies to hidden ones (scrolled off the top).
.PP
For example, here is what I
have in my own ~/.exmh-defaults file:
.DS
*m_tagnames:	hidden subject from x-filters-matched
*m_hidden:	-font 6x10
*m_default:	-foreground black
*m_subject:	-foreground blue
*m_x-phase-of-moon:	-foreground blue
*m_from:	-foreground "medium sea green"
.DE
.SH GEOMETRY AND POSITION RESOURCES
.PP
\fPExmh\fP automatically remembers the position of top-level windows,
both within a session and between successive runs of \fIexmh\fP.
It does this by saving some \fBposition\fP resource
specifications in your ~/.exmh-defaults file.
In addition, \fIexmh\fP will remember the geometry of the main window
and of the detached folder display.
.PP
\fIWarning\fP:  If you use a virtual root window manager,
the memory of a window location will cause problems if you
start \fIexmh\fP in a new "room".  If you move \fIexmh\fP to
a new room, you'll have to edit your ~/.exmh-defaults and delete
all the *position resource specifications.
Or, you can turn off the \fIRemember Window Positions\fP
preference item under Window Stuff.
Then, when exmh exits, it removes these for you.
.PP
You can adjust the width and height of
some of the text windows.
Here are the default values.  
.IP *Fltop*Canvas.width: 25
300
.IP *Fltop*Canvas.height: 25
100
.IP *Sedit*Text.width: 25
80
.IP *Sedit*Text.height: 25
24
.IP *Clip*Text.width: 25
80
.IP *Clip*Text.height: 25
48
.IP *Help*Text.width: 25
80
.IP *Help*Text.height: 25
30
.IP *Log*Text.width: 25
80
.IP *Log*Text.height: 25
20
.SH ICON POSIITONS
.PP
The \fBiconposition\fP resources specify the location of
the icons, both for the main window and the detached folder display (if any).
The format is +X+Y to specify the upper-left hand point.
If you use - instead of +, then the position is relative to
the lower-right corner instead of the upper-left corner.
By default no position is specified so your window manager
will place the icon.  The \fBiconic\fP resource indicates the
startup disposition of the window.
.IP exmh.iconposition: 25
(empty - no default position)
.IP exmh.iconic: 25
0
.IP *Fltop.iconposition: 25
(empty - no default position)
.IP *Fltop.iconic: 25
0

.SH ICON APPEARANCE
.PP
You can control the bitmap and the label on the icon.
There are three states for the icon:
no mail, spooled mail, and unread mail.
For each of these states you can define a bitmap and a label.
The label can include references to exmh variables,
as you will see in the default values.
The value for the Bitmap is a file name.  If it is relative
then it is assumed to be in the exmh script library.
Specify an absolute pathname otherwise.
.IP *iconUpBitmap: 25
flagup.bitmap
.IP *iconDownBitmap: 25
flagdown.bitmap
.IP *iconSpoolBitmap: 25
flagup.bitmap
.IP *iconUpLabel: 25
$flist(newMsgs) Unseen
.IP *iconDownLabel: 25
exmh
.IP *iconSpoolLabel: 25
$exmh(numUnInced) Spooled
.PP
You can reference any global exmh variable in the icon label.
The two most useful are $flist(newMsgs), which is a count
across all folders of messages in the unseen sequence,
and $exmh(numUnInced), which is the number of messages in
your system spool file.  This later value is only computed by
the background "count" operation.

.SH FOLDER DISPLAY RESOURCES
.IP fl_font
The font for the labels in the folder display. Default is "fixed".
.IP fl_xgap
The horizontal gap, in pixels, between labels in the folder display.
The default is 8.
.IP fl_ygap
The vertical gap, in pixels, between labels in the folder display.
The default is 8.
.IP fl_curbutton
The button that choses the current folder in the folder display.
The default is 1. (1=left, 2=middle, 3=right).
.IP fl_navbutton
The button that navigates nested folders in the folder display.
The default is 2. (1=left, 2=middle, 3=right).
.IP fl_tarbutton
The button that choses the target folder for refile in the folder display.
The default is 3. (1=left, 2=middle, 3=right).

.SH MIME RESOURCES
.PP
Resources are used to define the set of understood MIME types
and to define the font families used to display messages
in various character sets.
.IP mimeTypes
This lists the Content-Types for which handler procedures are defined.
For each of these there is a corresponding mime_\fIcontent/type\fP
resource that specifies the Tcl command used to display a part of
that type.  \fBmimeUTypes\fP is the parallel resource so that users can
add content types of their own.
For example:
.DS
text/plain text/richtext text/enriched multipart/mixed \\
	multipart/digest multipart/parallel multipart/alternative \\
	application/octet-stream message/external-body message/rfc822 \\
	image/gif
*mimeUTypes:

*mime_text/plain:		Mime_ShowText
*mime_text/richtext:		Mime_ShowRichText
*mime_text/enriched:		Mime_ShowRichText
*mime_multipart/mixed:		Mime_ShowMultipart
*mime_multipart/digest:		Mime_ShowMultipartDigest
*mime_multipart/parallel:	Mime_ShowMultipartParallel
*mime_multipart/alternative:	Mime_ShowMultipartAlternative
*mime_application/octet-stream:	Mime_ShowApplicationOctet
*mime_message/external-body:	Mime_ShowMessageExternal
*mime_message/rfc822:		Mime_ShowRfc822
*mime_image/gif:		Mime_ShowImage
.DE
.IP mimeCharsets
A font has a character set, which is an encoding for the characters.
This is an optional parameter to the text Content-type.
us-ascii is the basic ASCII charset, which lacks special characters
used in many European languages.
The iso-8859-1 character set has 8-bit characters and is used to
encode accented and other special characters used
in latin-based languages.
The iso-8859-8 is used for hebrew.
The iso-2022-jp is used for kanji.
You need a specialized version of the Tk toolkit to display kanji.
.DS
*mimeCharsets: us-ascii iso-8859-1 iso-8859-8 iso-2022-jp
*mimeUCharsets:
*mime_us-ascii_registry:	iso8859
*mime_us-ascii_encoding:	*
*mime_iso-8859-1_registry:	iso8859
*mime_iso-8859-1_encoding:	1
*mime_iso-8859-8_registry:	iso8859
*mime_iso-8859-8_encoding:	8
*mime_iso-2022-jp_registry:   jisx0208.1983
*mime_iso-2022-jp_encoding:   *
.DE
.IP mime_\fIcharset\fP_plain_families
A font has a family that determines the basic look for the font,
like times or helvetica.  Each of the family resources specifies
a list of families that are tried, in order, to find a font
that is supported by the X server.
The plain family resource lists a set
of font families that are used for ordinary text/plain messages.
.DS
*mime_us-ascii_plain_families:	fixed clean lucidatypewriter courier terminal
*mime_iso-8859-1_plain_families:	lucidatypewriter fixed courier terminal
*mime_iso-8859-8_plain_families:	fixed
*mime_iso-2022-jp_plain_families:     fixed
.DE
.IP mime_\fIcharset\fP_fixed_families
The fixed families are used to display fixed-width fonts.
Fixed is one of the types allowd by text/enriched fonts.
.DS
*mime_us-ascii_fixed_families:	lucidatypewriter fixed clean courier terminal
*mime_iso-8859-1_fixed_families:	lucidatypewriter fixed courier terminal
*mime_iso-8859-8_fixed_families:	fixed
*mime_iso-2022-jp_fixed_families:     fixed
.DE
.IP mime_\fIcharset\fP_proportional_families
The proportional families are used for text/enriched.
.DS
*mime_us-ascii_proportional_families:	times "new century schoolbook" \\
					lucidabright charter lucida helvetica
*mime_iso-8859-1_proportional_families:	times "new century schoolbook" \\
					lucidabright charter lucida helvetica
*mime_iso-8859-8_proportional_families:	
*mime_iso-2022-jp_proportional_families:
.DE
.IP mime_\fIcharset\fP_title_families
The title families are used for menu and section titles.
.DS
*mime_us-ascii_title_families:	times "new century schoolbook" \\
				lucidabright charter lucida helvetica
*mime_iso-8859-1_title_families:	times "new century schoolbook" \\
				lucidabright charter lucida helvetica
*mime_iso-8859-8_title_families:	
*mime_iso-2022-jp_title_families:
.DE
.IP mimeExtMethods
This resource lists thte set of external access methods
for use with message/external-body MIME parts.
For each of these methods there is a corresponding
resource that lists the Tcl command that handles the
access method.
.DS
*mimeExtMethods:	local-file anon-ftp
*mimeUExtMethods:
*mime_local-file:	MimeLocalFileTransfer
*mime_anon-ftp:		MimeFTPTransfer
.DE
.SH "SEDIT BINDING RESOURCES"
.PP
.IP sedit_typeKillsSel
Set to 1 or 0 to enable or disable type-in-kills selection:
if there is selected text, typing new characters delete the selection.
.IP sedit_scrollButton
Set to Middle (the default), Shift-Middle, or Right,
to control which button is used for drag-scrolling text widgets.
.IP sedit_editprocs
This is a list of built-in editing functions.  It doesn't really
make sense to change this unless you edit seditBind.tcl to provide
an implementation for the edit function.
.IP sedit_key_\fIfunction\fP
There is a corresponding resource for each function listed in sedit_editprocs.
The value of the resources is one or more event sequences that trigger
the function.

.SH "MISCELLANEOUS RESOURCES"
.PP
.IP helpInOneWindow
Set this to 1 to get a new preferences interface.
.SH "PROGRAMMING EXMH"
.PP
\fIExmh\fP is implemented as a Tcl/Tk script that uses the MH
programs to manage your mail.  The script is interpreted at runtime
and is therefore distributed in source form.  You can read the source
to figure out what really going on.  Furthermore, you can take advantage
of the Tcl library facility in order to override parts of the implementation.
Warning:  it is easy to add new Tcl procedures, or to replace whole
modules (i.e., files) of the exmh implementation.  It is more awkward
to override the definition of a single Tcl procedure, as explained below.
.PP
By the way, if you do anything interesting to the sources, send the
results to Brent.Welch@sun.com so they can be folded back into
the master sources.  Many of the good features in \fIexmh\fP came
about this way.
.PP
Even if you do not know Tcl you can probably figure it out as
you read through the source.  There is not enough room here to
talk in any detail about Tcl programming.  There are reasonably
good on-line manual pages that come with Tcl and Tk.  There are
books about Tcl, too.  John Ousterhout, the creator of Tcl and Tk,
has written \fITcl and the Tk Toolkit\fP, by Addison-Wesley.
I have written \fIPractical Programming in Tcl and Tk\fP,
published by Prentice-Hall, ISBN 0-13-182007-9.
.PP
Your custom Tcl code is kept in your ~/.tk/exmh directory.
You maintain this as a Tcl library, which amounts to keeping a file
called \fBtclIndex\fP up-to-date.  This index file records which files
implement which Tcl procedures.  The normal Tcl shells (tclsh and wish)
provide a Tcl command \fBauto_mkindex\fP that generates this file for
you.  Within a Tcl shell you use it like this:
.DS
auto_mkindex ~/.tk/exmh  *.tcl
.DE
The first argument is a directory name, and the second argument is
a filename pattern, which is usually *.tcl.  If you add a new file or
procedure to this directory, remember to update the index by running
this command.
.PP
The Tcl library facility is used by \fIexmh\fP for the main sources,
too.  The implementation has been chopped up into over 50 files,
and these are loaded on demand as different features of \fIexmh\fP
are invoked.
The per-user library directory is searched first, which means you
can replace parts of the \fIexmh\fP implementation.  While this is
quite flexible, it is not ideal.  The natural unit of replacement is
a whole file, which might contain several Tcl procedures, even though
you only want to change one.  Also, when a new \fIexmh\fP release comes
out, there might be incompatibilities with your customized code.
.PP
NOTE: if you use TclX, then the auto_path stuff is different and the
personal library seems not to work.  If you figure out the right
thing to do in auto_path_update (in the main \fIexmh\fP script), let me know.
.PP
In most cases you will merely supply new code as opposed to replacing
(i.e., fixing) parts of the implementation.  Because you can define
buttons and menus that invoke this new code without touching the
released sources, you should be able to graft on new functionality
somewhat cleanly.
.PP
The user.tcl file contains two empty hook procedures,
User_Init and User_Layout.  User_Init is called early, before most
other modules are initialized.  User_Layout is called late, just
after the widget tree has been created and basically every module
initialized.
.PP
.I IMPORTANT
You must supply both User_Init and User_Layout because of the way
Tcl auto-loading works.  Just copy user.tcl and edit it.
.PP
You should be able to override individual Tcl procs
in User_Layout without having them trashed by the auto load facility.
In this case you'd just source a file that has the new definitions.
It turns out that Tcl procs have a global scope, so you could also
just define the procs right inside User_Layout.  Say, for example, you
hate the constrained text scrolling.  Try this:
.DS
proc User_Layout {} {
    # This procedure is called late, after WidgetTextYview has
    # been defined by the Exmh library. Let's override it now.
    proc WidgetTextYview { t args } {
	eval {$t yview} $args
    }
}
.DE
.PP
There are some optional hook procedures with names beginning with
Hook_.  These procedures are called if they exist, so you do not
need stub versions if you don't use them.  
Because of the way the library facility works, though, you
have to include your Hook procedures in your copy of user.tcl
or source the file that contains them from inside your User_Init
procedure.
.PP
In addition, you can have
several hook procedures called from the same point by appending
things to the standard hook names.  That is, the implementation will
call all procedures that match the pattern Hook_Foo*, so you could
define Hook_FooBrent and Hook_FooWelch and both would be called at
the Foo hook point.  The hook points are:
.IP "Hook_FolderChange $folder"
Called after you changed into the named folder.
.IP folderHook(enter,$folder)
.IP folderHook(leave,$folder)
The \fBfolderHook\fP array can be used to define enter and leave hooks
for a folder.  Just set these array elements to be the Tcl commands you
want invoked before and after the folder change.
.IP "Hook_CheckPoint"
Called at Quit time.
.IP "Hook_SeditSend $draft"
Called as a message is being sent.  The argument is the
pathname of the draft message.  You could frob the message
here before it is delivered.  If this raises an error,
the message is not sent.
.IP "Hook_MsgShow $pathname mimeHdr"
Called before a message is displayed.  The first argument is the
pathname of the message file.  The second is the name of an array
that contains the header information plucked out of the message.
Because of multipart messages, the elements of the array look like:
.IP mimeHdr(0=1,hdrs)
Lists all the headers defined in the message.  The header keys
are downcased (from, to, subject, etc.).
.IP mimeHdr(0=1,hdr,\fIkey\fP)
Contains the header line with the given \fIkey\fP, e.g. \fBfrom\fP or \fBsubject\fP.
.PP
Note that the MsgParseFrom procedure, which is
defined in msgShow.tcl, will extract the address part
of a header line, so use it as a starting point for your code.
.SH CODE ORGANIZATION
.PP
I've tried to split up \fIexmh\fP into meaningful modules,
separating out display modules (e.g., fdisp) from those that
maintain display-independent data structures (e.g., flist).
Things like the Find and Pick dialogs are in their own file,
so you can easily replace those.
I have not documented the interfaces between modules at all,
so you'll have to read some code.
Note that the .tcl file names
reflect the names of the procedures defined in them so you
can locate definitions.  In addition, many modules use a single
global array to hold their state variables, and this array variable
has the same name as the module.
.PP
If you are really interested in the internals of exmh
(i.e., something about it really bugs you!) you can look into
the implementation in order to see what is wrong and how you
might do things better.  The following is a list of the files
in the implementation along with a short explaination of what
the Tcl procedures in it are for.
.IP exmh.MASTER
This is the main script.  It gets patched with site-dependent information
and the results are written to \fIexmh\fP, which gets installed.
It doesn't define much because it loads just about everything from
the script library.
.IP exmh-bg.MASTER
This is the main script for the background process.  It redefines a few
procedures, and loads in the rest of its implementation from the library.
The initial rendezvous between the background process is implemented
in this script and in some supporting routines in background.tcl
.IP install.tcl
These are supporting routines for the installation process.  This should
be generic enough for use with your own Tcl application.  Feel free
to borrow it.
.IP exmh.install
This is the installation script for exmh.
.IP exmh-async
This is the wrapper for external editors.
.PP
The remainder of the files are kept in the script library.
.PP
.IP aliases.tcl
A browser for the MH aliases file.
.IP audit.tcl
Maintain an audit log of mail handling operations.
.IP background.tcl
The background processing module.  This can run in a separate process
or as part of the main process.  The BgRPC routine is used to
invoke a background operation, and it works in either case.
.IP bindings.tcl
This has the default bindings and the implementation of the
binding user interface.
.IP busy.tcl
Three different ways to indicate that exmh is busy doing something.
.IP buttons.tcl
The resource-based button and menu implementation.
.IP cutbuffer.tcl
A stub for the C cutbuffer extension.
.IP dragNdrop.tcl
Drag-and-drop for exmh.
.IP editor.tcl
The interface to editors for message composition.
.IP env.tcl
Environment variable initialization.
.IP error.tcl
The error handler.
.IP extrasInit.tcl
This has Init routines for optional modules.  The idea is to
avoid loading their complete implementation until they
are actually used.
.IP exwin.tcl
The main window display is set up here.
The code that remembers where toplevel windows go is here.
.IP faces.tcl
The interface to the faces database.
.IP fcache.tcl
The folder cache display.
.IP fdisp.tcl
The main folder display.
.IP fdispColor.tcl
The color definitions for the folder display.
.IP fdispPopup.tcl
The nested folder popup implementation.
.IP fileselect.tcl
The file selection dialog.
.IP find.tcl
The find dialog.
.IP flag.tcl
The appearance of the icon is managed here.
.IP flist.tcl
The set of unseen folders is managed by this module.
.IP folder.tcl
Folder operations like Folder_Change and Folder_Commit.
.IP folderNew.tcl
The folder create and delete dialogs.
.IP ftoc.tcl
The scan listing (folder table-of-contents).
.IP ftocColor.tcl
Color definitions for the highlights in the scan listing.
.IP ftocFind.tcl
The search routines over the scan listing.
.IP help.tcl
Some very simple help text and a color key.
.IP import.tcl
Routines to import folders from UCB mail.
.IP inc.tcl
Several ways to incorporate mail.
.IP labels.tcl
There are three labels in the display - can you see them?
.IP mailcap.tcl
Routines to parse the mailcap files.
.IP main.tcl
The main Exmh procedure, plus Exmh_Status and Exmh_Debug.
.IP mh.tcl
A basic layer on top of the MH commands.
.IP mime.tcl
The mime display code.
.IP mosaic.tcl
Code to request Mosaic to display an HTML page.
.IP msg.tcl
Message operations - although these tend to be distributed
partly among ftoc.tcl and mh.tcl as well.
.IP msgShow.tcl
This used to be the main message display code,
but it has become dwarfed by the mime display.
.IP partial.tcl
Code to concatenate the parts of a message/partial MIME message.
.IP pgp.tcl
An interface to the Pretty Good Privacy system.
.IP pgpEWN.tcl
This implements the PGP function in the external editor What Now dialog.
.IP pgpExec.tcl
This executes the PGP program to get things done.
.IP pgpMatch.tcl
This looks for keys in your keyring.
.IP pgpMisc.tcl
This has the main post-processing hook for messages.
.IP pick.tcl
An interface to the MH \fIpick\fP program.
.IP preferences.tcl
The preferences user interface.
.IP print.tcl
Routines to print messages.
.IP ps.tcl
Code to rummage through the process table.  OS-specific.
.IP report.tcl
This implements the Bug Report and Register New User forms.
.IP rich2tk.tcl
This parses text/enriched MIME content-types.
.IP scan.tcl
This manages the scan caches.
.IP sedit.tcl
The main routines for the built-in editor.
.IP seditBind.tcl
The keybindings for the built-in editor.
.IP seditCompose.tcl
The mapping for the Compose key sequences that allow
input of 8-bit characters.
.IP seditEnriched.tcl
The composition of text/enriched is implemented here.
.IP seditExtras.tcl
More editor stuff, like Whom, Spell, Sign, Find, and
the dialogs associated with Insert Part.
.IP seditMime.tcl
The MIME multipart structuring is implemented here.
.IP seditQP.tcl
This is code to handle 8-bit characters via the MIME
quoted-printable encoding.
.IP select.tcl
The keyboard selection of folders and messages is implemented here.
.IP sound.tcl
Sound effects.
.IP text.tcl
Some text tagging routines.
.IP textButton.tcl
An implementation of a pseudo-button in a text widget.
.IP textSelect.tcl
The main guts of text bindings.
.IP thread.tcl
This displays the messages related to the current subject.
.IP uri.tcl
The code that scans messages for URLs.
.IP user.tcl
Stubs for User_Init and User_Layout.
.IP widgetMenu.tcl
Support for the popup menus used in MIME messages.
.IP widgetText.tcl
Constrained text scrolling and dragging a selection off the
window is handled by the routines here.
.IP widgets.tcl
A basic layer on top of the Tk widgets.  These routines integrate
the pack geometry manager.  Even more important, they guard
against errors that occur because of missing fonts.
You should try and use these instead of the straight
Tk widget commands.
.IP xns.tcl
An interface to xnsgetmail for those folks with mail on an XNS mail server.
.SH SEE ALSO
exmh-use, exmh-ref, exmh, mh
.SH AUTHOR
Brent.Welch@Sun.COM
.SH THANKS
To Xerox PARC/CSL, for supporting this work initially,
to Sun Microsystems Laboratories for continuing the support,
and to all the exmh users who contributed ideas and code.

