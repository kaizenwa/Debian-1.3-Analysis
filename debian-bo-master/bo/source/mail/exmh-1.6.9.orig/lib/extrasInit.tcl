# 
# extrasInit.tcl
#
# This has initialization code for some extra packages.
# The idea is to avoid auto_loading the whole package,
# while still allowing the package to manifest itself
# in the preferences dialog (for example).
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Faces_Init {} {
    global faces tk_version
    if {$faces(dir) == {}} {
	set faces(enabled) 0
    }
    # faces(suffix) starts out (by default) with {xpm gif xbm}
    if {![info exists faces(suffix)] || ([llength $faces(suffix)] == 0) ||
	($tk_version < 4.0)} {
	set faces(suffix) xbm
    }
    if {$tk_version >= 4.0} {
	# Double check the non-standard pixmap image type to avoid file stat
	if {[lsearch [image types] pixmap] < 0} {
	    set ix [lsearch $faces(suffix) xpm]
	    if {$ix >= 0} {
		set faces(suffix) [lreplace $faces(suffix) $ix $ix]
	    }
	}
    }

    Preferences_Add "Faces" \
"Exmh will display a bitmap or image of the person that sent the current message (or their organization).  This relies on the faces/picon database, or the presence of an X-Face: mail header.

Any given mail address can match a range of face images, from most specific (such as an X-Face) or most general (such as an a default image for a toplevel domain)." {

	{faces(xfaceProg) xfaceProg {} {X-Face command}
"A command to convert an X-Face: header line into a displayable X11 bitmap.

If you've patch uncompface, this can be:	uncompface -X
otherwise you should use this:			uncompface | ikon2xbm

Defining this enables the display of X-Face images.  This is independent of the facesaver database."}

	{faces(enabled) facesEnabled ON {Use faces database}
"Search for and display images from the facesaver database."}

	{faces(rowEnabled) facesRow OFF {Use faces row}
"Use a horizontal row to display all the matching images of a face, rather than the most specific one."}

	{faces(defer) facesDefer ON {Background processing} 
"When on, this causes faces display to be handled as a background task.  This allows the display of a faces row to be aborted when another message is selected.  If your machine can display faces quickly enough, you should turn this off."}

	{faces(palette) facesPalette {} {Size of color palette}
"This allows you to force exmh to render face images in a less color-consuming
manner.  Valid settings are as for the -palatte option of the Image Photo widget.

With the default setting, an empty string, images are rendered in as large a color cube as the photo widget can allocate.

When set to a single decimal number, this specifies the number of shades of gray to use.  A value of 2 results in a dithered monochrome; 32 results in a pleasing greyscale.

When set to three decimal numbers separated by slashes (/), specifying the number of shades of red, green and blue to use, respectively.  5/5/4 is a useful setting for an 8-bit Pseudocolor display"}

     }
}

proc Faces_Create { parent } {
    global faces

    # Remember these
    set faces(parent) $parent
    set faces(rowparent) [winfo parent $parent]

    trace variable faces(rowEnabled) w Faces_Setup
    Faces_Setup
}

proc Faces_Setup args {
    global faces faceCache

    # should call this when one of these changes:
    #	faces(path), faces(dir), faces(sets), env(FACEPATH)
    Face_SetPath

    catch {unset faceCache}

    # Clear any faces (and delete images)
    if [info exists faces(alloc)] {
	Face_Delete
    }

    set faces(alloc) 0		;# last label allocated
    set faces(avail) 0		;# next label available for use

    catch {destroy $faces(rowparent).faceRow}
    if [winfo exists $faces(parent).default] {
    	catch [concat destroy [winfo children $faces(parent)]]

	# shrink the hole (why do I have to do this?)
	$faces(parent) config -width 1
	update idletasks
	$faces(parent) config -width 0
    }

    if $faces(rowEnabled) {
	set row [Widget_Frame $faces(rowparent) faceRow Face {top fill}]
	pack $row -before $faces(parent) -side bottom

	set faces(frame) $row
	set faces(rowbg) [lindex [$row config -bg] 4]
    } else {
    	set faces(frame) $faces(parent)
    }

    global exmh
    set faces(default) [Widget_Label $faces(frame) default {left fill} \
			-bitmap @$exmh(library)/exmh.bitmap]

    # kludge to get default background of the labels
    if $faces(rowEnabled) {
	set f [FaceAlloc]
	set faces(facebg) [lindex [$f config -bg] 4]
	Face_Delete
    }
}


proc Sound_Init {} {
    global sound
    if {$sound(cmd) == {}} {
	set sound(enabled) 0
    } else {
	# Preferences_Add will set these variables to Xresource values,
	# but only if the variables are not already defined.
	# These sound variables are defined at install time,
	# so we need to unset them in order honor any per-user defaults.
	set cmd $sound(cmd) ; unset sound(cmd)
	Preferences_Add "Sound" \
"Exmh can provide audio feedback.  It can ring your terminal bell, or play audio files." \
	[list \
	    { sound(enabled) soundEnabled ON {Sound feedback}
"Enable audio feedback.  Exmh will make a sound when
new messages are incorporated into your folders
(except during startup) and when you try to change
folders without committing moves and delete operations."} \
	    { sound(multifile) soundMultiFile OFF {Play Multiple}
"If your play command can handle multiple audio files,
then set this option.  In this case Exmh can run the
audio program in the background when it needs to
play multiple sounds."} \
	    [list sound(bell) soundBell OFF {Use terminal bell} \
"Ring the terminal bell instead of playing an audio file."] \
	    [list sound(cmd) soundCmd $cmd {Play command} \
"The command line used to play audio files.  You may want
to add flags to control the volume, for example.  The
name of the audio file is appended to this command line."] \
	    [list sound(newMsg) soundNewMsg drip.au {Sound for a new message} \
"The name of an audio file to play when
new messages have arrived.  Relative pathnames are
searched for in the exmh script library directory."] \
	    [list sound(error) soundError clink.au {Sound for an error} \
"The name of an audio file to play when
you forget to commit pending operations.  Relative pathnames are
searched for in the exmh script library directory."] \
]
    }
}

proc Sedit_Init {} {
    global sedit

    set sedit(init) 1
    set sedit(height) 20
    set sedit(allids) {}

    if ![info exists sedit(key,sendMsg)] {
	set sedit(key,sendMsg) <Control-c><Control-c>
    }
    Preferences_Add "Simple Editor" \
"Exmh comes with a simple built in editor called \"sedit\".
It has about 20 keybindings for basic editing operations.
You can tune these bindings with the Bind dialog that defines
bindings for the Text and Entry widget classes."  {
    {sedit(pref,replPrefix) replPrefix "> " {Reply insert prefix}
"This string is prepended to lines included from the reply message
when you use the Insert @ command."}
    {sedit(formatDefault) seditFormatMail ON {Format Mail default}
"Sedit will format mail just before it gets sent out.  This includes
chopping long lines and expanding text/enriched directives.  You
can control whethor or not this happens with the Format Mail menu
item.  This Preference setting chooses the default for that item."}
    {sedit(mhnDefault) seditAttemptMhn OFF {Attempt mhn default}
"Sedit can send your message thru mhn in order to expand its #
MIME formating directives (see the man page about mhn for details).
You can control whethor or not this happens with the Attempt mhn menu
item.  This Preference setting chooses the default for that item."}
    {sedit(keepDefault) seditKeepOnSend OFF {Keep on send default}
"Sedit can save its window after you send a message.  This is useful
if you want to send variations on the same message to different addresses.
This Preference setting chooses the default for this option."}
    {sedit(quoteDefault) seditQuotedPrintable {CHOICE automatic always never} {Quoted-printable default}
"Sedit can encode text as quoted-printable to protect 8-bit characters.
Automatic means it will do this when you use the Compose key to
insert an 8-bit character.  Always means it always does it.
Never means it never does it.  You can also override this on
a per-message basis with the Quoted-Printable menu entry."}
    {sedit(lineLength)   seditLineLength 79 {Max Line Length}
"This is the length at which Format Mail chops lines.
It looks around for a word break when chopping."}
    {sedit(autoSign) seditAutoSign OFF {Automatically sign messages.}
"This will cause your .signature (or selected .signature* file) to
be automatically appended to your message when you Send it."}
    {sedit(sigDashes) seditSigDashes ON {Put a '-- ' before signature.}
"This puts a '-- ' on a line between your mail and signature,
as per ELM and also Usenet news.  This is only done for single-part mail."}
    {sedit(sigfileDefault) seditSignatureFile "" {Default signature file}
"This is the name of the default signature file.  If set to something,
then this will be used as the default signature file in the Sign menu
for the built-in editor.  It is assumed to match the ~/.signature* pattern."}
    {sedit(colorize) seditColorize OFF {Colorize multiparts}
"For debugging, the multipart structure of a message can be highlighted
by coloring different type parts with different background colors."}
    {sedit(iso) seditISO ON {Specify Charset for Text}
"If enabled, this option adds character set information to
text content types, and promotes all messages to at least
MIME content-type text/plain."}
    {sedit(charset) seditCharset {CHOICE iso-8859-1 iso-8859-2 iso-8859-8} {8-bit character set}
"If you have enabled support for ISO character sets and enter
text that has the 8-th bit set, then
this is the character set used for text content types"}
    {sedit(spell) seditSpell {CHOICE spell ispell custom} {spell program}
"This chooses the spell program used by the built-in editor.
Use custom if you want to define the program explicitly."}
    {sedit(spellCustom) seditSpellCustom {exmh-async xterm -e ispell} {custom spell command}
"There are two flavors of spell programs.  If your spell program just
prints out the misspelled words, then just specify it directly.
Examples include the standard \"spell\" program.  If you spell program
is interactive, then run it from exmh-async:
exmh-async xterm -e ispell
(This is faked - exmh-async isn't really used.  Instead a temporary
wish script is used.)"}
    }
    global sedit
    # Fixup code for 1.5minus left-overs
    if {[string compare $sedit(charset) us-ascii] == 0} {
	set sedit(charset) iso-8859-1
    }
}
if {[info command Sedit_CheckPoint] == ""} {
proc Sedit_CheckPoint {} {
    # Dummy routine overridden when/if sedit.tcl is auto-loaded
}
}

proc Pgp_Init {} {
    global pgp env miscRE

    set miscRE(headerend) {^(--+.*--+)?$}
    set miscRE(mimeheaders) {^content-[-a-z]+:}
    set miscRE(true) {^(on|y(es)?|t(rue)?)$}
    set miscRE(beginpgp) {^-+BEGIN PGP}
    set miscRE(beginpgpkeys) {^-+BEGIN PGP PUBLIC KEY BLOCK-+$}
    set miscRE(beginpgpclear) {^-+BEGIN PGP SIGNED MESSAGE-+$}

    # figure out the path for pgp files
    if [info exists env(PGPPATH)] {
	set pgp(pgppath) $env(PGPPATH)
    } elseif [file isdirectory $env(HOME)/.pgp] {
	set pgp(pgppath) $env(HOME)/.pgp
    } else {
	set pgp(pgppath) [pwd]
    }

    # if the user doesn't have public key ring, pgp isn't setup: give up !
    set pgp(pubring) $pgp(pgppath)/pubring.pgp
    if [file exists $pgp(pubring)] {
	set pgp(enabled) 1
    } else {
	set pgp(pubring) {}
	set pgp(enabled) 0
	return
    }

    set pgp(menutext,signclear) "Check the signature"
    set pgp(menutext,signbinary) "Show content"
    set pgp(menutext,encrypt) "Show content"
    set pgp(menutext,encryptsign) "Show content"
    set pgp(menutext,keys-only) "Show content"
    
    set pgp(decode,none) 0
    set pgp(decode,all) 1
    set pgp(decode,keys) {$action == "keys-only"}
    set pgp(decode,signed) {![regexp {encrypt} $action]}

#    {pgp(fullkeyring) pgpFullKeyRing OFF {Potentially search the full keyring}
#"When matching, exmh does a first strict selection based on the hostname 
#and then looks for the best match in this selection. If the first selection
#doesn't select anything, this option allows exmh to search the key in
#the whole keyring instead of giving up and query the user." }

    Preferences_Add "PGP interface" \
"PGP is the Pretty Good Privacy package from Zimmerman.
PGP lets you sign and encrypt messages using public keys.
There is considerable documentation that comes with PGP itself." {
    {pgp(keeppass)  pgpKeepPass  ON {Keep PGP password}
"Exmh tries to remember your PGP password between pgp
invocations. But the password is then kept in a global
variable, which is not safe, because of \"send\"'s power.
If you turn this feature off, exmh will use xterm to run
pgp so that it doesn't have to deal with the password at all." }
    {pgp(rfc822) pgpRfc822 OFF {Encrypt headers}
"Used to encrypt the whole message, instead of only encrypting
the body, so that the subject line (for instance) is also
safely transmitted." }
    {pgp(choosekey) pgpChooseKey ON {Always choose the sign-key}
"When signing a message, sedit can either use the default key or ask
the user to choose which key he wants to use. Of course, if you only
have 1 private key this setting doesn't interest you much." }
    {pgp(cacheids) pgpCacheIds {CHOICE persistent temporary none} {Cache map from email to public-key}
"The way exmh figures out the public-key to use for an email address is
often slow. This option allows you to cache the result of the matching
so that it doesn't have to be done over and over. Furthermore the cache
can be saved in a file .matchcache in your pgp directory so as to make
it persistent accross exmh sessions." }
    {pgp(minmatch) pgpMinMatch 75 {Minimum match correlation (in percents)}
"When trying to find the key corresponding to an email address,
exmh tries to be 'smart' and does an approximate matching. If the 
match's quality is better than the specified percentage, exmh will
assume it's the right key. Else it will query the user. Hence, a
value greater than 100 will make exmh always query the user." }
    {pgp(showinline) pgpShowInline {CHOICE none keys signed all} {Show pgp messages inline}
"controls which pgp parts get automatically decoded with pgp. Since
decoding generally takes time, and since clear signed messages can
be viewed without pgp, it makes sense to limit the decoding to rare
cases like key parts:
 - keys: only auto-decode key parts
 - signed: auto-decode key and signed parts" }
    {pgp(autoextract) pgpAutoExtract ON {Extract keys automatically}
"When you receive a keys-only part, you can have its content
displayed and you can extract its content into your public
key ring. The extraction can be safely done automatically,
but you might prefer doing it manually, with a menu entry
on the keys-only part." }
    {pgp(keyserver) pgpKeyServer "pgp-public-keys@jpunix.com" {Favorite public key server}
"When a signature check fails because of a missing key,
exmh allows you to ask a key server for the key.
Please select a key server that's close to
you so as to spread the load." }
    }

    PgpExec_Init
}

proc Glimpse_Init {} {
	global glimpse

	if {[string length $glimpse(path)] == 0} {
	    global exwin
	    catch {destroy $exwin(fopButtons).glimpse}
	    catch {$exwin(fopButtons).search.m entryconfigure Glimpse* -state disabled}
	    return
	}
	if [info exists glimpse(init)] { return }
	set glimpse(init) 1

	Preferences_Add "Glimpse" \
"Glimpse (which stands for GLobal IMPlicit SEarch) is an indexing and query
system that allows you to search through all your files very quickly.  You
could set here your default values. The 'Glimpse Window' allows you to re-
define them for a search in the menu 'opts'." {
        {glimpse(caseSensitive) glimpseCaseSensitive ON {Case sensitiv search}
"Determines if the search is case sensitive or not.  This could be
changed on the fly in the 'Glimpse Window' in the menu 'opts'"}
        {glimpse(wholeWord) glimpseWholeWord ON {Match only whole words}
"If set to on your search string is assumed to be a complete word.  This
could be changed on the fly in the 'Glimpse Window' in the menu 'opts'"}
        {glimpse(searchRange) glimpseSearchRange {CHOICE all subtree current all-in-one}
 {Default search range is}
"The default search range of glimpse:
    all:     search in all your mails
    subtree: search in the current and all subfolders.
    current: search is restricted to the current folder
This can be changed on the fly in the 'Glimpse Window' in the menu 'opts'"}
        {glimpse(maxHits) glimpseMaxHits {CHOICE 50 100 200 500 1000 2000 5000}
{Maximum number of matches *per folder*}
"Outputs only the first x matching records.
This applies on a per-folder basis, not per file.
If you have a single large glimpse index, it applies
to the whole search.

The maximum # of matches can be changed on the fly in the 'Glimpse Window'
in the menu 'opts'"}
        {glimpse(maxErrors) glimpseMaxErrors {CHOICE none 1 2 3 4 5 6 7 8}
{Maximum allowed errors}
"Specifying the maximum number of errors permitted in finding the approximate
matches (the default is none).  Generally, each insertion, deletion, or
substitution counts as one error.

If not set to 'none' your search string is assumed to be a complete word.

The number of errors allow can be changed on the fly in the 'Glimpse Window'
in the menu 'opts'"}
	}
	
}


