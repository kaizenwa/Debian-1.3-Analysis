#!/usr/local/bin/tclsh

#set groups {legal general port tix36to40 trivia}
set groups {legal general port tix36to40}

set group_names(general) {
    General Questions About Using The Tix Library
}
set group_names(legal) {
    Legal Issues
}
set group_names(tix36to40) {
    Porting from Tix 3.6 to Tix 4.x
}
set group_names(port) {
    Question About Porting to Specific Platforms/Software
}

set group_shortnames(general)	G
set group_shortnames(legal)	L
set group_shortnames(tix36to40)	X
set group_shortnames(port)	P
set group_shortnames(trivia)	T

#----------------------------------------------------------------------
#
#----------------------------------------------------------------------
proc start_group {group} {
    global cur_group cur_index
    set cur_group $group
    set cur_index 0
}

proc q {name question} {
    global cur_group cur_index 
    upvar $cur_group g
    global $cur_group-name $cur_group-list
    incr cur_index


    set g($cur_index) $question
    set $cur_group-name($cur_index) $name
    lappend $cur_group-list $cur_index

}

proc a {answer} {
    global cur_group cur_index
    upvar $cur_group:a g

    set g($cur_index) $answer
}
#----------------------------------------------------------------------
# General questions
#----------------------------------------------------------------------
start_group legal

q 1 {
    Is Tix free software?
}


a {

    Tix version 4.0.x is released under the "Berkeley" style
    license. So it is generally consider free software. Tix 4.1 is NOT
    free software. Please read carefully the file <a
    href=../license.terms>license.terms </a> in this Release of Tix.

}

#----------------------------------------------------------------------
# General questions
#----------------------------------------------------------------------
start_group general

q 1 {
    What does the "<code>-disablecallback</code>"
    option do?
}

a {

    Many Tix widgets have both a <code>-value</code> option and a
    <code>-command</code> option. Any modification of the
    <code>-value</code> will cause the <code>-command</code> callback
    to be executed. Sometimes this is undesirable. For example,
    calling "<code>config -value</code>" inside the callback procedure
    will cause the callback to be re-entered and thus an infinite
    recursion. <p>

    The <code>-disablecallback</code> can be used to advoid this
    problem. When this option is set, the <code>-command</code>
    callback will not be executed even if the -value of a widget is
    changed. Therefore, if you need to modify the -value of a widget
    inside its callback, do this:

    <blockquote><pre>
    proc my_callback {w} {
	$w config -disablecallback true
	$w config value blah
	$w config -disablecallback false
    }
    </pre></blockquote>

    If you find this too troublesome, you can call the command tixSetSilent:

    <blockquote><pre>
    proc my_callback {w} {
	tixSetSilent $w blah
    }
    </pre></blockquote>
}

q 2 {
    How do I set the width of the entry subwidget inside the tixControl widget?
}


a {

    You can use the option database or the -options flag to set the
    configuration options of the subwidgets. E.g: <pre>

option add *TixControl*entry.width 10
</pre>

  OR

<pre>
tixControl .c -options {
   entry.width  10
}
</pre>
}

q 3 {
    What is the "<code>setslient</code>" method?
}

a {
     This is an obsolete method. You could use it to achieve the same
     effect as the <code>-disablecallback</code> option.
     <code>selsilent</code> used to be a widget command for the
     ComboBox, Control, etc. It has been removed since Tix 4.0a4 and
     replaced by the <code>tixSetSilent</code> command. Please note
     that <code>tixSetSilent</code> is not a widget command but an
     external procedure.
}

q 4 {
    Is there a Tix interface builder in the works?
}

a {
    Yes. But I don't know when it will be finished. (probably in 96).
}


q 5 {
    Can you tell me about the syntax of tixForm
}

a {
    Please see the file <a href="../man/Form.html">man/Form.html</a>
    or <a href="../man/Form.n">man/Form.n</a>.
}

q 6 {

    I am not using the tixForm geometry manager, but it is giving me
errors about TixForm. What happened?
}

a {

    When you get error messages like this:

<pre> (TixForm) Error:Trying to use more than one geometry
           manager for the same master window.
           Giving up after 50 iterations.</pre>

    Most likely, the problem is when using tixLabelFrame widgets, you
    packed to the wrong frame: <p>

    This is WRONG:

<pre>   tixLabelFrame .d
        button .d.b
        pack .d.b </pre>

This is the correct way:

<pre>   tixLabelFrame .d
        set f [.d subwidget frame]
        button $f.b
        pack $f.b
        pack .d </pre>

    Remember you don't pack directly into a TixLabelFrame
    widget. Instead, you should pack into its <code>frame</code>
    subwidget.
}

q 7 {
    How do I generate the <code>tclIndex</code> file for Tix?
}

a {

    Tix <code>tclIndex</code> files cannot be generated using the
    standard auto_mkindex procedure. You must use the tixindex program
    in the <code>tools/</code> subdirectory in the Tix
    distribution. The syntax is 
    <pre> tixindex *.tcl
    </pre>
}

q 8 {
    Can I ignore the default arguments passed by the various
    <code>-command</code> and <code>-broeswcmd</code> options?
}


a {
    You can use the <code>tixBreak</code> command. For example:
<pre> tixFileSelectDialog .c -command "puts foo; tixBreak" </pre>
}

q 9 {
    What does <code>tixWidgetDoWhenIdle</code> do?
}

a {

    It does the same thing as tixDoWhileIdle (and "after -idle"). The
    difference is it takes its second argument as the name of a widget
    and executes this command only if the widget exists: i.e.: 


<pre>  tixWidgetDoWhenIdle tixComboBox::Update $w blah blah ..</pre>

    will execute tixComboBox::Update only if $w exists. $w may be
    destroyed after tixWidgetDoWhenIdle is called but before an idle
    event happens.
}

q feature_req {
    Why isn't such a feature in Tix? Will it be implemented?
}

a {

    Generally requests for new features are welcomed. You can send
    your requests to <a href=mailto:tix@xpi.com> tix@xpi.com </a> and
    we'll be happy to hear from you. <p>

    We can't guarantee to implement the requested features
    immediately. Usually it depends on how important the features. If
    the feature is requested by more people, it will usually get done
    faster.

    However, some frequently requested features probably won't be
    imlemented. Usually these features are cosmetic changes and:

    <ul>
    <li> they do not add new capability to the widgets
    <li> they are not universally liked
    <li> they confuse the user.
    </ul>

    <p>
    Some examples are:

    <ul>

    <li> <b>Different foreground and background colors for the
    NoteBook tabs</b>: having a lot of colors may antagonize the users
    that are "color haters"; also, the different colors don't make it
    easier for the user to locate the desired tab.

    <li> <b>Ring-binder metaphore for the NoteBook widget</b>: a waste
    of screen real estate.

    <li> <b>Rows of tabs for the NoteBook widget</b>: the user may be
    confused when the rows of tabs are switched. If you need to have a
    lot of tabs for the notebook, use the ListNoteBook widget instead.

    </ul>

}

q softwares {
    Who are using Tix in their software?
}

a {

    I have compiled a list of softwares that use Tix. See <a
    href=http://www.xpi.com/tix/software.html>
    http://www.xpi.com/tix/software.html</a>. (These are only the ones
    that I have heard of, either from the authors themselves or from
    the TCL FAQ. There should be more of them).


}

#----------------------------------------------------------------------
# Platform- and software packages - specific issues
#----------------------------------------------------------------------
start_group port

q 1 {
    The configure script gave me strange errors.
}

a {
    The problem may be you have several operating systems sharing the
    same file system. Some people encounter error messages like this:

<blockquote><pre>
# ./configure --prefix=/usr/vendor/tcl
loading cache ./config.cache
checking for a BSD compatible install... /usr/bin/installbsd -c
checking for ranlib... ranlib
checking how to run the C preprocessor... cc -E
checking for unistd.h... ./configure[603]: "${ac_cv_header_$ac_safe+set}": bad
substitution
</pre></blockquote>

    The problem is at line 2, configure loaded in ./config.cache,
    which may have been created by a different operating system, with
    settings only usuable for that operating system. To get around
    this, you should type

<blockquote><pre>
make distclean
./configure
make all
</pre></blockquote>
}


q tk41 {

    Does Tix 4.1 work with <b>Tk 4.1</b>
}

a {

    Yes, just enable the "Tk 4.1 ..." option in the setup program. It
    will also compile Tix in a dynamic lobrary.

}

q itcl {
    Does Tix work with <b>Incr Tcl 2.0</b>?
}

a {

    Yes just enable the "Itcl 2.0 ..." option in the setup
    program. Make sure you have ITcl 2.0 installed. Beta versions will
    *NOT* work. Also make sure you have installed the source tree of
    ITcl 2.0 in the same directory where you install the Tix source
    tree.
}

q expect {

    How do I get Tix to work with <b>Expect</b>?
}

a {
    From Paul Schmidt (kuato@netcom.com):

    <blockquote>

    I have integrated Tcl7.4, Tk4.0, Expect-5.19 and Tix4.0 on Linux
    1.2.13 (ELF) and Solaris 2.4. It isn't too hard. For an
    expectk+Tix binary you need to add a call to Tix_Init in
    exp_main_tk.c. If you can find the call to Tk_Init then just
    cut&paste and replace it with Tix_Init. Do the same if you want a
    Tk+Tix window shell in TkAppInit.c. Worked like a charm. If you
    have any problems just holler.

    </blockquote>

}

q 6 {
    <b>Solaris 2.4:</b>
    Filenames in FileSelectBox are chopped off.
}

a {
    <b>Problem:</b>
    <blockquote>

    With Tix4.0a7 (and also with Tix4.0a6) on Solaris 2.4, when
    running the widget demo, in tixFileSelectBox, in the two scolling
    lists (for Files an Directories), some of the file and directory
    names have their first 2 letters chopped off. And some files are
    repeated.

    </blockquote>
     
    <b>Solution:</b> tixwish has some conflicts with /usr/ucblib/libucb.so.1
    and you should not linke it tixwish (you don't need it). Here is
    a solution provided by Charles L Ditzel
    (<i>charles@hanami.cyberspace.com</i>):

    <blockquote>
    To fix the problem I was having, all I did was:

    <pre>
   unsetenv LD_LIBRARY_PATH
   set my PATH to something basic like:
     /usr/bin:/usr/ccs/bin:/bin:/usr/openwin/bin:/opt/SUNWspro/bin
   removed config.cache
   ./configure
   make clean
   make
    </pre>
 
    and now it works!! Must have been something in my old
    <code>PATH</code> or <code>LD_LIBRARY_PATH</code> that was
    causing it to pick up <code>/usr/ucblib/libucb.so</code>.

    </blockquote>
}

q 7 {
    Do I still need libXpm?
}

a {

    No, now Tix has its own XPM file reader. You no longer need libXpm.

}

q 8 {
    <a name=coredump1>I get a coredump as soon as tixwish starts up.
}

a {


     Try to get a backtrace of the stack when the core dump happens
     (with a debugger, for example). If the core dump happens right
     inside the call to <code>Tk_ConfigureWidget()</code> inside the
     file <code>tixInit.c</code>, then the problem is because you
     compiled <code>libtk.a</code> and <code>libtix.a</code> with
     different versions of the Th header file
     <code>tk.h</code>. Delete all the <code>.o</code> files from the
     src directory of Tix, fix the Makefile so that now you can
     compile <code>libtix.a</code> with the same tk.h that you used to
     compile <code>libtk.a</code>.

}

#----------------------------------------------------------------------
# Porting from Tix 3.6 to tix 4.x
#----------------------------------------------------------------------
start_group tix36to40

q 1 {
    What happened to the <code>tixInit</code> command?
}

a {
    You don't need to use it anymore. It is provided in Tix 4.x only for
    backward compatibility.
}

q 2 {
    How do I set the schemes and fontsets in Tix 4.x?
}

a {
    You can set the color schemes and fontsets using the standard X
    resource database (.Xdefaults file). You can add these two lines
    in the user's .Xdefaults file: <pre>

	*TixScheme:   Gray
	*TixFontSet:  14Point </pre>

    If you want to switch color schemes and fontsets during run time,
    you can issue the following commands: <pre>
	tix config -scheme Gray -fontset 14Point
    </pre>

    Please read the <a href=../man/tix.html>tix</a> manual page for
    more details
}

q 3 {
    How do I choose the default TK color schemes and fontsets? Tix is
    messing up the colors of my existing programs.
}

a {
   Add these two lines in your .Xdefaults:<pre>

	*TixScheme:   TK
	*TixFontSet:  TK </pre>
}

q 4 {
    I want the old bisque look of Tk 3.6. tk_bisque doesn't work.
}

a {

   The Tix widgets are not compatible with tk_bisque. If you want a
   bisque-ish look you can add to your .Xdefaults file the following
   line:<pre>
	*TixScheme:   Bisque</pre>
}

#----------------------------------------------------------------------
# Header, etc of this document
#----------------------------------------------------------------------

set header {
    <center><h1>Tix Frequently Asked Questions</h1></center>
}

set toc {
    <h3>Table of Contents</h3>
}
#----------------------------------------------------------------------
# Generate the FAQ.html file
#----------------------------------------------------------------------

puts $header
puts $toc
puts <DL>

# Generate the list of questions
#
#
foreach g $groups {
    upvar #0 $g group

    puts <DT><i><b>$group_names($g)</b></i><DD><ul>

    set i 1
    set names [lsort [array names group]]
    foreach name [set $g-list] {
	set anchor_name [set $g-name($name)]
	puts -nonewline "<li> <a href=#$g.$anchor_name> "
	puts -nonewline "\[$group_shortnames($g).$i\] </a> "
	puts -nonewline "$group($name)"

	incr i
    }
    puts </ul><p>
}

puts </DL>

# Generate the list of questions and answers
#
#

foreach g $groups {
    upvar #0 $g group
    upvar #0 $g:a answer

    puts <hr>
    puts <h3>$group_names($g)</h3>
    puts <DL>

    set i 1
    set names [lsort [array names group]]
    foreach name [set $g-list] {
	set anchor_name [set $g-name($name)]
	puts -nonewline "<DT> <b><a name=$g.$anchor_name> "
	puts -nonewline "\[$group_shortnames($g).$i\] </a> "
	puts -nonewline "$group($name)</b>"

	puts "<p>"

	puts <DD>
	puts "<b> ANSWER: </b> $answer($name)"
	puts <p>


	incr i
    }

    puts </DL>
}
