#define HELPWIDTH 60 
#define HELPSTR "\
LIST OF COMMANDS ******************************************

<i>/<m>           select a page relative to the current page 
<g>               select a page w.r.t. TeX-counters
<u>/<n>/<h>/<j>   scroll the visible area
<f>/<c>           make scrolling finer/coarser
<z>               center visible area
<+>/<->           zoom in/out
<v>               set zoom-factor

<b>               set a bookmark
<w>               move to a bookmark
<^>               move back

<o>               toggle greyscaling
<x>               toggle statusline-information
<t>               set unit of measurement

<l>               show/hide screenmark and pagemark
<y>               set pagemark at the position of screenmark
<a>               show/hide marked rectangle
<p>               show/hide printable area
<e>               set page-offset and -size

<k>               show/hide half-hyper-tex-mark
<TAB>             move to next href
<RET>             follow current href

<s>               search for text
<r>               re-read DVIfile and re-draw screen
<d>               load/kill DVIfile
<q>               quit tmview


GENERAL USAGE *******************************************

When everything is setup right, visiting a DVIfile with
tmview just means to navigate the visible area along that
file, using the cursor-keys.  Some of the following
commands obviously require an argument, f.e. <g>.
Arguments are to be entered BEFORE executing a command.
When executed by hitting the corresponding uppercase key,
you will be asked for the argument.  While the
user-interface of tmview is meant to save keystrokes, it
is not too intuitive. You may either read the following
instructions, or just do <caps-lock>.

The cursor-keys <page-up> and <page-down> are taken as <i>
resp. <m>. The keys <pos1> and <end> select the first
resp. the last page. The cursor-keys <left> <right> <up>
and <down> do <h> <j> <u> resp. <n>. All in all this
means, that the cursor-keys do what they are meant to.

When a mouse is connected, it moves any visible mark. When
the right or left mousebutton is no good for anything
else, moving the mouse while holding that button acts on
the visible area instead. (That sounds bad, but works out
fine) When the screenmark is shown (see <l>), the left
mouse-button sets the pagemark (see <y>) and the right
button marks a rectangle (see <a>). When the
half-hyper-mark is shown, the left mouse-button follows
the current href, if any.

*********************************************************
NON-INTUITIVE USER-INTERFACE ****************************
*********************************************************

To explain the way tmview expects to receive commands, a
short nomenclature used in the sequel is given:
 
Any text within `<' and `>' represents a single keystroke,
while `(' and `)' mark the beginning resp. the end of a
string to enter.  So <h> is about to hit the key named `h'
and (25.4) could stand for <2><5> <.><4>.  The carriage-
return-key is referred by <ret>, the backspace-key or 
delete-key by <del> and the escape-key by <esc>. Almost 
whenever a string is expected by tmview, you may use <del>
to remove the last character you typed. So even
<2><5><6><del><.><4> results in (25.4). Note that a string
does not need to be terminated by <ret>.  To simplify a
reference to a string, in this text any uppercase letters
within `(' and `)' are not meant as a string, but as the
name of some string. So we may read something like
`(PP)<h> scrolls PP percent to the left' as `typing in any
number just before <h> results in scrolling left according
to that number'.


COMMANDS AND ARGUMENTS***********************************

A command is executed by typing its name, which consists
of a single character. Some of the above listed commands
take numerical arguments.  Arguments are always
optional. They may be entered before executing the
command. Multiple arguments are separated by <,> or
<;>. If no argument is passed, a default is used. If an
argument is passed, it serves as the default for following
commands. Commands doing similar things share the same
default arguments.

Example: 
Typing (10)<h> results in scrolling 10% to the left and
sets the default for any scrolling commands to 10%. Thus
typing <j> afterwards results in scrolling 10% to the
right.


MAGIC ARGUMENT*******************************************

As a special argument some commands accept the magic
argument <*>. It is used either to vary the command in
some way or to get the arguments from another place. <#>
is equivalent to <*> and saves you from holding down the
shift-key on some keyboards.

<z> for example centers the visible area. It excepts as
argument the point which will be taken as origin. Since
this will become the default for a future <z>, you may
measure out the origin only once. Even quicker it is to
position the visible area by scrolling and then to do a
<*><z>.  This results in taking the current position as
centered and so as default for a coming <z>.

As another example look at the command <m>, which moves
pages forward, according to a given argument resp. a
default. By moving on the next page there are two modi
available: 1. keep the visible area; 2. do center like
<z><z>.  <*><m> toggles between these two modi. So in this
case <*> acts as a kind of flag argument.



SELECTING THE PAGE****************************************

<i>/<m> Select a page relative to the current page. 

     <i> moves towards the beginning of the DVIfile, while
     <m> moves towards the end. A single argument (N) may
     specify the amount of movement in pages. If the
     argument is (*), the page-moving-mode is toggled. See
     above.  

     Example: 
     (1)<i> selects the previous page <m> selects the next
     page

<g> Select a page with respect to TeX counters.  

     A list of ten arguments (COUNT0; COUNT1; ... COUNT9)
     specifies the page to be selected. <*> may be used as
     wildcard. If there are more than one but less than ten
     arguments given, the others will be taken as <*>.

     Example: 
     (26)(i) selects the first page after the current page
     with a value of 26 in \\count0.



MOVING AROUND ON CURRENT PAGE ******************************

<u>/<n>/<h>/<j> Scrolling the visible area

       A single argument (PP) may be used to specify the
       amount of scrolling in percent of the
       screen-width. <u>/<n> resp. <up>/down> both accept
       the argument (*) to toggle between: 1. stay on the
       current page; 2. scroll over pages. When scrolling
       over pages, you may view the whole document while
       using only the single key <n>.

       Example:
       (20)<h> scrolls 1/5 to the left.  

<f>/<c> Make scrolling finer/coarser

       These commands change the default argument for the
       above scrolling- commands. So <f> and <c> don't move
       the visible area at all, but they change the way the
       scrolling-commands act.

<z> Center the visible area

       Without any argument <z> centers horizontally only,
       while <z><z> centers in both directions.
       When two arguments (X,Y) are given, they describe
       the point on the page, which will become the middle
       of the visible area. When the argument (*) is given,
       the current position is taken as centered.  When the
       screenmark is shown (see below <l>), and the
       argument (*) is given, the position of the
       screenmark becomes the center. This does move the
       visible area, but it does not move the screenmark.


ZOOMING ****************************************************

<v> Set the zoom-factor
 
       This command requires one argument (F) which must be
       between 0.1 and 2.  F will become the zoom-factor
       and the visible-area will be redrawn. If the
       zoom-factor is 1, the pixels found in pk-files are
       just copied one by one to the screen. Since the
       resolution of our days screens seems to be less than
       that of our days printers, and since you may still
       want to use the same pk-files for printing and
       viewing, F=1 usually results in a magnification. So
       when you're just reading some text in some DVIfile
       you will set F to something like 0.3, depending on
       the involved resolutions. When the screenmark is
       visible (see <l>) the position of the screenmark is
       taken as the origin of zooming, i.e. it is
       fixed. When the screenmark is not visible, the
       middle of the screen is fixed. The current
       zoom-factor is displayed in the optional statusline,
       see <x>.

       There are two zooming modi. The fast modus works
       only if 1/F is an integer.  So good values are
       F=0.5, 0.333, 0.25, 0.2, 0.167 and so on. When in
       the fast modus, the <+>/<-> keys described below
       step through that values. And since F is to be small
       anyway, you may never use <v> to set it explicitly.

       The good thing about the slow modus is, that it
       allows you to choose the zoom-factor arbitrary
       (between 0.1 and 2). So poor students with small
       screens might find some optimum to make the text fit
       and still be readable. The bad thing about the slow
       modus is that it is slow.  But since once zoomed
       glyphs are kept in memory, this slowness only hurts
       the first few pages after changing the
       zoom-factor. Anyway, <*><v> brings you back in the
       fast modus, choosing the nearest possible
       zoom-factor therefore.

       Sorry. The problem may be solved either by a better
       zooming algorithm or faster computers.

<+>/<-> Zoom in/out

       Increase/decrease the zoom-factor. When in the fast
       modus, step through the fast values only (see
       above). When an argument (PP) is given, it is taken
       as the amount of increasing/decreasing in percent of
       the current zoom-factor. This is likely to result in
       the slow modus.

       Example:
       (0.17)<v>   set zoomfactor to 0.17               (slow)
       (10)<+>     set zoomfactor to 1.1*0.17=0.187     (slow)
       (3)<->      set zoomfactor to 0.97*1.87=0.181    (slow)
       <->           ...          to 0.97*0.181=0.176   (slow)
       (*)<v>        ...          to 1/6=0.167          (fast)
       <+>           ...          to 1/5=0.2            (fast)
             

BOOKMARKS **************************************************

A bookmark remembers what is seen on the screen. That is
the DVIfile, the page within that file, the position of the
visable area and the zoom-factor. There are three kind of
bookmarks ...

file-bookmarks:
Each file visited has a file-bookmark, containing the above
information about what was seen on the screen when visited
the last time, plus some information on the file, that is
the paper-offset and -position, the location of the
printable-area.  file-bookmarks are generated automaticly.
This results in easy re-visiting a DVIfile: you'll find it
as left. A file-bookmark is removed by killing the DVIfile
with <d><k>, see <d> below.

back-bookmarks:
When searching a text-string, following a href or moving to
a bookmark, the position within the DVIfile might be
changed to somewhere far far away. To simplify recovering
fromsuch excursions, a back-bookmark will be generated
automaticly.  To prevent getting fed up with thousands of
back-bookmarks, the total number of theese is limited. See
<^> below.

manual-bookmarks:
After all you may install your own bookmarks, marking often
visited places, say in some manuals. manual-bookmarks are
named by a number.  This number has to be unique whithin
the DVIfile they belong to.  To define a manual-bookmark
use <b>. Since manual-bookmarks belong to the DVIfile they
are defined on, they get lost, when that DVIfile is killed
by <d><k>.

All kind of bookmarks are kept in a ring-buffer. There is a
so called current bookmark of each type. Visiting the
bookmarks along the ring-buffer is done by <w> for file-
and manual-bookmarks, while <^> acts on back-bookmarks.

<b> Define/undefine manual-bookmark.  

       When the current position is not already defined as
       a manual-bookmark, <b> defines one. When an single
       numeric argument (NUM) is given, NUM will be the
       name of the newly defined bookmark. With no
       argument, a name will be generated automaticly. See
       <w> below, for how to visit manual-bookmarks.  When
       the current position is already defined as a
       manual-bookmark, <b> undefines that manual-bookmark.

<w> Move to bookmark.

       When a single numeric argument (NUM) is given, <w>
       moves to the manual-bookmark named NUM, if
       any. Since manual bookmarks are bound to DVIfiles,
       the current DVIfile will never change in that
       case. If no argument is given, <w> goes moves the
       postion either thrue the ring-buffer of
       file-bookmarks or thrue the one of
       manual-bookmarks. To toggle between theese two modi,
       use the magic argument <*>.

<^> Move back
   
       Move to the latest back-bookmark, if any. When a
       single numeric argument (TOTAL) is given, keep the
       TOTAL latest back-bookmarks and discard all the
       others.


CHOOSING WHAT'S ON THE DISPLAY *****************************

<o> Toggle greyscaling
 
       When the zoom-factor is less than 1, the glyphs may
       be displayed using grey-levels, making them more
       smooth. This takes some memory, so you may switch it
       off. On high-res displays there is no need for it
       anyway.

<x> Toggle statusline-information

       While the standard statusline shows you the
       page-number of the current page and the arguments
       you are about to enter, you may select optional
       information for measuring out distances and so. See
       below.

<t> Set unit of measurement

       Whenever you specify arguments which are to describe
       a point on the page, this is done w.r.t. a unit of
       measurement, i.e. cm, mm, a.s.o.. This unit is also
       used, when the position of a mark is displayed in
       the statusline.


MEASURING **************************************************

To allow you to measure distances on the page, there are
two marks, the screenmark, which is fixed on the physical
screen you're looking at, and the pagemark, which is fixed
on the DVIfiles page. When you move the visible-area, the
screenmark acts as drawn with edding on your monitor. The
pagemark acts as drawn on the page. The optional statusline
tells the position of the pagemark relative to the corner
of the sheet of paper you're viewing. It also tells the
position of the screenmark relative to the pagemark. To
measure distances you first may switch this marks on, using
<l>. When the marks are shown, the scrolling commands don't
act on the visible area anymore, but move the screenmark. 
For that case only moving the screenmark at the boarder of
the screen results in scrolling. To move the pagemark just
move the screenmark at the desired position and use <y> to
make the pagemark follow.
 
<l> Show/hide screenmark and pagemark

       This commands takes the two arguments
       (PM_X;PM_Y). The pagemark is put at position PM_X
       PM_Y w.r.t. the upper left corner of the page. The
       Screenmark may be moved with the scrolling-commands.

<y> Set pagemark at the position of the screenmark
       
Beside of these marks there are three rectangles for
measurement. First there is the boarder of the paper setup
by the command-line options -h,-v and -p.  Then there is
the printable area, setup with the -k command-line option.
Third the so called marked rectangle used.

<a> Show/hide marked rectangle

       The four arguments (LEFT,TOP,WIDTH,HIGHT) specify
       the position on page an the size of the marked
       rectangle. When pagemark and screenmark are shown,
       their positions are used as default. When they are
       hidden, the last position of the marked rectangle is
       used as default.

<p> Show/hide printable area 
       
       The four arguments (LEFT,RIGHT,TOP,BOTTOM) specify
       the margins of the printable area, w.r.t. the
       boarder of the page. When pagemark and screenmark
       are shown, the argument (*) sets the printable area
       to the rectangle described by screenmark and
       pagemark.  When they are hidden, (*) takes the
       command-line-options resp. defaults -k of the
       printable area.

<e> Set paper-offset and -size  

       The four arguments (HOFF;VOFF;WIDTH;HEIGHT) specify
       the boarder of the page. Have the top-left corner of
       a sheet of paper in mind. Then (HOFF,VOFF) is the
       offset of the DVIfile's origin to the left boarder
       of the paper.  Standard values are
       HOFF=VOFF=2.54cm. WIDTH and HEIGHT are the width and
       the height of the sheet of paper. The sheet of paper
       is represented only by a frame on the screen. It
       does not affect the drawing of the DVIfile.

       When pagemark and screenmark are shown, the argument
       (*) sets the boarder of the page to the rectangle
       described by screenmark and pagemark.  When they are
       hidden, (*) takes the command-line-options
       resp. defaults -h,-v and -p.


HALF-HYPER *************************************************

tmview does some of the fancy hyper-tex things. I talk
about HALF-hyper-tex, because tmview follows only links
which point to somewhere within the currently visited
dvi-file. So there is no connection to the net or so. But
you might find it usefull (when editing a major project) to
view an equation number this-and-that by clicking on
this-and-that whereever the text refers to that equation.
For information about hyper-tex, related macropackages and
fully compatible viewer scan the net ...

<k> Show/hide half-hyper-mark
<TAB> Goto next href
<RET> Follow current href, if any 


MISC *******************************************************

<s> Search for text

      You will be asked for the text-string to be searched.
      You may enter a regular expression describing that
      string, that includes especially just to enter the
      string as it is.

      tmview will take the entire DVIfile as one huge
      text-string and then search for the next substring,
      fitting the regular expression you've enterd. Thereby
      \"next\" is ment with respect to the current page.

      So far this sounds quite easy, but there are some ugly
      details, based on the fact, that a DVIfile contains
      information on how to draw a bitmap representing your
      text. It does not contain information about from what
      characters in which order your text is made up. Even
      the PKfiles used to draw your text consist only of
      lots of glyphs but no character-codes, like ASCII or
      so. Building a huge text-string from a DVIfile is some
      kind of guessing.

      Fisrt: What kind of huge-string is build from the DVIfile?

      This string will consists of the letters <A> ... <Z>,
      <a> ... <z>, the accent <\"> and the digits <0>
      ... <9>. It does NOT contain anything else, like
      <space>, <ret> or <->:
      Whenbeingprinteditwouldlooklikethisnotreadableatall.  
      Taking the DVIfile as huge string allows you to find
      all locations of a sub-string, say
      \"commandlineoptions\", even those that are seperarted
      by linebreaks (and hyphens) or pagebreaks. In turn,
      there is no chance to find all those locations, where
      \"commandlineoptions\" is seperated by a hypenation.  To
      keep tmview from being confused by headings, there is
      another rule for building up the huge text-string: any
      glyph outside the printable area (see <p>) is
      ignored. So you may setup the printable area to ignore
      headings when searching.

      Second: How is the huge-string build up?

      To translate the list of glyphs found in the DVIfile
      to a text-string, the tfm-files are asked for the
      encoding-scheme. This does work with dc-fonts and
      cm-fonts, since the following encoding-scheme names
      are accepted: \"ASCII\", \"TeX text\", \"TeX math italic\",
      \"TeX math symbols\", \"TeX typewriter text\", \"Extended
      TeX Font Encoding - Latin\", \"Adobe StandardEncoding\".
      The alphanumerics <A>...<Z>, <a>...<z>, <0> ... <9>
      are copied one by one to the huge text-string. Glyphs
      that \"look like\" a simple alphanumeric will be taken
      as that one it looks like.  So the Tex input '\\c o',
      producing an 'o'-with-an-cedilla-accent, will be
      represented as a simple (o) in the text-string. This
      rule also works for all kind of ligatures. The TeX
      input 'ffl\\AE' will be represented by (fflAE).  Any
      accent ON TOP of a glyph will be translated to a (\"),
      preceding whatever the glyph without that accent would
      be translated to.  The TeX input '\\\"a' producing the
      german umlaut 'a'-with-two-dots-on-top, will be found
      as (\"a) in the generated text-string.  The TeX input
      '\aa' producing the scandinavian
      'a'-with-circle-on-top will be found as (\"a) too. Any
      other glyphs are ignored.

      Third: In what does the above result?

      Visiting english documents, say manuals to some
      computer related stuff like elisp.dvi, searching for
      keywords works fine. Searching in documents in which
      extensive use of accents and funny characters is made
      works too, but requires some luck or/and experiance in
      how TeX acts on such things.


      Example: 

      Take the file story.tex from the TeXbook, chapter 6, 
      page 24.  It contains the line
              galaxy called \\\"O\\\"o\\c c,
      The text-string build from the corresponding story.dvi
      will therfore contain
              galaxycalled\"O\"oc
      You may search for ...    getting as result ...
      galaxy                    found
      galaxycalled              found
      galaxy called             not found
      d\"                        found
      galaxy.*\"O\"oc             found
      Ooc                       not found
      
  
<r> Re-read current DVIfile and re-draw screen

<d> Load/kill DVIfile

      After typing <d> you may select between <l> to load a 
      DVIfile and <k> to kill a DVIfile. 
      
      Loading a DVIFile: 

      tmview will look for a file-bookmark belonging to that
      file. If there is one, it becomes the current
      file-bookmark. The DVIfile will be shown as left, and
      any defined manual-bookmarks are accessable by
      <w>. When loading a DVIfile for the first time, a new
      file-bookmark will be generated.  This will be setup
      with default values from the command-line options and
      won't contain any manual-bookmarks.

      Killing a DVIfile:
      To kill a DVIfile means to kill its file-bookmark and
      any related manual-bookmark. Killing a DVIfile won't
      hurt the file itself.  You don't have to kill a
      DVIfile just to load another one.

<q> Quit tmview 
    
      When quitting, a startup-file will be written. When
      running tmview next time, you will find almost
      everything as you left it.


**********************************************************
End of help***********************************************
\n\n\n"

char helpstr[]=HELPSTR;



























