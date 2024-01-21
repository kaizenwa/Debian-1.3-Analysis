% Copyright 1990 - 1995 by AT&T Bell Laboratories.

% Permission to use, copy, modify, and distribute this software
% and its documentation for any purpose and without fee is hereby
% granted, provided that the above copyright notice appear in all
% copies and that both that the copyright notice and this
% permission notice and warranty disclaimer appear in supporting
% documentation, and that the names of AT&T Bell Laboratories or
% any of its entities not be used in advertising or publicity
% pertaining to distribution of the software without specific,
% written prior permission.

% AT&T disclaims all warranties with regard to this software,
% including all implied warranties of merchantability and fitness.
% In no event shall AT&T be liable for any special, indirect or
% consequential damages or any damages whatsoever resulting from
% loss of use, data or profits, whether in an action of contract,
% negligence or other tortious action, arising out of or in
% connection with the use or performance of this software.


% Change file for C version of MetaPost
%
% Derived from mf.ch (the change file for the C version of mf).
% Comments beginning with bracketed numbers of the form [pp.nnn] refer to
% the corresponding part and module number in mf.web as it appears in Vol D.
%
% Numbers of the form [pp.nnn] now refer to the corresponding part and
% module number of mp.web as implementors who read this change file are
% much better advised to use a current WEB listing of MetaPost instead
% of a possibly dated copy of Vol. D.

% Modification History:
%
% Revision 0.62  Jan 4 '95  by John Hobby
% (Changes that only effect the banner line are not listed here)
%
% Updated for web2c-6.1/kpathsea-2.6  Jan 29 '95  by Ulrik Vieth
% according to the current version of mf.ch.
%
% - produce separate binaries for INIMP, VIRMF (no '-I' flag!)
% - make a big version of MP by default (cf. TeX and METAFONT)
% - allow any input characters (0..255) (cf. TeX and METAFONT)
% - use web2c/kpathsea routines in file opening and searching
%
% Updated for Revision 0.63  Apr 12 '95  by Ulrik Vieth
%
% - fixed the parsing routines for ps_tab_file to be able to handle 
%   comments and blank lines in dvipsk's version of "psfonts.map".
% - increased max_read_files to 30 to avoid running out of read files
%   when reading stops before reaching EOF. (Better solution: write 
%   a 'closefrom' macro that reads to EOF, which causes file closing.)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [0] WEAVE: print changes only.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
\def\botofcontents{\vskip 0pt plus 1fil minus 1.5in}
@y
\def\botofcontents{\vskip 0pt plus 1fil minus 1.5in}
\let\maybe=\iffalse
\def\title{\MP\ changes for C}
\def\glob{13}\def\gglob{20, 25} % these are defined in module 1
\font\mc=cmr9
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [1.2] banner line
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d banner=='This is MetaPost, Version 0.631' {printed when \MP\ starts}
@y
@d banner=='This is MetaPost, Version 0.631' {more is printed later}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [1.7] Convert `debug..gubed' and `stat..tats' into #ifdefs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d debug==@{ {change this to `$\\{debug}\equiv\null$' when debugging}
@d gubed==@t@>@} {change this to `$\\{gubed}\equiv\null$' when debugging}
@y
@d debug==ifdef('DEBUG')
@d gubed==endif('DEBUG')
@z

@x
@d stat==@{ {change this to `$\\{stat}\equiv\null$' when gathering
  usage statistics}
@d tats==@t@>@} {change this to `$\\{tats}\equiv\null$' when gathering
  usage statistics}
@y
@d stat==ifdef('STAT')
@d tats==endif('STAT')
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [1.8] Same, for `init..tini'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d init== {change this to `$\\{init}\equiv\.{@@\{}$' in the production version}
@d tini== {change this to `$\\{tini}\equiv\.{@@\}}$' in the production version}
@y
@d init==ifdef('INIMP')
@d tini==endif('INIMP')
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [1.9] Get rid of compiler directives.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@<Compiler directives@>=
@{@&$C-,A+,D-@} {no range check, catch arithmetic overflow, no debug overhead}
@!debug @{@&$C+,D+@}@+ gubed {but turn everything on when debugging}
@y
@<Compiler directives@>=
{No compiler directives for C.}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [1.11] Compile-time constants.  Although we only change a few of
% these, listing them all makes the patch file for a big MetaPost simpler.
% 16K for BSD I/O; file_name_size is set from the system constant.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@<Constants...@>=
@!mem_max=30000; {greatest index in \MP's internal |mem| array;
  must be strictly less than |max_halfword|;
  must be equal to |mem_top| in \.{INIMP}, otherwise |>=mem_top|}
@!max_internal=100; {maximum number of internal quantities}
@!buf_size=500; {maximum number of characters simultaneously present in
  current lines of open files; must not exceed |max_halfword|}
@!error_line=72; {width of context lines on terminal error messages}
@!half_error_line=42; {width of first lines of contexts in terminal
  error messages; should be between 30 and |error_line-15|}
@!max_print_line=79; {width of longest text lines output; should be at least 60}
@!emergency_line_length=255;
  {\ps\ output lines can be this long in unusual circumstances}
@!stack_size=30; {maximum number of simultaneous input sources}
@!max_read_files=4; {maximum number of simultaneously open \&{readfrom} files}
@!max_strings=2500; {maximum number of strings; must not exceed |max_halfword|}
@!string_vacancies=9000; {the minimum number of characters that should be
  available for the user's identifier names and strings,
  after \MP's own error messages are stored}
@!strings_vacant=1000; {the minimum number of strings that should be available}
@!pool_size=32000; {maximum number of characters in strings, including all
  error messages and help texts, and the names of all identifiers;
  must exceed |string_vacancies| by the total
  length of \MP's own strings, which is currently about 22000}
@!font_max=50; {maximum font number for included text fonts}
@!font_mem_size=10000; {number of words for \.{TFM} information for text fonts}
@!file_name_size=40; {file names shouldn't be longer than this}
@!pool_name='MPlib:MP.POOL                         ';
  {string of length |file_name_size|; tells where the string pool appears}
@.MPlib@>
@!ps_tab_name='MPlib:PSFONTS.MAP                     ';
  {string of length |file_name_size|; locates font name translation table}
@!path_size=300; {maximum number of knots between breakpoints of a path}
@!bistack_size=785; {size of stack for bisection algorithms;
  should probably be left at this value}
@!header_size=100; {maximum number of \.{TFM} header words, times~4}
@!lig_table_size=5000; {maximum number of ligature/kern steps, must be
  at least 255 and at most 32510}
@!max_kerns=500; {maximum number of distinct kern amounts}
@!max_font_dimen=50; {maximum number of \&{fontdimen} parameters}
@y
@d file_name_size == PATH_MAX

@<Constants...@>=
@!mem_max=262140; {greatest index in \MP's internal |mem| array;
  must be strictly less than |max_halfword|;
  must be equal to |mem_top| in \.{INIMP}, otherwise |>=mem_top|}
@!max_internal=300; {maximum number of internal quantities}
@!buf_size=3000; {maximum number of characters simultaneously present in
  current lines of open files; must not exceed |max_halfword|}
@!error_line=79; {width of context lines on terminal error messages}
@!half_error_line=50; {width of first lines of contexts in terminal
  error messages; should be between 30 and |error_line-15|}
@!max_print_line=79; {width of longest text lines output; should be at least 60}
@!emergency_line_length=255;
  {\ps\ output lines can be this long in unusual circumstances}
@!stack_size=300; {maximum number of simultaneous input sources}
@!max_read_files=30; {maximum number of simultaneously open \&{readfrom} files}
@!max_strings=7500; {maximum number of strings; must not exceed |max_halfword|}
@!string_vacancies=74000; {the minimum number of characters that should be
  available for the user's identifier names and strings,
  after \MP's own error messages are stored}
@!strings_vacant=1000; {the minimum number of strings that should be available}
@!pool_size=100000; {maximum number of characters in strings, including all
  error messages and help texts, and the names of all identifiers;
  must exceed |string_vacancies| by the total
  length of \MP's own strings, which is currently about 22000}
@!font_max=50; {maximum font number for included text fonts}
@!font_mem_size=10000; {number of words for \.{TFM} information for text fonts}
@!pool_name='mp.pool';
  {string of length |file_name_size|; tells where the string pool appears}
@.MPlib@>
@!ps_tab_name='psfonts.map';
  {string of length |file_name_size|; locates font name translation table}
@!path_size=1000; {maximum number of knots between breakpoints of a path}
@!bistack_size=1500; {size of stack for bisection algorithms;
  should probably be left at this value}
@!header_size=100; {maximum number of \.{TFM} header words, times~4}
@!lig_table_size=15000; {maximum number of ligature/kern steps, must be
  at least 255 and at most 32510}
@!max_kerns=2500; {maximum number of distinct kern amounts}
@!max_font_dimen=50; {maximum number of \&{fontdimen} parameters}
@!mem_top=262140; {largest index in the |mem| array dumped by \.{INIMP};
  must be substantially larger than |mem_min|
  and not greater than |mem_max|}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [1.12] Sensitive compile-time constants.  mem_top is made into a
% #define, so it is easier to change for the trap test.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d mem_min=0 {smallest index in the |mem| array, must not be less
  than |min_halfword|}
@d mem_top==30000 {largest index in the |mem| array dumped by \.{INIMP};
  must be substantially larger than |mem_min|
  and not greater than |mem_max|}
@d hash_size=2100 {maximum number of symbolic tokens,
  must be less than |max_halfword-3*param_size|}
@d hash_prime=1777 {a prime number equal to about 85\pct! of |hash_size|}
@d max_in_open=6 {maximum number of input files and error insertions that
  can be going on simultaneously}
@d param_size=150 {maximum number of simultaneous macro parameters}
@d max_write_files=4 {maximum number of simultaneously open \&{write} files}
@y
@d mem_min=0 {smallest index in the |mem| array, must not be less
  than |min_halfword|}
@d hash_size=9500 {maximum number of symbolic tokens,
  must be less than |max_halfword-3*param_size|}
@d hash_prime=7919 {a prime number equal to about 85\pct! of |hash_size|}
@d max_in_open=15 {maximum number of input files and error insertions that
  can be going on simultaneously}
@d param_size=150 {maximum number of simultaneous macro parameters}
@d max_write_files=4 {maximum number of simultaneously open \&{write} files}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [1.16] Use C macros for `incr' and `decr'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d incr(#) == #:=#+1 {increase a variable by unity}
@d decr(#) == #:=#-1 {decrease a variable by unity}
@y
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [2.19] The text_char type is used as an array index into xord.  The
% default type `char' produces signed integers, which are bad array
% indices in C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
@x
@d text_char == char {the data type of characters in text files}
@y
@d text_char == ASCII_code {the data type of characters in text files}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [2.22] Allow any character as input.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@^character set dependencies@>
@^system dependencies@>

@<Set init...@>=
for i:=0 to @'37 do xchr[i]:=' ';
for i:=@'177 to @'377 do xchr[i]:=' ';
@y
@^character set dependencies@>
@^system dependencies@>

@d tab = @'11 { ASCII horizontal tab }
@d form_feed = @'14 { ASCII form feed }

@<Set init...@>=
for i:=0 to @'37 do xchr[i]:=chr(i);
for i:=@'177 to @'377 do xchr[i]:=chr(i);
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3.24] Remove file types we don't need.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
The program actually makes use also of a third kind of file, called a
|word_file|, when dumping and reloading mem information for its own
initialization.  We shall define a word file later; but it will be possible
for us to specify simple operations on word files before they are defined.
@y
I/O in C is done using standard I/O.  We will define the path numbers
in an include file for C which are used in searching for files to be
read.  We'll define all the file types in C also.
@z

@x
@!alpha_file=packed file of text_char; {files that contain textual data}
@!byte_file=packed file of eight_bits; {files that contain binary data}
@y
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3.26] Do file opening in C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ The \ph\ compiler with which the original version of \MF\ was prepared
extends the rules of \PASCAL\ in a very convenient way. To open file~|f|,
we can write
$$\vbox{\halign{#\hfil\qquad&#\hfil\cr
|reset(f,@t\\{name}@>,'/O')|&for input;\cr
|rewrite(f,@t\\{name}@>,'/O')|&for output.\cr}}$$
The `\\{name}' parameter, which is of type `\ignorespaces|packed
array[@t\<\\{any}>@>] of text_char|', stands for the name of
the external file that is being opened for input or output.
Blank spaces that might appear in \\{name} are ignored.

The `\.{/O}' parameter tells the operating system not to issue its own
error messages if something goes wrong. If a file of the specified name
cannot be found, or if such a file cannot be opened for some other reason
(e.g., someone may already be trying to write the same file), we will have
|@!erstat(f)<>0| after an unsuccessful |reset| or |rewrite|.  This allows
\MP\ to undertake appropriate corrective action.
@:PASCAL H}{\ph@>
@^system dependencies@>

\MP's file-opening procedures return |false| if no file identified by
|name_of_file| could be opened.

@d reset_OK(#)==erstat(#)=0
@d rewrite_OK(#)==erstat(#)=0

@p function a_open_in(var @!f:alpha_file):boolean;
  {open a text file for input}
begin reset(f,name_of_file,'/O'); a_open_in:=reset_OK(f);
end;
@#
function a_open_out(var @!f:alpha_file):boolean;
  {open a text file for output}
begin rewrite(f,name_of_file,'/O'); a_open_out:=rewrite_OK(f);
end;
@#
function b_open_in(var @!f:byte_file):boolean;
  {open a binary file for input}
begin rewrite(f,name_of_file,'/O'); b_open_in:=rewrite_OK(f);
end;
@#
function b_open_out(var @!f:byte_file):boolean;
  {open a binary file for output}
begin rewrite(f,name_of_file,'/O'); b_open_out:=rewrite_OK(f);
end;
@#
function w_open_in(var @!f:word_file):boolean;
  {open a word file for input}
begin reset(f,name_of_file,'/O'); w_open_in:=reset_OK(f);
end;
@#
function w_open_out(var @!f:word_file):boolean;
  {open a word file for output}
begin rewrite(f,name_of_file,'/O'); w_open_out:=rewrite_OK(f);
end;
@y
@ All of the file opening functions are defined in C.
@d no_file_path = -1
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3.27] Do file closing in C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ Files can be closed with the \ph\ routine `|close(f)|', which
@^system dependencies@>
should be used when all input or output with respect to |f| has been completed.
This makes |f| available to be opened again, if desired; and if |f| was used for
output, the |close| operation makes the corresponding external file appear
on the user's area, ready to be read.

@p procedure a_close(var @!f:alpha_file); {close a text file}
begin close(f);
end;
@#
procedure b_close(var @!f:byte_file); {close a binary file}
begin close(f);
end;
@#
procedure w_close(var @!f:word_file); {close a word file}
begin close(f);
end;
@y
@ And all the file closing routines as well.
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3.30] Do `input_ln' in C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
Standard \PASCAL\ says that a file should have |eoln| immediately
before |eof|, but \MP\ needs only a weaker restriction: If |eof|
occurs in the middle of a line, the system function |eoln| should return
a |true| result (even though |f^| will be undefined).

@p function input_ln(var @!f:alpha_file;@!bypass_eoln:boolean):boolean;
  {inputs the next line or returns |false|}
var @!last_nonblank:0..buf_size; {|last| with trailing blanks removed}
begin if bypass_eoln then if not eof(f) then get(f);
  {input the first character of the line into |f^|}
last:=first; {cf.\ Matthew 19\thinspace:\thinspace30}
if eof(f) then input_ln:=false
else  begin last_nonblank:=first;
  while not eoln(f) do
    begin if last>=max_buf_stack then
      begin max_buf_stack:=last+1;
      if max_buf_stack=buf_size then
        @<Report overflow of the input buffer, and abort@>;
      end;
    buffer[last]:=xord[f^]; get(f); incr(last);
    if buffer[last-1]<>" " then last_nonblank:=last;
    end;
  last:=last_nonblank; input_ln:=true;
  end;
end;
@y
We define |input_ln| in C, for efficiency.
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3.31] `term_in' and `term_out' are standard input and output.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
@x
@<Glob...@>=
@!term_in:alpha_file; {the terminal as an input file}
@!term_out:alpha_file; {the terminal as an output file}
@y
@d term_in==stdin {the terminal as an input file}
@d term_out==stdout {the terminal as an output file}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3.32] We don't need to open the terminal files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ Here is how to open the terminal files
in \ph. The `\.{/I}' switch suppresses the first |get|.
@^system dependencies@>

@d t_open_in==reset(term_in,'TTY:','/O/I') {open the terminal for text input}
@d t_open_out==rewrite(term_out,'TTY:','/O') {open the terminal for text output}
@y
@ Here is how to open the terminal files.  |t_open_out| does nothing.
|t_open_in|, on the other hand, does the work of ``rescanning,'' or getting
any command line arguments the user has provided.  It's defined in C.
  
@d t_open_out == {output already open for text output}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3.33] Flushing output.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
these operations can be specified in \ph:
@^system dependencies@>

@d update_terminal == break(term_out) {empty the terminal output buffer}
@d clear_terminal == break_in(term_in,true) {clear the terminal input buffer}
@y
these operations can be specified with {\mc UNIX}.  |update_terminal|
does an |fflush| (via the macro |flush|). |clear_terminal| is redefined
to do nothing, since the user should control the terminal.
@^system dependencies@>

@d update_terminal == flush (term_out)
@d clear_terminal == do_nothing
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3.36] Reading the command line.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ The following program does the required initialization
without retrieving a possible command line.
It should be clear how to modify this routine to deal with command lines,
if the system permits them.
@^system dependencies@>

@p function init_terminal:boolean; {gets the terminal input started}
label exit;
begin t_open_in;
loop@+begin wake_up_terminal; write(term_out,'**'); update_terminal;
@.**@>
  if not input_ln(term_in,true) then {this shouldn't happen}
    begin write_ln(term_out);
    write(term_out,'! End of file on the terminal... why?');
@.End of file on the terminal@>
    init_terminal:=false; return;
    end;
  loc:=first;
  while (loc<last)and(buffer[loc]=" ") do incr(loc);
  if loc<last then
    begin init_terminal:=true;
    return; {return unless the line was all blank}
    end;
  write_ln(term_out,'Please type the name of your input file.');
  end;
exit:end;
@y
@ The following program does the required initialization.
Iff anything has been specified on the command line, then |t_open_in|
will return with |last > first|.
@^system dependencies@>

@p
function init_terminal:boolean; {gets the terminal input started}
label exit;
begin
    t_open_in;
    if last > first then begin
        loc := first;
        while (loc < last) and (buffer[loc]=' ') do
	    incr(loc);
	if (loc < last-1) then 
	  if (buffer[loc]='-') and (buffer[loc+1]='T')
          then begin
            troff_mode := true; loc := loc + 2;
          end;
        if loc < last then begin
            init_terminal := true;
            goto exit;
        end;
    end;
    loop@+begin
        wake_up_terminal; write(term_out, '**'); update_terminal;
@.**@>
        if not input_ln(term_in,true) then begin {this shouldn't happen}
            write_ln(term_out);
            write(term_out, '! End of file on the terminal... why?');
@.End of file on the terminal@>
            init_terminal:=false;
	    return;
        end;

        loc:=first;
        while (loc<last)and(buffer[loc]=" ") do
            incr(loc);

        if loc<last then begin
           init_terminal:=true;
           return; {return unless the line was all blank}
        end;
        write_ln(term_out, 'Please type the name of your input file.');
    end;
exit:
end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [4.65] Open the pool file using a path, and can't do string
% assignments directly.  (`strcpy' and `strlen' work here because
% `pool_name' is a constant string, and thus ends in a null and doesn't
% start with a space.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
name_of_file:=pool_name; {we needn't set |name_length|}
if a_open_in(pool_file) then
@y
vstrcpy (name_of_file+1, pool_name); {copy the string}
name_of_file[0] := ' ';
name_of_file[strlen (pool_name) + 1] := ' ';
name_length := strlen (pool_name);
if a_open_in (pool_file, MP_POOL_PATH) then
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [4.65,66,67] Make `MF.POOL' lowercase in messages, and change how
% it's read.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
else  bad_pool('! I can''t read MP.POOL.')
@y
else  bad_pool('! I can''t read mp.pool.')
@z
   
@x
begin if eof(pool_file) then bad_pool('! MP.POOL has no check sum.');
@.MP.POOL has no check sum@>
read(pool_file,m,n); {read two digits of string length}
@y
begin if eof(pool_file) then bad_pool('! mp.pool has no check sum.');
@.MP.POOL has no check sum@>
read(pool_file,m); read(pool_file,n); {read two digits of string length}
@z
@x
    bad_pool('! MP.POOL line doesn''t begin with two digits.');
@y
    bad_pool('! mp.pool line doesn''t begin with two digits.');
@z
@x
  bad_pool('! MP.POOL check sum doesn''t have nine digits.');
@y
  bad_pool('! mp.pool check sum doesn''t have nine digits.');
@z
@x
done: if a<>@$ then bad_pool('! MP.POOL doesn''t match; TANGLE me again.');
@y
done: if a<>@$ then bad_pool('! mp.pool doesn''t match; tangle me again.');
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [5.75] Print rest of banner, eliminate the misleading message
% ``(no base preloaded)''.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ By popular demand, \MP\ prints the banner line only on the transcript file.
Thus there is nothing special to be printed here.

@<Initialize the output...@>=
update_terminal;
@y
@ Here is the very first thing that \MP\ prints: a headline that identifies
the version number and base name. The |term_offset| variable is temporarily
incorrect, but the discrepancy is not serious since we assume that the banner
and mem identifier together will occupy at most |max_print_line|
character positions.

@<Initialize the output...@>=
wterm (banner);
wterm (version_string);
if mem_ident>0 then print(mem_ident); print_ln;
update_terminal;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [6.90] Eliminate non-local goto.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@<Error hand...@>=
procedure jump_out;
begin goto end_of_MP;
end;
@y
@d do_final_end==begin
   update_terminal;
   ready_already:=0;
   if (history <> spotless) and (history <> warning_issued) then
       uexit(1)
   else
       uexit(0);
   end
@<Error hand...@>=
procedure jump_out;
begin
close_files_and_terminate;
do_final_end;
end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [6.93] Handle the switch-to-editor option.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
line ready to be edited. But such an extension requires some system
wizardry, so the present implementation simply types out the name of the
file that should be
edited and the relevant line number.
@^system dependencies@>

There is a secret `\.D' option available when the debugging routines haven't
been commented~out.
@^debugging@>
@y
line ready to be edited.
We do this by calling the external procedure |call_edit| with a pointer to
the filename, its length, and the line number.
However, here we just set up the variables that will be used as arguments,
since we don't want to do the switch-to-editor until after \MP\ has closed
its files.
@^system dependencies@>

There is a secret `\.D' option available when the debugging routines have
not been commented out.
@^debugging@>
@d edit_file==input_stack[file_ptr]
@z
@x
"E": if file_ptr>0 then
  begin print_nl("You want to edit file ");
@.You want to edit file x@>
  print(input_stack[file_ptr].name_field);
  print(" at line "); print_int(true_line);@/
  interaction:=scroll_mode; jump_out;
@y
"E": if file_ptr>0 then
    begin
    edit_name_start:=str_start[edit_file.name_field];
    edit_name_length:=length(edit_file.name_field);
    edit_line:=true_line;
    jump_out;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [7.96] Use a C macro for halfp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d halfp(#)==(#) div 2
@y
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [7.107-7.115] Optionally replace make_fraction etc. with external routines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@p function make_fraction(@!p,@!q:integer):fraction;
@y
In the C version, there are external routines that use double precision
floating point to simulate functions such as |make_fraction|.  This is carefully
done to be virtually machine-independent and it gives up to 12 times speed-up
on machines with hardware floating point.  Since some machines do not have fast
double-precision floating point, we provide a C preprocessor switch that allows
selecting the standard versions given below.

@p ifdef('FIXPT')@/
function make_fraction(@!p,@!q:integer):fraction;
@z
@x
  if negative then make_fraction:=-(f+n)@+else make_fraction:=f+n;
  end;
end;
@y
  if negative then make_fraction:=-(f+n)@+else make_fraction:=f+n;
  end;
end;@/
endif('FIXPT')
@z
@x
@p function take_fraction(@!q:integer;@!f:fraction):integer;
@y
@p ifdef('FIXPT')@/
function take_fraction(@!q:integer;@!f:fraction):integer;
@z
@x
else take_fraction:=n+p;
end;
@y
else take_fraction:=n+p;
end;@/
endif('FIXPT')
@z
@x
@p function take_scaled(@!q:integer;@!f:scaled):integer;
@y
@p ifdef('FIXPT')@/
function take_scaled(@!q:integer;@!f:scaled):integer;
@z
@x
else take_scaled:=n+p;
end;
@y
else take_scaled:=n+p;
end;@/
endif('FIXPT')
@z
@x
operands are positive. \ (This procedure is not used especially often,
so it is not part of \MP's inner loop.)

@p function make_scaled(@!p,@!q:integer):scaled;
@y
operands are positive. \ (This procedure is not used especially often,
so it is not part of \MP's inner loop, but we might as well allow for
an external C routine.)

@p ifdef('FIXPT')@/
function make_scaled(@!p,@!q:integer):scaled;
@z
@x
  if negative then make_scaled:=-(f+n)@+else make_scaled:=f+n;
  end;
end;
@y
  if negative then make_scaled:=-(f+n)@+else make_scaled:=f+n;
  end;
end;@/
endif('FIXPT')
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [9.153] Make it easy to build a bigger MetaPost.  (Nothing is changed
% in the basic version.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d min_quarterword=0 {smallest allowable value in a |quarterword|}
@d max_quarterword=255 {largest allowable value in a |quarterword|}
@d min_halfword==0 {smallest allowable value in a |halfword|}
@d max_halfword==65535 {largest allowable value in a |halfword|}
@y
@d min_quarterword=0 {smallest allowable value in a |quarterword|}
@d max_quarterword=255 {largest allowable value in a |quarterword|}
@d min_halfword==0 {smallest allowable value in a |halfword|}
@d max_halfword==262143 {largest allowable value in a |halfword|}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [9.155] Efficiency in C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ The operation of subtracting |min_halfword| occurs rather frequently in
\MP, so it is convenient to abbreviate this operation by using the macro
|ho| defined here.  \MP\ will run faster with respect to compilers that
don't optimize the expression `|x-0|', if this macro is simplified in the
obvious way when |min_halfword=0|. Similarly, |qi| and |qo| are used for
input to and output from quarterwords.
@^system dependencies@>

@d ho(#)==#-min_halfword
  {to take a sixteen-bit item from a halfword}
@d qo(#)==#-min_quarterword {to read eight bits from a quarterword}
@d qi(#)==#+min_quarterword {to store eight bits in a quarterword}

@y
@ The operation of subtracting |min_halfword| occurs rather frequently in
\MP, so it is convenient to abbreviate this operation by using the macro
|ho| defined here.  \MP\ will run faster with respect to compilers that
don't optimize the expression `|x-0|', if this macro is simplified in the
obvious way when |min_halfword=0|. Similarly, |qi| and |qo| are used for
input to and output from quarterwords.

We need not do this in C, since the |min_xxx| values are all zero, and
we can't depend on most C compilers to optimize this.
@^system dependencies@>

@d ho(#)==#
@d qo(#)==#
@d qi(#)==#
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [9.156] We've put the memory structure into the include file `texmf.h',
% since it's too hard to translate automatically.  Also, remove the
% `word_file' type.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@!two_halves = packed record@;@/
  @!rh:halfword;
  case two_choices of
  1: (@!lh:halfword);
  2: (@!b0:quarterword; @!b1:quarterword);
  end;
@!four_quarters = packed record@;@/
  @!b0:quarterword;
  @!b1:quarterword;
  @!b2:quarterword;
  @!b3:quarterword;
  end;
@!memory_word = record@;@/
  case three_choices of
  1: (@!int:integer);
  2: (@!hh:two_halves);
  3: (@!qqqq:four_quarters);
  end;
@!word_file = file of memory_word;
@y
@=#include "texmfmem.h";@>
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [10.169] Fix an unsigned/signed problem in getnode.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if r>p+1 then @<Allocate from the top of node |p| and |goto found|@>;
@y
if r>toint(p+1) then @<Allocate from the top of node |p| and |goto found|@>;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [11.178] Change the word `free' so that it doesn't conflict with the
% standard C library routine of the same name.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
been included. (You may want to decrease the size of |mem| while you
@^debugging@>
are debugging.)
@y
been included. (You may want to decrease the size of |mem| while you
@^debugging@>
are debugging.)

@d free==free_arr
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [11.182] Eliminate two unsigned comparisons to zero.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
repeat if (p>=lo_mem_max)or(p<mem_min) then clobbered:=true
  else if (rlink(p)>=lo_mem_max)or(rlink(p)<mem_min) then clobbered:=true
@y
repeat if (p>=lo_mem_max) then clobbered:=true
  else if (rlink(p)>=lo_mem_max) then clobbered:=true
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [12.194] Do `fix_date_and_time' in C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ The following procedure, which is called just before \MP\ initializes its
input and output, establishes the initial values of the date and time.
@^system dependencies@>
Since standard \PASCAL\ cannot provide such information, something special
is needed. The program here simply specifies July 4, 1776, at noon; but
users probably want a better approximation to the truth.

Note that the values are |scaled| integers. Hence \MP\ can no longer
be used after the year 32767.

@p procedure fix_date_and_time;
begin internal[time]:=12*60*unity; {minutes since midnight}
internal[day]:=4*unity; {fourth day of the month}
internal[month]:=7*unity; {seventh month of the year}
internal[year]:=1776*unity; {Anno Domini}
end;
@y
@ The following procedure, which is called just before \MP\ initializes its
input and output, establishes the initial values of the date and time.
It is calls an externally defined |date_and_time|, even though it could
be done from Pascal.
The external procedure also sets up interrupt catching.
@^system dependencies@>

Note that the values are |scaled| integers. Hence \MP\ can no longer
be used after the year 32767.

@p procedure fix_date_and_time;
begin
    date_and_time(internal[time],internal[day],internal[month],internal[year]);
    internal[time] := internal[time] * unity;
    internal[day] := internal[day] * unity;
    internal[month] := internal[month] * unity;
    internal[year] := internal[year] * unity;
end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [12.199] Allow <tab> and <form feed> as input.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
for k:=0 to " "-1 do char_class[k]:=invalid_class;
for k:=127 to 255 do char_class[k]:=invalid_class;
@y
for k:=0 to " "-1 do char_class[k]:=invalid_class;
for k:=127 to 255 do char_class[k]:=invalid_class;
char_class[tab]:=space_class;
char_class[form_feed]:=space_class;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [29.629] Allow invalid 8 bit codes when |tex_flushing|.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
invalid_class: @<Decry the invalid character and |goto restart|@>;
@y
invalid_class: if scanner_status=tex_flushing then goto switch
  else @<Decry the invalid character and |goto restart|@>;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.744] area and extension rules.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ The file names we shall deal with for illustrative purposes have the
following structure:  If the name contains `\.>' or `\.:', the file area
consists of all characters up to and including the final such character;
otherwise the file area is null.  If the remaining file name contains
`\..', the file extension consists of all such characters from the first
remaining `\..' to the end, otherwise the file extension is null.
@^system dependencies@>

We can scan such file names easily by using two global variables that keep track
of the occurrences of area and extension delimiters.  Note that these variables
cannot be of type |pool_pointer| because a string pool compaction could occur
while scanning a file name.

@<Glob...@>=
@!area_delimiter:integer;
  {most recent `\.>' or `\.:' relative to |str_start[str_ptr]|}
@!ext_delimiter:integer; {the relevant `\..', if any}
@y
@ The file names we shall deal with have the
following structure:  If the name contains `\./', the file area
consists of all characters up to and including the final such character;
otherwise the file area is null.  If the remaining file name contains
`\..', the file extension consists of all such characters from the first
remaining `\..' to the end, otherwise the file extension is null.
@^system dependencies@>

We can scan such file names easily by using two global variables that keep track
of the occurrences of area and extension delimiters.  Note that these variables
cannot be of type |pool_pointer| because a string pool compaction could occur
while scanning a file name.

@<Glob...@>=
@!area_delimiter:integer; {most recent `\./' relative to |str_start[str_ptr]|}
@!ext_delimiter:integer; {the relevant `\..', if any}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.745] MP and MF area directories.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d MP_area=="MPinputs:"
@.MPinputs@>
@d MF_area=="MFinputs:"
@.MFinputs@>
@d MP_font_area=="TeXfonts:"
@.TeXfonts@>
@y
In C, the default paths are specified in a separate
file, \.{site.h}.  The file opening procedures do path searching
based either on those default paths, or on paths given by the user
in environment variables.
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.747] more_name
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
begin if c=" " then more_name:=false
else  begin if (c=">")or(c=":") then
@y
begin if (c=" ")or(c=tab) then more_name:=false
else  begin if (c="/") then
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.750] In pack_file_name, leave room for the extra null we append at
% the end of a filename in make_c_string.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if k<=file_name_size then name_length:=k@+else name_length:=file_name_size;
@y
if k<file_name_size then name_length:=k@+else name_length:=file_name_size-1;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.751] The default mem and troff_mode flag.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d mem_default_length=15 {length of the |MP_mem_default| string}
@d mem_area_length=6 {length of its area part}
@d mem_ext_length=4 {length of its `\.{.mem}' part}
@y
In C, we don't give the area part, instead depending
on the path searching that will happen during file opening.  Also, the
length will be set in the main program.
We also need a variable that keeps track of whether the \.{-T} flag
has been used to request \.{troff} mode.

@d mem_area_length=0 {length of its area part}
@d mem_ext_length=4 {length of its `\.{.mem}' part}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.752] Where `plain.mem' is.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@!MP_mem_default:packed array[1..mem_default_length] of char;

@ @<Set init...@>=
MP_mem_default:='MPlib:plain.mem';
@.MPlib@>
@.plain@>
@^system dependencies@>
@y
@!mem_default_length: integer;
@!MP_mem_default: c_char_pointer;
@!troff_mode:boolean; {has the user requested \.{troff} mode?}

@ We set the name of the default format file and the length of that name
in C, instead of Pascal, since we want them to depend on the name of the
program.
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.754] Change to pack_buffered_name as with pack_file_name.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if k<=file_name_size then name_length:=k@+else name_length:=file_name_size;
@y
if k<file_name_size then name_length:=k@+else name_length:=file_name_size-1;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.755] Mem file opening: only try once, with path searching.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
  pack_buffered_name(0,loc,j-1); {try first without the system file area}
  if w_open_in(mem_file) then goto found;
  pack_buffered_name(mem_area_length,loc,j-1);
    {now try the system mem file area}
  if w_open_in(mem_file) then goto found;
@y
  pack_buffered_name(0,loc,j-1);
  if w_open_in(mem_file) then goto found;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (still [35.755]) Replace `PLAIN' in error messages with `default'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
  wterm_ln('Sorry, I can''t find that mem file;',' will try PLAIN.');
@y
  wterm_ln('Sorry, I can''t find that mem file;',' will try the default.');
@z
@x
  wterm_ln('I can''t find the PLAIN mem file!');
@.I can't find PLAIN...@>
@y
  wterm_ln('I can''t find the default mem file!');
@.I can't find default base...@>
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.757] Make scan_file_name ignore leading tabs as well as spaces.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@p procedure scan_file_name;
label done;
begin begin_name;
while buffer[loc]=" " do incr(loc);
@y
@p procedure scan_file_name;
label done;
begin begin_name;
while (buffer[loc]=" ")or(buffer[loc]=tab) do incr(loc);
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.759] `logname' is declared in <unistd.h> on some systems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
`\.{.mem}' and `\.{.tfm}' in order to make the names of \MP's output files.
@y
`\.{.mem}' and `\.{.tfm}' in order to make the names of \MP's output files.

@d log_name == texmf_log_name
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.763] <Scan file name...> needs similar leading tab treatment.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ @<Scan file name in the buffer@>=
begin begin_name; k:=first;
while (buffer[k]=" ")and(k<last) do incr(k);
@y
@ @<Scan file name in the buffer@>=
begin begin_name; k:=first;
while ((buffer[k]=" ")or(buffer[k]=tab))and(k<last) do incr(k);
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.766] Adjust for C string conventions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@!months:packed array [1..36] of char; {abbreviations of month names}
@y
@!months:c_char_pointer;
@z

@x
begin wlog(banner);
print(mem_ident); print("  ");
print_int(round_unscaled(internal[day])); print_char(" ");
months:='JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC';
@y
begin wlog(banner);
wlog (version_string);
print(mem_ident); print("  ");
print_int(round_unscaled(internal[day])); print_char(" ");
months := ' JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC';
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.767] a_open_in of input file needs path specifier; also, try to
% open the file with and without the `.mf'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if a_open_in(cur_file) then try_extension:=true
else begin if str_vs_str(ext,".mf")=0 then in_area:=MF_area
  else in_area:=MP_area;
  pack_file_name(cur_name,in_area,ext);
  try_extension:=a_open_in(cur_file);
  end;
@y
if str_vs_str(ext,".mf")=0 then
  try_extension:=a_open_in(cur_file,MF_INPUT_PATH)
else try_extension:=a_open_in(cur_file,MP_INPUT_PATH);
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.770] Get rid of return of name to string pool.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@<Flush |name| and replace it with |cur_name| if it won't be needed@>=
flush_string(name); name:=cur_name; cur_name:=0
@y
@<Flush |name| and replace it with |cur_name| if it won't be needed@>=
do_nothing
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.775] [Unique to MP] Path selector for |a_open_in| of mpx file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if not a_open_in(cur_file) then
  begin end_file_reading;
@y
if not a_open_in(cur_file,no_file_path) then
  begin end_file_reading;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.776] [Unique to MetaPost] Invoke |makempx|.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@<Try to make sure |name_of_file| refers to a valid \.{MPX} file and
  |goto not_found| if there is a problem@>=
copy_old_name(name)
{System-dependent code should be added here}
@y
@<Try to make sure |name_of_file| refers to a valid \.{MPX} file and
  |goto not_found| if there is a problem@>=
copy_old_name(name);
if not call_make_mpx(old_file_name,name_of_file) then goto not_found
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [35.781] [Unique to MP] Path selector for |a_open_in| of readfrom file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if not a_open_in(rd_file[n]) then goto not_found;
@y
if not a_open_in(rd_file[n],no_file_path) then goto not_found;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [42.1150] Fix threshold `conflict' with local variable names.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@p function threshold(@!m:integer):scaled;
var @!d:scaled; {lower bound on the smallest interval size}
begin excess:=min_cover(0)-m;
if excess<=0 then threshold:=0
else  begin repeat d:=perturbation;
  until min_cover(d+d)<=m;
  while min_cover(d)>m do d:=perturbation;
  threshold:=d;
  end;
end;
@y
@p function compute_threshold(@!m:integer):scaled;
var @!d:scaled; {lower bound on the smallest interval size}
begin excess:=min_cover(0)-m;
if excess<=0 then compute_threshold:=0
else  begin repeat d:=perturbation;
  until min_cover(d+d)<=m;
  while min_cover(d)>m do d:=perturbation;
  compute_threshold:=d;
  end;
end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [42.1151] Change the call to the threshold function.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
@x
begin d:=threshold(m); perturbation:=0;
@y
begin d:=compute_threshold(m); perturbation:=0;
@z


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [45.1133] Writing the tfm file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d tfm_out(#)==write(tfm_file,#) {output one byte to |tfm_file|}

@p procedure tfm_two(@!x:integer); {output two bytes to |tfm_file|}
begin tfm_out(x div 256); tfm_out(x mod 256);
end;
@#
procedure tfm_four(@!x:integer); {output four bytes to |tfm_file|}
begin if x>=0 then tfm_out(x div three_bytes)
else  begin x:=x+@'10000000000; {use two's complement for negative values}
  x:=x+@'10000000000;
  tfm_out((x div three_bytes) + 128);
  end;
x:=x mod three_bytes; tfm_out(x div unity);
x:=x mod unity; tfm_out(x div @'400);
tfm_out(x mod @'400);
end;
@#
procedure tfm_qqqq(@!x:four_quarters); {output four quarterwords to |tfm_file|}
begin tfm_out(qo(x.b0)); tfm_out(qo(x.b1)); tfm_out(qo(x.b2));
tfm_out(qo(x.b3));
end;
@y
Under {\mc UNIX}, we are using the binary input and output routines.
Hence, we redefine all the \.{TFM} input and output in terms of those
routines.

@d tfm_out(#) == b_write_byte(tfm_file, #)
@d tfm_two(#) == b_write_2_bytes(tfm_file, #)
@d tfm_four(#) == b_write_4_bytes(tfm_file, #)

@p procedure tfm_qqqq(@!x:four_quarters); {output four quarterwords to |tfm_file|}
begin tfm_out(qo(x.b0)); tfm_out(qo(x.b1)); tfm_out(qo(x.b2));
tfm_out(qo(x.b3));
end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [43.1181] [See TeX module 564] Reading tfm files. As a special case,
% whenever we open a tfm file for input, we read its first byte into
% "tfm_temp" right away.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d tfget==get(tfm_infile)
@d tfbyte==tfm_infile^
@y
@d tfget==tfm_temp:=getc(tfm_infile)
@d tfbyte==tfm_temp
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [43.1185] [See TeX module 575] We only want `eof' on the TFM file
% to be true if we previously had EOF, not if we're at EOF now.
% This is like `feof', and unlike our implementation of `eof' elsewhere.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if eof(tfm_infile) then goto bad_tfm;
@y
if feof(tfm_infile) then goto bad_tfm;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [43.1187] [See TeX module 563] Fix TFM file opening, and invoke
% an external program if the first open fails.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if cur_area="" then cur_area:=MP_font_area;
if cur_ext="" then cur_ext:=".tfm";
pack_cur_name;
if not b_open_in(tfm_infile) then goto bad_tfm;
@y
if cur_ext="" then cur_ext:=".tfm";
pack_cur_name;
if not b_open_in(tfm_infile) then begin
  if make_tex_tfm then begin
    if not b_open_in(tfm_infile) then goto bad_tfm;
  end else
    goto bad_tfm;
end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [43.1194] [Unique to MP] Path selector for |a_open_in| of ps_tab_file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
begin name_of_file:=ps_tab_name;
if a_open_in(ps_tab_file) then
@y
begin
vstrcpy (name_of_file+1, ps_tab_name); {copy the string}
name_of_file[0] := ' ';
name_of_file[strlen (ps_tab_name) + 1] := ' ';
name_length := strlen (ps_tab_name);
if a_open_in(ps_tab_file, PS_MAPFILE_PATH) then
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [43.1198] Allow blank lines and comment lines in ps_tab_file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ @<Read at most |lmax| characters from |ps_tab_file| into string |s|...@>=
str_room(lmax);
j:=lmax;
loop @+begin if eoln(ps_tab_file) then
    fatal_error("The psfont map file is bad!");
  read(ps_tab_file,c);
  if c=' ' then goto done;
@y
@ If we encounter the end of line before we have started reading
characters from |ps_tab_file|, we have found an entirely blank 
line and we skip over it.  Otherwise, we abort if the line ends 
prematurely.  If we encounter a comment character, we also skip 
over the line, since recent versions of the font map file are
allowed to include comments.

@<Read at most |lmax| characters from |ps_tab_file| into string |s|...@>=
str_room(lmax);
j:=lmax;
loop @+begin if eoln(ps_tab_file) then
    if j=lmax then begin flush_cur_string;
      goto common_ending; {skip over blank line}
      end
    else fatal_error("The psfont map file is bad!");
  read(ps_tab_file,c);
  if ((c='%')or(c='*')or(c=';')or(c='#')) then begin flush_cur_string;
    goto common_ending; {skip over comment line}
    end;
  if c=' ' then goto done;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [45.1278,1279,1281] Reading and writing of `mem_file'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d dump_wd(#)==begin mem_file^:=#; put(mem_file);@+end
@d dump_int(#)==begin mem_file^.int:=#; put(mem_file);@+end
@d dump_hh(#)==begin mem_file^.hh:=#; put(mem_file);@+end
@d dump_qqqq(#)==begin mem_file^.qqqq:=#; put(mem_file);@+end
@y
@z

@x
@d undump_wd(#)==begin get(mem_file); #:=mem_file^;@+end
@d undump_int(#)==begin get(mem_file); #:=mem_file^.int;@+end
@d undump_hh(#)==begin get(mem_file); #:=mem_file^.hh;@+end
@d undump_qqqq(#)==begin get(mem_file); #:=mem_file^.qqqq;@+end
@y
@z

@x
x:=mem_file^.int;
@y
undump_int(x);
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [45.1289] `eof' here means `have we previously encountered the
% end-of-file', not `are we at the end of the file'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
undump_int(x);@+if (x<>69073)or eof(mem_file) then goto off_base
@y
undump_int(x);@+if (x<>69073)or feof(mem_file) then goto off_base
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [45.1290] Eliminate possibly wrong word `preloaded' from mem_idents.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
print(" (preloaded mem="); print(job_name); print_char(" ");
@y
print(" (mem="); print(job_name); print_char(" ");
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [46.1294] Set internal[prologues] in troff mode.
% Also add call to exit() depending upon value of `history'.
% Also, add call to set_paths.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
t_open_out; {open the terminal for output}
@y
t_open_out; {open the terminal for output}
set_paths(MP_MEM_PATH_BIT + MP_INPUT_PATH_BIT + MP_POOL_PATH_BIT
          + MF_INPUT_PATH_BIT + TFM_FILE_PATH_BIT + PS_MAPFILE_PATH_BIT);
@z

@x
history:=spotless; {ready to go!}
@y
if troff_mode then @+internal[prologues]:=unity;
history:=spotless; {ready to go!}
@z

@x
end_of_MP: close_files_and_terminate;
final_end: ready_already:=0;
@y
close_files_and_terminate;
final_end: do_final_end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [46.1295] Print new line before termination; switch to editor if
% necessary.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
    print(log_name); print_char(".");
    end;
  end;
@y
    print(log_name); print_char(".");
    end;
  end;
print_ln;
if (edit_name_start<>0) and (interaction>batch_mode) then
    calledit(str_pool,edit_name_start,edit_name_length,edit_line);
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [47.1303-1304] Debugging.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
program below. (If |m=13|, there is an additional argument, |l|.)
@.debug \#@>

@d breakpoint=888 {place where a breakpoint is desirable}

@<Last-minute...@>=
@!debug procedure debug_help; {routine to display various things}
@y
program below. (If |m=13|, there is an additional argument, |l|.)
@.debug \#@>

Since the C version of |read| only works for |text_char| variables,
we need another routine to read integers from the terminal.

@d breakpoint=888 {place where a breakpoint is desirable}

@<Last-minute...@>=
@!debug function read_int:integer;
var @!c:text_char;
@!k:ASCII_code;
@!n:integer;
@!neg:boolean;
begin read(term_in,c);
n:=0; neg:=c=xchr['-'];
if neg then read(term_in,c);
k:=xord[c];
while (k>="0")and(k<="9") do
  begin n:=10*n+k-"0";
  read(term_in,c);
  k:=xord[c];
  end;
if neg then read_int:=-n else read_int:=n;
end;
@#
procedure debug_help; {routine to display various things}
@z

@x
  read(term_in,m);
  if m<0 then return
  else if m=0 then
    begin goto breakpoint;@\ {go to every label at least once}
    breakpoint: m:=0; @{'BREAKPOINT'@}@\
    end
  else  begin read(term_in,n);
@y
  m:=read_int;
  if m<0 then return
  else if m=0 then
    begin goto breakpoint;@\ {go to every label at least once}
    breakpoint: m:=0; @{'BREAKPOINT'@}@\
    end
  else  begin n:=read_int;
@z

@x
13: begin read(term_in,l); print_cmd_mod(n,l);
  end;
@y
13: begin l:=read_int; print_cmd_mod(n,l);
  end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [48.1305-1306] Add editor-switch variable to globals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
This section should be replaced, if necessary, by any special
modification of the program
that are necessary to make \MP\ work at a particular installation.
It is usually best to design your change file so that all changes to
previous sections preserve the section numbering; then everybody's version
will be consistent with the published program. More extensive changes,
which introduce new sections, can be inserted here; then only the index
itself will get a new section number.
@^system dependencies@>
@y
Here are the variables used to hold ``switch-to-editor'' information.
@^system dependencies@>

@<Global...@>=
@!edit_name_start: pool_pointer;
@!edit_name_length,@!edit_line,@!tfm_temp: integer;

@ The |edit_name_start| will be set to point into |str_pool| somewhere after
its beginning if \MP\ is supposed to switch to an editor on exit.

@<Set init...@>=
edit_name_start:=0;
@z
