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

%   Change file for the DVItype processor, for use with WEB to C
%   This file was created by John Hobby.  It is loosely based on the
%   change file for the WEB to C version of dvitype (due to Howard
%   Trickey and Pavel Curtis).

% History:
%   3/11/90  (JDH) Original version.
%   4/30/90  (JDH) Update to handle virtual fonts
%   4/16/93  (JDH) Make output go to standard output and require mpx file
%                  to be a command line argument.
%
%   1/18/95  (UV)  Update based on dvitype.ch for web2c-6.1
%   4/13/95  (UV)  Cosmetic changes for release of web2c-mp
%  10/08/95  (UV)  Bug fix: need to replace abs() with floating-point arg
%                  by fabs() because of different definition in cpascal.h
%                  as reported by Dane Dwyer <dwyer@geisel.csl.uiuc.edu>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [0] WEAVE: print changes only.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
\pageno=\contentspagenumber \advance\pageno by 1
@y
\pageno=\contentspagenumber \advance\pageno by 1
\let\maybe=\iffalse
\def\title{DVI$\,$\lowercase{to}MP changes for C}
@z
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [1] Change banner string.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d banner=='% Written by DVItoMP, Version 0.63'
@y
@d banner=='% Written by DVItoMP, Version 0.63' {more is printed later}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [3] Change filenames in program statement, etc.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@p program DVI_to_MP(@!dvi_file,@!mpx_file,@!output);
@y
@p program DVI_to_MP;
@z

@x
  begin @<Set initial values@>@/
@y
  begin 
  {read environment to find \.{TEXFONTS} and \.{VFFONTS}}
  set_paths (TFM_FILE_PATH_BIT + VF_FILE_PATH_BIT);
  @<Set initial values@>@/
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [5] Make name_length match the system constant.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@<Constants...@>=
@y
@d name_length==PATH_MAX
@<Constants...@>=
@z
@x
@!name_length=50; {a file name shouldn't be longer than this}
@y
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [7] Remove non-local goto.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
Such errors might be discovered inside of subroutines inside of subroutines,
so a procedure called |jump_out| has been introduced. This procedure, which
simply transfers control to the label |final_end| at the end of the program,
contains the only non-local |goto| statement in \.{DVItoMP}.
@^system dependencies@>

@d abort(#)==begin err_print_ln('DVItoMP abort: ',#);
    history:=fatal_error; jump_out;
    end
@d bad_dvi(#)==abort('Bad DVI file: ',#,'!')
@.Bad DVI file@>
@d warn(#)==begin err_print_ln('DVItoMP warning: ',#);
    history:=warning_given;
    end

@p procedure jump_out;
begin goto final_end;
end;
@y
Such errors might be discovered inside of subroutines inside of subroutines,
so a procedure called |jump_out| has been introduced. This procedure is
actually just a macro that calls |exit()| with non-zero exit status.
@^system dependencies@>

@d jump_out==uexit(history)
@d abort(#)==begin err_print_ln('DVItoMP abort: ',#);
    history:=fatal_error; jump_out;
    end
@d bad_dvi(#)==abort('Bad DVI file: ',#,'!')
@.Bad DVI file@>
@d warn(#)==begin err_print_ln('DVItoMP warning: ',#);
    history:=warning_given;
    end
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [11] Permissive input.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
@x
@!ASCII_code=" ".."~"; {a subrange of the integers}
@y
@!ASCII_code=0..255; {a subrange of the integers}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [12] The text_char type is used as an array index into `xord'.  The
% default type `char' produces signed integers, which are bad array
% indices in C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
@x
@d text_char == char {the data type of characters in text files}
@d first_text_char=0 {ordinal number of the smallest element of |text_char|}
@d last_text_char=127 {ordinal number of the largest element of |text_char|}
@y
@d text_char == ASCII_code {the data type of characters in text files}
@d first_text_char=0 {ordinal number of the smallest element of |text_char|}
@d last_text_char=255 {ordinal number of the largest element of |text_char|}
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [14] Fix up opening of files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ To prepare the |mpx_file| for output, we |rewrite| it.
@^system dependencies@>

@p procedure open_mpx_file; {prepares to write text on |mpx_file|}
begin rewrite(mpx_file);
end;
@y
@ To prepare the |mpx_file| for output, we |rewrite| it.
In C we retrieve the file name from the command line and use an external
|test_access| procedure to detect any problems so that we can issue
appropriate error messages.  This procedure takes the file name from
the global variable |cur_name|.  It has two other arguments that we
shall use later on to control path searching for input files.
@^system dependencies@>

@p procedure open_mpx_file; {prepares to write text on |mpx_file|}
begin
    argv (2, cur_name);
    rewrite (mpx_file, cur_name);
end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [19] Fix up opening the binary files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
specifies the file name. If |eof(f)| is true immediately after |reset(f,s)|
has acted, these routines assume that no file named |s| is accessible.
@^system dependencies@>

@p procedure open_dvi_file; {prepares to read packed bytes in |dvi_file|}
begin reset(dvi_file);
if eof(dvi_file) then abort('DVI file not found');
end;
@#
function open_tfm_file:boolean; {prepares to read packed bytes in |tfm_file|}
begin reset(tfm_file,cur_name);
return not eof(tfm_file);
end;
@#
function open_vf_file:boolean; {prepares to read packed bytes in |vf_file|}
begin reset(vf_file,cur_name);
return not eof(vf_file);
end;
@y
specifies the file name.  In C, we test the success of this operation by
first using the external |test_access| function, which also does path
searching based on the user's environment or the default path.
@^system dependencies@>

@p procedure open_dvi_file; {prepares to read packed bytes in |dvi_file|}
begin 
    argv(1, cur_name);
    reset(dvi_file, cur_name);
end;
@#
function open_tfm_file:boolean; {prepares to read packed bytes in |tfm_file|}
var @!ok:boolean;
begin ok:=test_read_access(cur_name, TFM_FILE_PATH);
if ok then reset(tfm_file, cur_name);
open_tfm_file:=ok;
end;
@#
function open_vf_file:boolean; {prepares to read packed bytes in |tfm_file|}
var @!ok:boolean;
begin ok:=test_read_access(cur_name, VF_FILE_PATH);
if ok then reset(vf_file, cur_name);
open_vf_file:=ok;
end;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [26] Make get_n_bytes routines work with 16-bit math.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
get_two_bytes:=a*256+b;
@y
get_two_bytes:=a*toint(256)+b;
@z
@x
get_three_bytes:=(a*256+b)*256+c;
@y
get_three_bytes:=(a*toint(256)+b)*256+c;
@z
@x
if a<128 then signed_trio:=(a*256+b)*256+c
else signed_trio:=((a-256)*256+b)*256+c;
@y
if a<128 then signed_trio:=(a*toint(256)+b)*256+c
else signed_trio:=((a-toint(256))*256+b)*256+c;
@z
@x
if a<128 then signed_quad:=((a*256+b)*256+c)*256+d
else signed_quad:=(((a-256)*256+b)*256+c)*256+d;
@y
if a<128 then signed_quad:=((a*toint(256)+b)*256+c)*256+d
else signed_quad:=(((a-256)*toint(256)+b)*256+c)*256+d;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [41] Fix abs() with floating-point arg.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
      begin if abs(font_scaled_size[f]-font_scaled_size[ff])
          <= font_tolerance then goto done;
      end
@y
      begin if fabs(font_scaled_size[f]-font_scaled_size[ff])
          <= font_tolerance then goto done;
      end
@z
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [43] Fix abs() with floating-point arg.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if abs(font_design_size[f]-font_design_size[ff]) > font_tolerance then
  font_warn('Inconsistent design sizes given for ')(ff)
@y
if fabs(font_design_size[f]-font_design_size[ff]) > font_tolerance then
  font_warn('Inconsistent design sizes given for ')(ff)
@z
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [46] Make 16-bit TFM calculations work.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
read_tfm_word; lh:=b2*256+b3;
read_tfm_word; font_bc[f]:=b0*256+b1; font_ec[f]:=b2*256+b3;
@y
read_tfm_word; lh:=b2*toint(256)+b3;
read_tfm_word; font_bc[f]:=b0*toint(256)+b1; font_ec[f]:=b2*toint(256)+b3;
@z
@x
    if b0<128 then tfm_check_sum:=((b0*256+b1)*256+b2)*256+b3
    else tfm_check_sum:=(((b0-256)*256+b1)*256+b2)*256+b3;
@y
    if b0<128 then tfm_check_sum:=((b0*toint(256)+b1)*256+b2)*256+b3
    else tfm_check_sum:=(((b0-256)*toint(256)+b1)*256+b2)*256+b3;
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [61] Don't set default_directory_name.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@d default_directory_name=='TeXfonts:' {change this to the correct name}
@d default_directory_name_length=9 {change this to the correct length}

@<Glob...@>=
@!default_directory:packed array[1..default_directory_name_length] of char;
@y
Actually, under {\mc UNIX} the standard area is defined in an external
file \.{site.h}.  And the users have a path searched for fonts,
by setting the \.{TEXFONTS} environment variable.
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [62] Remove initialization of default_directory.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ @<Set init...@>=
default_directory:=default_directory_name;
@y
@ (No initialization needs to be done.  Keep this module to preserve
numbering.)
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [63] Fix addition of ".vf" suffix for portability and keep lowercase.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@ The string |cur_name| is supposed to be set to the external name of the
\.{VF} file for the current font. This usually means that we need to
prepend the name of the default directory, and
to append the suffix `\.{.VF}'. Furthermore, we change lower case letters
to upper case, since |cur_name| is a \PASCAL\ string.
@y
@ The string |cur_name| is supposed to be set to the external name of the
\.{VF} file for the current font. This usually means that we need to,
at most sites, append the
suffix \.{.vf}.
@z

@x
if area_length[f]=0 then
  begin for k:=1 to default_directory_name_length do
    cur_name[k]:=default_directory[k];
  l:=default_directory_name_length;
  end
else l:=0;
@y
l:=0;
@z

@x
  if (names[k]>="a")and(names[k]<="z") then
      cur_name[l]:=xchr[names[k]-@'40]
  else cur_name[l]:=xchr[names[k]];
  end;
cur_name[l+1]:='.'; cur_name[l+2]:='V'; cur_name[l+3]:='F'
@y
  cur_name[l]:=xchr[names[k]];
  end;
cur_name[l+1]:='.'; cur_name[l+2]:='v'; cur_name[l+3]:='f'
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [63] Fix the changing of ".vf" to ".tfm" analogously.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
if l=0 then l:=default_directory_name_length;
@y
@z

@x
cur_name[l+2]:='T'; cur_name[l+3]:='F'; cur_name[l+4]:='M'
@y
cur_name[l+2]:='t'; cur_name[l+3]:='f'; cur_name[l+4]:='m'
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [78] Fix printing of real numbers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
  if (abs(x)>=4096.0)or(abs(y)>=4096.0)or(m>=4096.0)or(m<0) then
    begin warn('text scaled ',m:1:1,@|
        ' at (',x:1:1,',',y:1:1,') is out of range');
    end_char_string(60);
    end
  else end_char_string(40);
  print_ln(',_n',str_f:1,',',m:1:5,',',x:1:4,',',y:1:4,');');
@y
  if (fabs(x)>=4096.0)or(fabs(y)>=4096.0)or(m>=4096.0)or(m<0) then
    begin warn('text is out of range');
    end_char_string(60);
    end
  else end_char_string(40);
  print(',_n',str_f:1,',');
  fprint_real(mpx_file, m,1,5); print(',');
  fprint_real(mpx_file, x,1,4); print(',');
  fprint_real(mpx_file, y,1,4);
  print_ln(');');
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [79] Another fix for printing of real numbers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
  if (abs(xx1)>=4096.0)or(abs(yy1)>=4096.0)or@|
      (abs(xx2)>=4096.0)or(abs(yy2)>=4096.0)or(ww>=4096.0) then
    warn('hrule or vrule near (',xx1:1:1,',',yy1:1:1,') is out of range');
  print_ln('_r((',xx1:1:4,',',yy1:1:4,')..(',xx2:1:4,',',yy2:1:4,
      '), ',ww:1:4,');');
@y
  if (fabs(xx1)>=4096.0)or(fabs(yy1)>=4096.0)or@|
      (fabs(xx2)>=4096.0)or(fabs(yy2)>=4096.0)or(ww>=4096.0) then
    warn('hrule or vrule is out of range');
  print('_r((');
  fprint_real(mpx_file, xx1,1,4); print(',');
  fprint_real(mpx_file, yy1,1,4); print(')..(');
  fprint_real(mpx_file, xx2,1,4); print(',');
  fprint_real(mpx_file, yy2,1,4); print('), ');
  fprint_real(mpx_file, ww,1,4);
  print_ln(');');
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [80] Yet another fix for printing of real numbers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
print_ln('setbounds _p to (0,',dd:1:4,')--(',w:1:4,',',dd:1:4,')--');
print_ln(' (',w:1:4,',',h:1:4,')--(0,',h:1:4,')--cycle;');
@y
print('setbounds _p to (0,');
fprint_real(mpx_file, dd,1,4); print(')--(');
fprint_real(mpx_file, w,1,4);  print(',');
fprint_real(mpx_file, dd,1,4); print_ln(')--');@/
print(' (');
fprint_real(mpx_file, w,1,4);  print(',');
fprint_real(mpx_file, h,1,4);  print(')--(0,');
fprint_real(mpx_file, h,1,4);  print_ln(')--cycle;');
@z

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [96] Check usage; print newline at end of program; remove unused
% label.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@x
@p begin initialize; {get all variables initialized}
@y
@p begin initialize; {get all variables initialized}
if argc <> 3
then begin
  err_print_ln ('Usage: dvitomp <dvi file> <mpx file>.');
  jump_out;
end;
@z

@x
print_ln(banner);
@y
print (banner); 
print_ln (version_string);
@z

@x
final_end:end.
@y
if history<=cksum_trouble then uexit(0)
else uexit(history);
end.
@z
