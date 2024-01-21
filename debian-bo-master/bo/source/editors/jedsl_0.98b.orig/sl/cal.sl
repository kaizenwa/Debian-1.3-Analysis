%
%  calender for JED
%
%  It was written to test a mixture of S-Lang RPN and infix notation.
%
%  It pops up a buffer like:

%     Jun 1993		      Jul 1993		       Aug 1993
% S  M Tu  W Th  F  S	  S  M Tu  W Th  F  S	   S  M Tu  W Th  F  S
%       1  2  3  4  5 		      1  *  3 	   1  2  3  4  5  6  7 
% 6  7  8  9 10 11 12 	  4  5  6  7  8  9 10 	   8  9 10 11 12 13 14 
%13 14 15 16 17 18 19 	 11 12 13 14 15 16 17 	  15 16 17 18 19 20 21 
%20 21 22 23 24 25 26 	 18 19 20 21 22 23 24 	  22 23 24 25 26 27 28 
%27 28 29 30 		 25 26 27 28 29 30 31 	  29 30 31 
%
%  The asterisk denotes the current day.  
%  The actual computational part of the code presented here is a 
%  translation of cal.el included with the GNU Emacs distribution.
%  (suitably modified to work with 16 bit integers)
%----------------------------------------------------------------------



% convert betewwn numeric month and string
define cal_convert_month(month, type)
{
   variable lis, m, mnth;
   lis =  "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec";
   
   % zero type => convert numeric month to string
   if (type == 0) return (extract_element(lis, month - 1, ' '));
   
   for (m = 0; m < 12; ++m)
     { 
	mnth = extract_element(lis, m, ' ');
	!if (strcmp(month, mnth)) return (m + 1);
     }
}


% return nonzero if yearnum is a leap year
define cal_leap_year_p (year)
{
   ((not(year mod 4) and (year mod 100))
    or (not (year mod 400)));
}

% calculate nth day of year for given date
define cal_day_number(month, day, year)
{
   variable d;
   d = 31 * ( month - 1 ) + day;
   if (month > 2)
     {
	d = d - (month * 4 + 23) / 10;
	if (cal_leap_year_p (year)) d++;
     } 
   d;
}

% calculate day of week for given date
define cal_day_of_week(month, day, year)
{
   variable c, delta, n, a, b;
   
   n = cal_day_number(month, day, year);
   --year;
   
  a = n + year + year/4;
  c = year/100 * 3; b = 0;
  if (c mod 4) b = 1;

   return (a - (b + c/4)) mod 7;
}

variable Cal_CurDayNum, Cal_CurMonthNum, Cal_CurYearNum, Cal_CurDayVisible;


define cal_make_month (month, year, indent, day, highlight)
{
   variable first, nm, ny, max, i, istr, m;

   m = cal_convert_month (month, 1);
   
   first = cal_day_of_week(m, 1, year);
   nm = m + 1; ny = year;
   if (nm == 13) max = 31;
   else max = cal_day_number(nm, 1, ny) - cal_day_number(m, 1, year);
   
   ++indent;
   
   bob; goto_column(indent);
   insert("     "); insert(month); insert_single_space; insert(string(year));
   !if (down_1 ()) newline();
   goto_column(indent);
   
   insert(" S  M Tu  W Th  F  S");
   !if (down_1) newline ();
   goto_column(first * 3 + indent);
   
   for (i = 1; i <= max; ++i)
     { 
	if (first == 7)
	  {
	     !if (down_1 ()) {eol; newline}
	     goto_column(indent); first = 0;
	  }
	
	if ((day == i) and highlight)
	  {
	     if (day < 10) insert (" * ");
	     else insert ("** ");
	  }
	else vinsert ("%2d ", i, 1);
	++first;
     } 
}

%%% strcaps-- returns capitalized string
define strcaps(str)
{
   str = strlow(str);
   strsub(str, 1, toupper (str[0]));
}

    
define calendar ()
{
   
   variable month, year, t, m, nlines, wlines, obuf, default, n;
   variable this_day, this_month, this_year;

   n = 0;
   obuf = whatbuf;
   t = time;
   this_month = extract_element(t, 1, ' ');
   this_day = extract_element(t, 2, ' ');

   % Some systems display the time as: Tue Jul 06 16:31:18 1993
   % while others use Tue Jul 06 16:31:18 1993
   % this silly bit is a result.
   
   if (strlen(this_day) == 0)
     { 
	this_day = extract_element(t, 3, ' ');
	n = 1
     } 
   this_year = extract_element(t, 4 + n, ' ');
   
   default = Sprintf ("%s %s", this_month, this_year, 2);
   
   t = read_mini("Month Year:", default, Null_String);
   t = strtrim(t);
   
   month = strcaps(substr(extract_element(t, 0, ' '), 1, 3));
   year = integer(extract_element(t, 1, ' '));
   m = cal_convert_month(month, 1);
   
   this_month = cal_convert_month (this_month, 1);
   this_year = integer (this_year);
   this_day = integer(this_day);

   pop2buf("*calendar*"); set_readonly(0); erase_buffer();
   
   --m; if (0 == m) {m = 12; --year}
   cal_make_month (cal_convert_month(m, 0), 
		   year, 0, this_day,
		   ((m == this_month) and (year == this_year)));
   
   ++m; if (m == 13) {m = 1; ++year}
   cal_make_month (cal_convert_month(m, 0), 
		   year, 25, this_day,
		   ((m == this_month) and (year == this_year)));
   
   ++m;  if (m == 13) {m = 1; ++year} 
   cal_make_month (cal_convert_month(m, 0), 
		   year, 50, this_day,
		   ((m == this_month) and (year == this_year)));
   
   %
   % fix window size
   %
   if (nwindows == 2)
     {
	eob();  bskip_chars("\n\t ");
	nlines = what_line - window_info('r');
	
	if (nlines > 0)
	  {
	     loop (nlines) {call("enlarge_window") }
	  }
	else
	  {
	     call("other_window");
	     loop(- nlines) {call("enlarge_window")}
	     call("other_window");
	  } 
	 bob();
     } 
     
   set_readonly(1); set_buffer_modified_flag(0);
   bob(); pop2buf(obuf);
   %
   %  what the heck, give current time
   %
   message(time)
}
