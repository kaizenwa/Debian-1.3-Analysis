
{
  These routines are implemented in a C language file
  screen.c and this interface file is #included into
  zap.pas
}

module screen interface;

  export screen = (c_init, c_clearscreen, c_cleartoeos, c_cleartoeol,
                   c_gotoxy, c_home, c_right, c_left, c_up, c_down,
                   c_getch, c_tty_mode);

  function  c_init (How : Integer) : integer; C_language;
  procedure c_tty_mode (mode :  integer); C_language;

  procedure c_clearscreen; C_Language;
  procedure c_cleartoeos;  C_Language;
  procedure c_cleartoeol;  C_Language;
  procedure c_gotoxy (x,y:integer); C_Language;
  procedure c_home;        C_Language;

  procedure c_right(count : integer); C_Language;
  procedure c_left(count : integer);  C_Language;
  procedure c_up(count : integer);    C_Language;
  procedure c_down(count : integer);  C_Language;

  function c_getch: char;  C_Language;
end.
