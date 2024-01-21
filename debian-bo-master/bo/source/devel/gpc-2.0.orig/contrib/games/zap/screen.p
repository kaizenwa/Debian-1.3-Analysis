
module screen interface;

  export screen = (c_clearscreen, c_cleartoeos, c_cleartoeol,
   		   c_gotoxy, c_home);

  procedure c_clearscreen; C;
  procedure c_cleartoeos; C;
  procedure c_cleartoeol; C;
  procedure c_gotoxy (x,y:integer); C;
  procedure c_home; C;

end.
