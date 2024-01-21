with Text_Io; use Text_Io;
procedure Text_IO_Example is
   type Color is (red, blue, green);
   package Int_Io is new Integer_IO (Integer); use Int_Io;
   package Flt_Io is new Float_IO (Float); use Flt_Io;
   package Color_Io is new Enumeration_Io (Color); use Color_Io;
   X : Float;
   Last : Positive;
begin
   Put_Line ("The following should be a five in base two:"); 
   Put (5, Width => 7, Base => 2);
   New_Line;
   Get ("  23.4567 ", X, Last);
   Put_Line ("The following should be 2.34567E+01");
   Put (X);
   New_Line;
   Put_Line ("The following should be RED");
   Put (red, Set => Upper_Case);
   New_Line;
end Text_IO_Example;
