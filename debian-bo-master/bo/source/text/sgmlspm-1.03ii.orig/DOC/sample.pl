#!/usr/bin/perl

use SGMLS;

$this_parse = new SGMLS(STDIN); # Read from standard input.

while ($this_event = $this_parse->next_event) {
    my $type = $this_event->type;
    my $data = $this_event->data;
  SWITCH: {
      $type eq 'start_element' && do {
          print "Beginning element: " . $data->name . "\n";
          last SWITCH;
      };
      $type eq 'end_element' && do {
          print "Ending element: " . $data->name . "\n";
          last SWITCH;
      };
      $type eq 'cdata' && do {
          print "Character data: " . $data . "\n";
          last SWITCH;
      };
      $type eq 'sdata' && do {
          print "Special data: " . $data . "\n";
          last SWITCH;
      };
      $type eq 're' && do {
          print "Record End\n";
          last SWITCH;
      };
      $type eq 'pi' && do {
          print "Processing Instruction: " . $data . "\n";
          last SWITCH;
      };
      $type eq 'entity' && do {
          print "External Data Entity: " . $data->name .
              " with notation " . $data->notation->name . "\n";
          last SWITCH;
      };
      $type eq 'start_subdoc' && do {
          print "Beginning Subdocument Entity: " . $data->name . "\n";
          last SWITCH;
      };
      $type eq 'end_subdoc' && do {
          print "Ending Subdocument Entity: " . $data->name . "\n";
          last SWITCH;
      };
      $type eq 'conforming' && do {
          print "This is a conforming SGML document\n";
          last SWITCH;
      };
  }
}
