#
#  lang.pm
#
#  $Id: Lang.pm,v 1.1 1997/03/18 16:40:31 cg Exp $
#
#  Language support.
#
#  © Copyright 1997, Cees de Groot
#

package Lang;

###########
#
#  Part one: language names
#
@Languages = qw(
  en english english
  de deutsch german
  nl nederlands dutch
  fr français french
);


#
#  Any2ISO
#
#  Given a member of @Languages, return the ISO name. This is what we
#  use most of the time.
#
sub Any2ISO
{
  my $lang = shift (@_);
  
  $i = 0;
  foreach $l (@Languages)
    {
      ($l eq $lang) && last;
      $i++;
    }
  return $Languages[(int $i / 3) * 3];
}


#
#  ISO2Native
#
#  Given an ISO, give the Native name.
#
sub ISO2Native
{
  my $iso = shift (@_);

  $i = 0;
  foreach $l (@Languages)
    {
      ($l eq $iso) && last;
      $i++;
    }
  print "i = $i; ";
  return $Languages[$i + 1];

}


#
#  ISO2English
#
#  Given an ISO, give the English name.
#
sub ISO2English
{
  my $iso = shift (@_);

  $i = 0;
  foreach $l (@Languages)
    {
      ($l eq $iso) && last;
      $i++;
    }
  return $Languages[$i + 2];
}

##########
#
#  Part two: translations
#
sub Xlat
{
  my ($txt) = @_;

  return $txt if ($language eq "en");
  return $translations->{$txt}{$language};
};

$translations = {
  "string one" => {
     "nl" => "string een",
     "de" => "zeichenreihenfolge eins"
  },
  "string two" => {
     "nl" => "string twee",
     "de" => "zwei"
  }
};


1;
