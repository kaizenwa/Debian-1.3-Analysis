#
#  utils.pl
#
#  $Id: utils.pl,v 1.8 1997/03/21 09:18:23 cg Exp $
#
#  Driver utilities, splitted from driver.pl in order to cut down on
#  its size.
#
#  © Copyright 1996, Cees de Groot
#
package main;
use subs qw(Usage);

#
#  ProcessOptions
#
#  Walk through our arguments and get our options together. This
#  results in all the $format->{option} variables having their final
#  values.
#
sub ProcessOptions ()
{
  OPTPROC: while ($ARGV[0]) 
    {
      $curarg = $ARGV[0];  
      if ($curarg =~ /^--.*/)
	{
	  #
	  #  Long option, --opt[==value]
	  #
	  $long = 1;
	}
      elsif ($curarg =~ /^-.*/)
	{
	  #
	  #  Short option, -o value
	  #
	  $long = 0;
	}
      else
	{
	  #
	  #  Filename
	  #
	  push @InFiles, $curarg;
	  next OPTPROC;
	}

      #
      #  Start looking for the option
      #
      foreach $fmt (keys %FmtList)
	{
	  for $opt (@{$Formats{$fmt}{OPTIONS}})
	    {
	      if (($long && $curarg =~ /^--$opt->{option}.*/) ||
		  $curarg =~ /^-$opt->{short}/)
		{
		  #
		  #  Found it! Get the argument and see whether all is OK
		  #  with the option.
		  #
		  $optval = "";
		  if ($long)
		   {
		     if ($curarg =~ /^--$opt->{option}=.*/)
		       {
			 $optval = $curarg;
			 $optval =~ s/.*=(.*)/$1/;
		       }
		   }
		  else
		   {
		     if ($ARGV[1] =~ /^[^-].*/)
		       {
			 $optval = $ARGV[1];
		       }
		   }
		  $opt->{type} eq "f" && do
		    {
		      #
		      #  "f" -> flag. Increment, so '-v -v' can work.
		      #
		      $Formats{$fmt}{$opt->{option}} += 1;
		      next OPTPROC;
		    };
		  #
		  #  All other types require a value (for now).
		  #
		  shift @ARGV unless $long;
		  if ($optval eq "") 
		    {
		      Usage "Option $curarg: value required";
		    }
		  ($opt->{type} eq "i" || $opt->{type} eq "s") && do
		    {
		      #
		      #  "i" -> numeric value.
		      #  "s" -> string value.
		      #
		      #  No type checking yet...
		      #
		      $Formats{$fmt}{$opt->{option}} = $optval;
		      next OPTPROC;
		    };
		  $opt->{type} eq "l" && do
		    {
		      #
		      #  "l" -> list of values.
		      #
		      for $val (@{$opt->{'values'}})
			{
			  if ($val eq $optval)
			    {
			       $Formats{$fmt}{$opt->{option}} = $optval; 
			       next OPTPROC;
			    }
			}
		      Usage "Invalid value '$optval' for '--$opt->{option}'";
		    };
		  Usage "Unknown option type $opt->{type} in $fmt/$opt";
		}
	    } 
	}
      Usage "Unknown option $curarg";
    }
  continue
    {
      shift @ARGV;
    }
}


#
#  Usage
#
#  Standard function. Uses the information in the Formats hash to print
#  out help on options. May have a string explaining why the message
#  is printed.
#
sub Usage
{
  my ($msg) = @_;

  print "SGML-Tools driver version " . `cat $LibDir/VERSION` . "\n";
  print "Usage:\n";
  print "  driver.pl <format> [options] <infile>\n";
  @helplist = keys %Formats;
  @helplist = keys %FmtList if ($theFormat);
  foreach $fmt (@helplist)
    {
      if ($fmt eq "global")
        {
	  print "General options:\n";
	}
      else
        {
          print "Format: " . $fmt . "\n";
	}
      print $Formats{$fmt}{HELP};
      for $opt (@{$Formats{$fmt}{OPTIONS}})
        {
	  my $value = '';
	  if ($opt->{type} eq "i")
	    {
	      $value = "number";
	    }
          elsif ($opt->{type} eq "l")
	    {
	      $value = "{";
	      $first = 1;
	      for $val (@{$opt->{'values'}})
	        {
		  $first || ($value .= ",");
		  $first = 0;
		  $value .= $val;
		}
	      $value .= "}";
	    }
	  elsif ($opt->{type} eq "s")
            {
	      $value = "string";
	    }
	  print "  --$opt->{option}"; print "=$value" if $value;
	  print " -$opt->{short}"; print " $value" if $value;
	  print "\n";
	}
      print "\n";
    }

  $msg && print "Error: $msg\n\n";
  exit 1;
}


#
#  cleanUp
#
#  Tidy the disk on exit. If debugging, we leave the files
#  around.
#
sub cleanUp
{
  $global->{debug} || unlink <$TmpBase.*>;
  exit 0;
}


#
#  trapSignals
#
#  Make sure we clean up on dying.
#
sub trapSignals
{
  local( $sig );
  foreach $sig ( 'HUP', 'INT', 'QUIT', 'ILL',
		  'TRAP', 'IOT', 'BUS', 'FPE',
		  'USR1', 'SEGV', 'USR2',
		  'PIPE', 'ALRM', 'TERM', )
    {
      $SIG{ $sig } = "cleanUp";
    }
}

1;
