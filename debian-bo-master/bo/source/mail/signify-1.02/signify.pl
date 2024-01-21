#! /usr/bin/perl

#
# my %section (
#	lines		= \@lines,
#	next		= \%section,
#   colflag		= $bool,
#	weight		= $real,
#	repeat		= $bool,
#	minwidth	= $bool,
#	halign		= $number,
#	valign		= $number,
# );
#


$dbg  = 0;
$prog = "Signify";
$vers = "1.02";


%vars			= %ENV;
$vars{PROGRAM}	= "$prog v$vers";
$vars{SIGWIDTH}	= 79;
$vars{WEBSITE}	= "http://www.verisim.com/";
$errorfile		= "";
$outputfifo		= "";
$inputfile		= "$ENV{HOME}/.signify";


sub Error {
	die "$errorfile:$.:$_[0]\n";
}



sub SubstituteVars {
	my($line) = @_;
	$line =~ s/\$(\w+)/$vars{$1}/gs;
	return $line;
}



sub DefaultSection {
	my %section;
	my @lines;

	$section{lines}		= \@lines;
	$section{weight}	= 1;
	$section{colflag}	= 0;
	$section{repeat}	= 0;
	$section{minwidth}	= 0;
	$section{halign}	=-1;
	$section{valign}	=-1;

	return %section;
}



sub ReadCommand {
	my($line) = @_;
	if ($line =~ m/^\%\s*\$(\w+)=(.*)$/) {
		$vars{$1} = SubstituteVars($2);
		return 1;
	}
	return;
}



sub ReadSection {
	my($path,$line) = @_;
	my %section = DefaultSection();
	my $secttype;

	Error "Unknown section identifier" unless $line=~m/^\%\s*[\{\(\|]/;
	($secttype) = ($line =~ m/^\%\s*(.)/);
	$section{colflag} = ($secttype eq "(");

	$line =~ s/^\%\s*[\{\(\|]\s*//;
	foreach (split(/,\s*/,$line)) {
		if (/^bottom$/i)		{ $section{valign}	= -2; next; }
		if (/^center$/i)		{ $section{halign}	=  0; next; }
		if (/^center=(\d+)$/i)	{ $section{halign}	= $1; next; }
		if (/^left$/i)			{ $section{halign}	= -1; next; }
		if (/^minwidth$/i)		{ $section{minwidth}=  1; next; }
		if (/^repeat$/i)		{ $section{repeat}	=  1; next; }
		if (/^right$/i)			{ $section{halign}	= -2; next; }
		if (/^top$/i)			{ $section{valign}	= -1; next; }
		if (/^vcenter$/i)		{ $section{valign}	=  0; next; }
		if (/^weight=(.*)$/i)	{ $section{weight}	= $1; next; }

		Error "Unknown option '$_'";
	}

	while (1) {
		$line = <$path>;
		Error "Unexpected end-of-file" unless $line;
		chomp $line;
		print STDERR "Reading (file) line: $line\n" if $dbg;
		next if !$line || $line =~ m/^\#/;

		if ($line =~ m/^\%/) {
			return \%section if $line =~ m/^\%\s*\}/ && !$section{colflag};
			return \%section if $line =~ m/^\%\s*\)/ &&  $section{colflag};
#			print STDERR "colflag=$section{colflag}\n" if $dbg;
			Error "Incorrect section terminator" if $line =~ m/^\%\s*[\}\)]/;
#			Error "Cannot nest columns within another section" if $line =~ m/^\%\s*\(/;
#			Error "Cannot nest 'alternate' sections" if $line =~ m/^\%\s*\{/ && !$section{colflag};

			if ($line =~ m/^\%\s*[\{\(]/) {
				push @{$section{lines}},ReadSection($path,$line);
				next;
			}
			if ($line =~ m/^\%\s*\|/) {
				$line =~ s/\|/$secttype/;
				$section{next} = ReadSection($path,$line);
				return \%section;
			}

			next if ReadCommand($line);

			Error "Unknown command";
		} else {
			push @{$section{lines}},SubstituteVars($line);
		}
	}
}



sub ReadFile {
	my($file) = @_;
	my $section = DefaultSection();

	$section{halign} = -1;
	$section{valign} = -1;

	$errorfile=$file;
	open(FILE,"<$file") || die "Error: Could not read file '$file' -- $!\n";

	while (<FILE>) {
		chomp;
		next if !$_ || /^\#/;
		print STDERR "Reading (file) line: $_\n" if $dbg;

		if (/^\%\s*[\{\(]/) {
			push @{$section{lines}},ReadSection(FILE,$_);
			print STDERR "finished reading section\n" if $dbg;
		} elsif (/^\%/) {
			Error "Unknown command" unless ReadCommand($_);
		} else {
			print STDERR "Adding (base) line '$_'\n" if $dbg;
			push @{$section{lines}},SubstituteVars($_);
		}
	}

	close FILE;

	return \%section;
}



sub GenerateSig {
	my($sect) = @_;
	my(@lines,$line,$i);

	if ($$sect{colflag}) {

		my @cols;
		my $csect    = $sect;
		my $minwidth = 0;
		my $maxheight= 0;
		my $padcols  = 0;
		my $lastpad  = 0;

		while ($csect) {
			my @clines;
			my $count = 0;
			my $width = 0;
			push @cols,\@clines;

			foreach (@{$$csect{lines}}) {
				if (ref) {
					print STDERR "creference\n" if $dbg;
					push @clines,GenerateSig($_);
				} else {
					print STDERR "cline '$_'\n" if $dbg;
					push @clines,$_;
				}
			}
			foreach (@clines) {
				my $length = length;
				$count++;
				$width = $length if $length > $width;
			}

			$minwidth += $width;
			$maxheight = $count if $count > $maxheight;
			if ($$csect{minwidth}) {
				$lastpad = 0;
			} else {
				$padcols += 2 - $lastpad;
				$lastpad  = 1;
			}

			$csect = $$csect{next};
			print STDERR "minwidth=$minwidth, width=$width, maxheight=$maxheight, count=$count, varcols=$varcols\n" if $dbg;
		}

		my $padding = $vars{SIGWIDTH} - $minwidth;
		$csect = $sect;
		$lastpad = 0;
		foreach $col (@cols) {
			my $width  = 0;

			if (!$$csect{minwidth}) {
				my $spaces = 0;
				$spaces = int($padding / $padcols) if $padcols && !$lastpad;
				for ($i=0; $i < $maxheight; $i++) {
					$lines[$i] .= " " x $spaces;
				}
				$padding -= $spaces;
				$padcols--;
			}

			foreach (@$col) {
				my $length = length;
				$width = $length if $length > $width;
			}
			if ($$csect{halign} == -1) {
				foreach (@$col) {
					my $length = length;
					$_ = $_ . " " x ($width - $length);
				}
			}
			if ($$csect{halign} == -2) {
				foreach (@$col) {
					my $length = length;
					$_ = " " x ($width - $length) . $_;
				}
			}
			if ($$csect{halign} >= 0) {
				foreach (@$col) {
					my $length = length;
					$_ = " " x (($width - $length + 1) / 2) . $_ . " " x (($width - $length) / 2);
				}
			}

			$i = 0;
			if ($$csect{valign} == -2 && !$$csect{repeat}) {
				my $skip = int($maxheight - scalar(@$col));
				while ($i < $skip) {
					$lines[$i++] .= " " x $width;
				}
			}
			if ($$csect{valign} >= 0 && !$$csect{repeat}) {
				my $skip = int(($maxheight - scalar(@$col)) / 2);
				while ($i < $skip) {
					$lines[$i++] .= " " x $width;
				}
			}
			foreach $line (@$col) {
				$lines[$i++] .= $line;
			}
			while ($i < $maxheight && $$csect{repeat}) {
				foreach $line (@$col) {
					last if $i >= $maxheight;
					$lines[$i++] .= $line;
				}
			}
			while ($i < $maxheight) {
				$lines[$i++] .= " " x $width;
			}

			if (!$$csect{minwidth}) {
				my $spaces = 0;
				$spaces = int($padding / $padcols) if $padcols;
				for ($i=0; $i < $maxheight; $i++) {
					$lines[$i] .= " " x $spaces;
				}
				$padding -= $spaces;
				$padcols--;
				$lastpad = 1;
			} else {
				$lastpad = 0;
			}

			$csect = $$csect{next};
		}

	} else { # !colflag

		if ($$sect{next}) {
			my $total = 0.0;
			my $sptr  = $sect;

			while ($sptr) {
				$total += $$sptr{weight};
				$sptr   = $$sptr{next};
			}

			my $index = rand() * $total;

			while ($$sect{weight} < $index) {
				$index -= $$sect{weight};
				$sect   = $$sect{next};
			}
		}

		foreach (@{$$sect{lines}}) {
			if (ref) {
				print STDERR "reference\n" if $dbg;
				push @lines,GenerateSig($_);
			} else {
				print STDERR "line '$_'\n" if $dbg;
				push @lines,$_;
			}
		}

		if ($$sect{halign} >= 0) {
			my $center = $$sect{halign};
			$center = ($vars{SIGWIDTH}+1)/2 unless $center;

			for ($i=0; $i < @lines; $i++) {
				my $length = length($lines[$i]);
				$lines[$i] = " " x ($center - $length/2) . $lines[$i];
			}
		}
		if ($$sect{halign} == -2) {
			for ($i=0; $i < @lines; $i++) {
				my $length = length($lines[$i]);
				$lines[$i] = " " x ($vars{SIGWIDTH} - $length) . $lines[$i];
			}
		}

	} # colflag

	return @lines;
}



sub MakeFifo {
	my ($fifo) = @_;

	die "Error: '$fifo' already exists and is not a FIFO\n" if (-e $fifo && ! -p $fifo);
	system("mkfifo -m 644 $fifo") if (! -p $fifo);
	die "Error: Could not make FIFO '$fifo'\n" if (! -p $fifo);
}



sub LockFifo {
	my($fifo) = @_;
	my $lock = "$fifo.lock";

	if (open(LOCK,"<$lock")) {
		my $pid = <LOCK>;
		chomp $pid;
		kill 1,$pid;
		close(LOCK);
	}

	open(LOCK,">$lock") || die "Error: Could not write file '$lock' -- $!\n";
	print LOCK $$;
	close(LOCK);
}



###############################################################################



foreach (@ARGV) {
	if (/^--fifo=(.*)$/) {
		$outputfifo = $1;
		MakeFifo($outputfifo);
		next;
	}
	if (/^--input=(.*)$/) {
		$inputfile = $1;
		next;
	}

	die "Error: Unknown parameter '$_'\n";
}


$sectref = ReadFile($inputfile);
$nowtime = 840000000;


if ($outputfifo) {
	LockFifo($outputfifo);
	while (1) {
		srand(time() ^ $$);
		utime $nowtime,$nowtime,$outputfifo;
		open(FIFO,">$outputfifo") || die "Error: Could not write to '$outputfifo' -- $!\n";
		@lines = GenerateSig($sectref);
		foreach (@lines) {
			s/\s+$//;
			print FIFO $_,"\n";
		}
		close(FIFO);
		utime $nowtime,$nowtime,$outputfifo;
		sleep(1);
	}
} else {
	srand(time() ^ $$);
	@lines = GenerateSig($sectref);
	foreach (@lines) {
		s/\s+$//;
		print $_,"\n";
	}
}
