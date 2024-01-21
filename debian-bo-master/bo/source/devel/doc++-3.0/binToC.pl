#! /sgisoft/local/bin/perl

print "static unsigned char $ARGV[0] []={\n";
$cnt=1;
$first=1;

while (<STDIN>)
{
	while (length()>0){
		$x=ord();
		substr($_,0,1)='';
		if (!$first){
		    print ",";
		}
		$first=0;
		printf " %3d",$x;
		if ($cnt%18==0){
			print"\n";
		}
		$cnt++;
	}
}

print "};";
