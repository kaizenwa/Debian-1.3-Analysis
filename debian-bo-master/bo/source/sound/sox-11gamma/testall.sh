effect="$*"
t() {
	format=$1
	shift
	opts="$*"

	echo "Format: $format   Options: $opts"
	./sox monkey.voc $opts /tmp/monkey.$format $effect
	./sox $opts /tmp/monkey.$format /tmp/monkey.voc  $effect
}
t raw -r 8130 -t ub
t au 
t sf 
t aiff 
t hcom -r 22050 
t 8svx 
t sndr
t sndt 
t wav 
t smp 
t dat 
#t cdr - need separate test and a mono->stereo effect
t maud 
t wve 
