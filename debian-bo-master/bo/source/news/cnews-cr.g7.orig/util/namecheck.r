#! /bin/sh
# test namecheck, except for white-space cases

status=0
while read name response
do
	it="`echo \"$name\" | awk -f namecheck.awk`"
	if test " $it" != " $response"
	then
		echo "$name:"
		echo "saw:	$it"
		echo "not:	$response"
		status=1
	fi
done
exit $status
