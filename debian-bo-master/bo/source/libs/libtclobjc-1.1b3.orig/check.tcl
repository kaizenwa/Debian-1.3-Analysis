set mylist [[List alloc] init]
$mylist name
set myobject [[Object alloc] init]
$mylist count
$mylist addObject: $myobject
$mylist count
$mylist addObject: $myobject
$mylist count
$mylist addObjectIfAbsent: $myobject
$mylist count
