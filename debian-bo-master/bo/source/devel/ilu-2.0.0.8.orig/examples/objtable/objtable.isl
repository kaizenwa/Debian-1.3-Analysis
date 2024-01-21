interface objtable;

type filename = ilu.CString;

type file = object
  methods
    name () : filename
  end;

exception not-found : filename;

type server = object
  methods
    find-file (name : filename) : file raises not-found end
  end;
