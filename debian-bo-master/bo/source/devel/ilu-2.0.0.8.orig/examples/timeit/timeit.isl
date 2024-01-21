interface timeit;

type pageimg = sequence of byte;

type pageSeq = sequence of pageimg;

type p = object
  methods
    ping1 (p1 : cardinal) : cardinal,
    ping2 (p2 : real) :real,
    ping3 (p3 : ilu.CString) : ilu.CString,

    doctest (name : ilu.CString, count : cardinal, pagesize : cardinal) : pageSeq
	"simple test case to simulate fetching a document"

  end;
