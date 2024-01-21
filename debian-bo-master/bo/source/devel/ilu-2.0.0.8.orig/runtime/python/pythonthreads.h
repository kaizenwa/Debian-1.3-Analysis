/* $Id: pythonthreads.h,v 1.5 1996/07/11 22:06:42 janssen Exp $ */

#ifdef ILU_PYTHON_THREADS
extern ilu_boolean ilupython_threaded_operation;
#define EXIT_INTERPRETER(if_what) { PyObject *_save; { if (if_what) { _save = PyEval_SaveThread();  } }
#define ENTER_INTERPRETER(if_what) { if (if_what) { PyEval_RestoreThread(_save); } } }
#define CALL_KERNEL(unblock_cond, call) { EXIT_INTERPRETER(unblock_cond); call; ENTER_INTERPRETER(unblock_cond); }
#define ILUPY_ILLEGAL_IN_THREADED(unless_what) if (ilupython_threaded_operation && !unless_what) { char buf[1000]; sprintf(buf, "illegal internal call in threaded runtime: %s", ## unless_what); Py_FatalError(buf); }
#define ILUPY_ILLEGAL_IN_UNTHREADED if (!ilupython_threaded_operation) _ilu_Assert(0, "illegal internal call in single-threaded runtime"); 
#else
#define ilupython_threaded_operation ilu_FALSE
#define CALL_KERNEL(unblock_cond, call) call
#define EXIT_INTERPRETER(if_what)
#define ENTER_INTERPRETER(if_what)
#define ILUPY_ILLEGAL_IN_THREADED(unless_what)
#define ILUPY_ILLEGAL_IN_UNTHREADED _ilu_Assert(0, "illegal internal call in single-threaded runtime");
#endif /* ILU_PYTHON_THREADS */

#define NEW_THREAD_ENTER { { if (ilupython_threaded_operation) PyEval_RestoreThread(NULL); }
#define FINISHED_THREAD_EXIT { if (ilupython_threaded_operation) { if (PyEval_SaveThread() != NULL) { fprintf(stderr, "unexpected non-NULL stack from exiting thread\n"); } } } }
