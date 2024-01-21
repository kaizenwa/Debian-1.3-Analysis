
#if defined DECLARE_WORDS
	#ifdef code
	#	undef code
	#endif
	#ifdef variable
	#	undef variable
	#endif
	#define code(name, cname, class)    { cname, _##name, A_PRIMITIVE | class },
	#define variable(type, name, cname) { cname, (void (*)(void)) &_##name, A_USER },
#elif defined PROTOTYPES
    #define code(name, cname, class)    void _##name(void);
	#define variable(type, name, cname) extern type _##name;
#endif

