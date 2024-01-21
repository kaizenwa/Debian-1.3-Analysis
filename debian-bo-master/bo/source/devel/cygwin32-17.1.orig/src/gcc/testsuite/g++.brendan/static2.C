class A
{
        public:
        void    member(void)
        {
        }

        static void staticMember()
        {
	  member (); // illegal, no object for calling non-static method
        }
};

main()
{
        A::staticMember();
}
