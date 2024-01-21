struct base
{
    protected:
        void base_func() {}
};

struct derived : public base
{
    protected:
        void derived_func(base *ptr) { ptr->base_func(); }
};

