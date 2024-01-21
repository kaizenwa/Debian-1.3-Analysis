class foo {

public:

	virtual foo &operator <<(foo &(foo::*)(foo &));
};


foo &foo::operator<<(foo &(foo::*manip)(foo &))
{
 
   (this->*manip)(*this);
 
   return *this;
}
