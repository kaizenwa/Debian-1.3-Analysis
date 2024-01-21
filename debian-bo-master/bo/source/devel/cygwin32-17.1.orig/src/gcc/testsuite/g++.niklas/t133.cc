struct A { struct B { void operator = (const B&); }; };
void A::B::operator = (const B&) {}
