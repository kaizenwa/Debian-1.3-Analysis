class Y {
public:
   friend Y operator++ (Y&);
   friend Y operator++ (Y&, char);	// illegal
};
