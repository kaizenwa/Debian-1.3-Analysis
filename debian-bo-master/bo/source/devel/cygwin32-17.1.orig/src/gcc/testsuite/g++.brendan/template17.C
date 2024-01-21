struct Regex { int i; Regex(char *, int); };
template<class KEY>
class NDAMName {
public:
    static const Regex pattern;
    static unsigned sequence_number;
};

const Regex NDAMName<'L'>::pattern("^[Ll](.*)$",   1);
unsigned NDAMName<'L'>::sequence_number = 0;
