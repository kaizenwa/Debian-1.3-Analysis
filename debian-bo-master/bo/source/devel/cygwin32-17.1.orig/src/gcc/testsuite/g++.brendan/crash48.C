class internal {
	int field;
	int anotherfield;
};

class bug {
	internal* numbers;
	bug(int size);
};

bug::bug(int size)
{
	numbers = new internal(size * size);
}

main()
{
	bug test;
}
