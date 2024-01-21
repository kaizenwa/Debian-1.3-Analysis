char string[100] = "Hello World! This is a test string for you";

main()
{
 bcopy(string, string + 5, 20);
 bcopy(string + 6, string, 20);
}
