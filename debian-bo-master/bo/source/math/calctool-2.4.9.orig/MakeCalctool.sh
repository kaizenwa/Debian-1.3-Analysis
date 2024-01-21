echo
echo "clean old versions...."
echo
make clean
echo
echo "creating the 'lib'...."
echo
cp Imakefile.lib Imakefile
xmkmf -a
make || exit 1
mv ./libCalctool.a ./lib
echo
echo "the 'lib' is ready!!"
echo
echo "creating now the 'main' calctool...."
echo
cp Imakefile.main Imakefile
xmkmf
make || exit 1
echo
echo "calctool is ready!!"
exit 0
