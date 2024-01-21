use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Pg',
    'VERSION_FROM' => 'Pg.pm',
    'LIBS'	=> ['-L/usr/local/postgres95/lib -lpq'],
    'DEFINE'	=> '',
    'INC'	=> '-I/usr/local/postgres95/include',
);
