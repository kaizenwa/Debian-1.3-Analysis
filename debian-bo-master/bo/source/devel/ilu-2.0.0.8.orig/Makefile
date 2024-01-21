all:: Build

BootstrapMakefile: ./imake/configure
	@echo '******************************************************************************'
	@echo '***  Configuring with defaults.                                            ***'
	@echo '***  If you need something different, interrupt this process and           ***'
	@echo '***   type "./configure --help" to see other options.                      ***'
	@echo '***  Or read the top level README file.                                    ***'
	@echo '******************************************************************************'
	cd ./imake ; ./configure

ConfiguredMakefile:  BootstrapMakefile ./imake/ilu.defs.new ./imake/ilu.tmpl ./imake/ilu.rules
	make -f BootstrapMakefile ConfiguredMakefile

.makefiles: ConfiguredMakefile
	make -f ConfiguredMakefile Makefiles
	touch .makefiles

Build: .makefiles
	make -f ConfiguredMakefile

Install: Build
	make -f ConfiguredMakefile Install

Clean::
	make -f ConfiguredMakefile Clean

Dist:: .makefiles
	make -f ConfiguredMakefile Dist RELNUM=$(RELNUM)
