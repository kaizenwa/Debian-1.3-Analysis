test:
	errors=0; \
	for i in copy intfunc irr read; \
	do \
		testme=`basename $${i} .input`; \
		prog=../xspread flags=-X ./tester.sh $${testme}; \
		[ $$? -ne 0 ] && errors=`expr $${errors} + 1`; \
	done; \
	echo "$${errors} errors"
	@echo

clean:
	rm *.output
