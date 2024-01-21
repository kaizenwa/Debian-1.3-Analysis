/*$Id: c_comand.h,v 11.22 96/02/18 11:45:24 al Exp $ -*- C++ -*-
 * all of the commands
 */
#ifndef C_COMAND_H
#define C_COMAND_H
/*--------------------------------------------------------------------------*/
class CS;
/*--------------------------------------------------------------------------*/
class CMD {
public:
  static  void  cmdproc(const char*);
private:
  static  void	ac(CS&);
  static  void	alarm(CS&);
  static  void	alter(CS&);
  static  void	build(CS&);
  static  void	chdir(CS&);
  static  void	clear(CS&);
  static  void	comment(CS&);
  static  void	crtset(CS&);
  static  void	dc(CS&);
  static  void	del(CS&);
  static  void	disto(CS&);
  static  void	edit(CS&);
  static  void	end(CS&);
  static  void	ends(CS&);
  static  void	fanout(CS&);
  static  void	fault(CS&);
  static  void	file(CS&);
  static  void	fourier(CS&);
  static  void	generator(CS&);
  static  void	get(CS&);
  static  void	help(CS&);
  static  void	ic(CS&);
  static  void	insert(CS&);
  static  void	list(CS&);
  static  void	logger(CS&);
  static  void	mark(CS&);
  static  void	merge(CS&);
  static  void	model(CS&);
  static  void	modify(CS&);
  static  void	nodeset(CS&);
  static  void	noise(CS&);
  static  void	op(CS&);
  static  void	options(CS&);
  static  void	pause(CS&);
  static  void	plot(CS&);
  static  void	print(CS&);
  static  void	quit(CS&);
  static  void	restore(CS&);
  static  void	run(CS&);
  static  void	save(CS&);
  static  void	sens(CS&);
  static  void	status(CS&);
  static  void	subckt(CS&);
  static  void	sweep(CS&);
  static  void	system(CS&);
  static  void	temp(CS&);
  static  void	tf(CS&);
  static  void	title(CS&);
  static  void	tr(CS&);
  static  void	unfault(CS&);
  static  void	unmark(CS&);
public:
  static int count;		/* command counter, for history */
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
