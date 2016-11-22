
proSQLite: a Prolog interface to the SQLite database system.
--------------------


This is an interface of SQLite for Prolog systems.
The library was developed on SWI-Prolog under linux.
It should compile easily on linux or MACs and it comes packaged with Windows binaries.
It is also likely that it compiles easily on YAP Prolog.

On SWI-Prolog is higly recommended that you install from with the engine via 

   ?- pack_install( prosqlite ).

This takes care of everything and you can then load the library via

   ?- [library(prosqlite)].

If you compile SWI from sources, rebuilt the package with
   ?- pack_rebuild(prosqlite).

If you have problems compiling from sources, edit buildenv.sh to fit your system
and in a bourne-compatible shell do: 

$ source env/buildenv.sh
$ make

The main publication describing to this library is :
   
	ProSQLite: Prolog file based databases via an SQLite interface
	Canisius Sander, Nicos Angelopoulos and Lodewyk Wessels
	Proc. of Practical Aspects of Declarative Languages (PADL 2013).
	LNCS vol. 7752, p. 222-227 .January, 2013. Rome, Italy).
	Doi [10.1007/978-3-642-45284-0_15]


Library web-page:
   http://stoics.org.uk/~nicos/sware/prosqlite

---------



Nicos Angelopoulos, October 2016,
(1.0@November 2013. 0.1@August 2012)

---
http://stoics.org.uk/~nicos/
