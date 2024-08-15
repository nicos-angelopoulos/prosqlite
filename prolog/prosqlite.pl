:- module( prosqlite,
          [ sqlite_connect/2,           % +FileName, -Conn
            sqlite_connect/3,           % +FileName, -Conn, +Opts
            sqlite_disconnect/1,        % +Conn
            sqlite_disconnect/2,        % +Conn, +Opts
            sqlite_current_connection/1,% -Conn
            sqlite_query/2,             % +SQL, -Row
            sqlite_query/3,             % +Conn, +SQL, -Row
            sqlite_format_query/3,      % +Conn, +SQL, -Row
            sqlite_current_table/2,     % +Conn, -Table
            sqlite_current_table/3,     % +Conn, ?Table, -Facet
            sqlite_table_column/3,      % +Conn, ?Table, ?Column
            sqlite_table_column/4,      % +Conn, ?Table, ?Column, -Facet
            sqlite_table_count/3,       % +Conn, +Table, -Count
            sqlite_default_connection/1,% -Conn
            sqlite_date_sql_atom/2,     % ?Date, ?SqlAtom
            sqlite_pragma/3,            % ?Date, ?SqlAtom
            sqlite_version/2,           % -Version, -Date
            sqlite_binary_version/2,    % -Version, -Date
            sqlite_library_version/1,   % -Version
            sqlite_library_version/2,   % +Alias, -Version
            sqlite_library_c_version/1, % -Version
            sqlite_build_version/1,     % -Version
            sqlite_citation/2           % -Atom, Bibterm
          ] ).

:- use_module(library(lists)).  % append/3, member/2, memberchk/2.
:- use_module(library(apply)).  % maplist/2,3.
:- use_module(library(debug)).

:- at_halt(sqlite_disconnect).

:- use_module(library(shlib)).
:- use_foreign_library(foreign(prosqlite)).

:- dynamic(sqlite_connection/3).
:- dynamic(sqlite_db:sqlite_asserted/4).

/** <module>  proSQLite: a Prolog interface to the SQLite database system.

This library follows the design and borrows code from the ODBC library of SWI-Prolog 
http://www.swi-prolog.org/pldoc/packasqlite_connectge/odbc.html .

The SQLite system is a powerful zero-configuration management systme that interacts
with single-file databases that are cross-platform compatible binaries.

ProSQLite provides three layers of interaction with SQLite databases.
At the lower level is the querying via SQL statements. A second layer 
allows the interogation of the database dictionary, and the final level
facilitates the viewing of database tables as predicates.
See the publication pointed to by sqlite_citation/2, for further details.
If you use prosqlite in your research, please consider citing this publication.

The library has been developed and tested on SWI 6.3.2 and latest version was tested on 9.3.9.

The easiest way to install on SWI is via the package manager. 
Simply do:
==
     ?- pack_install(prosqlite).
==
You will also need the sqlite3{.so,.dll} in your path. If you have difficulties in MS platforms 
try dropping sqlite3.dll in installed prosqlite/lib/x86_64/ directory (or prosqlite/lib/i386-win32 if you are on 32 bits).
Alternative place it in SWI's bin/ directory.

There are good/full examples in the sources, directory examples/.
For instance test by :
==
     ?- [predicated].
     ?- predicated.
==

As of v2.0 errors and null values are better supported:
==
% assuming db is open in SQLiteBrowser, with unsaved changes:

?- sqlite_query(db1, "update ex_t set exid=99 where exf3='v13';", AffC ).

ERROR: Unhandled exception: SQLite code: 5, with short message: database is locked


?- ex_t(A,B,C), write(A:B:C), nl, fail.
1:v12:v13
2: : $null$
3: $null$ : w
false.

?- ex_t(A,B,'$null$')
|    .
exf3- $null$
A = 2,
B = '' ;
==

There is a sister package, db_facts (also installable via the manager).
Db_facts, allow interaction with the underlying database via Prolog terms,
That library can also be used as a common compatibility layer for the ODBC
and proSQLite libraries of SWI-Prolog, as it works on both type of connections.

ProSQLite is debug/1 aware: call =|debug(sqlite)|= to see what is sent to 
the sqlite engine.

There are MS wins DLLs included  in the sources and recent version of the SWI package
manager will install these properly.

---++ Predicates on Versions

There is a battery of predicates about versions. Some of these can be used to debug issues between 
compiled and loaded versions of SQLite, although we never had any issues and seems they have good 
backward compatibility.

Predicates
  * sqlite_version(V,D)
    * prosqlite pack Version in Pl term (Mj:Mn) and date (date(Y,M,D)) the version was published 
  * sqlite_binary_version(Vb,Db)
    * prosqlite binary (c/prosqlite.c) version and date. This might be running behind the overall pack version as it is only synchronised when prosqlite.c is changed
  * sqlite_build_version(Bv)
    * the atomic version SQLite headers against which prosqlite.c was compiled (SQLIte's SQLITE_VERSION macro)
  * sqlite_library_c_version(Cv)
    * the runtime atomic version of the SQLite binary that can be reached from prosqlite- through sqlite3_libversion() c function call
  * sqlite_library_version([Alias,] Lv)
    * the runtime atomic version of the SQLite binary that can has been reached in a current connection (via query interface, should be identical to Cv above)

---++ Thanks

Thanks to Samer Abdallah for 2 fixes. One on mapping blobs to strings and 
second for handling UTF text correctly.

Thanks to Christian Gimenez for suggesting replacing sqlite3_close() with sqlite3_close_().
The former returns the unhandled SQLITE_BUSY if there unfinalzed statements.
_v2 is designed for garbage collected languages, see http://sqlite.org/c3ref/close.html.

Thanks to Wolfram Diestel for spotting a bug in opening 2 dbs with distinct aliases.

Thanks to Steve Moyle for contributing safe_column_names/2 (Nov 2016).

Thanks to John B Thiel (JBThiel) for opening a number of issues which led to v2.0.

---++ History

@version 1.0, 2014/12/24
@version 1.1, 2016/10/9  changed to sqlite3_close() and fixed alias bug
@version 1.2, 2016/11/22 added safe_column_names/2
@version 1.4, 2018/3/18  fixed blobs support (see examples/two.pl), and logic for already opened file
@version 1.6, 2020/5/29  recompiled for SWI 8.2
@version 1.7, 2022/4/30  print message if new db file cannot be created
@version 1.8, 2022/5/29  fixed major bug of deleting existing files introduced in 1.7 + minor doc + aarch64-linux binary
@version 2.0  2024/8/15  better error propagation & nulls, keywords as fields, library + c + build versions, disconnect remove_predicates option
@license    MIT
@author Nicos Angelopoulos
@author Sander Canisius
@see Sander Canisius, Nicos Angelopoulos and Lodewyk Wessels. proSQLite: Prolog file based databases via an SQLite interface.  In the proceedings of Practical Aspects of Declarative languages (PADL 2013), (2013, Rome, Italy).
@see Sander Canisius, Nicos Angelopoulos and Lodewyk Wessels.  Exploring file based databases via an SQLite interface.  In the ICLP Workshop on Logic-based methods in Programming Environments, p. 2-9, (2012, Budapest, Hungary).
@see http://stoics.org.uk/~nicos/pbs/padl2013-prosqlite.pdf
@see http://stoics.org.uk/~nicos/sware/prosqlite
@see http://stoics.org.uk/~nicos/sware/db_facts
@see http://www.sqlite.org/
@see files in examples/ directory
@see also available as a SWI pack http://www.swi-prolog.org/pack/list
@tbd set pragmas

*/

/* defaults and start ups */
arity_flag_values( [arity,unary,both,palette] ).

%-Section interface predicates
%

%% sqlite_version( -Version, -Date ).
% 
% The current version and its publication date.
%
% Version is a Mj:Mn:Fx term and date is a date(Y,M,D) term.
%
%==
% ? sqlite_version(V, D).
% V = 2:0:0,
% D = date(2024, 8, 15).
%==
%
% @author nicos angelopoulos
%
sqlite_version(2:0:0, date(2024,8,15)).

/** sqlite_binary_version( -Version, -Date ).

   The current version and date of publication for the proSQLite c-code. 

   This is provided as the c code is not changed as often as the prolog part of the pack.
   These changes require re-compiling the binaries of the pack distribution.

==
?- sqlite_binary_version(V,D).
V = 1:3:0,
D = date(2018, 3, 17).

?- sqlite_version(V1,D1).
V1 = 1:8:2,
D1 = date(2024, 7, 6).
==

@author nicos angelopoulos
@version  0:2 24.08.03

*/
sqlite_binary_version( Ver, Date ) :-
     c_sqlite_version( Ver, Date ).

/** sqlite_library_c_version(-LibCVers).

Get the vesion of the SQLite version via the C-interface.

==
?- sqlite_library_c_version(V).
V = '3.45.1'.
==

@author  nicos angelopoulos
@version 0.1, 2024/8/14

*/
sqlite_library_c_version( Cersion ) :-
     c_library_version( Cersion ).

/** sqlite_build_version(-LibCVers).

Get the version of the SQLITE against which the prosqlite binaries where compiled.

The Atom is bound to the SQLITE_VERSION macro from the SQLite sources at compile time.

==
?- sqlite_build_version(LibCV).
LibCV = '3.45.1'.

?- sqlite_library_c_version(V).
V = '3.45.1'.

?- sqlite_connect('/tmp/testo.sqlite', testo, exists(false)).
true.

?- sqlite_library_version(V).
V = '3.45.1'.

?- sqlite_disconnect.
ERROR: Unknown procedure: sqlite_disconnect/0
ERROR:     However, there are definitions for:
ERROR:         prosqlite:sqlite_disconnect/1
ERROR:         prosqlite:sqlite_disconnect/2
false.

?- sqlite_disconnect_all.
ERROR: Unknown procedure: sqlite_disconnect_all/0 (DWIM could not correct goal)
^  Exception: (4) setup_call_cleanup('$toplevel':notrace(call_repl_loop_hook(begin, 0)), '$toplevel':'$query_loop'(0), '$toplevel':notrace(call_repl_loop_hook(end, 0))) ? Unknown option (h for help)
^  Exception: (4) setup_call_cleanup('$toplevel':notrace(call_repl_loop_hook(begin, 0)), '$toplevel':'$query_loop'(0), '$toplevel':notrace(call_repl_loop_hook(end, 0))) ? Unknown option (h for help)
^  Exception: (4) setup_call_cleanup('$toplevel':notrace(call_repl_loop_hook(begin, 0)), '$toplevel':'$query_loop'(0), '$toplevel':notrace(call_repl_loop_hook(end, 0))) ? abort
% Execution Aborted
?- sqlite_disconnet(testo).
Correct to: "sqlite_disconnect(testo)"? yes
true.
==

@author  nicos angelopoulos
@version 0.1, 2024/8/14

*/

sqlite_build_version( Bersion ) :-
     c_library_version( Bersion ).

/** sqlite_library_version(-LibVers).
    sqlite_library_version(+Alias, -LibVers).

Get the version of the underlying sqlite library. Either uses Alias or default connection (which should exist).

This version uses the query interface so it expects a default connection to have been established. 

==
?- sqlite_library_version(V). 
false.

?- sqlite_connect('/tmp/testo.sqlite', testo, exists(false)).

?- sqlite_library_version(V).
V = '3.37.2'.

?- sqlite_disconnect(testo).
==

@author  nicos angelopoulos
@version 0.1, 2024/8/3
@see sqlite_library_c_version/1
@see sqlite_build_version/1

*/
sqlite_library_version( V ) :-
     sqlite_query( 'SELECT sqlite_version()', Row ),
     Row = row(V),
     % first draft was like below, but changed to chime with sqlite_library_c_version/1 and sqlite_build/1
     % Row = row(Atm),
     % atomic_list_concat([Mj,Mn,Fx],'.',Atm),
     % maplist( atom_number, [Mj,Mn,Fx], [Nj,Nn,Nx] ),
     % V = Nj:Nn:Nx,
     !.
sqlite_library_version( Alias, V ) :-
     sqlite_query( Alias, 'SELECT sqlite_version()', Row ),
     Row = row(V).

%% sqlite_citation( -Atom, -Bibterm ).
% Succeeds once for each publication related to this library. Atom is the atom representation
% suitable for printing while Bibterm is a bibtex(Type,Key,Pairs) term of the same publication.
% Produces all related publications on backtracking.
sqlite_citation( Atom, bibtex(Type,Key,Pairs) ) :-
     Atom = 'Sander Canisius, Nicos Angelopoulos and Lodewyk Wessels. proSQLite: Prolog file based databases via an SQLite interface.  In the proceedings of Practical Aspects of Declarative languages (PADL 2013), (2013, Rome, Italy).',
     Type = inproceedings,
     Key  = 'CanisiusS+2013',
     Pairs = [
          author = 'Sander Canisius and Nicos Angelopoulos and Lodewyk Wessels',
          title  = 'Exploring file based databases via an Sqlite interface',
          booktitle = 'In ICLP Workshop on Logic-based methods in Programming Environments (WLPE\'12)',
          year = 2012,
          pages= '2-9',
          month = 'September',
          address = 'Budapest, Hungary',
          url     = 'http://stoics.org.uk/~nicos/pbs/padl2013-prosqlite.pdf'
     ].
     
sqlite_citation( Atom, bibtex(Type,Key,Pairs) ) :-
     Atom = 'Exploring file based databases via an Sqlite interface. \n Canisius Sander, Nicos Angelopoulos and Lodewyk Wessels \n In the ICLP Workshop on Logic-based methods in Programming Environments (WLPE\'12),\n p.2-9, 2012. Budapest, Hungary.',
     Type = inproceedings,
     Key  = 'CanisiusS+2012',
     Pairs = [
          author = 'Sander Canisius and Nicos Angelopoulos and Lodewyk Wessels',
          title  = 'Exploring file based databases via an Sqlite interface',
          booktitle = 'In ICLP Workshop on Logic-based methods in Programming Environments (WLPE\'12)',
          year = 2012,
          pages= '2-9',
          month = 'September',
          address = 'Budapest, Hungary',
          url     = 'http://stoics.org.uk/~nicos/pbs/wlpe2012_sqlite.pdf'
     ].


%% sqlite_connect(+File, ?Alias).
%
%  Open a connection to an sqlite File. If Alias is a variable, an opaque atom
%  is generated and unified to it. The opened db connection to  file can be accessed via Alias.
%
%  ==
%    sqlite_connect('uniprot.sqlite', uniprot).
%  ==
%
sqlite_connect(File, Conn) :-
     sqlite_connect(File, Conn, []).

/** sqlite_connect(+File, ?Connection, +Options).

Open a connection to an sqlite File. If Connection is unbound then if (a)  alias(Alias) option is given, 
Connection is bound to Alias, else (b) an opaque atom is generated. If Connection is ground, 
the opened can be accessed with Connection as a handle.

Options is a single term or a list of terms from the following:
  * alias(Atom)
     identify the connection as Alias (no default, interplays with Connection)

  * as_predicates(AsPred=false)
     if true, create hook predicates that map each sqlite table to a prolog predicate.
     These are created in module user (see at_module()). 
     The user should make sure the predicate is not previously defined.

  * at_module(AtMod=user)
     the module at which the predicates will be asserted at
     (if as_predicates(true)) is also given)

  * arity(Arity=arity)            
     Arity denotes the arity of access clauses to be added in the prolog database that
     correspond to SQLite tables. The default is =|arity|=, which asserts a 
     predicate matching the arity of the table.
     =|both|= adds two predicates, one matching the arity and a single argument one.
     The later can be interrogated with something like
     == 
     ?-  phones( [name=naku, telephone=T] ).
     ==
     =|unary|= only adds the unary version, and =|palette|= adds a suite of predicates
     with arities from 1 to N, where N is the number of columns.
     These can be interrogated by :
     == 
     ?-  phones( name=Name ).
     ?-  phones( name=naku, telephone=T ).
     ?-  phones( [name=naku, telephone=T] ).
     ==
     
     Predicated tables can be used to insert values to the database by virtue of all
     their columns are give ground values.

  * exists(Exists=true)
     do not throw an error if file does not exist and Exists is false.

  * ext(Ext=sqlite) 
     database files are assumed to have an sqlite extension.  
     To ovewrite this give a different Ext or '' for no extension.

  * table_as(Table,Pname,Arity)
     map the table to predicate with name Pname. Arity should be
     defined for this representaition as per arity() option.

  * verbose(Verb=false)
     Iff Verb==true print messages- currently about file used.

When unary predicates are defined the columns can be interrogated/accessed by list pairs of the form Column=Value.
Column-Value and Column:Value are also recognised. 

So for example, for table =|phones|= with columns Name, Address and Phone, prosqlite will add 

==
     phones(_,_,_) 
==

     as a response to as_predicates, and
     
==
     phones(_)  
==
if Arity is =|unary|=.

The latter can be interrogated by 

==
     phones( ['Name'=naku','Phone'=Phone] ).
==
which will return the phone number(s) associated with individual named by =|naku|=.

@author nicos angelopoulos
@version  0.2 2018/03/17,    fixed logic for existing connection to a file (existing alias is returned)
@version  0.3 2022/04/30,    add permissions error message, if new sqlite file cannot be created

@see examples/predicated.pl .
@see pack(prosqlite/examples/two.pl)

*/

sqlite_connect(FileIn, Conn, OptIn) :-
     to_list( OptIn, Opts ),
     ( memberchk(ext(Ext),Opts) -> true; Ext=sqlite ),
     ( file_name_extension(_,Ext,FileIn) -> File=FileIn; file_name_extension(FileIn,Ext,File) ),
     sqlite_connect_1(File, Conn, Opts).

sqlite_connect_1(File, _Conn, Opts) :-
     \+ exists_file(File),
     \+ memberchk(exists(false), Opts),
     !,
     open(File, read, _). % just so it throws a nice error
sqlite_connect_1(File1, Conn, Opts) :-
     sqlite_alias(Opts, Conn, Alias),
     \+ var(Alias),
     sqlite_connection(Alias,File2,_),
     !,
     ( File1==File2 -> 
          print_message( informational, sqlite(connection_already_open(Conn)) )
          ;
          sqlite_error( alias_in_use(Alias,File2) )
     ).
sqlite_connect_1(File, Alias, Opts) :-
     sqlite_alias(Opts, Conn, Alias),
     ( sqlite_connection(_Conn1,File,Alias1) ->
        Alias1 = Alias,
        ( (memberchk(verbose(Verb),Opts),Verb==true) -> 
                print_message( informational, sqlite(file_already_open(File,Alias1)) )
                ;
                true
        )
        ;
        ( (memberchk(verbose(Verb),Opts),Verb==true) -> 
            print_message( informational, sqlite(db_at(File)) )
            ;
            true
        ),
        % 22.04.30: adding the following 2 commands to get an informative error when file cannot be open for updating (report on github by korvo)
        open( File, append, TmpStream ),
        close( TmpStream ),
        c_sqlite_connect(File, Conn),
        asserta( sqlite_connection(Alias,File,Conn) ),
        ( sqlite_establish_predicates(Opts, Conn) ->
            true
            ;
            retractall( sqlite_connection(Alias,File,Conn) ),
            c_sqlite_disconnect(Conn),
            sqlite_error( predicated_creation_error(File,Alias) )
        )
    ).

/*
sqlite_connect(File, Conn, Opts) :-
     c_sqlite_connect(File, Internal),
     !,
     assert( sqlite_connection(Conn,File,Internal) ). 
     */

% this is currently private only for use with at_halt.
% 
sqlite_disconnect :-
     sqlite_connection(Alias,_,_),
     sqlite_disconnect( Alias, [] ),
     fail.
sqlite_disconnect.
     
/**  sqlite_disconnect(+Alias).
     sqlite_disconnect(+Alias, +Options).

Terminate the connection to an SQLite database file.

Options is a single term or a list of terms from the following:
  * remove_predicates(Rmv=abolish)
     defines the method of removing predicates that are defined on tables of connection Alias.
     (That is, that =|as_predicates(true)|+ was used when created Alias connection.)
     Set to =|retractall|= to only retract predicated definitions, by default these are abolished. 
     prolog_flag/2 key sqlite_remove_predicates can set a new default value.


Examples
==
   ?- 
        sqlite_disconnect(uniprot).

?- sqlite_connect( kword, kwordo, as_predicates(true) ).
true.

?- kwordo(A,B,C).
A = 1,
B = f11,
C = f21 ;
A = 2,
B = f12,
C = groupaa ;
false.

?- sqlite_disconnect( kwordo ).
true.

?- kwordo(A,B,C).
ERROR: Unknown procedure: kwordo/3 (DWIM could not correct goal)
?- sqlite_connect( kword, kwordo, as_predicates(true) ).
true.

?- kwordo(A,B,C).
A = 1,
B = f11,
C = f21 ;
A = 2,
B = f12,
C = groupaa ;
false.

?- sqlite_disconnect( kwordo, remove_predicates(retractall) ).
true.

?- kwordo(A,B,C).
false.

==

@author nicos angelopoulos
@version  0:2 2024/08/03

*/ 
sqlite_disconnect( Alias ) :-
     sqlite_disconnect( Alias, [] ).

sqlite_disconnect( Alias, OptIn ) :-
     once( sqlite_connection(Alias,_,Conn) ),
     !,
     debug( sqlite, 'Disconnecting from db with alias: ~w.', [Alias] ),
     findall( pam(Pname,Arity,Mod), sqlite_db:sqlite_asserted(Conn,Pname,Arity,Mod), PAs ),
     c_sqlite_disconnect( Conn ),
     retractall( sqlite_connection(Alias,_,Conn) ),
     to_list( OptIn, Opts ),
     sqlite_disconnect_predicates( Opts, Abo ),
     maplist( sqlite_clean_up_predicated_for(Abo,Conn), PAs ).
sqlite_disconnect( Alias, _ ) :-
     sqlite_fail( not_a_connection(Alias) ).

sqlite_disconnect_predicates( Opts, Rmv ) :-
     memberchk( remove_predicates(Rmv), Opts ),
     !.
sqlite_disconnect_predicates( _Opts, Rmv ) :-
     current_prolog_flag( sqlite_remove_predicates, Rmv ),
     !.
sqlite_disconnect_predicates( _Opts, Rmv ) :-
     Rmv = abolish.

sqlite_clean_up_predicated_for( Abo, Conn, Pam ) :-
     sqlite_clean_up_predicated_for_known( Abo, Conn, Pam ),
     !.
sqlite_clean_up_predicated_for( Abo, _Conn, _Pam ) :-
     throw( unknown_value(remove_predicates,Abo) ).

sqlite_clean_up_predicated_for_known( retractall, Conn, pam(Pname,Arity,Mod) ) :-
     functor( Head, Pname, Arity ),
     retractall( Mod:Head ),
     retractall( sqlite_db:sqlite_asserted(Conn,Pname,Arity,Mod) ).
sqlite_clean_up_predicated_for_known( abolish, Conn, pam(Pname,Arity,Mod) ) :-
     abolish( Mod:Pname, Arity ),
     retractall( sqlite_db:sqlite_asserted(Conn,Pname,Arity,Mod) ).

%% sqlite_current_connection(-Connection).
%
%  Return or interrogate the name of open connection handles.
%
sqlite_current_connection(Conn) :-
     sqlite_connection(Conn,_,_).

%% sqlite_default_connection(-Connection).
%
%  Return or interrogate the name of the default connection. This is the 
%  last connection opened.
%
sqlite_default_connection(Alias) :-
     sqlite_connection(Alias,_,_),
     !.

%% sqlite_query(+Sql, -Row).
%
%  Post an Sql query to default connection and get row result in Row.
%
sqlite_query(Sql, Row) :-
     sqlite_default_connection(Alias),
     sqlite_query(Alias, Sql, Row).

%% sqlite_query(+Connection, +Sql, -Row).
%
%  Post an Sql query to Sqlite Connection and get row result in Row.
%
sqlite_query(Alias, Query, Row) :-
     sqlite_alias_connection(Alias, Connection),
     debug( sqlite, 'Alias: ~w, sending: ~a', [Alias,Query] ),
     c_sqlite_query(Connection, Query, Row).

%% sqlite_format_query(+Connection, +FAs, -Row).
%
%  Post a format style Sql query to Sqlite Connection and get row result in Row.
%  FAs is a - pair structure : Format-Arguments.
%
%  ==
%     sqlite_format_query(uniprot, 'PRAGMA table_info(~w)'-Table, row(_, Column, _, _, _, _))
%  ==
%
%
sqlite_format_query(Alias, Format-Arguments, Row) :-
    format(atom(Query), Format, Arguments),
    sqlite_query(Alias, Query, Row).

%% sqlite_current_table(+Connection, -Table).
%
%  Return or interrogate tables in the Sqlite database associated with Connection.
%
sqlite_current_table(Alias, Table) :-
    var( Table ),
    !,
    sqlite_query(Alias, 'SELECT name FROM sqlite_master WHERE type = "table"', row(Table)).
sqlite_current_table(Alias, Table) :-
    ground( Table ),
    sqlite_query(Alias, 'SELECT name FROM sqlite_master WHERE type = "table"', row(TableIn)),
    %13.10.26: have a look at the C code above to see if 'row(Table)' can work on the above line.
    Table = TableIn,
    !.

%% sqlite_current_table(+Connection, ?Table, -Facet ).
%
%  Facet is a property of Table found at Connection. Currently only arity(Arity) is
%  delivered.
sqlite_current_table(Connection, Table, Facet ) :-
     sqlite_current_table(Connection, Table),
     sqlite_facet_table( Facet, Connection, Table ).

%% sqlite_table_column(+Connection, ?Table, -Column).
%
%  Return or interrogate tables and columns in the Sqlite database associated with Connection.
%
sqlite_table_column( Alias, Table, Column ) :-
     set_table( Alias, Table ),
    sqlite_format_query(Alias, 'PRAGMA table_info(~w)'-Table, row(_, Column, _, _, _, _)).

%% sqlite_table_column(+Connection, ?Table, ?Column, -Facet).
%
%   Facet is one of: 
%    *  position(Nth0)    position of the Column in the table, first being 0.
%    *  data_type(Dtype)  the data type for the column
%    *  nullable(Null)    can this column be set to the null value
%    *  defautl(Default)  the default value for the 
%    *  primary_key(Key)  is this column part of the primary key ?
%
sqlite_table_column(Alias, Table, Column, Facet) :-
    set_table( Alias, Table ),
    sqlite_format_query(Alias, 'PRAGMA table_info(~w)'-Table, Row ),
    Row = row(_, Column, _, _, _, _),
    sqlite_pragma_info_facet( Row, Facet ).

sqlite_pragma_info_facet( row(Nth0,_,_,_,_,_), position(Nth0) ).
sqlite_pragma_info_facet( row(_,_,Dtype,_,_,_), data_type(Dtype) ).
sqlite_pragma_info_facet( row(_,_,_,Null,_,_), nullable(Null) ).  % fixme, ensure same semantics as ODBC
sqlite_pragma_info_facet( row(_,_,_,_,Default,_), default(Default) ).
sqlite_pragma_info_facet( row(_,_,_,_,_,Key), primary_key(Key) ).

%% sqlite_pragma( +Alias, +Pragma, -Row ).
%
%  Interrogate SQLite Pragmas. Currently only reading is supported.
%  Pragma can be an atom or a - separated pair, as in =|table_info-TableName|=.
% 
%==
%     sqlite_pragma( phone_db, encoding, Row).
%==
sqlite_pragma( Alias, Pragma-Par, Row ) :-
     !,
     atomic_list_concat( ['PRAGMA',Pragma,'(~w)'],' ', Query ), 
     sqlite_format_query( Alias, Query-Par, Row ).
sqlite_pragma( Alias, Pragma, Row ) :-
     atomic_list_concat( ['PRAGMA',Pragma],' ', Query ), 
     sqlite_query( Alias, Query, Row ).

% pragmas_info( [...,encoding,...,secure_delete,synchronous,temp_store,writable_schema] ).
pragmas_comm( [shrink_memory] ).


set_table( Alias, Table ) :-
     ( var(Table) -> 
          sqlite_current_table(Alias, Table) 
          ;
          true
     ).

%% sqlite_table_count(+Connection, +Table, -Count).
% 
%  True if Count is the number of rows in Sqlite Connection associated Table.
%
sqlite_table_count(Alias, Table, Count) :-
     Sel = 'Select count (*) from ~w',
    sqlite_format_query(Alias, Sel-Table, row(Count)),
     !.

/** sqlite_date_sql_atom( Date, Sql ).

Convert between  a Prolog date/3 term and an Sql atom. 
The conversion is bidirectional.
*/
sqlite_date_sql_atom( date(Y,M,D), Sql ) :-
     ground( Sql ), 
     !,
     atomic_list_concat( Consts, '/', Sql ),
     maplist( atom_number, Consts, [Y,M,D] ).
sqlite_date_sql_atom( date(Y,M,D), Sql ) :-
     atomic_list_concat( ['"',Y], Ya ),
     atomic_list_concat( [D,'"'], Da ),
     atomic_list_concat( [Ya,M,Da], '/', Sql ).


%-Section non-interface sqlite specific predicates
%

sqlite_alias(Opts, _Conn, Alias) :-
     memberchk(alias(Alias), Opts),
     !.
sqlite_alias(_Opts, _Conn, Alias ) :-
     atomic( Alias ),
     !.
sqlite_alias(_Opts, Conn, Conn).

sqlite_establish_predicates( Opts, Conn ) :-
     memberchk(as_predicates(true), Opts), 
     !,
     findall(T-C, sqlite_table_column(Conn,T,C), TCs ),
     findall( T, member(T-_,TCs), RepTs ),
     sort( RepTs, Ts ),
     findall( T-Cs, (member(T,Ts),findall(C,member(T-C,TCs),Cs)), TdCs ),
     ( memberchk(at_module(Mod), Opts) -> true; Mod = user ),
     arity_option( Opts, ArityF ),
     sqlite_establish_tables(TdCs, Conn, Mod, ArityF, Opts ).
sqlite_establish_predicates(_Opts, _Conn ).

sqlite_establish_tables( [], _Conn, _Mod, _ArityF, _Opts ).
sqlite_establish_tables( [Table-Columns|T], Conn, Mod, ArityF, Opts ) :-
     ( memberchk(table_as(Table,Pname,TArityF), Opts) ->
          true
          ;
          Pname = Table, TArityF = ArityF
     ), 
     sqlite_establish_table(TArityF,Table,Pname,Columns,Conn,Mod),
          % Internal = 'Internal prosqlite error. Unable to establish table',
          % throw( Internal:TArityF/Table )  % handled furter up now
     sqlite_establish_tables( T, Conn, Mod, ArityF, Opts ).

sqlite_establish_table( arity, Table, Pname, Columns, Conn, Mod ) :-
     length( Columns, Arity ),
     sqlite_establish_table_typed( Table, Pname, Columns, Conn, Mod, predicate, Arity ).
sqlite_establish_table( both, Table, Pname, Columns, Conn, Mod ) :-
     sqlite_establish_table_typed( Table, Pname, Columns, Conn, Mod, unary, 1 ),
     length( Columns, Arity ),
     sqlite_establish_table_typed( Table, Pname, Columns, Conn, Mod, predicate, Arity ).
sqlite_establish_table( unary, Table, Pname, Columns, Conn, Mod ) :-
     sqlite_establish_table_typed( Table, Pname, Columns, Conn, Mod, unary, 1 ).
sqlite_establish_table( palette, Table, Pname, Columns, Conn, Mod ) :-
     length( Columns, Arity ),
     % Shorter is Arity - 1,
     findall( _, ( between(1,Arity,I), 
                   sqlite_establish_table_typed(Table, Pname, Columns, Conn, Mod, palette, I)
                 ), _ ).

sqlite_establish_table_typed( Table, Pname, Columns, Conn, Mod, ArityF, Arity ) :-
     functor( Head, Pname, Arity ),
     Head =..[Pname|Args],
     Body = prosqlite:sqlite_holds(Conn,Table,Arity,ArityF,Columns,Args),
     ( clause(Mod:Head,_Body) ->
          sqlite_fail( maps_to_existing_predicate(Pname,Arity) )
          ;
          true
     ),
     % retractall( Mod:Head ),   % fixme: double check this and test it works
     ( sqlite_db:sqlite_asserted(Conn1,Pname,Args,_Mod1) ->
          sqlite_fail( predicate_already_registered(Conn1,Pname,Arity) )
          ;
          Mod:assert((Head :- Body))
     ),
     assert( sqlite_db:sqlite_asserted(Conn,Pname,Arity,Mod) ).
     % assert((Head :- Body)),

sqlite_holds( AliasOr, Name, _Arity, Type, Columns, Args ) :-
     sqlite_alias_connection( AliasOr, Conn ),
     pl_as_predicate_to_sql_ready_data( Type, Columns, Args, KnwnClmPrs, UnKnwnCs, UnKnwnAs ),
     safe_column_names(Columns, SafeColumns),
     safe_column_names(UnKnwnCs, SafeUnKnwnCs),
     sqlite_holds_unknown( SafeUnKnwnCs, UnKnwnAs, KnwnClmPrs, Name, SafeColumns, Conn ).

/* fixme:
sqlite_holds_unknown( [], _UnKnwnAs, KnwnClmPrs, Name, Columns, Conn ) :-
     shall we throw an error if there is nothing to report and nothing to assert ?
     */

sqlite_holds_unknown( UnKnwnCs, UnKnwnAs, KnwnClmPrs, Name, _Columns, Conn ) :-
     sql_clm_value_pairs_to_where(KnwnClmPrs, Where),
     atomic_list_concat( UnKnwnCs, ',', UnC ),
     atomic_list_concat( ['Select ',UnC,'From',Name,Where,';'], ' ', Sql ),
     Row =.. [row|UnKnwnAs],
     debug( sqlite, 'Conn: ~w, sending: ~a', [Conn,Sql] ),
     c_sqlite_query(Conn, Sql, Row).

sqlite_alias_connection( Alias, Connection ) :-
     sqlite_connection( Alias,_,Connection ),
     !.
% allows access with either alias or connection :
sqlite_alias_connection( Connection, Connection ) :-
     sqlite_connection(_,_,Connection),
     !.
sqlite_alias_connection( Alias, _Connection ) :-
     sqlite_error( unknown_alias(Alias) ).

% fixme: we should really use the db_facts code here.
pl_as_predicate_to_sql_ready_data( unary, Columns, [Args], KnwnClmPrs, UnKnwnCs, UnKnwnAs ) :-
     pl_look_for_args_to_un_known( Args, Columns, KnwnClmPrs, UnKnwnCs, UnKnwnAs ).
pl_as_predicate_to_sql_ready_data( palette, Columns, ArgsIn, KnwnClmPrs, UnKnwnCs, UnKnwnAs ) :-
     ( (ArgsIn=[Args],is_list(Args)) -> true; Args = ArgsIn ),
     pl_args_column_arg_ground_or_not_pairs(Args,Columns,KnwnClmPrs,UnKnwnCs,UnKnwnAs),
     ( maplist(var,Args) ->
          true % then a palette predicate has been called with full arity and all variables
          ;
          % maplist( look_for_pair,Args,_,_),
          findall( LFA, (member(LFA,Args),look_for_pair_silent(LFA,_,_)), [] )
          % then a palette predicate has been called with full arity and look_for_pair
     ),
     !.
pl_as_predicate_to_sql_ready_data( palette, Columns, ArgsIn, KnwnClmPrs, UnKnwnCs, UnKnwnAs ) :-
     ( (ArgsIn=[Args],is_list(Args)) -> true; Args = ArgsIn ),
     pl_look_for_args_to_un_known( Args, Columns, KnwnClmPrs, UnKnwnCs, UnKnwnAs ).
pl_as_predicate_to_sql_ready_data( predicate, Columns, Args, KnwnClmPrs, UnKnwnCs, UnKnwnAs ) :-
     pl_args_column_arg_ground_or_not_pairs( Args, Columns, KnwnClmPrs, UnKnwnCs, UnKnwnAs ).

pl_args_column_arg_ground_or_not_pairs( [], [], [], [], [] ).
pl_args_column_arg_ground_or_not_pairs( [A|As], [C|Cs], Knwn, UnCs, UnAs ) :-
     ( ground(A) -> 
          Knwn = [C-A|TKnwn],
          TUnCs = UnCs,
          TUnAs = UnAs
          ;
          TKnwn = Knwn,
          UnCs = [C|TUnCs],
          UnAs = [A|TUnAs]
     ),
     pl_args_column_arg_ground_or_not_pairs( As, Cs, TKnwn, TUnCs, TUnAs ).

pl_look_for_args_to_un_known( [], _Columns, [], [], [] ).
pl_look_for_args_to_un_known( [A|As], Columns, Knwn, UnKnwnCs, UnKnownAs ) :-
     look_for_pair( A, Clm, Val ),
     is_one_of_columns( Clm, Columns ),
     ( ground(Val) ->
          Knwn = [Clm-Val|TKnwn],
          TUnKnwnCs = UnKnwnCs,
          TUnKnownAs = UnKnownAs
          ;
          TKnwn = Knwn,
          UnKnwnCs = [Clm|TUnKnwnCs],
          UnKnownAs = [Val|TUnKnownAs]
     ),
     pl_look_for_args_to_un_known( As, Columns, TKnwn, TUnKnwnCs, TUnKnownAs ).

is_one_of_columns( Clm, Columns ) :-
     memberchk( Clm, Columns ), 
     !.
is_one_of_columns( Clm, Columns ) :-
     sqlite_error( unknown_column(Clm,Columns) ).

look_for_pair( Pair, K, V ) :-
     look_for_pair_silent( Pair, K, V ),
     !.
look_for_pair( Term, _A, _B ) :-
    % print_message(informational, pack(git_fetch(Dir))).
     sqlite_error( pair_representation(Term) ).
     % type_error( 'Binary compound with functor {=,-,:}', Term ). 
     % Type = 'Binary compound with functor {=,-,:}',
     % print_message( error, error(type_error(Type,Term)) ),
     % abort.

look_for_pair_silent( A=B, A, B ).
look_for_pair_silent( A-B, A, B ).
look_for_pair_silent( A:B, A, B ).

/* error messages */

sqlite_error( Term ) :-
     Type = error,
     print_message( Type, sqlite(Term) ),
     abort.

sqlite_fail( Term ) :-
     Type = informational,
     sqlite_fail( Type, Term ).

sqlite_fail( Type, Term ) :-
     print_message( Type, sqlite(Term) ),
     fail.

%-Section error handling.

:- multifile prolog:message//1.

prolog:message(sqlite(Message)) -->
    message(Message).
prolog:message(sqlite_error(Code,Message)) -->
    message(sqlite_error(Code,Message)).

message( sqlite_error(Code,Mess) ) -->
     ['SQLite code: ~d, with short message: ~w' - [Code,Mess] ].
message( pair_representation(Term) ) -->
     ['Wrong term type ~q in predicated table arguments. Expected binary with functor, {=,:,-}.' - [Term] ].
message( unknown_column(Clm,Columns) ) -->
     [ 'Unkown column, ~q expected one in ~q.' - [Clm,Columns] ].
message( unknown_alias(Alias) ) -->
     ['Not a known alias or connection:~q.' - Alias ].
message( wrong_arity_value(ArityF) ) -->
     { arity_flag_values( Arities ) },
     [ 'Unrecognised arity option value ~q, expected ~q.' - [ArityF,Arities] ].
message( predicated_creation_error(File,Alias) ) -->
     [ 'Closed connection ~q to file ~q due to failure in predicated table creation.' - [Alias,File] ].
message( connection_already_open(Conn) ) -->
     [ 'Connection already open ~q.'- [Conn] ].
message( alias_in_use(Conn,File) ) --> 
     [ 'Alias/connection ~q already in use for file ~q.'- [Conn,File] ].
message( not_a_connection(Alias) ) -->
     [ 'Not an open connection or known alias to a connection: ~q' - [Alias] ].
message( insufficient_columns(Goal,Op) ) -->
     [ 'Insufficient number of known column values in ~q for operation ~q.' - [Goal,Op] ].
message( predicate_already_registered(Conn,Pname,Arity) ) -->
     [ 'Predicate ~q already registered by connection ~q' - [Pname/Arity,Conn] ].
message( maps_to_existing_predicate(Pname,Arity) ) -->
     ['Predicated table maps to existing predicate ~q.' - [Pname/Arity] ].
message( file_already_open(File,Alias) ) -->
     ['File, ~q already open with alias ~q.' - [File,Alias] ].
message( db_at(File) ) -->
     ['Using database from file: ~q.' - [File] ].
message( asserting_non_ground(Goal) ) -->
     [ 'Asserting non ground term ~q.' - [Goal] ].
message( debug(Format,Args) ) -->
     [ 'Found Format (1st arg) ~q and Args (2nd arg) ~q.' - [Format,Args] ].
     
%-Section sqlite non-specific auxiliary predicates 
%
to_list(OptIn, Opts) :-
     is_list(OptIn),
     !,
     Opts = OptIn.
to_list(Opt, [Opt] ).

dquote( Val, Quoted ) :-
     number( Val ), 
     !,
     Quoted = Val.
dquote( Val, Quoted ) :-
     atom( Val ),
     !,
     atomic_list_concat( ['"',Val,'"'], Quoted ).
dquote( Val, Quoted ) :-
     is_list( Val ),
     append( [0'"|Val], [0'"], QuotedCs ),
     atom_codes( Quoted, QuotedCs ).

sql_clm_value_pairs_to_where(Known, Where) :-
     sql_clm_value_pairs_to_where_conjunction(Known, Conjunction),
     sql_where_conjunction_to_where(Conjunction, Where).

sql_where_conjunction_to_where('', '' ) :- !.
sql_where_conjunction_to_where(Conjunction, Where ) :-
     atom_concat( 'Where ', Conjunction, Where ).

sql_clm_value_pairs_to_where_conjunction([], '').
sql_clm_value_pairs_to_where_conjunction([K-V|T], Where) :-
     sql_clm_value_pairs_to_where_conjunction(T, InWhere),
     sql_clm_and_val_to_sql_equals_atom(K, V, KVAtm),
     ( InWhere == '' -> 
          Where = KVAtm
          ;
          atomic_list_concat([KVAtm, ' AND ', InWhere], Where)
     ).

sql_clm_and_val_to_sql_equals_atom(K, V, KVAtm) :-
     ( number(V) -> 
          atom_number(Vatm, V),
          atom_concat('=',Vatm,EqV),
          atomic_list_concat( ['[',K,']',EqV], '', KVAtm )
          ;
          ( V == '$null$' ->
               atomic_list_concat( '[',K,'] IS NULL', KVAtm )
               ;
               atom_concat(V, '\'', VDsh),
               atom_concat('=\'',VDsh,EqV),
               atomic_list_concat( ['[',K,']',EqV], '', KVAtm )
          )
     ).

sqlite_facet_table( arity(Arity), Connection, Table ) :-
     findall( Column, sqlite_table_column(Connection, Table, Column), Columns ),
     length( Columns, Arity ).

arity_option( Opts, ArityF ) :-
     memberchk( arity(ArityF), Opts ),
     arity_flag_values( Arities ),
     memberchk( ArityF, Arities ),
     !.
arity_option( Opts, ArityF ) :-
     memberchk( arity(ArityF), Opts ),
     !,
     sqlite_fail( wrong_arity_value(ArityF) ).
arity_option( _Opts, arity ). % default for this flag, although we should 
                             % move all defaults to one location/list (fixme)

kv_decompose( [], [], [] ).
kv_decompose( [K-V|T], [K|Ks], [V|Vs] ) :-
     kv_decompose( T, Ks, Vs ).

%!  safe_column_names(+Cols, -SafeCols) is det
%
%   @author: Steve Moyle
%
%   +Cols is a list of column names stored in the sqlite db
%   -SafeCols is the each element of the input is the atom wrapped
%   in [].
%
%     sqlite (and possibly other RDBMSs) does not like columns with
%     names including periods like:
%
%     'id.orig_h'
%      They need to be refered to in the select statement as:
%      [id.orig_h]
%
%      ?- safe_column_names(['id.orig_h'], S).
%      Correct to: "prosqlite:safe_column_names(['id.orig_h'],S)"? yes
%      S = ['[id.orig_h]'].
%
safe_column_names([], []).
safe_column_names([Col | Cols], [SafeCol | SafeCols]) :-
    safe_column_name(Col, SafeCol),
    safe_column_names(Cols, SafeCols).

safe_column_name(Col, SafeCol) :-
        format(atom(SafeCol), '[~w]', [Col]).
