:- module( ex_two, [two/0,twice/0,twins/0] ).

:- use_module( library(prosqlite) ).

/**  two.

Tests the opening of two databases with system generated ids.<br>
Version 1.2 handles that wrongly.

==
% with old version:
?- two.
version(1:2:0,date(2016,11,22))
handles(<#40e844ff127f0000>,<#40e844ff127f0000>)

% note that handles are identical...

?- two.
version(1:3:0,date(2016,3,17))
handles(<sqlite>(0x22174b8),<sqlite>(0x28159c8))
?- twice.
version(1:3:0,date(2016,3,17))
handles(<sqlite>(0x22174b8),<sqlite>(0x22174b8))

?- twins
version(1:3:0,date(2016,3,17))
% Using database from file: '/tmp/sqlite_two_ex_first.sqlite'.
handles(<sqlite>(0xe5caa8),<sqlite>(0xe5caa8))

% in the above you get one info message as first call is with verbose(true)
==

@author nicos angelopoulos
@version  0.1 2018/3/17

*/
two :-
    sqlite_version( A, B ),
    write( version(A,B) ), nl,
    sqlite_connect( '/tmp/sqlite_two_ex_first.sqlite', Sqlite1, exists(false) ),
    sqlite_connect( '/tmp/sqlite_two_ex_second.sqlite', Sqlite2, exists(false) ),
    write( handles(Sqlite1,Sqlite2) ), nl.

twice :-
    sqlite_version( A, B ),
    write( version(A,B) ), nl,
    sqlite_connect( '/tmp/sqlite_two_ex_first.sqlite', Sqlite1, exists(false) ),
    sqlite_connect( '/tmp/sqlite_two_ex_first.sqlite', Sqlite2, exists(false) ),
    write( handles(Sqlite1,Sqlite2) ), nl.

twins :-
    sqlite_version( A, B ),
    write( version(A,B) ), nl,
    sqlite_connect( '/tmp/sqlite_two_ex_first.sqlite', Sqlite1, verbose(true) ),
    sqlite_connect( '/tmp/sqlite_two_ex_first.sqlite', Sqlite2 ),
    write( handles(Sqlite1,Sqlite2) ), nl.
    
