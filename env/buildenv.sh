# This file contains the environment that can be used to
# build the foreign pack outside Prolog.  This file must
# be loaded into a bourne-compatible shell using
#
#   $ source buildenv.sh

PATH='/usr/local/users/nicos/local/git/lib/swipl-6.1.12/bin/x86_64-linux:/usr/local/users/nicos/local/git/bin:/usr/local/users/nicos/local/bin:/home/nicos/bin:/usr/local/bin:/usr/local/users/nicos/local/bin:/usr/local/users/nicos/local/src/jdk1.3.1_02/bin:/home/nicos/bin:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games'
SWIPL='/usr/local/users/nicos/local/git/lib/swipl-6.1.12/bin/x86_64-linux/swipl'
SWIPLVERSION='60112'
SWIHOME='/usr/local/users/nicos/local/git/lib/swipl-6.1.12'
SWIARCH='x86_64-linux'
PACKSODIR='lib/x86_64-linux'
SWISOLIB=''
SWILIB='-lswipl'
CC='gcc'
LD='swipl-ld'
CFLAGS='-fno-strict-aliasing -pthread -fPIC  -I"/usr/local/users/nicos/local/git/lib/swipl-6.1.12/include"'
LDSOFLAGS='-rdynamic -O2 -pthread -Wl,-rpath=/usr/local/users/nicos/local/git/lib/swipl-6.1.12/lib/x86_64-linux  -shared'
SOEXT='so'
USER='nicos'
HOME='/home/nicos'

export  PATH SWIPL SWIPLVERSION SWIHOME SWIARCH PACKSODIR SWISOLIB SWILIB CC LD CFLAGS LDSOFLAGS SOEXT USER HOME
