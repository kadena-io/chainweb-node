AC_INIT([Haskell LevelDB package],
        [0.1],
        [mad.one@gmail.com],
        [leveldb],
        [http://thoughtpolice.github.com/hs-leveldb])

AC_LANG_CPLUSPLUS

dnl ****************
dnl Check for snappy
dnl ****************

AC_CHECK_HEADERS([snappy.h],
    [snappy=yes],
    [snappy=no])

if test "x$snappy" = "xyes"
then
  SNAPPY="-DSNAPPY"
  SNAPPYLIB="snappy"
else
  SNAPPY=""
  SNAPPYLIB=""
fi

dnl ********************
dnl Check for cstdatomic
dnl ********************

AC_MSG_CHECKING(whether compiler supports C++11 cstdatomic)

OLD_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -std=c++0x"
AC_COMPILE_IFELSE([
#include <cstdatomic>
int main() {}
], [
     AC_MSG_RESULT(yes)
     has_cstdatomic=yes
   ],
   [
     AC_MSG_RESULT(no)
     has_cstdatomic=no
   ])
CXXFLAGS="$OLD_CXXFLAGS"

if test "x$has_cstdatomic" = "xyes"
then
  CSTDATOMIC="-DLEVELDB_CSTDATOMIC_PRESENT -std=c++0x"
else
  CSTDATOMIC=""
fi

dnl *****************
dnl Substitute output
dnl *****************

AC_SUBST([SNAPPY])
AC_SUBST([SNAPPYLIB])

AC_SUBST([CSTDATOMIC])

AC_CONFIG_FILES([leveldb-haskell.buildinfo])
AC_OUTPUT
