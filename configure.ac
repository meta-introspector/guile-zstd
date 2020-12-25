#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])

AC_INIT([Guile-zstd], [0.1], [guile-user@gnu.org], [guile-zstd],
  [https://notabug.org/guile-zstd/guile-zstd])
AC_CONFIG_SRCDIR([zstd.scm])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects color-tests parallel-tests -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([zstd/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your GNU Guile installation.])
fi

if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

PKG_CHECK_MODULES([ZSTD], [libzstd], [:],
  [AC_MSG_ERROR([Libzstd not found; please install it first.])])

AC_MSG_CHECKING([zstd library directory])
PKG_CHECK_VAR([ZSTD_LIBDIR], [libzstd], [libdir])
AC_MSG_RESULT([$ZSTD_LIBDIR])
AS_IF([test "x$ZSTD_LIBDIR" = "x"], [
  AC_MSG_ERROR([unable to identify zstd library directory])
])
AC_SUBST([ZSTD_LIBDIR])


dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
