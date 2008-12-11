# -*- Autoconf -*-
AC_PREREQ(2.61)
AC_INIT(clusterl, 0.1.0)
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_CONFIG_SRCDIR([src/clusterl.erl])

# Standard Programs
AC_PROG_SED

# Erlang
AC_ERLANG_NEED_ERLC
AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_CHECK_LIB(kernel, [], [AC_MSG_FAILURE(cannot find kernel, [1])])
AC_ERLANG_CHECK_LIB(stdlib, [], [AC_MSG_FAILURE(cannot find stdlib, [1])])
AC_ERLANG_CHECK_LIB(mnesia, [], [AC_MSG_FAILURE(cannot find mnesia, [1])])
AC_ARG_VAR(DIALYZER, The path to the OTP tool dialyzer)
AC_PATH_PROG(DIALYZER, dialyzer)

# Output
AC_CONFIG_FILES([Makefile
                 src/Makefile])
AC_OUTPUT
