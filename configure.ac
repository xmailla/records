dnl
dnl Autoconf script for records
dnl


dnl Remember to update the version
AC_INIT([records], [1.5.1],[http://launchpad.net/~emacs+records],[records])
AC_MSG_NOTICE([$PACKAGE_STRING])
dnl Need this version.
AC_PREREQ(2.50)

dnl Initialize automake, requiring version 1.9 or better,
dnl using moder tar format and generate both bzip tarballs,
dnl zip archive and gzip tarballs
AM_INIT_AUTOMAKE([tar-ustar dist-bzip2 dist-zip 1.9])

dnl
dnl Apparently, if you run a shell window in Emacs, it sets
dnl the EMACS environment variable to 't'. Lets undo that.
dnl
if test "${EMACS}" = "t"; then
   EMACS=""
fi

dnl AC_ARG_WITH(xemacs
AC_ARG_WITH(xemacs,
		AC_HELP_STRING([--with-xemacs],
				[Use xemacs for compilation]),
		[ if test "$withval" != "no"; then EMACS=xemacs; else EMACS=${withval}; fi ])
AC_ARG_WITH(emacs,
		AC_HELP_STRING([--with-emacs],
				[Use emacs for compilation]),
		[ if test "$withval" != "no"; then EMACS=emacs; else EMACS=${withval}; fi ])

AC_CHECK_PROGS([EMACS], [xemacs emacs], [no])
if test "${EMACS}" = "no"; then
   AC_MSG_ERROR([no emacs command found])
fi

dnl 
AC_CHECK_PROGS([MAKE], [gmake make], [no])
if test "${MAKE}" = "no"; then
   AC_MSG_ERROR([no make command found])
fi

AC_PROG_MAKE_SET

dnl 
AC_CHECK_PROGS([MAKEINFO], [makeinfo], [makeinfo], [no])
if test "${MAKEINFO}" = "no"; then
   AC_MSG_ERROR([no makeinfo command found])
fi

dnl AC_EMACS_INFO

dnl for cygwin windows environment
AC_PATH_PROGS(CYGPATH, cygpath)
AC_PATH_PROGS(PERL, perl perl5)
AC_CHECK_PROGS(EGREP, egrep grep)


AM_PATH_LISPDIR

dnl AS_IF([test "$EMACS" = no], [AC_MSG_ERROR([cannot find Emacs])])
dnl AX_CHECK_EMACS_LIB([mailcrypt], [HAVE_MAILCRYPT=yes], [HAVE_MAILCRYPT=no])
AC_EMACS_VERSION
dnl AC_PATH_INFODIR
AC_CHECK_MAILCRYPT
dnl AC_CHECK_ELISP(mailcrypt)
dnl AC_CONFIG_FILES(Makefile records.el records-vars.el)
dnl AC_CONFIG_FILES(recordsadmin, [chmod +x recordsadmin])
dnl AC_SUBST(pkgdatadir)
AC_SUBST(MAKEINFO)
AC_SUBST(EMACS)
AC_CONFIG_FILES(Makefile src/Makefile doc/Makefile src/records-version.el src/records-vars.el)
AC_CONFIG_FILES([src/recordsadmin], [chmod +x src/recordsadmin])
AC_CONFIG_FILES(elisp-comp)
AC_OUTPUT
