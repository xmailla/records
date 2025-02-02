
bin_SCRIPTS = recordsadmin
my_lisp_sources = records-macro.el	\
records-dindex.el records-index.el records-search.el		\
records-tex.el records-util.el records-vars.el			\
records.el

dist_lisp_LISP = $(my_lisp_sources) records-autoloads.el records-version.el
dist_pkgdata_DATA = records-templ.tex
EXTRA_DIST = records-pkg.el.in

# list of Lisp files that will not be installed
# noinst_LISP = records-load.el

CLEANFILES = records-pkg.el

MAINTAINERCLEANFILES = Makefile.in
# MAINTAINERCLEANFILES = records-autoloads.el Makefile.in
