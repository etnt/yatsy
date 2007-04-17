AC_INIT(yatsy, 0.1, tobbe@tornkvist.org, yatsy)
AC_PREREQ(2.59c)
AC_COPYRIGHT(Copyright (C) 2007 Torbjorn Tornkvist)

YATSY_DIR=$(dirname `pwd`)
AC_SUBST(YATSY_DIR)

AC_ERLANG_NEED_ERL

AC_ARG_WITH([yaws_dir],
	[AS_HELP_STRING([--with-yaws_dir],
              [path to Yaws directory])],
            [with_yaws_dir="$withval"],
            [with_yaws_dir=no])

# If an explicit path to Yaws is given, then use that
# in order to locate Yaws.
AS_IF([test "x$with_yaws_dir" != xno],
            [
	    AC_PATH_PROG(YAWS, yaws, [], "$with_yaws_dir/bin")
	    AC_SUBST(YAWS)
	    YAWSDIR=`awk -F= '/yawsdir=/ { print [$]2; exit; }' $YAWS`
	    AC_SUBST(YAWSDIR)
	    ],
	    [   
	    AC_PATH_PROG(YAWS, yaws)
	    AC_SUBST(YAWS)
	    YAWSDIR=`awk -F= '/yawsdir=/ { print [$]2; exit; }' $YAWS`
	    AC_SUBST(YAWSDIR)
	    ])


define(OUTPUT_FILES,
        ../bin/yatsy)

AC_OUTPUT(OUTPUT_FILES,
[
chmod u+x ../bin/yatsy
])dnl
