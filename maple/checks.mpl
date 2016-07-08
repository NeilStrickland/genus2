# This file sets up a framework for checking various assertions.

# Usually, for each file foo.mpl containing definitions, we have 
# a corresponding file foo_check.mpl which checks that those 
# definitions have the required properties.  Each such checking file
# defines one or more functions with names like check_blah() (and
# no arguments).  Each such function should start with the line
#
#  printf("%a()\n",procname);
#
# which will cause the function to print its name when it is run.
# After that, there should be a number of statements like
#
# _ASSERT(<assertion>,<message>)
#
# See the definition of _ASSERT in util.mpl for explanation of what
# this does.

kernelopts(assertlevel=1):

# The variable assert_count is incremented by every call to _ASSERT().

assert_count := 0: #@ assert_count 

# This should not be required as _EnvExplicit is set in util.mpl
# already.  However, it seems to get unset at some point.
_EnvExplicit := true:

######################################################################

# checklist is a list of checking functions.  check_all() runs all
# the checking functions in the list.  add_check() adds a function 
# to the list.

checklist := []: #@ checklist 

#@ check_all 
check_all := proc() 
 global assert_count;
 local C;

 assert_count := 0;

 for C in checklist do C(); od:
end:

#@ add_check
add_check := proc(C)
 global checklist;

 checklist := [op(checklist),C];
end:

######################################################################


