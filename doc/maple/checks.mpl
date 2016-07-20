<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file sets up a framework for checking various assertions.
<a name="line_2"></a>
<a name="line_3"></a># Usually, for each file foo.mpl containing definitions, we have 
<a name="line_4"></a># a corresponding file foo_check.mpl which checks that those 
<a name="line_5"></a># definitions have the required properties.  Each such checking file
<a name="line_6"></a># defines one or more functions with names like check_blah() (and
<a name="line_7"></a># no arguments).  Each such function should start with the line
<a name="line_8"></a>#
<a name="line_9"></a>#  printf("%a()\n",procname);
<a name="line_10"></a>#
<a name="line_11"></a># which will cause the function to print its name when it is run.
<a name="line_12"></a># After that, there should be a number of statements like
<a name="line_13"></a>#
<a name="line_14"></a># _ASSERT(<assertion>,<message>)
<a name="line_15"></a>#
<a name="line_16"></a># See the definition of _ASSERT in util.mpl for explanation of what
<a name="line_17"></a># this does.
<a name="line_18"></a>
<a name="line_19"></a>kernelopts(assertlevel=1):
<a name="line_20"></a>
<a name="line_21"></a># The variable assert_count is incremented by every call to _ASSERT().
<a name="line_22"></a>
<a name="line_23"></a>assert_count := 0: <span style="color:red">#@ assert_count 
</span><a name="line_24"></a>
<a name="line_25"></a># This should not be required as _EnvExplicit is set in util.mpl
<a name="line_26"></a># already.  However, it seems to get unset at some point.
<a name="line_27"></a>_EnvExplicit := true:
<a name="line_28"></a>
<a name="line_29"></a>######################################################################
<a name="line_30"></a>
<a name="line_31"></a># checklist is a list of checking functions.  check_all() runs all
<a name="line_32"></a># the checking functions in the list.  add_check() adds a function 
<a name="line_33"></a># to the list.
<a name="line_34"></a>
<a name="line_35"></a>checklist := []: <span style="color:red">#@ checklist 
</span><a name="line_36"></a>
<a name="line_37"></a><span style="color:red">#@ check_all 
</span><a name="line_38"></a>check_all := proc() 
<a name="line_39"></a> global assert_count;
<a name="line_40"></a> local C;
<a name="line_41"></a>
<a name="line_42"></a> assert_count := 0;
<a name="line_43"></a>
<a name="line_44"></a> for C in checklist do C(); od:
<a name="line_45"></a>end:
<a name="line_46"></a>
<a name="line_47"></a><span style="color:red">#@ add_check
</span><a name="line_48"></a>add_check := proc(C)
<a name="line_49"></a> global checklist;
<a name="line_50"></a>
<a name="line_51"></a> checklist := [op(checklist),C];
<a name="line_52"></a>end:
<a name="line_53"></a>
<a name="line_54"></a>######################################################################
<a name="line_55"></a>
<a name="line_56"></a>
  </pre>
 </body>
</html>
    