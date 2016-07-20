<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_homology_embedded := proc()
<a name="line_2"></a> local i,j;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT(
<a name="line_7"></a>  {seq(seq(E_winding_numbers(c_E[k],48,i*0.1) -~ c_homology[k],k=0..16),i=1..9)} = {[0$4]},
<a name="line_8"></a>  "winding numbers of c[0],..,c[16]"
<a name="line_9"></a> );
<a name="line_10"></a>
<a name="line_11"></a>end:
<a name="line_12"></a>
<a name="line_13"></a>add_check(check_homology_embedded):  </pre>
 </body>
</html>
    