check_homology_embedded := proc()
 local i,j;

 printf("%a()\n",procname);

 _ASSERT(
  {seq(seq(E_winding_numbers(c_E[k],48,i*0.1) -~ c_homology[k],k=0..16),i=1..9)} = {[0$4]},
  "winding numbers of c[0],..,c[16]"
 );

end:

add_check(check_homology_embedded):