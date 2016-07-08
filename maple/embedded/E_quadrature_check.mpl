check_E_quadrature := proc()
 local Q;

 printf("%a()\n",procname);

 read(cat(data_dir,"/embedded/roothalf/quadrature_frobenius_256a.m"));
 Q := eval(quadrature_frobenius_256a);

 _ASSERT(abs(Q["curvature_error"]) < 10.^(-17),"Gauss-Bonet check");
 _ASSERT(abs(Q["max_stokes_error"]) < 10.^(-7),"Stokes check");
end:

add_check(check_E_quadrature):
