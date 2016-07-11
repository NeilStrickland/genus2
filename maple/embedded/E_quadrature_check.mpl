check_E_quadrature := proc()
 local Q,f;

 printf("%a()\n",procname);

 f := cat(data_dir,"/embedded/roothalf/quadrature_frobenius_256a.m");
 if not(FileTools[Exists](f)) then
  printf("Skipping check_E_quadrature() because data file %s does not exist.\n",f);
  printf("(It can be generated using build_data[\"E_quadrature_rule\"]())\n");
  return NULL;
 fi;

 read(f);
 Q := eval(quadrature_frobenius_256a);

 _ASSERT(abs(Q["curvature_error"]) < 10.^(-17),"Gauss-Bonet check");
 _ASSERT(abs(Q["max_stokes_error"]) < 10.^(-7),"Stokes check");
end:

add_check(check_E_quadrature):
