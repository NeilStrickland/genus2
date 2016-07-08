check_quadrature := proc()
 local Q;

 printf("%a()\n",procname);

 read(cat(data_dir,"/quadrature/wandzurat_xiao_30.mpl"));

 Q := eval(wandzurat_xiao_30):

 _ASSERT(Q["degree"] = 30 and Q["num_points"] = 175,
   "Basic parameters for wandzurat_xiao_30"
 );

 _ASSERT(Q["accuracy"] < 10.^(-98),
   "Accuracy of wandzurat_xiao_30"
 );

 read(cat(data_dir,"/quadrature/dunavant_19.mpl"));

 Q := eval(dunavant_19):

 _ASSERT(Q["degree"] = 19 and Q["num_points"] = 73,
   "Basic parameters for dunavant_19"
 );

 _ASSERT(Q["accuracy"] < 10.^(-98),
   "Accuracy of dunavant_19"
 );

 NULL;
end:


add_check(check_quadrature):
