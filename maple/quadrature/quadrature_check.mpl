check_quadrature := proc()
 local Q,f;

 printf("%a()\n",procname);

 f := cat(data_dir,"/quadrature/wandzurat_xiao_30.mpl");
 if not(FileTools[Exists](f)) then
  printf("Skipping check of wandzurat_xiao_30 because data file %s does not exist.\n",f);
  printf("(It can be generated using build_data[\"triangle_quadrature_rule\"]())\n");
  return NULL;
 else 
  read(f);

  Q := eval(wandzurat_xiao_30):

  _ASSERT(Q["degree"] = 30 and Q["num_points"] = 175,
    "Basic parameters for wandzurat_xiao_30"
  );

  _ASSERT(Q["accuracy"] < 10.^(-98),
    "Accuracy of wandzurat_xiao_30"
  );
 fi;

 f := cat(data_dir,"/quadrature/dunavant_19.mpl");
 if not(FileTools[Exists](f)) then
  printf("Skipping check of dunavant_19 because data file %s does not exist.\n",f);
 else 
  read(f);

  Q := eval(dunavant_19):

  _ASSERT(Q["degree"] = 19 and Q["num_points"] = 73,
    "Basic parameters for dunavant_19"
  );

  _ASSERT(Q["accuracy"] < 10.^(-98),
    "Accuracy of dunavant_19"
  );
 fi;
 
 NULL;
end:


add_check(check_quadrature):
