<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># Reading this file will perform all checks defined in the files
<a name="line_2"></a># listed below.  
<a name="line_3"></a>
<a name="line_4"></a>check_precromulent("E");
<a name="line_5"></a>check_precromulent("P");
<a name="line_6"></a>check_precromulent("H");
<a name="line_7"></a>
<a name="line_8"></a>olddir := currentdir(maple_dir):
<a name="line_9"></a>full_assert_count := 0:
<a name="line_10"></a>
<a name="line_11"></a>check_file := proc(f)
<a name="line_12"></a> global checklist,assert_count,full_assert_count;
<a name="line_13"></a> 
<a name="line_14"></a> printf("\nReading %s\n",f);
<a name="line_15"></a> checklist := []:
<a name="line_16"></a> assert_count := 0;
<a name="line_17"></a> read(f);
<a name="line_18"></a> check_all();
<a name="line_19"></a> full_assert_count := full_assert_count + assert_count;
<a name="line_20"></a>end:
<a name="line_21"></a>
<a name="line_22"></a>check_file("brent_check.mpl"):
<a name="line_23"></a>check_file("cromulent_check.mpl"):
<a name="line_24"></a>check_file("group_check.mpl"):
<a name="line_25"></a>check_file("groupoid_check.mpl"):
<a name="line_26"></a>check_file("homology_check.mpl"):
<a name="line_27"></a>check_file("nets_check.mpl"):
<a name="line_28"></a>check_file("Rn_check.mpl"):
<a name="line_29"></a>check_file("projective/PX_check.mpl"):
<a name="line_30"></a>check_file("projective/ellquot_check.mpl"):
<a name="line_31"></a>check_file("projective/galois_check.mpl"):
<a name="line_32"></a>check_file("projective/picard_fuchs_check.mpl"):
<a name="line_33"></a>check_file("hyperbolic/HX_check.mpl"):
<a name="line_34"></a>check_file("hyperbolic/geodesics_check.mpl"):
<a name="line_35"></a>check_file("hyperbolic/Pi_check.mpl"):
<a name="line_36"></a>check_file("hyperbolic/schwarz_check.mpl"):
<a name="line_37"></a>check_file("quadrature/quadrature_check.mpl"):
<a name="line_38"></a>check_file("embedded/EX_check.mpl"):
<a name="line_39"></a>check_file("embedded/annular_charts_check.mpl"):
<a name="line_40"></a>check_file("embedded/barycentric_check.mpl"):
<a name="line_41"></a>check_file("embedded/cayley_check.mpl"):
<a name="line_42"></a>check_file("embedded/curvature_check.mpl"):
<a name="line_43"></a>check_file("embedded/disc_proj_check.mpl"):
<a name="line_44"></a>check_file("embedded/E_quadrature_check.mpl"):
<a name="line_45"></a>check_file("embedded/extra_curves_check.mpl"):
<a name="line_46"></a>check_file("embedded/extra_vertices_check.mpl"):
<a name="line_47"></a>check_file("embedded/geometry_check.mpl"):
<a name="line_48"></a>check_file("embedded/homology_check.mpl"):
<a name="line_49"></a>check_file("embedded/invariants_check.mpl"):
<a name="line_50"></a>check_file("embedded/roothalf/E_roothalf_check.mpl"):
<a name="line_51"></a>check_file("embedded/roothalf/better_sphere_quotients_check.mpl"):
<a name="line_52"></a>check_file("embedded/roothalf/cayley_surface_check.mpl"):
<a name="line_53"></a>check_file("embedded/roothalf/crease_check.mpl"):
<a name="line_54"></a>check_file("embedded/roothalf/forms_check.mpl"):
<a name="line_55"></a>check_file("embedded/roothalf/galois_check.mpl"):
<a name="line_56"></a>check_file("embedded/roothalf/rational_check.mpl"):
<a name="line_57"></a>check_file("embedded/roothalf/sphere_quotients_check.mpl"):
<a name="line_58"></a>check_file("embedded/roothalf/square_diffeo_check.mpl"):
<a name="line_59"></a>check_file("embedded/roothalf/torus_quotients_check.mpl"):
<a name="line_60"></a>check_file("embedded/roothalf/zeta_check.mpl"):
<a name="line_61"></a>
<a name="line_62"></a>printf("full_assert_count = %d\n",full_assert_count):
<a name="line_63"></a>
<a name="line_64"></a>currentdir(olddir):
<a name="line_65"></a>
  </pre>
 </body>
</html>
    