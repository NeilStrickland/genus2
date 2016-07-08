# Reading this file will perform all checks defined in the files
# listed below.  

check_precromulent("E");
check_precromulent("P");
check_precromulent("H");

olddir := currentdir(maple_dir):
full_assert_count := 0:

check_file := proc(f)
 global checklist,assert_count,full_assert_count;
 
 printf("\nReading %s\n",f);
 checklist := []:
 assert_count := 0;
 read(f);
 check_all();
 full_assert_count := full_assert_count + assert_count;
end:

check_file("brent_check.mpl"):
check_file("cromulent_check.mpl"):
check_file("group_check.mpl"):
check_file("groupoid_check.mpl"):
check_file("homology_check.mpl"):
check_file("nets_check.mpl"):
check_file("Rn_check.mpl"):
check_file("embedded/EX_check.mpl"):
check_file("embedded/annular_charts_check.mpl"):
check_file("embedded/barycentric_check.mpl"):
check_file("embedded/cayley_check.mpl"):
check_file("embedded/curvature_check.mpl"):
check_file("embedded/disc_proj_check.mpl"):
check_file("embedded/extra_curves_check.mpl"):
check_file("embedded/extra_vertices_check.mpl"):
check_file("embedded/E_quadrature_check.mpl"):
check_file("embedded/geometry_check.mpl"):
check_file("embedded/homology_check.mpl"):
check_file("embedded/invariants_check.mpl"):
check_file("embedded/roothalf/E_roothalf_check.mpl"):
check_file("embedded/roothalf/better_sphere_quotients_check.mpl"):
check_file("embedded/roothalf/cayley_surface_check.mpl"):
check_file("embedded/roothalf/crease_check.mpl"):
check_file("embedded/roothalf/forms_check.mpl"):
check_file("embedded/roothalf/galois_check.mpl"):
check_file("embedded/roothalf/rational_check.mpl"):
check_file("embedded/roothalf/sphere_quotients_check.mpl"):
check_file("embedded/roothalf/square_diffeo_check.mpl"):
check_file("embedded/roothalf/torus_quotients_check.mpl"):
check_file("embedded/roothalf/zeta_check.mpl"):
check_file("hyperbolic/HX_check.mpl"):
check_file("hyperbolic/geodesics_check.mpl"):
check_file("hyperbolic/Pi_check.mpl"):
check_file("hyperbolic/schwarz_check.mpl"):
check_file("projective/PX_check.mpl"):
check_file("projective/ellquot_check.mpl"):
check_file("projective/galois_check.mpl"):
check_file("projective/picard_fuchs_check.mpl"):
check_file("quadrature/quadrature_check.mpl"):

printf("full_assert_count = %d\n",full_assert_count):

currentdir(olddir):

