<?php

$tex_dir = dirname(dirname(__DIR__));
$work_dir = $tex_dir . '/genus2';
$dist_dir = $tex_dir . '/genus2_dist';

function rm_rf($dir) {
 if (! $dir) { return(false); }
 
 $files = array_diff(scandir($dir), array('.', '..')); 

 foreach ($files as $file) { 
  (is_dir("$dir/$file")) ? rm_rf("$dir/$file") : unlink("$dir/$file"); 
 }

 return rmdir($dir); 
}

if (is_dir($dist_dir)) {
 rm_rf($dist_dir);
}

mkdir($dist_dir);

$subdirs =
 array(
       '/latex',
       '/latex/tikz_includes',
       '/maple',
       '/maple/domain',
       '/maple/quadrature',
       '/maple/projective',
       '/maple/hyperbolic',
       '/maple/embedded',
       '/maple/embedded/roothalf',
       '/worksheets',
       '/doc',
       '/data',
       '/data/domain',
       '/data/quadrature',
       '/data/projective',
       '/data/hyperbolic',
       '/data/embedded',
       '/data/embedded/roothalf',
       '/images',
       '/plots',
       '/plots/thumbs'
       );

foreach($subdirs as $d) {
 echo "Creating $d" . PHP_EOL;
 mkdir($dist_dir . $d);
}

$latex_files =
 array(
       '/latex/genus2.tex',
       '/latex/genus2.bib',
       '/latex/tikz_includes/H_to_P_graph.tex',
       '/latex/tikz_includes/y_proj_a.tex',
       '/latex/tikz_includes/y_proj_b.tex',
       '/latex/tikz_includes/y_proj_c.tex',
       '/latex/tikz_includes/y_proj.tex',
       '/latex/tikz_includes/y_proj_F4.tex',
       '/latex/tikz_includes/z_proj.tex',
       '/latex/tikz_includes/z_proj_F16.tex',
       '/latex/tikz_includes/w_proj_F16.tex',
       '/latex/tikz_includes/disc_pi.tex',
       '/latex/tikz_includes/disc_delta.tex',
       '/latex/tikz_includes/disc_zeta.tex',
       '/latex/tikz_includes/beta_plot.tex',
       );

$image_files = 
 array(
       '/images/XX.jpg',
       '/images/Omega.jpg',
       '/images/XOmega.jpg',
       '/images/curves.jpg',
       '/images/F4.jpg',
       '/images/pi_ring.jpg',
       '/images/delta_ring.jpg',
       '/images/zeta_ring.jpg',
       '/images/y_ring.jpg',
       '/images/E_to_S_u[1].jpg',
       '/images/E_to_S_u[2].jpg',
       '/images/E_to_S_u[3].jpg',
       );

$maple_files = 
 array(
       '/maple/util.mpl',
       '/maple/class.mpl',
       '/maple/Rn.mpl',
       '/maple/build_data.mpl',
       '/maple/build_data_toy.mpl',
       '/maple/brent.mpl',
       '/maple/group.mpl',
       '/maple/groupoid.mpl',
       '/maple/cromulent.mpl',
       '/maple/nets.mpl',
       '/maple/latex.mpl',
       '/maple/plots.mpl',
       '/maple/checks.mpl',
       '/maple/quadrature/quadrature.mpl',
       '/maple/domain/domain.mpl',
       '/maple/domain/domain_point.mpl',
       '/maple/domain/domain_edge.mpl',
       '/maple/domain/domain_face.mpl',
       '/maple/domain/grid.mpl',
       '/maple/embedded/EX.mpl',
       '/maple/embedded/invariants.mpl',
       '/maple/embedded/geometry.mpl',
       '/maple/embedded/curvature.mpl',
       '/maple/embedded/EX0.mpl',
       '/maple/embedded/extra_curves.mpl',
       '/maple/embedded/extra_vertices.mpl',
       '/maple/embedded/annular_charts.mpl',
       '/maple/embedded/disc_proj.mpl',
       '/maple/embedded/cayley.mpl',
       '/maple/embedded/barycentric.mpl',
       '/maple/embedded/E_domain.mpl',
       '/maple/embedded/E_quadrature.mpl',
       '/maple/embedded/homology.mpl',
       '/maple/embedded/roothalf/E_roothalf.mpl',
       '/maple/embedded/roothalf/cayley_surface.mpl',
       '/maple/embedded/roothalf/E_atlas.mpl',
       '/maple/embedded/roothalf/EH_atlas.mpl',
       '/maple/embedded/roothalf/crease.mpl',
       '/maple/embedded/roothalf/forms.mpl',
       '/maple/embedded/roothalf/rational.mpl',
       '/maple/embedded/roothalf/square_diffeo.mpl',
       '/maple/embedded/roothalf/zeta.mpl',
       '/maple/embedded/roothalf/sphere_quotients.mpl',
       '/maple/embedded/roothalf/better_sphere_quotients.mpl',
       '/maple/embedded/roothalf/E_to_S.mpl',
       '/maple/embedded/roothalf/torus_quotients.mpl',
       '/maple/embedded/roothalf/group64.mpl',
       '/maple/embedded/roothalf/galois.mpl',
       '/maple/embedded/roothalf/KR_subfields.mpl',
       '/maple/embedded/roothalf/plots.mpl',
       '/maple/embedded/roothalf/isometric_grid.mpl',
       '/maple/projective/PX.mpl',
       '/maple/projective/galois.mpl',
       '/maple/projective/ellquot.mpl',
       '/maple/projective/picard_fuchs.mpl',
       '/maple/projective/PK_subfields.mpl',
       '/maple/projective/PX0.mpl',
       '/maple/hyperbolic/Pi.mpl',
       '/maple/hyperbolic/HX.mpl',
       '/maple/hyperbolic/HX0.mpl',
       '/maple/hyperbolic/H_to_P.mpl',
       '/maple/hyperbolic/P_to_H.mpl',
       '/maple/hyperbolic/geodesics.mpl',
       '/maple/hyperbolic/schwarz.mpl',
       '/maple/hyperbolic/plots.mpl',
       '/maple/hyperbolic/automorphic.mpl',
       );

$check_files =
 array(
       'brent_check.mpl',
       'cromulent_check.mpl',
       'group_check.mpl',
       'groupoid_check.mpl',
       'homology_check.mpl',
       'nets_check.mpl',
       'Rn_check.mpl',
       'embedded/EX_check.mpl',
       'embedded/annular_charts_check.mpl',
       'embedded/barycentric_check.mpl',
       'embedded/cayley_check.mpl',
       'embedded/curvature_check.mpl',
       'embedded/disc_proj_check.mpl',
       'embedded/extra_curves_check.mpl',
       'embedded/extra_vertices_check.mpl',
       'embedded/E_quadrature_check.mpl',
       'embedded/geometry_check.mpl',
       'embedded/homology_check.mpl',
       'embedded/invariants_check.mpl',
       'embedded/roothalf/E_roothalf_check.mpl',
       'embedded/roothalf/better_sphere_quotients_check.mpl',
       'embedded/roothalf/cayley_surface_check.mpl',
       'embedded/roothalf/crease_check.mpl',
       'embedded/roothalf/forms_check.mpl',
       'embedded/roothalf/galois_check.mpl',
       'embedded/roothalf/rational_check.mpl',
       'embedded/roothalf/sphere_quotients_check.mpl',
       'embedded/roothalf/square_diffeo_check.mpl',
       'embedded/roothalf/torus_quotients_check.mpl',
       'embedded/roothalf/zeta_check.mpl',
       'hyperbolic/HX_check.mpl',
       'hyperbolic/geodesics_check.mpl',
       'hyperbolic/Pi_check.mpl',
       'hyperbolic/schwarz_check.mpl',
       'projective/PX_check.mpl',
       'projective/ellquot_check.mpl',
       'projective/galois_check.mpl',
       'projective/picard_fuchs_check.mpl',
       'quadrature/quadrature_check.mpl',
       );

$worksheet_files = 
 array(
       '/worksheets/checks.mw',
       '/worksheets/check_all.mw',
       '/worksheets/text_check.mw',
       '/worksheets/build_data.mw',
       '/worksheets/build_data.mw',
       '/worksheets/build_data_toy.mw',
       );

$files =
 array_merge(
	     $latex_files,
	     $image_files,
	     $maple_files,
	     $check_files,
	     $worksheet_files
	     );

foreach($files as $f) {
 echo "Copying $f" . PHP_EOL;
 copy($work_dir . $f,$dist_dir . $f);
}

?>
